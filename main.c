#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <pthread.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>

pthread_mutex_t key_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t key_cond = PTHREAD_COND_INITIALIZER;

volatile uint8_t key_pressed = 0xFF;

int hex_key_to_value(const char *key) {
  if (!strcmp(key, "1"))
    return 0x1;
  if (!strcmp(key, "2"))
    return 0x2;
  if (!strcmp(key, "3"))
    return 0x3;
  if (!strcmp(key, "C"))
    return 0xC;
  if (!strcmp(key, "4"))
    return 0x4;
  if (!strcmp(key, "5"))
    return 0x5;
  if (!strcmp(key, "6"))
    return 0x6;
  if (!strcmp(key, "D"))
    return 0xD;
  if (!strcmp(key, "7"))
    return 0x7;
  if (!strcmp(key, "8"))
    return 0x8;
  if (!strcmp(key, "9"))
    return 0x9;
  if (!strcmp(key, "E"))
    return 0xE;
  if (!strcmp(key, "A"))
    return 0xA;
  if (!strcmp(key, "0"))
    return 0x0;
  if (!strcmp(key, "B"))
    return 0xB;
  if (!strcmp(key, "F"))
    return 0xF;
  return 0xFF;
}

int keypress_cb(ClientData clientData, Tcl_Interp *interp, int argc,
                const char *argv[]) {
  if (argc < 2)
    return TCL_ERROR;

  uint8_t key = hex_key_to_value(argv[1]);

  pthread_mutex_lock(&key_mutex);
  key_pressed = key;
  pthread_cond_signal(&key_cond);
  pthread_mutex_unlock(&key_mutex);

  return TCL_OK;
}

void refresh_display(Tcl_Interp *interp, uint8_t display[64 * 32]) {
  Tcl_Eval(interp, ".main.display delete all");
  for (int i = 0; i < 64 * 32; i++) {
    int x = i % 64;
    int y = i / 64;
    if (display[i]) {
      char cmd[256];
      snprintf(cmd, sizeof(cmd),
               ".main.display create rectangle %d %d %d %d -fill white "
               "-outline white",
               x * 10, y * 10, (x + 1) * 10, (y + 1) * 10);
      Tcl_Eval(interp, cmd);
    }
  }
}

//#define printf(x, ...)

typedef struct state_t state_t;
typedef state_t (*function_t)(state_t);

struct state_t {
  uint8_t memory[4096];
  uint8_t V[16]; // (V0 to VF)
  uint16_t I;
  uint16_t pc;
  uint8_t display[64 * 32];
  uint16_t stack[16];
  uint16_t sp;
  uint8_t delay_timer;
  uint8_t sound_timer;
};

state_t fetch_instruction(state_t state);
state_t jump_to_address(state_t state) {
  uint16_t address =
      (state.memory[state.pc - 2] & 0x0F) << 8 | state.memory[state.pc - 1];
  uint16_t tpc = state.pc;
  state.pc = address;

  printf("jump_to_address 0x%03X -> 0x%03X\n", tpc, address);
  return state;
}

state_t return_from_subroutine(state_t state) {
  if (state.sp > 0) {
    uint16_t tpc = state.pc;
    state.pc = state.stack[--state.sp];
    printf("return_from_subroutine 0x%03X -> 0x%03X\n", tpc, state.pc);
  } else {
    printf("stack underflow on return\n");
    exit(1);
  }
  return state;
}

state_t call_subroutine(state_t state) {
  if (state.sp >= 16) {
    printf("stack overflow at 0x%03X\n", state.pc);
    exit(1);
  } else {
    state.stack[state.sp] = state.pc;
    state.sp++;
    uint16_t address =
        (state.memory[state.pc - 2] & 0x0F) << 8 | state.memory[state.pc - 1];
    uint16_t tpc = state.pc;
    state.pc = address;
    printf("call_subroutine 0x%03X -> 0x%03X\n", tpc, address);
  }
  return state;
}

#define VX (state.memory[state.pc - 2] & 0x0F)
#define VY ((state.memory[state.pc - 1] & 0xF0) >> 4)

state_t set_vx_nn(state_t state) {
  uint8_t x = (state.memory[state.pc - 2] & 0x0F);
  uint8_t nn = state.memory[state.pc - 1];
  state.V[x] = nn;
  printf("set V[%X] to 0x%02X\n", x, nn);
  return state;
}

state_t set_vx_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;
  state.V[x] = state.V[y];
  printf("set V[%X] to V[%X]\n", x, y);
  return state;
}

state_t vx_or_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;
  state.V[x] |= state.V[y];
  printf("V[%X] |= V[%X] : 0x%02X\n", x, y, state.V[x]);
  return state;
}

state_t vx_and_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;
  state.V[x] &= state.V[y];
  printf("V[%X] &= V[%X] : 0x%02X\n", x, y, state.V[x]);
  return state;
}

state_t vx_xor_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;
  state.V[x] ^= state.V[y];
  printf("V[%X] ^= V[%X] : 0x%02X\n", x, y, state.V[x]);
  return state;
}

state_t vx_add_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;
  uint16_t result = state.V[x] + state.V[y];
  state.V[0xF] = (result > 0xFF) ? 1 : 0;
  // lower 8 bits in VX
  state.V[x] = result & 0xFF;
  printf("V[%X] += V[%X] : 0x%02X, VF: %d\n", x, y, state.V[x], state.V[0xF]);
  return state;
}

state_t vx_sub_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;
  state.V[0xF] = (state.V[x] >= state.V[y]) ? 1 : 0;
  state.V[x] = state.V[x] - state.V[y];
  printf("V[%X] -= V[%X] : 0x%02X, VF: %d\n", x, y, state.V[x], state.V[0xF]);
  return state;
}

state_t vx_shift_right(state_t state) {
  uint8_t x = VX;

  // set VF to the least significant bit of VX before shifting
  state.V[0xF] = state.V[x] & 0x01;

  // shift VX right by 1
  state.V[x] >>= 1;
  printf("V[%X] >>= 1 : 0x%02X, VF (old LSB): %d\n", x, state.V[x],
         state.V[0xF]);
  return state;
}

state_t vx_set_vy_minus_vx(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;

  // set VF to 1 if VY >= VX (no borrow), 0 otherwise
  state.V[0xF] = (state.V[y] >= state.V[x]) ? 1 : 0;

  // Set VX to VY - VX
  state.V[x] = state.V[y] - state.V[x];
  printf("V[%X] = V[%X] - V[%X] : 0x%02X, VF: %d\n", x, y, x, state.V[x],
         state.V[0xF]);
  return state;
}

state_t vx_shift_left(state_t state) {
  uint8_t x = (state.memory[state.pc - 2] & 0x0F);

  // set VF to the most significant bit of VX before shifting
  state.V[0xF] = (state.V[x] & 0x80) >> 7;

  // shift VX left by 1
  state.V[x] <<= 1;
  printf("V[%X] <<= 1 : 0x%02X, VF (old MSB): %d\n", x, state.V[x],
         state.V[0xF]);
  return state;
}

state_t add_nn_to_vx(state_t state) {
  uint8_t x = VX;
  uint8_t nn = state.memory[state.pc - 1];
  state.V[x] += nn;
  printf("add 0x%02X to V[%X], result: 0x%02X\n", nn, x, state.V[x]);
  return state;
}

state_t unknown_instruction(state_t state) {
  exit(1);
  return state;
}

state_t set_index_register(state_t state) {
  uint16_t address =
      (state.memory[state.pc - 2] & 0x0F) << 8 | state.memory[state.pc - 1];
  state.I = address;
  printf("Set I to 0x%03X\n", address);
  return state;
}

state_t skip_if_vx_not_nn(state_t state) {
  uint8_t x = VX;
  uint8_t nn = state.memory[state.pc - 1];
  if (state.V[x] != nn) {
    state.pc += 2;
    printf("skip_if_vx_not_nn V[%X] (0x%02X) != 0x%02X, skipping\n", x,
           state.V[x], nn);
  } else {
    printf("skip_if_vx_not_nn V[%X] (0x%02X) == 0x%02X, not skipping\n", x,
           state.V[x], nn);
  }
  return state;
}

state_t skip_if_vx_eq_nn(state_t state) {
  uint8_t x = VX;
  uint8_t nn = state.memory[state.pc - 1];

  if (state.V[x] == nn) {
    state.pc += 2;
    printf("skip_if_vx_eq_nn V[%X] (0x%02X) == 0x%02X, skipping\n", x,
           state.V[x], nn);
  } else {
    printf("skip_if_vx_eq_nn V[%X] (0x%02X) != 0x%02X, not skipping\n", x,
           state.V[x], nn);
  }
  return state;
}

state_t skip_if_vx_eq_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;

  if (state.V[x] == state.V[y]) {
    state.pc += 2;
    printf("skip_if_vx_eq_vy V[%X] (0x%02X) == V[%X] (0x%02X), skipping\n", x,
           state.V[x], y, state.V[y]);
  } else {
    printf("skip_if_vx_eq_vy V[%X] (0x%02X) != V[%X] (0x%02X), not skipping\n",
           x, state.V[x], y, state.V[y]);
  }
  return state;
}

state_t skip_if_vx_ne_vy(state_t state) {
  uint8_t x = VX;
  uint8_t y = VY;

  if (state.V[x] != state.V[y]) {
    state.pc += 2;
    printf("skip_if_vx_ne_vy V[%X] (0x%02X) != V[%X] (0x%02X), skipping\n", x,
           state.V[x], y, state.V[y]);
  } else {
    printf("skip_if_vx_ne_vy V[%X] (0x%02X) == V[%X] (0x%02X), not skipping\n",
           x, state.V[x], y, state.V[y]);
  }
  return state;
}

state_t vx_rand_and_nn(state_t state) {
  uint8_t x = VX;
  uint8_t nn = state.memory[state.pc - 1];
  uint8_t random_value = rand() % 256;

  state.V[x] = random_value & nn;
  printf("V[%X] = rand (0x%02X) AND 0x%02X : 0x%02X\n", x, random_value, nn,
         state.V[x]);

  return state;
}

state_t jump_nnn_plus_v0(state_t state) {
  uint16_t nnn =
      ((state.memory[state.pc - 2] & 0x0F) << 8) | state.memory[state.pc - 1];
  uint16_t new_pc = nnn + state.V[0]; // NNN + V0
  printf("jump to 0x%03X + V[0] (0x%02X) : 0x%03X\n", nnn, state.V[0], new_pc);
  state.pc = new_pc;
  return state;
}

void wait_for_kp_s_vx(state_t *state, int vx_index) {
  pthread_mutex_lock(&key_mutex);

  while (key_pressed == 0xFF) {
    pthread_cond_wait(&key_cond, &key_mutex);
  }

  state->V[vx_index] = key_pressed;

  key_pressed = 0xFF;
  pthread_mutex_unlock(&key_mutex);
}

static function_t instruction_map[] = {
    [0x1] = jump_to_address,        [0x2] = call_subroutine,
    [0xE] = return_from_subroutine, [0x6] = set_vx_nn,
    [0x0] = unknown_instruction,    [0xA] = set_index_register,
    [0x7] = add_nn_to_vx,           [0x4] = skip_if_vx_not_nn,
    [0x3] = skip_if_vx_eq_nn,       [0x5] = skip_if_vx_eq_vy,
    [0x9] = skip_if_vx_ne_vy,       [0xB] = jump_nnn_plus_v0,
    [0xC] = vx_rand_and_nn,
};

static function_t instruction_8xy_map[] = {
    [0x0] = set_vx_vy,      [0x1] = vx_or_vy,           [0x2] = vx_and_vy,
    [0x3] = vx_xor_vy,      [0x4] = vx_add_vy,          [0x5] = vx_sub_vy,
    [0x6] = vx_shift_right, [0x7] = vx_set_vy_minus_vx, [0xE] = vx_shift_left};

state_t sub_dispatch(state_t state, function_t sub_map[], uint8_t sub_opcode) {
  if (sub_map[sub_opcode]) {
    state = sub_map[sub_opcode](state);
  } else {
    state = unknown_instruction(state);
  }
  return state;
}

void run_vm(state_t state) {
  while (state.pc < 4096) {
    uint16_t instruction =
        (state.memory[state.pc] << 8) | state.memory[state.pc + 1];
    state.pc += 2;

    uint8_t opcode = (instruction & 0xF000) >> 12;

    uint8_t sub_opcode = instruction & 0x000F;
    uint8_t x = (instruction & 0x0F00) >> 8;
    if (opcode == 0x0) {
      if (sub_opcode == 0xE) {
        state = return_from_subroutine(state);
      } else {
        state = unknown_instruction(state);
      }
    } else if (opcode == 0x8) {
      state = sub_dispatch(state, instruction_8xy_map, sub_opcode);
    } else if (opcode == 0xF) {
      if (sub_opcode == 0x0A) {
        wait_for_kp_s_vx(&state, x);
      } else {
        state = unknown_instruction(state);
      }
    } else if (instruction_map[opcode]) {
      state = instruction_map[opcode](state);
    } else {
      state = unknown_instruction(state);
    }

    if (state.pc >= 4096) {
      printf("terminating due to program counter out of bounds or unknown "
             "state\n");
      break;
    }
  }
}

void *tk_thread_main(void *arg) {
  Tcl_Interp *interp = Tcl_CreateInterp();
  if (Tcl_Init(interp) == TCL_ERROR || Tk_Init(interp) == TCL_ERROR) {
    fprintf(stderr, "tcl/tk init failed: %s\n", Tcl_GetStringResult(interp));
    return NULL;
  }

  Tcl_CreateCommand(interp, "keypress_cb", keypress_cb, NULL, NULL);

  if (Tcl_EvalFile(interp, "gui.tcl") != TCL_OK) {
    fprintf(stderr, "rrror loading tcl/tk: %s\n", Tcl_GetStringResult(interp));
    return NULL;
  }

  Tk_MainLoop();
  Tcl_DeleteInterp(interp);
  return NULL;
}

int main() {
  srand((unsigned int)time(NULL));
  pthread_t tk_thread;

  if (pthread_create(&tk_thread, NULL, tk_thread_main, NULL)) {
    fprintf(stderr, "error creating tk thread\n");
    return 1;
  }

  state_t s = {0};
  s.pc = 0x200;

  uint8_t program[] = {
      0xF0,
      0x0A, // F00A: wait for a key press, store in V0
      0x30,
      0x01, // 3001: skip next instruction if V0 == 0x01
      0x12,
      0x00, // 1200: jump to 0x200 (loop back to the beginning if V0 != 0x01)
      0x00,
      0xE0 // 00E0: clear the screen (exit condition when V0 == 0x01)
  };

  for (int i = 0; i < sizeof(program); i++) {
    s.memory[0x200 + i] = program[i];
  }

  run_vm(s);

  return 0;
}