#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include <time.h>
#include <tk.h>

typedef struct state_t state_t;
typedef state_t (*function_t)(state_t);

typedef struct {
  uint16_t instruction, nnn;
  uint8_t opcode, sub_opcode, n, nn, vx, vy;
} curr_op_t;

struct state_t {
  uint8_t memory[4096];
  uint8_t V[16]; // (V0 to VF)
  uint16_t I;
  uint16_t pc;
  uint8_t display[64 * 32];
  uint16_t stack[16];
  uint16_t sp;
  curr_op_t cop;
  uint8_t delay_timer;
  uint8_t sound_timer;
};

#define instruction state.cop.instruction
#define opcode state.cop.opcode
#define sub_opcode state.cop.sub_opcode
#define vx state.cop.vx
#define vy state.cop.vy
#define nn state.cop.nn
#define nnn state.cop.nnn

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

state_t jump_to_address(state_t state) {
  printf("jump_to_address 0x%03X -> 0x%03X\n", state.pc, nnn);
  state.pc = nnn;
  return state;
}

state_t return_from_subroutine(state_t state) {
  if (state.sp > 0) {
    state.pc = state.stack[--state.sp];
    printf("return_from_subroutine 0x%03X\n", state.pc);
  } else {
    printf("stack underflow on return, exiting...\n");
    exit(1);
  }
  return state;
}

state_t call_subroutine(state_t state) {
  if (state.sp >= 16) {
    printf("stack overflow at 0x%03X, exiting...\n", state.pc);
    exit(1);
  } else {
    printf("call_subroutine 0x%03X -> 0x%03X\n", state.pc, nnn);
    state.stack[state.sp] = state.pc;
    state.sp++;
    state.pc = nnn;
  }
  return state;
}

#define VX (state.memory[state.pc - 2] & 0x0F)
#define VY ((state.memory[state.pc - 1] & 0xF0) >> 4)

state_t set_vx_nn(state_t state) {
  printf("set V[%X] to 0x%02X\n", vx, nn);
  state.V[vx] = nn;
  return state;
}

state_t set_vx_vy(state_t state) {
  printf("set V[%X] to V[%X]\n", vx, vy);
  state.V[vx] = state.V[vy];
  return state;
}

state_t vx_or_vy(state_t state) {
  state.V[vx] |= state.V[vy];
  printf("V[%X] |= V[%X] : 0x%02X\n", vx, vy, state.V[vx]);
  return state;
}

state_t vx_and_vy(state_t state) {
  state.V[vx] &= state.V[vy];
  printf("V[%X] &= V[%X] : 0x%02X\n", vx, vy, state.V[vx]);
  return state;
}

state_t vx_xor_vy(state_t state) {
  state.V[vx] ^= state.V[vy];
  printf("V[%X] ^= V[%X] : 0x%02X\n", vx, vy, state.V[vx]);
  return state;
}

state_t vx_add_vy(state_t state) {
  uint16_t result = state.V[vx] + state.V[vy];
  state.V[0xF] = (result > 0xFF) ? 1 : 0;
  state.V[vx] = result & 0xFF; // lower 8 bits in VX
  printf("V[%X] += V[%X] : 0x%02X, VF: %d\n", vx, vy, state.V[vx],
         state.V[0xF]);
  return state;
}

state_t vx_sub_vy(state_t state) {
  state.V[0xF] = (state.V[vx] >= state.V[vy]) ? 1 : 0;
  state.V[vx] = state.V[vx] - state.V[vy];
  printf("V[%X] -= V[%X] : 0x%02X, VF: %d\n", vx, vy, state.V[vx],
         state.V[0xF]);
  return state;
}

state_t vx_shift_right(state_t state) {
  // set VF to the least significant bit of VX before shifting
  state.V[0xF] = state.V[vx] & 0x01;

  state.V[vx] >>= 1;
  printf("V[%X] >>= 1 : 0x%02X, VF (old LSB): %d\n", vx, state.V[vx],
         state.V[0xF]);
  return state;
}

state_t vx_set_vy_minus_vx(state_t state) {
  // set VF to 1 if VY >= VX (no borrow), 0 otherwise
  state.V[0xF] = (state.V[vy] >= state.V[vx]) ? 1 : 0;
  state.V[vx] = state.V[vy] - state.V[vx];
  printf("V[%X] = V[%X] - V[%X] : 0x%02X, VF: %d\n", vx, vy, vx, state.V[vx],
         state.V[0xF]);
  return state;
}

state_t vx_shift_left(state_t state) {
  // set VF to the most significant bit of VX before shifting
  state.V[0xF] = (state.V[vx] & 0x80) >> 7;
  state.V[vx] <<= 1;
  printf("V[%X] <<= 1 : 0x%02X, VF (old MSB): %d\n", vx, state.V[vx],
         state.V[0xF]);
  return state;
}

state_t add_nn_to_vx(state_t state) {
  state.V[vx] += nn;
  printf("add 0x%02X to V[%X], result: 0x%02X\n", nn, vx, state.V[vx]);
  return state;
}

state_t unknown_instruction(state_t state) {
  printf("-> unknown_instruction 0x%04X (opcode 0x%01X, sub_opcode "
         "0x%02X)\n",
         instruction, opcode, sub_opcode);
  exit(1);
  return state;
}

state_t set_index_register(state_t state) {
  state.I = nnn;
  printf("Set I to 0x%03X\n", nnn);
  return state;
}

state_t skip_if_vx_not_nn(state_t state) {
  if (state.V[vx] != nn) {
    state.pc += 2;
    printf("skip_if_vx_not_nn V[%X] (0x%02X) != 0x%02X, skipping\n", vx,
           state.V[vx], nn);
  } else {
    printf("skip_if_vx_not_nn V[%X] (0x%02X) == 0x%02X, not skipping\n", vx,
           state.V[vx], nn);
  }
  return state;
}

state_t skip_if_vx_eq_nn(state_t state) {
  if (state.V[vx] == nn) {
    state.pc += 2;
    printf("skip_if_vx_eq_nn V[%X] (0x%02X) == 0x%02X, skipping\n", vx,
           state.V[vx], nn);
  } else {
    printf("skip_if_vx_eq_nn V[%X] (0x%02X) != 0x%02X, not skipping\n", vx,
           state.V[vx], nn);
  }
  return state;
}

state_t skip_if_vx_eq_vy(state_t state) {
  if (state.V[vx] == state.V[vy]) {
    state.pc += 2;
    printf("skip_if_vx_eq_vy V[%X] (0x%02X) == V[%X] (0x%02X), skipping\n", vx,
           state.V[vx], vy, state.V[vy]);
  } else {
    printf("skip_if_vx_eq_vy V[%X] (0x%02X) != V[%X] (0x%02X), not skipping\n",
           vx, state.V[vx], vy, state.V[vy]);
  }
  return state;
}

state_t skip_if_vx_ne_vy(state_t state) {
  if (state.V[vx] != state.V[vy]) {
    state.pc += 2;
    printf("skip_if_vx_ne_vy V[%X] (0x%02X) != V[%X] (0x%02X), skipping\n", vx,
           state.V[vx], vy, state.V[vy]);
  } else {
    printf("skip_if_vx_ne_vy V[%X] (0x%02X) == V[%X] (0x%02X), not skipping\n",
           vx, state.V[vx], vy, state.V[vy]);
  }
  return state;
}

state_t vx_rand_and_nn(state_t state) {
  uint8_t random_value = rand() % 256;

  state.V[vx] = random_value & nn;
  printf("V[%X] = rand (0x%02X) AND 0x%02X : 0x%02X\n", vx, random_value, nn,
         state.V[vx]);

  return state;
}

state_t jump_nnn_plus_v0(state_t state) {
  state.pc = nnn + state.V[0];
  printf("jump to 0x%03X + V[0] (0x%02X) : 0x%03X\n", nnn, state.V[0],
         state.pc);
  return state;
}

state_t wait_for_kp_s_vx(state_t state) {
  pthread_mutex_lock(&key_mutex);
  printf("wait_for_kp_s_vx\n");

  while (key_pressed == 0xFF) {
    pthread_cond_wait(&key_cond, &key_mutex);
  }

  state.V[VX] = key_pressed;
  printf("wait_for_kp_s_vx key: 0x%02X\n", key_pressed);
  key_pressed = 0xFF;
  pthread_mutex_unlock(&key_mutex);

  return state;
}

state_t sub_dispatch(state_t state, function_t sub_map[]) {
  printf("-> sub_dispatch for instruction 0x%04X (opcode 0x%01X, sub_opcode "
         "0x%02X)\n",
         instruction, opcode, sub_opcode);
  if (sub_map[sub_opcode]) {
    state = sub_map[sub_opcode](state);
  } else {
    state = unknown_instruction(state);
  }
  return state;
}

static function_t sub_instruction_map_0[] = {
    [0xE] = return_from_subroutine,
};

static function_t sub_instruction_map_8[] = {
    [0x0] = set_vx_vy,      [0x1] = vx_or_vy,           [0x2] = vx_and_vy,
    [0x3] = vx_xor_vy,      [0x4] = vx_add_vy,          [0x5] = vx_sub_vy,
    [0x6] = vx_shift_right, [0x7] = vx_set_vy_minus_vx, [0xE] = vx_shift_left};

static function_t sub_instruction_map_e[] = {0};

static function_t sub_instruction_map_f[] = {
    [0x0A] = wait_for_kp_s_vx,
};

state_t dispatch_0(state_t state) {
  return sub_dispatch(state, sub_instruction_map_0);
}

state_t dispatch_8(state_t state) {
  return sub_dispatch(state, sub_instruction_map_8);
}

state_t dispatch_e(state_t state) {
  return sub_dispatch(state, sub_instruction_map_e);
}

state_t dispatch_f(state_t state) {
  return sub_dispatch(state, sub_instruction_map_f);
}

static function_t instruction_map[] = {
    [0x0] = dispatch_0,        [0x1] = jump_to_address,
    [0x2] = call_subroutine,   [0xE] = return_from_subroutine,
    [0x6] = set_vx_nn,         [0xA] = set_index_register,
    [0x7] = add_nn_to_vx,      [0x8] = dispatch_8,
    [0x4] = skip_if_vx_not_nn, [0x3] = skip_if_vx_eq_nn,
    [0x5] = skip_if_vx_eq_vy,  [0x9] = skip_if_vx_ne_vy,
    [0xB] = jump_nnn_plus_v0,  [0xC] = vx_rand_and_nn,
    [0xE] = dispatch_e,        [0xF] = dispatch_f};

void run_vm(state_t state) {
  while (state.pc < 4096) {

    instruction = (state.memory[state.pc] << 8) | state.memory[state.pc + 1];
    opcode = (instruction & 0xF000) >> 12;
    sub_opcode = instruction & 0x000F;
    vx = (instruction & 0x0F00) >> 8;
    vy = (instruction & 0x00F0) >> 4;
    nn = instruction & 0x00FF;
    nnn = instruction & 0x0FFF;

    state.pc += 2;

    if (instruction_map[opcode]) {
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
      0x60, 0x01, // 6001: Set V0 to 0x01
      0x61, 0x02, // 6102: Set V1 to 0x02
      0x62, 0x03, // 6203: Set V2 to 0x03
      0x63, 0x05, // 6305: Set V3 to 0x05
      0xA3, 0x00, // A300: Set I to 0x300
      0xC3, 0x0F, // C30F: V3 = random number AND 0x0F
      0x73, 0x02, // 7302: Add 0x02 to V3
      0x83, 0x14, // 8314: V3 += V1 (with carry)
      0x83, 0x15, // 8315: V3 -= V1 (with borrow)
      0x83, 0x16, // 8316: V3 >>= 1, VF = old LSB
      0x83, 0x17, // 8317: V3 = V1 - V3, VF = 0 on borrow
      0x83, 0x1E, // 831E: V3 <<= 1, VF = old MSB
      0x43, 0x04, // 4304: Skip next instruction if V3 == 0x04
      0x12, 0x1C, // 121C: Jump to address 0x21C (skip subroutine call)
      0x22, 0x20, // 2220: Call subroutine at 0x220
      0x12, 0x00, // 1200: Jump to 0x200 (infinite loop)

      // Subroutine at 0x220
      0x62, 0x04, // 6204: Set V2 to 0x04
      0x52, 0x03, // 5203: Skip next instruction if V2 == V3
      0x00, 0xEE, // 00EE: Return from subroutine

      // Address 0x21C (skip point)
      0x70, 0x01, // 7001: Increment V0 by 1
      0x12, 0x00  // 1200: Jump to 0x200 (back to start of the main loop)
  };

  for (int i = 0; i < sizeof(program); i++) {
    s.memory[0x200 + i] = program[i];
  }

  run_vm(s);

  return 0;
}