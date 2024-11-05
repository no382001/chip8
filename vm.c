#include "vm.h"

#include <pthread.h>
#include <tcl.h>
#include <tk.h>

pthread_mutex_t key_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t key_cond = PTHREAD_COND_INITIALIZER;
volatile uint8_t key_pressed = 0xFF;

pthread_mutex_t display_mutex = PTHREAD_MUTEX_INITIALIZER;
uint8_t display_dirty = 1;

//#define printf(x, ...)

/*******************************
 *   instruction implementations
 */

#define instruction state.cop.instruction
#define opcode state.cop.opcode
#define vx state.cop.vx
#define vy state.cop.vy
#define n state.cop.n
#define nn state.cop.nn
#define nnn state.cop.nnn

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

state_t clear_display(state_t state) {
  printf("clear_display\n");
  for (uint8_t i = 0; i < 64 * 32; i++) {
    state.display[i] = 0;
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
  printf("-> unknown_instruction 0x%04X (opcode 0x%01X, n "
         "0x%02X)\n",
         instruction, opcode, n);
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

state_t wait_for_kp_save_to_vx(state_t state) {
  pthread_mutex_lock(&key_mutex);
  printf("wait_for_kp_save_to_vx\n");

  while (key_pressed == 0xFF) {
    pthread_cond_wait(&key_cond, &key_mutex);
  }

  state.V[vx] = key_pressed;
  printf("wait_for_kp_save_to_vx key: 0x%02X\n", key_pressed);
  key_pressed = 0xFF;
  pthread_mutex_unlock(&key_mutex);

  return state;
}

state_t kp_skip_if_vx(state_t state) {
  if (key_pressed == state.V[vx]) {
    state.pc += 2;
    printf("kp_skip_if_vx: V[%X] (0x%02X) skipping\n", vx, state.V[vx]);
  } else {
    printf("kp_skip_if_vx: V[%X] (0x%02X) not skipping\n", vx, state.V[vx]);
  }

  return state;
}

state_t kp_skip_if_not_vx(state_t state) {
  if (key_pressed != state.V[vx]) {
    state.pc += 2;
    printf("kp_skip_if_not_vx: V[%X] (0x%02X) skipping\n", vx, state.V[vx]);
  } else {
    printf("kp_skip_if_not_vx: V[%X] (0x%02X) not skipping\n", vx, state.V[vx]);
  }

  return state;
}

state_t store_bcd_vx(state_t state) {
  uint8_t value = state.V[vx];
  state.memory[state.I] = value / 100;
  state.memory[state.I + 1] = (value / 10) % 10;
  state.memory[state.I + 2] = value % 10;
  printf("store_bcd_vx V[%X] = %d as [%d, %d, %d] at I (0x%03X)\n", vx, value,
         state.memory[state.I], state.memory[state.I + 1],
         state.memory[state.I + 2], state.I);
  return state;
}

state_t save_v0_vx(state_t state) {
  for (uint8_t i = 0; i <= vx; i++) {
    state.memory[state.I + i] = state.V[i];
    printf("save_v0_vx V[%X] = 0x%02X at 0x%03X\n", i, state.V[i], state.I + i);
  }
  return state;
}

state_t load_v0_vx(state_t state) {
  for (uint8_t i = 0; i <= vx; i++) {
    state.V[i] = state.memory[state.I + i];
    printf("load_v0_vx [0x%03X] = 0x%02X into V[%X]\n", state.I + i, state.V[i],
           i);
  }
  return state;
}

state_t set_vx_to_delay_timer(state_t state) {
  state.V[vx] = state.delay_timer;
  printf("set_vx_to_delay_timer: V[%X] to 0x%02X\n", vx, state.delay_timer);
  return state;
}

state_t set_delay_timer_to_vx(state_t state) {
  state.delay_timer = state.V[vx];
  printf("set_delay_timer_to_vx: V[%X] = 0x%02X\n", vx, state.V[vx]);
  return state;
}

state_t set_sound_timer_to_vx(state_t state) {
  state.sound_timer = state.V[vx];
  printf("set_sound_timer_to_vx: V[%X] = 0x%02X\n", vx, state.V[vx]);
  return state;
}

state_t add_vx_to_i(state_t state) {
  state.I += state.V[vx];
  printf("add_vx_to_i:  V[%X] + (0x%02X) = 0x%03X\n", vx, state.V[vx], state.I);
  return state;
}

state_t draw_sprite(state_t state) {
  uint8_t x = state.V[vx] % 64;
  uint8_t y = state.V[vy] % 32;
  uint8_t height = n;
  uint8_t pixel = {0};

  state.V[0xF] = 0;

  for (uint8_t row = 0; row < height; row++) {
    pixel = state.memory[state.I + row];
    for (uint8_t col = 0; col < 8; col++) {
      if (pixel & (0x80 >> col)) {
        int display_index = x + col + ((y + row) * 64);

        if (state.display[display_index] == 1) {
          state.V[0xF] = 1;
        }

        state.display[display_index] ^= 1;
      }
    }
  }

  printf("draw_sprite (V[%X] = %d, V[%X] = %d), h: %d\n", vx, x, vy, y, height);
  // pthread_mutex_lock(&display_mutex);
  display_dirty = 1;
  // pthread_mutex_unlock(&display_mutex);
  return state;
}

state_t set_i_to_sprite_location(state_t state) {
  state.I = 0x050 + (state.V[vx] * 5);
  printf("I = hex sprite for V[%X] (0x%02X), I set to: 0x%03X\n", vx,
         state.V[vx], state.I);
  return state;
}

/*******************************
 *   instruction maps
 */

static function_t sub_instruction_map_0[] = {
    [0x0] = clear_display,
    [0xE] = return_from_subroutine,
};

static function_t sub_instruction_map_8[] = {
    [0x0] = set_vx_vy,      [0x1] = vx_or_vy,           [0x2] = vx_and_vy,
    [0x3] = vx_xor_vy,      [0x4] = vx_add_vy,          [0x5] = vx_sub_vy,
    [0x6] = vx_shift_right, [0x7] = vx_set_vy_minus_vx, [0xE] = vx_shift_left};

static function_t sub_instruction_map_e[] = {
    [0x9E] = kp_skip_if_vx, [0xA1] = kp_skip_if_not_vx};

static function_t sub_instruction_map_f[] = {[0x07] = set_vx_to_delay_timer,
                                             [0x15] = set_delay_timer_to_vx,
                                             [0x18] = set_sound_timer_to_vx,
                                             [0x1E] = add_vx_to_i,
                                             [0x0A] = wait_for_kp_save_to_vx,
                                             [0x33] = store_bcd_vx,
                                             [0x55] = save_v0_vx,
                                             [0x65] = load_v0_vx,
                                             [0x9] = set_i_to_sprite_location};

static function_t instruction_map[] = {
    [0x0] = dispatch_0,        [0x1] = jump_to_address,
    [0x2] = call_subroutine,   [0xE] = return_from_subroutine,
    [0x6] = set_vx_nn,         [0xA] = set_index_register,
    [0x7] = add_nn_to_vx,      [0x8] = dispatch_8,
    [0x4] = skip_if_vx_not_nn, [0x3] = skip_if_vx_eq_nn,
    [0x5] = skip_if_vx_eq_vy,  [0x9] = skip_if_vx_ne_vy,
    [0xB] = jump_nnn_plus_v0,  [0xC] = vx_rand_and_nn,
    [0xD] = draw_sprite,       [0xE] = dispatch_e,
    [0xF] = dispatch_f};

/*******************************
 *   dispatch functions
 */

state_t sub_dispatch(state_t state, function_t sub_map[]) {
  /*printf("-> sub_dispatch for instruction 0x%04X (opcode 0x%01X, n "
         "0x%02X)\n",
         instruction, opcode, n);*/
  if (sub_map[n]) {
    state = sub_map[n](state);
  } else {
    state = unknown_instruction(state);
  }
  return state;
}

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

/*******************************
 *   main loop
 */

void run_vm(state_t state) {
  while (state.pc < 4096) {

    instruction = (state.memory[state.pc] << 8) | state.memory[state.pc + 1];
    opcode = (instruction & 0xF000) >> 12;
    n = instruction & 0x000F;
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