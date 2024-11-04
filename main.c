#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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

    if (opcode == 0x0) {
      uint8_t sub_opcode = instruction & 0x000F;
      if (sub_opcode == 0xE) {
        state = return_from_subroutine(state);
      } else {
        state = unknown_instruction(state);
      }
    } else if (opcode == 0x8) {
      uint8_t sub_opcode = instruction & 0x000F;
      state = sub_dispatch(state, instruction_8xy_map, sub_opcode);
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

int main() {
  srand((unsigned int)time(NULL));

  state_t s = {0};
  s.pc = 0x200;

  uint8_t program[] = {
      0x60, 0x01, // 6001: set v0 to 0x01
      0x61, 0x02, // 6102: set v1 to 0x02
      0x62, 0x03, // 6203: set v2 to 0x03
      0x63, 0x05, // 6305: set v3 to 0x05
      0xa3, 0x00, // a300: set i to 0x300
      0xc3, 0x0f, // c30f: v3 = rand and 0x0f
      0x73, 0x02, // 7302: add 0x02 to v3
      0x83, 0x14, // 8314: v3 += v1 (with carry)
      0x83, 0x15, // 8315: v3 -= v1 (with borrow)
      0x83, 0x16, // 8316: v3 >>= 1, vf = old lsb
      0x83, 0x17, // 8317: v3 = v1 - v3, vf = 0 on borrow
      0x83, 0x1e, // 831e: v3 <<= 1, vf = old msb
      0x43, 0x04, // 4304: skip next instruction if v3 == 0x04
      0x12, 0x1c, // 121c: jump to address 0x21c (skip subroutine call)
      0x22, 0x20, // 2220: call subroutine at 0x220
      0x12, 0x00, // 1200: jump to 0x200 (infinite loop)

      // subroutine at 0x220
      0x62, 0x04, // 6204: set v2 to 0x04
      0x52, 0x03, // 5203: skip next instruction if v2 == v3
      0x00, 0xee, // 00ee: return from subroutine

      // address 0x21c (skip point)
      0x70, 0x01, // 7001: increment v0 by 1
      0x12, 0x00  // 1200: jump to 0x200 (back to start of the main loop)
  };

  for (int i = 0; i < sizeof(program); i++) {
    s.memory[0x200 + i] = program[i];
  }

  run_vm(s);

  return 0;
}