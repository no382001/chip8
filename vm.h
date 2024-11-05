#pragma once
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct state_t state_t;
typedef state_t (*function_t)(state_t);

typedef struct {
  uint16_t instruction;
  uint8_t opcode; // 0xF000
  uint8_t n;      // 0x000F
  uint8_t nn;     // 0x00FF
  uint16_t nnn;   // 0x0FFF
  uint8_t vx;     // 0x0F00
  uint8_t vy;     // 0x00F0
} curr_op_t;

struct state_t {
  uint8_t memory[4096];
  uint8_t V[16]; // (V0 ... VF)
  uint16_t I;
  uint16_t pc;
  uint8_t *display;
  uint16_t stack[16];
  uint16_t sp;
  curr_op_t cop;
  uint8_t delay_timer;
  uint8_t sound_timer;
};

state_t jump_to_address(state_t state);
state_t return_from_subroutine(state_t state);
state_t clear_display(state_t state);
state_t call_subroutine(state_t state);
state_t set_vx_nn(state_t state);
state_t set_vx_vy(state_t state);
state_t vx_or_vy(state_t state);
state_t vx_and_vy(state_t state);
state_t vx_xor_vy(state_t state);
state_t vx_add_vy(state_t state);
state_t vx_sub_vy(state_t state);
state_t vx_shift_right(state_t state);
state_t vx_set_vy_minus_vx(state_t state);
state_t vx_shift_left(state_t state);
state_t add_nn_to_vx(state_t state);
state_t unknown_instruction(state_t state);
state_t set_index_register(state_t state);
state_t skip_if_vx_not_nn(state_t state);
state_t skip_if_vx_eq_nn(state_t state);
state_t skip_if_vx_eq_vy(state_t state);
state_t skip_if_vx_ne_vy(state_t state);
state_t vx_rand_and_nn(state_t state);
state_t jump_nnn_plus_v0(state_t state);
state_t wait_for_kp_save_to_vx(state_t state);
state_t kp_skip_if_vx(state_t state);
state_t kp_skip_if_not_vx(state_t state);
state_t store_bcd_vx(state_t state);
state_t save_v0_vx(state_t state);
state_t load_v0_vx(state_t state);
state_t set_vx_to_delay_timer(state_t state);
state_t set_delay_timer_to_vx(state_t state);
state_t set_sound_timer_to_vx(state_t state);
state_t add_vx_to_i(state_t state);
state_t draw_sprite(state_t state);

state_t sub_dispatch(state_t state, function_t sub_map[]);
state_t dispatch_0(state_t state);
state_t dispatch_8(state_t state);
state_t dispatch_e(state_t state);
state_t dispatch_f(state_t state);

void run_vm(state_t state);