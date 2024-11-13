#pragma once
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "MiniFB.h"

typedef struct state_t state_t;
typedef void (*function_t)(state_t *);

typedef struct {
  uint16_t instruction;
  uint8_t opcode; // 0xF000
  uint8_t n;      // 0x000F
  uint8_t nn;     // 0x00FF
  uint16_t nnn;   // 0x0FFF
  uint8_t vx;     // 0x0F00
  uint8_t vy;     // 0x00F0
} curr_op_t;

typedef void (*keyboard_cb_t)(state_t *state);

typedef struct {
  uint8_t key_pressed;
  keyboard_cb_t k_cb;
  struct mfb_window *window;
} input_t;

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
  input_t input;
};

void jump_to_address(state_t *state);
void return_from_subroutine(state_t *state);
void clear_display(state_t *state);
void call_subroutine(state_t *state);
void set_vx_nn(state_t *state);
void set_vx_vy(state_t *state);
void vx_or_vy(state_t *state);
void vx_and_vy(state_t *state);
void vx_xor_vy(state_t *state);
void vx_add_vy(state_t *state);
void vx_sub_vy(state_t *state);
void vx_shift_right(state_t *state);
void vx_set_vy_minus_vx(state_t *state);
void vx_shift_left(state_t *state);
void add_nn_to_vx(state_t *state);
void unknown_instruction(state_t *state);
void set_index_register(state_t *state);
void skip_if_vx_not_nn(state_t *state);
void skip_if_vx_eq_nn(state_t *state);
void skip_if_vx_eq_vy(state_t *state);
void skip_if_vx_ne_vy(state_t *state);
void vx_rand_and_nn(state_t *state);
void jump_nnn_plus_v0(state_t *state);
void wait_for_kp_save_to_vx(state_t *state);
void kp_skip_if_vx(state_t *state);
void kp_skip_if_not_vx(state_t *state);
void store_bcd_vx(state_t *state);
void save_v0_vx(state_t *state);
void load_v0_vx(state_t *state);
void set_vx_to_delay_timer(state_t *state);
void set_delay_timer_to_vx(state_t *state);
void set_sound_timer_to_vx(state_t *state);
void add_vx_to_i(state_t *state);
void draw_sprite(state_t *state);
void set_i_to_sprite_location(state_t *state);

void sub_dispatch(state_t *state, uint8_t mask, function_t sub_map[]);
void dispatch_0(state_t *state);
void dispatch_8(state_t *state);
void dispatch_e(state_t *state);
void dispatch_f(state_t *state);

void vm_cycle(state_t *state);
#define pressed state->input.key_pressed