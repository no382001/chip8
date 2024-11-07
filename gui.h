#pragma once
#include "MiniFB.h"
#include "vm.h"

#define CHIP8_W 64
#define CHIP8_H 32
#define KEYBOARD_ROWS 4
#define KEYBOARD_COLS 4
#define KEY_SIZE 20
#define PIXEL_SIZE 5

#define WINDOW_W 400
#define WINDOW_H WINDOW_W

#define BORDER_SIZE 2
#define BORDER_COLOR 0xFF888888

void keyboard_callback(struct mfb_window *window, mfb_key key, mfb_key_mod mod,
                       bool is_pressed);
void mouse_callback(struct mfb_window *window, mfb_mouse_button button,
                    mfb_key_mod mod, bool is_pressed);

void draw_chip8_display(uint32_t *buffer);