#include "gui.h"

void keyboard_callback(struct mfb_window *window, mfb_key key, mfb_key_mod mod,
                       bool is_pressed) {
  if (is_pressed) {
    if (key == KB_KEY_ESCAPE) {
      printf("escape key pressed. exiting...\n");
      mfb_close(window);
    } else if (key == KB_KEY_SPACE) {
      printf("spacebar pressed!\n");
    }
  }
}

void mouse_callback(struct mfb_window *window, mfb_mouse_button button,
                    mfb_key_mod mod, bool is_pressed) {
  if (is_pressed) {
    int x = mfb_get_mouse_x(window);
    int y = mfb_get_mouse_y(window);

    if (button == MOUSE_LEFT) {
      printf("left mouse button clicked at (%d, %d)\n", x, y);
    }
  }
}

uint32_t buffer[WINDOW_W * WINDOW_H] = {0};
uint8_t chip8_display[CHIP8_W * CHIP8_H] = {0};

void draw_chip8_display(uint32_t *buffer) {
  int display_width = CHIP8_W * PIXEL_SIZE + 2 * BORDER_SIZE;
  int display_height = CHIP8_H * PIXEL_SIZE + 2 * BORDER_SIZE;
  int offset_x = (WINDOW_W - display_width) / 2;
  int offset_y = (WINDOW_H - display_height) / 2;

  for (int y = 0; y < BORDER_SIZE; y++) {
    for (int x = 0; x < display_width; x++) {
      buffer[(y + offset_y) * WINDOW_W + (x + offset_x)] = BORDER_COLOR;
    }
  }

  for (int y = display_height - BORDER_SIZE; y < display_height; y++) {
    for (int x = 0; x < display_width; x++) {
      buffer[(y + offset_y) * WINDOW_W + (x + offset_x)] = BORDER_COLOR;
    }
  }

  for (int y = 0; y < display_height; y++) {
    for (int x = 0; x < BORDER_SIZE; x++) {
      buffer[(y + offset_y) * WINDOW_W + (x + offset_x)] = BORDER_COLOR;
      buffer[(y + offset_y) * WINDOW_W +
             (x + offset_x + display_width - BORDER_SIZE)] = BORDER_COLOR;
    }
  }

  for (int y = 0; y < CHIP8_H; y++) {
    for (int x = 0; x < CHIP8_W; x++) {
      int buffer_x = x * PIXEL_SIZE + offset_x + BORDER_SIZE;
      int buffer_y = y * PIXEL_SIZE + offset_y + BORDER_SIZE;
      uint32_t color = chip8_display[y * CHIP8_W + x] ? 0xFFFFFFFF : 0x00000000;

      for (int i = 0; i < PIXEL_SIZE; i++) {
        for (int j = 0; j < PIXEL_SIZE; j++) {
          if ((buffer_y + j) < WINDOW_H && (buffer_x + i) < WINDOW_W) {
            buffer[(buffer_y + j) * WINDOW_W + (buffer_x + i)] = color;
          }
        }
      }
    }
  }
}

const uint8_t ASCII_0[] = {0x1F, 0x11, 0x11, 0x11, 0x1F};
const uint8_t ASCII_1[] = {0x04, 0x0C, 0x04, 0x04, 0x0E};
const uint8_t ASCII_2[] = {0x1E, 0x01, 0x1E, 0x10, 0x1F};
const uint8_t ASCII_3[] = {0x1F, 0x01, 0x1E, 0x01, 0x1F};
const uint8_t ASCII_4[] = {0x11, 0x11, 0x1F, 0x01, 0x01};
const uint8_t ASCII_5[] = {0x1F, 0x10, 0x1E, 0x01, 0x1F};
const uint8_t ASCII_6[] = {0x1F, 0x10, 0x1F, 0x11, 0x1F};
const uint8_t ASCII_7[] = {0x1F, 0x01, 0x02, 0x04, 0x08};
const uint8_t ASCII_8[] = {0x1F, 0x11, 0x1F, 0x11, 0x1F};
const uint8_t ASCII_9[] = {0x1F, 0x11, 0x1F, 0x01, 0x1F};
const uint8_t ASCII_A[] = {0x1F, 0x11, 0x1F, 0x11, 0x11};
const uint8_t ASCII_B[] = {0x1E, 0x11, 0x1E, 0x11, 0x1E};
const uint8_t ASCII_C[] = {0x1F, 0x10, 0x10, 0x10, 0x1F};
const uint8_t ASCII_D[] = {0x1E, 0x11, 0x11, 0x11, 0x1E};
const uint8_t ASCII_E[] = {0x1F, 0x10, 0x1F, 0x10, 0x1F};
const uint8_t ASCII_F[] = {0x1F, 0x10, 0x1F, 0x10, 0x10};