#include "MiniFB.h"
#include "gui.h"
#include "vm.h"

uint8_t g_display[64 * 32] = {0};

#define MEMORY_START 0x200
#define MEMORY_SIZE 4096

void load_rom(const char *filename, state_t *state) {
  FILE *rom = fopen(filename, "rb");
  if (rom == NULL) {
    fprintf(stderr, "failed to open rom file: %s\n", filename);
    exit(1);
  }

  fseek(rom, 0, SEEK_END);
  long rom_size = ftell(rom);
  fseek(rom, 0, SEEK_SET);

  if (rom_size > MEMORY_SIZE - MEMORY_START) {
    fprintf(stderr, "rom file is too large to fit in memory\n");
    fclose(rom);
    exit(1);
  }

  fread(state->memory + MEMORY_START, 1, rom_size, rom);
  fclose(rom);

  printf("loaded rom: %s (%ld bytes)\n", filename, rom_size);
}

#define CHIP8_W 64
#define CHIP8_H 32
#define KEYBOARD_ROWS 4
#define KEYBOARD_COLS 4
#define KEY_SIZE 20
#define PIXEL_SIZE 5

#define WINDOW_W 400
#define WINDOW_H WINDOW_W

static uint32_t buffer[WINDOW_W * WINDOW_H] = {0};

static uint8_t chip8_display[CHIP8_W * CHIP8_H] = {0};

#define BORDER_SIZE 2
#define BORDER_COLOR 0xFF888888

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

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <rom file>\n", argv[0]);
    return 1;
  }

  srand((unsigned int)time(NULL));

  state_t s = {.display = chip8_display, .key_pressed = 0xFF};
  s.pc = MEMORY_START;

  load_rom(argv[1], &s);

  struct mfb_window *window =
      mfb_open_ex("chip8", WINDOW_W, WINDOW_H, WF_RESIZABLE);

  if (!window)
    return -1;

  mfb_set_keyboard_callback(window, keyboard_callback);
  mfb_set_mouse_button_callback(window, mouse_callback);

  do {

    mfb_update_state state = mfb_update(window, buffer);
    if (state != STATE_OK)
      break;

    vm_cycle(&s);
    draw_chip8_display(buffer);

  } while (mfb_wait_sync(window));

  return 0;
}