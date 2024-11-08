#include "MiniFB.h"
#include "gui.h"
#include "time.h"
#include "vm.h"

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

extern uint32_t buffer[WINDOW_W * WINDOW_H];
extern uint8_t chip8_display[CHIP8_W * CHIP8_H];

static uint8_t key_pressed = 0xFF;
void keyboard_callback(struct mfb_window *window, mfb_key key, mfb_key_mod mod,
                       bool is_pressed) {
  if (is_pressed && key_pressed == 0xFF) {
    switch (key) {
    case KB_KEY_ESCAPE:
      mfb_close(window);
      break;
    case KB_KEY_1:
      key_pressed = 0x1;
      break;
    case KB_KEY_2:
      key_pressed = 0x2;
      break;
    case KB_KEY_3:
      key_pressed = 0x3;
      break;
    case KB_KEY_4:
      key_pressed = 0xC;
      break;
    case KB_KEY_Q:
      key_pressed = 0x4;
      break;
    case KB_KEY_W:
      key_pressed = 0x5;
      break;
    case KB_KEY_E:
      key_pressed = 0x6;
      break;
    case KB_KEY_R:
      key_pressed = 0xD;
      break;
    case KB_KEY_A:
      key_pressed = 0x7;
      break;
    case KB_KEY_S:
      key_pressed = 0x8;
      break;
    case KB_KEY_D:
      key_pressed = 0x9;
      break;
    case KB_KEY_F:
      key_pressed = 0xE;
      break;
    case KB_KEY_Z:
      key_pressed = 0xA;
      break;
    case KB_KEY_X:
      key_pressed = 0x0;
      break;
    case KB_KEY_C:
      key_pressed = 0xB;
      break;
    case KB_KEY_V:
      key_pressed = 0xF;
      break;
    default:
      break;
    }
  } else {
    key_pressed = 0xFF;
  }
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <rom file>\n", argv[0]);
    return 1;
  }

  srand((unsigned int)time(NULL));

  state_t s = {.display = chip8_display, .key_pressed = &key_pressed};
  s.pc = MEMORY_START;

  load_rom(argv[1], &s);

  struct mfb_window *window =
      mfb_open_ex("chip8", WINDOW_W, WINDOW_H, WF_RESIZABLE);

  if (!window) {
    printf("failed to create window...\n");
    return -1;
  }

  mfb_set_keyboard_callback(window, keyboard_callback);

  // clock_t start_time, end_time = {0};
  // double frame_time = {0};
  do {
    // start_time = clock();

    mfb_update_state state = mfb_update(window, buffer);
    if (state != STATE_OK)
      break;

    vm_cycle(&s);
    draw_chip8_display(buffer);

    // end_time = clock();
    // frame_time = ((double)(end_time - start_time) / CLOCKS_PER_SEC) *
    // 1000.0; printf("frame time: %.2f ms\n", frame_time);

  } while (1 /*mfb_wait_sync(window)*/);

  return 0;
}