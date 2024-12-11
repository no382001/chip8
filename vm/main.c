#include "MiniFB.h"
#include "gui.h"
#include "vm.h"
#include <time.h>
#include <unistd.h>

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

  long bytes_read = fread(state->memory + MEMORY_START, 1, rom_size, rom);

  if (bytes_read != rom_size) {
    fprintf(stderr, "error reading rom file\n");
    fclose(rom);
    exit(1);
  }
  fclose(rom);

  printf("loaded rom: %s (%ld bytes)\n", filename, rom_size);

  /** /
  unsigned start_addr = MEMORY_START;
  for (int i = 0; i < rom_size; i += 2) {
    unsigned addr = start_addr + i;
    uint8_t high = state->memory[addr];
    uint8_t low = 0x00;
    if (i + 1 < rom_size) {
      low = state->memory[addr + 1];
    }

    printf("0x%03X: 0x%02X 0x%02X\n", addr, high, low);
  }
  /**/
}

extern uint32_t buffer[WINDOW_W * WINDOW_H];
extern uint8_t chip8_display[CHIP8_W * CHIP8_H];

void keyboard_poll(state_t *state) {
  mfb_update(state->input.window, buffer);
  const uint8_t *keys = mfb_get_key_buffer(state->input.window);

  if (keys[KB_KEY_ESCAPE]) {
    mfb_close(state->input.window);
    return;
  }

  if (keys[KB_KEY_1])
    pressed = 0x1;
  else if (keys[KB_KEY_2])
    pressed = 0x2;
  else if (keys[KB_KEY_3])
    pressed = 0x3;
  else if (keys[KB_KEY_4])
    pressed = 0xC;
  else if (keys[KB_KEY_Q])
    pressed = 0x4;
  else if (keys[KB_KEY_W])
    pressed = 0x5;
  else if (keys[KB_KEY_E])
    pressed = 0x6;
  else if (keys[KB_KEY_R])
    pressed = 0xD;
  else if (keys[KB_KEY_A])
    pressed = 0x7;
  else if (keys[KB_KEY_S])
    pressed = 0x8;
  else if (keys[KB_KEY_D])
    pressed = 0x9;
  else if (keys[KB_KEY_F])
    pressed = 0xE;
  else if (keys[KB_KEY_Z])
    pressed = 0xA;
  else if (keys[KB_KEY_X])
    pressed = 0x0;
  else if (keys[KB_KEY_C])
    pressed = 0xB;
  else if (keys[KB_KEY_V])
    pressed = 0xF;
  else
    pressed = 0xFF;
}

static bool step = false;
#define PRINT_USAGE                                                            \
  { fprintf(stderr, "Usage: %s [-s] -r <rom file>\n", argv[0]); }

int main(int argc, char *argv[]) {
  if (argc < 3) {
    PRINT_USAGE
    return 1;
  }

  const char *rom_file = NULL;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-s") == 0) {
      step = true;
    } else if (strcmp(argv[i], "-r") == 0) {
      if (i + 1 < argc) {
        rom_file = argv[i + 1];
        i++;
      } else {
        fprintf(stderr, "error: -r option requires an argument\n");
        return 1;
      }
    } else {
      fprintf(stderr, "unknown argument: %s\n", argv[i]);
      PRINT_USAGE
      return 1;
    }
  }

  if (rom_file == NULL) {
    fprintf(stderr, "rrror: no ROM file specified\n");
    PRINT_USAGE
    return 1;
  }

  srand((unsigned int)time(NULL));

  struct mfb_window *window =
      mfb_open_ex("chip8", WINDOW_W, WINDOW_H, WF_RESIZABLE);

  if (!window) {
    printf("failed to create window...\n");
    return -1;
  }

  state_t s = {.display = chip8_display,
               .input = {0xFF, keyboard_poll, window}};
  s.pc = MEMORY_START;

  load_rom(rom_file, &s);
  prev_state_t prev_state = {0};
  do {
    mfb_update_state state = mfb_update(window, buffer);
    if (state != STATE_OK)
      break;

    vm_cycle(&s);
    draw_chip8_display(buffer);
    draw_debug_info(buffer, &s, &prev_state);

    if (step) {

      bool key_pressed = false;
      while (!key_pressed) {
        usleep(100000);
        mfb_update_state step_state = mfb_update(window, buffer);
        if (step_state != STATE_OK)
          break;

        const uint8_t *keys = mfb_get_key_buffer(window);
        if (keys) {
          for (int k = 0; k < KB_KEY_LAST; k++) {
            if (keys[k]) {
              key_pressed = true;
              break;
            }
          }
        }
      }
    }
  } while (1);

  return 0;
}