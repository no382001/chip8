#include "MiniFB.h"
#include "gui.h"
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

  if (!window) {
    printf("failed to create window...\n");
    return -1;
  }

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