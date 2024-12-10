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

  long bytes_read = fread(state->memory + MEMORY_START, 1, rom_size, rom);

  if (bytes_read != rom_size) {
    fprintf(stderr, "error reading rom file\n");
    fclose(rom);
    exit(1);
  }
  fclose(rom);

  printf("loaded rom: %s (%ld bytes)\n", filename, rom_size);
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

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <rom file>\n", argv[0]);
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

  load_rom(argv[1], &s);
  // clock_t start_time, end_time = {0};
  // double frame_time = {0};
  do {
    // start_time = clock();

    mfb_update_state state = mfb_update(window, buffer);
    if (state != STATE_OK)
      break;

    vm_cycle(&s);
    draw_chip8_display(buffer);
    draw_debug_info(buffer, &s);

    // end_time = clock();
    // frame_time = ((double)(end_time - start_time) / CLOCKS_PER_SEC) *
    // 1000.0; printf("frame time: %.2f ms\n", frame_time);

  } while (1 /*mfb_wait_sync(window)*/);

  return 0;
}