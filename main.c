#include "gui.h"
#include "vm.h"

uint8_t g_display[64 * 32] = {0};

#define MEMORY_START 0x200
#define MEMORY_SIZE 4096

void load_rom(const char *filename, state_t *state) {
  FILE *rom = fopen(filename, "rb");
  if (rom == NULL) {
    fprintf(stderr, "failed to open ROM file: %s\n", filename);
    exit(1);
  }

  fseek(rom, 0, SEEK_END);
  long rom_size = ftell(rom);
  fseek(rom, 0, SEEK_SET);

  if (rom_size > MEMORY_SIZE - MEMORY_START) {
    fprintf(stderr, "ROM file is too large to fit in memory\n");
    fclose(rom);
    exit(1);
  }

  fread(state->memory + MEMORY_START, 1, rom_size, rom);
  fclose(rom);

  printf("loaded ROM: %s (%ld bytes)\n", filename, rom_size);
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <ROM file>\n", argv[0]);
    return 1;
  }
  srand((unsigned int)time(NULL));
  pthread_t tk_thread;

  if (pthread_create(&tk_thread, NULL, tk_thread_main, NULL)) {
    fprintf(stderr, "error creating tk thread\n");
    return 1;
  }

  state_t s = {.display = g_display};
  s.pc = 0x200;

  load_rom(argv[1], &s);

  run_vm(s);

  return 0;
}