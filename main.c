#include "gui.h"
#include "vm.h"

uint8_t g_display[64 * 32] = {0};

int main() {
  srand((unsigned int)time(NULL));
  pthread_t tk_thread;

  if (pthread_create(&tk_thread, NULL, tk_thread_main, NULL)) {
    fprintf(stderr, "error creating tk thread\n");
    return 1;
  }

  state_t s = {.display = g_display};
  s.pc = 0x200;

  uint8_t program[] = {
      0x60, 0x01, // 6001: Set V0 to 0x01
      0x61, 0x02, // 6102: Set V1 to 0x02
      0x62, 0x03, // 6203: Set V2 to 0x03
      0x63, 0x05, // 6305: Set V3 to 0x05
      0xA3, 0x00, // A300: Set I to 0x300
      0xC3, 0x0F, // C30F: V3 = random number AND 0x0F
      0x73, 0x02, // 7302: Add 0x02 to V3
      0x83, 0x14, // 8314: V3 += V1 (with carry)
      0x83, 0x15, // 8315: V3 -= V1 (with borrow)
      0x83, 0x16, // 8316: V3 >>= 1, VF = old LSB
      0x83, 0x17, // 8317: V3 = V1 - V3, VF = 0 on borrow
      0x83, 0x1E, // 831E: V3 <<= 1, VF = old MSB
      0x43, 0x04, // 4304: Skip next instruction if V3 == 0x04
      0x12, 0x1C, // 121C: Jump to address 0x21C (skip subroutine call)
      0x22, 0x20, // 2220: Call subroutine at 0x220
      0x12, 0x00, // 1200: Jump to 0x200 (infinite loop)

      // Subroutine at 0x220
      0x62, 0x04, // 6204: Set V2 to 0x04
      0x52, 0x03, // 5203: Skip next instruction if V2 == V3
      0x00, 0xEE, // 00EE: Return from subroutine

      // Address 0x21C (skip point)
      0x70, 0x01, // 7001: Increment V0 by 1
      0x12, 0x00  // 1200: Jump to 0x200 (back to start of the main loop)
  };

  for (int i = 0; i < sizeof(program); i++) {
    s.memory[0x200 + i] = program[i];
  }

  run_vm(s);

  return 0;
}