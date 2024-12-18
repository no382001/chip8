#include "gui.h"

uint32_t buffer[WINDOW_W * WINDOW_H] = {0};
uint8_t chip8_display[CHIP8_W * CHIP8_H] = {0};

static int display_width = CHIP8_W * PIXEL_SIZE + 2 * BORDER_SIZE;
static int display_height = CHIP8_H * PIXEL_SIZE + 2 * BORDER_SIZE;

void draw_chip8_display(uint32_t *buffer) {
  int offset_x = (WINDOW_W - display_width) / 2;
  int offset_y = (WINDOW_H - display_height) / 4;

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

static const uint8_t ASCII_FONT[0x7F - 0x20][5] = {
    // 0x20 ' ' (space)
    {0x00, 0x00, 0x00, 0x00, 0x00},
    // 0x21 '!'
    {0x04, 0x04, 0x04, 0x00, 0x04},
    // 0x22 '"'
    {0x0A, 0x0A, 0x00, 0x00, 0x00},
    // 0x23 '#'
    {0x0A, 0x1F, 0x0A, 0x1F, 0x0A},
    // 0x24 '$'
    {0x04, 0x1E, 0x05, 0x1E, 0x04},
    // 0x25 '%'
    {0x19, 0x19, 0x02, 0x04, 0x13},
    // 0x26 '&'
    {0x0C, 0x12, 0x0C, 0x12, 0x1D},
    // 0x27 '''
    {0x04, 0x04, 0x00, 0x00, 0x00},
    // 0x28 '('
    {0x02, 0x04, 0x04, 0x04, 0x02},
    // 0x29 ')'
    {0x08, 0x04, 0x04, 0x04, 0x08},
    // 0x2A '*'
    {0x00, 0x0A, 0x04, 0x0A, 0x00},
    // 0x2B '+'
    {0x00, 0x04, 0x1F, 0x04, 0x00},
    // 0x2C ','
    {0x00, 0x00, 0x00, 0x04, 0x08},
    // 0x2D '-'
    {0x00, 0x00, 0x1F, 0x00, 0x00},
    // 0x2E '.'
    {0x00, 0x00, 0x00, 0x00, 0x04},
    // 0x2F '/'
    {0x01, 0x02, 0x04, 0x08, 0x10},
    // 0x30 '0'
    {0x1F, 0x11, 0x11, 0x11, 0x1F},
    // 0x31 '1'
    {0x04, 0x0C, 0x04, 0x04, 0x0E},
    // 0x32 '2'
    {0x1E, 0x01, 0x1E, 0x10, 0x1F},
    // 0x33 '3'
    {0x1F, 0x01, 0x1E, 0x01, 0x1F},
    // 0x34 '4'
    {0x11, 0x11, 0x1F, 0x01, 0x01},
    // 0x35 '5'
    {0x1F, 0x10, 0x1E, 0x01, 0x1F},
    // 0x36 '6'
    {0x1F, 0x10, 0x1F, 0x11, 0x1F},
    // 0x37 '7'
    {0x1F, 0x01, 0x02, 0x04, 0x08},
    // 0x38 '8'
    {0x1F, 0x11, 0x1F, 0x11, 0x1F},
    // 0x39 '9'
    {0x1F, 0x11, 0x1F, 0x01, 0x1F},
    // 0x3A ':'
    {0x00, 0x04, 0x00, 0x04, 0x00},
    // 0x3B ';'
    {0x00, 0x04, 0x00, 0x04, 0x08},
    // 0x3C '<'
    {0x02, 0x04, 0x08, 0x04, 0x02},
    // 0x3D '='
    {0x00, 0x1F, 0x00, 0x1F, 0x00},
    // 0x3E '>'
    {0x08, 0x04, 0x02, 0x04, 0x08},
    // 0x3F '?'
    {0x1F, 0x01, 0x06, 0x00, 0x04},
    // 0x40 '@'
    {0x1F, 0x11, 0x17, 0x10, 0x1F},
    // 0x41 'A'
    {0x1F, 0x11, 0x1F, 0x11, 0x11},
    // 0x42 'B'
    {0x1E, 0x11, 0x1E, 0x11, 0x1E},
    // 0x43 'C'
    {0x1F, 0x10, 0x10, 0x10, 0x1F},
    // 0x44 'D'
    {0x1E, 0x11, 0x11, 0x11, 0x1E},
    // 0x45 'E'
    {0x1F, 0x10, 0x1F, 0x10, 0x1F},
    // 0x46 'F'
    {0x1F, 0x10, 0x1F, 0x10, 0x10},
    // 0x47 'G'
    {0x1F, 0x10, 0x17, 0x11, 0x1F},
    // 0x48 'H'
    {0x11, 0x11, 0x1F, 0x11, 0x11},
    // 0x49 'I'
    {0x0E, 0x04, 0x04, 0x04, 0x0E},
    // 0x4A 'J'
    {0x07, 0x01, 0x01, 0x11, 0x0E},
    // 0x4B 'K'
    {0x11, 0x12, 0x1C, 0x12, 0x11},
    // 0x4C 'L'
    {0x10, 0x10, 0x10, 0x10, 0x1F},
    // 0x4D 'M'
    {0x11, 0x1B, 0x15, 0x11, 0x11},
    // 0x4E 'N'
    {0x11, 0x19, 0x15, 0x13, 0x11},
    // 0x4F 'O'
    {0x1F, 0x11, 0x11, 0x11, 0x1F},
    // 0x50 'P'
    {0x1E, 0x11, 0x1E, 0x10, 0x10},
    // 0x51 'Q'
    {0x1F, 0x11, 0x11, 0x13, 0x1F},
    // 0x52 'R'
    {0x1E, 0x11, 0x1E, 0x12, 0x11},
    // 0x53 'S'
    {0x1F, 0x10, 0x1F, 0x01, 0x1F},
    // 0x54 'T'
    {0x1F, 0x04, 0x04, 0x04, 0x04},
    // 0x55 'U'
    {0x11, 0x11, 0x11, 0x11, 0x1F},
    // 0x56 'V'
    {0x11, 0x11, 0x0A, 0x0A, 0x04},
    // 0x57 'W'
    {0x11, 0x11, 0x15, 0x1B, 0x11},
    // 0x58 'X'
    {0x11, 0x0A, 0x04, 0x0A, 0x11},
    // 0x59 'Y'
    {0x11, 0x11, 0x1F, 0x04, 0x04},
    // 0x5A 'Z'
    {0x1F, 0x02, 0x04, 0x08, 0x1F},
    // 0x5B '['
    {0x0E, 0x08, 0x08, 0x08, 0x0E},
    // 0x5C '\'
    {0x10, 0x08, 0x04, 0x02, 0x01},
    // 0x5D ']'
    {0x0E, 0x02, 0x02, 0x02, 0x0E},
    // 0x5E '^'
    {0x04, 0x0A, 0x11, 0x00, 0x00},
    // 0x5F '_'
    {0x00, 0x00, 0x00, 0x00, 0x1F},
    // 0x60 '`'
    {0x04, 0x02, 0x00, 0x00, 0x00},
    // 0x61 'a'
    {0x00, 0x0E, 0x01, 0x0F, 0x0F},
    // 0x62 'b'
    {0x10, 0x10, 0x1E, 0x11, 0x1E},
    // 0x63 'c'
    {0x00, 0x0F, 0x10, 0x10, 0x0F},
    // 0x64 'd'
    {0x01, 0x01, 0x0F, 0x11, 0x0F},
    // 0x65 'e'
    {0x0E, 0x11, 0x1F, 0x10, 0x0E},
    // 0x66 'f'
    {0x06, 0x08, 0x1C, 0x08, 0x08},
    // 0x67 'g'
    {0x0F, 0x11, 0x0F, 0x01, 0x0E},
    // 0x68 'h'
    {0x10, 0x10, 0x1E, 0x11, 0x11},
    // 0x69 'i'
    {0x04, 0x00, 0x0C, 0x04, 0x0E},
    // 0x6A 'j'
    {0x02, 0x00, 0x06, 0x02, 0x1C},
    // 0x6B 'k'
    {0x10, 0x12, 0x1C, 0x12, 0x11},
    // 0x6C 'l'
    {0x0C, 0x04, 0x04, 0x04, 0x0E},
    // 0x6D 'm'
    {0x00, 0x1B, 0x15, 0x15, 0x15},
    // 0x6E 'n'
    {0x00, 0x1E, 0x11, 0x11, 0x11},
    // 0x6F 'o'
    {0x00, 0x0E, 0x11, 0x11, 0x0E},
    // 0x70 'p'
    {0x00, 0x1E, 0x11, 0x1E, 0x10},
    // 0x71 'q'
    {0x00, 0x0F, 0x11, 0x0F, 0x01},
    // 0x72 'r'
    {0x00, 0x1A, 0x14, 0x10, 0x10},
    // 0x73 's'
    {0x00, 0x0F, 0x10, 0x0E, 0x1E},
    // 0x74 't'
    {0x08, 0x1C, 0x08, 0x08, 0x06},
    // 0x75 'u'
    {0x00, 0x11, 0x11, 0x11, 0x0F},
    // 0x76 'v'
    {0x00, 0x11, 0x11, 0x0A, 0x04},
    // 0x77 'w'
    {0x00, 0x11, 0x15, 0x1B, 0x11},
    // 0x78 'x'
    {0x00, 0x11, 0x0A, 0x0A, 0x11},
    // 0x79 'y'
    {0x00, 0x11, 0x0F, 0x01, 0x0E},
    // 0x7A 'z'
    {0x00, 0x1F, 0x02, 0x04, 0x1F},
    // 0x7B '{'
    {0x02, 0x04, 0x08, 0x04, 0x02},
    // 0x7C '|'
    {0x04, 0x04, 0x04, 0x04, 0x04},
    // 0x7D '}'
    {0x08, 0x04, 0x02, 0x04, 0x08},
    // 0x7E '~'
    {0x00, 0x09, 0x15, 0x12, 0x00}};

static const uint8_t ASCII_UNKNOWN[5] = {0x1F, 0x11, 0x15, 0x11,
                                         0x1F}; // a sort of box

static const uint8_t *get_char_bitmap(char c) {
  if (c >= 0x20 && c <= 0x7E) {
    return ASCII_FONT[c - 0x20];
  }
  return ASCII_UNKNOWN;
}

static void draw_char(uint32_t *buffer, int bx, int by, const uint8_t *ch_data,
                      uint32_t fg_color, uint32_t bg_color) {
  for (int row = 0; row < 5; row++) {
    uint8_t line = ch_data[row];
    for (int col = 0; col < 5; col++) {
      int pixel_on = (line & (1 << (4 - col))) ? 1 : 0;
      int x = bx + col;
      int y = by + row;

      if (x >= 0 && x < WINDOW_W && y >= 0 && y < WINDOW_H) {
        buffer[y * WINDOW_W + x] = pixel_on ? fg_color : bg_color;
      }
    }
  }
}

static void draw_char_scaled(uint32_t *buffer, int bx, int by,
                             const uint8_t *ch_data, uint32_t fg_color,
                             uint32_t bg_color, int scale) {
  // scale > 1 means each pixel in ch_data is drawn as a scale x scale block.
  for (int row = 0; row < 5; row++) {
    uint8_t line = ch_data[row];
    for (int col = 0; col < 5; col++) {
      int pixel_on = (line & (1 << (4 - col))) ? 1 : 0;
      uint32_t color = pixel_on ? fg_color : bg_color;

      int start_x = bx + col * scale;
      int start_y = by + row * scale;

      for (int sy = 0; sy < scale; sy++) {
        for (int sx = 0; sx < scale; sx++) {
          int x = start_x + sx;
          int y = start_y + sy;
          if (x >= 0 && x < WINDOW_W && y >= 0 && y < WINDOW_H) {
            buffer[y * WINDOW_W + x] = color;
          }
        }
      }
    }
  }
}

static void draw_string(uint32_t *buffer, int start_x, int start_y,
                        const char *str, uint32_t fg_color, uint32_t bg_color) {
  int x = start_x;
  for (const char *p = str; *p; p++) {
    const uint8_t *ch_data = get_char_bitmap(*p);
    // draw_char(buffer, x, start_y, ch_data, fg_color, bg_color);
    draw_char_scaled(buffer, x, start_y, ch_data, fg_color, bg_color, 3);
    x += 6 * 3;
  }
}

void draw_debug_info(uint32_t *buffer, state_t *s, prev_state_t *prev_state) {
  int display_offset_x = (WINDOW_W - display_width) / 2;
  int display_offset_y = (WINDOW_H - display_height) / 4;

  int info_x = 0;
  char buf[64];
  uint32_t default_color = 0xFFFFFFFF; // white
  uint32_t changed_color = 0xFFFFFF00; // yellow
  uint32_t current_color;

  // PC
  current_color = (s->pc != prev_state->pc) ? changed_color : default_color;
  snprintf(buf, sizeof(buf), "PC: %03X", s->pc);
  draw_string(buffer, info_x + display_offset_x, display_offset_y - 40, buf,
              current_color, 0x00000000);

  // I
  current_color = (s->I != prev_state->I) ? changed_color : default_color;
  snprintf(buf, sizeof(buf), "I : %03X", s->I);
  draw_string(buffer, info_x + display_offset_x, display_offset_y - 20, buf,
              current_color, 0x00000000);

  int info_y = 10;

  // V0-VF
  for (int i = 0; i < 16 / 2; i++) {
    current_color =
        (s->V[i] != prev_state->V[i]) ? changed_color : default_color;
    snprintf(buf, sizeof(buf), "V%X: %02X", i, s->V[i]);
    draw_string(buffer, info_x + display_offset_x,
                info_y + display_offset_y + display_height, buf, current_color,
                0x00000000);
    info_y += 20;
  }

  info_y -= 20 * 8;
  info_x += 140;

  for (int i = 0; i < 16 / 2; i++) {
    current_color =
        (s->V[i + 8] != prev_state->V[i + 8]) ? changed_color : default_color;
    snprintf(buf, sizeof(buf), "V%X: %02X", i + 8, s->V[i + 8]);
    draw_string(buffer, info_x + display_offset_x,
                info_y + display_offset_y + display_height, buf, current_color,
                0x00000000);
    info_y += 20;
  }

  prev_state->pc = s->pc;
  prev_state->I = s->I;
  for (int i = 0; i < 16; i++) {
    prev_state->V[i] = s->V[i];
  }
}
