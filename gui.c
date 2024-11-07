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