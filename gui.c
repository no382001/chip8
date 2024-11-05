#include "gui.h"

extern pthread_mutex_t key_mutex;
extern pthread_cond_t key_cond;
extern volatile uint8_t key_pressed;

extern pthread_mutex_t display_mutex;
extern uint8_t display_dirty;

int hex_key_to_value(const char *key) {
  if (!strcmp(key, "1"))
    return 0x1;
  if (!strcmp(key, "2"))
    return 0x2;
  if (!strcmp(key, "3"))
    return 0x3;
  if (!strcmp(key, "C"))
    return 0xC;
  if (!strcmp(key, "4"))
    return 0x4;
  if (!strcmp(key, "5"))
    return 0x5;
  if (!strcmp(key, "6"))
    return 0x6;
  if (!strcmp(key, "D"))
    return 0xD;
  if (!strcmp(key, "7"))
    return 0x7;
  if (!strcmp(key, "8"))
    return 0x8;
  if (!strcmp(key, "9"))
    return 0x9;
  if (!strcmp(key, "E"))
    return 0xE;
  if (!strcmp(key, "A"))
    return 0xA;
  if (!strcmp(key, "0"))
    return 0x0;
  if (!strcmp(key, "B"))
    return 0xB;
  if (!strcmp(key, "F"))
    return 0xF;
  return 0xFF;
}

int keypress_cb(ClientData clientData, Tcl_Interp *interp, int argc,
                const char *argv[]) {
  if (argc < 2)
    return TCL_ERROR;

  uint8_t key = hex_key_to_value(argv[1]);

  pthread_mutex_lock(&key_mutex);
  key_pressed = key;
  pthread_cond_signal(&key_cond);
  pthread_mutex_unlock(&key_mutex);

  return TCL_OK;
}

void refresh_display_cb(ClientData cd) {
  if (!display_dirty) {
    return;
  }
  Tcl_Interp *interp = (Tcl_Interp *)cd;
  // pthread_mutex_lock(&display_mutex);

  Tcl_Eval(interp, ".main.display delete all");
  for (int i = 0; i < 64 * 32; i++) {
    int x = i % 64;
    int y = i / 64;
    if (g_display[i]) {
      char cmd[256];
      snprintf(cmd, sizeof(cmd),
               ".main.display create rectangle %d %d %d %d -fill white "
               "-outline white",
               x * 10, y * 10, (x + 1) * 10, (y + 1) * 10);
      Tcl_Eval(interp, cmd);
    }
  }

  // pthread_mutex_unlock(&display_mutex);
  display_dirty = 0;
}

void *tk_thread_main(void *arg) {
  Tcl_Interp *interp = Tcl_CreateInterp();
  if (Tcl_Init(interp) == TCL_ERROR || Tk_Init(interp) == TCL_ERROR) {
    fprintf(stderr, "tcl/tk init failed: %s\n", Tcl_GetStringResult(interp));
    return NULL;
  }

  Tcl_CreateCommand(interp, "keypress_cb", keypress_cb, NULL, NULL);

  if (Tcl_EvalFile(interp, "gui.tcl") != TCL_OK) {
    fprintf(stderr, "rrror loading tcl/tk: %s\n", Tcl_GetStringResult(interp));
    return NULL;
  }

  Tcl_CreateTimerHandler(50, refresh_display_cb, (ClientData)interp);

  Tk_MainLoop();
  Tcl_DeleteInterp(interp);
  return NULL;
}