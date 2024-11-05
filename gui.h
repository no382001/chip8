#pragma once
#include "vm.h"
#include <pthread.h>
#include <tcl.h>
#include <tk.h>

extern pthread_mutex_t key_mutex;
extern pthread_cond_t key_cond;
extern volatile uint8_t key_pressed;

extern pthread_mutex_t display_mutex;

extern uint8_t g_display[64 * 32];

int hex_key_to_value(const char *key);
int keypress_cb(ClientData clientData, Tcl_Interp *interp, int argc,
                const char *argv[]);
void refresh_display_cb(ClientData cd);
void *tk_thread_main(void *arg);