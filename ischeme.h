#pragma once

#ifdef __cplusplus
extern "C" {
#endif

enum PortType {
    PORT_FREE           = 0,
    PORT_INPUT          = 1<<1,
    PORT_OUTPUT         = 1<<2,
    PORT_FILE           = 1<<3,
    PORT_STRING         = 1<<4,
    PORT_STDIO          = 1<<5,
    PORT_STDIN          = 1<<1 | 1<<3 | 1<<5,
    PORT_STDOUT         = 1<<2 | 1<<3 | 1<<5,
    PORT_STDERR         = 1<<2 | 1<<3 | 1<<5 | 1<<6,
    PORT_INPUT_FILE     = 1<<1 | 1<<3,
    PORT_INPUT_STRING   = 1<<1 | 1<<4,
    PORT_OUTPUT_FILE    = 1<<2 | 1<<3,
    PORT_OUTPUT_STRING  = 1<<2 | 1<<4,
    PORT_EOF            = 1<<7,
};

struct Cell;

void ischeme_init();
Cell *ischeme_ctx_new();
Cell *ischeme_port_new(Cell *ctx, PortType type, const char *des);
void ischeme_port_close(Cell *ctx, Cell *port);
Cell *ischeme_eval(Cell *ctx, Cell *port);
Cell *ischeme_eval_string(Cell *ctx, const char *expr);
Cell *ischeme_eval_file(Cell *ctx, const char *file);
void ischeme_print(Cell *ctx, Cell *c);
void ischeme_print_to(Cell *ctx, Cell *c, Cell *port);
void ischeme_free(Cell *c);

#ifdef __cplusplus
}
#endif
