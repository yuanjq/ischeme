#include <stdio.h>
#include <unistd.h>
#include "ischeme.h"
#include "cell.h"

void isc_helper() {
    printf("iScheme v0.1 by yuanjq\n");
    printf("Options and arguments:\n");
    printf("  -\t\t program read from stdin\n");
    printf("  -s\t\t program read form sting\n");
    printf("  file\t\t program read from file\n");
    printf("  -h(--help)\t print this help message and exit\n");
    printf("  -v(--version)\t iScheme's version\n");
}

int main(int argc, char *argv[]) {
    PortType pt = PORT_FREE;
    FILE *in = NULL;
    char *str = NULL;
    if (argc == 1) {
        in = stdin;
    } else {
        if (!strcmp(argv[1], "-h")  || !strcmp(argv[1], "--help")) {
            isc_helper();
            return 0;
        } else if (!strcmp(argv[1], "-v") || !strcmp(argv[1], "--version")) {
            printf("iScheme v0.1\n");
            return 0;
        } else if (!strcmp(argv[1], "-")) {
            in = stdin;
        } else if (!strcmp(argv[1], "-s")) {
            if (argc != 3) {
                IError("invalid arguments");
                return -1;
            }
            pt = PORT_INPUT_STRING;
            str = argv[2];
        } else if (!access(argv[1], F_OK | R_OK)) {
            pt = PORT_INPUT_FILE;
            in = fopen(argv[1], "r");
            str = argv[1];
            if (!in) {
                IError("cant't open file '%s'", str);
                return -1;
            }
        } else {
            isc_helper();
            return 0;
        }
    }

    if (in == stdin) {
        pt = PORT_STDIN;
        printf("iScheme v0.1 by yuanjq\n");
    }

    Cell *ctx;
    ctx = isc_init(pt, in, str);
    if (!ctx) {
        return -1;
    }
    isc_repl(ctx);
    isc_finalize(ctx);
    return 0;
}

