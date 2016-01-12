#ifndef ISCHEME_H
#define ISCHEME_H

#include <setjmp.h>

//#define IDEBUG_MORE

#ifdef  IDEBUG_MORE
#define IMessage(fmt, ...)  printf("*Message*: "fmt"\n", ##__VA_ARGS__)
#define IWarning(fmt, ...)  printf("*Warning*: "fmt"\n", ##__VA_ARGS__)
#define IError(fmt, ...)    printf("*Error*[F:%s][L:%d]: "fmt"\n", __FUNCTION__, __LINE__, ##__VA_ARGS__)
#define ITraceEnter()       IMessage("Func %s enter.", __FUNCTION__)
#define ITraceLeave()       IMessage("Func %s leave.", __FUNCTION__)
#else
#define IMessage(fmt, ...)
#define IWarning(fmt, ...)
#define IError(fmt, ...)
#define ITraceEnter()
#define ITraceLeave()
#endif

typedef unsigned char   bool;
typedef unsigned int    Char;
typedef char*           String;
typedef const char*     Symbol;

typedef struct _Number  Number;
typedef struct _Pair    Pair;
typedef struct _Port    Port;
typedef struct _Conti   Conti;
typedef struct _Cell    Cell;
typedef struct _OpCode  OpCode;
typedef struct _IScheme IScheme;

typedef enum _Op            Op;
typedef enum _Radix         Radix;
typedef enum _Exactness     Exactness;
typedef enum _NumberType    NumberType;
typedef enum _PortType      PortType;
typedef enum _Token         Token;
typedef enum _Type          Type;

typedef Cell* (*OpFunc)(IScheme*, int);
typedef Cell*(*Reader)(IScheme*, int);
typedef Cell*(*EProc)(IScheme*, Cell*);

#define TRUE                1
#define FALSE               0
#define SEGS_NUM            100
#define SEG_CELLS_NUM       5000
#define SEG_MEM_SIZE        (SEG_CELLS_NUM * sizeof(Cell))
#define MAX_LOAD_FILES      256
#define STR_BUF_SIZE        1024

#define T_MASK              0x0000FFFF
#define M_MASK              0xFFFF0000
#define M_IMMUTABLE         0x00010000
#define M_REFERENCE         0x00020000
#define T(c)                (c->t & T_MASK)
#define M(c)                (c->t & M_MASK)

enum Ret {
    RET_FAILED = -1,
    RET_SUCCESSED = 0
};

enum _Type {
    FREE = 0,
    CHAR,
    BOOLEAN,
    NUMBER,
    STRING,
    SYMBOL,
    SYNTAX,
    PAIR,
    LIST,
    VECTOR,
    PORT,
    EXPR,
    PROC,
    IPROC,
    EPROC,
    MACRO,
    CONTI,
    ENVIR,
    PROMISE,
};

enum _Token {
    TOK_EOF = -1,
    TOK_SYMBOL,
    TOK_LPAREN,
    TOK_LBRACKET,
    TOK_LBRACE,
    TOK_RPAREN,
    TOK_RBRACKET,
    TOK_RBRACE,
    TOK_DOT,
    TOK_QUOTE,
    TOK_DQUOTE,
    TOK_QQUOTE,
    TOK_UNQUOTE,
    TOK_UNQUOTE_SPLICING,
    TOK_CONST,
    TOK_VECTOR,
    TOK_MAX
};

enum _PortType {
    PORT_FREE   = 0,
    PORT_INPUT  = 1<<1,
    PORT_OUTPUT = 1<<2,
    PORT_ERROR  = 1<<3,
    PORT_FILE   = 1<<4,
    PORT_STRING = 1<<5,
    PORT_EOF    = 1<<6,
};

enum _NumberType {
    NUMBER_LONG,
    NUMBER_DOUBLE,
    NUMBER_FRACTION,
    NUMBER_COMPLEX,
};

enum _Exactness {
    NO_EXACTNESS,
    INEXACT,
    EXACT
};

enum _Radix {
    NO_RADIX = 0,
    BIN = 2,
    OCT = 8,
    DEC = 10,
    HEX = 16
};

enum _Op {
    #define _OPCODE(f, n, t1, o, m1, m2, t2) o,
    #include "opcodes.h"
    #undef _OPCODE
    OPCODE_MAX
};

struct _Number {
    unsigned char t;
    union {
        long l;
        double d;
        struct {
            Number *nr;
            Number *dr;
        } fn;
        struct {
            Number *rl;
            Number *im;
        } cx;
    };
};

struct _Port {
    int t;
    union {
        struct {
            FILE *file;
            String name;
            int curr_line;
        } f;
        struct {
          char *start;
          char *end;
          char *cur;
        } s;
    };
};

struct _Conti {
    Op op;
    Cell *args;
    Cell *envir;
    Cell *code;
};

struct _Pair{
    Cell *a;
    Cell *d;
};

struct _Cell {
    int t;
    union {
        Char    chr;
        String  str;
        Number  *num;
        Pair    *pair;
        Port    *port;
        Conti   *conti;
        EProc   proc;
        Cell    *next;
    };
};

struct _IScheme {
    int last_seg;
    int free_cell_count;
    Cell *segs[SEGS_NUM];
    Cell *free_cells;
    Cell *global_envir;
    Cell *symbols;
    Cell *inport;
    Cell *outport;
    Cell *load_files[MAX_LOAD_FILES];
    int cur_file_idx;

    Op op;
    Cell *retnv;
    Cell *args;
    Cell *envir;
    Cell *code;
    Cell *contis;

    Cell *sym_lambda;
    Cell *sym_quote;
    jmp_buf jmpbuf;
    char buff[STR_BUF_SIZE];
};

struct _OpCode {
    OpFunc func;
    String name;
    unsigned char t;
    int min_args;
    int max_args;
    unsigned char *arg_types;
};

#endif
