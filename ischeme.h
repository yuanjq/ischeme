#ifndef ISCHEME_H
#define ISCHEME_H

#define IWarning(fmt, ...)  printf("*Warning*: " fmt, ##__VA_ARGS__)
#define IError(fmt, ...)    printf("*Error*: " fmt, ##__VA_ARGS__)
#define IDEBUG_MORE

#ifdef  IDEBUG_MORE
#define IMessage(fmt, ...)  printf("*Message*: " fmt, ##__VA_ARGS__)
#define ITraceEnter()     IMessage("Func %s enter.", __FUNCTION__)
#define ITraceLeave()     IMessage("Func %s leave.", __FUNCTION__)
#else
#define IMessage(fmt, ...)
#define ITraceEnter()
#define ITraceLeave()
#endif

typedef unsigned char   bool;
typedef unsigned char   uint8;
typedef unsigned int    Char;
typedef const char*     String;
typedef const char*     Symbol;

typedef struct _Number  Number;
typedef struct _Pair    Pair;
typedef struct _Port    Port;
typedef struct _Conti   Conti;
typedef struct _Cell    Cell;
typedef struct _OpCode  OpCode;
typedef struct _IScheme IScheme;

typedef Cell* (*OpFunc)(IScheme*, int);
typedef Cell*(*Reader)(IScheme*, int);

#define TRUE              1
#define FALSE             0
#define SEGS_NUM          100
#define SEG_CELLS_NUM     5000
#define SEG_MEM_SIZE      (SEG_CELLS_NUM * sizeof(Cell))
#define MAX_LOAD_FILES    256
#define STR_BUF_SIZE      1024

#define TYPE_MASK         0x0000FFFF
#define MARK_IMMUTABLE    0x00010000
#define MARK_REFERENCE    0x00020000


enum Ret {
    RET_FAILED = -1,
    RET_SUCCESSED = 0
};

enum Type {
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
    LAMBDA,
    PROC,
    MACRO,
    CONTI,
};

enum Token {
    TOK_EOF = -1,
    TOK_ATOM,
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

enum PortType {
    PORT_FREE   = 0,
    PORT_INPUT  = 1<<1,
    PORT_OUTPUT = 1<<2,
    PORT_ERROR  = 1<<3,
    PORT_FILE   = 1<<4,
    PORT_STRING = 1<<5,
    PORT_EOF    = 1<<6,
};

enum NumberType {
    NUMBER_LONG,
    NUMBER_DOUBLE,
    NUMBER_FRACTION,
    NUMBER_COMPLEX,
};

enum Exactness {
    NO_EXACTNESS,
    INEXACT,
    EXACT
};

enum Radix {
    NO_RADIX = 0,
    BIN = 2,
    OCT = 8,
    DEC = 10,
    HEX = 16
};

enum Op {
    #define _OPCODE(f, n, t, o) o,
    #include "opcodes.h"
    #undef _OPCODE
    OP_MAX
};

struct _Number {
    uint8 t;
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
            char *name;
            int curLine;
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
        Cell    *next;
    };
};

struct _IScheme {
    int lastSeg;
    int freeCellCount;
    Cell *segs[SEGS_NUM];
    Cell *freeCells;
    Cell *globalEnvir;
    Cell *symbols;
    Cell *inPort;
    Cell *outPort;
    Cell *loadFiles[MAX_LOAD_FILES];
    int curFileIdx;

    Op op;
    Cell *retnv;
    Cell *args;
    Cell *envir;
    Cell *code;
    Cell *contis;
    char buff[STR_BUF_SIZE];
};

struct _OpCode {
    OpFunc func;
    String name;
    int t;
};

#endif
