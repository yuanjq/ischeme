#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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

typedef unsigned char   UInt8;
typedef unsigned char   Boolean;
typedef const char*     String;
typedef const char*     Symbol;
typedef struct _Cell    Cell;
typedef struct _Number  Number;
//typedef struct _Conti   Conti;
typedef struct _Port    Port;
typedef struct _Conti   Conti;
typedef struct _IScheme IScheme;

#define TRUE              1
#define FALSE             0
#define SEGS_NUM          100
#define SEG_CELLS_NUM     5000
#define SEG_MEM_SIZE      (SEG_CELLS_NUM * sizeof(Cell))

enum Type {SYNTAX=1, CHAR, BOOLEAN, NUMBER, STRING, SYMBOL, CONS, VECTOR, PORT, EXPR, LAMBDA, PROC, MACRO, CONTI};
enum PortType {
    PORT_FREE   = 0,
    PORT_INPUT  = 1,
    PORT_OUTPUT = 2,
    PORT_FILE   = 4,
    PORT_STRING = 8,
    PORT_EOF    = 32,
};

enum Op {
    #define _OPCODE(f, n, t, o) o,
    #include "opcodes.h"
    #undef _OPCODE
    OP_MAX
};

struct _Number {
    Boolean fixed;
    union {
        long l;
        double d;
    };
};

struct _Port {
    int t;
    union {
        struct {
            FILE *file;
            char *name;
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

struct _Cell {
    Type t;
    union {
        Number num;
        String str;
        struct {
            Cell *a;
            Cell *d;
        } cons;
        Port *port;
        Conti *conti;
        Cell *next;
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

    Op op;
    Cell *args;
    Cell *envir;
    Cell *code;
    //Cell *retValue;
    Cell *conti;
};

static Cell* op_func(IScheme*, int);

typedef Cell* (*OpFunc)(IScheme*, int);
typedef struct _OpCode OpCode;
struct _OpCode {
    OpFunc func;
    String name;
    int t;
};

static OpCode g_opcodes[] = {
    #define _OPCODE(f, n, t, o) {f, n, t},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};

static IScheme g_isc = {0};

static void gc(IScheme *isc);//, Cell *args, Cell *env);

/************** memery manager ************/
static int seg_alloc(IScheme *isc, int num)
{
    if (isc->lastSeg + num >= SEGS_NUM) return 0;

    for (int i=0; i<num; i++)
    {
        int idx = ++isc->lastSeg;
        isc->segs[idx] = (Cell*)malloc(SEG_MEM_SIZE);
        Cell *pNew = isc->segs[idx];
        isc->freeCellCount += SEG_CELLS_NUM;

        Cell *pLast = pNew + SEG_CELLS_NUM - 1;
        for (Cell *c = pNew; c <= pLast; c++) c->next = c + 1;

        pLast->next = isc->freeCells;
        isc->freeCells = pNew;
    }

    return num;
}

static Cell *cell_alloc()//Cell *args, Cell *env)
{
    if (!g_isc.freeCells) {
        gc(&g_isc);//, args, env);
        if (!g_isc.freeCells && seg_alloc(&g_isc, 1) <= 0) {
            IError("no memery.");
            return NULL;
        }
    }
    Cell *c = g_isc.freeCells;
    g_isc.freeCells = c->next;
    g_isc.freeCellCount--;
    return c;
}

static void gc(IScheme *isc)//, Cell *args, Cell *env)
{
    ITraceEnter();
    // TODO:
    ITraceLeave();
}


/*************** syntax **************/
static Cell *cons(Cell *a, Cell *d) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = CONS;
        c->cons.a = a;
        c->cons.d = d;
    }
    return c;
}

static Cell *mkLong(long n)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = NUMBER;
        c->num.fixed = TRUE;
        c->num.l = n;
    }
    return c;
}

static Cell *mkDouble(double n)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = NUMBER;
        c->num.fixed = TRUE;
        c->num.d = n;
    }
    return c;
}

static Cell *mkNumber(Number n)
{
    if (n.fixed) return mkLong(n.l);
    else return mkDouble(n.d);
}

static Cell *mkSymbol(String s)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = SYMBOL;
        c->str = s;
    }
    return c;
}

static Cell *mkLambda(Cell *a, Cell *e)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = LAMBDA;
        c->cons.a = a;
        c->cons.d = e;
    }
    return c;
}

static Cell *mkConti(IScheme *isc, Op op, Cell *args, Cell *code)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = CONTI;
        c->conti = (Conti*)malloc(sizeof(Conti));
        if (c->conti) {
            c->conti->op = op;
            c->conti->args = args;
            c->conti->envir = isc->envir;
            c->conti->code = code;
        } else {
            IError("no memory.");
            return NULL;
        }
    }
    isc->conti = cons(c, isc->conti);
    return c;
}

static Boolean isCons(Cell *c)      { return c && c->t == CONS; }
static Boolean isSymbol(Cell *c)	{ return c && c->t == SYMBOL; }

static String symbol(Cell *c)       { return isSymbol(c) ? c->str: NULL; }

static Cell *car(Cell *c)        { return isCons(c) ? c->cons.a : NULL; }
static Cell *cdr(Cell *c)        { return isCons(c) ? c->cons.d : NULL; }
static Cell *caar(Cell *c)       { return car(car(c)); }
static Cell *cadr(Cell *c)       { return car(cdr(c)); }
static Cell *cdar(Cell *c)       { return cdr(car(c)); }
static Cell *cddr(Cell *c)       { return cdr(cdr(c)); }

static Cell *rplaca(Cell *c, Cell *a)    { return isCons(c) ? c->cons.a = a : NULL; }
static Cell *rplacd(Cell *c, Cell *d)    { return isCons(c) ? c->cons.d = d : NULL; }


/***************** repl loop ******************/
static inline int getChar(IScheme *isc) {
    Port *p = isc->inPort->port;
    if (p->t & PORT_EOF) return EOF;
    int c = 0;
    if (p->t & PORT_FILE) {
        c = fgetc(p->f.file);
    } else {
        if (p->s.cur == 0 || p->s.cur == p->s.end)
            c == EOF;
        else
            c = *p->s.cur++;
    }

    if (c == EOF) p->t |= PORT_EOF;
    return c;
}

#define gotoOp(sc, o)      {sc->op=o; goto Begin;}
static Cell *op_func(IScheme *isc, int op)
{
Begin:
    switch (op) {
    case OP_LOAD:
        break;
    case OP_REPL_LOOP:
        mkConti(isc, OP_REPL_LOOP, isc->args, isc->envir);
        mkConti(isc, OP_REPL_PRINT, isc->args, isc->envir);
        mkConti(isc, OP_REPL_EVAL, isc->args, isc->envir);
        gotoOp(isc, OP_REPL_READ);
    case OP_REPL_READ:

        break;
    case OP_REPL_EVAL:
        break;
    case OP_REPL_PRINT:
        break;
    }
    return 0;
}

static Cell *findSymbol(IScheme *isc, String s)
{
    for (Cell *c = isc->symbols; c; c = cdr(c)) {
        String sym = symbol(car(c));
        if (sym && !strcmp(sym, s))
          return car(c);
    }
    return NULL;
}

static Cell* internal(IScheme *isc, String s)
{
    Cell *c = NULL;
    if ((c = findSymbol(isc, s))) return c;
    c = mkSymbol(strdup(s));
    isc->symbols = cons(c, isc->symbols);
    return c;
}

static void new_syntax(IScheme *isc, String s)
{
    Cell *c = internal(isc, s);
    c->t = SYNTAX;
}

static Cell *findEnvir(IScheme *isc, Cell *s)
{
    for (Cell *e = isc->globalEnvir; e; e = cdr(isc->globalEnvir)) {
        if (isCons(car(e)) && caar(e) == s)
            return car(e);
    }
    return NULL;
}

static void new_envir(IScheme *isc, Cell *s, Cell *v)
{
    Cell *e;
    if ((e = findEnvir(isc, s))) {
        rplacd(e, v);
    } else {
        isc->globalEnvir = cons(e, isc->globalEnvir);
    }
}

static void isc_init()
{
    g_isc.freeCellCount = 0;
    g_isc.freeCells = NULL;
    g_isc.lastSeg = -1;
    g_isc.globalEnvir = NULL;
    seg_alloc(&g_isc, 3);

    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) new_syntax(&g_isc, g_opcodes[i].name);
            new_envir(&g_isc, internal(&g_isc, g_opcodes[i].name), mkLong(i));
        }
    }
}

static void isc_repl()
{
    g_isc.op = OP_REPL_LOOP;
    for (;;) {
        if (g_opcodes[g_isc.op].func(&g_isc, g_isc.op) < 0)
            break;
    }
}

int main(int argc, char *argv[])
{
    isc_init();

    isc_repl();
    return 0;
}

