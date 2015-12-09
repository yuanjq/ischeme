#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ischeme.h"

#define DELIMITERS  "()[]{}\";\f\t\v\n\r "
static IScheme g_isc = {0};
static Cell* op_func(IScheme*, int);
static OpCode g_opcodes[] = {
    #define _OPCODE(f, n, t, o) {f, n, t},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};
static Reader g_readers[256];

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
        c->t = PAIR;
        c->pair.a = a;
        c->pair.d = d;
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
        c->str = strdup(s);
    }
    return c;
}

static Cell *mkLambda(Cell *a, Cell *e)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = LAMBDA;
        c->pair.a = a;
        c->pair.d = e;
    }
    return c;
}

static Cell *mkPort(FILE *f, String name)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = PORT;
        c->port = (Port*)malloc(sizeof(Port));
        if (c->port) {
            c->port->t = PORT_FILE;
            c->port->f.file = f;
            c->port->f.name = strdup(name);
        } else {
            IError("no memory.");
            return NULL;
        }
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

static Boolean isPair(Cell *c)      { return c && c->t == PAIR; }
static Boolean isSymbol(Cell *c)	{ return c && c->t == SYMBOL; }
static Boolean isPort(Cell *)       { return c && c->t == PORT; }

static String symbol(Cell *c)       { return isSymbol(c) ? c->str: NULL; }
static Port* port(Cell *c)          { return isPort(c) ? c->port : NULL; }

static Cell *car(Cell *c)        { return isPair(c) ? c->pair.a : NULL; }
static Cell *cdr(Cell *c)        { return isPair(c) ? c->pair.d : NULL; }
static Cell *caar(Cell *c)       { return car(car(c)); }
static Cell *cadr(Cell *c)       { return car(cdr(c)); }
static Cell *cdar(Cell *c)       { return cdr(car(c)); }
static Cell *cddr(Cell *c)       { return cdr(cdr(c)); }

static Cell *rplaca(Cell *c, Cell *a)    { return isPair(c) ? c->pair.a = a : NULL; }
static Cell *rplacd(Cell *c, Cell *d)    { return isPair(c) ? c->pair.d = d : NULL; }


/***************** repl loop ******************/
static inline int getChar(Cell *in) {
    //if (!isPort(in)) return EOF;
    Port *p = in->port;
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

static inline void ungetChar(Cell *out, int c) {
    //if (!isPort(out)) return;
    Port *p = out->port;
    if (c == EOF) return;
    if (p->t & PORT_FILE) {
        ungetc(c, p->f.file);
    } else {
        if (p->s.cur != p->s.start) p->s.cur--;
    }
}

#define gotoOp(sc, o)      {sc->op=o; goto Loop;}
static Cell *op_func(IScheme *isc, int op)
{
Loop:
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
    c = mkSymbol(s);
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

static Cell *readIllegal(IScheme *isc, int c)
{
    return (Cell*)-1;
}

static Cell *readBlank(IScheme *isc, int c)
{
    return NULL;
}

static inline String readUpto(IScheme *isc, String s)
{
    char *p = isc->inBuff;
    while ((p - isc->inBuff < sizeof(isc->inBuff)) &&
           !strchr(s, (*p++ = getChar(isc->inPort))));
    ungetChar(isc, *--p);
    *p = '\0';
    return isc->inBuff;
}

static Boolean startWith(String s, String w)
{
    char *p1, p2;
    p1 = s; p2 = w;
    while (*p1 != '\0' && *p2 != '\0' && *p1++ == *p2++);
    if (*p2 == '\0')
        return TRUE;
    else
        return FALSE;
}

static Cell *readAlpha(IScheme *isc, int c)
{
    ungetChar(isc->inPort, c);
    char *str = readUpto(isc, DELIMITERS);
    char *p = str;

    Exactness exactness = NO_EXACTNESS;
    Radix radix = NO_RADIX;
    while (*p == '#') {
        switch (*++p) {
        case 'b': case 'B':
            if (radix != NO_RADIX)
                return RET_FAILED;
            radix = BIN;
            break;
        case 'o': case 'O':
            if (radix != NO_RADIX)
                return RET_FAILED;
            radix = OCT;
            break;
        case 'd': case 'D':
            if (radix != NO_RADIX)
                return RET_FAILED;
            radix = DEC;
            break;
        case 'x': case 'X':
            if (radix != NO_RADIX)
                return RET_FAILED;
            radix = HEX;
            break;
        case 'e': case 'E':
            if (exactnes != NO_EXACTNESS)
                return RET_FAILED;
            exactness = EXACT;
            break;
        case 'i': case 'I':
            if (exactnes != NO_EXACTNESS)
                return RET_FAILED;
            exactness = INEXACT;
            break;
        default:
            return RET_FAILED;
        }
        ++p;
    }

    if (radix == NO_RADIX) {
        radix = DEC;
    }

    //int sign =
}
#if 0
static Cell *readAlpha(IScheme *isc, int c)
{
    char buf[1024];
    int idx = 0;
    buf[idx++] = c;
    while ((c = getc(in)) > 0 && (readAlpha == _readers[c] || readNumber == _readers[c] || readSign == _readers[c]))
        buf[idx++] = c;
    ungetc(c, in);
    buf[idx] = '\0';
    return mkSymbol(buf);
}
#endif
static Cell *readHash(IScheme *isc, int c)
{

}

static Cell *readString(IScheme *isc, int c)
{
    char buf[1024];
    int idx = 0;
    int c;
    while ((c = getc(in)) > 0 && c != q)
    {
        if ('\\' == c)
        {
            buf[idx++] = c;
            buf[idx++] = getc(in);
        }
        else
            buf[idx++] = c;
    }
    if (c != q) error("EOF in string");
    buf[idx++] = '\0';
    return mkString(strdup(buf));
}

static Cell *readSign(IScheme *isc, int c)
{
    int d = getc(in);
    ungetc(d, in);
    return (d > 0 && _readers[d] == readNumber) ? readNumber(c, in) : readAlpha(c, in);
}

static Cell *readQuote(IScheme *isc, int c)
{
    Cell *cell = readFile(in);
    cell = cons(cell, 0);
    cell = cons(_s_quote, cell);
    return cell;
}

static Cell *readQuasiquote(IScheme *isc, int c)
{
    return 0;
}

static Cell *readUnquote(IScheme *isc, int c)
{
    return 0;
}

static Cell *readList(IScheme *isc, int c)
{
    Cell *head, *tail, *cell = 0;
    head = tail = cons(0, 0);

    switch (c) {
    case '(': c = ')'; break;
    case '[': c = ']'; break;
    case '{': c = '}'; break;
    }

    int d, m = 0, n = 0;
    for (;;) {
        while (isspace((d = getc(in))));
        if (c == d) break;
        if (feof(in)) error("end of file");
        if (d == ')' || d == ']' || d == '}' || n > 0) error("mismatched parentheses");
        if (d == '.') {
            if (m = 0 || n++ > 0) error("illegal use of '.'");
            rplacd(tail, readFile(in));
        }
        else {
            m++;
            ungetc(d, in);
            cell = readFile(in);
            tail = rplacd(tail, cons(cell, 0));
        }
    }

    head = cdr(head);
    if (head && symbolP(car(head))) {
        Cell *syntax = assq(car(head), cdr(_syntaxTable));
        if (syntax) head = apply(cdr(syntax), cdr(head), _environment);
    }

    return head;
}

static Cell *readSemi(IScheme *isc, int c)
{
    while ((c = getc(in)) && c != '\n' && c != '\r');
    return 0;
}

static void initReaders(Reader r, char *chrs)
{
    while (*chrs) g_readers[*chrs++] = r;
}

static void isc_init(FILE *in, String name)
{
    g_isc.freeCellCount = 0;
    g_isc.freeCells = NULL;
    g_isc.lastSeg = -1;
    g_isc.globalEnvir = NULL;
    seg_alloc(&g_isc, 3);

    g_isc.inPort = mkPort(in, name);
    g_isc.outPort = mkPort(stdout, NULL);

    for (int i = 0;  i < 256;  i++) g_readers[i]= readIllegal;
    initReaders(readBlank,  " \t\n\v\f\r");
    initReaders(readAlpha,  "0123456789");
    initReaders(readAlpha,  "abcdefghijklmnopqrstuvwxyz");
    initReaders(readAlpha,  "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    initReaders(readAlpha,  "!#$%&*/:<=>?@\\^_|~");
    initReaders(readAlpha,  ".");
    initReaders(readHash,   "#");
    initReaders(readSign,   "+-");
    initReaders(readString, "\"");
    initReaders(readQuote,  "'");
    initReaders(readQuasiquote, "`");
    initReaders(readUnquote, ",");
    initReaders(readList,   "([{");
    initReaders(readSemi,   ";");

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

static void isc_finalize()
{

}

int main(int argc, char *argv[])
{
    FILE *in = NULL;
    String name = NULL;
    if (argc == 1) {
        in = stdin;
    } else {
        in = fopen(argv[1], "r");
        name = argv[1];
        if (!in) {
            IError("cant't open file '%s'.", name);
            return -1;
        }
    }

    isc_init(in, name);
    isc_repl();
    isc_finalize();
    return 0;
}

