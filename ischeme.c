#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ischeme.h"

#define CELL_TRUE    &g_true;
#define CELL_FALSE   &g_false;
#define CELL_NIL     &g_nil;
#define DELIMITERS   "()[]{}\";\f\t\v\n\r "

static IScheme g_isc = {0};
static Cell* op_func(IScheme*, int);
static OpCode g_opcodes[] = {
    #define _OPCODE(f, n, t, o) {f, n, t},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};

static Reader g_readers[256];
static Cell g_true;
static Cell g_false;
static Cell g_nil;

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

static Cell *mk_long(long n)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = NUMBER;
        c->num.fixed = TRUE;
        c->num.l = n;
    }
    return c;
}

static Cell *mk_double(double n)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = NUMBER;
        c->num.fixed = TRUE;
        c->num.d = n;
    }
    return c;
}

static Cell *mk_number(Number n)
{
    if (n.fixed) return mk_long(n.l);
    else return mk_double(n.d);
}

static Cell *mk_symbol(String s)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = SYMBOL;
        c->str = strdup(s);
    }
    return c;
}

static Cell *mk_lambda(Cell *a, Cell *e)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = LAMBDA;
        c->pair.a = a;
        c->pair.d = e;
    }
    return c;
}

static Cell *mk_port(FILE *f, String name)
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

static Cell *mk_conti(IScheme *isc, Op op, Cell *args, Cell *code)
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

static Boolean is_pair(Cell *c)     { return c && c->t == PAIR; }
static Boolean is_symbol(Cell *c)	{ return c && c->t == SYMBOL; }
static Boolean is_port(Cell *)      { return c && c->t == PORT; }

static String symbol(Cell *c)       { return is_symbol(c) ? c->str: NULL; }
static Port* port(Cell *c)          { return is_port(c) ? c->port : NULL; }

static Cell *car(Cell *c)        { return is_pair(c) ? c->pair.a : NULL; }
static Cell *cdr(Cell *c)        { return is_pair(c) ? c->pair.d : NULL; }
static Cell *caar(Cell *c)       { return car(car(c)); }
static Cell *cadr(Cell *c)       { return car(cdr(c)); }
static Cell *cdar(Cell *c)       { return cdr(car(c)); }
static Cell *cddr(Cell *c)       { return cdr(cdr(c)); }

static Cell *rplaca(Cell *c, Cell *a)    { return is_pair(c) ? c->pair.a = a : NULL; }
static Cell *rplacd(Cell *c, Cell *d)    { return is_pair(c) ? c->pair.d = d : NULL; }


/***************** repl loop ******************/
static inline int get_char(Cell *in) {
    //if (!is_port(in)) return EOF;
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

static inline void unget_char(Cell *out, int c) {
    //if (!is_port(out)) return;
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
        mk_conti(isc, OP_REPL_LOOP, isc->args, isc->envir);
        mk_conti(isc, OP_REPL_PRINT, isc->args, isc->envir);
        mk_conti(isc, OP_REPL_EVAL, isc->args, isc->envir);
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

static Cell *find_symbol(IScheme *isc, String s)
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
    if ((c = find_symbol(isc, s))) return c;
    c = mk_symbol(s);
    isc->symbols = cons(c, isc->symbols);
    return c;
}

static void new_syntax(IScheme *isc, String s)
{
    Cell *c = internal(isc, s);
    c->t = SYNTAX;
}

static Cell *find_envir(IScheme *isc, Cell *s)
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
    if ((e = find_envir(isc, s))) {
        rplacd(e, v);
    } else {
        isc->globalEnvir = cons(e, isc->globalEnvir);
    }
}

static Cell *read_illegal(IScheme *isc, int c)
{
    return (Cell*)-1;
}

static Cell *read_blank(IScheme *isc, int c)
{
    return NULL;
}

static inline String read_upto(IScheme *isc, String s)
{
    char *p = isc->inBuff;
    while ((p - isc->inBuff < sizeof(isc->inBuff)) &&
           !strchr(s, (*p++ = get_char(isc->inPort))));
    unget_char(isc, *--p);
    *p = '\0';
    return isc->inBuff;
}

static Boolean start_with(String s, String w)
{
    char *p1, p2;
    p1 = s; p2 = w;
    while (*p1 != '\0' && *p2 != '\0' && *p1++ == *p2++);
    if (*p2 == '\0')
        return TRUE;
    else
        return FALSE;
}

static Cell *read_alpha(IScheme *isc, int c)
{
    unget_char(isc->inPort, c);
    char *str = read_upto(isc, DELIMITERS);
    char *p = str;

    Exactness exactness = NO_EXACTNESS;
    Radix radix = NO_RADIX;
    while (*p == '#') {
        switch (*++p) {
        case 'b': case 'B':
            if (radix != NO_RADIX)
                return CELL_NIL;
            radix = BIN;
            break;
        case 'o': case 'O':
            if (radix != NO_RADIX)
                return CELL_NIL;
            radix = OCT;
            break;
        case 'd': case 'D':
            if (radix != NO_RADIX)
                return CELL_NIL;
            radix = DEC;
            break;
        case 'x': case 'X':
            if (radix != NO_RADIX)
                return CELL_NIL;
            radix = HEX;
            break;
        case 'e': case 'E':
            if (exactnes != NO_EXACTNESS)
                return CELL_NIL;
            exactness = EXACT;
            break;
        case 'i': case 'I':
            if (exactnes != NO_EXACTNESS)
                return CELL_NIL;
            exactness = INEXACT;
            break;
        default:
            return CELL_NIL;
        }
        ++p;
    }

    if (radix == NO_RADIX) {
        radix = DEC;
    }

    //int sign =
}
#if 0
static Cell *read_alpha(IScheme *isc, int c)
{
    char buf[1024];
    int idx = 0;
    buf[idx++] = c;
    while ((c = getc(in)) > 0 && (read_alpha == _readers[c] || read_number == _readers[c] || read_sign == _readers[c]))
        buf[idx++] = c;
    ungetc(c, in);
    buf[idx] = '\0';
    return mk_symbol(buf);
}
#endif
static Cell *read_hash(IScheme *isc, int c)
{

}

static Cell *read_string(IScheme *isc, int c)
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

static Cell *read_sign(IScheme *isc, int c)
{
    int d = getc(in);
    ungetc(d, in);
    return (d > 0 && _readers[d] == read_number) ? read_number(c, in) : read_alpha(c, in);
}

static Cell *read_quote(IScheme *isc, int c)
{
    Cell *cell = readFile(in);
    cell = cons(cell, 0);
    cell = cons(_s_quote, cell);
    return cell;
}

static Cell *read_quasiquote(IScheme *isc, int c)
{
    return 0;
}

static Cell *read_unquote(IScheme *isc, int c)
{
    return 0;
}

static Cell *read_list(IScheme *isc, int c)
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

static Cell *read_semi(IScheme *isc, int c)
{
    while ((c = getc(in)) && c != '\n' && c != '\r');
    return 0;
}

static void init_readers(Reader r, char *chrs)
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

    g_isc.inPort = mk_port(in, name);
    g_isc.outPort = mk_port(stdout, NULL);

    for (int i = 0;  i < 256;  i++) g_readers[i]= read_illegal;
    init_readers(read_blank,  " \t\n\v\f\r");
    init_readers(read_alpha,  "0123456789");
    init_readers(read_alpha,  "abcdefghijklmnopqrstuvwxyz");
    init_readers(read_alpha,  "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    init_readers(read_alpha,  "!#$%&*/:<=>?@\\^_|~");
    init_readers(read_alpha,  ".");
    init_readers(read_hash,   "#");
    init_readers(read_sign,   "+-");
    init_readers(read_string, "\"");
    init_readers(read_quote,  "'");
    init_readers(read_quasiquote, "`");
    init_readers(read_unquote, ",");
    init_readers(read_list,   "([{");
    init_readers(read_semi,   ";");

    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) new_syntax(&g_isc, g_opcodes[i].name);
            new_envir(&g_isc, internal(&g_isc, g_opcodes[i].name), mk_long(i));
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

