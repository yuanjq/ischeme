#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ischeme.h"

#define CELL_TRUE    &g_true;
#define CELL_FALSE   &g_false;
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
    Cell *c = NULL;
    Pair *pair = malloc(sizeof(Pair));
    if (pair) {
        c = cell_alloc();
        if (c) {
            c->t = PAIR;
            c->pair = pair;
            pair->a = a;
            pair->d = d;
        }
    }
    return c;
}

static Cell *mk_char(Char chr) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = CHAR;
        c->chr = chr;
    }
    return c;
}

static Cell *mk_long(long n)
{
    Cell *c = NULL;
    Number *num = malloc(sizeof(Number));
    if (num) {
        c = cell_alloc();
        if (c) {
            c->t = NUMBER;
            c->num = num;
            c->num->t = NUMBER_LONG;
            c->num->l = n;
        }
    }
    return c;
}

static Cell *mk_double(double n)
{
    Cell *c = NULL;
    Number *num = malloc(sizeof(Number));
    if (num) {
        c = cell_alloc();
        if (c) {
            c->t = NUMBER;
            c->num = num;
            num->t = NUMBER_DOUBLE;
            num->d = n;
        }
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
    Cell *c = NULL;
    Pair *pair = malloc(sizeof(Pair));
    if (pair) {
        c = cell_alloc();
        if (c) {
            c->t = LAMBDA;
            c->pair = pair;
            pair->a = a;
            pair->d = e;
        }
    }
    return c;
}

static Cell *mk_port(FILE *f, String name)
{
    Cell *c = NULL;
    Port *port = (Port*)malloc(sizeof(Port));
    if (port) {
        c = cell_alloc();
        if (c) {
            c->t = PORT;
            c->port = port;
            port->t = PORT_FILE;
            port->f.file = f;
            port->f.name = strdup(name);
        }
    }
    return c;
}

static Cell *mk_conti(IScheme *isc, Op op, Cell *args, Cell *code)
{
    Cell *c = NULL;
    Conti *conti = (Conti*)malloc(sizeof(Conti));
    if (conti) {
        c = cell_alloc();
        if (c) {
            c->t = CONTI;
            c->conti->op = op;
            c->conti->args = args;
            c->conti->envir = isc->envir;
            c->conti->code = code;
        }
        isc->conti = cons(c, isc->conti);
    }
    return c;
}

static bool is_pair(Cell *c)     { return c && c->t == PAIR; }
static bool is_symbol(Cell *c)	 { return c && c->t == SYMBOL; }
static bool is_port(Cell *)      { return c && c->t == PORT; }

static String symbol(Cell *c)    { return is_symbol(c) ? c->str: NULL; }
static Port* port(Cell *c)       { return is_port(c) ? c->port : NULL; }

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

static inline int read_upto(Cell *port, char *upto, char *out, int size)
{
    if (!is_port(port) || out || size <= 0)
        return -1;
    int c;
    char *p = out;

    for (;;) {
        if (p - out < size - 1) {
            if ((c = get_char(port)) < 0) {
                IError("end of file.");
                return -1;
            }
            if (strchr(upto, c))
                break;
            *p++ = c;
        } else {
            *p = '\0';
            IError("symbol name too loog.");
            return -1;
        }
    }
    unget_char(port, c);
    *p = '\0';
    return p - in;
}

static bool start_with(String s, String w)
{
    char *p1, p2;
    p1 = s; p2 = w;
    while (*p1 != '\0' && *p2 != '\0' && *p1++ == *p2++);
    if (*p2 == '\0')
        return TRUE;
    else
        return FALSE;
}

static double xtod(char num[], int radix)
{
    double dnum = 0;
    int i, j, k=-1, n=0;

    for (i=0;; i++)
    {
        if (num[i] == '\0')
            break;
        if (num[i] == '.')
        {
            j=i;
            ++k;
            continue;
        }
        if (k >= 0)
            ++k;
    }

    j = k>=0 ? j : i;
    for(i = j-1; i >= 0; i--)
    {
        if (num[j-i-1] >= 'a' && num[j-i-1] <= 'f') {
            n = num[j-i-1] - 'a' + 10;
        } else if (num[j-i-1] >= 'A' && num[j-i-1] <= 'F') {
            n = num[j-i-1] - 'A' + 10;
        } else  if (num[j-i-1] >= '0' && num[j-i-1] <= '9') {
            n = num[j-i-1] - '0';
        }
        dnum += n * pow(radix, i);
    }

    if(k >= 0)
    {
        for(i=0; i<k; i++)
        {
            if (num[i+j+1] >= 'a' && num[i+j+1] <= 'f') {
                n = num[i+j+1] - 'a' + 10;
            } else if (num[i+j+1] >= 'A' && num[i+j+1] <= 'F') {
                n = num[i+j+1] - 'A' + 10;
            } else  if (num[i+j+1] >= '0' && num[i+j+1] <= '9') {
                n = num[i+j+1] - '0';
            }
            dnum += n * pow(radix, -i-1);
        }
    }
    return dnum;
}

static Cell *read_alpha(IScheme *isc, int c)
{
    unget_char(isc->inPort, c);
    char *p = isc->inBuff;
    int totalLen = read_upto(isc->inPort, p, DELIMITERS);

    Exactness exactness = NO_EXACTNESS;
    Radix radix = NO_RADIX;
    char *pEnd = p + totalLen;
    while (p < pEnd && *p == '#') {
        if (p + 2 > pEnd)
            goto Error0;
        switch (p[1]) {
        case 'b': case 'B':
            if (radix != NO_RADIX)
                goto Error0;
            radix = BIN;
            break;
        case 'o': case 'O':
            if (radix != NO_RADIX)
                goto Error0;
            radix = OCT;
            break;
        case 'd': case 'D':
            if (radix != NO_RADIX)
                goto Error0;
            radix = DEC;
            break;
        case 'x': case 'X':
            if (radix != NO_RADIX)
                goto Error0;
            radix = HEX;
            break;
        case 'e': case 'E':
            if (exactnes != NO_EXACTNESS)
                goto Error0;
            exactness = EXACT;
            break;
        case 'i': case 'I':
            if (exactnes != NO_EXACTNESS)
                goto Error0;
            exactness = INEXACT;
            break;
        case 'f': case 'F':
            if (exactnes != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pEnd)
                goto Error0;
            return CELL_FALSE;
        case 't': case "T":
            if (exactnes != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pEnd)
                goto Error0;
            return CELL_TRUE;
        case '\\':
            if (exactnes != NO_EXACTNESS || radix != NO_RADIX || p + 3 != pEnd)
                goto Error0;
            return mk_char(p[2]);
        default:
            goto Error0;
        }
        p += 2;
    }

    int sign = 0, len = 0;
    char *pStart = NULL;
    bool bIsNumber = TRUE;
    bool bFindDot = FALSE;
    bool bFindNum = FALSE;

    c = *p;
    if (c == '+') {
        sign = 1;
        ++p;
    }
    else if (c == '-') {
        sign = -1;
        ++p;
    }

    //pStart = p;
    while (p < pEnd && (c = *p) != '\0') {
        if (bIsNumber) {
            if (c == '+' || c == '-') {

            } else if (c == '.') {
                if (bFindDot) {
                    if (exactnes != NO_EXACTNESS || radix != NO_RADIX)
                        goto Error0;
                    else
                        bIsNumber = FALSE;
                }
                bFindDot = TRUE;
            } else if (c == '/') {

            } else if (c == 'i') {

            } else {
                switch (radix) {
                case BIN:
                    if (!(c >= '0' && c <= '1')) {
                        goto Error1;
                    }
                    break;
                case OCT:
                    if (!(c >= '0' && c <= '7')) {
                        goto Error1;
                    }
                    break;
                case DEC:
                    if (!(c >= '0' && c <= '9')) {
                        goto Error1;
                    }
                    break;
                case HEX:
                    if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
                        goto Error1;
                    }
                    break;
                default:
                    if (!(c >= '0' && c <= '9')) {
                        bIsNumber = FALSE;
                    }
                    break;
                }
            }
        } else {
            break;
        }
    }

    if (len > 0) {
        xtod(pStart, len);
    }
    if (*p == 'i') {

    } else if (*p == '/') {

    } else if (*p == '+' || *p == '-') {

    } else if (*p != '\0') {

    } else {

    }

Error0:
    return NULL;
Error1:
    return NULL;
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

