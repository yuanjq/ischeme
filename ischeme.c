#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ischeme.h"

#define CELL_TRUE       &g_true
#define CELL_FALSE      &g_false
#define CELL_NIL        &g_nil
#define CELL_EOF        (Cell*)-1
#define DELIMITERS      "()[]{}\";\f\t\v\n\r "
#define READERS_NUM     128

static IScheme g_isc = {0};
static Cell* op_func(IScheme*, int);
static OpCode g_opcodes[] = {
    #define _OPCODE(f, n, t, o) {f, n, t},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};

static Reader g_readers[READERS_NUM];
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
static bool is_pair(Cell *c)     { return c && c->t == PAIR; }
static bool is_symbol(Cell *c)	 { return c && c->t == SYMBOL; }
static bool is_port(Cell *c)     { return c && c->t == PORT; }
static bool is_conti(Cell *c)    { return c && c->t == CONTI; }
static bool is_port_eof(Cell *c) { return c && c->t == PORT && c->port && c->port->t & PORT_EOF; }

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

static Cell *mk_bool(bool b) {
    if (b)
        return CELL_TRUE;
    else
        return CELL_FALSE;
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

static Cell *mk_number(Number *num)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = NUMBER;
        c->num = num;
    }
    return c;
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
        isc->contis = cons(c, isc->contis);
    }
    return c;
}


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

static int skip_space(IScheme *isc) {
    int c = 0, curLine = 0;
    do {
        c = get_char(isc);
        if (c == '\n') ++curLine;
    } while(isspace(c));
    if (isc->loadFiles[isc->curFileIdx] && isc->loadFiles[isc->curFileIdx]->port->t & PORT_FILE)
        isc->loadFiles[isc->curFileIdx]->port->f.curLine += curLine;
    if (c != EOF) {
        unget_char(isc, c);
        return 0;
    }
    return EOF;
}

static int get_token(IScheme *isc) {
    int c = skip_space(isc);
    //TODO
}

static inline Cell *read_port(Cell *port)
{
    Cell *ret = CELL_EOF;
    int c;
    while (ret == NULL) {
        while (isspace((c = get_char(port))));
        if (c < 0 || c >= READERS_NUM) return CELL_EOF;
        ret = g_readers[c](isc, c);
    }
    return ret;
}

static inline Cell *retn_helper(IScheme *isc, Cell *v) {

    if (v == CELL_EOF || !is_pair(isc->contis) || !is_conti(car(isc->contis)))
        return CELL_FALSE;
    Conti *conti = car(isc->contis);
    isc->retnv = v;
    isc->op = conti->op;
    isc->args = conti->args;
    isc->code = conti->code;
    isc->envir = conti->envir;
    isc->contis = cdr(isc->contis);
    return CELL_TRUE;
}

#define gotoOp(sc, o)      { sc->op=o; goto Loop; }
#define retnOp(sc, r)      { return retn_helper(sc, r); }

static Cell *op_func0(IScheme *isc, int op)
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
        retnOp(isc, read_port(isc->inPort));
    case OP_REPL_EVAL:
    {

        break;
    }
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

static Cell *assq(Cell *key, Cell *list)
{
    for (; list; list = cdr(list))
        if (!strcmp(symbol(key), symbol(caar(list)))) return car(list);
    return CELL_NIL;
}

static Cell* mk_syntax(IScheme *isc, String s)
{
    Cell *c = internal(isc, s);
    c->t = SYNTAX;
    return c;
}

static Cell *find_envir(IScheme *isc, Cell *s)
{
    for (Cell *e = isc->globalEnvir; e; e = cdr(isc->globalEnvir)) {
        if (isCons(car(e)) && caar(e) == s)
            return car(e);
    }
    return NULL;
}

static Cell* mk_envir(IScheme *isc, Cell *s, Cell *v)
{
    Cell *e = NULL;
    if ((e = find_envir(isc, s))) {
        rplacd(e, v);
    } else {
        e = cons(s, v);
        isc->globalEnvir = cons(e, isc->globalEnvir);
    }
    return e;
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

static double xtod(char *num, int size, int radix)
{
    double dnum = 0;
    int i, j, k=-1, n=0;

    for (i=0; i < size; i++)
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

static Number *read_real(char **ppStart, char *pEnd, Radix radix)
{
    Number *number = NULL;
    char *p = *ppStart;
    int c = *p;
    uint8 sign = 0;
    bool bIsNumber = TRUE;
    bool bFindNum = FALSE;
    bool bFindDot = FALSE;
    bool bFindSlash = FALSE;
    char *pBuf = NULL;

    if (pEnd - p >= INTL_BUF_SIZE)
        goto Error;
    pBuf = malloc(INTL_BUF_SIZE);
    if (!pBuf)
        goto Error;

    if (c == '+') {
        sign = 1;
        ++p;
    }
    else if (c == '-') {
        sign = -1;
        ++p;
    }

    char *pStart = p;
    while (p < pEnd && (c = *p) != '\0') {
        if (c == '.') {
            if (bFindDot || bFindSlash)
                goto Error;
            bFindDot = TRUE;
        } else if (c == '/') {
            if (bFindDot || bFindSlash || !bFindNum)
                goto Error;
            bufLen = p - pStart + 1;
            memcpy(pBuf, pStart, bufLen - 1);
            pBuf[bufLen - 1] = '\0';
            pStart = p + 1;
            bFindSlash = TRUE;
            bFindNum = FALSE;
        } else {
            switch (radix) {
            case BIN:
                if (!(c >= '0' && c <= '1')) {
                    goto Error;
                }
                break;
            case OCT:
                if (!(c >= '0' && c <= '7')) {
                    goto Error;
                }
                break;
            case DEC:
                if (!(c >= '0' && c <= '9')) {
                    goto Error;
                }
                break;
            case HEX:
                if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
                    goto Error;
                }
                break;
            default:
                if (!(c >= '0' && c <= '9')) {
                    bIsNumber = FALSE;
                    goto Error;
                }
                break;
            }
            bFindNum = TRUE;
        }
        ++p;
    }
    pEnd = p;

    if (bFindSlash) {
        if (!bFindNum)
            goto Error;
        Number *nr = malloc(sizeof(Number));
        if (!nr) {
            goto Error;
        }
        nr->t = NUMBER_LONG;
        nr->l = xtod(pBuf, bufLen, radix);

        bufLen = pEnd - pStart + 1;
        memcpy(pBuf, pStart, bufLen - 1);
        pBuf[bufLen - 1] = '\0';
        Number *dr = malloc(sizeof(Number));
        if (!dr) {
            free(nr);
            goto Error;
        }
        dr->t = NUMBER_LONG;
        dr->l = xtod(pBuf, bufLen, radix);

        number = malloc(sizeof(Number));
        if (!number) {
            free(nr);
            free(dr);
            goto Error;
        }
        number->t = NUMBER_FRACTION;
        number->fn.nr = nr;
        number->fn.dr = dr;
    } else {
        if (!bFindNum)
            goto Error;
        number = malloc(sizeof(Number));
        if (!number)
            goto Error;

        bufLen = pEnd - pStart + 1;
        memcpy(pBuf, pStart, bufLen - 1);
        pBuf[bufLen - 1] = '\0';
        double db = xtod(pBuf, pStart, bufLen - 1);
        if (bFindDot) {
            number->t = NUMBER_DOUBLE;
            number->d = db;
        } else {
            number->t = NUMBER_LONG;
            number->d = db;
        }
    }
    *ppStart = pEnd;

Error:
    if (pBuf) {
        free(pBuf);
    }
    return number;
}

static Cell *read_alpha(IScheme *isc, int c)
{
    Number *real = NULL;
    unget_char(isc->inPort, c);
    char *p = isc->inBuff;
    int totalLen = read_upto(isc->inPort, p, DELIMITERS);

    if (totalLen <= 0) {
        IError("read error.");
        return NULL;
    }

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

    if (p < pEnd) {
        real = read_real(&p, pEnd, radix);
        if ((!real && (radix != NO_RADIX || exactness != NO_EXACTNESS)) || p > pEnd)
            goto Error1;
        if (p == pEnd) {
            if (!real)
                return mk_symbol(isc->inBuff);
            else
                return mk_number(real);
        } else {
            c = *p;
            if (c == '+' || c == '-') {
                Number *imag = read_real(&p, pEnd, radix);
                c = *p;
                if (!imag && (radix != NO_RADIX || exactness != NO_EXACTNESS)) {
                    goto Error1;
                }
                else if (p != pEnd - 1 || (c != 'i' && c != 'I')) {
                    goto MkSymbol;
                }

                Number *num = malloc(sizeof(Number));
                if (!num) {
                    goto Error1;
                }
                num->t = NUMBER_COMPLEX;
                num->cx.rl = real;
                num->cx.im = imag;
                return mk_number(num);
            } else if (c == 'i' || c == 'I') {
                if (p != pEnd - 1) {
                    goto MkSymbol;
                }

                Number *num = malloc(sizeof(Number));
                if (!num) {
                    goto Error1;
                }
                num->t = NUMBER_COMPLEX;
                num->cx.rl = real;
                num->cx.im = imag;
                return mk_number(num);
            } else {
                goto MkSymbol;
            }
        }
    }
    goto Error1;

MkSymbol:
    if (real) {
        free(real);
    }
    return mk_symbol(isc->inBuff);
Error0:
    IError("bad syntax '%s'.\n", p);
    return NULL;
Error1:
    if (real) {
        free(real);
    }
    IError("read error.\n");
    return NULL;
}

static Cell *read_hash(IScheme *isc, int c)
{

}

static Cell *read_string(IScheme *isc, int q)
{
    char *buf = isc->inBuff;
    Cell *port = isc->inPort;
    int idx = 0;
    int c;
    while (idx < INTL_BUF_SIZE && (c = get_char(port)) > 0 && c != q)
    {
        if ('\\' == c)
        {
            buf[idx++] = c;
            buf[idx++] = get_char(port);
        }
        else
            buf[idx++] = c;
    }
    if (c != q) IError("EOF in string.\n");
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
    Cell *head, *tail, *cell;
    head = tail = cons(CELL_NIL, CELL_NIL);

    switch (c) {
    case '(': c = ')'; break;
    case '[': c = ']'; break;
    case '{': c = '}'; break;
    }

    int d;
    bool isPair = FALSE;
    bool shouldEnd = FALSE;
    Cell *port = isc->inPort;
    for (;;) {
        while (isspace((d = get_char(port))));
        if (c == d) break;
        if (is_port_eof(port)) {
            IError("end of file.");
            return CELL_EOF;
        }
        if (d == ')' || d == ']' || d == '}') {
            IError("unexcepted '%c'.\n", d);
            return CELL_EOF;
        }

        if (d == '.') {
            d = get_char(port);
            if (strchr(" \n\t", d)) {
                // TODO
                rplacd(tail, cons(cell, CELL_NIL));
            } else {
                unget_char(port, d);
                unget_char(port, '.');
            }
        } else {
            unget_char(port, d);
        }

        cell = read_port(port);
        if (cell == CELL_EOF)
            return CELL_EOF;
        tail = rplacd(tail, cons(cell, CELL_NIL));
    }

    return cdr(head);
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
    g_true.t = BOOL;
    g_true.chr = TRUE;
    g_false.t = BOOL;
    g_false.chr = FALSE;

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
    init_readers(read_alpha,   "+-");
    init_readers(read_string, "\"");
    init_readers(read_quote,  "'");
    init_readers(read_quasiquote, "`");
    init_readers(read_unquote, ",");
    init_readers(read_list,   "([{");
    init_readers(read_semi,   ";");

    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) mk_syntax(&g_isc, g_opcodes[i].name);
            mk_envir(&g_isc, internal(&g_isc, g_opcodes[i].name), mk_long(i));
        }
    }
}

static void isc_repl()
{
    g_isc.op = OP_REPL_LOOP;
    for (;;) {
        if (g_opcodes[g_isc.op].func(&g_isc, g_isc.op) != CELL_TRUE)
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

