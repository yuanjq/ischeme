#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include "ischeme.h"

#define CELL_TRUE       &g_true
#define CELL_FALSE      &g_false
#define CELL_NIL        &g_nil
#define CELL_EOF        &g_eof
#define CELL_UNDEF      &g_undef
#define CELL_ERR        (Cell*)-1
#define DELIMITERS      "()[]{}\";\f\t\v\n\r "

#define SyntaxError     "SyntaxError"
#define MemoryError     "MemoryError"
#define IndexError      "IndexError"
#define IOError         "IOError"
#define TypeError       "TypeError"
#define ValueError      "ValueError"
#define ReferenceError  "ReferenceError"
#define ArithmeticError "ArithmeticError"

#define gotoOp(sc, o)      	({ sc->op=o; return CELL_TRUE; })
#define popOp(sc, r)       	return pop_op(sc, r)
#define pushOp(sc,o,a,c)   	push_op(sc, o, a, c)
#define jmpErr(et,eb,...)	({ print_err(gp_isc,et,eb,##__VA_ARGS__); gp_isc->op = OP_ERROR; longjmp(gp_isc->jmpbuf, 0); })
#define closure_args(c)     c->clsr->args
#define closure_code(c)     c->clsr->code
#define closure_env(c)      c->clsr->env
#define contis_car(c)       c->pair->a
#define contis_cdr(c)       c->pair->d
#define find_envir(s,e)     assq(s,e)

static IScheme *gp_isc = NULL;
static Reader g_readers[TOK_MAX];
static Cell g_true = {t:BOOLEAN, {chr:TRUE}};
static Cell g_false = {t:BOOLEAN, {chr:FALSE}};
static Cell g_nil;
static Cell g_eof;
static Cell g_undef;


#define T_ANY       "\001"
#define T_CHAR      "\002"
#define T_NUMBER    "\003"
#define T_INTEGER   "\004"
#define T_NATURAL   "\005"
#define T_STRING    "\006"
#define T_SYMBOL    "\007"
#define T_PAIR      "\010"
#define T_LIST      "\011"
#define T_VECTOR    "\012"
#define T_PROC      "\013"
#define T_ENVIR     "\014"
#define T_CONTI     "\015"
#define T_PORT      "\016"
#define T_INPORT    "\017"
#define T_OUTPORT   "\020"

static Cell* op_func0(IScheme*, int);
static Cell* op_func1(IScheme*, int);
static Cell* op_func2(IScheme*, int);
static Cell* op_func3(IScheme*, int);
static OpCode g_opcodes[] = {
    #define _OPCODE(f, n, t1, o, m1, m2, t2) {f, n, t1, m1, m2, t2},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};

static const char *ascii32[32]={
	"nul",
	"soh",
	"stx",
	"etx",
	"eot",
	"enq",
	"ack",
	"bel",
	"bs",
	"ht",
	"lf",
	"vt",
	"ff",
	"cr",
	"so",
	"si",
	"dle",
	"dc1",
	"dc2",
	"dc3",
	"dc4",
	"nak",
	"syn",
	"etb",
	"can",
	"em",
	"sub",
	"esc",
	"fs",
	"gs",
	"rs",
	"us"
};


static void print_err(IScheme *isc, String, String, ...);


/***************** memory manager ***************/
static int seg_alloc(IScheme *isc, int num)
{
    if (isc->last_seg + num >= SEGS_NUM) return 0;

    for (int i=0; i<num; i++)
    {
        int idx = ++isc->last_seg;
        isc->segs[idx] = (Cell*)malloc(SEG_MEM_SIZE);
        Cell *new = isc->segs[idx];
        isc->free_cell_count += SEG_CELLS_NUM;

        Cell *pLast = new + SEG_CELLS_NUM - 1;
        for (Cell *c = new; c <= pLast; c++) c->next = c + 1;

        pLast->next = isc->free_cells;
        isc->free_cells = new;
    }

    return num;
}

static void gc(IScheme *isc)//, Cell *args, Cell *env)
{
    ITraceEnter();
    // TODO:
    ITraceLeave();
}

static Cell *cell_alloc()//Cell *args, Cell *env)
{
    if (!gp_isc->free_cells) {
        gc(gp_isc);//, args, env);
        if (!gp_isc->free_cells && seg_alloc(gp_isc, 1) <= 0) {
            jmpErr(MemoryError, "no memery.");
        }
    }
    Cell *c = gp_isc->free_cells;
    gp_isc->free_cells = c->next;
    --gp_isc->free_cell_count;
    return c;
}


/****************** syntax ******************/
static bool is_nil(Cell *c)       { return c == CELL_NIL; }
static bool is_any(Cell *c)       { return TRUE; }
static bool is_char(Cell *c)     { return c && T(c) == CHAR; }
static bool is_number(Cell *c)  { return c && T(c) == NUMBER; }
static bool is_integer(Cell *c) { return is_number(c) && c->num->t == NUMBER_LONG; }
static bool is_natural(Cell *c) { return is_integer(c) && c->num->l >= 0; }
static bool is_string(Cell *c)  { return c && T(c) == STRING; }
static bool is_pair(Cell *c)     { return c && T(c) == PAIR; }
static bool is_vector(Cell *c)  { return c && T(c) == VECTOR; }
static bool is_symbol(Cell *c)  { return c && T(c) == SYMBOL; }
static bool is_syntax(Cell *c)  { return c && T(c) == SYNTAX; }
static bool is_proc(Cell *c)     { return c && T(c) == PROC; }
static bool is_iproc(Cell *c)   { return c && T(c) == IPROC; }
static bool is_eproc(Cell *c)   { return c && T(c) == EPROC; }
static bool is_envir(Cell *c)   { return c && T(c) == ENVIR; }
static bool is_macro(Cell *c)   { return c && T(c) == MACRO; }
static bool is_promise(Cell *c) { return c && T(c) == PROMISE; }
static bool is_port(Cell *c)     { return c && T(c) == PORT; }
static bool is_inport(Cell *c)  { return is_port(c) && c->port->t & PORT_INPUT; }
static bool is_outport(Cell *c) { return is_port(c) && c->port->t & PORT_OUTPUT; }
static bool is_conti(Cell *c)    { return c && T(c) == CONTI; }
static bool is_contis(Cell *c)    { return c && T(c) == CONTIS; }
static bool is_port_eof(Cell *c) { return c && T(c) == PORT && c->port && c->port->t & PORT_EOF; }
static bool is_immutable(Cell *c) { return c && c->t & M_IMMUTABLE; }

static bool is_interactive(IScheme *isc) {
    return isc->cur_file_idx == 0 &&
           isc->load_files[0]->port->t & PORT_FILE &&
           isc->load_files[0]->port->f.file == stdin;
}

static String symbol(Cell *c)   { return is_symbol(c) ? c->str: NULL; }
static String string(Cell *c)   { return is_string(c) ? c->str: NULL; }
static Port* port(Cell *c)       { return is_port(c) ? c->port : NULL; }

#define car(c)      ({ c && T(c) == PAIR ? c->pair->a : NULL; })
#define cdr(c)      ({ c && T(c) == PAIR ? c->pair->d : NULL; })
#define caar(c)     car(car(c))
#define cadr(c)     car(cdr(c))
#define cdar(c)     cdr(car(c))
#define cddr(c)     cdr(cdr(c))
#define caaar(c)    car(car(car(c)))
#define caadr(c)    car(car(cdr(c)))
#define cadar(c)    car(cdr(car(c)))
#define caddr(c)    car(cdr(cdr(c)))
#define cdaar(c)    cdr(car(car(c)))
#define cdadr(c)    cdr(car(cdr(c)))
#define cddar(c)    cdr(cdr(car(c)))
#define cdddr(c)    cdr(cdr(cdr(c)))

static Cell *rplaca(Cell *c, Cell *a)    { return is_pair(c) ? c->pair->a = a : NULL; }
static Cell *rplacd(Cell *c, Cell *d)    { return is_pair(c) ? c->pair->d = d : NULL; }


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

static bool is_list(Cell *c) {
    for (; is_pair(c); c = cdr(c));
    if (c == CELL_NIL) return TRUE;
    return FALSE;
}

static String string_dup(String str) {
    char *s = malloc(strlen(str) + 1);
    if (s) {
        strcpy(s, str);
    }
    return s;
}

static Cell *mk_string(String str) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = STRING;
        c->str = str;
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

static Cell *mk_long(long n) {
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

static Cell *mk_double(double n) {
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

static Cell *mk_number(Number *num) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = NUMBER;
        c->num = num;
    }
    return c;
}

static Cell *mk_symbol(String s) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = SYMBOL;
        c->str = string_dup(s);
    }
    return c;
}

static Cell *mk_closure(Cell *a, Cell *e) {
    Cell *c = NULL;
    Closure *clsr = malloc(sizeof(Closure));
    if (clsr) {
        c = cell_alloc();
        if (c) {
            c->t = CLOSURE;
            c->clsr = clsr;
            clsr->args = car(a);
            clsr->code = cdr(a);
            clsr->env = cons(CELL_NIL, cdr(e));
        }
    }
    return c;
}

static Cell *mk_proc(Cell *a, Cell *e) {
    Cell *c = mk_closure(a, e);
    c->t = PROC;
    return c;
}

static Cell *mk_port(FILE *f, String name, int t) {
    Cell *c = NULL;
    Port *port = (Port*)malloc(sizeof(Port));
    if (port) {
        c = cell_alloc();
        if (c) {
            c->t = PORT;
            c->port = port;
            port->t = t;
            port->f.file = f;
            if (name) {
                port->f.name = string_dup(name);
            }
        }
    }
    return c;
}

static Cell *mk_syntax(int op) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = SYNTAX;
        c->chr = op;
    }
    return c;
}

static Cell *mk_iproc(int op) {
    Cell *c = cell_alloc();
    if (c) {
        c->t = IPROC;
        c->chr = op;
    }
    return c;
}

static Cell *mk_contis(IScheme *isc) {
    Cell *c = cons(car(isc->contis), cdr(isc->contis));
    c->t = CONTIS;
    return c;
}


/********************* stack frame ********************/
static void print_err(IScheme *isc, String etype, String ebody, ...) {
    va_list ap;
    va_start(ap, ebody);

    Port *in = isc->load_files[isc->cur_file_idx]->port;
    Port *out = isc->outport->port;
    fprintf(out->f.file, "%s", etype);
    fprintf(out->f.file, ": ");
    vfprintf(out->f.file, ebody, ap);
    va_end(ap);
    if (in->t & PORT_FILE && in->f.file != stdin)
        fprintf(out->f.file, "\n\t at %s:%d\n", out->f.name, out->f.curr_line);
    else
        fputc('\n', out->f.file);
}

static inline Cell *pop_op(IScheme *isc, Cell *v) {
    if (!is_pair(isc->contis) || !is_conti(car(isc->contis)))
        return CELL_FALSE;
    Conti *conti = car(isc->contis)->conti;
    isc->retnv = v;
    isc->op = conti->op;
    isc->args = conti->args;
    isc->code = conti->code;
    isc->envir = conti->envir;
    isc->contis = cdr(isc->contis);
    return CELL_TRUE;
}

static void push_op(IScheme *isc, Op op, Cell *args, Cell *code) {
    Conti *conti = (Conti*)malloc(sizeof(Conti));
    if (conti) {
        Cell *c = cell_alloc();
        if (c) {
            c->t = CONTI;
            c->conti = conti;
            c->conti->op = op;
            c->conti->args = args;
            c->conti->envir = isc->envir;
            c->conti->code = code;
        }
        isc->contis = cons(c, isc->contis);
    }
}


/***************** I/O handler ******************/
static void port_close(IScheme *isc, Cell* p, int f) {
	Port *port = p->port;
	port->t &= ~f;
	if((port->t & (PORT_INPUT | PORT_OUTPUT)) == 0) {
		if(port->t & PORT_FILE) {
			if(port->f.name) free(port->f.name);
			fclose(port->f.file);
		}
		port->t = PORT_FREE;
	}
}

static int push_load_file(IScheme *isc, String name) {
	if (isc->cur_file_idx == MAX_LOAD_FILES-1) return 0;
	FILE *fin = fopen(name, "r");
	if(fin!=0) {
		isc->cur_file_idx++;
		isc->load_files[isc->cur_file_idx]->port->t= PORT_FILE | PORT_INPUT;
		isc->load_files[isc->cur_file_idx]->port->f.file = fin;
		isc->load_files[isc->cur_file_idx]->port->f.curr_line = 0;
		if(name) isc->load_files[isc->cur_file_idx]->port->f.name = string_dup(name);
		isc->inport= (Cell*)isc->load_files + isc->cur_file_idx;
	}
	return fin != 0;
}

static void pop_load_file(IScheme *isc) {
	if(isc->cur_file_idx != 0) {
		port_close(isc, isc->inport, PORT_INPUT);
		--isc->cur_file_idx;
		isc->inport = (Cell*)isc->load_files + isc->cur_file_idx;
	}
}

int write_char(Cell *out, int c) {
	Port *pt = out->port;
	if(pt->t & PORT_FILE) {
        c = fputc(c, pt->f.file);
        fflush(pt->f.file);
		return c;
	} else {
		if(pt->s.end <= pt->s.cur) {
           jmpErr(MemoryError, "write char out of range.");
        }
		*pt->s.cur++ = c;
        return c;
	}
    jmpErr(IOError, "invalid out port.");
}

int write_string(Cell *out, String s) {
	Port *pt= out->port;
	int len = strlen(s);
	if (pt->t & PORT_FILE) {
        len = fwrite(s, 1, len, pt->f.file);
        fflush(pt->f.file);
		return len;
	} else if (pt->t & PORT_STRING) {
	    if (len > pt->s.end - pt->s.cur) {
           jmpErr(MemoryError, "write string out of range.");
        }
		for(; len; len--) *pt->s.cur++ = *s++;
        return len;
	}
    jmpErr(IOError, "invalid out port.");
}

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
        if (p->s.cur != p->s.start) --p->s.cur;
    }
}

static inline int skip_line(IScheme *isc) {
    int c = 0, n = 0;
    while (c = get_char(isc->inport) != EOF && c != '\n') ++n;
    if (c == '\n') {
        if (isc->load_files[isc->cur_file_idx] && isc->load_files[isc->cur_file_idx]->port->t & PORT_FILE)
            ++isc->load_files[isc->cur_file_idx]->port->f.curr_line;
        return ++n;
    }
    return EOF;
}

static int skip_comment(IScheme *isc) {
    int n = 0;
    int c = get_char(isc->inport);
    if (c == EOF) return EOF;
    if (c == ';') {
        ++n;
        c = skip_line(isc);
        if (c == EOF) return EOF;
        n += c;
    } else if (c == '#') {
        c = get_char(isc->inport);
        if (c == EOF) return EOF;
        if (c != '!')  {
            unget_char(isc->inport, c);
            unget_char(isc->inport, '#');
            return 0;
        }
        n += 2;
        c = skip_line(isc);
        if (c == EOF) return EOF;
        n += c;
    } else {
        unget_char(isc->inport, c);
        return 0;
    }
    if (c == '\n') return ++n;
    return EOF;
}

static int skip_space(IScheme *isc) {
    int c = 0, n = 0, curr_line = 0;
    do {
        c = get_char(isc->inport);
        if (c == '\n') ++curr_line;
        else if (c == ';' || c == '#') {
            unget_char(isc->inport, c);
            c = skip_comment(isc);
            if (c <= 0) return n;
            n += c;
            continue;
        }
        ++n;
    } while(isspace(c));
    if (isc->load_files[isc->cur_file_idx] && isc->load_files[isc->cur_file_idx]->port->t & PORT_FILE)
        isc->load_files[isc->cur_file_idx]->port->f.curr_line += curr_line;
    if (c != EOF) {
        unget_char(isc->inport, c);
        return --n;
    }
    return EOF;
}

static Cell *reverse(IScheme *isc, Cell *old) {
    Cell *new = CELL_NIL;
    for (; is_pair(old); old = cdr(old))
        new = cons(car(old), new);
    return new;
}

static int get_token(IScheme *isc) {
    int c = skip_space(isc);
    if (c == EOF) return TOK_EOF;
    switch (c = get_char(isc->inport)) {
    case EOF: return TOK_EOF;
    case '(': return TOK_LPAREN;
    case ')': return TOK_RPAREN;
    case '[': return TOK_LBRACKET;
    case ']': return TOK_RBRACKET;
    case '{': return TOK_LBRACE;
    case '}': return TOK_RBRACE;
    case '.':
        if (skip_space(isc) > 0) return TOK_DOT;
        unget_char(isc->inport, c);
        return TOK_SYMBOL;
    case '\'': return TOK_QUOTE;
    case '`': return TOK_QQUOTE;
    case '"': return TOK_DQUOTE;
    case ',':
        c = get_char(isc->inport);
        if (c == '@') return TOK_UNQUOTE_SPLICING;
        else unget_char(isc->inport, c);
        return TOK_UNQUOTE;
    case '#':
        c = get_char(isc->inport);
        if (c == '(') return TOK_VECTOR;
        else if (c == '!') {
            c = skip_line(isc);
            if (c == EOF) return TOK_EOF;
            return get_token(isc);
        } else if (strchr("tfeibodx\\", c)) {
            unget_char(isc->inport, c);
            unget_char(isc->inport, '#');
            return TOK_CONST;
        }
        jmpErr(SyntaxError, "bad syntax '#%c'.", c);
    case ';':
        c = skip_line(isc);
        if (c == EOF) return TOK_EOF;
        return get_token(isc);
    default:
        unget_char(isc->inport, c);
        return TOK_SYMBOL;
    }
}

static Cell *read_cell(IScheme *isc) {
    int t = get_token(isc);
    if (t == TOK_EOF) {
        jmpErr(IOError, "end of file.");
    }
    return g_readers[t](isc, t);
}

static Cell *read_cell_by_token(IScheme *isc, int t) {
    if (t == TOK_EOF) {
        jmpErr(IOError, "end of file.");
    }
    return g_readers[t](isc, t);
}

static bool num_equal(Number *a, Number *b) {
    if (!a || !b) return FALSE;
    if (a->t != b->t) return FALSE;
    switch (a->t) {
    case NUMBER_LONG:
        return a->l == b->l;
    case NUMBER_DOUBLE:
        return a->d == b->d;
    case NUMBER_FRACTION:
        return (a->fn.nr->l == b->fn.nr->l && a->fn.dr == b->fn.dr);
    case NUMBER_COMPLEX:
        return num_equal(a->cx.rl, b->cx.rl) && num_equal(a->cx.im, b->cx.im);
    }
    return FALSE;
}

static bool equal(Cell *a, Cell *b) {
    if (a == b) return TRUE;
    if (a->t != b->t) return FALSE;
    switch (a->t) {
    case CHAR:
        return a->chr == b->chr;
    case NUMBER:
        return num_equal(a->num, b->num);
    case STRING:
    case SYMBOL:
        if (a->str && b->str) return !strcmp(a->str, b->str);
        break;
    }
    return FALSE;
}

static Cell *find_symbol(IScheme *isc, String s) {
    for (Cell *c = isc->symbols; c; c = cdr(c)) {
        String sym = symbol(car(c));
        if (sym && !strcmp(sym, s))
          return car(c);
    }
    return CELL_NIL;
}

static Cell* internal(IScheme *isc, String s) {
    Cell *c = NULL;
    if ((c = find_symbol(isc, s)) != CELL_NIL) return c;
    c = mk_symbol(s);
    isc->symbols = cons(c, isc->symbols);
    return c;
}

static Cell *assq(Cell *key, Cell *list) {
    Cell *c = CELL_NIL;
    if (!is_symbol(key)) return CELL_NIL;
    for (; is_pair(list); list = cdr(list)) {
        if (is_pair(c = car(list)) && equal(car(c), key)) {
            return c;
        }
    }
    return CELL_NIL;
}

static int length(Cell *list) {
    int len = 0;
    for (; is_pair(list); list = cdr(list)) ++len;
    return len;
}

static Cell *set_envir(Cell *env, Cell *s, Cell *v) {
    Cell *e = CELL_NIL;
    if ((e = find_envir(s, cdr(env))) != CELL_NIL) {
        rplacd(e, v);
    } else {
        e = cons(s, v);
        rplacd(env, cons(e, cdr(env)));
    }
    return e;
}

static Cell* mk_envir(Cell *env, Cell *s, Cell *v) {
    Cell *e = cons(s, v);
    rplacd(env, cons(e, cdr(env)));
    return e;
}

static Cell *read_illegal(IScheme *isc, int c) {
    return CELL_EOF;
}

static inline int read_upto(Cell *port, char *upto, char *out, int size) {
    if (!is_port(port) || !out || size <= 0)
        return -1;
    int c;
    char *p = out;

    for (;;) {
        if (p - out < size) {
            if ((c = get_char(port)) < 0) {
                return -1;
            }
            if (strchr(upto, c))
                break;
            *p++ = c;
        } else {
            return -1;
        }
    }
    unget_char(port, c);
    *p = '\0';
    return p - out;
}

static bool start_with(char *s, char *w) {
    char *p1, *p2;
    p1 = s; p2 = w;
    while (*p1 != '\0' && *p2 != '\0' && *p1++ == *p2++);
    if (*p2 == '\0')
        return TRUE;
    else
        return FALSE;
}

static double xtod(char *num, int size, int radix) {
    double dnum = 0;
    int i, j, k=-1, n=0;

    for (i=0; i < size; i++) {
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
    for(i = j-1; i >= 0; i--) {
        if (num[j-i-1] >= 'a' && num[j-i-1] <= 'f') {
            n = num[j-i-1] - 'a' + 10;
        } else if (num[j-i-1] >= 'A' && num[j-i-1] <= 'F') {
            n = num[j-i-1] - 'A' + 10;
        } else  if (num[j-i-1] >= '0' && num[j-i-1] <= '9') {
            n = num[j-i-1] - '0';
        }
        dnum += n * pow(radix, i);
    }

    if(k >= 0) {
        for(i=0; i<k; i++) {
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

static String cell_to_string(Cell *c) {
    return NULL;
}

static void write_cell(Cell *port, Cell *c, bool readable, bool more, bool top) {
	String s = gp_isc->buff;
	if (c == CELL_NIL) {
		s = "()";
	} else if (c == CELL_TRUE) {
		s = "#t";
	} else if (c == CELL_FALSE) {
		s = "#f";
	} else if (c == CELL_EOF) {
		s = "#<EOF>";
	} else if (is_port(c)) {
		snprintf(s, STR_BUF_SIZE, "#<PORT:%p>", c);
	} else if (is_symbol(c)) {
	    if (more) {
            if (top) write_string(port, "#<SYMBOL:");
            write_string(port, symbol(c));
            if (top) write_string(port, ">");
            return;
        } else {
		    snprintf(s, STR_BUF_SIZE, "#<SYMBOL:%s>", symbol(c));
        }
	} else if (is_macro(c)) {
		snprintf(s, STR_BUF_SIZE, "#<MACRO:%p>", c);
	} else if (is_promise(c)) {
	   	snprintf(s, STR_BUF_SIZE, "#<PROMISE:%p>", c);
	} else if (is_proc(c) || is_iproc(c) || is_eproc(c)) {
	    if (is_proc(c) && more) {
            if (top) write_string(port, "#<PROCEDURE>\n");
            write_string(port, "(lambda ");
            write_cell(port, c->clsr->args, readable, more, 0);
            write_string(port, " ");
            write_cell(port, c->clsr->code, readable, more, 0);
            write_string(port, ")");
            return;
        } else {
		    snprintf(s, STR_BUF_SIZE, "#<PROCEDURE:%p>", c);
        }
	} else if (is_contis(c)) {
		snprintf(s, STR_BUF_SIZE, "#<CONTINUATION:%p>", c);
	} else if (is_number(c)) {
		Number *num = c->num;
        if (num->t == NUMBER_LONG) {
            snprintf(s, STR_BUF_SIZE, "%ld", num->l);
        } else if (num->t == NUMBER_DOUBLE) {
            snprintf(s, STR_BUF_SIZE, "%lf", num->d);
        } else if (num->t == NUMBER_FRACTION) {
            if (num->fn.nr->t == NUMBER_DOUBLE || num->fn.dr->t == NUMBER_DOUBLE) {
                snprintf(s, STR_BUF_SIZE, "%lf/%lf", num->fn.nr->d, num->fn.dr->d);
            } else {
                snprintf(s, STR_BUF_SIZE, "%ld/%ld", num->fn.nr->l, num->fn.dr->l);
            }
        } else {
            // TODO
        }
	} else if (is_string(c)) {
	    char *fmt;
	    if (readable) fmt = "%s";
        else fmt = "\"%s\"";
        snprintf(s, STR_BUF_SIZE, fmt, string(c));
	} else if (is_char(c)) {
	    if (readable) {
            snprintf(s, STR_BUF_SIZE, "%c", c->chr);
        } else {
    		switch(c->chr) {
    		case ' ': s = "#\\space"; break;
    		case '\n': s = "#\\newline"; break;
    		case '\r': s = "#\\return"; break;
    		case '\t': s =  "#\\tab"; break;
    		default:
    			if(c->chr == 127) {
    				s = "#\\del"; break;
    			} else if(c->chr < 32) {
    				snprintf(s, STR_BUF_SIZE, "#\\%s", ascii32[c->chr]);
    				break;
    			}
    			snprintf(s, STR_BUF_SIZE, "#\\%c", c->chr); break;
    			break;
    		}
        }
    } else if (is_pair(c)) {
        if (more) {
            if (top) write_string(port, "#<PAIR>\n");
            write_string(port, "(");
            write_cell(port, car(c), readable, more, 0);
            write_string(port, " ");
            write_cell(port, cdr(c), readable, more, 0);
            write_string(port, ")");
            return;
        } else {
            snprintf(s, STR_BUF_SIZE, "#<PAIR:%p>", c);
        }
    } else {
        snprintf(s, STR_BUF_SIZE, "unknown:%p", c);
    }

    write_string(port, s);
}

static void print_cell_more(Cell *c, Cell *p) {
    write_cell(p, c, 0, 1, 1);
}

static void print_cell_readable(Cell *c, Cell *p) {
    write_cell(p, c, 1, 0, 1);
}

static void print_cell(Cell *c, Cell *p) {
    write_cell(p, c, 0, 0, 1);
}

static Number *exact_to_inexact(Number *num) {
    switch (num->t) {
    case NUMBER_LONG:
        num->t = NUMBER_DOUBLE;
        num->d = num->l;
        break;
    case NUMBER_DOUBLE:
        return num;
    case NUMBER_FRACTION:
    {
        num->t = NUMBER_DOUBLE;
        Number *nr = num->fn.nr;
        Number *dr = num->fn.dr;
        num->d = nr->l / dr->l;
        free(nr);
        free(dr);
        break;
    }
    case NUMBER_COMPLEX:
        exact_to_inexact(num->cx.rl);
        exact_to_inexact(num->cx.im);
        break;
    }
    return num;
}

static Number *inexact_to_exact(Number *num) {
    switch (num->t) {
    case NUMBER_LONG:
        return num;
    case NUMBER_DOUBLE:
        // TODO: double to faction.
        num->t = NUMBER_LONG;
        num->l = num->d;
        break;
    case NUMBER_FRACTION:
        return num;
    case NUMBER_COMPLEX:
        inexact_to_exact(num->cx.rl);
        inexact_to_exact(num->cx.im);
        break;
    }
    return num;
}

static Number *read_real(char **ppstart, char *pend, Exactness exact, Radix radix) {
    Number *number = NULL;
    char *p = *ppstart;
    int c = *p;
    unsigned char sign = 0;
    int buflen = 0;
    bool find_num = FALSE;
    bool find_dot = FALSE;
    bool find_slash = FALSE;
    char *buf = NULL;

    if (pend - p >= STR_BUF_SIZE) {
        goto Error;
    }
    buf = malloc(STR_BUF_SIZE);
    if (!buf) {
        goto Error;
    }

    if (c == '+') {
        sign = 1;
        ++p;
    } else if (c == '-') {
        sign = -1;
        ++p;
    }

    char *pstart = p;
    while (p < pend && (c = *p) != '\0') {
        if (c == '.') {
            if (find_dot || find_slash) {
                goto Error;
            }
            find_dot = TRUE;
        } else if (c == '/') {
            if (find_dot || find_slash || !find_num) {
                goto Error;
            }
            buflen = p - pstart + 1;
            memcpy(buf, pstart, buflen - 1);
            buf[buflen - 1] = '\0';
            pstart = p + 1;
            find_slash = TRUE;
            find_num = FALSE;
        } else if (c == '+' || c == '-' || c == 'i' || c == 'I') {
            break;
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
                    goto Error;
                }
                break;
            }
            find_num = TRUE;
        }
        ++p;
    }

    if (radix == NO_RADIX)
        radix = DEC;
    pend = p;

    if (find_slash) {
        if (!find_num) {
            goto Error;
        }
        Number *nr = malloc(sizeof(Number));
        if (!nr) {
            goto Error;
        }
        nr->t = NUMBER_LONG;
        nr->l = xtod(buf, buflen, radix);

        buflen = pend - pstart + 1;
        memcpy(buf, pstart, buflen - 1);
        buf[buflen - 1] = '\0';
        Number *dr = malloc(sizeof(Number));
        if (!dr) {
            free(nr);
            goto Error;
        }
        dr->t = NUMBER_LONG;
        dr->l = xtod(buf, buflen, radix);

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
        if (!find_num) {
            goto Error;
        }
        number = malloc(sizeof(Number));
        if (!number) {
            goto Error;
        }
        buflen = pend - pstart + 1;
        memcpy(buf, pstart, buflen - 1);
        buf[buflen - 1] = '\0';
        double db = xtod(buf, buflen, radix);
        if (find_dot) {
            number->t = NUMBER_DOUBLE;
            number->d = db;
        } else {
            number->t = NUMBER_LONG;
            number->l = db;
        }
    }
    *ppstart = pend;
    if (exact == EXACT)
        inexact_to_exact(number);
    else if (exact == INEXACT)
        exact_to_inexact(number);

Error:
    if (buf) {
        free(buf);
    }
    return number;
}

static Cell *read_number(char **ppstart, char *pend, Exactness exact, Radix radix) {
    char *p = *ppstart;
    Number *real = NULL, *imag = NULL;
    
    real = read_real(&p, pend, exact, radix);
    if (!real || p > pend)
        goto Error;
 
    if (p == pend) {
        return mk_number(real);
    } else {
        int c = *p;
        if (c == '+' || c == '-') {
            imag = read_real(&p, pend, exact, radix);
            if (!imag || p > pend) {
                goto Error;
            }
            c = *p;
            if (p != pend - 1 || (c != 'i' && c != 'I')) {
                goto Error;
            }
            Number *num = malloc(sizeof(Number));
            if (!num) {
                goto Error;
            }
            num->t = NUMBER_COMPLEX;
            num->cx.rl = real;
            num->cx.im = imag;
            return mk_number(num);
        } else if (c == 'i' || c == 'I') {
            if (p != pend - 1) {
                goto Error;
            }

            Number *num = malloc(sizeof(Number));
            if (!num) {
                goto Error;
            }
            num->t = NUMBER_COMPLEX;
            num->cx.rl = 0;
            num->cx.im = real;
            return mk_number(num);
        }
    }
    
Error:
    if (real) free(real);
    if (imag) free(imag);
    return CELL_NIL;
}

static Cell *read_symbol(IScheme *isc, int c) {
    char *p = isc->buff;
    int total_len = read_upto(isc->inport, DELIMITERS, p, STR_BUF_SIZE);

    if (total_len <= 0) {
        jmpErr(IOError, "read error.");
    }
    Cell *num = read_number(&p, p + total_len, NO_EXACTNESS, NO_RADIX);
    if (num != CELL_NIL) {
        return num;
    }
    return mk_symbol(isc->buff);
}

static Cell *read_const(IScheme *isc, int c) {
    Number *real = NULL, *imag = NULL;
    char *p = isc->buff;
    int total_len = read_upto(isc->inport, DELIMITERS, p, STR_BUF_SIZE);

    if (total_len <= 0) {
        jmpErr(IOError, "read error.");
    }
    Exactness exact = NO_EXACTNESS;
    Radix radix = NO_RADIX;
    char *pend = p + total_len;
    while (p < pend && *p == '#') {
        if (p + 2 > pend)
            goto Error;
        switch (p[1]) {
        case 'b': case 'B':
            if (radix != NO_RADIX)
                goto Error;
            radix = BIN;
            break;
        case 'o': case 'O':
            if (radix != NO_RADIX)
                goto Error;
            radix = OCT;
            break;
        case 'd': case 'D':
            if (radix != NO_RADIX)
                goto Error;
            radix = DEC;
            break;
        case 'x': case 'X':
            if (radix != NO_RADIX)
                goto Error;
            radix = HEX;
            break;
        case 'e': case 'E':
            if (exact != NO_EXACTNESS)
                goto Error;
            exact = EXACT;
            break;
        case 'i': case 'I':
            if (exact != NO_EXACTNESS)
                goto Error;
            exact = INEXACT;
            break;
        case 'f': case 'F':
            if (exact != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pend)
                goto Error;
            return CELL_FALSE;
        case 't': case 'T':
            if (exact != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pend)
                goto Error;
            return CELL_TRUE;
        case '\\':
            if (exact != NO_EXACTNESS || radix != NO_RADIX || p + 2 == pend)
                goto Error;
            p += 2;
            if (pend - p > 1) {
                if (pend - p == 3 && !strncasecmp(p, "tab", 3)) {
                    return mk_char('\t');
                } else if (pend - p == 4 && !strncasecmp(p, "space", 4)) {
                    return mk_char(' ');
                } else if (pend - p == 6 && !strncasecmp(p, "return", 6)) {
                    return mk_char('\r');
                } else if (pend - p == 7 && !strncasecmp(p, "newline", 7)) {
                    return mk_char('\n');
                }
                goto Error;
            }
            return mk_char(p[0]);
        default:
            goto Error;
        }
        p += 2;
    }

    Cell *num = read_number(&p, pend, exact, radix);
    if (num != CELL_NIL) {
        return num;
    }

Error:
    jmpErr(SyntaxError, "bad syntax: %s", isc->buff);
}

static Cell *read_string(IScheme *isc, int q) {
    char *buf = isc->buff;
    Cell *port = isc->inport;
    int buf_size = STR_BUF_SIZE;
    int idx = 0;
    int c;
    while ((c = get_char(port)) > 0 && c != '\"')
    {
        if (idx >= buf_size) {
            char *tmp = buf;
            buf_size += STR_BUF_SIZE * 5;
            buf = malloc(buf_size);
            if (!buf) jmpErr(MemoryError, "no memory.");
            memcpy(buf, tmp, idx);
        }
        if ('\\' == c)
        {
            buf[idx++] = c;
            buf[idx++] = get_char(port);
        }
        else
            buf[idx++] = c;
    }
    if (c != '\"') jmpErr(IOError, "EOF in string.");
    buf[idx++] = '\0';
    
    if (buf_size == STR_BUF_SIZE) {
        buf = string_dup(buf);
        if (!buf) jmpErr(MemoryError, "no memory.");
    }
    return mk_string(buf);
}

static Cell *read_quote(IScheme *isc, int c) {
    Cell *cell = read_cell(isc);
    if (cell == CELL_EOF)
        return CELL_EOF;
    cell = cons(isc->sym_quote, cell);
    return cell;
}

static Cell *read_quasiquote(IScheme *isc, int c) {
    return 0;
}

static Cell *read_unquote(IScheme *isc, int c) {
    return 0;
}

static Cell *read_unquote_splicing(IScheme *isc, int c) {
    return 0;
}

static Cell *read_vector(IScheme *isc, int c) {
    return 0;
}

static Cell *read_list(IScheme *isc, int c) {
    Cell *head, *tail, *cell = CELL_NIL;
    head = tail = cons(CELL_NIL, CELL_NIL);

    switch (c) {
    case TOK_LPAREN:    c = TOK_RPAREN; break;
    case TOK_LBRACKET:  c = TOK_RBRACKET; break;
    case TOK_LBRACE:    c = TOK_RBRACE; break;
    }

    int d;
    Cell *port = isc->inport;
    for (;;) {
        d = get_token(isc);
        if (d == TOK_EOF) {
            jmpErr(IOError, "end of file.");
        }
        if (c == d) break;
        if (d == TOK_RPAREN || d == TOK_RBRACKET || d == TOK_RBRACE) {
            jmpErr(SyntaxError, "unmatched brackets.");
        }

        if (d == TOK_DOT) {
            if (cell == CELL_NIL) {
                jmpErr(SyntaxError, "illegal used of dot.");
            }
            cell = read_cell(isc);
            if (cell == CELL_EOF)
                return CELL_EOF;
            tail = rplacd(tail, cell);
            c = get_token(isc);
            if (c != TOK_RPAREN) {
                jmpErr(SyntaxError, "illegal used of dot.");
            }
        }
        cell = read_cell_by_token(isc, d);
        if (cell == CELL_EOF)
            return CELL_EOF;
        tail = rplacd(tail, cons(cell, CELL_NIL));
    }
    return cdr(head);
}

static Cell *op_func0(IScheme *isc, int op) {
    Cell *c;
    switch (op) {
    case OP_REPL_LOOP:
        if (isc->inport->port->t & PORT_EOF) {
            if (isc->cur_file_idx == 0)
                return CELL_FALSE;
            pop_load_file(isc);
            popOp(isc, isc->retnv);
        }
        if (is_interactive(isc)) {
            isc->contis = CELL_NIL;
            write_string(isc->outport, ">> ");
        }
        pushOp(isc, OP_REPL_LOOP, isc->args, isc->envir);
        pushOp(isc, OP_REPL_PRINT, isc->args, isc->envir);
        pushOp(isc, OP_REPL_EVAL, isc->args, isc->global_envir);
        gotoOp(isc, OP_REPL_READ);
    case OP_REPL_READ:
        c = read_cell(isc);
        if (c == CELL_EOF) {
            gotoOp(isc, OP_ERROR);
        }
        popOp(isc, c);
    case OP_REPL_EVAL:
        isc->code = isc->retnv;
        gotoOp(isc, OP_EVAL);
    case OP_REPL_PRINT:
        if (is_interactive(isc)) {
            if (isc->retnv != CELL_UNDEF) {
                print_cell(isc->retnv, isc->outport);
            }
            write_string(isc->outport, "\n");
        }
        popOp(isc, isc->retnv);
    case OP_DEF0:
        if (!is_pair(isc->code))
            jmpErr(SyntaxError, "missing expression after identifier.");
        if (is_immutable(car(isc->code)))
            jmpErr(ValueError, "unable to alter immutable atom.");
        if (is_pair(c = car(isc->code))) {
            Cell *e = cdr(isc->code);
            e = cons(isc->sym_lambda, cons(cdr(c), e));
            c = car(c);
            if (is_pair(c)) {
                for (; is_pair(c); c = car(c)) {
                    e = cons(isc->sym_lambda, cons(cdr(c), cons(e, CELL_NIL)));
                }
            }
            isc->code = e;
        } else if (is_symbol(c = car(isc->code))) {
            isc->code = cadr(isc->code);
        } else {
            jmpErr(SyntaxError, "invalid define expression.");
        }
        pushOp(isc, OP_DEF1, CELL_NIL, c);
        gotoOp(isc, OP_EVAL);
    case OP_DEF1:
        set_envir(isc->envir, isc->code, isc->retnv);
        popOp(isc, CELL_UNDEF);
    case OP_LAMBDA:
        popOp(isc, mk_proc(isc->code, isc->envir));
    case OP_EVAL:
        if (is_symbol(isc->code)) {
            c = find_envir(isc->code, cdr(isc->envir));
            if (is_pair(c)) popOp(isc, cdr(c));
            else jmpErr(ReferenceError, "unbound variable:%s", symbol(isc->code));
        } else if (is_pair(isc->code)) {
            if (is_syntax(c = car(isc->code))) {
                isc->code = cdr(isc->code);
                gotoOp(isc, c->chr);
            } else if (is_symbol(c = car(isc->code))) {
                c = find_envir(c, cdr(isc->envir));
                if (is_pair(c) && is_syntax(c = cdr(c))) {
                    isc->code = cdr(isc->code);
                    gotoOp(isc, c->chr);
                }
            }
            pushOp(isc, OP_EVAL_ARGS, CELL_NIL, cdr(isc->code));
            isc->code = car(isc->code);
            gotoOp(isc, OP_EVAL);
        } else {
            popOp(isc, isc->code);
        }
        break;
    #if 0
    case OP_EVAL_OPC:
        if (is_macro(isc->retnv)) {
            // TODO: macro
        } else {
            isc->code = cdr(isc->code);
            gotoOp(isc, OP_EVAL_ARGS);
        }
        break;
    #endif
    case OP_EVAL_ARGS:
        isc->args = cons(isc->retnv, isc->args);
        if (is_pair(isc->code)) {
            pushOp(isc, OP_EVAL_ARGS, isc->args, cdr(isc->code));
            isc->code = car(isc->code);
            isc->args = CELL_NIL;
            gotoOp(isc, OP_EVAL);
        } else {
            isc->args = reverse(isc, isc->args);
            isc->code = car(isc->args);
            isc->args = cdr(isc->args);
            gotoOp(isc, OP_APPLY);
        }
        break;
    case OP_APPLY:
        if (is_syntax(isc->code) || is_iproc(isc->code)) {
            gotoOp(isc, isc->code->chr);
        } else if (is_eproc(isc->code)) {
            popOp(isc, isc->code->eproc(isc, isc->args));
        } else if (is_proc(isc->code)) {
            Cell *env = closure_env(isc->code);
            Cell *fp = closure_args(isc->code);
            Cell *ap = isc->args;
            for (; is_pair(fp); fp = cdr(fp), ap = cdr(ap)) {
                if (ap == CELL_NIL) jmpErr(SyntaxError, "too few arguments.");
                mk_envir(env, car(fp), car(ap));
            }
            if (fp == CELL_NIL && ap != CELL_NIL) jmpErr(SyntaxError, "too much arguments.");
            isc->code = closure_code(isc->code);
            isc->args = CELL_NIL;
            isc->envir = env;
            gotoOp(isc, OP_EVAL_LIST);
        } else if (is_contis(isc->code)) {
            isc->contis = cons(contis_car(isc->code), contis_cdr(isc->code));            
            popOp(isc, isc->args != CELL_NIL ? car(isc->args) : CELL_NIL);
        }
        jmpErr(SyntaxError, "illegal procudure.");
    case OP_EVAL_LIST: 
        if (is_pair(isc->code)) {
            if (cdr(isc->code) != CELL_NIL) {
                pushOp(isc, OP_EVAL_LIST, CELL_NIL, cdr(isc->code));
            }
            isc->code = car(isc->code);
            gotoOp(isc, OP_EVAL);
        } else {
            gotoOp(isc, OP_EVAL);
        }
     case OP_CALLCC:
        c = car(isc->args);
        if (length(closure_args(c)) != 1) {
            jmpErr(TypeError, "invalid number of arguments to procedure at #<PROCEDURE call-with-current-continuation>.");
        }
        isc->code = c;
        isc->args = cons(mk_contis(isc), CELL_NIL);
        gotoOp(isc, OP_APPLY);
     case OP_IF0:
        pushOp(isc, OP_IF1, CELL_NIL, cdr(isc->code));
        isc->code = car(isc->code);
        gotoOp(isc, OP_EVAL);
    case OP_IF1: 
        if (isc->retnv == CELL_FALSE) {
            if (length(isc->code) == 2) {
                isc->code = cdr(isc->code);
                isc->args = CELL_NIL;
                gotoOp(isc, OP_EVAL_LIST);
            }
            popOp(isc, CELL_UNDEF);
        }
        isc->code = car(isc->code);
        isc->args = CELL_NIL;
        gotoOp(isc, OP_EVAL_LIST);
    case OP_LET0:
        isc->args = CELL_NIL;
        isc->retnv = isc->code;
        isc->code = is_symbol(car(isc->code)) ? cadr(isc->code) : car(isc->code);
        gotoOp(isc, OP_LET1);
    case OP_LET1:
        isc->args = cons(isc->retnv, isc->args);
        if (is_pair(isc->code)) {
            if (!is_pair(car(isc->code)) || !is_symbol(caar(isc->code)) || !is_pair(cdar(isc->code))) {
                jmpErr(SyntaxError, "let: bad syntax of binding.");
            }
            pushOp(isc, OP_LET1, isc->args, cdr(isc->code));
            isc->code = cadar(isc->code);
            isc->args = CELL_NIL;
            gotoOp(isc, OP_EVAL);
        } else {
            isc->args = reverse(isc, isc->args);
            isc->code = car(isc->args);
            isc->args = cdr(isc->args);
            gotoOp(isc, OP_LET2);
        }
    case OP_LET2:
        isc->envir = cons(CELL_NIL, cdr(isc->envir));
        c = is_symbol(car(isc->code)) ? cadr(isc->code) : car(isc->code);
        for (Cell *a = isc->args; !is_nil(a); c = cdr(c), a = cdr(a)) {
            mk_envir(isc->envir, caar(c), car(a));
        }
        if (is_symbol(car(isc->code))) {
            for (c = cadr(isc->code), isc->args = CELL_NIL; !is_nil(c); c = cdr(c)) {
                isc->args = cons(caar(c), isc->args);
            }
            c = mk_closure(cons(reverse(isc, isc->args), cddr(isc->code)), isc->envir);
            isc->envir = closure_env(c);
            mk_envir(isc->envir, car(isc->code), c);
            isc->code = cddr(isc->code);
            isc->args = CELL_NIL;
        } else {
            isc->code = cdr(isc->code);
            isc->args = CELL_NIL;
        }
        gotoOp(isc, OP_EVAL_LIST);
    case OP_LETSEQ0:
        if (is_nil(car(isc->code))) {
            isc->code = cdr(isc->code);
            isc->args = CELL_NIL;
            gotoOp(isc, OP_EVAL_LIST);
        }
        for (c = car(isc->code); !is_nil(c); c = cdr(c)) {
            if (!is_pair(c) || !is_pair(car(c)) || !is_symbol(caar(c)))
                jmpErr(SyntaxError, "let*: bad syntax of binding.");
        }
        c = car(isc->code);
        pushOp(isc, OP_LETSEQ1, cdr(isc->code), c);
        isc->args = CELL_NIL;
        isc->code = cadar(c);
        gotoOp(isc, OP_EVAL);
    case OP_LETSEQ1:
        isc->envir = cons(CELL_NIL, cdr(isc->envir));
        gotoOp(isc, OP_LETSEQ2);
    case OP_LETSEQ2:
        mk_envir(isc->envir, caar(isc->code), isc->retnv);
        isc->code = cdr(isc->code);
        if (is_pair(isc->code)) {
            pushOp(isc, OP_LETSEQ2, isc->args, isc->code);
            isc->code = cadar(isc->code);
            gotoOp(isc, OP_EVAL);
        } else {
            isc->code = isc->args;
            isc->args = CELL_NIL;
            gotoOp(isc, OP_EVAL_LIST);
        }
    case OP_ERROR:
        gotoOp(isc, OP_REPL_LOOP);
    }
    return CELL_TRUE;
}

static Cell *op_func1(IScheme *isc, int op) {
    switch (op) {
    case OP_LOAD:
        break;
    case OP_DISPLAY:
    {
        int len = length(isc->args);
        if (len == 1) {
            print_cell_readable(car(isc->args), isc->outport);
        } else if (len == 2) {
            print_cell_readable(car(isc->args), cadr(isc->args));
        }
        popOp(isc, CELL_UNDEF);
    }
    }
    return CELL_TRUE;
}

static Cell *op_func2(IScheme *isc, int op) {
    switch (op) {

    }
    return CELL_TRUE;
}

static Cell *op_func3(IScheme *isc, int op) {
    switch (op) {
    case OP_ADD:
    {
        int total=0;
        for (Cell *lst = isc->args, *c; is_pair(lst); lst = cdr(lst)) {
            total += car(lst)->num->l;
        }
        Number *num = malloc(sizeof(Number));
        if (!num) jmpErr(MemoryError, "no memory.");
        num->t = NUMBER_LONG;
        num->l = total;
        popOp(isc, mk_number(num));
    }
    case OP_SUB:
        break;
    case OP_MULTI:
        break;
    case OP_DIV:
        break;
    }
    return CELL_TRUE;
}

static void init_readers() {
    for (int i = 0;  i < TOK_MAX;  i++) g_readers[i]= read_illegal;
    g_readers[TOK_SYMBOL]   = read_symbol;
    g_readers[TOK_LPAREN]   = read_list;
    g_readers[TOK_LBRACKET] = read_list;
    g_readers[TOK_LBRACE]   = read_list;
    g_readers[TOK_QUOTE]    = read_quote;
    g_readers[TOK_DQUOTE]   = read_string;
    g_readers[TOK_QQUOTE]   = read_quasiquote;
    g_readers[TOK_UNQUOTE]  = read_unquote;
    g_readers[TOK_UNQUOTE_SPLICING] = read_unquote_splicing;
    g_readers[TOK_CONST]    = read_const;
    g_readers[TOK_VECTOR]   = read_vector;
}

static void isc_init(FILE *in, String name) {
    gp_isc = malloc(sizeof(IScheme));
    if (!gp_isc) {
        IError("no memory.");
        return;
    }
    memset(gp_isc, 0x0, sizeof(IScheme));
    gp_isc->last_seg = -1;
    
    seg_alloc(gp_isc, 3);
    init_readers();

    gp_isc->inport = mk_port(in, name, PORT_FILE | PORT_INPUT);
    gp_isc->outport = mk_port(stdout, NULL, PORT_FILE | PORT_OUTPUT);
    gp_isc->sym_lambda = internal(gp_isc, "lambda");
    gp_isc->sym_quote = internal(gp_isc, "quote");
    gp_isc->load_files[0] = gp_isc->inport;
    gp_isc->global_envir = cons(internal(gp_isc, "*global-envir*"), CELL_NIL);
    gp_isc->envir = gp_isc->global_envir;
        
    Cell *c = CELL_NIL;
    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) c = mk_syntax(i);
            else c = mk_iproc(i);
            mk_envir(gp_isc->global_envir, internal(gp_isc, g_opcodes[i].name), c);
        }
    }
    mk_envir(gp_isc->global_envir, internal(gp_isc, "call/cc"),
        cdr(find_envir(internal(gp_isc, g_opcodes[OP_CALLCC].name), cdr(gp_isc->global_envir))));
}

static struct {
    bool (*func)(Cell*);
    char *kind;
} g_arg_inspector[] = {
    {is_any,        0},
    {is_char,       "character"},
    {is_number,     "number"},
    {is_integer,    "integer"},
    {is_natural,    "natural"},
    {is_string,     "string"},
    {is_symbol,     "symbol"},
    {is_pair,       "pair"},
    {is_list,       "list"},
    {is_vector,     "vector"},
    {is_proc,       "procedure"},    
    {is_envir,      "environment"},
    {is_contis,     "continuation"},
    {is_port,       "port"},
    {is_inport,     "input port"},
    {is_outport,    "output port"},
};

bool arg_type_check(IScheme *isc) {
    OpCode  *opc = g_opcodes + isc->op;
    int n;
    if (opc->t == IPROC) {
        n = length(isc->args);
        if (n < opc->min_args) {
            snprintf(isc->buff, STR_BUF_SIZE, "%s: unexpected number of arguments, expected at least %d but %d %s given",
                    opc->name,
                    opc->min_args,
                    n,
                    n>1?"were":"was");
            return FALSE;
        }
        if (n > opc->max_args) {
            snprintf(isc->buff, STR_BUF_SIZE, "%s: unexpected number of arguments, expected at most %d but %d %s given",
                    opc->name,
                    opc->max_args,
                    n,
                    n>1?"were":"was");
            return FALSE;
        }
        if (opc->arg_types != 0) {
            int index = 0;
            const char *arg_types = opc->arg_types;
            Cell *args = isc->args;
            do {
                Cell *arg = car(args);
                if (!g_arg_inspector[arg_types[0]-1].func(arg)) break;
                if (arg_types[1] != 0) ++arg_types;
                args = cdr(args);
                ++index;
            } while (index < n);
            if (index < n) {
                snprintf(isc->buff, STR_BUF_SIZE, "%s: unmatched type of argument %d, must be %s",
                        opc->name,
                        index + 1,
                        g_arg_inspector[arg_types[0]-1].kind);
                return FALSE;
            }
        }
    } else if (opc->t == SYNTAX) { 
        n = length(isc->code);
        if (n < opc->min_args || n > opc->max_args) {
            snprintf(isc->buff, STR_BUF_SIZE, "%s: bad syntax", opc->name);
            return FALSE;
        }
    }
    return TRUE;
}

static void isc_repl() {
    gp_isc->op = OP_REPL_LOOP;
    for (;;) {
        setjmp(gp_isc->jmpbuf);
        if (!arg_type_check(gp_isc)) {
            print_err(gp_isc, SyntaxError, gp_isc->buff);
            break;
        }
        if (g_opcodes[gp_isc->op].func(gp_isc, gp_isc->op) != CELL_TRUE)
            break;
    }
}

static void isc_finalize() {

}

int main(int argc, char *argv[]) {
    FILE *in = NULL;
    String name = NULL;
    if (argc == 1) {
        in = stdin;
    } else {
        if (!strcmp(argv[1], "-h")  || !strcmp(argv[1], "--help")) {
            printf("iScheme v0.1 by yuanjq\n");
            printf("Options and arguments:\n");
            printf("  -\t\t program read from stdin\n");
            printf("  file\t\t program read from file\n");
            printf("  -h(--help)\t print this help message and exit\n");
            printf("  -v(--version)\t iScheme's version\n");
            return 0;
        } else if (!strcmp(argv[1], "-v") || !strcmp(argv[1], "--version")) {
            printf("iScheme v0.1\n");
            return 0;
        } else if (!strcmp(argv[1], "-")) {
            in = stdin;            
        } else {
            in = fopen(argv[1], "r");
            name = argv[1];
            if (!in) {
                IError("cant't open file '%s'.", name);
                return -1;
            }
        }
    }

    if (in == stdin) {
        printf("iScheme v0.1 by yuanjq\n");
    }
    isc_init(in, name);
    isc_repl();
    isc_finalize();
    return 0;
}

