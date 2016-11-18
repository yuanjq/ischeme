#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <unistd.h>
#include "ischeme.h"

//#define MACRO_DEBUG

#define CELL_TRUE               &g_true
#define CELL_FALSE              &g_false
#define CELL_NIL                &g_nil
#define CELL_EOF                &g_eof
#define CELL_UNDEF              &g_undef
#define CELL_ELLIPSIS           &g_ellipsis
#define CELL_ERR                (Cell*)-1
#define CELL_SPLICING           (Cell*)-2

#define gotoOp(sc,o)      	    ({ctx_op(sc)=o; goto Loop;})
#define gotoOpEx(sc,o,a,c,d,e)  ({ctx_op(sc)=o; ctx_args(sc)=a; \
                                  ctx_code(sc)=c; ctx_data(sc)=d; \
                                  ctx_env(sc)=e; goto Loop;})
#define popOp(sc,r)             ({pop_op(sc, r); goto Loop;})
#define pushOp(sc,o,a,c)        push_op(sc, o, a, c, CELL_NIL, ctx_env(sc))
#define pushOpEx(sc,o,a,c,d,e)  push_op(sc, o, a, c, d, e)
#define gotoErr(sc, e)          ({ctx_ret(sc) = e; gotoOp(sc, OP_ERROR);})
#define find_env(s,e)           assq(s,e)

#define DELIMITERS              "()[]{}\";\f\t\v\n\r "
static OpCode g_opcodes[] = {
    #define _OPCODE(n, t1, o, m1, m2, t2) {n, t1, m1, m2, t2},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};

static const char *ascii32[32]={
	"nul",  "soh",  "stx",  "etx",  "eot",  "enq",  "ack",  "bel", 
    "bs",   "ht",   "lf",   "vt",   "ff",   "cr",   "so",   "si", 
    "dle",  "dc1",  "dc2",  "dc3",  "dc4",  "nak",  "syn",  "etb", 
    "can",  "em",   "sub",  "esc",  "fs",   "gs",   "rs",   "us"
};

static Reader g_readers[TOK_MAX];
static Cell g_true = {t:BOOLEAN, {bl:TRUE}};
static Cell g_false = {t:BOOLEAN, {bl:FALSE}};
static Cell g_nil;
static Cell g_eof;
static Cell g_undef;
static Cell g_ellipsis;


/****************** function ****************/
static void print_err(Cell*, char*, char*, ...);
static Cell *arg_type_check(Cell*);


/****************** syntax ******************/
static inline bool is_nil(Cell *c)     { return c == CELL_NIL; }
static inline bool is_true(Cell *c)    { return c == CELL_TRUE; }
static inline bool is_false(Cell *c)   { return c == CELL_FALSE; }
static inline bool is_eof(Cell *c)     { return c == CELL_EOF; }
static inline bool is_undef(Cell *c)   { return c == CELL_UNDEF; }
static inline bool is_ellipsis(Cell *c){ return c == CELL_ELLIPSIS; }
static inline bool is_any(Cell *c)     { return TRUE; }
static inline bool is_boolean(Cell *c) { return c && T(c) == BOOLEAN; }
static inline bool is_char(Cell *c)    { return c && T(c) == CHAR; }
static inline bool is_number(Cell *c)  { return c && T(c) == NUMBER; }
static inline bool is_real(Cell *c)    { return c && T(c) == NUMBER && (number_type(c) == NUMBER_LONG || number_type(c) == NUMBER_DOUBLE || number_type(c) == NUMBER_FRACTION); }
static inline bool is_integer(Cell *c) { return is_number(c) && number_type(c) == NUMBER_LONG; }
static inline bool is_natural(Cell *c) { return is_integer(c) && number_long(c) >= 0; }
static inline bool is_string(Cell *c)  { return c && T(c) == STRING; }
static inline bool is_pair(Cell *c)    { return c && T(c) == PAIR; }
static inline bool is_vector(Cell *c)  { return c && T(c) == VECTOR; }
static inline bool is_symbol(Cell *c)  { return c && T(c) == SYMBOL; }
static inline bool is_syntax(Cell *c)  { return c && T(c) == SYNTAX; }
static inline bool is_closure_expr(Cell *c) { return c && T(c) == CLOSURE_EXPR; }
static inline bool is_closure(Cell *c) { return c && T(c) == CLOSURE; }
static inline bool is_proc(Cell *c)    { return c && T(c) == PROC; }
static inline bool is_iproc(Cell *c)   { return c && T(c) == IPROC; }
static inline bool is_eproc(Cell *c)   { return c && T(c) == EPROC; }
static inline bool is_procs(Cell *c)   { return c && (T(c) == PROC || T(c) == IPROC || T(c) == EPROC); }
static inline bool is_envir(Cell *c)   { return c && T(c) == ENVIR; }
static inline bool is_macro(Cell *c)   { return c && T(c) == MACRO; }
static inline bool is_promise(Cell *c) { return c && T(c) == PROMISE; }
static inline bool is_port(Cell *c)    { return c && T(c) == PORT; }
static inline bool is_inport(Cell *c)  { return is_port(c) && port_type(c) & PORT_INPUT; }
static inline bool is_outport(Cell *c) { return is_port(c) && port_type(c) & PORT_OUTPUT; }
static inline bool is_continue(Cell *c) { return c && T(c) == CONTINUE; }
static inline bool is_continues(Cell *c) { return c && T(c) == CONTINUES; }
static inline bool is_exception(Cell *c) { return c && T(c) == EXCEPTION; }
static inline bool is_port_eof(Cell *c) { return c && T(c) == PORT && ((port_type(c) & PORT_FILE) && feof(port_file(c)) || (port_type(c) & PORT_STRING) && port_string_pos(c) == port_string_end(c)); }
static inline bool is_immutable(Cell *c) { return c && cell_type(c) & M_IMMUTABLE; }
static inline bool is_interactive(Cell *ctx) {
    return port_type(ctx_load_file(ctx, 0)) & PORT_FILE &&
           port_file(ctx_load_file(ctx, 0)) == stdin;
}

static inline char *symbol(Cell *c)   { return is_symbol(c) ? symbol_data(c) : NULL; }
static inline char *string(Cell *c)   { return is_string(c) ? string_data(c) : NULL; }
static inline Port *port(Cell *c)     { return is_port(c) ? &c->port : NULL; }

#define car(c)      ({ c && T(c) == PAIR ? c->pair.a : NULL; })
#define cdr(c)      ({ c && T(c) == PAIR ? c->pair.d : NULL; })
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

static inline Cell *rplaca(Cell *c, Cell *a)    { return is_pair(c) ? c->pair.a = a : NULL; }
static inline Cell *rplacd(Cell *c, Cell *d)    { return is_pair(c) ? c->pair.d = d : NULL; }

static Cell *cons(Cell *ctx, Cell *a, Cell *d) {
    Cell *c = pair_new(ctx);
    if (c) {
        pair_car(c) = a;
        pair_cdr(c) = d;
    }
    return c;
}

static bool is_list(Cell *c) {
    for (; is_pair(c); c = cdr(c));
    if (c == CELL_NIL) return TRUE;
    return FALSE;
}

static Cell *mk_bool(bool b) {
    if (b)
        return CELL_TRUE;
    else
        return CELL_FALSE;
}

static Cell *mk_char(Cell *ctx, Char chr) {
    Cell *c = char_new(ctx);
    if (c) {
        char_value(c) = chr;
    }
    return c;
}

static char *string_dup(char *str) {
    char *s = cell_malloc(strlen(str) + 1);
    if (s) {
        strcpy(s, str);
    }
    return s;
}

static Cell *mk_string(Cell *ctx, char *fmt, ...) {
    int size = 0;
    va_list ap;
    Cell *c = NULL;

    va_start(ap, fmt);
    size = vsnprintf(NULL, size, fmt, ap);
    va_end(ap);

    size++;
    c = cell_alloc(ctx, cell_sizeof(str) + size);
    if (c) {
        cell_type(c) = STRING;
        string_size(c) = size;

        va_start(ap, fmt);
        size = vsnprintf(string_data(c), size, fmt, ap);
        va_end(ap);
    }
    return c;
}

static Cell *mk_symbol(Cell *ctx, char *s) {
    Cell *c = mk_string(ctx, s);
    if (c) {
        cell_type(c) = SYMBOL;
    }
    return c;
}

static Cell *mk_vector(Cell *ctx, uint len, Cell *fill) {
    Cell *c = cell_alloc(ctx, cell_sizeof(vector) + len * S(Cell*));
    if (c) {
        cell_type(c) = VECTOR;
        vector_length(c) = len;
        for (uint i=0; i<len; i++) {
            vector_data(c)[i] = fill;
        }
    }
    return c;
}

static Cell *mk_closure_expr(Cell *ctx, Cell *expr, Cell *env) {
    Cell *c = closure_expr_new(ctx);
    if (c) {
        cell_type(c) = CLOSURE_EXPR;
        closure_expr_expr(c) = expr;
        closure_expr_env(c) = env;
    }
    return c;
}

static Cell *mk_closure(Cell *ctx, Cell *a, Cell *e) {
    Cell *c = closure_new(ctx);
    if (c) {
        closure_args(c) = car(a);
        closure_code(c) = cdr(a);
        closure_env(c) = cons(ctx, CELL_NIL, cdr(e));
    }
    return c;
}

static Cell *mk_proc(Cell *ctx, Cell *name, Cell *clos) {
    Cell *c = proc_new(ctx);
    if (c) {
        proc_name(c) = name;
        proc_closure(c) = clos;
    }
    return c;
}

static Cell *mk_iproc(Cell *ctx, int op) {
    Cell *c = iproc_new(ctx);
    if (c) {
        iproc_op(c) = op;
    }
    return c;
}

static Cell *mk_macro(Cell *ctx, Cell *mchs, Cell *env) {
    Cell *c = macro_new(ctx);
    if (c) {
        macro_matchers(c) = mchs;
        macro_env(c) = env;
    }
    return c;
}

static Cell *mk_port(Cell *ctx, FILE *f, char *name, int t) {
    Cell *c = port_new(ctx);
    if (c) {
        port_type(c) = t;
        port_file(c) = f;
        if (name) {
            port_file_name(c) = mk_string(ctx, name);
        }
    }
    return c;
}

static Cell *mk_syntax(Cell *ctx, int op) {
    Cell *c = syntax_new(ctx);
    if (c) {
        syntax_op(c) = op;
    }
    return c;
}

static Cell *mk_continues(Cell *ctx, Cell *c) {
    c = cons(ctx, c, ctx_continue(ctx));
    if (c) cell_type(c) = CONTINUES;
    return c;
}

static Cell *mk_exception(Cell *ctx, uchar t, Cell *msg, Cell *trg, Cell *src) {
    Cell *c = exception_new(ctx);
    if (c) {
        exception_type(c) = t;
        exception_msg(c) = msg;
        exception_trg(c) = trg;
        exception_src(c) = src;
    }
}

/********************* stack frame ********************/
static void print_err(Cell *ctx, char *etype, char *ebody, ...) {
    va_list ap;
    va_start(ap, ebody);

    Cell *in = ctx_load_file(ctx, ctx_file_idx(ctx));
    Cell *out = ctx_outport(ctx);
    fprintf(port_file(out), "%s", etype);
    fprintf(port_file(out), ": ");
    vfprintf(port_file(out), ebody, ap);
    va_end(ap);
    if (port_type(in) & PORT_FILE && port_file(in) != stdin)
        fprintf(port_file(out), "\n\t at %s:%d\n", string(port_file_name(out)), port_file_pos(out));
    else
        fputc('\n', port_file(out));
}

static inline bool pop_op(Cell *ctx, Cell *v) {
    Cell *conts = ctx_continue(ctx);
    Cell *cont = continues_car(conts);
    if (!is_continue(cont)) {
        return FALSE;
    }
    ctx_ret(ctx) = v;
    ctx_op(ctx) = continue_op(cont);
    ctx_args(ctx) = continue_args(cont);
    ctx_code(ctx) = continue_code(cont);
    ctx_data(ctx) = continue_data(cont);
    ctx_env(ctx) = continue_env(cont);
    ctx_continue(ctx) = continues_cdr(conts);
    return TRUE;
}

static void push_op(Cell *ctx, Op op, Cell *args, Cell *code, Cell *data, Cell *env) {
    Cell *c = continue_new(ctx);
    if (c) {
        continue_op(c) = op;
        continue_args(c) = args;
        continue_code(c) = code;
        continue_data(c) = data;
        continue_env(c) = env;
        ctx_continue(ctx) = mk_continues(ctx, c);
    }
}

/***************** I/O handler ******************/
static inline void port_flush(Cell *p) {
    fflush(port_file(p));
}

static void port_close(Cell *ctx, Cell* p, int f) {
    port_type(p) &= ~f;
	if((port_type(p) & (PORT_INPUT | PORT_OUTPUT)) == 0) {
		if(port_type(p) & PORT_FILE) {
            port_file_name(p) = NULL;
			fclose(port_file(p));
		}
		port_type(p) = PORT_FREE;
	}
}

static Cell *push_load_file(Cell *ctx, char *name) {
	if (ctx_file_idx(ctx) == MAX_LOAD_FILES-1)
        return mk_exception(ctx, IOError, mk_string(ctx, "exceeded max load files"), NULL, NULL);
	FILE *fin = fopen(name, "r");
	if (!fin) {
        char buf[128];
        snprintf(buf, sizeof(buf), "can not open file \"%s\"", name);
        return mk_exception(ctx, IOError, mk_string(ctx, buf), NULL, NULL);
    }
    ctx_file_idx(ctx) += 1;
    ctx_load_file(ctx, ctx_file_idx(ctx)) = mk_port(ctx, fin, name, PORT_FILE | PORT_INPUT);
    ctx_inport(ctx) = ctx_load_file(ctx, ctx_file_idx(ctx));
    return ctx_inport(ctx);
}

static void pop_load_file(Cell *ctx) {
	if(ctx_file_idx(ctx) != 0) {
		port_close(ctx, ctx_inport(ctx), PORT_INPUT);
        ctx_file_idx(ctx) -= 1;
        ctx_inport(ctx) = ctx_load_file(ctx, ctx_file_idx(ctx));
	}
}

Cell *write_char(Cell *ctx, Cell *out, int c) {
	if(port_type(out) & PORT_FILE) {
        c = fputc(c, port_file(out));
        port_flush(out);
		return CELL_UNDEF;
	} else {
		if(port_string_end(out) <= port_string_pos(out)) {
           return mk_exception(ctx, MemoryError, mk_string(ctx, "write char out of range"), NULL, NULL);
        }
        *port_string_pos(out)++ = c;
        return CELL_UNDEF;
	}
    return mk_exception(ctx, IOError, mk_string(ctx, "invalid out port"), NULL, NULL);
}

Cell *write_string(Cell *ctx, Cell *out, char *s) {
	int len = strlen(s);
	if (port_type(out) & PORT_FILE) {
        len = fwrite(s, 1, len, port_file(out));
        port_flush(out);
		return CELL_UNDEF;
	} else if (port_type(out) & PORT_STRING) {
	    if (len > port_string_end(out) - port_string_pos(out)) {
           return mk_exception(ctx, MemoryError, mk_string(ctx, "write string out of range"), NULL, NULL);
        }
        for (; len; len--) *port_string_pos(out)++ = *s++;
        return CELL_UNDEF;
	}
    return mk_exception(ctx, IOError, mk_string(ctx, "invalid out port"), NULL, NULL);
}

static inline int get_char(Cell *in) {
    if (is_port_eof(in)) return EOF;
    int c = 0;
    if (port_type(in) & PORT_FILE) {
        c = fgetc(port_file(in));
    } else {
        if (port_string_pos(in) == 0 || port_string_pos(in) == port_string_end(in))
            c == EOF;
        else
            c = *port_string_pos(in)++;
    }
    return c;
}

static inline void unget_char(Cell *out, int c) {
    if (c == EOF) return;
    if (port_type(out) & PORT_FILE) {
        ungetc(c, port_file(out));
    } else {
        if (port_string_pos(out) != port_string_start(out)) --port_string_pos(out);
    }
}

static inline int skip_line(Cell *ctx) {
    int c = 0, n = 0;
    while ((c = get_char(ctx_inport(ctx))) != EOF && c != '\n') ++n;
    if (c == '\n') {
        if (ctx_load_file(ctx, ctx_file_idx(ctx)) && port_type(ctx_load_file(ctx, ctx_file_idx(ctx))) & PORT_FILE)
            ++port_file_pos(ctx_load_file(ctx, ctx_file_idx(ctx)));
        return ++n;
    }
    return EOF;
}

static int skip_comment(Cell *ctx, int c) {
    int n = 0;
    if (c == EOF) return EOF;
    if (c == ';') {
        c = skip_line(ctx);
        if (c == EOF) return EOF;
        n += c;
    } else if (c == '#') {
        c = get_char(ctx_inport(ctx));
        if (c == EOF) return EOF;
        if (c != '!')  {
            unget_char(ctx_inport(ctx), c);
            unget_char(ctx_inport(ctx), '#');
            return 0;
        }
        n += 2;
        c = skip_line(ctx);
        if (c == EOF) return EOF;
        n += c;
    }
    return n;
}

static int skip_space(Cell *ctx) {
    int c = 0, sum = 0, line = 0;
    do {
        c = get_char(ctx_inport(ctx));
        ++sum;
        if (c == '\n') ++line;
        else if (c == ';' || c == '#') {
            int n = skip_comment(ctx, c);
            if (n == EOF) return EOF;
            else if (n == 0) return sum;
            sum += n;
            ++line;
            continue;
        }
    } while(isspace(c) || c == ';' || c == '#');
    if (port_type(ctx_load_file(ctx, ctx_file_idx(ctx))) & PORT_FILE)
        port_file_pos(ctx_load_file(ctx, ctx_file_idx(ctx))) += line;
    if (c != EOF) {
        unget_char(ctx_inport(ctx), c);
        return --sum;
    }
    return EOF;
}

static Cell *reverse(Cell *ctx, Cell *old) {
    Cell *new = CELL_NIL;
    for (; is_pair(old); old = cdr(old))
        new = cons(ctx, car(old), new);
    return new;
}

static int get_token(Cell *ctx) {
    int c = skip_space(ctx);
    if (c == EOF) return TOK_EOF;
    switch (c = get_char(ctx_inport(ctx))) {
    case EOF: return TOK_EOF;
    case '(': return TOK_LPAREN;
    case ')': return TOK_RPAREN;
    case '[': return TOK_LBRACKET;
    case ']': return TOK_RBRACKET;
    case '{': return TOK_LBRACE;
    case '}': return TOK_RBRACE;
    case '.': {
        if (skip_space(ctx) > 0) return TOK_DOT;
        int a, b, d;
        if ((a = get_char(ctx_inport(ctx))) == '.') {
            if ((b = get_char(ctx_inport(ctx))) == '.') {
                d = get_char(ctx_inport(ctx));
                if (strchr(DELIMITERS, d)) {
                    unget_char(ctx_inport(ctx), d);
                    return TOK_ELLIPSIS;
                } else {
                    unget_char(ctx_inport(ctx), d);
                    unget_char(ctx_inport(ctx), b);
                }
            } else {
                unget_char(ctx_inport(ctx), b);
            }
        }

        unget_char(ctx_inport(ctx), a);
        unget_char(ctx_inport(ctx), c);
        return TOK_SYMBOL;
    }
    case '\'': return TOK_QUOTE;
    case '`': return TOK_QQUOTE;
    case '"': return TOK_DQUOTE;
    case ',':
        c = get_char(ctx_inport(ctx));
        if (c == '@') return TOK_UNQUOTE_SPLICING;
        else unget_char(ctx_inport(ctx), c);
        return TOK_UNQUOTE;
    case '#':
        c = get_char(ctx_inport(ctx));
        if (c == '(') return TOK_VECTOR;
        else if (c == '!') {
            c = skip_line(ctx);
            if (c == EOF) return TOK_EOF;
            return get_token(ctx);
        } else if (strchr("tfeibodx\\", c)) {
            unget_char(ctx_inport(ctx), c);
            unget_char(ctx_inport(ctx), '#');
            return TOK_CONST;
        }
        return TOK_ERR;
    case ';':
        c = skip_line(ctx);
        if (c == EOF) return TOK_EOF;
        return get_token(ctx);
    default:
        unget_char(ctx_inport(ctx), c);
        return TOK_SYMBOL;
    }
}

static Cell *read_cell(Cell *ctx) {
    int t = get_token(ctx);
    if (t == TOK_ERR) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
    } else if (t == TOK_EOF) {
        return CELL_EOF;
    }
    return g_readers[t](ctx, t);
}

static Cell *read_cell_by_token(Cell *ctx, int t) {
    if (t == TOK_ERR) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
    } else if (t == TOK_EOF) {
        return CELL_EOF;
    }
    return g_readers[t](ctx, t);
}

static bool num_equal(Cell *a, Cell *b) {
    if (!a || !b) return FALSE;
    if (number_type(a) != number_type(b)) return FALSE;
    switch (number_type(a)) {
    case NUMBER_LONG:
        return number_long(a) == number_long(b);
    case NUMBER_DOUBLE:
    {
        double sub = number_double(a) - number_double(b);
        return sub > -0.000001 && sub < 0.000001;
    }
    case NUMBER_FRACTION:
        return (number_long(number_fn_nr(a)) == number_long(number_fn_nr(b)) && number_long(number_fn_dr(a)) == number_long(number_fn_dr(b)));
    case NUMBER_COMPLEX:
        return num_equal(number_cx_rl(a), number_cx_rl(b)) && num_equal(number_cx_im(a), number_cx_im(b));
    }
    return FALSE;
}

static double real_compare(Cell *a, Cell *b) {
    double d1=0, d2=0;
    switch (number_type(a)) {
    case NUMBER_LONG:
        d1 = number_long(a);
        break;
    case NUMBER_DOUBLE:
        d1 = number_double(a);
        break;
    case NUMBER_FRACTION:
        d1 = number_long(number_fn_nr(a)) / number_long(number_fn_dr(a));
        break;
    }
    switch (number_type(b)) {
    case NUMBER_LONG:
        d2 = number_long(b);
        break;
    case NUMBER_DOUBLE:
        d2 = number_double(b);
        break;
    case NUMBER_FRACTION:
        d2 = number_long(number_fn_nr(b)) / number_long(number_fn_dr(b));
        break;
    }
    return d1 - d2;
}

static bool equal(Cell *a, Cell *b) {
    if (is_closure_expr(a)) a = closure_expr_expr(a);
    if (is_closure_expr(b)) b = closure_expr_expr(b);
    if (a == b) return TRUE;
    if (cell_type(a) != cell_type(b)) return FALSE;
    switch (cell_type(a)) {
    case CHAR:
        return a->chr == b->chr;
    case BOOLEAN:
    case PROC:
        return a == b;
    case NUMBER:
        return num_equal(a, b);
    case STRING:
    case SYMBOL:
        if (string_data(a) && string_data(b)) return !strcmp(string_data(a), string_data(b));
        break;
    case IPROC:
        return iproc_op(a) == iproc_op(b);
    case EPROC:
        return a->eproc == b->eproc;
    case PAIR:
        if (!equal(car(a), car(b)))
            return FALSE;
        return equal(cdr(a), cdr(b));
    case VECTOR:
        if (vector_length(a) != vector_length(b))
            return FALSE;
        for (uint i=0, len=vector_length(a); i<len; i++) {
            if (!equal(vector_data(a)[i], vector_data(b)[i]))
                return FALSE;
        }
        return TRUE;
    } 
    return FALSE;
}

static bool eqv(Cell *a, Cell *b) {
    if (is_closure_expr(a)) a = closure_expr_expr(a);
    if (is_closure_expr(b)) b = closure_expr_expr(b);
    if (a == b) return TRUE;
    if (cell_type(a) != cell_type(b)) return FALSE;
    switch (cell_type(a)) {
    case PAIR:
    case VECTOR:
    case STRING:
        return a == b;
    }
    return equal(a, b);
}

static bool eq(Cell *a, Cell *b) {
    if (is_closure_expr(a)) a = closure_expr_expr(a);
    if (is_closure_expr(b)) b = closure_expr_expr(b);
    if (cell_type(a) != cell_type(b)) return FALSE;
    if (is_symbol(a)) return equal(a, b);
    return a == b;
}

static Cell *find_symbol(Cell *ctx, char *s) {
    for (Cell *c = ctx_symbols(ctx); c; c = cdr(c)) {
        char *sym = symbol(car(c));
        if (sym && !strcmp(sym, s))
          return car(c);
    }
    return CELL_NIL;
}

static Cell* internal(Cell *ctx, char *s) {
    Cell *c = NULL;
    if ((c = find_symbol(ctx, s)) != CELL_NIL) return c;
    c = mk_symbol(ctx, s);
    ctx_symbols(ctx) = cons(ctx, c, ctx_symbols(ctx));
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

static Cell *list_ref(Cell *ls, Cell *n) {
    long l = number_long(n);
    for (long i=0; is_pair(ls); ls=cdr(ls)) {
        if (i == l) return car(ls);
        ++i;
    }
    return CELL_NIL;
}

static Cell *list_tail(Cell *ls) {
    for (; is_pair(ls); ls=cdr(ls)) {
        if (is_nil(cdr(ls))) {
            return car(ls);
        }
    }
    return CELL_NIL;
}

static void list_set(Cell *ls, Cell *n, Cell *v) {
    long l = number_long(n);
    for (long i=0; is_pair(ls); ls=cdr(ls)) {
        if (i == l) {
            rplaca(ls, v);
            return;
        }
        ++i;
    }
}

static void list_add(Cell *ctx, Cell *ls, Cell *c) {
    if (!is_pair(ls)) return;
    for (; is_pair(ls); ls=cdr(ls)) {
        if (cdr(ls) == CELL_NIL) {
            rplacd(ls, cons(ctx, c, CELL_NIL));
            return;
        }
    }
}

static void list_extend(Cell *ctx, Cell *ls, Cell *c) {
    if (!is_pair(ls)) return;
    for (; is_pair(ls); ls=cdr(ls)) {
        if (cdr(ls) == CELL_NIL) {
            if (is_pair(c)) {
                Cell *d, *e;
                d = e = cons(ctx, CELL_NIL, CELL_NIL);
                for (;; c=cdr(c)) {
                    e = rplacd(e, cons(ctx, car(c), CELL_NIL));
                    if (is_nil(cdr(c))) break;
                    if (!is_pair(cdr(c))) {
                        rplacd(e, cdr(c));
                        break;
                    }
                }
                rplacd(ls, cdr(d));
            } else {
                rplacd(ls, cons(ctx, c, CELL_NIL));
            }
            return;
        }
    }
}

static Cell *list_pop(Cell *ls) {
    Cell *c = CELL_NIL;
    if (!is_pair(ls)) return CELL_NIL;
    if (cdr(ls) == CELL_NIL) {
        c = car(ls);
        rplaca(ls, CELL_NIL);
    } else {
        for (; is_pair(ls); ls=cdr(ls)) {
            if (is_pair(cdr(ls)) && cddr(ls) == CELL_NIL) {
                c = cadr(ls);
                rplacd(ls, CELL_NIL);
            }
        }
    }
    return c;
}

static void alist_update(Cell *ctx, Cell *ls1, Cell *ls2) {
    for (; is_pair(ls2); ls2=cdr(ls2)) {
        if (!is_pair(car(ls2)))
            continue;
        bool found = FALSE;
        for (Cell *ls=ls1; is_pair(ls); ls=cdr(ls)) {
            if (!is_pair(car(ls)))
                continue;
            if (equal(caar(ls), caar(ls2))) {
                found = TRUE;
                list_add(ctx, cdar(ls), cdar(ls2));
            }
        }
        if (!found) {
            list_add(ctx, ls1, cons(ctx, caar(ls2), cons(ctx, cdar(ls2), CELL_NIL)));
        }
    }
}

static void alist_append(Cell *ctx, Cell *ls, Cell *pair) {
    if (!is_pair(ls) || !is_pair(pair)) return;
    bool found = FALSE;
    Cell *k = car(pair), *v = cdr(pair);
    for (Cell *c=ls; is_pair(c); c=cdr(c)) {
        if (!is_pair(car(c)))
            continue;
        if (equal(caar(c), k)) {
            found = TRUE;
            list_add(ctx, cdar(c), v);
            break;
        }
    }
    if (!found) {
        list_add(ctx, ls, cons(ctx, k, cons(ctx, v, CELL_NIL)));
    }
}

static uint length(Cell *list) {
    uint len = 0;
    for (; is_pair(list); list = cdr(list)) ++len;
    return len;
}

static Cell *set_env(Cell *ctx, Cell *env, Cell *s, Cell *v) {
    Cell *e = CELL_NIL;
    if ((e = find_env(s, cdr(env))) != CELL_NIL) {
        rplacd(e, v);
    } else {
        e = cons(ctx, s, v);
        rplacd(env, cons(ctx, e, cdr(env)));
    }
    return e;
}

static Cell* mk_env(Cell *ctx, Cell *env, Cell *s, Cell *v) {
    Cell *e = cons(ctx, s, v);
    rplacd(env, cons(ctx, e, cdr(env)));
    return e;
}

static Cell *read_illegal(Cell *ctx, int c) {
    char *str = "";
    switch (c) {
    case TOK_RPAREN:
        str = ")";
        break;
    case TOK_RBRACKET:
        str = "]";
        break;
    case TOK_RBRACE:
        str = "}";
        break;
    case TOK_DOT:
        str = ".";
        break;
    case TOK_ELLIPSIS:
        str = "...";
        break;
    }
    return mk_exception(ctx, SyntaxError, mk_string(ctx, "unexpected symbol: %s", str), NULL, NULL);
}

static inline int read_upto(Cell *port, char *upto, char **out, uint *psize) {
    int c;
    char *p = *out;
    uint size = *psize;

    for (;;) {
        if (p - *out < size) {
            if ((c = get_char(port)) < 0) {
                return EOF;
            }
            if (strchr(upto, c))
                break;
            *p++ = c;
        } else {
            char *tmp = cell_malloc(2*size);
            if (tmp) {
                memcpy(*out, p, p - *out);
                if (size != STR_BUF_SIZE) cell_free(p);
                size = 2 * size;
                *psize = size;
                p = tmp + (p - *out);
                *out = tmp;
            } else {
                *out = NULL;
                return EOF;
            }
        }
    }
    unget_char(port, c);
    *p = '\0';
    return p - *out;
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


static Cell *write_number(Cell *ctx, Cell *port, Cell *num, char *s) {
    uchar t = number_type(num);
    if (t == NUMBER_LONG) {
        snprintf(s, STR_BUF_SIZE, "%ld", number_long(num));
    } else if (t == NUMBER_DOUBLE) {
        snprintf(s, STR_BUF_SIZE, "%lf", number_double(num));
    } else if (t == NUMBER_FRACTION) {
        snprintf(s, STR_BUF_SIZE, "%ld/%ld", number_long(number_fn_nr(num)), number_long(number_fn_dr(num)));
    } else if (t == NUMBER_COMPLEX) {
        Cell *im = number_cx_im(num);
        t = number_type(im);
        write_number(ctx, port, number_cx_rl(num), s);
        if ((t == NUMBER_LONG && number_long(im) >= 0) ||
            (t == NUMBER_DOUBLE && number_double(im) >= 0) ||
            (t == NUMBER_FRACTION && number_long(number_fn_nr(im)) >= 0)) {
            write_char(ctx, port, '+');
        }
        write_number(ctx, port, im, s);
        write_char(ctx, port, 'i');
        return CELL_NIL;
    }
    write_string(ctx, port, s);
    return CELL_NIL;
}

static void write_cell(Cell *ctx, Cell *port, Cell *c, bool readable, bool more, bool top) {
    char buf[STR_BUF_SIZE];
    char *s = buf;
	if (is_nil(c)) {
        write_string(ctx, port, "()");
        return;
	} else if (is_true(c)) {
		s = "#t";
	} else if (is_false(c)) {
		s = "#f";
	} else if (is_undef(c)) {
        s = "#<UNDEF>";
    } else if (is_ellipsis(c)) {
        s = "...";
	} else if (is_eof(c)) {
		s = "#<EOF>";
	} else if (is_port(c)) {
		snprintf(s, STR_BUF_SIZE, "#<PORT:%p>", c);
	} else if (is_symbol(c)) {
        if (c == ctx_quote(ctx)) {
            write_char(ctx, port, '\'');
        } else if (c == ctx_quasiquote(ctx)) {
            write_char(ctx, port, '`');
        } else if (c == ctx_unquote(ctx)) {
            write_char(ctx, port, ',');
        } else if (c == ctx_unquote_splicing(ctx)) {
            write_string(ctx, port, ",@");
        } else {
            write_string(ctx, port, symbol(c));
        }
        return;
	} else if (is_macro(c)) {
		snprintf(s, STR_BUF_SIZE, "#<MACRO:%p>", c);
	} else if (is_promise(c)) {
	   	snprintf(s, STR_BUF_SIZE, "#<PROMISE:%p>", c);
	} else if (is_proc(c) || is_iproc(c) || is_eproc(c)) {
	    if (is_proc(c) && more) {
            if (top) write_string(ctx, port, "#<PROCEDURE>\n");
            write_string(ctx, port, "(lambda ");
            write_cell(ctx, port, closure_args(c), readable, more, 0);
            write_string(ctx, port, " ");
            write_cell(ctx, port, closure_code(c), readable, more, 0);
            write_string(ctx, port, ")");
            return;
        } else {
		    snprintf(s, STR_BUF_SIZE, "#<PROCEDURE:%p>", c);
        }
	} else if (is_continues(c)) {
		snprintf(s, STR_BUF_SIZE, "#<CONTINUATION:%p>", c);
	} else if (is_number(c)) {
        write_number(ctx, port, c, s);
        return;
	} else if (is_string(c)) {
        if (!readable) write_char(ctx, port, '\"');
        write_string(ctx, port, string(c));
        if (!readable) write_char(ctx, port, '\"');
        return;
	} else if (is_char(c)) {
	    if (readable) {
            snprintf(s, STR_BUF_SIZE, "%c", c->chr);
        } else {
    		switch(char_value(c)) {
    		case ' ': s = "#\\space"; break;
    		case '\n': s = "#\\newline"; break;
    		case '\r': s = "#\\return"; break;
    		case '\t': s =  "#\\tab"; break;
    		default:
    			if(char_value(c) == 127) {
    				s = "#\\del"; break;
    			} else if(char_value(c) < 32) {
    				snprintf(s, STR_BUF_SIZE, "#\\%s", ascii32[char_value(c)]);
    				break;
    			}
    			snprintf(s, STR_BUF_SIZE, "#\\%c", char_value(c)); break;
    			break;
    		}
        }
    } else if (is_pair(c)) {
        Cell *d = car(c);
        bool quote = (d == ctx_quote(ctx)) || (d == ctx_quasiquote(ctx)) || (d == ctx_unquote(ctx) || (d == ctx_unquote_splicing(ctx)));
        if (quote && !(is_pair(cdr(c)) && is_nil(cadr(c)) && is_nil(cddr(c)))) quote = TRUE;
        if (!quote) {
            write_char(ctx, port, '(');
        } else {
            c = cdr(c);
            write_cell(ctx, port, d, readable, more, top);
        }
        for (; is_pair(c); c=cdr(c)) {
            write_cell(ctx, port, car(c), readable, more, top);
            if (!is_nil(cdr(c))) write_char(ctx, port, ' ');
            if (!is_nil(cdr(c)) && !is_pair(cdr(c))) {
                write_string(ctx, port, ". ");
                write_cell(ctx, port, cdr(c), readable, more, top);
            }
        }
        if (!quote) write_char(ctx, port, ')');
        return;
    } else if (is_vector(c)) {
        write_string(ctx, port, "#(");
        for (uint i=0; i<vector_length(c); i++) {
            write_cell(ctx, port, vector_data(c)[i], readable, more, top);
            if (i != vector_length(c)-1) write_char(ctx, port, ' ');
        }
        write_char(ctx, port, ')');
        return;
    } else if (is_exception(c)) {
        char *etype = NULL;
        switch (exception_type(c)) {
        case SyntaxError:
            etype = "SyntaxError";
            break;
        case MemoryError:
            etype = "MemoryError";
            break;
        case IndexError:
            etype = "IndexError";
            break;
        case IOError:
            etype = "IOError";
            break;
        case TypeError:
            etype = "TypeError";
            break;
        case ValueError:
            etype = "ValueError";
            break;
        case ReferenceError:
            etype = "ReferenceError";
            break;
        case ArithmeticError:
            etype = "ArithmeticError";
            break;
        }
        write_string(ctx, port, etype);
        write_string(ctx, port, ": ");
        write_string(ctx, port, string(exception_msg(c)));
        if (exception_trg(c)) {
            write_char(ctx, port, ' ');
            write_cell(ctx, port, exception_trg(c), readable, more, top);
        }
        write_char(ctx, port, '\n');
        return;
    } else if (is_closure_expr(c)) {
        write_cell(ctx, port, closure_expr_expr(c), readable, more, top);
        return;
    } else {
        snprintf(s, STR_BUF_SIZE, "unknown:%p", c);
    }
    write_string(ctx, port, s);
}

static void print_cell_more(Cell *ctx, Cell *p, Cell *c) {
    write_cell(ctx, p, c, 0, 1, 1);
}

static void print_cell_readable(Cell *ctx, Cell *p, Cell *c) {
    write_cell(ctx, p, c, 1, 0, 1);
}

static void print_cell(Cell *ctx, Cell *p, Cell *c) {
    write_cell(ctx, p, c, 0, 0, 1);
}

/**************** numeric operations *******************/
static void *memdup(void* src, int n) {
    void* des = cell_malloc(n);
    if (des) memcpy(des, src, n);
    return des;
}

long num_gcd(long bg, long sm)
{
    if (bg < 0) bg = -bg;
    if (sm < 0) sm = -sm;
    if (bg < sm)
    {
        return num_gcd(sm, bg);
    }

    if(sm == 0) return bg;
    long res = bg % sm;
    while (res != 0)
    {
        bg = sm;
        sm = res ;
        res = bg % sm;
    }
    return sm;
}

static Cell *exact_to_inexact(Cell *ctx, Cell *num) {
    switch (number_type(num)) {
    case NUMBER_LONG:
        number_type(num) = NUMBER_DOUBLE;
        number_double(num) = number_long(num);
        break;
    case NUMBER_DOUBLE:
        return num;
    case NUMBER_FRACTION:
    {
        number_type(num) = NUMBER_DOUBLE;
        Cell *nr = number_fn_nr(num);
        Cell *dr = number_fn_dr(num);
        number_double(num) = (double)number_long(nr) / (double)number_long(dr);
        break;
    }
    case NUMBER_COMPLEX:
        exact_to_inexact(ctx, number_cx_rl(num));
        exact_to_inexact(ctx, number_cx_im(num));
        break;
    }
    return num;
}

static Cell *inexact_to_exact(Cell *ctx, Cell *num) {
    switch (number_type(num)) {
    case NUMBER_LONG:
        return num;
    case NUMBER_DOUBLE:
    {
        int n = 0;
        double decimal = 0.0, inter = 0;
        number_type(num) = NUMBER_FRACTION;
        decimal = modf(number_double(num), &inter);
        if (decimal > 0.0) {
            for(;;) {
                decimal *= 10;
                ++n;
                if (decimal - (long)decimal < 0.000001) {
                    break;
                }
            }
        }
        long bg = pow(10, n);
        long divisor = num_gcd(bg, decimal);
        number_fn_dr(num) = number_new(ctx);
        number_type(number_fn_dr(num)) = NUMBER_LONG;
        number_long(number_fn_dr(num)) = bg / divisor;

        number_fn_nr(num) = number_new(ctx);
        number_type(number_fn_nr(num)) = NUMBER_LONG;
        number_long(number_fn_nr(num)) = decimal / divisor + number_long(number_fn_dr(num)) * inter;
        break;
    }
    case NUMBER_FRACTION:
        return num;
    case NUMBER_COMPLEX:
        number_cx_rl(num) = inexact_to_exact(ctx, number_cx_rl(num));
        number_cx_im(num) = inexact_to_exact(ctx, number_cx_im(num));
        break;
    }
    return num;
}

static Cell *mk_long(Cell *ctx, long l) {
    Cell *num = number_new(ctx);
    if (num) {
        number_type(num) = NUMBER_LONG;
        number_long(num) = l;
    }
    return num;
}
static Cell *mk_double(Cell *ctx, double d) {
    Cell *num = number_new(ctx);
    if (num) {
        number_type(num) = NUMBER_DOUBLE;
        number_double(num) = d;
    }
    return num;
}
static Cell *mk_fraction(Cell *ctx, long nr, long dr) {
    char s = 1;
    long gcd = num_gcd(nr, dr);
    Cell *num = number_new(ctx);
    if (num) {
        if (dr < 0) s = -1;
        number_type(num) = NUMBER_FRACTION;
        number_fn_nr(num) = number_new(ctx);
        if (number_fn_nr(num)) {
            number_type(number_fn_nr(num)) = NUMBER_LONG;
            number_long(number_fn_nr(num)) = s * nr / gcd;
        }

        number_fn_dr(num) = number_new(ctx);
        if (number_fn_dr(num)) {
            number_type(number_fn_dr(num)) = NUMBER_LONG;
            number_long(number_fn_dr(num)) = s * dr / gcd;
        }
    }
    return num;
}

static Cell *mk_complex(Cell *ctx, Cell *rl, Cell *im) {
    Cell *num = number_new(ctx);
    if (num) {
        number_type(num) = NUMBER_COMPLEX;
        number_cx_rl(num) = rl;
        number_cx_im(num) = im;
    }
    return num;
}

static Cell *_num_calcu(Cell *ctx, int op, Cell *a, Cell *b) {
    Cell *num = NULL;
    long nr, dr, gcd;
    switch (number_type(a)) {
    case NUMBER_LONG:
        switch (number_type(b)) {
        case NUMBER_LONG:
            switch (op) {
            case OP_ADD:
                num = mk_long(ctx, number_long(a) + number_long(b));
                break;
            case OP_SUB:
                num = mk_long(ctx, number_long(a) - number_long(b));
                break;
            case OP_MULTI:
                num = mk_long(ctx, number_long(a) * number_long(b));
                break;
            case OP_DIV:
            {
                gcd = num_gcd(number_long(a), number_long(b));
                if (gcd == number_long(b)) {
                    num = mk_long(ctx, number_long(a) / gcd);
                } else {
                    num = mk_fraction(ctx, number_long(a), number_long(b));
                }
                break;
            }}
            break;
        case NUMBER_DOUBLE:
            num = _num_calcu(ctx, op, exact_to_inexact(ctx, a), b);
            break;
        case NUMBER_FRACTION:
        {
            nr = number_long(number_fn_nr(b));
            dr = number_long(number_fn_dr(b));
            switch (op) {
            case OP_ADD:
                nr = nr + number_long(a) * dr;
                num = mk_fraction(ctx, nr, dr);
                break;
            case OP_SUB:
                nr = number_long(a) * dr - nr;
                num = mk_fraction(ctx, nr, dr);
                break;
            case OP_MULTI:
            {
                nr = number_long(a) * nr;
                gcd = num_gcd(number_long(a) * number_long(number_fn_nr(b)), dr);
                if (gcd == dr)
                    num = mk_long(ctx, nr / dr);
                else
                    num = mk_fraction(ctx, nr, dr);
                break;
            }
            case OP_DIV:
            {
                nr = number_long(a) * dr;
                dr = number_long(number_fn_nr(b));
                gcd = num_gcd(nr, dr);
                if (gcd == nr)
                    num = mk_long(ctx, nr / dr);
                else
                    num = mk_fraction(ctx, nr, dr);
                break;
            }}
            break;
        }
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, _num_calcu(ctx, op, a, number_cx_rl(b)), number_cx_im(b));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, _num_calcu(ctx, op, a, number_cx_rl(b)), _num_calcu(ctx, op, a, number_cx_im(b)));
                break;
            case OP_DIV:
            {
                Cell *rl, *im, *nr, *dr;
                nr = _num_calcu(ctx, OP_MULTI, a, number_cx_rl(b));
                dr = _num_calcu(ctx, OP_ADD, _num_calcu(ctx, OP_MULTI, number_cx_rl(b), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_im(b), number_cx_im(b)));
                rl = _num_calcu(ctx, OP_DIV, nr, dr);
                
                nr = _num_calcu(ctx, OP_MULTI, mk_long(ctx, -1), _num_calcu(ctx, OP_MULTI, a, number_cx_im(b)));
                im = _num_calcu(ctx, OP_DIV, nr, dr);
                num = mk_complex(ctx, rl, im);
                break;
            }}
            break;
        }
        break;
    case NUMBER_DOUBLE:
        switch (number_type(b)) {
        case NUMBER_LONG:
        case NUMBER_FRACTION:
            num = _num_calcu(ctx, op, a, exact_to_inexact(ctx, b));
            break;
        case NUMBER_DOUBLE:
            switch (op) {
            case OP_ADD:
                num = mk_double(ctx, number_double(a) + number_double(b));
                break;
            case OP_SUB:
                num = mk_double(ctx, number_double(a) - number_double(b));
                break;
            case OP_MULTI:
                num = mk_double(ctx, number_double(a) * number_double(b));
                break;
            case OP_DIV:
                num = mk_double(ctx, number_double(a) / number_double(b));
            }
            break;
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, _num_calcu(ctx, op, a, number_cx_rl(b)), number_cx_im(b));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, _num_calcu(ctx, op, a, number_cx_rl(b)), _num_calcu(ctx, op, a, number_cx_im(b)));
                break;
            case OP_DIV:
            {
                double dr = pow(number_double(number_cx_rl(b)), 2) + pow(number_double(number_cx_im(b)), 2);
                num = mk_complex(ctx, mk_double(ctx, number_double(a) * number_double(number_cx_rl(b)) / dr), mk_double(ctx, -1 * number_double(a) * number_double(number_cx_im(b)) / dr));
                break;
            }}
            break;
        }
        break;
    case NUMBER_FRACTION:
        switch (number_type(b)) {
        case NUMBER_LONG:
            switch (op) {
            case OP_ADD:
            case OP_MULTI:
                num = _num_calcu(ctx, op, b, a);
                break;
            case OP_SUB:
                nr = number_long(number_fn_nr(a));
                dr = number_long(number_fn_dr(a));
                nr = nr - number_long(b) * dr;
                num = mk_fraction(ctx, nr, dr);
                break;
            case OP_DIV:
                nr = number_long(number_fn_nr(a));
                dr = number_long(number_fn_nr(a)) * number_long(b);
                gcd = num_gcd(nr, dr);
                if (gcd == nr)
                    num = mk_long(ctx, nr / dr);
                else
                    num = mk_fraction(ctx, nr, dr);
                break;
            }
            break;
        case NUMBER_DOUBLE:
            num = _num_calcu(ctx, op, exact_to_inexact(ctx, a), b);
            break;
        case NUMBER_FRACTION:
        {
            switch (op) {
            case OP_ADD:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_dr(b)) + number_long(number_fn_dr(a)) * number_long(number_fn_nr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_dr(b));
                break;
            case OP_SUB:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_dr(b)) - number_long(number_fn_dr(a)) * number_long(number_fn_nr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_dr(b));
                break;
            case OP_MULTI:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_nr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_dr(b));
                break;
            case OP_DIV:
                nr = number_long(number_fn_nr(a)) * number_long(number_fn_dr(b));
                dr = number_long(number_fn_dr(a)) * number_long(number_fn_nr(b));
                break;
            }
            gcd = num_gcd(nr, dr);
            if (gcd == dr)
                num = mk_long(ctx, nr / dr);
            else
                num = mk_fraction(ctx, nr, dr);
            break;
        }
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, _num_calcu(ctx, op, a, number_cx_rl(b)), number_cx_im(b));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, _num_calcu(ctx, OP_MULTI, a, number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, a, number_cx_im(b)));
                break;
            case OP_DIV:
            {
                Cell *rl, *im, *nr, *dr;
                nr = _num_calcu(ctx, OP_MULTI, a, number_cx_rl(b));
                dr = _num_calcu(ctx, OP_ADD, _num_calcu(ctx, OP_MULTI, number_cx_rl(b), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_im(b), number_cx_im(b)));
                rl = _num_calcu(ctx, OP_DIV, nr, dr);
                
                nr = _num_calcu(ctx, OP_MULTI, mk_long(ctx, -1), _num_calcu(ctx, OP_MULTI, a, number_cx_im(b)));
                im = _num_calcu(ctx, OP_DIV, nr, dr);
                num = mk_complex(ctx, rl, im);
                break;
            }}
        }
        break;
    case NUMBER_COMPLEX:
        switch (number_type(b)) {
        case NUMBER_COMPLEX:
            switch (op) {
            case OP_ADD:
            case OP_SUB:
                num = mk_complex(ctx, _num_calcu(ctx, op, number_cx_rl(a), number_cx_rl(b)),
                                     _num_calcu(ctx, op, number_cx_im(a), number_cx_im(b)));
                break;
            case OP_MULTI:
                num = mk_complex(ctx, _num_calcu(ctx, OP_SUB, _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_im(b))),
                                     _num_calcu(ctx, OP_ADD, _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_im(b))));
                break;
            case OP_DIV:
            {
                Cell *rl, *im, *nr, *dr;
                nr = _num_calcu(ctx, OP_ADD, _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_im(b)));
                dr = _num_calcu(ctx, OP_ADD, _num_calcu(ctx, OP_MULTI, number_cx_rl(b), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_im(b), number_cx_im(b)));
                rl = _num_calcu(ctx, OP_DIV, nr, dr);

                nr = _num_calcu(ctx, OP_SUB, _num_calcu(ctx, OP_MULTI, number_cx_im(a), number_cx_rl(b)), _num_calcu(ctx, OP_MULTI, number_cx_rl(a), number_cx_im(b)));
                im = _num_calcu(ctx, OP_DIV, nr, dr);
                num = mk_complex(ctx, rl, im);
                break;
            }}
            break;
        default:
            switch (op) {
            case OP_ADD:
            case OP_MULTI:
                num = _num_calcu(ctx, op, b, a);
                break;
            case OP_SUB:
                num = mk_complex(ctx, _num_calcu(ctx, op, number_cx_rl(a), b), number_cx_im(a));
                break;
            case OP_DIV:
                num = mk_complex(ctx, _num_calcu(ctx, op, number_cx_rl(a), b), _num_calcu(ctx, op, number_cx_im(a), b));
                break;
            }
        }
        break;
    }
    return num;
}

static Cell *num_calcu(Cell *ctx, int op, Cell *a, Cell *b) {
    Cell *sum = NULL;
    if (number_type(a) == NUMBER_DOUBLE ||
        (number_type(a) == NUMBER_COMPLEX && 
            (number_type(number_cx_rl(a)) == NUMBER_DOUBLE ||
             number_type(number_cx_im(a)) == NUMBER_DOUBLE))) {
        if (number_type(b) == NUMBER_LONG || number_type(b) == NUMBER_FRACTION ||
            (number_type(b) == NUMBER_COMPLEX && number_type(number_cx_rl(b)) != NUMBER_DOUBLE))
        b = exact_to_inexact(ctx, b);
    } else {
        if (number_type(b) == NUMBER_DOUBLE ||
            (number_type(b) == NUMBER_COMPLEX && 
                (number_type(number_cx_rl(b)) == NUMBER_DOUBLE ||
                 number_type(number_cx_im(b)) == NUMBER_DOUBLE))) {
            a = exact_to_inexact(ctx, a);
        }
    }
    return _num_calcu(ctx, op, a, b);
}

static Cell *read_real(Cell *ctx, char **ppstart, char *pend, Exactness exact, Radix radix) {
    Cell *num = NULL;
    char *p = *ppstart;
    int c = *p;
    char sign = 1;
    int buflen = 0;
    bool find_num = FALSE;
    bool find_dot = FALSE;
    bool find_slash = FALSE;
    char *buf = NULL;

    if (pend - p >= STR_BUF_SIZE) {
        goto Error;
    }
    buf = cell_malloc(STR_BUF_SIZE);
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
        long nr = xtod(buf, buflen, radix);
        buflen = pend - pstart + 1;
        memcpy(buf, pstart, buflen - 1);
        buf[buflen - 1] = '\0';
        
        long dr = xtod(buf, buflen, radix);
        num = mk_fraction(ctx, sign * nr, dr);
    } else {
        if (!find_num) {
            goto Error;
        }
        num = number_new(ctx);
        if (!num) {
            goto Error;
        }
        buflen = pend - pstart + 1;
        memcpy(buf, pstart, buflen - 1);
        buf[buflen - 1] = '\0';
        double db = xtod(buf, buflen, radix);
        if (find_dot) {
            number_type(num) = NUMBER_DOUBLE;
            number_double(num) = sign * db;
        } else {
            number_type(num) = NUMBER_LONG;
            number_long(num) = sign * db;
        }
    }
    *ppstart = pend;
    if (exact == EXACT)
        num = inexact_to_exact(ctx, num);
    else if (exact == INEXACT)
        num = exact_to_inexact(ctx, num);

Error:
    if (buf) {
        cell_free(buf);
    }
    return num;
}

static Cell *read_number(Cell *ctx, char *pstart, uint size, Exactness exact, Radix radix) {
    char *p = pstart;
    char *pend = p + size;
    Cell *real = NULL, *imag = NULL;
    
    real = read_real(ctx, &p, pend, exact, radix);
    if (!real || p > pend)
        goto Error;
 
    if (p == pend) {
        return real;
    } else {
        int c = *p;
        if (c == '+' || c == '-') {
            imag = read_real(ctx, &p, pend, exact, radix);
            if (!imag || p > pend) {
                goto Error;
            }
            c = *p;
            if (p != pend - 1 || (c != 'i' && c != 'I')) {
                goto Error;
            }
        } else if (c == 'i' || c == 'I') {
            if (p != pend - 1) {
                goto Error;
            }
            imag = real;
            if (number_type(imag) == NUMBER_LONG || number_type(imag) == NUMBER_FRACTION)
                real = mk_long(ctx, 0);
            else
                real = mk_double(ctx, 0);
        }
        if (exact == NO_EXACTNESS &&
            (number_type(real) == NUMBER_DOUBLE || number_type(imag) == NUMBER_DOUBLE)) 
        {
            if (number_type(real) != NUMBER_DOUBLE)
                real = exact_to_inexact(ctx, real);
            if (number_type(imag) != NUMBER_DOUBLE)
                imag = exact_to_inexact(ctx, imag);
        }
        Cell *num = number_new(ctx);
        if (!num) {
            goto Error;
        }     
        number_type(num) = NUMBER_COMPLEX;
        number_cx_rl(num) = real;
        number_cx_im(num) = imag;
        return num;
    }
    
Error:
    if (real) cell_free(real);
    if (imag) cell_free(imag);
    return CELL_NIL;
}

static Cell *read_symbol(Cell *ctx, int _) {
    uint size = STR_BUF_SIZE;
    char buf[STR_BUF_SIZE] = "";
    char *p = buf;
    int total_len = read_upto(ctx_inport(ctx), DELIMITERS, &p, &size);

    if (total_len <= 0) {
        return CELL_EOF;
    }
    Cell *c = read_number(ctx, p, total_len, NO_EXACTNESS, NO_RADIX);
    if (c == CELL_NIL) {
        if ((c = find_symbol(ctx, p)) == CELL_NIL) {
            c = mk_symbol(ctx, p);
        }
    }
    if (size != STR_BUF_SIZE) cell_free(p);
    return c;
}

static Cell *read_const(Cell *ctx, int c) {
    Cell *ret = NULL;
    Cell *real = NULL, *imag = NULL;
    uint size = STR_BUF_SIZE;
    char buf[STR_BUF_SIZE] = "";
    char *p = buf;
    int total_len = read_upto(ctx_inport(ctx), DELIMITERS, &p, &size);

    if (total_len <= 0) {
        goto Error;
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
                    return mk_char(ctx, '\t');
                } else if (pend - p == 5 && !strncasecmp(p, "space", 5)) {
                    return mk_char(ctx, ' ');
                } else if (pend - p == 6 && !strncasecmp(p, "return", 6)) {
                    return mk_char(ctx, '\r');
                } else if (pend - p == 7 && !strncasecmp(p, "newline", 7)) {
                    return mk_char(ctx, '\n');
                }
                goto Error;
            }
            return mk_char(ctx, p[0]);
        default:
            goto Error;
        }
        p += 2;
    }

    Cell *num = read_number(ctx, p, total_len, exact, radix);
    if (num != CELL_NIL) {
        if (size != STR_BUF_SIZE) cell_free(p);
        return num;
    }

Error:
    if (size != STR_BUF_SIZE) cell_free(p);
    return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
}

static Cell *read_string(Cell *ctx, int q) {
    char buf[STR_BUF_SIZE] = "";
    char *p = buf;
    Cell *ret = NULL;
    Cell *port = ctx_inport(ctx);
    int buf_size = STR_BUF_SIZE;
    int idx = 0;
    int c;
    while ((c = get_char(port)) > 0 && c != '\"')
    {
        if (idx >= buf_size) {
            char *tmp;
            int tmp_size = buf_size * 5;
            tmp = cell_malloc(tmp_size);
            if (!tmp) {
                ret = mk_exception(ctx, MemoryError, mk_string(ctx, "no memory"), NULL, NULL);
                goto Err;
            }
            memcpy(tmp, p, idx);
            p = tmp;
            buf_size = tmp_size;
        }
        if ('\\' == c)
        {
            c = get_char(port);
            switch (c) {
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case 'f': c = '\f'; break;
            case 'n': c = '\n'; break;
            case 'r': c = '\r'; break;
            case 't': c = '\t'; break;
            case 'v': c = '\v'; break;
            case '\\':
            case '\'':
            case '\"': break;
            default:
                ret = mk_exception(ctx, IOError, mk_string(ctx, "invalid escape sequence in string"), NULL, NULL);
                goto Err;
            }
            p[idx++] = c;
        }
        else
            p[idx++] = c;
    }
    if (c != '\"') {
        ret = mk_exception(ctx, IOError, mk_string(ctx, "EOF in string"), NULL, NULL);
        goto Err;
    }
    p[idx++] = '\0';
    
    if (buf_size == STR_BUF_SIZE) {
        p = string_dup(buf);
        if (!p) {
            ret = mk_exception(ctx, MemoryError, mk_string(ctx, "no memory"), NULL, NULL);
            goto Err;
        }
    }
    return mk_string(ctx, p);

Err:
   if (buf_size != STR_BUF_SIZE) cell_free(p);
   return ret;
}

static Cell *read_quote(Cell *ctx, int c) {
    Cell *cell = read_cell(ctx);
    if (cell == CELL_EOF || is_exception(cell))
        return cell;
    cell = cons(ctx, ctx_quote(ctx), cons(ctx, cell, CELL_NIL));
    return cell;
}

static Cell *read_quasiquote(Cell *ctx, int c) {
    Cell *cell = read_cell(ctx);
    if (cell == CELL_EOF || is_exception(cell))
        return cell;
    cell = cons(ctx, ctx_quasiquote(ctx), cons(ctx, cell, CELL_NIL));
    return cell;
}

static Cell *read_unquote(Cell *ctx, int c) {
    Cell *cell = read_cell(ctx);
    if (cell == CELL_EOF || is_exception(cell))
        return cell;
    cell = cons(ctx, ctx_unquote(ctx), cons(ctx, cell, CELL_NIL));
    return cell;
}

static Cell *read_unquote_splicing(Cell *ctx, int c) {
    Cell *cell = read_cell(ctx);
    if (cell == CELL_EOF || is_exception(cell))
        return cell;
    cell = cons(ctx, ctx_unquote_splicing(ctx), cons(ctx, cell, CELL_NIL));
    return cell;
}

static Cell *read_vector(Cell *ctx, int c) {
    uint len = 0;
    Cell *head, *tail, *cell = CELL_NIL;
    head = tail = cons(ctx, CELL_NIL, CELL_NIL);
    for (;;) {
        c = get_token(ctx);
        if (c == TOK_EOF) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unexpected eof"), NULL, NULL);
        }
        if (c == TOK_RPAREN) break;
        if (c == TOK_RPAREN || c == TOK_RBRACKET || c == TOK_RBRACE) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched brackets"), NULL, NULL);
        }

        if (c == TOK_DOT) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal used of dot"), NULL, NULL);
        } else if (c == TOK_ELLIPSIS) {
            cell = CELL_ELLIPSIS;
        } else {
            cell = read_cell_by_token(ctx, c);
        }
        if (cell == CELL_EOF || is_exception(cell))
            return cell;
        tail = rplacd(tail, cons(ctx, cell, CELL_NIL));
        ++len;
    }
    cell = mk_vector(ctx, len, CELL_UNDEF);
    uint i = 0;
    vector_length(cell) = len;
    for (Cell *ls=cdr(head); i<len && is_pair(ls); i++, ls=cdr(ls)) {
        vector_data(cell)[i] = car(ls);
    }
    return cell;
}

static Cell *read_list(Cell *ctx, int c) {
    Cell *head, *tail, *cell = CELL_NIL;
    head = tail = cons(ctx, CELL_NIL, CELL_NIL);

    switch (c) {
    case TOK_LPAREN:    c = TOK_RPAREN; break;
    case TOK_LBRACKET:  c = TOK_RBRACKET; break;
    case TOK_LBRACE:    c = TOK_RBRACE; break;
    }

    int d;
    Cell *port = ctx_inport(ctx);
    for (;;) {
        d = get_token(ctx);
        if (d == TOK_EOF) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unexpected eof"), NULL, NULL);
        }
        if (c == d) break;
        if (d == TOK_RPAREN || d == TOK_RBRACKET || d == TOK_RBRACE) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched brackets"), NULL, NULL);
        }

        if (d == TOK_DOT) {
            if (cell == CELL_NIL) {
                return mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal used of dot"), NULL, NULL);
            }
            cell = read_cell(ctx);
            if (cell == CELL_EOF || is_exception(cell))
                return cell;
            tail = rplacd(tail, cell);
            c = get_token(ctx);
            if (c != TOK_RPAREN) {
                return mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal used of dot"), NULL, NULL);
            }
            break;
        } else if (d == TOK_ELLIPSIS) {
            cell = CELL_ELLIPSIS;
        } else {
            cell = read_cell_by_token(ctx, d);
        }
        if (cell == CELL_EOF || is_exception(cell))
            return cell;
        tail = rplacd(tail, cons(ctx, cell, CELL_NIL));
    }
    return cdr(head);
}

/********************** macro **********************/
static Cell *mk_literal_matcher(Cell *ctx, Cell *name, Cell *env) {
    Cell *c = matcher_new(ctx);
    if (c) {
        matcher_type(c) = MatcherLiteral;
        matcher_name(c) = name;
        matcher_value(c) = env;
    }
    return c;
}

static Cell *mk_variable_matcher(Cell *ctx, Cell *name) {
    Cell *c = matcher_new(ctx);
    if (c) {
        matcher_type(c) = MatcherVariable;
        matcher_name(c) = name;
    }
    return c;
}

static Cell *mk_rest_matcher(Cell *ctx, Cell *mt) {
    Cell *c = matcher_new(ctx);
    if (c) {
        matcher_type(c) = MatcherRest;
        matcher_value(c) = mt;
    }
    return c;
}

static Cell *mk_underscore_matcher(Cell *ctx, Cell *name) {
    Cell *c = matcher_new(ctx);
    if (c) {
        matcher_type(c) = MatcherUnderscore;
        matcher_name(c) = name;
    }
    return c;
}

static Cell *mk_constant_matcher(Cell *ctx, Cell *value) {
    Cell *c = matcher_new(ctx);
    if (c) {
        matcher_type(c) = MatcherConstant;
        matcher_value(c) = value;
    }
    return c;
}

static Cell *mk_sequence_matcher(Cell *ctx) {
    Cell *c = matcher_new(ctx);
    if (c) {
        matcher_type(c) = MatcherSequence;
        matcher_value(c) = NULL;
    }
    return c;
}

static Cell *mk_constant_expander(Cell *ctx, Cell *v) {
    Cell *c = expander_new(ctx);
    if (c) {
        expander_type(c) = ExpanderConstant;
        expander_n(c) = 0;
        expander_value(c) = v;
    }
    return c;
}

static Cell *mk_variable_expander(Cell *ctx, Cell *n) {
    Cell *c = expander_new(ctx);
    if (c) {
        expander_type(c) = ExpanderVariable;
        expander_n(c) = 0;
        expander_name(c) = n;
    }
    return c;
}

static Cell *mk_sequence_expander(Cell *ctx) {
    Cell *c = expander_new(ctx);
    if (c) {
        expander_type(c) = ExpanderSequence;
    }
    return c;
}

static Cell *sequence_matcher_add(Cell *ctx, Cell *seq, Cell *sub) {
    if (matcher_value(seq) == NULL) {
        matcher_value(seq) = cons(ctx, sub, CELL_NIL);
        return CELL_TRUE;
    }
    list_add(ctx, matcher_value(seq), sub);
    return CELL_TRUE;
}

static Cell *sequence_expander_add(Cell *ctx, Cell *seq, Cell *sub) {
    if (expander_name(seq) == NULL) {
        if (expander_type(sub) == ExpanderVariable)
            expander_name(seq) = cons(ctx, expander_name(sub), CELL_NIL);
        else if (expander_type(sub) == ExpanderSequence)
            expander_name(seq) = expander_name(sub);
    } else {
        if (expander_type(sub) == ExpanderVariable)
            list_add(ctx, expander_name(seq), expander_name(sub));
        else if (expander_type(sub) == ExpanderSequence)
            list_extend(ctx, expander_name(seq), expander_name(sub));
    }

    if (expander_value(seq) == NULL) {
        expander_value(seq) = cons(ctx, sub, CELL_NIL);
    } else {
        list_add(ctx, expander_value(seq), sub);
    }
}

static Cell *syntax_pattern_match(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env, Cell *md);
static Cell *syntax_template_expand(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx);

static bool syntax_pattern_match_literal(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env) {
    if (!is_symbol(expr)) {
        return FALSE;
    }
    Cell *c1 = find_env(expr, expr_env);
    Cell *c2 = find_env(matcher_name(mt), matcher_value(mt));
    if (c1 == c2) {
        return TRUE;
    }
    return FALSE;
}

static Cell *syntax_pattern_match_sequence(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env, Cell *md) {
    for (Cell *ls=matcher_value(mt); is_pair(ls); ls=cdr(ls)) {
        expr = syntax_pattern_match(ctx, car(ls), expr, expr_env, md);
        if (expr == CELL_ERR) break;
    }
    return expr;
}

Cell *syntax_pattern_match(Cell *ctx, Cell *mt, Cell *expr, Cell *expr_env, Cell *md) {
    switch (matcher_type(mt)) {
    case MatcherLiteral:
        if (matcher_repeat(mt)) {
            while (is_pair(expr)) {
                if (!syntax_pattern_match_literal(ctx, mt, car(expr), expr_env)) break;
                expr = cdr(expr);
            }
            return expr;
        }
        if (!syntax_pattern_match_literal(ctx, mt, car(expr), expr_env))
            return CELL_ERR;
        return cdr(expr);
    case MatcherConstant:
        if (!is_pair(expr) || !equal(car(expr), matcher_value(mt))){
            return CELL_ERR;
        }
        if (matcher_repeat(mt)) {
            expr = cdr(expr);
            while (is_pair(expr) && equal(car(expr), matcher_value(mt)))
                expr = cdr(expr);
            return expr;
        }
        return cdr(expr);
    case MatcherVariable: {
        Cell *rt = CELL_NIL;
        if (matcher_repeat(mt)) {
            if (!is_pair(expr)) return expr;
            rt = cons(ctx, CELL_NIL, CELL_NIL);
            while (is_pair(expr)) {
                list_add(ctx, rt, car(expr));
                expr = cdr(expr);
            }
            rt = cdr(rt);
        } else {
            if (!is_pair(expr))
                return CELL_ERR;
            rt = car(expr);
            expr = cdr(expr);
        }
        alist_append(ctx, md, cons(ctx, matcher_name(mt), rt));
        return expr;
    }
    case MatcherUnderscore:
        if (matcher_repeat(mt)) {
            while (is_pair(expr)) expr = cdr(expr);
        } else {
            if (!is_pair(expr)) return CELL_ERR;
            expr = cdr(expr);
        }
        return expr;
    case MatcherRest:
        return syntax_pattern_match(ctx, matcher_value(mt), cons(ctx, expr, CELL_NIL), expr_env, md);
    case MatcherSequence:
        if (matcher_repeat(mt)) {
            Cell *tmp_md = cons(ctx, CELL_NIL, CELL_NIL);
            while (is_pair(expr)) {
                if (syntax_pattern_match_sequence(ctx, mt, car(expr), expr_env, tmp_md) != CELL_NIL) break;
                expr = cdr(expr);
            }
            alist_update(ctx, md, cdr(tmp_md));
            return expr;
        }
        if (!is_pair(expr)) return CELL_ERR;
        if (syntax_pattern_match_sequence(ctx, mt, car(expr), expr_env, md) != CELL_NIL) return CELL_ERR;
        return cdr(expr);
    }
    return CELL_ERR;
}

static Cell *_variable_expand(Cell *ctx, Cell *v) {
    Cell *ret = cons(ctx, CELL_NIL, CELL_NIL);
    for (; is_pair(v); v=cdr(v)) {
        list_extend(ctx, ret, car(v));
    }
    return cdr(ret);
}

static Cell *_sequence_expand0(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx) {
    Cell *ret = cons(ctx, CELL_NIL, CELL_NIL);
    for (Cell *ls=expander_value(expd); is_pair(ls); ls=cdr(ls)) {
        Cell *c = syntax_template_expand(ctx, car(ls), md, env, idx);
        if (c) list_extend(ctx, ret, c);
    }
    return cons(ctx, cdr(ret), CELL_NIL);
}

static Cell *_sequence_expand(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx, int expd_n) {
    if (expd_n == 0) return _sequence_expand0(ctx, expd, md, env, idx);
    int len=0;
    for (Cell *c=expander_name(expd); is_pair(c); c=cdr(c)) {
        Cell *var = assq(car(c), md);
        var = cadr(var);
        for (Cell *ls=idx; is_pair(var) && is_pair(ls) && !is_nil(car(ls)); ls=cdr(ls)) {
            var = list_ref(var, car(ls));
        }
        if (!is_pair(var)) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "too many ellipsis for variable:"), c, NULL);
        }
        if (len == 0 || len == length(var))
            len = length(var);
        else
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched ellipsis counts for variable:"), c, NULL);
    }
    if (len > 0) {
        list_add(ctx, idx, mk_long(ctx, 0));
        Cell *ret = cons(ctx, CELL_NIL, CELL_NIL);
        for (int i=0; i<len; i++) {
            list_set(idx, mk_long(ctx, length(idx) - 1), mk_long(ctx, i));
            list_extend(ctx, ret, _sequence_expand(ctx, expd, md, env, idx, expd_n - 1));
        }
        list_pop(idx);
        return cdr(ret);
    }
    return CELL_NIL;
}

static Cell *_transform_closure_expr(Cell *ctx, Cell *expr, Cell *env) {
    if (is_pair(expr)) {
        for (Cell *ls=expr, *c; is_pair(ls); ls=cdr(ls)) {
            c = car(ls);
            if (is_pair(c) || is_symbol(c)) {
                rplaca(ls, mk_closure_expr(ctx, c, env));
            }
        }
        return expr;
    } else if (is_symbol(expr)) {
        return mk_closure_expr(ctx, expr, env);
    }
    return expr; 
}

static Cell *syntax_template_expand(Cell *ctx, Cell *expd, Cell *md, Cell *env, Cell *idx) {
    switch (expander_type(expd)) {
    case ExpanderConstant:
        return expander_value(expd);
    case ExpanderVariable: {
        Cell *var = assq(expander_name(expd), md);
        if (is_nil(var)) return NULL;
        var = cdr(var);
        for (Cell *ls=cdr(idx); is_pair(ls) && !is_nil(car(ls)) && is_pair(var); ls=cdr(ls)) {
            var = list_ref(var, car(ls));
        }
        uint n = expander_n(expd);
        while (n > 0) {
            var = _variable_expand(ctx, var);
            if (var == CELL_ERR) {
                return mk_exception(ctx, SyntaxError, mk_string(ctx, "too many ellipsis for variable:"), expander_name(expd), NULL);
            }
            --n;
        }
        return _transform_closure_expr(ctx, var, env);
    }
    case ExpanderSequence:
        return _sequence_expand(ctx, expd, md, env, idx, expander_n(expd));
    }
    return CELL_ERR;
}

static bool is_contains(Cell *ls, Cell *c) {
    for (; is_pair(ls); ls=cdr(ls)) {
        if (equal(car(ls), c))
            return TRUE;
    }
    return FALSE;
}

static Cell *_syntax_pattern_analyze(Cell *ctx, Cell *lit, Cell *pat, Cell *syn_env, Cell *pat_vars) {
    if (is_pair(pat)) {
        bool ellipsis = FALSE;
        Cell *mt = mk_sequence_matcher(ctx);
        Cell *sub;
        while (is_pair(pat)) {
            sub = _syntax_pattern_analyze(ctx, lit, car(pat), syn_env, pat_vars);
            if (is_exception(sub)) return sub;
            sequence_matcher_add(ctx, mt, sub);
            pat = cdr(pat);
            if (is_pair(pat) && is_ellipsis(car(pat))) {
                if (ellipsis == TRUE) mk_exception(ctx, SyntaxError, mk_string(ctx, "misplaced ellipsis in pattern"), NULL, NULL);
                ellipsis = TRUE;
                matcher_repeat(sub) = TRUE;
                pat = cdr(pat);
            }
        }
        if (!is_nil(pat)) {
            sub = _syntax_pattern_analyze(ctx, lit, pat, syn_env, pat_vars);
            sequence_matcher_add(ctx, mt, mk_rest_matcher(ctx, sub));
        }
        return mt;
    } else if (is_symbol(pat)) {
        if (is_contains(lit, pat)) {
            return mk_literal_matcher(ctx, pat, syn_env);
        } else if (!strcmp(symbol_data(pat), "_")) {
            return mk_underscore_matcher(ctx, pat);
        } else if (is_contains(pat_vars, pat)) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "duplicated pattern variable:"), pat, NULL);
        }
        list_add(ctx, pat_vars, pat);
        return mk_variable_matcher(ctx, pat);
    }
    return mk_constant_matcher(ctx, pat);
}

static Cell *syntax_pattern_analyze(Cell *ctx, Cell *lit, Cell *pattern, Cell *syn_env, Cell *pat_vars) {
    if (!is_pair(pattern) || !is_symbol(car(pattern))) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax pattern format:"), pattern, NULL);
    }
    return _syntax_pattern_analyze(ctx, lit, cdr(pattern), syn_env, pat_vars);
}

Cell *syntax_template_analyze(Cell *ctx, Cell *tmpl, Cell *pat_vars) {
    if (is_pair(tmpl)) {
        Cell *seq = mk_sequence_expander(ctx);
        while (is_pair(tmpl)) {
            Cell *sub = syntax_template_analyze(ctx, car(tmpl), pat_vars);
            tmpl = cdr(tmpl);
            while (is_pair(tmpl) && is_ellipsis(car(tmpl))) {
                expander_n(sub) += 1;
                tmpl = cdr(tmpl);
            }
            sequence_expander_add(ctx, seq, sub);
        }
        if (!is_nil(tmpl)) {
            Cell *sub = syntax_template_analyze(ctx, tmpl, pat_vars);
            sequence_expander_add(ctx, seq, sub);
        }
        return seq;
    } else if (is_symbol(tmpl)) {
        if (is_contains(pat_vars, tmpl)) {
            return mk_variable_expander(ctx, tmpl);
        }
    }
    return mk_constant_expander(ctx, tmpl);
}

static Cell *syntax_matcher_analyze(Cell *ctx, Cell *lit, Cell *matches, Cell *syn_env) {
    if (!is_pair(matches)) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax format in syntax rules:"), matches, NULL);
    }

    Cell *machers = cons(ctx, syn_env, CELL_NIL);
    for (Cell *ls=matches, *macher; is_pair(ls); ls=cdr(ls)) {
        Cell *pat_vars = cons(ctx, CELL_NIL, CELL_NIL);
        macher = car(ls);
        if (!is_pair(macher) || !is_pair(car(macher)) || !is_pair(cdr(macher)) || !is_nil(cddr(macher))) {
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax format in syntax rules:"), macher, NULL);
        }
        Cell *pattern = syntax_pattern_analyze(ctx, lit, car(macher), syn_env, pat_vars);
        if (is_exception(pattern)) return pattern;
        Cell *template = syntax_template_analyze(ctx, cadr(macher), cdr(pat_vars));
        if (is_exception(template)) return template;
        macher = cons(ctx, pattern, template);
        list_add(ctx, machers, macher);
    }
    return mk_proc(ctx, CELL_NIL, mk_closure(ctx, cons(ctx, CELL_NIL, cons(ctx, cons(ctx, ctx_quote(ctx), cons(ctx, machers, CELL_NIL)), CELL_NIL)), cons(ctx, CELL_NIL, cdr(syn_env))));
}

static Cell *syntax_transform(Cell *ctx, Cell *machers, Cell *syn_env, Cell *expr, Cell *expr_env) {
    Cell *template = NULL, *md;
    Cell *ls = machers;
    for (; is_pair(ls); ls=cdr(ls)) {
        md = cons(ctx, CELL_NIL, CELL_NIL);
        Cell *rt = syntax_pattern_match(ctx, caar(ls), cons(ctx, expr, CELL_NIL), expr_env, md);
        if (rt == CELL_NIL)
            break;
    }
    if (is_pair(ls) && is_pair(car(ls))) template = cdar(ls);
    if (!template) return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched pattern"), NULL, NULL);
    #ifdef MACRO_DEBUG
    write_string(ctx, ctx_outport(ctx), "\n*Macro match dict*\n");
    print_cell(ctx, ctx_outport(ctx), cdr(md));
    write_char(ctx, ctx_outport(ctx), '\n');
    #endif
    return syntax_template_expand(ctx, template, cdr(md), expr_env, cons(ctx, CELL_NIL, CELL_NIL));
}
/******************** macro end *********************/

static Cell *isc_repl(Cell *ctx) {
    Cell *a, *b, *c, *d, *e;
    ctx_op(ctx) = OP_REPL_LOOP;
Loop:
    if ((c = arg_type_check(ctx)) != CELL_TRUE) {
        ctx_op(ctx) = OP_ERROR;
        ctx_ret(ctx) = c;
    }
    int op = ctx_op(ctx);
    switch (op) {
    case OP_REPL_LOOP:
        if (is_port_eof(ctx_inport(ctx))) {
            if (ctx_file_idx(ctx) == 0) {
                break;
            }
            pop_load_file(ctx);
            popOp(ctx, ctx_ret(ctx));
        }
        if (is_interactive(ctx)) {
            ctx_continue(ctx) = CELL_NIL;
            if (ctx_file_idx(ctx) == 0) {
                write_string(ctx, ctx_outport(ctx), ">> ");
            }
        }
        pushOp(ctx, OP_REPL_LOOP, ctx_args(ctx), ctx_global_env(ctx));
        gotoOp(ctx, OP_REPL_READ);
    case OP_REPL_READ:
        c = read_cell(ctx);
        if (c == CELL_EOF) {
            if (ctx_file_idx(ctx) == 0) {
                break;
            }
            pop_load_file(ctx);
            popOp(ctx, ctx_ret(ctx));

        }
        if (is_exception(c)) {
            gotoErr(ctx, c);
        }
        ctx_code(ctx) = c;
        gotoOp(ctx, OP_REPL_EVAL);
    case OP_REPL_EVAL:
        pushOp(ctx, OP_REPL_PRINT, CELL_NIL, CELL_NIL);
        gotoOp(ctx, OP_EVAL);
    case OP_REPL_PRINT:
        if (is_interactive(ctx)) {
            if (ctx_ret(ctx) != CELL_UNDEF) {
                print_cell(ctx, ctx_outport(ctx), ctx_ret(ctx));
                write_string(ctx, ctx_outport(ctx), "\n");
            }
        }
        popOp(ctx, ctx_ret(ctx));
    case OP_ERROR:
        print_cell(ctx, ctx_outport(ctx), ctx_ret(ctx));
        if (is_interactive(ctx)) {
            int c;
            while ((c = get_char(ctx_inport(ctx))) != '\n' && c != EOF);
            ctx_args(ctx) = CELL_NIL;
            gotoOp(ctx, OP_REPL_LOOP);
        }
        break;
    case OP_DEF:
        c = ctx_code(ctx);
        if (is_pair(d = car(c))) {
            e = cdr(c);
            for (; is_pair(d); d = car(d)) {
                c = car(d);
                e = cons(ctx, ctx_lambda(ctx), cons(ctx, cdr(d), e));
            }
            ctx_code(ctx) = e;
        } else if (is_symbol(c = car(c))) {
            ctx_code(ctx) = cadr(ctx_code(ctx));
        } else {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid define expression"), NULL, NULL));
        }
        pushOp(ctx, OP_DEF1, CELL_NIL, c);
        gotoOp(ctx, OP_EVAL);
    case OP_DEF1:
        c = ctx_ret(ctx);
        if (is_proc(c)) {
            if (is_nil(proc_name(c))) {
                proc_name(c) = ctx_code(ctx);
            }
            mk_env(ctx, closure_env(proc_closure(c)), ctx_code(ctx), c);
        }
        set_env(ctx, ctx_env(ctx), ctx_code(ctx), c);
        popOp(ctx, CELL_UNDEF);
    case OP_SET:
        c = car(ctx_code(ctx));
        if (!is_symbol(c)) gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "parameter is not an identifier:"), c, NULL));
        if (is_nil(find_env(c, cdr(ctx_env(ctx))))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "unbound variable:"), c, NULL));
        }
        if (is_immutable(c)) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "unable to alter immutable atom"), NULL, NULL));
        }
        ctx_code(ctx) = cadr(ctx_code(ctx));
        pushOp(ctx, OP_DEF1, CELL_NIL, c);
        gotoOp(ctx, OP_EVAL);
    case OP_DEF_SYNTAX:
        if (!is_pair(ctx_code(ctx)) || !is_symbol(car(ctx_code(ctx))) || !is_pair(cdr(ctx_code(ctx)))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax defined"), NULL, NULL));
        }
        pushOp(ctx, OP_DEF_SYNTAX1, CELL_NIL, car(ctx_code(ctx)));
        ctx_code(ctx) = cadr(ctx_code(ctx));
        ctx_args(ctx) = CELL_NIL;
        gotoOp(ctx, OP_EVAL);
    case OP_DEF_SYNTAX1:
        if (!is_proc(ctx_ret(ctx))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax defined"), NULL, NULL));
        }
        pushOp(ctx, OP_DEF_SYNTAX2, CELL_NIL, ctx_code(ctx));
        ctx_code(ctx) = ctx_ret(ctx);
        ctx_args(ctx) = CELL_NIL;
        gotoOp(ctx, OP_APPLY);
    case OP_DEF_SYNTAX2:
        if (!is_pair(ctx_ret(ctx)) || !is_pair(car(ctx_ret(ctx))) || !is_pair(cdr(ctx_ret(ctx)))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax defined"), NULL, NULL));
        }
        set_env(ctx, ctx_env(ctx), ctx_code(ctx), mk_macro(ctx, cdr(ctx_ret(ctx)), car(ctx_ret(ctx))));
        popOp(ctx, CELL_UNDEF);
    case OP_SYNTAX_RULES:
        if (!is_pair(ctx_code(ctx)) || !is_pair(cdr(ctx_code(ctx)))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax format in syntax rules:"), ctx_code(ctx), NULL));
        }
        c = syntax_matcher_analyze(ctx, car(ctx_code(ctx)), cdr(ctx_code(ctx)), ctx_env(ctx));
        if (is_exception(c)) {
            gotoErr(ctx, c);
        }
        popOp(ctx, c);
    case OP_LAMBDA:
        popOp(ctx, mk_proc(ctx, CELL_NIL, mk_closure(ctx, ctx_code(ctx), ctx_env(ctx))));
    case OP_EVAL:
        if (is_nil(ctx_code(ctx))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal empty expresion"), NULL, NULL));
        }
        if (is_closure_expr(ctx_code(ctx))) {
            ctx_env(ctx) = closure_expr_env(ctx_code(ctx));
            ctx_code(ctx) = closure_expr_expr(ctx_code(ctx));
        }
        if (is_symbol(ctx_code(ctx))) {
            c = find_env(ctx_code(ctx), cdr(ctx_env(ctx)));
            if (is_pair(c)) {
                c = cdr(c);
                if (is_syntax(c)) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid use of syntax as value:"), ctx_code(ctx), NULL));
                }
                popOp(ctx, c);
            }
            gotoErr(ctx, mk_exception(ctx, ReferenceError, mk_string(ctx, "unbound variable:"), ctx_code(ctx), NULL));
        } else if (is_pair(ctx_code(ctx))) {
            c = car(ctx_code(ctx));
            d = ctx_env(ctx);
            if (is_syntax(c)) {
                ctx_code(ctx) = cdr(ctx_code(ctx));
                gotoOp(ctx, syntax_op(c));
            } else if (is_symbol(c)) {
                c = find_env(c, cdr(d));
                if (is_pair(c)) {
                    c = cdr(c);
                    if (is_syntax(c)) {
                        ctx_code(ctx) = cdr(ctx_code(ctx));
                        gotoOp(ctx, syntax_op(c));
                    } else if (is_macro(c)) {
                        ctx_code(ctx) = syntax_transform(ctx, macro_matchers(c), macro_env(c), cdr(ctx_code(ctx)), ctx_env(ctx));
                        if (is_exception(ctx_code(ctx))) {
                            gotoErr(ctx, ctx_code(ctx));
                        }
                        #ifdef MACRO_DEBUG
                        write_string(ctx, ctx_outport(ctx), "\n*Macro transform code*\n");
                        print_cell(ctx, ctx_outport(ctx), ctx_code(ctx));
                        write_char(ctx, ctx_outport(ctx), '\n');
                        #endif
                        ctx_code(ctx) = mk_closure(ctx, cons(ctx, CELL_NIL, ctx_code(ctx)), macro_env(c));
                        ctx_args(ctx) = CELL_NIL;
                        gotoOp(ctx, OP_APPLY);
                    }
                }
            }
            pushOp(ctx, OP_EVAL_ARGS, CELL_NIL, cdr(ctx_code(ctx)));
            ctx_code(ctx) = car(ctx_code(ctx));
            gotoOp(ctx, OP_EVAL);
        }
        popOp(ctx, ctx_code(ctx));
    case OP_EVAL_ARGS:
        ctx_args(ctx) = cons(ctx, ctx_ret(ctx), ctx_args(ctx));
        if (is_pair(ctx_code(ctx))) {
            pushOp(ctx, OP_EVAL_ARGS, ctx_args(ctx), cdr(ctx_code(ctx)));
            ctx_code(ctx) = car(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
            gotoOp(ctx, OP_EVAL);
        }
        ctx_args(ctx) = reverse(ctx, ctx_args(ctx));
        ctx_code(ctx) = car(ctx_args(ctx));
        ctx_args(ctx) = cdr(ctx_args(ctx));
        gotoOp(ctx, OP_APPLY);
    case OP_APPLY:
        if (is_syntax(ctx_code(ctx))) {
            gotoOp(ctx, syntax_op(ctx_code(ctx)));
        } else if (is_iproc(ctx_code(ctx))) {
            gotoOp(ctx, iproc_op(ctx_code(ctx)));
        } else if (is_eproc(ctx_code(ctx))) {
            popOp(ctx, ctx_code(ctx)->eproc(ctx, ctx_args(ctx)));
        } else if (is_proc(ctx_code(ctx)) || is_closure(ctx_code(ctx))) {
            char *pname = "lambda";
            if (is_proc(ctx_code(ctx))) {
                pname = string(proc_name(ctx_code(ctx)));
                ctx_code(ctx) = proc_closure(ctx_code(ctx));
            }
            Cell *env = closure_env(ctx_code(ctx));
            Cell *fp = closure_args(ctx_code(ctx));
            Cell *ap = ctx_args(ctx);

            int min_args = length(fp);
            int max_args = is_list(fp) ? min_args : 0xFFFF;
            int n = length(ap);
            if (n < min_args) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "%s: unexpected number of arguments, expected at least %d but %d %s given",
                                pname, min_args, n, n>1?"were":"was"), NULL, NULL));
            }
            if (n > max_args) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "%s: unexpected number of arguments, expected at most %d but %d %s given",
                                pname, max_args, n, n>1?"were":"was"), NULL, NULL));
            }
            for (; is_pair(fp); fp = cdr(fp), ap = cdr(ap)) {
                mk_env(ctx, env, car(fp), car(ap));
            }
            if (!is_nil(fp) && !is_nil(ap)) {
                mk_env(ctx, env, fp, ap);
            }
            ctx_code(ctx) = closure_code(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
            ctx_env(ctx) = env;
            gotoOp(ctx, OP_EVAL_LIST);
        } else if (is_continues(ctx_code(ctx))) {
            ctx_continue(ctx) = cons(ctx, continues_car(ctx_code(ctx)), continues_cdr(ctx_code(ctx)));            
            popOp(ctx, ctx_args(ctx) != CELL_NIL ? car(ctx_args(ctx)) : CELL_NIL);
        }
        gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal procedure: "), ctx_code(ctx), NULL));
     case OP_EVAL_LIST:
        if (is_pair(ctx_code(ctx))) {
            if (!is_nil(cdr(ctx_code(ctx)))) {
                pushOp(ctx, OP_EVAL_LIST, CELL_NIL, cdr(ctx_code(ctx)));
            }
            ctx_code(ctx) = car(ctx_code(ctx));
            gotoOp(ctx, OP_EVAL);
        } else {
            gotoOp(ctx, OP_EVAL);
        }
     case OP_CALLCC:
        c = proc_closure(car(ctx_args(ctx)));
        if (length(closure_args(c)) != 1) {
            gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "invalid number of arguments to procedure at #<PROCEDURE call-with-current-continuation>"), NULL, NULL));
        }
        ctx_code(ctx) = c;
        ctx_args(ctx) = cons(ctx, ctx_continue(ctx), CELL_NIL);
        gotoOp(ctx, OP_APPLY);
    case OP_AND:
    case OP_OR:
        /*
        if (OP_AND == op) {
            d = CELL_TRUE;
            e = CELL_FALSE;
        } else {
            d = CELL_FALSE;
            e = CELL_TRUE;
        }
        for (c=ctx_code(ctx); is_pair(c); c=cdr(c)) {
            d = apply(ctx, OP_EVAL, CELL_NIL, car(c), ctx_env(ctx));
            if (d == e || (!is_false(d) && !is_false(e))) popOp(ctx, d);
            if (is_exception(d)) gotoErr(ctx, d);
        }
        popOp(ctx, d);
        */
        break;
     case OP_IF:
        pushOp(ctx, OP_IF1, CELL_NIL, cdr(ctx_code(ctx)));
        ctx_code(ctx) = car(ctx_code(ctx));
        gotoOp(ctx, OP_EVAL);
    case OP_IF1: 
        if (ctx_ret(ctx) == CELL_FALSE) {
            if (length(ctx_code(ctx)) == 2) {
                ctx_code(ctx) = cdr(ctx_code(ctx));
                ctx_args(ctx) = CELL_NIL;
                gotoOp(ctx, OP_EVAL_LIST);
            }
            popOp(ctx, CELL_UNDEF);
        }
        ctx_code(ctx) = cons(ctx, car(ctx_code(ctx)), CELL_NIL);
        ctx_args(ctx) = CELL_NIL;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_COND:
        c = ctx_code(ctx);
        for (Cell *ls=c; is_pair(ls); ls=cdr(ls)) {
            a = car(ls);
            if (!is_pair(a)) {
                if (is_pair(cdr(ls)))
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "cond: clause lack of test expression"), NULL, NULL));
            }
            d = car(a);
            if (is_symbol(d) && !strcmp(symbol(d), "else")) {
                if (is_pair(cdr(ls)))
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "cond: 'else' clause must be last"), NULL, NULL));
                if (!is_pair(cdr(a)))
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "cond: missing expressions in 'else' clause"), NULL, NULL));
            }
        }
        if (is_pair(c)) {
            e = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
            ctx_ret(ctx) = CELL_FALSE;
            gotoOpEx(ctx, OP_COND1, CELL_NIL, c, CELL_NIL, e);
        }
        popOp(ctx, CELL_UNDEF);
    case OP_COND1:
        a = ctx_ret(ctx);
        c = ctx_code(ctx);
        d = ctx_data(ctx);
        if (!is_false(a)) {
            b = car(d);
            if (is_symbol(b) && !strcmp(symbol(b), "=>")) {
                if (!is_pair(cdr(d)) || !is_nil(cddr(d))) return mk_exception(ctx, SyntaxError, mk_string(ctx, "cond: 'bad clause form with '=>'"), NULL, NULL);
                d = cadr(d);
                if (is_proc(d)) {
                    ctx_ret(ctx) = d;
                    gotoOpEx(ctx, OP_COND2, CELL_NIL, a, CELL_NIL, ctx_env(ctx));
                }
                pushOp(ctx, OP_COND2, CELL_NIL, a);
                gotoOpEx(ctx, OP_EVAL, CELL_NIL, d, CELL_NIL, ctx_env(ctx));
            }
            gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, d, CELL_NIL, ctx_env(ctx));
        }
        if (is_pair(c)) {
            d = car(c);
            pushOpEx(ctx, OP_COND1, CELL_NIL, cdr(c), cdr(d), e);
            a = car(d);
            if (is_symbol(a) && !strcmp(symbol(a), "else")) {
                gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, cdr(d), CELL_NIL, e);
            }
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, a, CELL_NIL, e);
        }
        popOp(ctx, CELL_UNDEF);
    case OP_COND2:
        a = ctx_ret(ctx);
        c = ctx_code(ctx);
        if (!is_proc(a)) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "cond: not a procedure"), NULL, NULL));
        }
        gotoOpEx(ctx, OP_APPLY, cons(ctx, c, CELL_NIL), a, CELL_NIL, ctx_env(ctx));
    case OP_CASE:
        c = car(ctx_code(ctx));
        for (Cell *ls=cdr(ctx_code(ctx)); is_pair(ls); ls=cdr(ls)) {
            d = car(ls);
            if (!is_pair(d)) {
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "case: illegal format clause"), NULL, NULL));
            }
            e = car(d);
            if (is_symbol(e) && !strcmp(symbol(e), "else")) {
                if (is_pair(cdr(ls))) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "case: 'else' clause must be last"), NULL, NULL));
                }
                continue;
            }
            if (!is_list(e)) {
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "case: datum not a sequence"), NULL, NULL));
            }
            if (!is_pair(cdr(d))) {
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "case: missing expression after datum sequence"), NULL, NULL));
            }
        }
        d = cdr(ctx_code(ctx));
        if (is_pair(d)) {
            e = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
            pushOpEx(ctx, OP_CASE1, CELL_NIL, d, CELL_NIL, e);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, c, CELL_NIL, e);
        }
        popOp(ctx, CELL_UNDEF);
    case OP_CASE1:
        c = ctx_code(ctx);
        d = ctx_ret(ctx);
        for (Cell *ls=c; is_pair(ls); ls=cdr(ls)) {
            a = car(ls);
            b = car(a);
            if ((is_symbol(b) && !strcmp(symbol(b), "else")) ||
                 is_contains(b, d)) {
                gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, cdr(a), CELL_NIL, ctx_env(ctx));
            }
        }
        popOp(ctx, CELL_UNDEF);
    case OP_BEGIN:
        ctx_env(ctx) = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
        ctx_args(ctx) = CELL_NIL;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_DO:
        c = car(ctx_code(ctx));
        d = cdr(ctx_code(ctx));
        e = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
        if (is_pair(c)) {
            b = cons(ctx, CELL_NIL, CELL_NIL);
            for (Cell *ls=c; is_pair(ls); ls=cdr(ls)) {
                a = car(ls);
                if (!is_pair(a) || !is_symbol(car(a)) || !is_pair(cdr(a)) || is_contains(b, car(a))) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "do"), NULL, NULL));
                }
                list_add(ctx, b, car(a));
                mk_env(ctx, e, car(a), CELL_UNDEF);
            }
            if (!is_pair(d) || !is_pair(car(d))) {
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "do: missing test expression"), NULL, NULL)); 
            }
            pushOpEx(ctx, OP_DO1, CELL_NIL, cdr(c), cons(ctx, ctx_code(ctx), cons(ctx, cdr(b), e)), ctx_env(ctx));
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cadar(c), CELL_NIL, ctx_env(ctx));
        }
        if (!is_pair(d) || !is_pair(car(d))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "do: missing test expression"), NULL, NULL)); 
        }
        gotoOpEx(ctx, OP_DO2, CELL_NIL, ctx_code(ctx), CELL_NIL, e);
    case OP_DO1:
        c = ctx_code(ctx);
        ctx_args(ctx) = cons(ctx, ctx_ret(ctx), ctx_args(ctx));
        if (is_pair(c)) {
            pushOpEx(ctx, OP_DO1, ctx_args(ctx), cdr(ctx_code(ctx)), ctx_data(ctx), ctx_env(ctx));
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cadar(c), CELL_NIL, ctx_env(ctx));
        }
        ctx_args(ctx) = reverse(ctx, ctx_args(ctx));
        c = car(ctx_data(ctx));
        b = cadr(ctx_data(ctx));
        e = cddr(ctx_data(ctx));
        for (Cell *ls=ctx_args(ctx); is_pair(ls); ls=cdr(ls), b=cdr(b)) {
            set_env(ctx, e, car(b), car(ls));
        }
        b = cons(ctx, CELL_NIL, CELL_NIL);
        for (Cell *ls=car(c); is_pair(ls); ls=cdr(ls)) {
            a = car(ls);
            if (is_pair(cddr(a))) {
                list_add(ctx, b, cons(ctx, car(a), caddr(a)));
            }
        }
        gotoOpEx(ctx, OP_DO2, cdr(b), c, CELL_NIL, e);
    case OP_DO2:
        pushOp(ctx, OP_DO3, ctx_args(ctx), ctx_code(ctx));
        gotoOpEx(ctx, OP_EVAL, CELL_NIL, caadr(ctx_code(ctx)), CELL_NIL, ctx_env(ctx));
    case OP_DO3:
        c = ctx_ret(ctx);
        if (!is_false(c)) {
            c = cdadr(ctx_code(ctx));
            if (is_nil(c)) popOp(ctx, CELL_UNDEF);
            gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, c, CELL_NIL, ctx_env(ctx));
        }
        c = ctx_args(ctx);
        if (is_pair(c)) {
            pushOpEx(ctx, OP_DO4, CELL_NIL, cdr(c), cons(ctx, c, ctx_code(ctx)), ctx_env(ctx));
            pushOpEx(ctx, OP_EVAL, CELL_NIL, cdar(c), CELL_NIL, ctx_env(ctx));
        } else {
            pushOpEx(ctx, OP_DO4, CELL_NIL, CELL_NIL, cons(ctx, CELL_NIL, ctx_code(ctx)), ctx_env(ctx));
        }
        c = cddr(ctx_code(ctx));
        if (is_pair(c)) {
            gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, c, CELL_NIL, ctx_env(ctx));
        }
        popOp(ctx, CELL_UNDEF);
    case OP_DO4:
        c = ctx_code(ctx);
        d = ctx_data(ctx);
        if (!is_nil(car(d))) {
            ctx_args(ctx) = cons(ctx, ctx_ret(ctx), ctx_args(ctx));
        }
        if (is_pair(c)) {
            pushOpEx(ctx, OP_DO4, ctx_args(ctx), cdr(c), ctx_data(ctx), ctx_env(ctx));
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cdar(c), CELL_NIL, ctx_env(ctx));
        }
        ctx_args(ctx) = reverse(ctx, ctx_args(ctx));
        for (a=car(d), b=ctx_args(ctx); is_pair(a); a=cdr(a), b=cdr(b)) {
            set_env(ctx, ctx_env(ctx), caar(a), car(b));
        }
        gotoOpEx(ctx, OP_DO2, car(d), cdr(d), CELL_NIL, ctx_env(ctx));
    case OP_LET:
        ctx_args(ctx) = CELL_NIL;
        ctx_ret(ctx) = ctx_code(ctx);
        ctx_code(ctx) = is_symbol(car(ctx_code(ctx))) ? cadr(ctx_code(ctx)) : car(ctx_code(ctx));
        gotoOp(ctx, OP_LET1);
    case OP_LET1:
        ctx_args(ctx) = cons(ctx, ctx_ret(ctx), ctx_args(ctx));
        if (is_pair(ctx_code(ctx))) {
            if (!is_pair(c = car(ctx_code(ctx))) || !is_pair(cdr(c))) {
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "let: bad syntax of binding"), NULL, NULL));
            }
            if (!is_symbol(car(c))) {
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "let: bad syntax of binding"), NULL, NULL));
            }
            pushOp(ctx, OP_LET1, ctx_args(ctx), cdr(ctx_code(ctx)));
            ctx_code(ctx) = cadar(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
            gotoOp(ctx, OP_EVAL);
        } else {
            ctx_args(ctx) = reverse(ctx, ctx_args(ctx));
            ctx_code(ctx) = car(ctx_args(ctx));
            ctx_args(ctx) = cdr(ctx_args(ctx));
            gotoOp(ctx, OP_LET2);
        }
    case OP_LET2:
        ctx_env(ctx) = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
        c = is_symbol(car(ctx_code(ctx))) ? cadr(ctx_code(ctx)) : car(ctx_code(ctx));
        if (is_symbol(car(ctx_code(ctx)))) {
            d = cddr(ctx_code(ctx));
        } else {
            d = cdr(ctx_code(ctx));
        }
        for (Cell *a = ctx_args(ctx); !is_nil(a); c = cdr(c), a = cdr(a)) {
            mk_env(ctx, ctx_env(ctx), caar(c), car(a));
        }
        if (is_symbol(car(ctx_code(ctx)))) {
            for (c = cadr(ctx_code(ctx)), ctx_args(ctx) = CELL_NIL; !is_nil(c); c = cdr(c)) {
                ctx_args(ctx) = cons(ctx, caar(c), ctx_args(ctx));
            }
            c = mk_closure(ctx, cons(ctx, reverse(ctx, ctx_args(ctx)), cddr(ctx_code(ctx))), ctx_env(ctx));
            ctx_env(ctx) = closure_env(c);
            mk_env(ctx, ctx_env(ctx), car(ctx_code(ctx)), c);
            ctx_code(ctx) = cddr(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
        } else {
            ctx_code(ctx) = cdr(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
        }
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LETSEQ:
        if (is_nil(car(ctx_code(ctx)))) {
            ctx_code(ctx) = cdr(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
            gotoOp(ctx, OP_EVAL_LIST);
        }
        for (c = car(ctx_code(ctx)); !is_nil(c); c = cdr(c)) {
            if (!is_pair(c) || !is_pair(car(c)) || !is_symbol(caar(c)))
                gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "let*: bad syntax of binding"), NULL, NULL));
        }
        c = car(ctx_code(ctx));
        pushOp(ctx, OP_LETSEQ1, cdr(ctx_code(ctx)), c);
        ctx_args(ctx) = CELL_NIL;
        ctx_code(ctx) = cadar(c);
        gotoOp(ctx, OP_EVAL);
    case OP_LETSEQ1:
        ctx_env(ctx) = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
        gotoOp(ctx, OP_LETSEQ2);
    case OP_LETSEQ2:
        mk_env(ctx, ctx_env(ctx), caar(ctx_code(ctx)), ctx_ret(ctx));
        ctx_code(ctx) = cdr(ctx_code(ctx));
        if (is_pair(ctx_code(ctx))) {
            pushOp(ctx, OP_LETSEQ2, ctx_args(ctx), ctx_code(ctx));
            ctx_code(ctx) = cadar(ctx_code(ctx));
            gotoOp(ctx, OP_EVAL);
        } else {
            ctx_code(ctx) = ctx_args(ctx);
            ctx_args(ctx) = CELL_NIL;
            gotoOp(ctx, OP_EVAL_LIST);
        }
    case OP_LETREC:
        c = car(ctx_code(ctx));
        d = cdr(ctx_code(ctx));
        e = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
        if (is_pair(c)) {
            b = cons(ctx, CELL_NIL, CELL_NIL);
            for (Cell *ls=c; is_pair(ls); ls=cdr(ls)) {
                a = car(ls);
                if (!is_pair(a)) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "letrec"), NULL, NULL));
                }
                if (!is_symbol(car(a)) || !is_pair(cdr(a)) || !is_nil(cddr(a)) || is_contains(b, car(a))) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "let-rec"), NULL, NULL));
                }
                list_add(ctx, b, car(a));
                mk_env(ctx, e, car(a), CELL_UNDEF);
            }
            b = cdr(b);
            pushOpEx(ctx, OP_LETREC1, cons(ctx, b, d), cdr(c), CELL_NIL, e);
            c = cadar(c);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, c, CELL_NIL, e);
        }
        ctx_code(ctx) = d;
        ctx_env(ctx) = e;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LETREC1:
        c = ctx_code(ctx);
        ctx_data(ctx) = cons(ctx, ctx_ret(ctx), ctx_data(ctx));
        if (is_pair(c)) {
            pushOpEx(ctx, OP_LETREC1, ctx_args(ctx), cdr(c), ctx_data(ctx), ctx_env(ctx));
            c = cadar(c);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, c, CELL_NIL, ctx_env(ctx));
        }
        a = car(ctx_args(ctx));
        b = reverse(ctx, ctx_data(ctx));
        d = cdr(ctx_args(ctx));
        e = ctx_env(ctx);
        for (Cell *ls=a; is_pair(ls); ls=cdr(ls), b=cdr(b)) {
            set_env(ctx, e, car(ls), car(b));
        }
        ctx_code(ctx) = d;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LET_SYNTAX:
    case OP_LETREC_SYNTAX:
        c = car(ctx_code(ctx));
        d = cdr(ctx_code(ctx));
        e = cons(ctx, mk_long(ctx, op), cdr(ctx_env(ctx)));
        if (is_pair(c)) {
            char *opn = (op == OP_LET_SYNTAX) ? "let-syntax" : "letrec-syntax";
            b = cons(ctx, CELL_NIL, CELL_NIL);
            for (Cell *ls=c; is_pair(ls); ls=cdr(ls)) {
                a = car(ls);
                if (!is_pair(a)) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, opn), NULL, NULL));
                }
                if (!is_symbol(car(a)) || !is_pair(cdr(a)) || !is_nil(cddr(a)) || is_contains(b, car(a))) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, opn), NULL, NULL));
                }
                list_add(ctx, b, car(a));
                if (op ==  OP_LETREC_SYNTAX)
                mk_env(ctx, e, car(a), CELL_UNDEF);
            }
            if (op == OP_LET_SYNTAX) {
                d = cons(ctx, d, CELL_NIL);
            }
            pushOpEx(ctx, OP_LET_SYNTAX1, cdr(b), cdr(c), d, e);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cadar(c), CELL_NIL, e);
        }
        ctx_args(ctx) = CELL_NIL;
        ctx_code(ctx) = d;
        ctx_env(ctx) = e;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LET_SYNTAX1:
        c = ctx_ret(ctx);
        if (!is_proc(c)) {
            op = number_long(car(ctx_env(ctx)));
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, (op == OP_LET_SYNTAX) ? "let-syntax" : "letrc-syntax"), NULL, NULL));
        }
        pushOpEx(ctx, OP_LET_SYNTAX2, ctx_args(ctx), ctx_code(ctx), ctx_data(ctx), ctx_env(ctx));
        gotoOpEx(ctx, OP_APPLY, CELL_NIL, c, CELL_NIL, ctx_env(ctx));
    case OP_LET_SYNTAX2:
        a = ctx_ret(ctx);
        b = ctx_args(ctx);
        c = ctx_code(ctx);
        d = ctx_data(ctx);
        e = ctx_env(ctx);
        op = number_long(car(e));
        if (!is_pair(a) || !is_pair(car(a)) || !is_pair(cdr(a))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, (op == OP_LET_SYNTAX) ? "let-syntax" : "letrc-syntax"), NULL, NULL));
        }
        if (op == OP_LETREC_SYNTAX) {
            for (Cell *ls=b; is_pair(ls); ls=cdr(ls)) {
                set_env(ctx, e, car(ls), mk_macro(ctx, cdr(a), car(a)));
            }
        } else {
            list_add(ctx, d, mk_macro(ctx, cdr(a), car(a)));
        }
        if (is_pair(c)) {
            pushOpEx(ctx, OP_LET_SYNTAX1, b, cdr(c), d, e);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cadar(c), CELL_NIL, e);
        }
        if (op == OP_LET_SYNTAX) {
            c = cdr(d);
            d = car(d);
            for (Cell *ls=b; is_pair(ls); ls=cdr(ls), c=cdr(c)) {
                mk_env(ctx, e, car(ls), car(c));
            }
        }
        ctx_code(ctx) = d;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_QUOTE:
        popOp(ctx, car(ctx_code(ctx)));
    case OP_QUASIQUOTE:
        ctx_code(ctx) = cons(ctx, mk_long(ctx, 1), car(ctx_code(ctx)));
        gotoOp(ctx, OP_QUASIQUOTE1);
    case OP_QUASIQUOTE1:
        c = cdr(ctx_code(ctx));
        d = car(ctx_code(ctx));
        if (is_pair(c)) {
            long lv = number_long(d);
            e = car(c);
            if (e == ctx_quasiquote(ctx)) {
                ++lv;
            } else if (e == ctx_unquote(ctx) || e == ctx_unquote_splicing(ctx)) {
                --lv;
                if (lv == 0) {
                    pushOp(ctx, OP_QUASIQUOTE2, CELL_NIL, e);
                    ctx_args(ctx) = CELL_NIL;
                    ctx_code(ctx) = cadr(c);
                    gotoOp(ctx, OP_EVAL);
                }
            }
            if (lv != number_long(d)) {
                d = mk_long(ctx, lv);
            }
            pushOp(ctx, OP_QUASIQUOTE3, CELL_NIL, cons(ctx, d, cdr(c)));
            ctx_code(ctx) = cons(ctx, d, car(c));
            gotoOp(ctx, OP_QUASIQUOTE1);
        }
        popOp(ctx, c);
    case OP_QUASIQUOTE2:
        c = ctx_code(ctx);
        d = ctx_ret(ctx);
        if (c == ctx_unquote(ctx)) {
            popOp(ctx, d);
        } else if (!is_list(d)) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "unquote-splicing: not a list"), NULL, NULL));
        }
        popOp(ctx, cons(ctx, CELL_SPLICING, d));
    case OP_QUASIQUOTE3:
        c = cdr(ctx_code(ctx));
        d = car(ctx_code(ctx));
        e = ctx_ret(ctx);
        if (is_pair(e) && car(e) == CELL_SPLICING) {
            for (Cell *ls=cdr(e); is_pair(ls); ls=cdr(ls)) {
                ctx_args(ctx) = cons(ctx, car(ls), ctx_args(ctx));
            }
        } else {
            ctx_args(ctx) = cons(ctx, e, ctx_args(ctx));
        }
        if (!is_pair(c)) {
            if (!is_nil(c)) {
                ctx_args(ctx) = cons(ctx, c, ctx_args(ctx));
            }
            popOp(ctx, reverse(ctx, ctx_args(ctx)));
        }
        pushOp(ctx, OP_QUASIQUOTE3, ctx_args(ctx), cons(ctx, d, cdr(c)));
        ctx_args(ctx) = CELL_NIL;
        ctx_code(ctx) = cons(ctx, d, car(c));
        gotoOp(ctx, OP_QUASIQUOTE1);
    case OP_UNQUOTE:
        gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "unquote: not in quasiquote"), NULL, NULL));
    case OP_UNQUOTE_SPLICING:
        gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "unquote-splicing: not in quasiquote"), NULL, NULL));

/************* core proc **************/
    case OP_MAP: {
        c = car(ctx_args(ctx));
        d = cdr(ctx_args(ctx));
        int min_args, max_args;
        if (is_proc(c)) {
            min_args = length(closure_args(proc_closure(c)));
            max_args = is_list(closure_args(proc_closure(c))) ? min_args : 0xFFFF;
        } else if (is_iproc(c)) {
            OpCode  *opc = g_opcodes + iproc_op(c);
            min_args = opc->min_args;
            max_args = opc->max_args;
        } else {
            gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "map: upsupport c function"), NULL, NULL));
        }
        int n = length(d);
        if (n < min_args) {
            gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "%s: unexpected number of arguments, expected at least %d but %d %s given",
                            proc_name(c), min_args, n, n>1?"were":"was"), NULL, NULL));
        }
        if (n > max_args) {
            gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "%s: unexpected number of arguments, expected at most %d but %d %s given",
                            proc_name(c), max_args, n, n>1?"were":"was"), NULL, NULL));
        }
        if (is_pair(d)) {
            n = length(car(d));
            a = cons(ctx, caar(d), CELL_NIL);
            if (n > 1) {
                b = cons(ctx, cdar(d), CELL_NIL);
            } else {
                b = CELL_NIL;
            }
            if (is_pair(cdr(d))) {
                for (Cell *ls=cdr(d); is_pair(ls); ls=cdr(ls)) {
                    e = car(ls);
                    if (n != length(e)) {
                        gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "map: all lists must have same size"), NULL, NULL));
                    }
                    list_add(ctx, a, car(e));
                    if (n > 1) {
                        list_add(ctx, b, cdr(e));
                    }
                }
            }
            e = cons(ctx, CELL_NIL, cdr(ctx_env(ctx)));
            pushOpEx(ctx, OP_MAP1, CELL_NIL, b, c, e);
            gotoOpEx(ctx, OP_APPLY, a, c, CELL_NIL, e);
        }
        popOp(ctx, CELL_NIL);
    }
    case OP_MAP1:
        ctx_args(ctx) = cons(ctx, ctx_ret(ctx), ctx_args(ctx));
        c = ctx_code(ctx);
        if (is_pair(c)) {
            int n = length(car(c));
            d = cons(ctx, caar(c), CELL_NIL);
            if (n > 1) {
                e = cons(ctx, cdar(c), CELL_NIL);
            } else {
                e = CELL_NIL;
            }
            for (Cell *ls=cdr(c); is_pair(ls); ls=cdr(ls)) {
                list_add(ctx, d, caar(ls));
                if (n > 1) list_add(ctx, e, cdar(ls));
            }
            pushOpEx(ctx, OP_MAP1, ctx_args(ctx), e, ctx_data(ctx), ctx_env(ctx));
            gotoOpEx(ctx, OP_APPLY, d, ctx_data(ctx), CELL_NIL, ctx_env(ctx));
        }
        popOp(ctx, reverse(ctx, ctx_args(ctx)));
    case OP_LOAD:
        c = push_load_file(ctx, string(car(ctx_args(ctx))));
        if (is_exception(c))
            gotoErr(ctx, c);
        gotoOp(ctx, OP_REPL_LOOP);
    case OP_DISPLAY: {
        int len = length(ctx_args(ctx));
        if (len == 1) {
            print_cell_readable(ctx, ctx_outport(ctx), car(ctx_args(ctx)));
        } else if (len == 2) {
            print_cell_readable(ctx, cadr(ctx_args(ctx)), car(ctx_args(ctx)));
        }
        popOp(ctx, CELL_UNDEF);
    }
    case OP_NEWLINE:
        if (is_pair(ctx_args(ctx))) c = car(ctx_args(ctx));
        else c = ctx_outport(ctx);
        write_char(ctx, c, '\n');
        popOp(ctx, CELL_UNDEF);
    case OP_BOOLEAN_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_boolean(c) ? CELL_TRUE : CELL_FALSE);
    case OP_SYMBOL_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_symbol(c) ? CELL_TRUE : CELL_FALSE);
    case OP_PAIR_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_pair(c) ? CELL_TRUE : CELL_FALSE);
    case OP_PROCEDURE_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_proc(c) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_char(c) ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_string(c) ? CELL_TRUE : CELL_FALSE);
    case OP_NOT:
        c = car(ctx_args(ctx));
        popOp(ctx, is_false(c) ? CELL_TRUE : CELL_FALSE);
    case OP_NULL_P:
        c = is_nil(car(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE;
        popOp(ctx, c);
    case OP_EQ_P:
        c = eq(car(ctx_args(ctx)), cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE;
        popOp(ctx, c);
    case OP_EQV_P:
        c = eqv(car(ctx_args(ctx)), cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE;
        popOp(ctx, c);
    case OP_EQUAL_P:
        c = equal(car(ctx_args(ctx)), cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE;
        popOp(ctx, c);
    case OP_LIST:
        popOp(ctx, ctx_args(ctx));
    case OP_LIST_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_list(c) ? CELL_TRUE : CELL_FALSE);
    case OP_CONS:
        c = ctx_args(ctx);
        popOp(ctx, cons(ctx, car(c), cadr(c)));
    case OP_CAR:
        popOp(ctx, caar(ctx_args(ctx)));
    case OP_CDR:
        popOp(ctx, cdar(ctx_args(ctx)));
    case OP_SET_CAR:
        rplaca(car(ctx_args(ctx)), cadr(ctx_args(ctx)));
        popOp(ctx, CELL_UNDEF);
    case OP_SET_CDR:
        rplacd(car(ctx_args(ctx)), cadr(ctx_args(ctx)));
        popOp(ctx, CELL_UNDEF);
    case OP_LENGTH:
        popOp(ctx, mk_long(ctx, length(car(ctx_args(ctx)))));
    case OP_APPEND:
        if (is_nil(ctx_args(ctx)))
            popOp(ctx, CELL_NIL);
        c = cons(ctx, CELL_NIL, CELL_NIL);
        d = ctx_args(ctx);
        uint i=0, len = length(d);
        for (; is_pair(d); d=cdr(d), ++i) {
            if(i != len-1 && !is_list(car(d))) {
                char buf[STR_BUF_SIZE] = "";
                snprintf(buf, STR_BUF_SIZE, "append: unmatched type of argument %d, must be list", i + 1);
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, buf), NULL, NULL));
            }
            list_extend(ctx, c, car(d));
        }
        popOp(ctx, cdr(c));
    case OP_REVERSE:
        popOp(ctx, reverse(ctx, car(ctx_args(ctx))));
    case OP_LIST_TAIL:
    case OP_LIST_REF: {
        uint idx = number_long(cadr(ctx_args(ctx)));
        uint len = length(car(ctx_args(ctx)));
        c = car(ctx_args(ctx));
        if (op == OP_LIST_TAIL) {
            if (idx > len) {
                gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of bounds"), NULL, NULL));
            }
            for (uint i=1; i<=idx; ++i)
                c = cdr(c);
        } else {
            if (idx >= len) {
                gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of bounds"), NULL, NULL));
            }
            for (uint i=0; i<idx; ++i) {
                c = cdr(c);
            }
            c = car(c);
        }
        popOp(ctx, c);
    }
    case OP_MEMQ:
    case OP_MEMV:
    case OP_MEMBER:
    case OP_ASSQ:
    case OP_ASSV:
    case OP_ASSOC: {
        bool (*func)(Cell*,Cell*);
        switch (op) {
        case OP_MEMQ:
        case OP_ASSQ:
            func = eq;
            break;
        case OP_MEMV:
        case OP_ASSV:
            func = eqv;
            break;
        case OP_MEMBER:
        case OP_ASSOC:
            func = equal;
            break;
        }
        c = car(ctx_args(ctx));
        d = cadr(ctx_args(ctx));

        switch (op) {
        case OP_MEMQ:
        case OP_MEMV:
        case OP_MEMBER:
            for (; is_pair(d); d=cdr(d)) {
                if (func(c, car(d)))
                    break;
            }
            popOp(ctx, is_nil(d) ? CELL_FALSE : d);
        case OP_ASSQ:
        case OP_ASSV:
        case OP_ASSOC:
            for (; is_pair(d); d=cdr(d)) {
                if (!is_pair(car(d))) {
                    gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "non-pair in alist"), cadr(ctx_args(ctx)), NULL));
                }
                if (func(c, caar(d))) {
                    popOp(ctx, car(d));
                }
            }
            popOp(ctx, CELL_FALSE);
        }
        break;
    }
    case OP_VECTOR: {
        uint len = length(ctx_args(ctx)), i = 0;
        c = mk_vector(ctx, len, CELL_UNDEF);
        for (Cell *ls=ctx_args(ctx); i<len && is_pair(ls); i++, ls=cdr(ls)) {
            vector_data(c)[i] = car(ls);
        }
        cell_type(c) |= M_IMMUTABLE;
        popOp(ctx, c);
    }
    case OP_VECTOR_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_vector(c) ? CELL_TRUE : CELL_FALSE);
    case OP_VECTOR_REF:
        c = car(ctx_args(ctx));
        d = cadr(ctx_args(ctx));
        if (number_long(d) >= vector_length(c)) {
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of bounds:"), d, NULL));
        }
        popOp(ctx, vector_data(c)[number_long(d)]);
    case OP_VECTOR_SET:
        c = car(ctx_args(ctx));
        d = cadr(ctx_args(ctx));
        e = caddr(ctx_args(ctx));
        if (number_long(d) >= vector_length(c)) {
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of bounds:"), d, NULL));
        }
        vector_data(c)[number_long(d)] = e;
        popOp(ctx, CELL_UNDEF);
    case OP_VECTOR_FILL:
        c = car(ctx_args(ctx));
        d = cdar(ctx_args(ctx));
        for (uint i=0, len=length(ctx_args(ctx)); i<len; i++)
            vector_data(c)[i] = d;
        popOp(ctx, CELL_UNDEF);
    case OP_VECTOR_LENGTH:
        c = car(ctx_args(ctx));
        popOp(ctx, mk_long(ctx, vector_length(c)));
    case OP_VECTOR_TO_LIST:
        c = car(ctx_args(ctx));
        d = e = cons(ctx, CELL_NIL, CELL_NIL);
        for (uint i=0, len=vector_length(c); i<len; ++i) {
            e = rplacd(e, cons(ctx, vector_data(c)[i], CELL_NIL));
        }
        popOp(ctx, cdr(d));
    case OP_MAKE_VECTOR:
        c = car(ctx_args(ctx));
        if (length(ctx_args(ctx)) == 2)
            d = cadr(ctx_args(ctx));
        else
            d = CELL_UNDEF;
        popOp(ctx, mk_vector(ctx, number_long(c), d));
    case OP_NUMBER_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_number(c) ? CELL_TRUE : CELL_FALSE);
    case OP_PORT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_port(c) ? CELL_TRUE : CELL_FALSE);

/************* numeric ************/
    case OP_ODD_P:
        c = car(ctx_args(ctx));
        popOp(ctx, number_long(c) % 2 ? CELL_TRUE : CELL_FALSE);
    case OP_EVEN_P:
        c = car(ctx_args(ctx));
        popOp(ctx, number_long(c) % 2 ? CELL_FALSE : CELL_TRUE);
    case OP_LP:
    case OP_GP:
    case OP_LEP:
    case OP_GEP:
        c = car(ctx_args(ctx));
        d = cdr(ctx_args(ctx));
        for (double e; is_pair(d); d=cdr(d)) {
            e = real_compare(c, car(d));
            switch (op) {
            case OP_LP:
                if (e >= 0) popOp(ctx, CELL_FALSE);
                break;
            case OP_GP:
                if (e <= 0) popOp(ctx, CELL_FALSE);
                break;
            case OP_LEP:
                if (e > 0) popOp(ctx, CELL_FALSE);
                break;
            case OP_GEP:
                if (e < 0) popOp(ctx, CELL_FALSE);
            }
            c = car(d);
        }
        popOp(ctx, CELL_TRUE);
    case OP_EP:
        c = ctx_args(ctx);
        popOp(ctx, num_equal(car(c), cadr(c)) ? CELL_TRUE : CELL_FALSE);
    case OP_ADD:
    case OP_SUB:
    case OP_MULTI:
    case OP_DIV:
        e = ctx_args(ctx);
        if (length(e) <= 1) {
            if (op == OP_ADD || op == OP_SUB)
                c = mk_long(ctx, 0);
            else
                c = mk_long(ctx, 1);
        } else {
            c = car(e);
            e = cdr(e);
        }
        for (Cell *i = NULL; is_pair(e); e = cdr(e)) {
            i = car(e);
            if (!is_number(i)) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "type of arguments error"), NULL, NULL));
            }
            c = num_calcu(ctx, op, c, i);
        }
        popOp(ctx, c);
    default:
        break;
    }
    return CELL_EOF;
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

static Cell *isc_init(FILE *in, char *name) {
    Segment *seg;
    Cell dummy_ctx, *ctx = NULL;

    seg = cell_mk_segment(SEG_INIT_MEM_SIZE, 0);
    if (!seg) {
        IError("no memory");
        return NULL;
    }
    dummy_ctx.t = CONTEXT;
    ctx_segments(&dummy_ctx) = seg;
    ctx = context_new(&dummy_ctx);
    ctx_segments(ctx) = seg;

    init_readers();
    ctx_inport(ctx) = mk_port(ctx, in, name, PORT_FILE | PORT_INPUT);
    ctx_outport(ctx) = mk_port(ctx, stdout, NULL, PORT_FILE | PORT_OUTPUT);
    ctx_lambda(ctx) = internal(ctx, "lambda");
    ctx_quote(ctx) = internal(ctx, "quote");
    ctx_unquote(ctx) = internal(ctx, "unquote");
    ctx_quasiquote(ctx) = internal(ctx, "quasiquote");
    ctx_unquote_splicing(ctx) = internal(ctx, "unquote-splicing");
 
    ctx_load_file(ctx, 0) = ctx_inport(ctx);
    ctx_global_env(ctx) = cons(ctx, internal(ctx, "*global-envir*"), CELL_NIL);
    ctx_env(ctx) = ctx_global_env(ctx);
        
    Cell *c = CELL_NIL;
    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) c = mk_syntax(ctx, i);
            else c = mk_iproc(ctx, i);
            mk_env(ctx, ctx_global_env(ctx), internal(ctx, g_opcodes[i].name), c);
        }
    }
    mk_env(ctx, ctx_global_env(ctx), internal(ctx, "call/cc"),
        cdr(find_env(internal(ctx, g_opcodes[OP_CALLCC].name), cdr(ctx_global_env(ctx)))));
    return ctx;
}

static struct {
    bool (*func)(Cell*);
    char *kind;
} g_arg_inspector[] = {
    {is_any,        0},
    {is_char,       "character"},
    {is_number,     "number"},
    {is_real,       "real"},
    {is_integer,    "integer"},
    {is_natural,    "nonnegative integer"},
    {is_string,     "string"},
    {is_symbol,     "symbol"},
    {is_pair,       "pair"},
    {is_list,       "list"},
    {is_vector,     "vector"},
    {is_procs,       "procedure"},    
    {is_envir,      "environment"},
    {is_continues,  "continuation"},
    {is_port,       "port"},
    {is_inport,     "input port"},
    {is_outport,    "output port"},
};

static Cell *arg_type_check(Cell *ctx) {
    char buf[STR_BUF_SIZE] = "";
    OpCode  *opc = g_opcodes + ctx_op(ctx);
    int n;
    if (opc->t == IPROC) {
        n = length(ctx_args(ctx));
        if (n < opc->min_args) {
            snprintf(buf, STR_BUF_SIZE, "%s: unexpected number of arguments, expected at least %d but %d %s given",
                    opc->name,
                    opc->min_args,
                    n,
                    n>1?"were":"was");
            goto Err;
        }
        if (n > opc->max_args) {
            snprintf(buf, STR_BUF_SIZE, "%s: unexpected number of arguments, expected at most %d but %d %s given",
                    opc->name,
                    opc->max_args,
                    n,
                    n>1?"were":"was");
            goto Err;
        }
        if (opc->arg_types != 0) {
            int index = 0;
            const char *arg_types = opc->arg_types;
            Cell *args = ctx_args(ctx);
            do {
                Cell *arg = car(args);
                if (!g_arg_inspector[arg_types[0]-1].func(arg)) break;
                if (arg_types[1] != 0) ++arg_types;
                args = cdr(args);
                ++index;
            } while (index < n);
            if (index < n) {
                snprintf(buf, STR_BUF_SIZE, "%s: unmatched type of argument %d, must be %s",
                        opc->name,
                        index + 1,
                        g_arg_inspector[arg_types[0]-1].kind);
                goto Err;
            }
        }
    } else if (opc->t == SYNTAX) {
        n = length(ctx_code(ctx));
        if (n < opc->min_args || n > opc->max_args) {
            snprintf(buf, STR_BUF_SIZE, "%s: bad syntax", opc->name);
            goto Err;
        }
    }
    return CELL_TRUE;
Err:
    return mk_exception(ctx, SyntaxError, mk_string(ctx, buf), NULL, NULL);
}

static void isc_finalize(Cell *ctx) {

}

void isc_helper() {
    printf("iScheme v0.1 by yuanjq\n");
    printf("Options and arguments:\n");
    printf("  -\t\t program read from stdin\n");
    printf("  file\t\t program read from file\n");
    printf("  -h(--help)\t print this help message and exit\n");
    printf("  -v(--version)\t iScheme's version\n");
}

int main(int argc, char *argv[]) {
    FILE *in = NULL;
    char *name = NULL;
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
        } else if (!access(argv[1], F_OK | R_OK)) {
            in = fopen(argv[1], "r");
            name = argv[1];
            if (!in) {
                IError("cant't open file '%s'", name);
                return -1;
            }
        } else {
            isc_helper();
            return 0;
        }
    }

    if (in == stdin) {
        printf("iScheme v0.1 by yuanjq\n");
    }

    Cell *ctx;
    ctx = isc_init(in, name);
    if (!ctx) {
        return -1;
    }
    isc_repl(ctx);
    isc_finalize(ctx);
    return 0;
}

