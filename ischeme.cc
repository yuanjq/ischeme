#include <strings.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <fcntl.h>
#include <unistd.h>
#include "ischeme.h"
#include "cell.h"

Cell g_true;
Cell g_false;
Cell g_nil;
Cell g_eof;
Cell g_undef;
Cell g_error;
Cell g_ellipsis;
Cell g_splicing;

#define gotoOp(sc,o)                    ({ctx_op(sc)=o; goto Loop;})
#define gotoOpEx(sc,o,a,c,d,e)          ({ctx_op(sc)=o; ctx_args(sc)=a; \
                                          ctx_code(sc)=c; ctx_data(sc)=d; \
                                          ctx_env(sc)=e; goto Loop;})
#define popOp(sc,r)                     ({pop_op(sc, r); goto Loop;})
#define pushOp(sc,o,a,c)                push_op(sc, o, a, c, CELL_NIL, ctx_env(sc))
#define pushOpEx(sc,o,a,c,d,e)          push_op(sc, o, a, c, d, e)
#define gotoErr(sc, e)                  ({ctx_ret(sc) = e; gotoOp(sc, OP_ERROR);})

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

/****************** function ****************/
static Cell *arg_type_check(Cell*);

Cell *cons(Cell *ctx, Cell *a, Cell *d) {
    Cell *c = pair_new(ctx);
    pair_car(c) = a;
    pair_cdr(c) = d;
    return c;
}

bool is_list(Cell *c) {
    for (; is_pair(c); c = cdr(c));
    if (c == CELL_NIL) return TRUE;
    return FALSE;
}

bool is_contains(Cell *ls, Cell *c) {
    for (; is_pair(ls); ls=cdr(ls)) {
        if (equal(car(ls), c))
            return TRUE;
    }
    return FALSE;
}

uint length(Cell *list) {
    uint len = 0;
    for (; is_pair(list); list = cdr(list)) ++len;
    return len;
}

Cell *mk_bool(bool b) {
    if (b)
        return CELL_TRUE;
    else
        return CELL_FALSE;
}

Cell *mk_char(Cell *ctx, char chr) {
    Cell *c = char_new(ctx);
    char_value(c) = chr;
    return c;
}

Cell *mk_string(Cell *ctx, const char *fmt, ...) {
    int size = 0;
    va_list ap;
    Cell *c = NULL;

    va_start(ap, fmt);
    size = vsnprintf(NULL, size, fmt, ap);
    va_end(ap);
    c = (Cell*)cell_alloc(ctx, cell_sizeof(str) + size + 1);
    cell_type(c) = STRING;
    cell_ptrtag(c) = POINTER_TAG;
    string_size(c) = size;
    string_data(c) = (char*)(&string_data(c) + 1);
    va_start(ap, fmt);
    vsnprintf(string_data(c), size + 1, fmt, ap);
    va_end(ap);
    return c;
}

Cell *mk_string(Cell *ctx, uint size, char fill) {
    Cell *c = (Cell*)cell_alloc(ctx, cell_sizeof(str) + size + 1);
    cell_type(c) = STRING;
    cell_ptrtag(c) = POINTER_TAG;
    string_size(c) = size;
    string_data(c) = (char*)(&string_data(c) + 1);
    memset(string_data(c), fill, size);
    return c;
}

Cell *mk_string(Cell *ctx, uint size) {
    return mk_string(ctx, size, ' ');
}

Cell *mk_symbol(Cell *ctx, const char *s) {
    Cell *c = mk_string(ctx, s);
    cell_type(c) = SYMBOL;
    return c;
}

Cell *mk_vector(Cell *ctx, uint len, Cell *fill) {
    Cell *c = (Cell*)cell_alloc(ctx, cell_sizeof(vect) + len * S(Cell*));
    cell_type(c) = VECTOR;
    vector_length(c) = len;
    for (uint i=0; i<len; i++) {
        vector_data(c)[i] = fill;
    }
    return c;
}

Cell *mk_env(Cell *ctx, bool immtb, Cell *outer) {
    Cell *c = env_new(ctx);
    env_immtb(c) = immtb;
    env_map(c) = map<char*,Cell*,ptrCmp>();
    env_outer(c) = outer;
    return c;
}

Cell *mk_closure(Cell *ctx, Cell *a, Cell *e) {
    gc_var1(c);
    c = closure_new(ctx);
    gc_preserve1(ctx, c);
    closure_args(c) = car(a);
    closure_code(c) = cdr(a);
    closure_env(c) = e;//mk_env(ctx, false, e);
    gc_release(ctx);
    return c;
}

Cell *mk_proc(Cell *ctx, Cell *name, Cell *clos) {
    Cell *c = proc_new(ctx);
    proc_name(c) = name;
    proc_closure(c) = clos;
    return c;
}

Cell *mk_iproc(Cell *ctx, int op) {
    Cell *c = iproc_new(ctx);
    iproc_op(c) = Op(op);
    return c;
}

Cell *mk_macro(Cell *ctx, Cell *mchs, Cell *env) {
    Cell *c = macro_new(ctx);
    macro_matchers(c) = mchs;
    macro_env(c) = env;
    return c;
}

Cell *mk_promise(Cell *ctx, Cell *expr) {
    Cell *c = promise_new(ctx);
    promise_result(c) = NULL;
    promise_expr(c) = expr;
    return c;
}

Cell *mk_port(Cell *ctx, FILE *f, const char *name, int t) {
    gc_var1(c);
    c = port_file_new(ctx);
    port_type(c) = t;
    port_file(c) = f;
    if (name) {
        gc_preserve1(ctx, c);
        port_file_name(c) = mk_string(ctx, name);
        gc_release(ctx);
    }
    port_file_bufidx(c) = -1;
    port_file_buflen(c) = 0;
    return c;
}

Cell *mk_port(Cell *ctx, char *start, char *end, int t) {
    Cell *c = port_string_new(ctx);
    port_type(c) = t;
    port_string_start(c) = start;
    port_string_pos(c) = start;
    port_string_end(c) = end;
    return c;
}

Cell *mk_syntax(Cell *ctx, int op) {
    Cell *c = syntax_new(ctx);
    syntax_op(c) = Op(op);
    return c;
}

Cell *mk_continue(Cell *ctx, Cell *ins, Cell *winds) {
    Cell *c = continue_new(ctx);
    continue_ins(c) = ins;
    continue_winds(c) = winds;
    return c;
}

Cell *mk_multival(Cell *ctx, Cell *c) {
    Cell *var = multivar_new(ctx);
    multivar_n(var) = length(c);
    multivar_var(var) = c;
    return var;
}

Cell *mk_exception_f(Cell *ctx, ErrorType t, Cell *msg, Cell *trg, Cell *src) {
    Cell *c = exception_new(ctx);
    exception_type(c) = t;
    exception_msg(c) = msg;
    exception_trg(c) = trg;
    exception_src(c) = src;
    return c;
}

/********************* stack frame ********************/
static void print_err(Cell *ctx, char *etype, char *ebody, ...) {
    va_list ap;
    va_start(ap, ebody);

    Cell *out = ctx_stderrport(ctx);
    fprintf(port_file(out), "%s", etype);
    fprintf(port_file(out), ": ");
    vfprintf(port_file(out), ebody, ap);
    va_end(ap);
    fprintf(port_file(out), "\n\t at %s:%d\n", string(port_file_name(out)), port_file_pos(out));
}

static inline bool pop_op(Cell *ctx, Cell *v) {
    Cell *insts = ctx_instructs(ctx);
    Cell *inst = car(insts);
    if (!is_instruct(inst)) {
        return FALSE;
    }
    ctx_ret(ctx) = v;
    ctx_op(ctx) = instruct_op(inst);
    ctx_args(ctx) = instruct_args(inst);
    ctx_code(ctx) = instruct_code(inst);
    ctx_data(ctx) = instruct_data(inst);
    ctx_env(ctx) = instruct_env(inst);
    ctx_instructs(ctx) = cdr(insts);
    return TRUE;
}

static void push_op(Cell *ctx, Op op, Cell *args, Cell *code, Cell *data, Cell *env) {
    gc_var1(c);
    c = instruct_new(ctx);
    gc_preserve1(ctx, c);
    instruct_op(c) = op;
    instruct_args(c) = args;
    instruct_code(c) = code;
    instruct_data(c) = data;
    instruct_env(c) = env;
    ctx_instructs(ctx) = cons(ctx, c, ctx_instructs(ctx));
    gc_release(ctx);
}

/***************** I/O handler ******************/
static inline void port_flush(Cell *p) {
    fflush(port_file(p));
}

static void port_close(Cell *ctx, Cell* p) {
    if(port_type(p) & PORT_FILE) {
        port_file_name(p) = NULL;
        fclose(port_file(p));
        port_file(p) = NULL;
    }
    port_type(p) = PORT_FREE;
}

static Cell *push_load_file(Cell *ctx, char *name) {
	FILE *fin = fopen(name, "r");
	if (!fin) {
        char buf[STR_BUF_SIZE];
        snprintf(buf, sizeof(buf), "can not open file \"%s\"", name);
        return mk_exception(ctx, IOError, mk_string(ctx, buf), NULL, NULL);
    }
    ctx_inport(ctx) = mk_port(ctx, fin, name, PORT_INPUT_FILE);
    ctx_inports_push(ctx, ctx_inport(ctx));
    return ctx_inport(ctx);
}

static void pop_load_file(Cell *ctx) {
    if (ctx_inports(ctx).size() > 1) {
		port_close(ctx, ctx_inport(ctx));
        ctx_inports_pop(ctx);
        ctx_inport(ctx) = ctx_inports_tail(ctx);
	}
}

Cell *write_char(Cell *ctx, Cell *out, int c) {
	if(port_type(out) & PORT_OUTPUT_FILE) {
        c = fputc(c, port_file(out));
        port_flush(out);
        if (ctx_transc_port(ctx) && (port_file(out) == stdout || port_file(out) == stderr)) {
            if (ctx_transc_inportp(ctx)) {
                ctx_transc_inportp(ctx) = false;
                fputc('\n', port_file(ctx_transc_port(ctx)));
            }
            fputc(c, port_file(ctx_transc_port(ctx)));
            port_flush(ctx_transc_port(ctx));
        }
		return CELL_UNDEF;
	} else if (port_type(out) & PORT_OUTPUT_STRING) {
		if(port_string_end(out) <= port_string_pos(out)) {
           return mk_exception(ctx, MemoryError, mk_string(ctx, "write char out of range"), NULL, NULL);
        }
        *port_string_pos(out)++ = c;
        return CELL_UNDEF;
	}
    return mk_exception(ctx, IOError, mk_string(ctx, "invalid out port"), NULL, NULL);
}

Cell *write_string(Cell *ctx, Cell *out, const char *s) {
	int len = strlen(s);
	if (port_type(out) & PORT_OUTPUT_FILE) {
        len = fwrite(s, 1, len, port_file(out));
        port_flush(out);
        if (ctx_transc_port(ctx) && (port_file(out) == stdout || port_file(out) == stderr)) {
            if (ctx_transc_inportp(ctx)) {
                ctx_transc_inportp(ctx) = false;
                fputc('\n', port_file(ctx_transc_port(ctx)));
            }
            fwrite(s, 1, len, port_file(ctx_transc_port(ctx)));
            port_flush(ctx_transc_port(ctx));
        }
		return CELL_UNDEF;
	} else if (port_type(out) & PORT_OUTPUT_STRING) {
	    if (len > port_string_end(out) - port_string_pos(out)) {
           return mk_exception(ctx, MemoryError, mk_string(ctx, "write string out of range"), NULL, NULL);
        }
        for (; len; len--) *port_string_pos(out)++ = *s++;
        return CELL_UNDEF;
	}
    return mk_exception(ctx, IOError, mk_string(ctx, "invalid out port"), NULL, NULL);
}

static int char_ready_p (Cell *port) {
    FILE *in = port_file(port);
    int flags = fcntl(fileno(in), F_GETFL), res;
    if (!(flags & O_NONBLOCK)) fcntl(fileno(in), F_SETFL, flags | O_NONBLOCK);
    res = getc(in);
    if (!(flags & O_NONBLOCK)) fcntl(fileno(in), F_SETFL, flags);
    if (res == EOF || ferror(in)) {
        clearerr(in);
        return false;
    }
    ungetc(res, in);
    return true;
}

static inline int get_char(Cell *ctx, Cell *in) {
    int c = EOF;
    if (!(port_type(in) & PORT_INPUT)) {
        return EOF;
    }
    if (port_type(in) & PORT_FILE) {
        if (is_interactive(ctx)) {
            c = fgetc(port_file(in));
            if (c == EOF) return EOF;
        } else if (++port_file_bufidx(in) < port_file_buflen(in)) {
            c = port_file_buf(in)[port_file_bufidx(in)];
        } else {
            char *s = fgets(port_file_buf(in), PORT_BUF_SIZE, port_file(in));
            if (!s) {
                return EOF;
            }
            c = s[0];
            port_file_buflen(in) = strlen(s);
            port_file_bufidx(in) = 0;
        }
        
        if (ctx_transc_port(ctx) && port_file(in) == stdin) {
            if (!ctx_transc_inportp(ctx) && c == '\n') {
                ctx_transc_inportp(ctx) = true;
                return c;
            }
            ctx_transc_idx(ctx) = port_file_bufidx(in);
            fputc(c, port_file(ctx_transc_port(ctx)));
            port_flush(ctx_transc_port(ctx));
        }
    } else if (port_type(in) & PORT_STRING) {
        if (port_string_pos(in) == port_string_end(in))
            c == EOF;
        else
            c = *port_string_pos(in)++;
    }
    return c;
}

static inline void unget_char(Cell *ctx, Cell *out, int c) {
    if (c == EOF || !(port_type(out) & PORT_INPUT)) return;
    if (is_interactive(ctx)) {
        ungetc(c, port_file(out));
    } else if (port_type(out) & PORT_FILE) {
        --port_file_bufidx(out);
    } else if (port_type(out) & PORT_STRING) {
        if (port_string_pos(out) != port_string_start(out)) --port_string_pos(out);
    }
}

static inline int peek_char(Cell *ctx, Cell *in) {
    int c = get_char(ctx, in);
    unget_char(ctx, in, c);
    return c;
}

static inline int skip_line(Cell *ctx, Cell *port) {
    int c = 0, n = 0;
    while ((c = get_char(ctx, port)) != EOF && c != '\n') ++n;
    if (c == '\n') {
        if (port_type(port) & PORT_INPUT_FILE)
            ++port_file_pos(port);
        return ++n;
    }
    return EOF;
}

static inline int skip_comment(Cell *ctx, Cell *port, int c) {
    int n = 0;
    if (c == EOF) return EOF;
    if (c == ';') {
        c = skip_line(ctx, port);
        if (c == EOF) return EOF;
        n += c;
    } else if (c == '#') {
        c = get_char(ctx, port);
        if (c == EOF) return EOF;
        if (c != '!')  {
            unget_char(ctx, port, c);
            unget_char(ctx, port, '#');
            return 0;
        }
        n += 2;
        c = skip_line(ctx, port);
        if (c == EOF) return EOF;
        n += c;
    }
    return n;
}

static inline int skip_space(Cell *ctx, Cell *port) {
    int c = 0, sum = 0, line = 0;
    do {
        c = get_char(ctx, port);
        ++sum;
        if (c == '\n') ++line;
        else if (c == ';' || c == '#') {
            int n = skip_comment(ctx, port, c);
            if (n == EOF) return EOF;
            else if (n == 0) return sum;
            sum += n;
            ++line;
            continue;
        }
    } while(isspace(c) || c == ';' || c == '#');
    if (port_type(port) & PORT_INPUT_FILE)
        port_file_pos(port) += line;
    if (c != EOF) {
        unget_char(ctx, port, c);
        return --sum;
    }
    return EOF;
}

static Cell *reverse(Cell *ctx, Cell *old) {
    gc_var1(ret);
    gc_preserve1(ctx, ret);
    ret = CELL_NIL;
    for (; is_pair(old); old = cdr(old))
        ret = cons(ctx, car(old), ret);
    gc_release(ctx);
    return ret;
}

static int get_token(Cell *ctx, Cell *port) {
    int c = skip_space(ctx, port);
    if (c == EOF) return TOK_EOF;
    switch (c = get_char(ctx, port)) {
    case EOF: return TOK_EOF;
    case '(': return TOK_LPAREN;
    case ')': return TOK_RPAREN;
    case '[': return TOK_LBRACKET;
    case ']': return TOK_RBRACKET;
    case '{': return TOK_LBRACE;
    case '}': return TOK_RBRACE;
    case '.': {
        if (skip_space(ctx, port) > 0) return TOK_DOT;
        int a, b, d;
        if ((a = get_char(ctx, port)) == '.') {
            if ((b = get_char(ctx, port)) == '.') {
                d = get_char(ctx, port);
                if (strchr(DELIMITERS, d)) {
                    unget_char(ctx, port, d);
                    return TOK_ELLIPSIS;
                } else {
                    unget_char(ctx, port, d);
                    unget_char(ctx, port, b);
                }
            } else {
                unget_char(ctx, port, b);
            }
        }

        unget_char(ctx, port, a);
        unget_char(ctx, port, c);
        return TOK_SYMBOL;
    }
    case '\'': return TOK_QUOTE;
    case '`': return TOK_QQUOTE;
    case '"': return TOK_DQUOTE;
    case ',':
        c = get_char(ctx, port);
        if (c == '@') return TOK_UNQUOTE_SPLICING;
        else unget_char(ctx, port, c);
        return TOK_UNQUOTE;
    case '#':
        c = get_char(ctx, port);
        if (c == '(') return TOK_VECTOR;
        else if (c == '!') {
            c = skip_line(ctx, port);
            if (c == EOF) return TOK_EOF;
            return get_token(ctx, port);
        } else if (strchr("tfeibodx\\", c)) {
            unget_char(ctx, port, c);
            unget_char(ctx, port, '#');
            return TOK_CONST;
        }
        return TOK_ERR;
    case ';':
        c = skip_line(ctx, port);
        if (c == EOF) return TOK_EOF;
        return get_token(ctx, port);
    default:
        unget_char(ctx, port, c);
        return TOK_SYMBOL;
    }
}

static Cell *read_cell(Cell *ctx, Cell *port) {
    int t = get_token(ctx, port);
    if (t == TOK_ERR) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
    } else if (t == TOK_EOF) {
        return CELL_EOF;
    }
    return g_readers[t](ctx, port, t);
}

static Cell *read_cell_by_token(Cell *ctx, Cell *port, int t) {
    if (t == TOK_ERR) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
    } else if (t == TOK_EOF) {
        return CELL_EOF;
    }
    return g_readers[t](ctx, port, t);
}

bool equal(Cell *a, Cell *b) {
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

static Cell *find_symbol(Cell *ctx, const char *s) {
    for (Cell *c = ctx_symbols(ctx); c; c = cdr(c)) {
        char *sym = symbol(car(c));
        if (sym && !strcmp(sym, s))
          return car(c);
    }
    return CELL_NIL;
}

static Cell* internal(Cell *ctx, const char *s) {
    gc_var1(c);
    if ((c = find_symbol(ctx, s)) != CELL_NIL) return c;
    gc_preserve1(ctx, c);
    c = mk_symbol(ctx, s);
    ctx_symbols(ctx) = cons(ctx, c, ctx_symbols(ctx));
    gc_release(ctx);
    return c;
}

Cell *assq(Cell *key, Cell *list) {
    Cell *c = CELL_NIL;
    if (!is_symbol(key)) return CELL_NIL;
    for (; is_pair(list); list = cdr(list)) {
        c = car(list);
        if (is_pair(c) && equal(car(c), key)) {
            return c;
        }
    }
    return CELL_NIL;
}

Cell *list_ref(Cell *ls, Cell *n) {
    long l = number_long(n);
    for (long i=0; is_pair(ls); ls=cdr(ls)) {
        if (i == l) return car(ls);
        ++i;
    }
    return CELL_NIL;
}

Cell *list_tail(Cell *ls, uint n) {
    for (uint i=0; is_pair(ls); ls=cdr(ls), ++i) {
        if (i == n) return ls;
    }
    return CELL_NIL;
}

void list_set(Cell *ls, Cell *n, Cell *v) {
    long l = number_long(n);
    for (long i=0; is_pair(ls); ls=cdr(ls)) {
        if (i == l) {
            rplaca(ls, v);
            return;
        }
        ++i;
    }
}

void list_add(Cell *ctx, Cell *ls, Cell *c) {
    if (!is_pair(ls)) return;
    for (; is_pair(ls); ls=cdr(ls)) {
        if (cdr(ls) == CELL_NIL) {
            rplacd(ls, cons(ctx, c, CELL_NIL));
            return;
        }
    }
}

void list_extend(Cell *ctx, Cell *ls, Cell *c) {
    if (!is_pair(ls)) return;
    for (; is_pair(ls); ls=cdr(ls)) {
        if (cdr(ls) == CELL_NIL) {
            if (is_pair(c)) {
                gc_var2(d, e);
                gc_preserve2(ctx, d, e);
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
                gc_release(ctx);
            } else {
                rplacd(ls, cons(ctx, c, CELL_NIL));
            }
            return;
        }
    }
}

Cell *list_pop(Cell *ls) {
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

void alist_update(Cell *ctx, Cell *ls1, Cell *ls2) {
    gc_var2(v1, v2);
    gc_preserve2(ctx, v1, v2);
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
            list_add(ctx, ls1, v1 = cons(ctx, caar(ls2),  v2 = cons(ctx, cdar(ls2), CELL_NIL)));
        }
    }
    gc_release(ctx);
}

void alist_append(Cell *ctx, Cell *ls, Cell *pair) {
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
        gc_var2(v1, v2);
        gc_preserve2(ctx, v1, v2);
        list_add(ctx, ls, v1 = cons(ctx, k, v2 = cons(ctx, v, CELL_NIL)));
        gc_release(ctx);
    }
}

Cell *find_env(Cell *env, Cell *k) {
    char *s = symbol(k);
    map<char*,Cell*,ptrCmp> *mp;
    map<char*,Cell*,ptrCmp>::iterator it;
    Cell *e = env;
    for (; is_env(e); e=env_outer(e)) {
        mp = &env_map(e);
        it = mp->find(s);
        if (it != mp->end()) {
            break;
        }
    }
    if (!is_nil(e)) {
        return (*it).second;
    }
    return CELL_NIL;
}

static void env_set(Cell *ctx, Cell *env, Cell *k, Cell *v) {
    char *s = symbol(k);
    map<char*,Cell*,ptrCmp> *mp;
    map<char*,Cell*,ptrCmp>::iterator it;
    Cell *e = env;
    for (; is_env(e); e=env_outer(e)) {
        mp = &env_map(e);
        it = mp->find(s);
        if (it != mp->end()) {
            break;
        }
    }
    if (is_nil(e)) {
        env_map(env).insert({s, cons(ctx, k, v)});
    } else {
        rplacd((*it).second, v);
    }
}

static void env_add(Cell *ctx, Cell *env, Cell *k, Cell *v) {
    char *s = symbol(k);
    map<char*,Cell*,ptrCmp> *mp = &env_map(env);
    map<char*,Cell*,ptrCmp>::iterator it = mp->find(s);
    if (it == mp->end()) {
        mp->insert({s, cons(ctx, k, v)});
    } else {
        rplacd((*it).second, v);
    }
}

static Cell *read_illegal(Cell *ctx, Cell *port, int c) {
    const char *str = "";
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

static inline int read_upto(Cell *ctx, Cell *port, char *upto, char **out, uint *psize) {
    int c;
    char *p = *out;
    uint size = *psize;

    for (;;) {
        if (p - *out < size) {
            if ((c = get_char(ctx, port)) < 0) {
                return EOF;
            }
            if (strchr(upto, c))
                break;
            *p++ = c;
        } else {
            char *tmp = (char*)cell_malloc(2*size);
            if (tmp) {
                memcpy(*out, p, p - *out);
                if (size != STR_BUF_SIZE) cell_free(p);
                size = 2 * size;
                *psize = size;
                p = tmp + (p - *out);
                *out = tmp;
            } else {
                if (size != STR_BUF_SIZE) cell_free(p);
                *out = NULL;
                return EOF;
            }
        }
    }
    unget_char(ctx, port, c);
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
    char buf[STR_BUF_SIZE] = {0};
    char *s = buf;
	if (is_nil(c)) {
        write_string(ctx, port, "()");
        return;
	} else if (is_true(c)) {
		s = CSTR("#t");
	} else if (is_false(c)) {
		s = CSTR("#f");
	} else if (is_undef(c)) {
        s = CSTR("#<UNDEF>");
    } else if (is_ellipsis(c)) {
        s = CSTR("...");
	} else if (is_eof(c)) {
		s = CSTR("#<EOF>");
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
	} else if (is_continue(c)) {
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
    		case ' ' : s = CSTR("#\\space"); break;
    		case '\n': s = CSTR("#\\newline"); break;
    		case '\r': s = CSTR("#\\return"); break;
    		case '\t': s = CSTR("#\\tab"); break;
    		default:
    			if(char_value(c) == 127) {
    				s = CSTR("#\\del"); break;
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
        const char *etype = NULL;
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
    } else if (is_promise(c)) {
        snprintf(s, STR_BUF_SIZE, "#<PROMISE:%p>", c);
    } else if (is_multivar(c)) {
        write_string(ctx, port, "#<MULTIVAR:");
        for (Cell *ls=multivar_var(c); is_pair(ls);) {
            write_cell(ctx, port, car(ls), readable, more, top);
            ls = cdr(ls);
            if (is_pair(ls)) {
                write_char(ctx, port, ' ');
            }
        }
        write_char(ctx, port, '>');
    } else if (is_env(c)) {
        snprintf(s, STR_BUF_SIZE, "#<ENVIRONMENT:%p>", c);
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

void print_cell(Cell *ctx, Cell *p, Cell *c) {
    write_cell(ctx, p, c, 0, 0, 1);
}

/**************** numeric operations *******************/
static Cell *read_real(Cell *ctx, char **ppstart, char *pend, Exactness exact, Radix radix) {
    gc_var1(num);
    char *p = *ppstart;
    int c = *p;
    char sign = 1;
    int buflen = 0;
    bool find_num = FALSE;
    bool find_dot = FALSE;
    bool find_slash = FALSE;
    char *buf = NULL;
    char *pstart = NULL;

    gc_preserve1(ctx, num);
    if (pend - p >= STR_BUF_SIZE) {
        goto Error;
    }
    buf = (char*)cell_malloc(STR_BUF_SIZE);
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

    pstart = p;
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
        } else if (c == '+' || c == '-' || c == 'i' || c == 'I' || c == '@') {
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
        num = num_itoe(ctx, num);
    else if (exact == INEXACT)
        num = num_etoi(ctx, num);

Error:
    gc_release(ctx);
    if (buf) {
        cell_free(buf);
    }
    return num;
}

static Cell *read_number(Cell *ctx, char *pstart, uint size, Exactness exact, Radix radix) {
    Cell *num;
    char *p = pstart;
    char *pend = p + size;
    gc_var2(real, imag);

    real = read_real(ctx, &p, pend, exact, radix);
    if (!real || p > pend) 
        return CELL_NIL;
    if (p == pend)
        return real;
    gc_preserve2(ctx, real, imag);
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
    } else if (c == '@') {
        ++p;
        Cell *r = real;
        Cell *thet = read_real(ctx, &p, pend, exact, radix);
        if (!thet || p != pend) {
            goto Error;
        }
        real = mk_double(ctx, number_double(num_etoi(ctx, r)) * cos(number_double(num_etoi(ctx, thet))));
        imag = mk_double(ctx, number_double(num_etoi(ctx, r)) * sin(number_double(num_etoi(ctx, thet))));
    }
    if (exact == NO_EXACTNESS &&
        (number_type(real) == NUMBER_DOUBLE || number_type(imag) == NUMBER_DOUBLE)) 
    {
        if (number_type(real) != NUMBER_DOUBLE)
            real = num_etoi(ctx, real);
        if (number_type(imag) != NUMBER_DOUBLE)
            imag = num_etoi(ctx, imag);
    }
    num = number_new(ctx);
    number_type(num) = NUMBER_COMPLEX;
    number_cx_rl(num) = real;
    number_cx_im(num) = imag;
    gc_release(ctx);
    return num;
    
Error:
    gc_release(ctx);
    return CELL_NIL;
}

static Cell *read_symbol(Cell *ctx, Cell *port, int _) {
    uint size = STR_BUF_SIZE;
    char buf[STR_BUF_SIZE] = "";
    char *p = buf;
    int total_len = read_upto(ctx, port, CSTR(DELIMITERS), &p, &size);

    if (total_len <= 0) {
        return CELL_EOF;
    }
    gc_var1(c);
    gc_preserve1(ctx, c);
    c = read_number(ctx, p, total_len, NO_EXACTNESS, NO_RADIX);
    if (c == CELL_NIL) {
        if ((c = find_symbol(ctx, p)) == CELL_NIL) {
            c = mk_symbol(ctx, p);
        }
    }
    if (size != STR_BUF_SIZE) cell_free(p);
    gc_release(ctx);
    return c;
}

static Cell *read_const_from_string(Cell *ctx, Cell *port, char *pst, uint size) {
    Cell *ret = NULL;
    Cell *real = NULL, *imag = NULL;
    Cell *num = NULL;
    Exactness exact = NO_EXACTNESS;
    Radix radix = NO_RADIX;
    int c;
    char *p = pst;
    char *pend = pst + size;

    while (p < pend && *p == '#') {
        if (p + 2 > pend && p[1] != '\\')
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
            if (exact != NO_EXACTNESS || radix != NO_RADIX)
                goto Error;
            p += 2;
            if (p == pend) {
                c = get_char(ctx, port);
                if (!strchr(CSTR(DELIMITERS), c)) {
                    goto Error;
                }
                int d = get_char(ctx, port);
                if (!strchr(CSTR(DELIMITERS), d)) {
                    goto Error;
                }
                unget_char(ctx, port, d);
                return mk_char(ctx, c);
            } else if (pend - p > 1) {
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
        size -= 2;
    }

    num = read_number(ctx, p, size, exact, radix);
    if (num != CELL_NIL) {
        return num;
    }

Error:
    return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
}

static Cell *read_const(Cell *ctx, Cell *port, int c) {
    Cell *ret = NULL;
    uint size = STR_BUF_SIZE;
    char buf[STR_BUF_SIZE] = "";
    char *p = buf;
    int total_len = read_upto(ctx, port, CSTR(DELIMITERS), &p, &size);
    Cell *num = NULL;

    if (total_len <= 0) {
        return mk_exception(ctx, SyntaxError, mk_string(ctx, "bad syntax"), NULL, NULL);
    }
    ret = read_const_from_string(ctx, port, p, total_len);
    if (size != STR_BUF_SIZE) cell_free(p);
    return ret;
}

static char *string_dup(char *str) {
    char *s = (char*)cell_malloc(strlen(str) + 1);
    if (s) {
        strcpy(s, str);
    }
    return s;
}

static Cell *read_string(Cell *ctx, Cell *port, int q) {
    char buf[STR_BUF_SIZE] = "";
    char *p = buf;
    Cell *ret = NULL;
    int buf_size = STR_BUF_SIZE;
    int idx = 0;
    int c;
    while ((c = get_char(ctx, port)) > 0 && c != '\"')
    {
        if (idx >= buf_size) {
            char *tmp;
            int tmp_size = buf_size * 5;
            tmp = (char*)cell_malloc(tmp_size);
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
            c = get_char(ctx, port);
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

static Cell *read_quote(Cell *ctx, Cell *port, int c) {
    gc_var2(c1, c2);
    c1 = read_cell(ctx, port);
    if (c1 == CELL_EOF || is_exception(c1))
        return c1;
    gc_preserve2(ctx, c1, c2);
    c1 = cons(ctx, ctx_quote(ctx), c2 = cons(ctx, c1, CELL_NIL));
    gc_release(ctx);
    return c1;
}

static Cell *read_quasiquote(Cell *ctx, Cell *port, int c) {
    gc_var2(c1, c2);
    c1 = read_cell(ctx, port);
    if (c1 == CELL_EOF || is_exception(c1))
        return c1;
    gc_preserve2(ctx, c1, c2);
    c1 = cons(ctx, ctx_quasiquote(ctx), c2 = cons(ctx, c1, CELL_NIL));
    gc_release(ctx);
    return c1;
}

static Cell *read_unquote(Cell *ctx, Cell *port, int c) {
    gc_var2(c1, c2);
    c1 = read_cell(ctx, port);
    if (c1 == CELL_EOF || is_exception(c1))
        return c1;
    gc_preserve2(ctx, c1, c2);
    c1 = cons(ctx, ctx_unquote(ctx), c2 = cons(ctx, c1, CELL_NIL));
    gc_release(ctx);
    return c1;
}

static Cell *read_unquote_splicing(Cell *ctx, Cell *port, int c) {
    gc_var2(c1, c2);
    c1 = read_cell(ctx, port);
    if (c1 == CELL_EOF || is_exception(c1))
        return c1;
    gc_preserve2(ctx, c1, c2);
    c1 = cons(ctx, ctx_unquote_splicing(ctx), c2 = cons(ctx, c1, CELL_NIL));
    gc_release(ctx);
    return c1;
}

static Cell *read_vector(Cell *ctx, Cell *port, int c) {
    uint len = 0;
    gc_var3(head, tail, cell);
    gc_preserve3(ctx, head, tail, cell);
    cell = CELL_NIL;
    head = tail = cons(ctx, CELL_NIL, CELL_NIL);
    for (;;) {
        c = get_token(ctx, port);
        if (c == TOK_EOF) {
            gc_release(ctx);
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unexpected eof"), NULL, NULL);
        }
        if (c == TOK_RPAREN) break;
        if (c == TOK_RPAREN || c == TOK_RBRACKET || c == TOK_RBRACE) {
            gc_release(ctx);
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched brackets"), NULL, NULL);
        }

        if (c == TOK_DOT) {
            gc_release(ctx);
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal used of dot"), NULL, NULL);
        } else if (c == TOK_ELLIPSIS) {
            cell = CELL_ELLIPSIS;
        } else {
            cell = read_cell_by_token(ctx, port, c);
        }
        if (cell == CELL_EOF || is_exception(cell)) {
            gc_release(ctx);
            return cell;
        }
        tail = rplacd(tail, cons(ctx, cell, CELL_NIL));
        ++len;
    }
    cell = mk_vector(ctx, len, CELL_UNDEF);
    uint i = 0;
    vector_length(cell) = len;
    for (Cell *ls=cdr(head); i<len && is_pair(ls); i++, ls=cdr(ls)) {
        vector_data(cell)[i] = car(ls);
    }
    gc_release(ctx);
    return cell;
}

static Cell *read_list(Cell *ctx, Cell *port, int c) {
    gc_var3(head, tail, cell);
    gc_preserve3(ctx, head, tail, cell);
    cell = CELL_NIL;
    head = tail = cons(ctx, CELL_NIL, CELL_NIL);

    switch (c) {
    case TOK_LPAREN:    c = TOK_RPAREN; break;
    case TOK_LBRACKET:  c = TOK_RBRACKET; break;
    case TOK_LBRACE:    c = TOK_RBRACE; break;
    }

    int d;
    for (;;) {
        d = get_token(ctx, port);
        if (d == TOK_EOF) {
            gc_release(ctx);
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unexpected eof"), NULL, NULL);
        }
        if (c == d) break;
        if (d == TOK_RPAREN || d == TOK_RBRACKET || d == TOK_RBRACE) {
            gc_release(ctx);
            return mk_exception(ctx, SyntaxError, mk_string(ctx, "unmatched brackets"), NULL, NULL);
        }

        if (d == TOK_DOT) {
            if (cell == CELL_NIL) {
                gc_release(ctx);
                return mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal used of dot"), NULL, NULL);
            }
            cell = read_cell(ctx, port);
            if (cell == CELL_EOF || is_exception(cell)) {
                gc_release(ctx);
                return cell;
            }
            tail = rplacd(tail, cell);
            c = get_token(ctx, port);
            if (c != TOK_RPAREN) {
                gc_release(ctx);
                return mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal used of dot"), NULL, NULL);
            }
            break;
        } else if (d == TOK_ELLIPSIS) {
            cell = CELL_ELLIPSIS;
        } else {
            cell = read_cell_by_token(ctx, port, d);
        }
        if (cell == CELL_EOF || is_exception(cell)) {
            gc_release(ctx);
            return cell;
        }
        tail = rplacd(tail, cons(ctx, cell, CELL_NIL));
    }
    gc_release(ctx);
    return cdr(head);
}

Cell *isc_repl(Cell *ctx) {
    gc_var6(a, b, c, d, e, f);
    gc_preserve6(ctx, a, b, c, d, e, f);
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
            if (ctx_inports_size(ctx) == 1) {
                gc_release(ctx);
                return ctx_ret(ctx);
            }
            pop_load_file(ctx);
            popOp(ctx, ctx_ret(ctx));
        }
        if (is_interactive(ctx)) {
            ctx_instructs(ctx) = CELL_NIL;
            if (ctx_inports_size(ctx) == 1) {
                write_string(ctx, ctx_stdoutport(ctx), ">> ");
            }
        }
        pushOp(ctx, OP_REPL_LOOP, ctx_args(ctx), CELL_NIL);
        gotoOp(ctx, OP_REPL_READ);
    case OP_REPL_READ:
        c = read_cell(ctx, ctx_inport(ctx));
        if (c == CELL_EOF) {
            if (ctx_inports_size(ctx) == 1) {
                gc_release(ctx);
                return ctx_ret(ctx);
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
        c = ctx_ret(ctx);
        if (is_interactive(ctx)) {
            if (is_multivar(c)) {
                for (Cell *ls=multivar_var(c); is_pair(ls); ls=cdr(ls)) {
                    print_cell(ctx, ctx_stdoutport(ctx), car(ls));
                    write_string(ctx, ctx_stdoutport(ctx), "\n");
                }
            } else if (!is_undef(c)) {
                print_cell(ctx, ctx_stdoutport(ctx), c);
                write_string(ctx, ctx_stdoutport(ctx), "\n");
            }
        }
        popOp(ctx, c);
    case OP_ERROR:
        print_cell(ctx, ctx_stderrport(ctx), ctx_ret(ctx));
        if (is_interactive(ctx)) {
            int c;
            while ((c = get_char(ctx, ctx_inport(ctx))) != '\n' && c != EOF);
            ctx_args(ctx) = CELL_NIL;
            ctx_env(ctx) = ctx_global_env(ctx);
            gotoOp(ctx, OP_REPL_LOOP);
        }
        gc_release(ctx);
        return ctx_ret(ctx);
    case OP_DEF:
        c = ctx_code(ctx);
        if (is_closure_expr(car(c))) {
            rplaca(c, closure_expr_expr(car(c)));
        }
        if (is_pair(d = car(c))) {
            e = cdr(c);
            for (; is_pair(d); d = car(d)) {
                c = car(d);
                e = cons(ctx, ctx_lambda(ctx), f = cons(ctx, cdr(d), e));
            }
            ctx_code(ctx) = e;
        } else if (is_symbol(car(c))) {
            c = car(c);
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
            env_add(ctx, closure_env(proc_closure(c)), ctx_code(ctx), c);
        }
        if (env_immtb(ctx_env(ctx))) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "define: environment is not mutable"), NULL, NULL));
        }
        env_add(ctx, ctx_env(ctx), ctx_code(ctx), c);
        popOp(ctx, CELL_UNDEF);
    case OP_SET:
        c = car(ctx_code(ctx));
        if (!is_symbol(c)) gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "parameter is not an identifier:"), c, NULL));
        if (is_nil(find_env(ctx_env(ctx), c))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "unbound variable:"), c, NULL));
        }
        if (is_immutable(c)) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "unable to alter immutable atom"), NULL, NULL));
        }
        ctx_code(ctx) = cadr(ctx_code(ctx));
        pushOp(ctx, OP_SET1, CELL_NIL, c);
        gotoOp(ctx, OP_EVAL);
    case OP_SET1:
        c = ctx_ret(ctx);
        if (env_immtb(ctx_env(ctx))) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "set!: environment is not mutable"), NULL, NULL));
        }
        env_set(ctx, ctx_env(ctx), ctx_code(ctx), c);
        popOp(ctx, CELL_UNDEF);
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
        if (!is_pair(ctx_ret(ctx)) || !is_env(car(ctx_ret(ctx))) || !is_pair(cdr(ctx_ret(ctx)))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax defined"), NULL, NULL));
        }
        env_set(ctx, ctx_env(ctx), ctx_code(ctx), mk_macro(ctx, cdr(ctx_ret(ctx)), car(ctx_ret(ctx))));
        popOp(ctx, CELL_UNDEF);
    case OP_SYNTAX_RULES:
        if (!is_pair(ctx_code(ctx)) || !is_pair(cdr(ctx_code(ctx)))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "invalid syntax format in syntax rules:"), ctx_code(ctx), NULL));
        }
        c = macro_analyze(ctx, car(ctx_code(ctx)), cdr(ctx_code(ctx)), ctx_env(ctx));
        if (is_exception(c)) {
            gotoErr(ctx, c);
        }
        popOp(ctx, c);
    case OP_LAMBDA:
        popOp(ctx, mk_proc(ctx, CELL_NIL, c = mk_closure(ctx, ctx_code(ctx), ctx_env(ctx))));
    case OP_EVAL:
        if (is_nil(ctx_code(ctx))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal empty expresion"), NULL, NULL));
        }
        if (is_closure_expr(ctx_code(ctx))) {
            ctx_env(ctx) = closure_expr_env(ctx_code(ctx));
            ctx_code(ctx) = closure_expr_expr(ctx_code(ctx));
        }
        if (is_symbol(ctx_code(ctx))) {
            c = find_env(ctx_env(ctx), ctx_code(ctx));
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
                c = find_env(d, c);
                if (is_pair(c)) {
                    c = cdr(c);
                    if (is_syntax(c)) {
                        ctx_code(ctx) = cdr(ctx_code(ctx));
                        gotoOp(ctx, syntax_op(c));
                    } else if (is_macro(c)) {
                        ctx_code(ctx) = macro_transform(ctx, macro_matchers(c), macro_env(c), cdr(ctx_code(ctx)), ctx_env(ctx));
                        if (is_exception(ctx_code(ctx))) {
                            gotoErr(ctx, ctx_code(ctx));
                        }
                        #ifdef MACRO_DEBUG
                        write_string(ctx, ctx_stdoutport(ctx), "\n*Macro transform code*\n");
                        print_cell(ctx, ctx_stdoutport(ctx), ctx_code(ctx));
                        write_char(ctx, ctx_stdoutport(ctx), '\n');
                        #endif
                        ctx_code(ctx) = mk_closure(ctx, d = cons(ctx, CELL_NIL, ctx_code(ctx)), macro_env(c));
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
    case OP_APPLY:
        if (is_syntax(ctx_code(ctx))) {
            gotoOp(ctx, syntax_op(ctx_code(ctx)));
        } else if (is_iproc(ctx_code(ctx))) {
            gotoOp(ctx, iproc_op(ctx_code(ctx)));
        } else if (is_eproc(ctx_code(ctx))) {
            popOp(ctx, ctx_code(ctx)->eproc(ctx, ctx_args(ctx)));
        } else if (is_proc(ctx_code(ctx)) || is_closure(ctx_code(ctx))) {
            const char *pname = "lambda";
            if (is_proc(ctx_code(ctx))) {
                pname = string(proc_name(ctx_code(ctx)));
                ctx_code(ctx) = proc_closure(ctx_code(ctx));
            }
            e = closure_env(ctx_code(ctx));
            a = ctx_args(ctx);
            b = closure_args(ctx_code(ctx));

            int min_args = length(b);
            int max_args = is_list(b) ? min_args : 0xFFFF;
            int n = length(a);
            if (n < min_args) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "%s: unexpected number of arguments, expected at least %d but %d %s given",
                                pname, min_args, n, n>1?"were":"was"), NULL, NULL));
            }
            if (n > max_args) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "%s: unexpected number of arguments, expected at most %d but %d %s given",
                                pname, max_args, n, n>1?"were":"was"), NULL, NULL));
            }
            for (; is_pair(b); b = cdr(b), a = cdr(a)) {
                env_add(ctx, e, car(b), car(a));
            }
            if (!is_nil(b) && !is_nil(a)) {
                env_add(ctx, e, b, a);
            }
            ctx_code(ctx) = closure_code(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
            ctx_env(ctx) = e;
            gotoOp(ctx, OP_EVAL_LIST);
        } else if (is_continue(c = ctx_code(ctx))) {
            a = ctx_args(ctx);
            b = ctx_winds(ctx);
            d = continue_winds(c);
            if (!eq(b, d)) {
                Cell *x, *y;
                uint lb = length(b), ld = length(d);
                x = lb > ld ? list_tail(b, lb - ld) : b;
                y = ld > lb ? list_tail(d, ld - lb) : d;
                for (; !eq(x, y); x=cdr(x), y=cdr(y));
                pushOpEx(ctx, OP_APPLY_WIND3, a, c, CELL_NIL, ctx_env(ctx));
                gotoOpEx(ctx, OP_APPLY_WIND, x, b, d, ctx_env(ctx));
            }
            ctx_instructs(ctx) = continue_ins(c);
            if (is_pair(a) && length(a) == 1) {
                popOp(ctx, car(a));
            }
            popOp(ctx, mk_multival(ctx, a));
        }
        gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "illegal procedure: "), ctx_code(ctx), NULL));
    case OP_APPLY_WIND:
        a = ctx_args(ctx);
        c = ctx_code(ctx);
        d = ctx_data(ctx);
        if (!eq(a, c)) {
            pushOpEx(ctx, OP_APPLY_WIND, a, cdr(c), d, ctx_env(ctx));
            ctx_winds(ctx) = cdr(c);
            gotoOpEx(ctx, OP_APPLY, CELL_NIL, cdar(c), CELL_NIL, ctx_env(ctx));
        }
        gotoOpEx(ctx, OP_APPLY_WIND1, a, d, CELL_NIL, ctx_env(ctx));
    case OP_APPLY_WIND1:
        a = ctx_args(ctx);
        c = ctx_code(ctx);
        if (!eq(a, c)) {
            pushOpEx(ctx, OP_APPLY_WIND2, c, CELL_NIL, CELL_NIL, ctx_env(ctx));
            pushOpEx(ctx, OP_APPLY, CELL_NIL, caar(c), CELL_NIL, ctx_env(ctx));
            gotoOpEx(ctx, OP_APPLY_WIND1, a, cdr(c), CELL_NIL, ctx_env(ctx));
        }
        popOp(ctx, CELL_UNDEF);
    case OP_APPLY_WIND2:
        ctx_winds(ctx) = ctx_args(ctx);
        popOp(ctx, CELL_UNDEF);
    case OP_APPLY_WIND3:
        a = ctx_args(ctx);
        ctx_instructs(ctx) = continue_ins(ctx_code(ctx));
        if (is_pair(a) && length(a) == 1) {
            popOp(ctx, car(a));
        }
        popOp(ctx, mk_multival(ctx, a));
    case OP_CALLCC:
        c = proc_closure(car(ctx_args(ctx)));
        if (length(closure_args(c)) != 1) {
            gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "invalid number of arguments to procedure at #<PROCEDURE call-with-current-continuation>"), NULL, NULL));
        }
        ctx_code(ctx) = c;
        d = mk_continue(ctx, ctx_instructs(ctx), ctx_winds(ctx));
        ctx_args(ctx) = cons(ctx, d, CELL_NIL);
        gotoOp(ctx, OP_APPLY);
    case OP_VALUES:
        popOp(ctx, mk_multival(ctx, ctx_args(ctx)));
    case OP_CALL_WITH_VALUES:
        a = ctx_args(ctx);
        pushOp(ctx, OP_CALL_WITH_VALUES1, CELL_NIL, cadr(a));
        gotoOpEx(ctx, OP_APPLY, CELL_NIL, car(a), CELL_NIL, ctx_env(ctx));
    case OP_CALL_WITH_VALUES1:
        d = ctx_ret(ctx);
        if (is_multivar(d)) {
            d = multivar_var(d);
        } else {
            d = cons(ctx, d, CELL_NIL);
        }
        gotoOpEx(ctx, OP_APPLY, d, ctx_code(ctx), CELL_NIL, ctx_env(ctx));
    case OP_DYNAMIC_WIND:
        c = car(ctx_args(ctx));
        pushOp(ctx, OP_DYNAMIC_WIND1, ctx_args(ctx), CELL_NIL);
        gotoOpEx(ctx, OP_APPLY, CELL_NIL, c, CELL_NIL, ctx_env(ctx));
    case OP_DYNAMIC_WIND1:
        a = car(ctx_args(ctx));
        b = cadr(ctx_args(ctx));
        c = caddr(ctx_args(ctx));
        ctx_winds(ctx) = cons(ctx, d = cons(ctx, a, c), ctx_winds(ctx));
        pushOp(ctx, OP_DYNAMIC_WIND2, ctx_winds(ctx), c);
        gotoOpEx(ctx, OP_APPLY, CELL_NIL, b, CELL_NIL, ctx_env(ctx));
    case OP_DYNAMIC_WIND2:
        ctx_winds(ctx) = cdr(ctx_args(ctx));
        pushOp(ctx, OP_DYNAMIC_WIND3, ctx_ret(ctx), CELL_NIL);
        gotoOpEx(ctx, OP_APPLY, CELL_NIL, ctx_code(ctx), CELL_NIL, ctx_env(ctx));
    case OP_DYNAMIC_WIND3:
        popOp(ctx, ctx_args(ctx));
    case OP_SCHEME_REPORT_ENV:
        if (number_long(car(ctx_args(ctx))) != 5) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "scheme-report-environment: unsupported revision"), NULL, NULL));
        }
        popOp(ctx, ctx_r5rs_env(ctx));
    case OP_NULL_ENV:
        if (number_long(car(ctx_args(ctx))) != 5) {
            gotoErr(ctx, mk_exception(ctx, ValueError, mk_string(ctx, "null-environment: unsupported revision"), NULL, NULL));
        }
        popOp(ctx, ctx_null_env(ctx));
    case OP_INTERACTION_ENV:
        popOp(ctx, ctx_global_env(ctx));
    case OP_FORCE:
        c = car(ctx_args(ctx));
        if (promise_result(c)) {
            popOp(ctx, promise_result(c));
        }
        pushOpEx(ctx, OP_FORCE1, CELL_NIL, CELL_NIL, c, ctx_env(ctx));
        gotoOpEx(ctx, OP_EVAL, CELL_NIL, promise_expr(c), CELL_NIL, ctx_env(ctx));
    case OP_FORCE1:
        c = ctx_ret(ctx);
        d = ctx_data(ctx);
        promise_result(d) = c;
        popOp(ctx, c);
    case OP_AND:
    case OP_OR:
        a = ctx_code(ctx);
        if (is_nil(a)) {
            if (OP_AND == op) {
                popOp(ctx, CELL_TRUE);
            }
            popOp(ctx, CELL_FALSE);
        }
        pushOp(ctx, OP_ANDOR, b = mk_long(ctx, op), cdr(a));
        ctx_code(ctx) = car(a);
        gotoOp(ctx, OP_EVAL);
    case OP_ANDOR:
        a = ctx_ret(ctx);
        b = ctx_args(ctx);
        op = number_long(b);
        if (OP_AND == op) {
            if (is_false(a)) popOp(ctx, a);
        } else {
            if (!is_false(a)) popOp(ctx, a);
        }
        c = ctx_code(ctx);
        if (is_pair(c)) {
            pushOp(ctx, OP_ANDOR, ctx_args(ctx), cdr(c));
            ctx_code(ctx) = car(c);
            gotoOp(ctx, OP_EVAL);
        }
        popOp(ctx, a);
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
            e = mk_env(ctx, false, ctx_env(ctx));
            d = car(c);
            a = car(d);
            if (is_symbol(a) && !strcmp(symbol(a), "else")) {
                gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, cdr(d), CELL_NIL, e);
            }
            pushOpEx(ctx, OP_COND1, CELL_NIL, cdr(c), cdr(d), e);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, a, CELL_NIL, e);
        }
        popOp(ctx, CELL_UNDEF);
    case OP_COND1:
        a = ctx_ret(ctx);
        c = ctx_code(ctx);
        d = ctx_data(ctx);
        e = ctx_env(ctx);
        if (!is_false(a)) {
            b = car(d);
            if (is_symbol(b) && !strcmp(symbol(b), "=>")) {
                if (!is_pair(cdr(d)) || !is_nil(cddr(d))) {
                    gc_release(ctx);
                    return mk_exception(ctx, SyntaxError, mk_string(ctx, "cond: 'bad clause form with '=>'"), NULL, NULL);
                }
                d = cadr(d);
                if (is_proc(d)) {
                    ctx_ret(ctx) = d;
                    gotoOpEx(ctx, OP_COND2, CELL_NIL, a, CELL_NIL, e);
                }
                pushOp(ctx, OP_COND2, CELL_NIL, a);
                gotoOpEx(ctx, OP_EVAL, CELL_NIL, d, CELL_NIL, e);
            }
            gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, d, CELL_NIL, e);
        }
        if (is_pair(c)) {
            d = car(c);
            a = car(d);
            if (is_symbol(a) && !strcmp(symbol(a), "else")) {
                gotoOpEx(ctx, OP_EVAL_LIST, CELL_NIL, cdr(d), CELL_NIL, e);
            }
            pushOpEx(ctx, OP_COND1, CELL_NIL, cdr(c), cdr(d), e);
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
            e = mk_env(ctx, false, ctx_env(ctx));
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
        ctx_args(ctx) = CELL_NIL;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_DO:
        c = car(ctx_code(ctx));
        d = cdr(ctx_code(ctx));
        e = mk_env(ctx, false, ctx_env(ctx));
        if (is_pair(c)) {
            b = cons(ctx, CELL_NIL, CELL_NIL);
            for (Cell *ls=c; is_pair(ls); ls=cdr(ls)) {
                a = car(ls);
                if (!is_pair(a) || !is_symbol(car(a)) || !is_pair(cdr(a)) || is_contains(b, car(a))) {
                    gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "do"), NULL, NULL));
                }
                list_add(ctx, b, car(a));
                env_add(ctx, e, car(a), CELL_UNDEF);
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
            env_set(ctx, e, car(b), car(ls));
        }
        b = cons(ctx, CELL_NIL, CELL_NIL);
        for (Cell *ls=car(c); is_pair(ls); ls=cdr(ls)) {
            a = car(ls);
            if (is_pair(cddr(a))) {
                list_add(ctx, b, d = cons(ctx, car(a), caddr(a)));
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
            env_set(ctx, ctx_env(ctx), caar(a), car(b));
        }
        gotoOpEx(ctx, OP_DO2, car(d), cdr(d), CELL_NIL, ctx_env(ctx));
    case OP_LET:
        ctx_args(ctx) = CELL_NIL;
        ctx_ret(ctx) = ctx_code(ctx);
        ctx_code(ctx) = is_symbol(car(ctx_code(ctx))) ? cadr(ctx_code(ctx)) : car(ctx_code(ctx));
        ctx_env(ctx) = mk_env(ctx, false, ctx_env(ctx));
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
        c = is_symbol(car(ctx_code(ctx))) ? cadr(ctx_code(ctx)) : car(ctx_code(ctx));
        if (is_symbol(car(ctx_code(ctx)))) {
            d = cddr(ctx_code(ctx));
        } else {
            d = cdr(ctx_code(ctx));
        }
        for (Cell *a = ctx_args(ctx); !is_nil(a); c = cdr(c), a = cdr(a)) {
            env_add(ctx, ctx_env(ctx), caar(c), car(a));
        }
        if (is_symbol(car(ctx_code(ctx)))) {
            for (c = cadr(ctx_code(ctx)), ctx_args(ctx) = CELL_NIL; !is_nil(c); c = cdr(c)) {
                ctx_args(ctx) = cons(ctx, caar(c), ctx_args(ctx));
            }
            c = mk_closure(ctx, e = cons(ctx, f = reverse(ctx, ctx_args(ctx)), cddr(ctx_code(ctx))), ctx_env(ctx));
            ctx_env(ctx) = closure_env(c);
            env_add(ctx, ctx_env(ctx), car(ctx_code(ctx)), c);
            ctx_code(ctx) = cddr(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
        } else {
            ctx_code(ctx) = cdr(ctx_code(ctx));
            ctx_args(ctx) = CELL_NIL;
        }
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LETSEQ:
        ctx_env(ctx) = mk_env(ctx, false, ctx_env(ctx));
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
        ctx_env(ctx) = mk_env(ctx, false, ctx_env(ctx));
        env_add(ctx, ctx_env(ctx), caar(ctx_code(ctx)), ctx_ret(ctx));
        ctx_code(ctx) = cdr(ctx_code(ctx));
        if (is_pair(ctx_code(ctx))) {
            pushOp(ctx, OP_LETSEQ1, ctx_args(ctx), ctx_code(ctx));
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
        e = mk_env(ctx, false, ctx_env(ctx));
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
                env_add(ctx, e, car(a), CELL_UNDEF);
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
            env_set(ctx, e, car(ls), car(b));
        }
        ctx_code(ctx) = d;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LET_SYNTAX:
    case OP_LETREC_SYNTAX:
        c = car(ctx_code(ctx));
        d = cdr(ctx_code(ctx));
        e = mk_env(ctx, false, ctx_env(ctx));
        if (is_pair(c)) {
            const char *opn = (op == OP_LET_SYNTAX) ? "let-syntax" : "letrec-syntax";
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
                env_add(ctx, e, car(a), CELL_UNDEF);
            }
            if (op == OP_LET_SYNTAX) {
                d = cons(ctx, d, CELL_NIL);
            }
            rplaca(b, mk_long(ctx, op));
            pushOpEx(ctx, OP_LET_SYNTAX1, b, cdr(c), d, e);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cadar(c), CELL_NIL, e);
        }
        ctx_args(ctx) = CELL_NIL;
        ctx_code(ctx) = d;
        ctx_env(ctx) = e;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_LET_SYNTAX1:
        c = ctx_ret(ctx);
        if (!is_proc(c)) {
            op = number_long(car(ctx_args(ctx)));
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, (op == OP_LET_SYNTAX) ? "let-syntax" : "letrc-syntax"), NULL, NULL));
        }
        pushOpEx(ctx, OP_LET_SYNTAX2, ctx_args(ctx), ctx_code(ctx), ctx_data(ctx), ctx_env(ctx));
        gotoOpEx(ctx, OP_APPLY, CELL_NIL, c, CELL_NIL, ctx_env(ctx));
    case OP_LET_SYNTAX2:
        a = ctx_ret(ctx);
        b = cdr(ctx_args(ctx));
        c = ctx_code(ctx);
        d = ctx_data(ctx);
        e = ctx_env(ctx);
        op = number_long(car(ctx_args(ctx)));
        if (!is_pair(a) || !is_env(car(a)) || !is_pair(cdr(a))) {
            gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, (op == OP_LET_SYNTAX) ? "let-syntax" : "letrc-syntax"), NULL, NULL));
        }
        if (op == OP_LETREC_SYNTAX) {
            for (Cell *ls=b; is_pair(ls); ls=cdr(ls)) {
                env_set(ctx, e, car(ls), f = mk_macro(ctx, cdr(a), car(a)));
            }
        } else {
            list_add(ctx, d, f = mk_macro(ctx, cdr(a), car(a)));
        }
        if (is_pair(c)) {
            pushOpEx(ctx, OP_LET_SYNTAX1, b, cdr(c), d, e);
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, cadar(c), CELL_NIL, e);
        }
        if (op == OP_LET_SYNTAX) {
            c = cdr(d);
            d = car(d);
            for (Cell *ls=b; is_pair(ls); ls=cdr(ls), c=cdr(c)) {
                env_add(ctx, e, car(ls), car(c));
            }
        }
        ctx_code(ctx) = d;
        gotoOp(ctx, OP_EVAL_LIST);
    case OP_QUOTE:
        popOp(ctx, car(ctx_code(ctx)));
    case OP_QUASIQUOTE:
        ctx_code(ctx) = cons(ctx, c = mk_long(ctx, 1), car(ctx_code(ctx)));
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
            pushOp(ctx, OP_QUASIQUOTE3, CELL_NIL, f = cons(ctx, d, cdr(c)));
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
        pushOp(ctx, OP_QUASIQUOTE3, ctx_args(ctx), f = cons(ctx, d, cdr(c)));
        ctx_args(ctx) = CELL_NIL;
        ctx_code(ctx) = cons(ctx, d, car(c));
        gotoOp(ctx, OP_QUASIQUOTE1);
    case OP_UNQUOTE:
        gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "unquote: not in quasiquote"), NULL, NULL));
    case OP_UNQUOTE_SPLICING:
        gotoErr(ctx, mk_exception(ctx, SyntaxError, mk_string(ctx, "unquote-splicing: not in quasiquote"), NULL, NULL));
    case OP_DELAY:
        popOp(ctx, mk_promise(ctx, car(ctx_code(ctx))));

/************* core proc **************/
    case OP_MAP:
    case OP_FOREACH: {
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
            e = mk_env(ctx, false, ctx_env(ctx));
            if (op == OP_MAP) {
                pushOpEx(ctx, OP_MAP1, CELL_NIL, b, c, e);
            } else {
                pushOpEx(ctx, OP_FOREACH1, CELL_NIL, b, c, e);
            }
            gotoOpEx(ctx, OP_APPLY, a, c, CELL_NIL, e);
        }
        popOp(ctx, CELL_NIL);
    }
    case OP_MAP1:
    case OP_FOREACH1:
        if (op == OP_MAP1) {
            ctx_args(ctx) = cons(ctx, ctx_ret(ctx), ctx_args(ctx));
        }
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
            pushOpEx(ctx, Op(op), ctx_args(ctx), e, ctx_data(ctx), ctx_env(ctx));
            gotoOpEx(ctx, OP_APPLY, d, ctx_data(ctx), CELL_NIL, ctx_env(ctx));
        }
        if (op == OP_MAP1) {
            popOp(ctx, reverse(ctx, ctx_args(ctx)));
        }
        popOp(ctx, CELL_UNDEF);
    case OP_PEVAL:
        c = car(ctx_args(ctx));
        d = cdr(ctx_args(ctx));
        if (is_nil(d)) {
            gotoOpEx(ctx, OP_EVAL, CELL_NIL, c, CELL_NIL, ctx_global_env(ctx));
        } else if (!is_pair(d) || !is_env(car(d))) {
            gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "eval: unmatched type of argument 2, must be enviroment"), NULL, NULL));
        }
        gotoOpEx(ctx, OP_EVAL, CELL_NIL, c, CELL_NIL, car(d));
    case OP_PAPPLY:
        c = car(ctx_args(ctx));
        d = cdr(ctx_args(ctx));
        if (!is_proc(c) && !is_continue(c)) {
            gotoErr(ctx, mk_exception(ctx, TypeError,
                    mk_string(ctx, "apply: unmatched type of argument 1, must be precedure"), NULL, NULL));
        }
        if (is_nil(cdr(d))) {
            if (!is_pair(car(d))) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "invalid arguments"), NULL, NULL));
            }
            gotoOpEx(ctx, OP_APPLY, car(d), c, CELL_NIL, ctx_env(ctx));
        }
        a = b = cons(ctx, CELL_NIL, CELL_NIL);
        for (e=d; is_pair(e); e=cdr(e)) {
            if (is_nil(cdr(e))) {
                if (!is_pair(car(e))) {
                    gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "invalid arguments"), NULL, NULL));
                }
                list_extend(ctx, a, car(e));
                break;
            }
            a = rplacd(a, cons(ctx, car(e), CELL_NIL));
        }
        gotoOpEx(ctx, OP_APPLY, cdr(b), c, CELL_NIL, ctx_env(ctx));
    case OP_LOAD:
        c = push_load_file(ctx, string(car(ctx_args(ctx))));
        if (is_exception(c))
            gotoErr(ctx, c);
        gotoOp(ctx, OP_REPL_LOOP);
    case OP_TRANSCRIPT_ON: {
        if (ctx_transc_port(ctx)) {
            port_close(ctx, ctx_transc_port(ctx));
        }
        a = car(ctx_args(ctx));
        char *name = string(a);
        FILE *trsf = fopen(name, "w+");
        if (!trsf) {
            gotoErr(ctx, mk_exception(ctx, IOError, mk_string(ctx, "transcript-on: can not open file:%s", name), NULL, NULL));
        }
        ctx_transc_port(ctx) = mk_port(ctx, trsf, name, PORT_OUTPUT_FILE);
        ctx_transc_idx(ctx) = port_file_bufidx(ctx_inport(ctx));
        ctx_transc_inportp(ctx) = false;
        popOp(ctx, CELL_UNDEF);
    }
    case OP_TRANSCRIPT_OFF:
        if (ctx_transc_port(ctx)) {
            port_close(ctx, ctx_transc_port(ctx));
            ctx_transc_port(ctx) = NULL;
        }
        popOp(ctx, CELL_UNDEF);
    case OP_DISPLAY: {
        int len = length(ctx_args(ctx));
        if (len == 1) {
            print_cell_readable(ctx, ctx_stdoutport(ctx), car(ctx_args(ctx)));
        } else if (len == 2) {
            print_cell_readable(ctx, cadr(ctx_args(ctx)), car(ctx_args(ctx)));
        }
        popOp(ctx, CELL_UNDEF);
    }
    case OP_NEWLINE:
        if (is_pair(ctx_args(ctx))) c = car(ctx_args(ctx));
        else c = ctx_stdoutport(ctx);
        write_char(ctx, c, '\n');
        popOp(ctx, CELL_UNDEF);
    case OP_READ:
        a = ctx_args(ctx);
        if (is_pair(a)) {
            c = read_cell(ctx, car(a));
        } else {
            c = read_cell(ctx, ctx_stdinport(ctx));
        }
        popOp(ctx, c);
    case OP_WRITE:
        a = ctx_args(ctx);
        if (length(a) > 1) {
            print_cell(ctx, cadr(a), car(a));
        } else {
            print_cell(ctx, ctx_stdoutport(ctx), car(a));
        }
        popOp(ctx, CELL_UNDEF);
    case OP_READ_CHAR: {
        int chr;
        a = ctx_args(ctx);
        if (is_pair(a)) {
            chr = get_char(ctx, car(a));
        } else {
            chr = get_char(ctx, ctx_stdinport(ctx));
        }
        if (chr == EOF) {
            c = CELL_EOF;
        } else {
            c = mk_char(ctx, chr);
        }
        popOp(ctx, c);
    }
    case OP_WRITE_CHAR:
        a = ctx_args(ctx);
        if (length(a) > 1) {
            print_cell_readable(ctx, cadr(a), car(a));
        } else {
            print_cell_readable(ctx, ctx_stdoutport(ctx), car(a));
        }
        popOp(ctx, CELL_UNDEF);
    case OP_PEEK_CHAR: {
        int chr;
        a = ctx_args(ctx);
        if (is_pair(a)) {
            chr = peek_char(ctx, car(a));
        } else {
            chr = peek_char(ctx, ctx_stdinport(ctx));
        }
        if (chr == EOF) {
            c = CELL_EOF;
        } else {
            c = mk_char(ctx, chr);
        }
        popOp(ctx, c);
    }
    case OP_CHAR_READY_P:
        a = ctx_args(ctx);
        if (is_pair(a)) {
            a = car(a);
        } else {
            a = ctx_stdinport(ctx);
        }
        popOp(ctx, char_ready_p(a) ? CELL_TRUE : CELL_FALSE);
    case OP_EOF_OBJECT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_eof(c) ? CELL_TRUE : CELL_FALSE);
    case OP_OPEN_INPUT_FILE: {
        a = car(ctx_args(ctx));
        const char *nm = string(a);
        if (!access(nm, F_OK | R_OK)) {
            FILE *in = fopen(nm, "r");
            if (!in) {
                gotoErr(ctx, mk_exception(ctx, IOError, mk_string(ctx, "can not open file '%s'", nm), NULL, NULL));
            }
            popOp(ctx, mk_port(ctx, in, nm, PORT_INPUT_FILE));
        }
        gotoErr(ctx, mk_exception(ctx, IOError, mk_string(ctx, "can not open file '%s'", nm), NULL, NULL));
    }
    case OP_OPEN_OUTPUT_FILE: {
        a = car(ctx_args(ctx));
        const char *nm = string(a);
        FILE *out = fopen(nm, "w");
        if (!out) {
            gotoErr(ctx, mk_exception(ctx, IOError, mk_string(ctx, "can not open file '%s'", nm), NULL, NULL));
        }
        popOp(ctx, mk_port(ctx, out, nm, PORT_OUTPUT_FILE));
    }
    case OP_CLOSE_INPUT_PORT:
        a = car(ctx_args(ctx));
        if (port_type(a) & PORT_INPUT) {
            if (port_type(a) & PORT_INPUT_FILE) {
                fclose(port_file(a));
            }
            port_type(a) = PORT_FREE;
        }
        popOp(ctx, CELL_UNDEF);
    case OP_CLOSE_OUTPUT_PORT:
        a = car(ctx_args(ctx));
        if (port_type(a) & PORT_OUTPUT) {
            if (port_type(a) & PORT_OUTPUT_FILE) {
                fclose(port_file(a));
            }
            port_type(a) = PORT_FREE;
        }
        popOp(ctx, CELL_UNDEF);
    case OP_CURR_INPUT_PORT:
        popOp(ctx, ctx_stdinport(ctx));
    case OP_CURR_OUTPUT_PORT:
        popOp(ctx, ctx_stdoutport(ctx));
    case OP_SET_CURR_INPUT_PORT:
        ctx_stdinport(ctx) = car(ctx_args(ctx));
        popOp(ctx, CELL_UNDEF);
    case OP_SET_CURR_OUTPUT_PORT:
        ctx_stdoutport(ctx) = car(ctx_args(ctx));
        popOp(ctx, CELL_UNDEF);
    case OP_CALL_WITH_INPUT_FILE:
        a = ctx_args(ctx);
        pushOp(ctx, OP_CALL_WITH_INPUT_FILE1, CELL_NIL, cadr(a));
        gotoOpEx(ctx, OP_OPEN_INPUT_FILE, car(a), CELL_NIL, CELL_NIL, ctx_env(ctx));
    case OP_CALL_WITH_INPUT_FILE1:
        a = ctx_ret(ctx);
        c = ctx_code(ctx);
        pushOp(ctx, OP_CALL_WITH_INPUT_FILE2, CELL_NIL, a);
        gotoOpEx(ctx, OP_APPLY, a, c, CELL_NIL, ctx_env(ctx));
    case OP_CALL_WITH_INPUT_FILE2:
        c = ctx_code(ctx);
        if (port_type(c) & PORT_INPUT) {
            if (port_type(c) & PORT_INPUT_FILE) {
                fclose(port_file(c));
            }
            port_type(c) = PORT_FREE;
        }
        popOp(ctx, ctx_ret(ctx));
    case OP_CALL_WITH_OUTPUT_FILE:
        a = ctx_args(ctx);
        pushOp(ctx, OP_CALL_WITH_OUTPUT_FILE1, CELL_NIL, cadr(a));
        gotoOpEx(ctx, OP_OPEN_OUTPUT_FILE, car(a), CELL_NIL, CELL_NIL, ctx_env(ctx));
    case OP_CALL_WITH_OUTPUT_FILE1:
        a = ctx_ret(ctx);
        c = ctx_code(ctx);
        pushOp(ctx, OP_CALL_WITH_OUTPUT_FILE2, CELL_NIL, a);
        gotoOpEx(ctx, OP_APPLY, a, c, CELL_NIL, ctx_env(ctx));
    case OP_CALL_WITH_OUTPUT_FILE2:
        c = ctx_code(ctx);
        if (port_type(c) & PORT_OUTPUT) {
            if (port_type(c) & PORT_OUTPUT_FILE) {
                fclose(port_file(c));
            }
            port_type(c) = PORT_FREE;
        }
        popOp(ctx, ctx_ret(ctx));
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
    case OP_CHAR_EP:
        popOp(ctx, char_value(car(ctx_args(ctx))) == char_value(cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_LP:
        popOp(ctx, char_value(car(ctx_args(ctx))) < char_value(cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_LEP:
        popOp(ctx, char_value(car(ctx_args(ctx))) <= char_value(cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_GP:
        popOp(ctx, char_value(car(ctx_args(ctx))) > char_value(cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_GEP:
        popOp(ctx, char_value(car(ctx_args(ctx))) >= char_value(cadr(ctx_args(ctx))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_CI_EP:
        popOp(ctx, toupper(char_value(car(ctx_args(ctx)))) == toupper(char_value(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_CI_LP:
        popOp(ctx, toupper(char_value(car(ctx_args(ctx)))) < toupper(char_value(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_CI_LEP:
        popOp(ctx, toupper(char_value(car(ctx_args(ctx)))) <= toupper(char_value(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_CI_GP:
        popOp(ctx, toupper(char_value(car(ctx_args(ctx)))) > toupper(char_value(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_CI_GEP:
        popOp(ctx, toupper(char_value(car(ctx_args(ctx)))) >= toupper(char_value(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_ALPHA_P:
        popOp(ctx, isalpha(char_value(car(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_NUMBER_P:
        popOp(ctx, isdigit(char_value(car(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_SPACE_P:
        popOp(ctx, isspace(char_value(car(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_UPPER_P:
        popOp(ctx, isupper(char_value(car(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_LOWER_P:
        popOp(ctx, islower(char_value(car(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_CHAR_TO_INTEGER:
        popOp(ctx, mk_long(ctx, char_value(car(ctx_args(ctx)))));
    case OP_CHAR_UPCASE:
        a = car(ctx_args(ctx));
        if (isalpha(char_value(a)) && islower(char_value(a))) {
            a = mk_char(ctx, toupper(char_value(a)));
        }
        popOp(ctx, a);
    case OP_CHAR_DOWNCASE:
        a = car(ctx_args(ctx));
        if (isalpha(char_value(a)) && isupper(char_value(a))) {
            a = mk_char(ctx, toupper(char_value(a)));
        }
        popOp(ctx, a);
    case OP_STRING:
        a = ctx_args(ctx);
        c = mk_string(ctx, length(a));
        for (uint i=0; is_pair(a); a=cdr(a), ++i) {
            string_data(c)[i] = char_value(car(a));
        }
        popOp(ctx, c);
    case OP_MAKE_STRING:
        a = ctx_args(ctx);
        if (length(a) > 1) {
            popOp(ctx, mk_string(ctx, number_long(car(a)), char_value(cadr(a))));
        }
        popOp(ctx, mk_string(ctx, number_long(car(a))));
    case OP_STRING_LENGTH:
        popOp(ctx, mk_long(ctx, string_size(car(ctx_args(ctx)))));
    case OP_STRING_REF: {
        a = car(ctx_args(ctx));
        uint idx = number_long(cadr(ctx_args(ctx)));
        if (idx >= string_size(a)) {
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "string-ref: index out of range: %d", idx), NULL, NULL));
        }
        popOp(ctx, mk_char(ctx, string_data(a)[idx]));
    }
    case OP_STRING_SET: {
        a = car(ctx_args(ctx));
        uint idx = number_long(cadr(ctx_args(ctx)));
        if (idx >= string_size(a)) {
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "string-set!: index out of range: %d", idx), NULL, NULL));
        }
        string_data(a)[idx] = char_value(caddr(ctx_args(ctx)));
        popOp(ctx, CELL_UNDEF);
    }
    case OP_SUBSTRING: {
        uint st = number_long(cadr(ctx_args(ctx)));
        uint ed = number_long(caddr(ctx_args(ctx)));
        a = car(ctx_args(ctx));
        if (st > ed || ed > string_size(a)) {
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "substring: range error"), NULL, NULL));
        }
        c = mk_string(ctx, ed - st);
        memcpy(string_data(c), string_data(a) + st, ed - st);
        popOp(ctx, c);
    }
    case OP_STRING_APPEND: {
        uint sz = 0;     
        for (a=ctx_args(ctx); is_pair(a); a=cdr(a)) {
            sz += string_size(car(a));
        }
        c = mk_string(ctx, sz);
        for (a=ctx_args(ctx), sz=0; is_pair(a); a=cdr(a)) {
            b = car(a);
            memcpy(string_data(c) + sz, string_data(b), string_size(b));
            sz += string_size(b);
        }
        popOp(ctx, c);
    }
    case OP_STRING_COPY:
        a = car(ctx_args(ctx));
        c = mk_string(ctx, string_size(a));
        memcpy(string_data(c), string_data(a), string_size(a));
        popOp(ctx, c);
    case OP_STRING_FILL:
        a = car(ctx_args(ctx));
        memset(string_data(a), char_value(cadr(ctx_args(ctx))), string_size(a));
        popOp(ctx, CELL_UNDEF);
    case OP_STRING_TO_SYMBOL:
        popOp(ctx, mk_symbol(ctx, string_data(car(ctx_args(ctx)))));
    case OP_STRING_TO_LIST:
        a = car(ctx_args(ctx));
        c = CELL_NIL;
        for (uint i=0; i<string_size(a); ++i) {
            c = cons(ctx, mk_char(ctx, string_data(a)[i]), c);
        }
        popOp(ctx, reverse(ctx, c));
    case OP_STRING_TO_NUMBER:
        a = car(ctx_args(ctx));
        c = read_const_from_string(ctx, ctx_inport(ctx), string_data(a), string_size(a));
        if (is_nil(c)) {
            popOp(ctx, CELL_FALSE);
        }
        popOp(ctx, c);
    case OP_STRING_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_string(c) ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_EP:
        popOp(ctx, !strcmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_LP:
        popOp(ctx, strcmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) < 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_LEP:
        popOp(ctx, strcmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) <= 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_GP:
        popOp(ctx, strcmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) > 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_GEP:
        popOp(ctx, strcmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) >= 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_CI_EP:
        popOp(ctx, !strcasecmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_CI_LP:
        popOp(ctx, strcasecmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) < 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_CI_LEP:
        popOp(ctx, strcasecmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) <= 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_CI_GP:
        popOp(ctx, strcasecmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) > 0 ? CELL_TRUE : CELL_FALSE);
    case OP_STRING_CI_GEP:
        popOp(ctx, strcasecmp(string_data(car(ctx_args(ctx))), string_data(cadr(ctx_args(ctx)))) >= 0 ? CELL_TRUE : CELL_FALSE);
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
    case OP_LIST_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_list(c) ? CELL_TRUE : CELL_FALSE);
    case OP_LIST:
        popOp(ctx, ctx_args(ctx));
    case OP_LIST_TO_STRING:
        a = car(ctx_args(ctx));
        b = mk_string(ctx, length(a));
        for (uint i=0; is_pair(a); a=cdr(a), ++i) {
            c = car(a);
            if (!is_char(c)) {
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, "list->string: not a character list"), NULL, NULL));
            }
            string_data(b)[i] = char_value(c);
        }
        popOp(ctx, b);
    case OP_LIST_TO_VECTOR:
        a = car(ctx_args(ctx));
        c = mk_vector(ctx, length(a), CELL_UNDEF);
        for (uint i=0; is_pair(a); a=cdr(a), ++i) {
            vector_data(c)[i] = car(a);
        }
        popOp(ctx, c);
    case OP_SYMBOL_TO_STRING:
        popOp(ctx, mk_string(ctx, symbol_data(car(ctx_args(ctx)))));
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
    case OP_APPEND: {
        if (is_nil(ctx_args(ctx)))
            popOp(ctx, CELL_NIL);
        c = cons(ctx, CELL_NIL, CELL_NIL);
        d = ctx_args(ctx);
        uint i=0, len = length(d);
        for (; is_pair(d); d=cdr(d), ++i) {
            e = car(d);
            if(i != len-1 && !is_list(e)) {
                char buf[STR_BUF_SIZE] = "";
                snprintf(buf, STR_BUF_SIZE, "append: unmatched type of argument %d, must be list", i + 1);
                gotoErr(ctx, mk_exception(ctx, TypeError, mk_string(ctx, buf), NULL, NULL));
            }
            if (!is_nil(e)) {
                list_extend(ctx, c, e);
            }
        }
        popOp(ctx, cdr(c));
    }
    case OP_REVERSE:
        popOp(ctx, reverse(ctx, car(ctx_args(ctx))));
    case OP_LIST_TAIL:
    case OP_LIST_REF: {
        uint idx = number_long(cadr(ctx_args(ctx)));
        uint len = length(car(ctx_args(ctx)));
        c = car(ctx_args(ctx));
        if (op == OP_LIST_TAIL) {
            if (idx > len) {
                gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of range: %d", idx), NULL, NULL));
            }
            for (uint i=1; i<=idx; ++i)
                c = cdr(c);
        } else {
            if (idx >= len) {
                gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of range: %d", idx), NULL, NULL));
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
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of range: %d", number_long(d)), NULL, NULL));
        }
        popOp(ctx, vector_data(c)[number_long(d)]);
    case OP_VECTOR_SET:
        c = car(ctx_args(ctx));
        d = cadr(ctx_args(ctx));
        e = caddr(ctx_args(ctx));
        if (number_long(d) >= vector_length(c)) {
            gotoErr(ctx, mk_exception(ctx, IndexError, mk_string(ctx, "index out of range: %d", number_long(d)), NULL, NULL));
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
    case OP_PORT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_port(c) ? CELL_TRUE : CELL_FALSE);
    case OP_INPUT_PORT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_inport(c) ? CELL_TRUE : CELL_FALSE);
    case OP_OUTPUT_PORT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_outport(c) ? CELL_TRUE : CELL_FALSE);

/************* numeric ************/
    case OP_NUMBER_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_number(c) ? CELL_TRUE : CELL_FALSE);
    case OP_INTEGER_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_integer(c) ? CELL_TRUE : CELL_FALSE);
    case OP_RATIONAL_P:
    case OP_REAL_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_real(c) ? CELL_TRUE : CELL_FALSE);
    case OP_COMPLEX_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_complex(c) ? CELL_TRUE : CELL_FALSE);
    case OP_EXACT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_exact(c) ? CELL_TRUE : CELL_FALSE);
    case OP_INEXACT_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_inexact(c) ? CELL_TRUE : CELL_FALSE);
    case OP_ZERO_P:
        c = car(ctx_args(ctx));
        popOp(ctx, is_zero(c) ? CELL_TRUE : CELL_FALSE);
    case OP_POSITIVE_P:
        c = car(ctx_args(ctx));
        d = CELL_FALSE;
        switch (number_type(c)) {
        case NUMBER_LONG:
            d = number_long(c) > 0 ? CELL_TRUE : CELL_FALSE;
            break;
        case NUMBER_DOUBLE:
            d = number_double(c) > 0 ? CELL_TRUE : CELL_FALSE;
            break;
        case NUMBER_FRACTION:
            d = number_long(number_fn_nr(c)) > 0 ? CELL_TRUE : CELL_FALSE;
            break;
        }
        popOp(ctx, d);
    case OP_NEGATIVE_P:
        c = car(ctx_args(ctx));
        d = CELL_FALSE;
        switch (number_type(c)) {
        case NUMBER_LONG:
            d = number_long(c) < 0 ? CELL_TRUE : CELL_FALSE;
            break;
        case NUMBER_DOUBLE:
            d = number_double(c) < 0 ? CELL_TRUE : CELL_FALSE;
            break;
        case NUMBER_FRACTION:
            d = number_long(number_fn_nr(c)) < 0 ? CELL_TRUE : CELL_FALSE;
            break;
        }
        popOp(ctx, d);
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
            e = num_real_compare(c, car(d));
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
    case OP_ABS:
        c = car(ctx_args(ctx));
        switch (number_type(c)) {
        case NUMBER_LONG:
            d = (number_long(c) < 0) ? mk_long(ctx, -number_long(c)) : mk_long(ctx, number_long(c));
            break;
        case NUMBER_DOUBLE:
            d = (number_double(c) < 0) ? mk_double(ctx, -number_double(c)) : mk_double(ctx, number_double(c));
            break;
        case NUMBER_FRACTION:
            d = number_long(number_fn_nr(c)) < 0 ? mk_fraction(ctx, -number_long(number_fn_nr(c)), number_long(number_fn_dr(c))) : mk_fraction(ctx, number_long(number_fn_nr(c)), number_long(number_fn_dr(c)));
            break;
        }
        popOp(ctx, d);
    default:
        break;
    }
    gc_release(ctx);
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

Cell *isc_init(PortType pt, FILE *in, char *src) {
    Cell *port;
    gc_var1(ctx);
    ctx = ischeme_ctx_new();
    gc_preserve1(ctx, ctx);
    if (pt == PORT_STDIN) {
        port = ctx_stdinport(ctx);
    } else if (pt == PORT_INPUT_FILE) {
        port = mk_port(ctx, in, src, pt);
    } else if (pt == PORT_INPUT_STRING) {
        port =  mk_port(ctx, src, src + strlen(src), pt);
    }
    ctx_inport(ctx) = port;
    ctx_inports_push(ctx, port);
    gc_release(ctx);
    return ctx;
}

#define def_func(_fc) \
bool _fc##_f(Cell *c) { \
    return _fc(c); \
}
def_func(is_any)
def_func(is_char)
def_func(is_letter)
def_func(is_number)
def_func(is_real)
def_func(is_integer)
def_func(is_natural)
def_func(is_string)
def_func(is_symbol)
def_func(is_pair)
def_func(is_list)
def_func(is_vector)
def_func(is_procs)
def_func(is_env)
def_func(is_continue)
def_func(is_port)
def_func(is_inport)
def_func(is_outport)
def_func(is_promise)
#undef def_func

static struct {
    bool (*func)(Cell*);
    const char *kind;
} g_arg_inspector[] = {
    {is_any_f,      0},
    {is_char_f,     "character"},
    {is_letter_f,   "letter"},
    {is_number_f,   "number"},
    {is_real_f,     "real"},
    {is_integer_f,  "integer"},
    {is_natural_f,  "nonnegative integer"},
    {is_string_f,   "string"},
    {is_symbol_f,   "symbol"},
    {is_pair_f,     "pair"},
    {is_list_f,     "list"},
    {is_vector_f,   "vector"},
    {is_procs_f,    "procedure"},    
    {is_env_f,      "environment"},
    {is_continue_f, "continuation"},
    {is_port_f,     "port"},
    {is_inport_f,   "input port"},
    {is_outport_f,  "output port"},
    {is_promise_f,  "promise"},
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
                if (is_closure_expr(arg)) arg = closure_expr_expr(arg);
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

void isc_finalize(Cell *ctx) {

}

/**************** ffi ****************/
#ifdef __cplusplus
extern "C" {
#endif

void ischeme_init() {
}

Cell *ischeme_ctx_new() {
    Segment *seg;
    Cell dummy_ctx;
    gc_var2(ctx, c);

    seg = cell_mk_segment(ISC_SEG_INIT_SIZE);
    if (!seg) {
        IError("no memory");
        return NULL;
    }
    dummy_ctx.t = CONTEXT;
    ctx_segments(&dummy_ctx) = seg;
    ctx = context_new(&dummy_ctx);
    gc_preserve2(ctx, ctx, c);
    ctx_segments(ctx) = seg;

    g_true.t = BOOLEAN;
    g_false.t = BOOLEAN;
    init_readers();
    ctx_inports(ctx) = vector<Cell*>();
    ctx_stdinport(ctx) = mk_port(ctx, stdin, NULL, PORT_STDIN);
    ctx_stdoutport(ctx) = mk_port(ctx, stdout, NULL, PORT_STDOUT);
    ctx_stderrport(ctx) = mk_port(ctx, stderr, NULL, PORT_STDERR);
    ctx_inport(ctx) = ctx_stdinport(ctx);
    ctx_transc_port(ctx) = NULL;
    ctx_instructs(ctx) = CELL_NIL;
    ctx_winds(ctx) = CELL_NIL;
    ctx_saves(ctx) = NULL;
    ctx_lambda(ctx) = internal(ctx, "lambda");
    ctx_quote(ctx) = internal(ctx, "quote");
    ctx_unquote(ctx) = internal(ctx, "unquote");
    ctx_quasiquote(ctx) = internal(ctx, "quasiquote");
    ctx_unquote_splicing(ctx) = internal(ctx, "unquote-splicing");

    Cell *syntax_env = mk_env(ctx, false, CELL_NIL);
    Cell *std_env = mk_env(ctx, false, syntax_env);
    ctx_null_env(ctx) = mk_env(ctx, true, syntax_env);
    ctx_r5rs_env(ctx) = mk_env(ctx, true, std_env);
    ctx_global_env(ctx) = mk_env(ctx, false, std_env);
    ctx_env(ctx) = ctx_global_env(ctx);
        
    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) {
                c = mk_syntax(ctx, i);
                env_add(ctx, syntax_env, internal(ctx, g_opcodes[i].name), c);
            } else {
                c = mk_iproc(ctx, i);
                env_add(ctx, std_env, internal(ctx, g_opcodes[i].name), c);
            }
        }
    }
    env_add(ctx, std_env, internal(ctx, "call/cc"),
            cdr(find_env(std_env, internal(ctx, g_opcodes[OP_CALLCC].name))));
    c = ischeme_port_new(ctx, PORT_INPUT_FILE, ISC_LIB_DIR "/init.isc");
    ischeme_eval(ctx, c);
    ischeme_port_close(ctx, c);
    gc_release(ctx);
    return ctx;
}

Cell *ischeme_port_new(Cell *ctx, PortType pt, const char *src) {
    Cell *port = NULL;
    if (pt & PORT_STDIO) {
        if (pt == PORT_STDIN)
            port = mk_port(ctx, stdin, "stdin", pt);
        else if (pt == PORT_STDOUT)
            port = mk_port(ctx, stdout, "stdout", pt);
    } else if (pt & PORT_STRING) {
        port = mk_port(ctx, const_cast<char*>(src), const_cast<char*>(src) + strlen(src), pt);
    } else if (pt & PORT_FILE) {
        if (!access(src, F_OK | R_OK)) {
            FILE *fp = fopen(src, "r");
            if (!fp) {
                IError("cant't open file '%s'", src);
                return NULL;
            }
            port = mk_port(ctx, fp, src, pt);
        }
    }
    return port; 
}

void ischeme_port_close(Cell *ctx, Cell *port) {
    if (is_port(port) && (port_type(port) & PORT_FILE)) {
        fclose(port_file(port));
        port_type(port) = PORT_FREE;
    }
}

Cell *ischeme_eval(Cell *ctx, Cell *port) {
    if (!ctx || !port || !(port_type(port) & PORT_INPUT)) {
        IError("invalid parameters");
        return NULL;
    }
    ctx_inports_push(ctx, port);
    ctx_inport(ctx) = port;
    isc_repl(ctx);
    ctx_inports_pop(ctx);
    return CELL_NIL;
}

Cell *ischeme_eval_string(Cell *ctx, const char *expr) {
    if (!ctx || !expr) {
        IError("invalid parameters");
        return NULL;
    }
    Cell *port = ischeme_port_new(ctx, PORT_INPUT_STRING, expr);
    return ischeme_eval(ctx, port);
}

Cell *ischeme_eval_file(Cell *ctx, const char *file) {
    if (!ctx || !file) {
        IError("invalid parameters");
        return NULL;
    }
    Cell *port = ischeme_port_new(ctx, PORT_INPUT_FILE, file);
    return ischeme_eval(ctx, port);
}
void ischeme_print(Cell *ctx, Cell *c) {
    print_cell(ctx, ctx_stdoutport(ctx), c);
}

void ischeme_print_to(Cell *ctx, Cell *c, Cell *port) {
    print_cell(ctx, port, c);
}

void ischeme_free(Cell *c) {
    // TODO:free
}

#ifdef __cplusplus
}
#endif
