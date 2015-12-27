#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include "ischeme.h"

#define CELL_TRUE       &g_true
#define CELL_FALSE      &g_false
#define CELL_NIL        &g_nil
#define CELL_EOF        &g_eof
#define CELL_ERR        (Cell*)-1
#define DELIMITERS      "()[]{}\";\f\t\v\n\r "

#define gotoOp(sc, o)      	sc->op=o; return CELL_TRUE
#define popOp(sc, r)       	return pop_op(sc, r)
#define pushOp(sc,o,a,e)   	push_op(sc, o, a, e)
#define Error(sc,f,...)	    return error_helper(sc,f,##__VA_ARGS__)
#define closure_code(c)     car(c)
#define closure_env(c)      cdr(c)


static IScheme *gp_isc = NULL;
static Cell* op_func0(IScheme*, int);
static OpCode g_opcodes[] = {
    #define _OPCODE(f, n, t, o) {f, n, t},
    #include "opcodes.h"
    #undef _OPCODE
    {0}
};

static Reader g_readers[TOK_MAX];
static Cell g_true = {t:BOOLEAN, {chr:TRUE}};
static Cell g_false = {t:BOOLEAN, {chr:FALSE}};
static Cell g_nil;
static Cell g_eof;

static void gc(IScheme *isc);//, Cell *args, Cell *env);


/************** memery manager ************/
static int seg_alloc(IScheme *isc, int num)
{
    if (isc->last_seg + num >= SEGS_NUM) return 0;

    for (int i=0; i<num; i++)
    {
        int idx = ++isc->last_seg;
        isc->segs[idx] = (Cell*)malloc(SEG_MEM_SIZE);
        Cell *pNew = isc->segs[idx];
        isc->free_cell_count += SEG_CELLS_NUM;

        Cell *pLast = pNew + SEG_CELLS_NUM - 1;
        for (Cell *c = pNew; c <= pLast; c++) c->next = c + 1;

        pLast->next = isc->free_cells;
        isc->free_cells = pNew;
    }

    return num;
}

static Cell *cell_alloc()//Cell *args, Cell *env)
{
    if (!gp_isc->free_cells) {
        gc(gp_isc);//, args, env);
        if (!gp_isc->free_cells && seg_alloc(gp_isc, 1) <= 0) {
            IError("no memery.");
            return NULL;
        }
    }
    Cell *c = gp_isc->free_cells;
    gp_isc->free_cells = c->next;
    gp_isc->free_cell_count--;
    return c;
}

static void gc(IScheme *isc)//, Cell *args, Cell *env)
{
    ITraceEnter();
    // TODO:
    ITraceLeave();
}


/*************** syntax **************/
static bool is_pair(Cell *c)     { return c && T(c) == PAIR; }
static bool is_symbol(Cell *c)  { return c && T(c) == SYMBOL; }
static bool is_syntax(Cell *c)  { return c && T(c) == SYNTAX; }
static bool is_proc(Cell *c)     { return c && T(c) == PROC; }
static bool is_iproc(Cell *c)   { return c && T(c) == IPROC; }
static bool is_eproc(Cell *c)   { return c && T(c) == EPROC; }
static bool is_macro(Cell *c)   { return c && T(c) == MACRO; }
static bool is_promise(Cell *c)   { return c && T(c) == PROMISE; }

static bool is_port(Cell *c)     { return c && T(c) == PORT; }
static bool is_conti(Cell *c)    { return c && T(c) == CONTI; }
static bool is_port_eof(Cell *c) { return c && T(c) == PORT && c->port && c->port->t & PORT_EOF; }

static bool is_immutable(Cell *c)   { return c && c->t & M_IMMUTABLE; }
static bool is_interactive(IScheme *isc) {
    return isc->cur_file_idx == 0 &&
           isc->load_files[0]->t & PORT_FILE &&
           isc->load_files[0]->port->f.file == stdin;
}

static String symbol(Cell *c)    { return is_symbol(c) ? c->str: NULL; }
static Port* port(Cell *c)       { return is_port(c) ? c->port : NULL; }

static Cell *car(Cell *c)        { return (is_pair(c) || is_proc(c) || is_macro(c) || is_promise(c)) ? c->pair.a : NULL; }
static Cell *cdr(Cell *c)        { return (is_pair(c) || is_proc(c) || is_macro(c) || is_promise(c)) ? c->pair.d : NULL; }
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

static Cell *mk_syntax(int op)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = SYNTAX;
        c->chr = op;
    }
    return c;
}

static Cell *mk_iproc(int op)
{
    Cell *c = cell_alloc();
    if (c) {
        c->t = IPROC;
        c->chr = op;
    }
    return c;
}

static Cell *mk_conti(IScheme *isc)
{
    return 0;
}


/***************** repl loop ******************/
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
		if(name) isc->load_files[isc->cur_file_idx]->port->f.name = strdup(name);
		isc->in_port= isc->load_files + isc->cur_file_idx;
	}
	return fin != 0;
}

static void pop_load_file(IScheme *isc) {
	if(isc->cur_file_idx != 0) {
		port_close(isc, isc->in_port, PORT_INPUT);
		isc->cur_file_idx--;
		isc->in_port = isc->load_files + isc->cur_file_idx;
	}
}

void write_char(IScheme *isc, int c) {
	Port *pt = isc->out_port->port;
	if(pt->t & PORT_FILE) {
		fputc(c, pt->f.file);
	} else {
		if(pt->s.cur < pt->s.end) {
			*pt->s.cur++ = c;
		} else {
		    IError("write char out of range.");
		}
	}
}

void write_string(IScheme *isc, String s) {
	Port *pt= isc->out_port->port;
	int len = strlen(s);
	if(pt->t & PORT_FILE) {
		fwrite(s, 1, len, pt->f.file);
	} else {
		for(; len; len--) {
			if(pt->s.cur < pt->s.end) {
				*pt->s.curr++=*s++;
			} else {
                IError("write string out of range.");
			}
		}
	}
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
        if (p->s.cur != p->s.start) p->s.cur--;
    }
}

static inline int skip_line(IScheme *isc) {
    int c = 0, n = 0;
    while (c = get_char(isc->in_port) != EOF && c != '\n') ++n;
    if (c == '\n') {
        if (isc->load_files[isc->cur_file_idx] && isc->load_files[isc->cur_file_idx]->port->t & PORT_FILE)
            ++isc->load_files[isc->cur_file_idx]->port->f.curLine;
        return ++n;
    }
    return EOF;
}

static int skip_comment(IScheme *isc) {
    int n = 0;
    int c = get_char(isc->in_port);
    if (c == EOF) return EOF;
    if (c == ';') {
        ++n;
        c = skip_line(isc);
        if (c == EOF) return EOF;
        n += c;
    } else if (c == '#') {
        c = get_char(isc->in_port);
        if (c == EOF) return EOF;
        if (c != '!')  {
            unget_char(isc->in_port, c);
            unget_char(isc->in_port, '#');
            return 0;
        }
        n += 2;
        c = skip_line(isc);
        if (c == EOF) return EOF;
        n += c;
    } else {
        unget_char(isc->in_port, c);
        return 0;
    }
    if (c == '\n') return ++n;
    return EOF;
}

static int skip_space(IScheme *isc) {
    int c = 0, n = 0, curLine = 0;
    do {
        c = get_char(isc->in_port);
        if (c == '\n') ++curLine;
        else if (c == ';' || c == '#') {
            unget_char(isc->in_port, c);
            c = skip_comment(isc);
            if (c <= 0) return n;
            n += c;
            continue;
        }
        ++n;
    } while(isspace(c));
    if (isc->load_files[isc->cur_file_idx] && isc->load_files[isc->cur_file_idx]->port->t & PORT_FILE)
        isc->load_files[isc->cur_file_idx]->port->f.curLine += curLine;
    if (c != EOF) {
        unget_char(isc->in_port, c);
        return --n;
    }
    return EOF;
}

static Cell *reverse(IScheme *isc, Cell *old) {
    Cell *new = CELL_NIL;
    for (; is_pair(old); old = cdr(old))
        new = cons(isc, car(old), new);
    return new;
}

static int get_token(IScheme *isc) {
    int c = skip_space(isc);
    if (c == EOF) return TOK_EOF;
    switch (c = get_char(isc->in_port)) {
    case EOF: return TOK_EOF;
    case '(': return TOK_LPAREN;
    case ')': return TOK_RPAREN;
    case '[': return TOK_LBRACKET;
    case ']': return TOK_RBRACKET;
    case '{': return TOK_LBRACE;
    case '}': return TOK_RBRACE;
    case '.':
        if (skip_space(isc) > 0) return TOK_DOT;
        unget_char(isc->in_port, c);
        return TOK_ATOM;
    case '\'': return TOK_QUOTE;
    case '`': return TOK_QQUOTE;
    case '"': return TOK_DQUOTE;
    case ',':
        c = get_char(isc->in_port);
        if (c == '@') return TOK_UNQUOTE_SPLICING;
        else unget_char(isc->in_port, c);
        return TOK_UNQUOTE;
    case '#':
        c = get_char(isc->in_port);
        if (c == '(') return TOK_VECTOR;
        else if (c == '!') {
            c = skip_line(isc);
            if (c == EOF) return TOK_EOF;
            return get_token(isc);
        } else if (strchr("tfeibodx\\", c)) {
            unget_char(isc->in_port, c);
            return TOK_CONST;
        }
        IError("bad syntax '#%c'.\n", c);
        return TOK_EOF;
    case ';':
        c = skip_line(isc);
        if (c == EOF) return TOK_EOF;
        return get_token(isc);
    default:
        unget_char(isc->in_port, c);
        return TOK_ATOM;
    }
}

static Cell *read_cell(IScheme *isc)
{
    int t = get_token(isc->in_port);
    if (t == TOK_EOF) {
        IError("end of file.");
        return CELL_EOF;
    }
    return g_readers[t](isc, t);
}

static Cell *read_cell_by_token(IScheme *isc, int t)
{
    if (t == TOK_EOF) {
        IError("end of file.");
        return CELL_EOF;
    }
    return g_readers[t](isc, t);
}

static inline Cell *pop_op(IScheme *isc, Cell *v)
{
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

static Cell *error_helper(IScheme *isc, String fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);

    Port *in = isc->load_files[isc->cur_file_idx]->port;
    Port *out = isc->out_port->port;
    fprintf(out->f.file, "*Error*: ");
    if (in->t & PORT_FILE && in->f.file != stdin) {
        fprintf(out->f.file, "file \"%s\", line %d\n", out->f.name, out->f.curLine);
    }
    vfprintf(out->f.file, fmt, ap);
    va_end(ap);
    isc->op = OP_ERROR;
    return CELL_TRUE;
}

static void push_op(IScheme *isc, Op op, Cell *args, Cell *code)
{
    Conti *conti = (Conti*)malloc(sizeof(Conti));
    if (conti) {
        Cell *c = cell_alloc();
        if (c) {
            c->t = CONTI;
            c->conti->op = op;
            c->conti->args = args;
            c->conti->envir = isc->envir;
            c->conti->code = code;
        }
        isc->contis = cons(c, isc->contis);
    }
}

static Cell *op_func0(IScheme *isc, int op)
{
    Cell *c;
    switch (op) {
    case OP_REPL_LOOP:
        if (isc->in_port->port->t & PORT_EOF) {
            if (isc->cur_file_idx == 0)
                return CELL_FALSE;
            pop_load_file(isc);
            popOp(isc, isc->retnv);
        }
        if (is_interactive(isc)) {
            isc->envir = isc->global_envir;
            isc->contis = CELL_NIL;
            write_string("\n>> ");
        }
        pushOp(isc, OP_REPL_LOOP, isc->args, isc->envir);
        pushOp(isc, OP_REPL_PRINT, isc->args, isc->envir);
        pushOp(isc, OP_REPL_EVAL, isc->args, isc->envir);
        gotoOp(isc, OP_REPL_READ);
    case OP_REPL_READ:
        popOp(isc, read_cell(isc));
    case OP_REPL_EVAL:
        isc->code = isc->retnv;
        gotoOp(isc, OP_EVAL);
    case OP_REPL_PRINT:
        if (is_interactive(isc)) {
            
        } else {
            popOp(isc, isc->retnv);
        }
        break;
    case OP_DEF0:
        if (!is_pair(isc->code))
            Error(isc, "missing expression after identifier.");
        if (is_immutable(car(isc->code)))
            Error(isc, "unable to alter immutable atom.");
        if (is_pair(c = car(isc->code))) {
            Cell *e = cadr(isc->code);
            for (; is_pair(c); c = car(c)) {
                e = cons(isc->sym_lambda, cons(cadr(c), e));
            }
            isc->code = e;
        } else if (is_symbol(c = car(isc->code))) {
            isc->code = cadr(isc->code);
        } else {
            Error(isc, "invalid define expression.");
        }
        pushOp(isc, OP_DEF1, CELL_NIL, c)
        gotoOp(isc, OP_EVAL);
    case OP_DEF1:
        mk_envir(&isc->envir, isc->code, isc->retnv);
        popOp(isc, CELL_NIL);
    case OP_LAMBDA:
        popOp(isc, mk_lambda(isc->code, isc->envir));        
    case OP_EVAL:
        if (is_symbol(isc->code)) {
            c = assq(isc->code, isc->envir);
            if (is_pair(c)) popOp(isc, cdr(c));
            else Error(isc, "unbound variable:%s\n", symbol(isc->code));
        } else if (is_pair(isc->code)) {
            if (is_syntax(c = car(isc->code))) {
                isc->code = cdr(isc->code);
                gotoOp(isc, c->chr);
            } else {
                pushOp(isc, OP_EVAL_OPC, CELL_NIL, isc->code);
                isc->code = car(isc->code);
                gotoOp(isc, OP_EVAL);
            }
        } else{
            popOp(isc, isc->code);
        }
        break;
    case OP_EVAL_OPC:
        if (is_macro(isc->retnv)) {
            // TODO
        } else {
            isc->code = cdr(isc->code);
            gotoOp(isc, OP_EVAL_ARGS);
        }
        break;
    case OP_EVAL_ARGS:
        isc->args = cons(isc, isc->retnv, isc->args);
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
            popOp(isc, isc->code->proc(isc, isc->args));
        } else if (is_proc(isc->code) || is_macro(isc->code) || is_promise(isc->code)) {
            Cell **env = &closure_env(isc->code);
            Cell *fp = car(closure_code(isc->code));
            Cell *ap = isc->args;
            for (; is_pair(fp); fp = cdr(fp), ap = cdr(ap)) {
                if (ap == CELL_NIL) Error(isc, "too few arguments."); 
                mk_envir(env, car(fp), car(ap));
            }
            if (fp == CELL_NIL && ap != CELL_NIL) Error(isc, "too much arguments.");
            if (is_symbol(fp)) mk_envir(env, fp, ap);
            isc->code = cdr(closure_code(isc->code));
            isc->args = CELL_NIL;
            gotoOp(isc, OP_EVAL_LIST);
        } else if (is_conti(isc->code)) {
            popOp(isc, isc->args != CELL_NIL ? car(isc->args) : CELL_NIL);
        } 
        Error(isc, "illegal procudure.");
    case OP_EVAL_LIST:
        if (!is_pair(isc->code)) popOp(isc, isc->code);
        if (cdr(isc->code) != CELL_NIL) pushOp(isc, OP_EVAL_LIST, CELL_NIL, cdr(isc->code));
        isc->code = car(isc->code);
        gotoOp(isc, OP_EVAL);
    case OP_ERROR:
        gotoOp(isc, OP_REPL_LOOP);
    }
    return CELL_TRUE;
}

static Cell *op_func1(IScheme *isc, int op)
{
    switch (op) {
    case OP_LOAD:
        break;

    }
    return CELL_EOF;
}

static Cell *op_func2(IScheme *isc, int op)
{
    switch (op) {

    }
    return CELL_EOF;
}

static Cell *op_func3(IScheme *isc, int op)
{
    switch (op) {

    }
    return CELL_EOF;
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
	String s = symbol(key);
    for (; is_pair(list); list = cdr(list)) {
        if (!strcmp(s, symbol(caar(list)))
            return car(list);
    }
    return NULL;
}

static Cell *find_envir(Cell *env, Cell *s)
{
    for (Cell *e = env; is_pair(e); e = cdr(e)) {
        if (is_pair(car(e)) && caar(e) == s)
            return car(e);
    }
    return CELL_NIL;
}

static Cell* mk_envir(Cell **env, Cell *s, Cell *v)
{
    Cell *e = NULL;
    if ((e = find_envir(*env, s))) {
        rplacd(e, v);
    } else {
        e = cons(s, v);
        **env = cons(e, *env);
    }
    return e;
}

static Cell *read_illegal(IScheme *isc, int c)
{
    return CELL_EOF;
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

    if (pEnd - p >= STR_BUF_SIZE)
        goto Error;
    pBuf = malloc(STR_BUF_SIZE);
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

static Cell *read_atom(IScheme *isc, int c)
{
    Number *real = NULL;
    unget_char(isc->in_port, c);
    char *p = isc->buff;
    int totalLen = read_upto(isc->in_port, p, DELIMITERS);

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
            if (exactness != NO_EXACTNESS)
                goto Error0;
            exactness = EXACT;
            break;
        case 'i': case 'I':
            if (exactness != NO_EXACTNESS)
                goto Error0;
            exactness = INEXACT;
            break;
        case 'f': case 'F':
            if (exactness != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pEnd)
                goto Error0;
            return CELL_FALSE;
        case 't': case "T":
            if (exactness != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pEnd)
                goto Error0;
            return CELL_TRUE;
        case '\\':
            if (exactness != NO_EXACTNESS || radix != NO_RADIX || p + 3 != pEnd)
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
                return mk_symbol(isc->buff);
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
                num->cx.rl = 0;
                num->cx.im = real;
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
    return mk_symbol(isc->buff);
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

static Cell *read_string(IScheme *isc, int q)
{
    char *buf = isc->buff;
    Cell *port = isc->in_port;
    int idx = 0;
    int c;
    while (idx < STR_BUF_SIZE && (c = get_char(port)) > 0 && c != q)
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

static Cell *read_unquote_splicing(IScheme *isc, int c)
{
    return 0;
}

static Cell *read_const(IScheme *isc, int c)
{
    return 0;
}

static Cell *read_vector(IScheme *isc, int c)
{
    return 0;
}

static Cell *read_list(IScheme *isc, int c)
{
    Cell *head, *tail, *cell = CELL_NIL;
    head = tail = cons(CELL_NIL, CELL_NIL);

    switch (c) {
    case '(': c = TOK_RPAREN; break;
    case '[': c = TOK_RBRACKET; break;
    case '{': c = TOK_RBRACE; break;
    }

    int d;
    Cell *port = isc->in_port;
    for (;;) {
        d = get_token(isc);
        if (d == TOK_EOF) {
            IError("end of file.");
            return CELL_EOF;
        }
        if (c == d) break;
        if (d == TOK_RPAREN || d == TOK_RBRACKET || d == TOK_RBRACE) {
            IError("unmatched brackets.");
            return CELL_EOF;
        }

        if (d == TOK_DOT) {
            if (cell == CELL_NIL) {
                IError("illegal used of dot.");
                return CELL_EOF;
            }
            cell = read_cell(isc);
            if (cell == CELL_EOF)
                return CELL_EOF;
            tail = rplacd(tail, cell);
            c = get_token(isc);
            if (c != TOK_RPAREN) {
                IError("illegal used of dot.");
                return CELL_EOF;
            }
        }
        cell = read_cell_by_token(isc, d);
        if (cell == CELL_EOF)
            return CELL_EOF;
        tail = rplacd(tail, cons(cell, CELL_NIL));
    }
    return cdr(head);
}

static void init_readers()
{
    for (int i = 0;  i < TOK_MAX;  i++) g_readers[i]= read_illegal;
    g_readers[TOK_ATOM]     = read_atom;
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

static void isc_init(FILE *in, String name)
{
    gp_isc = malloc(sizeof(IScheme));
    if (!gp_isc) {
        IError("no memory.");
        return;
    }
    
    gp_isc->free_cell_count = 0;
    gp_isc->free_cells = NULL;
    gp_isc->last_seg = -1;
    gp_isc->global_envir = NULL;

    gp_isc->in_port = mk_port(in, name);
    gp_isc->out_port = mk_port(stdout, NULL);
    gp_isc->sym_lambda = internal(gp_isc, "lambda");
    
    init_readers();
    seg_alloc(gp_isc, 3);

    Cell *c = CELL_NIL;
    for (int i = 0; i < sizeof(g_opcodes)/sizeof(OpCode); i++) {
        if (g_opcodes[i].name) {
            if (g_opcodes[i].t == SYNTAX) c = mk_syntax(i);
            else c = mk_iproc(i);
            mk_envir(&gp_isc->global_envir, internal(gp_isc, g_opcodes[i].name), c);
        }
    }
}

static void isc_repl()
{
    gp_isc->op = OP_REPL_LOOP;
    for (;;) {
        if (g_opcodes[gp_isc->op].func(gp_isc, gp_isc->op) != CELL_TRUE)
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
        if (!strcmp(argv[1], "-h")  || !strcmp(argv[1], "--help")) {
            printf("iScheme v0.1 by yjq\n");
            printf("Options and arguments:\n");
            printf("-\t\t: program read from stdin\n");
            printf("file\t\t: program read from file");
            printf("-h\t\t: print this help message and exit (also --help)");
            printf("-v\t\t: iScheme's version (also --version)");
            return 0;
        } else if (!strcmp(argv[1], "-v") || strcmp(argv[1], "--version")) {
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
        printf("iScheme v0.1 by yjq\n");
    }
    isc_init(in, name);
    isc_repl();
    isc_finalize();
    return 0;
}
