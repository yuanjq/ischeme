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
#define CELL_ERR        (Cell*)-1
#define DELIMITERS      "()[]{}\";\f\t\v\n\r "

#define gotoOp(sc, o)      	{ sc->op=o; return CELL_TRUE; }
#define popOp(sc, r)       	return pop_op(sc, r)
#define pushOp(sc,o,a,e)   	push_op(sc, o, a, e)
#define Error(sc,f,...)	    return error_helper(sc,f,##__VA_ARGS__)
#define closure_code(c)     car(c)
#define closure_env(c)      cdr(c)


static IScheme *gp_isc = NULL;

static Cell* op_func0(IScheme*, int);
static Cell* op_func1(IScheme*, int);
static Cell* op_func2(IScheme*, int);
static Cell* op_func3(IScheme*, int);
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

static void gc(IScheme *isc);//, Cell *args, Cell *env);
static void print_cell(IScheme *isc, Cell *c);
static Cell* mk_envir(Cell **env, Cell *s, Cell *v);
static Cell *assq(Cell *key, Cell *list);


/************** memery manager ************/
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
    --gp_isc->free_cell_count;
    return c;
}

static void gc(IScheme *isc)//, Cell *args, Cell *env)
{
    ITraceEnter();
    // TODO:
    ITraceLeave();
}


/*************** syntax **************/
static bool is_char(Cell *c)     { return c && T(c) == CHAR; }
static bool is_number(Cell *c)  { return c && T(c) == NUMBER; }
static bool is_string(Cell *c)  { return c && T(c) == STRING; }
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
           isc->load_files[0]->port->t & PORT_FILE &&
           isc->load_files[0]->port->f.file == stdin;
}

static String symbol(Cell *c)   { return is_symbol(c) ? c->str: NULL; }
static String string(Cell *c)   { return is_string(c) ? c->str: NULL; }
static Port* port(Cell *c)       { return is_port(c) ? c->port : NULL; }

static Cell *car(Cell *c)        { return (is_pair(c) || is_proc(c) || is_macro(c) || is_promise(c)) ? c->pair->a : NULL; }
static Cell *cdr(Cell *c)        { return (is_pair(c) || is_proc(c) || is_macro(c) || is_promise(c)) ? c->pair->d : NULL; }
static Cell *caar(Cell *c)       { return car(car(c)); }
static Cell *cadr(Cell *c)       { return car(cdr(c)); }
static Cell *cdar(Cell *c)       { return cdr(car(c)); }
static Cell *cddr(Cell *c)       { return cdr(cdr(c)); }

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
        c->str = string_dup(s);
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

static Cell *mk_port(FILE *f, String name, int t)
{
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

int write_char(IScheme *isc, int c) {
	Port *pt = isc->outport->port;
	if(pt->t & PORT_FILE) {
		return fputc(c, pt->f.file);
	} else {
		if(pt->s.end <= pt->s.cur) {
           IError("write char out of range.\n");
           return -1;
        }
		*pt->s.cur++ = c;
        return c;
	}
    IError("invalid out port.\n");
    return -1;
}

int write_string(IScheme *isc, String s) {
	Port *pt= isc->outport->port;
	int len = strlen(s);
	if (pt->t & PORT_FILE) {
		return fwrite(s, 1, len, pt->f.file);
	} else if (pt->t & PORT_STRING) {
	    if (len > pt->s.end - pt->s.cur) {
           IError("write string out of range.\n");
           return -1; 
        }
		for(; len; len--) *pt->s.cur++ = *s++;
        return len;
	}
    IError("invalid out port.\n");
    return -1;
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
        return TOK_ATOM;
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
            return TOK_CONST;
        }
        IError("bad syntax '#%c'.\n", c);
        return TOK_EOF;
    case ';':
        c = skip_line(isc);
        if (c == EOF) return TOK_EOF;
        return get_token(isc);
    default:
        unget_char(isc->inport, c);
        return TOK_ATOM;
    }
}

static Cell *read_cell(IScheme *isc)
{
    int t = get_token(isc);
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

static Cell *error_helper(IScheme *isc, String fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);

    Port *in = isc->load_files[isc->cur_file_idx]->port;
    Port *out = isc->outport->port;
    fprintf(out->f.file, "*Error*: ");
    if (in->t & PORT_FILE && in->f.file != stdin) {
        fprintf(out->f.file, "file \"%s\", line %d\n", out->f.name, out->f.curr_line);
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
            c->conti = conti;
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
    IMessage("[F:%s][L:%d][O:%d]", __FUNCTION__, __LINE__, op);
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
            isc->envir = isc->global_envir;
            isc->contis = CELL_NIL;
            write_string(isc, "\n>> ");
        }
        pushOp(isc, OP_REPL_LOOP, isc->args, isc->envir);
        pushOp(isc, OP_REPL_PRINT, isc->args, isc->envir);
        pushOp(isc, OP_REPL_EVAL, isc->args, isc->envir);
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
            print_cell(isc, isc->retnv);
        }
        popOp(isc, isc->retnv);
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
        pushOp(isc, OP_DEF1, CELL_NIL, c);
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
            popOp(isc, isc->code->proc(isc, isc->args));
        } else if (is_proc(isc->code) || is_macro(isc->code) || is_promise(isc->code)) {
            Cell *env = closure_env(isc->code);
            Cell *fp = car(closure_code(isc->code));
            Cell *ap = isc->args;
            for (; is_pair(fp); fp = cdr(fp), ap = cdr(ap)) {
                if (ap == CELL_NIL) Error(isc, "too few arguments."); 
                mk_envir(&env, car(fp), car(ap));
            }
            if (fp == CELL_NIL && ap != CELL_NIL) Error(isc, "too much arguments.");
            if (is_symbol(fp)) mk_envir(&env, fp, ap);
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
        if (!strcmp(s, symbol(caar(list))))
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
        *env = cons(e, *env);
    }
    return e;
}

static Cell *read_illegal(IScheme *isc, int c)
{
    return CELL_EOF;
}

static inline int read_upto(Cell *port, char *upto, char *out, int size)
{
    if (!is_port(port) || !out || size <= 0)
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
    return p - out;
}

static bool start_with(char *s, char *w)
{
    char *p1, *p2;
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

static Number *read_real(char **ppstart, char *pend, Exactness exact, Radix radix)
{
    Number *number = NULL;
    char *p = *ppstart;
    int c = *p;
    uint8 sign = 0;
    int buflen = 0;
    bool must_number = (exact != NO_EXACTNESS || radix != NO_RADIX);
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
                goto IsSym;
            }
            find_dot = TRUE;
        } else if (c == '/') {
            if (find_dot || find_slash || !find_num) {
                goto IsSym;
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
                    goto IsSym;
                }
                break;
            case OCT:
                if (!(c >= '0' && c <= '7')) {
                    goto IsSym;
                }
                break;
            case DEC:
                if (!(c >= '0' && c <= '9')) {
                    goto IsSym;
                }
                break;
            case HEX:
                if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
                    goto IsSym;
                }
                break;
            default:
                if (!(c >= '0' && c <= '9')) {
                    goto IsSym;
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
            goto IsSym;
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
            goto IsSym;
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
    goto Exit;
IsSym:
    if (must_number) {
        goto Error;
    }
    goto Exit;
Error:
    number = (Number*)-1;
Exit:
    if (buf) {
        free(buf);
    }
    return number;
}

static String cell_to_string(IScheme *isc, Cell *c)
{
	String s = isc->buff;
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
		snprintf(s, STR_BUF_SIZE, "#<SYMBOL:%s>", symbol(c));
	} else if (is_macro(c)) {
		snprintf(s, STR_BUF_SIZE, "#<MACRO:%p>", c);
	} else if (is_promise(c)) {
		snprintf(s, STR_BUF_SIZE, "#<PROMISE:%p>", c);
	} else if (is_proc(c) || is_iproc(c) || is_eproc(c)) {
		snprintf(s, STR_BUF_SIZE, "#<PROCEDURE:%p>", c);
	} else if (is_conti(c)) {
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
		snprintf(s, STR_BUF_SIZE, "\"%s\"", string(c));
	} else if (is_char(c)) {
		switch(c->chr) {
		case ' ':
			s = "#\\space"; break;
		case '\n':
			s = "#\\newline"; break;
		case '\r':
			s = "#\\return"; break;
		case '\t':
			s =  "#\\tab"; break;
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
	} else {
        snprintf(s, STR_BUF_SIZE, "unknow:%p", c);
    }
    return s;
}

static void print_cell(IScheme *isc, Cell *c)
{
    write_string(isc, cell_to_string(isc, c));
}

static Cell *read_atom(IScheme *isc, int c)
{
    Number *real = NULL, *imag = NULL;
    char *p = isc->buff;
    int totalLen = read_upto(isc->inport, DELIMITERS, p, STR_BUF_SIZE);

    if (totalLen <= 0) {
        IError("read error.");
        return CELL_EOF;
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
        case 't': case 'T':
            if (exactness != NO_EXACTNESS || radix != NO_RADIX || p + 2 != pEnd)
                goto Error0;
            return CELL_TRUE;
        case '\\':
            if (exactness != NO_EXACTNESS || radix != NO_RADIX || p + 2 == pEnd)
                goto Error0;
            p += 2;
            if (pEnd - p > 1) {
                if (pEnd - p == 3 && !strncasecmp(p, "tab", 3)) {
                    return mk_char('\t');
                } else if (pEnd - p == 4 && !strncasecmp(p, "space", 4)) {
                    return mk_char(' ');
                } else if (pEnd - p == 6 && !strncasecmp(p, "return", 6)) {
                    return mk_char('\r');
                } else if (pEnd - p == 7 && !strncasecmp(p, "newline", 7)) {
                    return mk_char('\n');
                }
                goto Error0;
            }
            return mk_char(p[2]);
        default:
            goto Error0;
        }
        p += 2;
    }

    if (p < pEnd) {
        printf("p:%s\n", p);
        real = read_real(&p, pEnd, exactness, radix);
        printf("real:%p\n", real);
        if (real < 0 || p > pEnd)
            goto Error1;
        else if (real == NULL)
            goto MkSym;
        printf("num t:%d\n", real->t);
        if (p == pEnd) {
            return mk_number(real);
        } else {
            c = *p;
            if (c == '+' || c == '-') {
                imag = read_real(&p, pEnd, exactness, radix);
                printf("num2 t:%d\n", imag->t);
                 if (imag < 0 || p > pEnd)
                    goto Error1;
                else if (imag == NULL)
                    goto MkSym;
                c = *p;
                if (p != pEnd - 1 || (c != 'i' && c != 'I')) {
                    goto MkSym;
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
                    goto MkSym;
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
                goto MkSym;
            }
        }
    }
    goto Error1;

MkSym:
    if (real > 0) free(real);
    if (imag > 0) free(imag);
    return mk_symbol(isc->buff);
Error0:
    IError("bad syntax '%s'.\n", p);
    return CELL_EOF;
Error1:
    if (real > 0) free(real);
    if (imag > 0) free(imag);
    IError("read error.\n");
    return CELL_EOF;
}

static Cell *read_string(IScheme *isc, int q)
{
    char *buf = isc->buff;
    Cell *port = isc->inport;
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
    return mk_string(string_dup(buf));
}

static Cell *read_quote(IScheme *isc, int c)
{
    Cell *cell = read_cell(isc);
    if (cell == CELL_EOF)
        return CELL_EOF;
    cell = cons(isc->sym_quote, cell);
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
    case TOK_LPAREN:    c = TOK_RPAREN; break;
    case TOK_LBRACKET:  c = TOK_RBRACKET; break;
    case TOK_LBRACE:    c = TOK_RBRACE; break;
    }

    int d;
    Cell *port = isc->inport;
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
    memset(gp_isc, 0x0, sizeof(IScheme));
    gp_isc->last_seg = -1;
    
    seg_alloc(gp_isc, 3);
    init_readers();

    gp_isc->inport = mk_port(in, name, PORT_FILE | PORT_INPUT);
    gp_isc->outport = mk_port(stdout, NULL, PORT_FILE | PORT_OUTPUT);
    gp_isc->sym_lambda = internal(gp_isc, "lambda");
    gp_isc->sym_quote = internal(gp_isc, "quote");
    gp_isc->load_files[0] = gp_isc->inport;
        
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
            printf("iScheme v0.1 by yuanjq\n");
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
        printf("iScheme v0.1 by yuanjq\n");
    }
    isc_init(in, name);
    isc_repl();
    isc_finalize();
    return 0;
}
