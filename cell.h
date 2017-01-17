#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string>
#include <vector>

using std::vector;

#define IDEBUG_MORE

#ifdef  IDEBUG_MORE
#define IMessage(fmt, ...)      printf("*Message*: " fmt "\n", ##__VA_ARGS__)
#define IWarning(fmt, ...)      printf("*Warning*: " fmt "\n", ##__VA_ARGS__)
#define IError(fmt, ...)        printf("*Error*: " fmt "\n", ##__VA_ARGS__)
#define ITraceEnter()           IMessage("Func %s enter.", __FUNCTION__)
#define ITraceLeave()           IMessage("Func %s leave.", __FUNCTION__)
#else
#define IMessage(fmt, ...)
#define IWarning(fmt, ...)
#define IError(fmt, ...)
#define ITraceEnter()
#define ITraceLeave()
#endif

typedef unsigned char           uchar;
typedef unsigned short          ushort;
typedef unsigned int            uint;
typedef unsigned long           ulong;

#define TRUE                    1
#define FALSE                   0
#define ISC_SEG_NUM             32
#define ISC_SEG_SIZE            (8*1024*1024)
#define ISC_SEG_INIT_SIZE       ISC_SEG_SIZE
#define ISC_SEG_MAX_SIZE        (ISC_SEG_NUM*ISC_SEG_SIZE)
#define ISC_SEG_GROW_THRESHOLD  0.75
#define ISC_SEG_REDU_THRESHOLD  0.25
#define ISC_SEG_GROW_RATIO      1.5
#define STR_BUF_SIZE            128

#define POINTER_TAG             0xA745C39BuL
#define T_MASK                  0x00FF
#define M_MASK                  0xFF00
#define M_IMMUTABLE             0x0100
#define M_REFERENCE             0x0200
#define T(c)                    ((c)->t & T_MASK)
#define M(c)                    ((c)->t & M_MASK)

#define S(t)                    sizeof(t)
#define CSTR(s)                 const_cast<char*>(s)
#define cell_align(n, bits)     (((n)+(1<<(bits))-1)&(((ulong)-1)-((1<<(bits))-1)))
#ifdef __x86_64__
#define segment_align(n)        cell_align(n, 3)
#else
#define segment_align(n)        cell_align(n, 2)
#endif
#define segment_align_size(s)   (segment_align(S(Segment)) + segment_align(s))

#define cell_malloc             malloc
#define cell_free               free
#define cell_field(c,t,f)       ((c)->t.f)
#define cell_sizeof(x)          (offsetof(Cell, op) + S(((Cell*)0)->x))
#define cell_new(_c,_x,_t)      ({ Cell *c = (Cell*)cell_alloc(_c, cell_sizeof(_x)); if (c) { c->t = _t; c->ptrtag = POINTER_TAG; } c; })
#define cell_ptrtag(c)          ((c)->ptrtag)
#define cell_markedp(c)         ((c)->marked)

#define syntax_new(c)           cell_new(c, op, SYNTAX)
#define iproc_new(c)            cell_new(c, op, IPROC)
#define char_new(c)             cell_new(c, chr, CHAR)
#define symbol_new(c)           cell_new(c, str, SYMBOL)
#define number_new(c)           cell_new(c, num, NUMBER)
#define pair_new(c)             cell_new(c, pair, PAIR)
#define port_new(c)             cell_new(c, port, PORT)
#define instruct_new(c)         cell_new(c, inst, INSTRUCT)
#define continue_new(c)         cell_new(c, cont, CONTINUE)
#define procedure_new(c)        cell_new(c, eproc, EPROC)
#define closure_new(c)          cell_new(c, clos, CLOSURE)
#define macro_new(c)            cell_new(c, macro, MACRO)
#define closure_expr_new(c)     cell_new(c, closexpr, CLOSURE_EXPR);
#define proc_new(c)             cell_new(c, proc, PROC)
#define context_new(c)          cell_new(c, ctx, CONTEXT)
#define exception_new(c)        cell_new(c, excpt, EXCEPTION)
#define matcher_new(c)          cell_new(c, mt, MATCHER)
#define expander_new(c)         cell_new(c, expd, EXPANDER)
#define promise_new(c)          cell_new(c, proms, PROMISE)
#define multivar_new(c)         cell_new(c, multiv, MULTIVAR)

#define cell_type(c)            (c->t)
#define syntax_op(c)            (c->op)
#define iproc_op(c)             (c->op)
#define macro_matchers(c)       (cell_field(c,macro,mchs))
#define macro_env(c)            (cell_field(c,macro,env))

#define char_value(c)           (c->chr)
#define string_size(s)          (cell_field(s,str,size))
#define string_data(s)          (cell_field(s,str,data))
#define symbol_size(s)          string_size(s)
#define symbol_data(s)          string_data(s)

#define pair_car(c)             (cell_field(c,pair,a))
#define pair_cdr(c)             (cell_field(c,pair,d))

#define vector_length(c)        (cell_field(c,vect,length))
#define vector_data(c)          (cell_field(c,vect,data))

#define number_type(n)          (cell_field(n,num,t))
#define number_long(n)          (cell_field(n,num,l))
#define number_double(n)        (cell_field(n,num,d))
#define number_fn_nr(n)         (cell_field(n,num,fn.nr))
#define number_fn_dr(n)         (cell_field(n,num,fn.dr))
#define number_cx_rl(n)         (cell_field(n,num,cx.rl))
#define number_cx_im(n)         (cell_field(n,num,cx.im))

#define port_type(p)            (cell_field(p,port,t))
#define port_file(p)            (cell_field(p,port,f.file))
#define port_file_name(p)       (cell_field(p,port,f.name))
#define port_file_pos(p)        (cell_field(p,port,f.pos))
#define port_string_start(p)    (cell_field(p,port,s.start))
#define port_string_end(p)      (cell_field(p,port,s.end))
#define port_string_pos(p)      (cell_field(p,port,s.pos))

#define instruct_op(c)          (cell_field(c,inst,op))
#define instruct_args(c)        (cell_field(c,inst,args))
#define instruct_env(c)         (cell_field(c,inst,env))
#define instruct_code(c)        (cell_field(c,inst,code))
#define instruct_data(c)        (cell_field(c,inst,data))

#define continue_ins(c)         (cell_field(c,cont,ins))
#define continue_winds(c)       (cell_field(c,cont,winds))

#define closure_args(c)         (cell_field(c,clos,args))
#define closure_code(c)         (cell_field(c,clos,code))
#define closure_env(c)          (cell_field(c,clos,env))

#define closure_expr_expr(c)    (cell_field(c,closexpr,expr))
#define closure_expr_env(c)     (cell_field(c,closexpr,env))

#define proc_name(c)            (cell_field(c,proc,name))
#define proc_closure(c)         (cell_field(c,proc,clos))

#define exception_type(e)       (cell_field(e,excpt,t))
#define exception_msg(e)        (cell_field(e,excpt,msg))
#define exception_src(e)        (cell_field(e,excpt,src))
#define exception_trg(e)        (cell_field(e,excpt,trg))

#define matcher_type(m)         (cell_field(m,mt,t))
#define matcher_repeat(m)       (cell_field(m,mt,rept))
#define matcher_name(m)         (cell_field(m,mt,name))
#define matcher_value(m)        (cell_field(m,mt,value))

#define expander_type(e)        (cell_field(e,expd,t))
#define expander_n(e)           (cell_field(e,expd,n))
#define expander_name(e)        (cell_field(e,expd,name))
#define expander_value(e)       (cell_field(e,expd,value))

#define promise_result(p)       (cell_field(p,proms,ret))
#define promise_expr(p)         (cell_field(p,proms,expr))

#define multivar_n(v)           (cell_field(v,multiv,n))
#define multivar_var(v)         (cell_field(v,multiv,var))

#define ctx_segments(c)         (cell_field(c,ctx,segments))
#define ctx_global_env(c)       (cell_field(c,ctx,global_env))
#define ctx_symbols(c)          (cell_field(c,ctx,symbols))
#define ctx_inport(c)           (cell_field(c,ctx,inport))
#define ctx_outport(c)          (cell_field(c,ctx,outport))
#define ctx_inports(c)          (cell_field(c,ctx,inports))
#define ctx_inports_head(c)     (ctx_inports(c).front())
#define ctx_inports_tail(c)     (ctx_inports(c).back())
#define ctx_inports_push(c,p)   (ctx_inports(c).push_back(p))
#define ctx_inports_pop(c)      (ctx_inports(c).pop_back())
#define ctx_inports_size(c)     (ctx_inports(c).size())
#define ctx_op(c)               (cell_field(c,ctx,op))
#define ctx_ret(c)              (cell_field(c,ctx,ret))
#define ctx_args(c)             (cell_field(c,ctx,args))
#define ctx_env(c)              (cell_field(c,ctx,env))
#define ctx_code(c)             (cell_field(c,ctx,code))
#define ctx_data(c)             (cell_field(c,ctx,data))
#define ctx_instructs(c)        (cell_field(c,ctx,ins))
#define ctx_winds(c)            (cell_field(c,ctx,winds))
#define ctx_saves(c)            (cell_field(c,ctx,saves))

#define ctx_lambda(c)           (cell_field(c,ctx,lambda))
#define ctx_quote(c)            (cell_field(c,ctx,quote))
#define ctx_quasiquote(c)       (cell_field(c,ctx,qquote))
#define ctx_unquote(c)          (cell_field(c,ctx,uquote))
#define ctx_unquote_splicing(c) (cell_field(c,ctx,uquotes))
#define ctx_syntax_expr(c)      (cell_field(c,ctx,synepr))

enum Type {
    FREE = 0,
    CHAR,
    BOOLEAN,
    NUMBER,
    STRING,
    SYMBOL,
    SYNTAX,
    PAIR,
    LIST,
    VECTOR,
    PORT,
    PROC,
    IPROC,
    EPROC,
    CLOSURE,
    CONTEXT,
    MACRO,
    MATCHER,
    EXPANDER,
    CLOSURE_EXPR,
    INSTRUCT,
    CONTINUE,
    ENVIR,
    PROMISE,
    MULTIVAR,
    EXCEPTION,
};

enum Token {
    TOK_ERR = -2,
    TOK_EOF = -1,
    TOK_SYMBOL,
    TOK_LPAREN,
    TOK_LBRACKET,
    TOK_LBRACE,
    TOK_RPAREN,
    TOK_RBRACKET,
    TOK_RBRACE,
    TOK_DOT,
    TOK_ELLIPSIS,
    TOK_QUOTE,
    TOK_DQUOTE,
    TOK_QQUOTE,
    TOK_UNQUOTE,
    TOK_UNQUOTE_SPLICING,
    TOK_CONST,
    TOK_VECTOR,
    TOK_MAX
};

enum NumberType {
    NUMBER_LONG,
    NUMBER_DOUBLE,
    NUMBER_FRACTION,
    NUMBER_COMPLEX,
};

enum Exactness {
    NO_EXACTNESS,
    INEXACT,
    EXACT
};

enum Radix {
    NO_RADIX = 0,
    BIN = 2,
    OCT = 8,
    DEC = 10,
    HEX = 16
};

enum Op {
    #define _OPCODE(n, t1, o, m1, m2, t2) o,
    #include "opcodes.h"
    #undef _OPCODE
    OPCODE_MAX
};

enum ErrorType {
    SyntaxError,
    MemoryError,
    IndexError,
    IOError,
    TypeError,
    ValueError,
    ReferenceError,
    ArithmeticError,
};

enum MatcherType {
    MatcherLiteral,
    MatcherConstant,
    MatcherVariable,
    MatcherUnderscore,
    MatcherRest,
    MatcherSequence,
};

enum ExpanderType {
    ExpanderConstant,
    ExpanderVariable,
    ExpanderSequence,
};

struct Cell;
typedef Cell*(*Reader)(Cell*, int);
typedef Cell*(*EProc)(Cell*, Cell*);

struct String {
    uint size;
    char *data;
};

struct Number {
    uchar t;
    union {
        long l;
        double d;
        struct {
            Cell *nr;
            Cell *dr;
        } fn;
        struct {
            Cell *rl;
            Cell *im;
        } cx;
    };
};

struct Port {
    uint t;
    union {
        struct {
            FILE *file;
            Cell *name;
            int pos;
        } f;
        struct {
            char *start;
            char *end;
            char *pos;
        } s;
    };
};

struct Instruct {
    Op op;
    Cell *args;
    Cell *env;
    Cell *code;
    Cell *data;
};

struct Continue {
    Cell *ins;
    Cell *winds;
};

struct Pair {
    Cell *a;
    Cell *d;
};

struct Vector {
    uint length;
    Cell *data[];
};

struct Closure {
    Cell *args;
    Cell *code;
    Cell *env;
};

struct ClosureExpr {
    Cell *expr;
    Cell *env;
};

struct Proc {
    Cell *name;
    Cell *clos;
};

struct Exception {
    ErrorType t;
    Cell *msg;
    Cell *trg;
    Cell *src;
};

struct Preserved {
    Cell **var;
    Preserved *next;
};

struct Segment;
struct Context {
    Segment *segments;
    Cell *global_env;
    Cell *symbols;
    Cell *inport;
    Cell *outport;
    vector<Cell*> inports;

    Op op;
    Cell *ret;
    Cell *args;
    Cell *env;
    Cell *code;
    Cell *data;
    Cell *ins;
    Cell *winds;
    Preserved *saves;

    Cell *lambda;
    Cell *quote;
    Cell *qquote;
    Cell *uquote;
    Cell *uquotes;
    Cell *synepr;
};

struct Matcher {
    MatcherType t;
    bool rept;
    Cell *name;
    Cell *value;
};

struct Expander {
    ExpanderType t;
    uint n;
    Cell *name;
    Cell *value;
};

struct Macro {
    Cell *mchs;
    Cell *env;
};

struct Promise {
    Cell *ret;
    Cell *expr;
};

struct MultiVar {
    uint n;
    Cell *var;
};

struct Cell {
    Cell() {};
    ~Cell() {};

    ushort t;
    uchar marked;
    uint ptrtag;
    union {
        Op          op;
        uchar       chr;
        bool        bl;
        String      str;
        Number      num;
        Pair        pair;
        Vector      vect;
        Port        port;
        Instruct    inst;
        Continue    cont;
        EProc       eproc;
        Closure     clos;
        Proc        proc;
        Context     ctx;
        Exception   excpt;
        Matcher     mt;
        Expander    expd;
        Macro       macro;
        ClosureExpr closexpr;
        Promise     proms;
        MultiVar    multiv;
    };
};

struct OpCode {
    const char *name;
    unsigned char t;
    int min_args;
    int max_args;
    const char *arg_types;
};
#define DELIMITERS      "()[]{}\";\f\t\v\n\r "

#define T_ANY       	"\001"
#define T_CHAR      	"\002"
#define T_NUMBER    	"\003"
#define T_REAL      	"\004"
#define T_INTEGER   	"\005"
#define T_NATURAL   	"\006"
#define T_STRING    	"\007"
#define T_SYMBOL    	"\010"
#define T_PAIR      	"\011"
#define T_LIST      	"\012"
#define T_VECTOR    	"\013"
#define T_PROC      	"\014"
#define T_ENVIR     	"\015"
#define T_CONTI     	"\016"
#define T_PORT      	"\017"
#define T_INPORT    	"\020"
#define T_OUTPORT   	"\021"
#define T_PROMISE       "\022"

#define car(c)      	((c) && T(c) == PAIR ? (c)->pair.a : NULL)
#define cdr(c)      	((c) && T(c) == PAIR ? (c)->pair.d : NULL)
#define caar(c)     	car(car(c))
#define cadr(c)     	car(cdr(c))
#define cdar(c)     	cdr(car(c))
#define cddr(c)     	cdr(cdr(c))
#define caaar(c)    	car(car(car(c)))
#define caadr(c)    	car(car(cdr(c)))
#define cadar(c)    	car(cdr(car(c)))
#define caddr(c)    	car(cdr(cdr(c)))
#define cdaar(c)    	cdr(car(car(c)))
#define cdadr(c)    	cdr(car(cdr(c)))
#define cddar(c)    	cdr(cdr(car(c)))
#define cdddr(c)    	cdr(cdr(cdr(c)))

#define is_nil(c) 		((c) == CELL_NIL)
#define is_true(c)  	((c) == CELL_TRUE)
#define is_false(c) 	((c) == CELL_FALSE)
#define is_eof(c)   	((c) == CELL_EOF)
#define is_undef(c) 	((c) == CELL_UNDEF)
#define is_ellipsis(c)	((c) == CELL_ELLIPSIS)
#define is_any(c)     	(TRUE)
#define is_boolean(c) 	((c) && T(c) == BOOLEAN)
#define is_char(c)    	((c) && T(c) == CHAR)
#define is_number(c)  	((c) && T(c) == NUMBER)
#define is_real(c)    	((c) && T(c) == NUMBER && (number_type(c) == NUMBER_LONG || number_type(c) == NUMBER_DOUBLE || number_type(c) == NUMBER_FRACTION))
#define is_integer(c) 	(is_number(c) && number_type(c) == NUMBER_LONG)
#define is_natural(c) 	(is_integer_f(c) && number_long(c) >= 0)
#define is_string(c)  	((c) && T(c) == STRING)
#define is_pair(c)    	((c) && T(c) == PAIR)
#define is_vector(c)  	((c) && T(c) == VECTOR)
#define is_symbol(c)  	((c) && T(c) == SYMBOL)
#define is_syntax(c)  	((c) && T(c) == SYNTAX)
#define is_closure_expr(c)	((c) && T(c) == CLOSURE_EXPR)
#define is_closure(c) 	((c) && T(c) == CLOSURE)
#define is_proc(c)    	((c) && T(c) == PROC)
#define is_iproc(c)   	((c) && T(c) == IPROC)
#define is_eproc(c)   	((c) && T(c) == EPROC)
#define is_procs(c)   	((c) && T(c) == PROC || T(c) == IPROC || T(c) == EPROC)
#define is_envir(c)   	((c) && T(c) == ENVIR)
#define is_macro(c)   	((c) && T(c) == MACRO)
#define is_promise(c) 	((c) && T(c) == PROMISE)
#define is_port(c)    	((c) && T(c) == PORT)
#define is_inport(c)  	(is_port(c) && port_type(c) & PORT_INPUT)
#define is_outport(c) 	(is_port(c) && port_type(c) & PORT_OUTPUT)
#define is_instruct(c) 	((c) && T(c) == INSTRUCT)
#define is_continue(c) 	((c) && T(c) == CONTINUE)
#define is_exception(c) ((c) && T(c) == EXCEPTION)
#define is_multivar(c)  ((c) && T(c) == MULTIVAR)
#define is_port_eof(c) 	((c) && T(c) == PORT && ((port_type(c) & PORT_FILE) && feof(port_file(c)) || (port_type(c) & PORT_STRING) && port_string_pos(c) == port_string_end(c)))
#define is_immutable(c) ((c) && cell_type(c) & M_IMMUTABLE)
#define is_interactive(ctx) (port_type(ctx_inports_head(ctx)) & PORT_FILE && port_file(ctx_inports_head(ctx)) == stdin)

#define symbol(c)   	(is_symbol(c) ? symbol_data(c) : CSTR(""))
#define string(c)   	(is_string(c) ? string_data(c) : CSTR(""))
#define port(c)     	(is_port(c) ? &c->port : NULL)

#define rplaca(c, _a)   (is_pair(c) ? c->pair.a = _a : NULL)
#define rplacd(c, _d)   (is_pair(c) ? c->pair.d = _d : NULL)

/************** function *************/
void *cell_alloc(Cell *ctx, uint size);
uint cell_gc(Cell *, uint*);
Segment *cell_mk_segment(uint);
