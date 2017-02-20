#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <string>
#include <vector>
#include <map>

using std::vector;
using std::map;

#define IDEBUG_MORE
//#define MACRO_DEBUG

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
#define STR_BUF_SIZE            256
#define PORT_BUF_SIZE           512

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
#define port_file_new(c)        cell_new(c, port.f, PORT)
#define port_string_new(c)      cell_new(c, port.s, PORT)
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
#define env_new(c)              cell_new(c, env, ENV)
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

#define port_type(p)            (cell_field(p,port.s,t))
#define port_file(p)            (cell_field(p,port,f.file))
#define port_file_name(p)       (cell_field(p,port,f.name))
#define port_file_pos(p)        (cell_field(p,port,f.pos))
#define port_file_buf(p)        (cell_field(p,port,f.buf))
#define port_file_bufidx(p)     (cell_field(p,port,f.bidx))
#define port_file_buflen(p)     (cell_field(p,port,f.blen))
#define port_string_start(p)    (cell_field(p,port,s.start))
#define port_string_end(p)      (cell_field(p,port,s.end))
#define port_string_pos(p)      (cell_field(p,port,s.pos))
#define port_string_src(p)      (cell_field(p,port,s.src))

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

#define env_immtb(e)            (cell_field(e,env,immtb))
#define env_map(e)              (cell_field(e,env,mp))
#define env_outer(e)            (cell_field(e,env,outer))

#define promise_result(p)       (cell_field(p,proms,ret))
#define promise_expr(p)         (cell_field(p,proms,expr))

#define multivar_n(v)           (cell_field(v,multiv,n))
#define multivar_var(v)         (cell_field(v,multiv,var))

#define ctx_segments(c)         (cell_field(c,ctx,segments))
#define ctx_global_env(c)       (cell_field(c,ctx,global_env))
#define ctx_r5rs_env(c)         (cell_field(c,ctx,r5rs_env))
#define ctx_null_env(c)         (cell_field(c,ctx,null_env))
#define ctx_symbols(c)          (cell_field(c,ctx,symbols))
#define ctx_stdinport(c)        (cell_field(c,ctx,stdinport))
#define ctx_stdoutport(c)       (cell_field(c,ctx,stdoutport))
#define ctx_stderrport(c)       (cell_field(c,ctx,stderrport))
#define ctx_inport(c)           (cell_field(c,ctx,inport))
#define ctx_outport(c)          (cell_field(c,ctx,outport))
#define ctx_transc_port(c)      (cell_field(c,ctx,trsport))
#define ctx_transc_idx(c)       (cell_field(c,ctx,trsidx))
#define ctx_transc_inportp(c)   (cell_field(c,ctx,trsinp))
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
    ENV,
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
typedef Cell*(*Reader)(Cell*, Cell*, int);
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
    union {
        struct {
            uint t;
            FILE *file;
            Cell *name;
            int pos;
            char buf[PORT_BUF_SIZE];
            int bidx;
            int blen;
        } f;
        struct {
            uint t;
            char *start;
            char *end;
            char *pos;
            Cell *src;
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
    Cell *r5rs_env;
    Cell *null_env;
    Cell *symbols;
    Cell *stdinport;
    Cell *stdoutport;
    Cell *stderrport;
    Cell *inport;
    Cell *outport;
    Cell *trsport;
    bool trsinp;
    int trsidx;
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

struct ptrCmp {
    bool operator()(const char *s1, const char *s2) const {
        return strcmp(s1, s2) < 0;
    }
};

struct Env {
    bool immtb;
    map<char*, Cell*, ptrCmp> mp;
    Cell *outer;
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
        Env         env;
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
#define T_LETTER        "\003"
#define T_NUMBER    	"\004"
#define T_REAL      	"\005"
#define T_INTEGER   	"\006"
#define T_NATURAL   	"\007"
#define T_STRING    	"\010"
#define T_SYMBOL    	"\011"
#define T_PAIR      	"\012"
#define T_LIST      	"\013"
#define T_VECTOR    	"\014"
#define T_PROC      	"\015"
#define T_ENV     	    "\016"
#define T_CONTI     	"\017"
#define T_PORT      	"\020"
#define T_INPORT    	"\021"
#define T_OUTPORT   	"\022"
#define T_PROMISE       "\023"

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
#define is_letter(c)    ((c) && T(c) == CHAR && isalpha(char_value(c)))
#define is_number(c)  	((c) && T(c) == NUMBER)
#define is_real(c)    	((c) && T(c) == NUMBER && (number_type(c) == NUMBER_LONG || number_type(c) == NUMBER_DOUBLE || number_type(c) == NUMBER_FRACTION))
#define is_integer(c) 	(is_number(c) && number_type(c) == NUMBER_LONG)
#define is_natural(c) 	(is_integer_f(c) && number_long(c) >= 0)
#define is_complex(c)   (is_number(c) && number_type(c) == NUMBER_COMPLEX)
#define is_exact(c)     (is_number(c) && (number_type(c) == NUMBER_LONG || number_type(c) == NUMBER_FRACTION || (number_type(c) == NUMBER_COMPLEX && number_type(number_cx_rl(c)) != NUMBER_DOUBLE)))
#define is_inexact(c)   (!is_exact(c))
#define is_zero(c)      (is_integer(c) && number_long(c) == 0)
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
#define is_env(c)   	((c) && T(c) == ENV)
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

#define gc_var(a,b)                     Cell *a=NULL; Preserved b={NULL,NULL}
#define gc_var1(a)                      gc_var(a, __preserved_var1)
#define gc_var2(a,b)                    gc_var1(a); gc_var(b, __preserved_var2)
#define gc_var3(a,b,c)                  gc_var2(a,b); gc_var(c, __preserved_var3)
#define gc_var4(a,b,c,d)                gc_var3(a,b,c); gc_var(d, __preserved_var4)
#define gc_var5(a,b,c,d,e)              gc_var4(a,b,c,d); gc_var(e, __preserved_var5)
#define gc_var6(a,b,c,d,e,f)            gc_var5(a,b,c,d,e); gc_var(f, __preserved_var6)
#define gc_var7(a,b,c,d,e,f,g)          gc_var6(a,b,c,d,e,f); gc_var(g, __preserved_var7)
#define gc_var8(a,b,c,d,e,f,g,h)        gc_var7(a,b,c,d,e,f,g); gc_var(h, __preserved_var8)

#define gc_preserve(ctx,a,b)            do { \
                                            (b).var = &(a); \
                                            (b).next = ctx_saves(ctx); \
                                            ctx_saves(ctx) = &(b); \
                                        } while (0)
#define gc_preserve1(s,a)               gc_preserve(s,a,__preserved_var1)
#define gc_preserve2(s,a,b)             gc_preserve1(s,a); gc_preserve(s,b,__preserved_var2)
#define gc_preserve3(s,a,b,c)           gc_preserve2(s,a,b); gc_preserve(s,c,__preserved_var3)
#define gc_preserve4(s,a,b,c,d)         gc_preserve3(s,a,b,c); gc_preserve(s,d,__preserved_var4)
#define gc_preserve5(s,a,b,c,d,e)       gc_preserve4(s,a,b,c,d); gc_preserve(s,e,__preserved_var5)
#define gc_preserve6(s,a,b,c,d,e,f)     gc_preserve5(s,a,b,c,d,e); gc_preserve(s,f,__preserved_var6)
#define gc_preserve7(s,a,b,c,d,e,f,g)   gc_preserve6(s,a,b,c,d,e,f); gc_preserve(s,g,__preserved_var7)
#define gc_preserve8(s,a,b,c,d,e,f,g,h) gc_preserve7(s,a,b,c,d,e,f,g); gc_preserve(s,h,__preserved_var8)
#define gc_release(s)                   (ctx_saves(s) = __preserved_var1.next)

#define CELL_TRUE                       &g_true
#define CELL_FALSE                      &g_false
#define CELL_NIL                        &g_nil
#define CELL_EOF                        &g_eof
#define CELL_UNDEF                      &g_undef
#define CELL_ERR                        &g_error
#define CELL_ELLIPSIS                   &g_ellipsis
#define CELL_SPLICING                   &g_splicing

extern Cell g_true;
extern Cell g_false;
extern Cell g_nil;
extern Cell g_eof;
extern Cell g_undef;
extern Cell g_error;
extern Cell g_ellipsis;
extern Cell g_splicing;

/************** function *************/
void *cell_alloc(Cell *ctx, uint size);
uint cell_gc(Cell *, uint*);
Segment *cell_mk_segment(uint);

bool is_list(Cell *c);
bool is_contains(Cell *ls, Cell *c);
bool equal(Cell *a, Cell *b);
Cell *assq(Cell *key, Cell *list);
Cell *find_env(Cell *env, Cell *k);
uint length(Cell *list);
void list_add(Cell *ctx, Cell *ls, Cell *c);
void list_extend(Cell *ctx, Cell *ls, Cell *c);
void list_set(Cell *ls, Cell *n, Cell *v);
Cell *list_ref(Cell *ls, Cell *n);
Cell *list_tail(Cell *ls, uint n);
Cell *list_pop(Cell *ls);
void alist_update(Cell *ctx, Cell *ls1, Cell *ls2);
void alist_append(Cell *ctx, Cell *ls, Cell *pair);

Cell *cons(Cell *ctx, Cell *a, Cell *d);
Cell *mk_bool(bool b);
Cell *mk_char(Cell *ctx, char chr);
Cell *mk_string(Cell *ctx, const char *fmt, ...);
Cell *mk_string(Cell *ctx, uint size, char fill);
Cell *mk_string(Cell *ctx, uint size);
Cell *mk_symbol(Cell *ctx, const char *s);
Cell *mk_vector(Cell *ctx, uint len, Cell *fill);
Cell *mk_env(Cell *ctx, bool immtb, Cell *outer);
Cell *mk_iproc(Cell *ctx, int op);
Cell *mk_macro(Cell *ctx, Cell *mchs, Cell *env);
Cell *mk_promise(Cell *ctx, Cell *expr);
Cell *mk_port(Cell *ctx, FILE *f, const char *name, int t);
Cell *mk_port(Cell *ctx, char *start, char *end, int t);
Cell *mk_syntax(Cell *ctx, int op);
Cell *mk_continue(Cell *ctx, Cell *ins, Cell *winds);
Cell *mk_multival(Cell *ctx, Cell *c);
Cell *mk_closure(Cell *ctx, Cell *a, Cell *e);
Cell *mk_proc(Cell *ctx, Cell *name, Cell *clos);
Cell *mk_long(Cell *ctx, long l);
Cell *mk_double(Cell *ctx, double d);
Cell *mk_fraction(Cell *ctx, long nr, long dr);
Cell *mk_complex(Cell *ctx, Cell *rl, Cell *im);
Cell *mk_exception_f(Cell *ctx, ErrorType t, Cell *msg, Cell *trg, Cell *src);
#define mk_exception(c,t,msg,trg,src)   ({ \
                                            gc_var3(_msg,_trg,_src); \
                                            gc_preserve3(c,_msg,_trg,_src); \
                                            _msg=msg; _trg=trg; _src=src; \
                                            Cell *excpt = mk_exception_f(ctx,t,_msg,_trg,_src); \
                                            gc_release(ctx); \
                                            excpt; \
                                        })

Cell *num_calcu(Cell *ctx, int op, Cell *a, Cell *b);
Cell *num_etoi(Cell *ctx, Cell *num);
Cell *num_itoe(Cell *ctx, Cell *num);
bool num_equal(Cell *a, Cell *b);
double num_real_compare(Cell *a, Cell *b);

Cell *macro_analyze(Cell *ctx, Cell *lit, Cell *matches, Cell *syn_env);
Cell *macro_transform(Cell *ctx, Cell *machers, Cell *syn_env, Cell *expr, Cell *expr_env);
