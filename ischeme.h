#ifndef ISCHEME_H
#define ISCHEME_H

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

//#define IDEBUG_MORE

#ifdef  IDEBUG_MORE
#define IMessage(fmt, ...)  printf("*Message*: "fmt"\n", ##__VA_ARGS__)
#define IWarning(fmt, ...)  printf("*Warning*: "fmt"\n", ##__VA_ARGS__)
#define IError(fmt, ...)    printf("*Error*: "fmt"\n", ##__VA_ARGS__)
#define ITraceEnter()       IMessage("Func %s enter.", __FUNCTION__)
#define ITraceLeave()       IMessage("Func %s leave.", __FUNCTION__)
#else
#define IMessage(fmt, ...)
#define IWarning(fmt, ...)
#define IError(fmt, ...)
#define ITraceEnter()
#define ITraceLeave()
#endif

typedef unsigned char       bool;
typedef unsigned char       uchar;
typedef unsigned int        uint;

typedef unsigned char       Char;
typedef unsigned char       Boolean;
typedef struct _String      String;
typedef struct _String      Symbol;
typedef struct _Number      Number;
typedef struct _Pair        Pair;
typedef struct _Vector      Vector;
typedef struct _Port        Port;
typedef struct _Continue    Continue;
typedef struct _Closure     Closure;
typedef struct _Proc        Proc;
typedef struct _ClosureExpr ClosureExpr;
typedef struct _Exception   Exception;
typedef struct _Cell        Cell;
typedef struct _OpCode      OpCode;
typedef struct _SegFreeList SegFreeList;
typedef struct _Segment     Segment;
typedef struct _Context     Context;
typedef struct _Matcher     Matcher;
typedef struct _Expander    Expander;
typedef struct _Macro       Macro;

typedef enum _Op            Op;
typedef enum _Radix         Radix;
typedef enum _Exactness     Exactness;
typedef enum _NumberType    NumberType;
typedef enum _PortType      PortType;
typedef enum _Token         Token;
typedef enum _Type          Type;
typedef enum _Error         Error;
typedef enum _MatcherType   MatcherType;
typedef enum _ExpanderType  ExpanderType;

typedef Cell*(*Reader)(Cell*, int);
typedef Cell*(*EProc)(Cell*, Cell*);

#define TRUE                    1
#define FALSE                   0
#define MAX_SEGS_COUNT          128
#define SEG_INIT_MEM_SIZE       (100*1024*1024)
#define MAX_LOAD_FILES          256
#define STR_BUF_SIZE            128

#define T_MASK                  0x0000FFFF
#define M_MASK                  0xFFFF0000
#define M_IMMUTABLE             0x00010000
#define M_REFERENCE             0x00020000
#define T(c)                    (c->t & T_MASK)
#define M(c)                    (c->t & M_MASK)


#define S(t)                    sizeof(t)
#define cell_align(n, bits)     (((n)+(1<<(bits))-1)&(((uint)-1)-((1<<(bits))-1)))
#define segment_align(n)        cell_align(n, 4)
#define segment_align_size(s)   (S(Segment) + (s) + segment_align(1))

#define cell_malloc             malloc
#define cell_free               free
#define cell_field(c,t,f)       ((c)->t.f)
#define cell_sizeof(x)          (offsetof(Cell, chr) + S(((Cell*)0)->x))
#define cell_new(_c,_x,_t)      ({ Cell *c = cell_alloc(_c, cell_sizeof(_x));\
                                   if (c) c->t = _t; c;})

#define syntax_new(c)           cell_new(c, op, SYNTAX)
#define iproc_new(c)            cell_new(c, op, IPROC)
#define macro_new(c)            cell_new(c, op, MACRO)
#define char_new(c)             cell_new(c, chr, CHAR)
#define symbol_new(c)           cell_new(c, str, SYMBOL)
#define number_new(c)           cell_new(c, num, NUMBER)
#define pair_new(c)             cell_new(c, pair, PAIR)
#define port_new(c)             cell_new(c, port, PORT)
#define continue_new(c)         cell_new(c, cont, CONTINUE)
#define procedure_new(c)        cell_new(c, eproc, EPROC)
#define closure_new(c)          cell_new(c, clos, CLOSURE)
#define closure_expr_new(c)     cell_new(c, closexpr, CLOSURE_EXPR);
#define proc_new(c)             cell_new(c, proc, PROC)
#define context_new(c)          cell_new(c, ctx, CONTEXT)
#define exception_new(c)        cell_new(c, excpt, EXCEPTION)
#define matcher_new(c)          cell_new(c, mt, MATCHER)
#define expander_new(c)         cell_new(c, expd, EXPANDER)

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

#define vector_length(c)        (cell_field(c,vector,length))
#define vector_data(c)          (cell_field(c,vector,data))

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

#define continue_op(c)          (cell_field(c,cont,op))
#define continue_args(c)        (cell_field(c,cont,args))
#define continue_env(c)         (cell_field(c,cont,env))
#define continue_code(c)        (cell_field(c,cont,code))
#define continue_data(c)        (cell_field(c,cont,data))
#define continues_car(c)        (c->pair.a)
#define continues_cdr(c)        (c->pair.d)

#define closure_name(c)         (cell_field(c,clos,name))
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

#define ctx_segments(c)         (cell_field(c,ctx,segments))
#define ctx_global_env(c)       (cell_field(c,ctx,global_env))
#define ctx_symbols(c)          (cell_field(c,ctx,symbols))
#define ctx_inport(c)           (cell_field(c,ctx,inport))
#define ctx_outport(c)          (cell_field(c,ctx,outport))
#define ctx_load_files(c)       (cell_field(c,ctx,load_files))
#define ctx_load_file(c,n)      (cell_field(c,ctx,load_files)[n])
#define ctx_file_idx(c)         (cell_field(c,ctx,file_idx))
#define ctx_op(c)               (cell_field(c,ctx,op))
#define ctx_ret(c)              (cell_field(c,ctx,ret))
#define ctx_args(c)             (cell_field(c,ctx,args))
#define ctx_env(c)              (cell_field(c,ctx,env))
#define ctx_code(c)             (cell_field(c,ctx,code))
#define ctx_data(c)             (cell_field(c,ctx,data))
#define ctx_continue(c)         (cell_field(c,ctx,cont))
#define ctx_lambda(c)           (cell_field(c,ctx,lambda))
#define ctx_quote(c)            (cell_field(c,ctx,quote))
#define ctx_quasiquote(c)       (cell_field(c,ctx,qquote))
#define ctx_unquote(c)          (cell_field(c,ctx,uquote))
#define ctx_unquote_splicing(c) (cell_field(c,ctx,uquotes))
#define ctx_syntax_expr(c)      (cell_field(c,ctx,synepr))

enum _Type {
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
    CONTINUE,
    CONTINUES,
    ENVIR,
    PROMISE,
    EXCEPTION,
};

enum _Token {
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

enum _PortType {
    PORT_FREE   = 0,
    PORT_INPUT  = 1<<1,
    PORT_OUTPUT = 1<<2,
    PORT_ERROR  = 1<<3,
    PORT_FILE   = 1<<4,
    PORT_STRING = 1<<5,
    PORT_EOF    = 1<<6,
};

enum _NumberType {
    NUMBER_LONG,
    NUMBER_DOUBLE,
    NUMBER_FRACTION,
    NUMBER_COMPLEX,
};

enum _Exactness {
    NO_EXACTNESS,
    INEXACT,
    EXACT
};

enum _Radix {
    NO_RADIX = 0,
    BIN = 2,
    OCT = 8,
    DEC = 10,
    HEX = 16
};

enum _Op {
    #define _OPCODE(n, t1, o, m1, m2, t2) o,
    #include "opcodes.h"
    #undef _OPCODE
    OPCODE_MAX
};

enum _Error {
    SyntaxError,
    MemoryError,
    IndexError,
    IOError,
    TypeError,
    ValueError,
    ReferenceError,
    ArithmeticError,
};

enum _MatcherType {
    MatcherLiteral,
    MatcherConstant,
    MatcherVariable,
    MatcherUnderscore,
    MatcherRest,
    MatcherSequence,
};

enum _ExpanderType {
    ExpanderConstant,
    ExpanderVariable,
    ExpanderSequence,
};

struct _String {
    uint size;
    char data[];
};

struct _Number {
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

struct _Port {
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

struct _Continue {
    Op op;
    Cell *args;
    Cell *env;
    Cell *code;
    Cell *data;
};

struct _Pair {
    Cell *a;
    Cell *d;
};

struct _Vector {
    uint length;
    Cell *data[];
};

struct _Closure {
    Cell *name;
    Cell *args;
    Cell *code;
    Cell *data;
    Cell *env;
};

struct _ClosureExpr {
    Cell *expr;
    Cell *env;
};

struct _Proc {
    Cell *name;
    Cell *clos;
};

struct _Exception {
    Error t;
    Cell *msg;
    Cell *trg;
    Cell *src;
};

struct _SegFreeList {
  uint size;
  SegFreeList *next;
};

struct _Segment {
  uint size, max_size;
  SegFreeList *free_list;
  Segment *next;
  char *data;
};

struct _Context {
    Segment *segments;
    Cell *global_env;
    Cell *symbols;
    Cell *inport;
    Cell *outport;
    Cell *load_files[MAX_LOAD_FILES];
    int file_idx;

    Op op;
    Cell *ret;
    Cell *args;
    Cell *env;
    Cell *code;
    Cell *data;
    Cell *cont;

    Cell *lambda;
    Cell *quote;
    Cell *qquote;
    Cell *uquote;
    Cell *uquotes;
    Cell *synepr;
};

struct _Matcher {
    MatcherType t;
    bool rept;
    Cell *name;
    Cell *value;
};

struct _Expander {
    ExpanderType t;
    uint n;
    Cell *name;
    Cell *value;
};

struct _Macro {
    Cell *mchs;
    Cell *env;
};

struct _Cell {
    uint t;
    union {
        int         op;
        Char        chr;
        Boolean     bl;
        String      str;
        Number      num;
        Pair        pair;
        Vector      vector;
        Port        port;
        Continue    cont;
        EProc       eproc;
        Closure     clos;
        Proc        proc;
        Context     ctx;
        Exception   excpt;
        Matcher     mt;
        Expander    expd;
        Macro       macro;
        ClosureExpr  closexpr;
    };
};

struct _OpCode {
    char *name;
    unsigned char t;
    int min_args;
    int max_args;
    unsigned char *arg_types;
};

#define T_ANY       "\001"
#define T_CHAR      "\002"
#define T_NUMBER    "\003"
#define T_REAL      "\004"
#define T_INTEGER   "\005"
#define T_NATURAL   "\006"
#define T_STRING    "\007"
#define T_SYMBOL    "\010"
#define T_PAIR      "\011"
#define T_LIST      "\012"
#define T_VECTOR    "\013"
#define T_PROC      "\014"
#define T_ENVIR     "\015"
#define T_CONTI     "\016"
#define T_PORT      "\017"
#define T_INPORT    "\020"
#define T_OUTPORT   "\021"


/************** function *************/
void *cell_alloc(Cell *ctx, uint size);
uint cell_gc(Cell *, uint*);
Segment *cell_mk_segment(int, int);

#endif
