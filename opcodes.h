_OPCODE(NULL,                             0,          OP_REPL_READ,               0,  0,  0)
_OPCODE(NULL,                             0,          OP_REPL_EVAL,               0,  0,  0)
_OPCODE(NULL,                             0,          OP_REPL_PRINT,              0,  0,  0)
_OPCODE(NULL,                             0,          OP_REPL_LOOP,               0,  0,  0)
_OPCODE(NULL,                             0,          OP_ERROR,                   0,  0,  0)
_OPCODE(NULL,                             0,          OP_EVAL,                    0,  0,  0)
_OPCODE(NULL,                             0,          OP_EVAL_ARGS,               0,  0,  0)
_OPCODE(NULL,                             0,          OP_EVAL_LIST,               0,  0,  0)
_OPCODE(NULL,                             0,          OP_APPLY,                   0,  0,  0)
_OPCODE(NULL,                             0,          OP_APPLY_WIND,              0,  0,  0)
_OPCODE(NULL,                             0,          OP_APPLY_WIND1,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_APPLY_WIND2,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_APPLY_WIND3,             0,  0,  0)
_OPCODE("define",                         SYNTAX,     OP_DEF,                     2,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_DEF1,                    0,  0,  0)
_OPCODE("lambda",                         SYNTAX,     OP_LAMBDA,                  2,  0xFFFF, 0)
_OPCODE("do",                             SYNTAX,     OP_DO,                      2,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_DO1,                     0,  0,  0)
_OPCODE(NULL,                             0,          OP_DO2,                     0,  0,  0)
_OPCODE(NULL,                             0,          OP_DO3,                     0,  0,  0)
_OPCODE(NULL,                             0,          OP_DO4,                     0,  0,  0)
_OPCODE("let",                            SYNTAX,     OP_LET,                     2,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_LET1,                    0,  0, 0)
_OPCODE(NULL,                             0,          OP_LET2,                    0,  0, 0)
_OPCODE("let*",                           SYNTAX,     OP_LETSEQ,                  2,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_LETSEQ1,                 0,  0, 0)
_OPCODE("letrec",                         SYNTAX,     OP_LETREC,                  2,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_LETREC1,                 0,  0, 0)
_OPCODE("set!",                           SYNTAX,     OP_SET,                     2,  2,  0)
_OPCODE(NULL,                             0,          OP_SET1,                    0,  0,  0)
_OPCODE("if",                             SYNTAX,     OP_IF,                      2,  3,  0)
_OPCODE(NULL,                             0,          OP_IF1,                     0,  0,  0)
_OPCODE("cond",                           SYNTAX,     OP_COND,                    0,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_COND1,                   0,  0,  0)
_OPCODE(NULL,                             0,          OP_COND2,                   0,  0,  0)
_OPCODE("case",                           SYNTAX,     OP_CASE,                    1,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_CASE1,                   0,  0,  0)
_OPCODE("and",                            SYNTAX,     OP_AND,                     0,  0xFFFF, 0)
_OPCODE("or",                             SYNTAX,     OP_OR,                      0,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_ANDOR,                   0,  0,  0)
_OPCODE("begin",                          SYNTAX,     OP_BEGIN,                   0,  0xFFFF, 0)
_OPCODE("define-syntax",                  SYNTAX,     OP_DEF_SYNTAX,              2,  2,  0)
_OPCODE(NULL,                             0,          OP_DEF_SYNTAX1,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_DEF_SYNTAX2,             0,  0,  0)
_OPCODE("let-syntax",                     SYNTAX,     OP_LET_SYNTAX,              2,  0xFFFF, 0)
_OPCODE(NULL,                             0,          OP_LET_SYNTAX1,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_LET_SYNTAX2,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_LET_SYNTAX3,             0,  0,  0)
_OPCODE("letrec-syntax",                  SYNTAX,     OP_LETREC_SYNTAX,           2,  0xFFFF, 0)
_OPCODE("syntax-rules",                   SYNTAX,     OP_SYNTAX_RULES,            0,  0xFFFF, 0)
_OPCODE("quote",                          SYNTAX,     OP_QUOTE,                   1,  1,  0)
_OPCODE("quasiquote",                     SYNTAX,     OP_QUASIQUOTE,              1,  1,  0)
_OPCODE(NULL,                             0,          OP_QUASIQUOTE1,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_QUASIQUOTE2,             0,  0,  0)
_OPCODE(NULL,                             0,          OP_QUASIQUOTE3,             0,  0,  0)
_OPCODE("unquote",                        SYNTAX,     OP_UNQUOTE,                 1,  1,  0)
_OPCODE("unquote-splicing",               SYNTAX,     OP_UNQUOTE_SPLICING,        1,  1,  0)
_OPCODE("delay",                          SYNTAX,     OP_DELAY,                   1,  1,  0)

_OPCODE("map",                            IPROC,      OP_MAP,                     2,  0xFFFF, T_PROC T_LIST)
_OPCODE(NULL,                             0,          OP_MAP1,                    0,  0,  0)
_OPCODE("for-each",                       IPROC,      OP_FOREACH,                 2,  0xFFFF, T_PROC T_LIST)
_OPCODE(NULL,                             0,          OP_FOREACH1,                0,  0,  0)
_OPCODE("eval",                           IPROC,      OP_PEVAL,                   1,  2,  0)
_OPCODE("apply",                          IPROC,      OP_PAPPLY,                  2,  0xFFFF, 0)
_OPCODE("force",                          IPROC,      OP_FORCE,                   1,  1,  T_PROMISE)
_OPCODE(NULL,                             0,          OP_FORCE1,                  0,  0,  0)
_OPCODE("call-with-current-continuation", IPROC,      OP_CALLCC,                  1,  1,  T_PROC)
_OPCODE("values",                         IPROC,      OP_VALUES,                  0,  0xFFFF, 0)
_OPCODE("call-with-values",               IPROC,      OP_CALL_WITH_VALUES,        2,  2,  T_PROC)
_OPCODE(NULL,                             0,          OP_CALL_WITH_VALUES1,       0,  0,  0)
_OPCODE("dynamic-wind",                   IPROC,      OP_DYNAMIC_WIND,            3,  3,  T_PROC)
_OPCODE(NULL,                             0,          OP_DYNAMIC_WIND1,           0,  0,  0)
_OPCODE(NULL,                             0,          OP_DYNAMIC_WIND2,           0,  0,  0)
_OPCODE(NULL,                             0,          OP_DYNAMIC_WIND3,           0,  0,  0)
_OPCODE("scheme-report-environment",      IPROC,      OP_SCHEME_REPORT_ENV,       1,  1,  T_INTEGER)
_OPCODE("null-environment",               IPROC,      OP_NULL_ENV,                1,  1,  T_INTEGER)
_OPCODE("interaction-environment",        IPROC,      OP_INTERACTION_ENV,         0,  0,  0)

_OPCODE("load",                           IPROC,      OP_LOAD,                    1,  1,  T_STRING)
_OPCODE("transcript-on",                  IPROC,      OP_TRANSCRIPT_ON,           1,  1,  T_STRING)
_OPCODE("transcript-off",                 IPROC,      OP_TRANSCRIPT_OFF,          0,  0,  0)
_OPCODE("display",                        IPROC,      OP_DISPLAY,                 1,  2,  T_ANY T_OUTPORT)
_OPCODE("newline",                        IPROC,      OP_NEWLINE,                 0,  1,  T_OUTPORT)
_OPCODE("read",                           IPROC,      OP_READ,                    0,  0,  0)
_OPCODE("write",                          IPROC,      OP_WRITE,                   0,  0,  0)
_OPCODE("read-char",                      IPROC,      OP_READ_CHAR,               0,  0,  0)
_OPCODE("write-char",                     IPROC,      OP_WRITE_CHAR,              0,  0,  0)
_OPCODE("peek-char",                      IPROC,      OP_PEEK_CHAR,               0,  0,  0)
_OPCODE("char-ready?",                    IPROC,      OP_CHAR_READY_P,            0,  0,  0)
_OPCODE("eof-object?",                    IPROC,      OP_EOF_OBJECT_P,            0,  0,  0)
_OPCODE("open-input-file",                IPROC,      OP_OPEN_INPUT_FILE,         0,  0,  0)
_OPCODE("open-output-file",               IPROC,      OP_OPEN_OUTPUT_FILE,        0,  0,  0)
_OPCODE("close-input-port",               IPROC,      OP_CLOSE_INPUT_PORT,        0,  0,  0)
_OPCODE("close-output-port",              IPROC,      OP_CLOSE_OUTPUT_PORT,       0,  0,  0)
_OPCODE("port?",                          IPROC,      OP_PORT_P,                  0,  0,  0)
_OPCODE("input-port?",                    IPROC,      OP_INPUT_PORT_P,            0,  0,  0)
_OPCODE("output-port?",                   IPROC,      OP_OUTPUT_PORT_P,           0,  0,  0)
_OPCODE("current-input-port",             IPROC,      OP_CURR_INPUT_PORT,         0,  0,  0)
_OPCODE("current-output-port",            IPROC,      OP_CURR_OUTPUT_PORT,        0,  0,  0)
_OPCODE("call-with-input-file",           IPROC,      OP_CALL_WITH_INPUT_FILE,    0,  0,  0)
_OPCODE("call-with-output-file",          IPROC,      OP_CALL_WITH_OUTPUT_FILE,   0,  0,  0)
_OPCODE("with-input-from-file",           IPROC,      OP_WITH_INPUT_FROM_FILE,    0,  0,  0)
_OPCODE("with-output-to-file",            IPROC,      OP_WITH_OUTPUT_TO_FILE,     0,  0,  0)

_OPCODE("not",                            IPROC,      OP_NOT,                     1,  1,  0)
_OPCODE("null?",                          IPROC,      OP_NULL_P,                  1,  1,  0)
_OPCODE("boolean?",                       IPROC,      OP_BOOLEAN_P,               1,  1,  0)
_OPCODE("symbol?",                        IPROC,      OP_SYMBOL_P,                1,  1,  0)
_OPCODE("pair?",                          IPROC,      OP_PAIR_P,                  1,  1,  0)
_OPCODE("procedure?",                     IPROC,      OP_PROCEDURE_P,             1,  1,  0)
_OPCODE("eq?",                            IPROC,      OP_EQ_P,                    2,  2,  0)
_OPCODE("eqv?",                           IPROC,      OP_EQV_P,                   2,  2,  0)
_OPCODE("equal?",                         IPROC,      OP_EQUAL_P,                 2,  2,  0)

_OPCODE("char?",                          IPROC,      OP_CHAR_P,                  1,  1,  0)
_OPCODE("char=?",                         IPROC,      OP_CHAR_EP,                 0,  0,  0)
_OPCODE("char<?",                         IPROC,      OP_CHAR_LP,                 0,  0,  0)
_OPCODE("char<=?",                        IPROC,      OP_CHAR_LEP,                0,  0,  0)
_OPCODE("char>?",                         IPROC,      OP_CHAR_GP,                 0,  0,  0)
_OPCODE("char>=?",                        IPROC,      OP_CHAR_GEP,                0,  0,  0)
_OPCODE("char-ci=?",                      IPROC,      OP_CHAR_CI_EP,              0,  0,  0)
_OPCODE("char-ci<?",                      IPROC,      OP_CHAR_CI_LP,              0,  0,  0)
_OPCODE("char-ci<=?",                     IPROC,      OP_CHAR_CI_LEP,             0,  0,  0)
_OPCODE("char-ci>?",                      IPROC,      OP_CHAR_CI_GP,              0,  0,  0)
_OPCODE("char-ci>=?",                     IPROC,      OP_CHAR_CI_GEP,             0,  0,  0)
_OPCODE("char-alphabetic?",               IPROC,      OP_CHAR_ALPHA_P,            0,  0,  0)
_OPCODE("char-numeric?",                  IPROC,      OP_CHAR_NUMBER_P,           0,  0,  0)
_OPCODE("char-whitespace?",               IPROC,      OP_CHAR_SPACE_P,            0,  0,  0)
_OPCODE("char-upper-case?",               IPROC,      OP_CHAR_UPPER_P,            0,  0,  0)
_OPCODE("char-lower-case?",               IPROC,      OP_CHAR_LOWER_P,            0,  0,  0)

_OPCODE("string?",                        IPROC,      OP_STRING_P,                1,  1,  0)
_OPCODE("string=?",                       IPROC,      OP_STRING_EP,               0,  0,  0)
_OPCODE("string<?",                       IPROC,      OP_STRING_LP,               0,  0,  0)
_OPCODE("string<=?",                      IPROC,      OP_STRING_LEP,              0,  0,  0)
_OPCODE("string>?",                       IPROC,      OP_STRING_GP,               0,  0,  0)
_OPCODE("string>=?",                      IPROC,      OP_STRING_GEP,              0,  0,  0)
_OPCODE("string-ci=?",                    IPROC,      OP_STRING_CI_EP,            0,  0,  0)
_OPCODE("string-ci<?",                    IPROC,      OP_STRING_CI_LP,            0,  0,  0)
_OPCODE("string-ci<=?",                   IPROC,      OP_STRING_CI_LEP,           0,  0,  0)
_OPCODE("string-ci>?",                    IPROC,      OP_STRING_CI_GP,            0,  0,  0)
_OPCODE("string-ci>=?",                   IPROC,      OP_STRING_CI_GEP,           0,  0,  0)

_OPCODE("vector?",                        IPROC,      OP_VECTOR_P,                1,  1,  0)
_OPCODE("vector",                         IPROC,      OP_VECTOR,                  0,  0xFFFF, 0)
_OPCODE("make-vector",                    IPROC,      OP_MAKE_VECTOR,             1,  2,  T_NATURAL T_ANY)
_OPCODE("vector-length",                  IPROC,      OP_VECTOR_LENGTH,           1,  1,  T_VECTOR)
_OPCODE("vector-ref",                     IPROC,      OP_VECTOR_REF,              2,  2,  T_VECTOR T_NATURAL)
_OPCODE("vector-set!",                    IPROC,      OP_VECTOR_SET,              3,  3,  T_VECTOR T_NATURAL T_ANY)
_OPCODE("vector-fill!",                   IPROC,      OP_VECTOR_FILL,             2,  0xFFFF, 0)
_OPCODE("vector->list",                   IPROC,      OP_VECTOR_TO_LIST,          1,  1,  T_VECTOR)
_OPCODE("list?",                          IPROC,      OP_LIST_P,                  1,  1,  0)
_OPCODE("list",                           IPROC,      OP_LIST,                    0,  0xFFFF, 0)
_OPCODE("list->string",                   IPROC,      OP_LIST_TO_STRING,          1,  1,  T_LIST)
_OPCODE("list->vector",                   IPROC,      OP_LIST_TO_VECTOR,          1,  1,  T_LIST)
_OPCODE("symbol->string",                 IPROC,      OP_SYMBOL_STRING,           0,  0,  0)
_OPCODE("string",                         IPROC,      OP_STRING,                  0,  0,  0)
_OPCODE("make-string",                    IPROC,      OP_MAKE_STRING,             0,  0,  0)
_OPCODE("string-length",                  IPROC,      OP_STRING_LENGTH,           0,  0,  0)
_OPCODE("string-ref",                     IPROC,      OP_STRING_REF,              0,  0,  0)
_OPCODE("string-set!",                    IPROC,      OP_STRING_SET,              0,  0,  0)
_OPCODE("substring",                      IPROC,      OP_SUBSTRING,               0,  0,  0)
_OPCODE("string-append",                  IPROC,      OP_STRING_APPEND,           0,  0,  0)
_OPCODE("string-copy",                    IPROC,      OP_STRING_COPY,             0,  0,  0)
_OPCODE("string-fill!",                   IPROC,      OP_STRING_FILL,             0,  0,  0)
_OPCODE("string->symbol",                 IPROC,      OP_STRING_TO_SYMBOL,        0,  0,  0)
_OPCODE("string->list",                   IPROC,      OP_STRING_TO_LIST,          0,  0,  0)
_OPCODE("string->number",                 IPROC,      OP_STRING_TO_NUMBER,        0,  0,  0)
_OPCODE("char->integer",                  IPROC,      OP_CHAR_TO_INTEGER,         0,  0,  0)
_OPCODE("char-upcase",                    IPROC,      OP_CHAR_UPCASE,             0,  0,  0)
_OPCODE("char-downcase",                  IPROC,      OP_CHAR_DOWNCASE,           0,  0,  0)

_OPCODE("cons",                           IPROC,      OP_CONS,                    2,  2,  0)
_OPCODE("car",                            IPROC,      OP_CAR,                     1,  1,  T_PAIR)
_OPCODE("cdr",                            IPROC,      OP_CDR,                     1,  1,  T_PAIR)
_OPCODE("set-car!",                       IPROC,      OP_SET_CAR,                 2,  2,  T_PAIR T_ANY)
_OPCODE("set-cdr!",                       IPROC,      OP_SET_CDR,                 2,  2,  T_PAIR T_ANY)
_OPCODE("length",                         IPROC,      OP_LENGTH,                  1,  1,  T_LIST)
_OPCODE("append",                         IPROC,      OP_APPEND,                  0,  0xFFFF, 0)
_OPCODE("reverse",                        IPROC,      OP_REVERSE,                 1,  1,  T_LIST)
_OPCODE("list-tail",                      IPROC,      OP_LIST_TAIL,               2,  2,  T_LIST T_NATURAL)
_OPCODE("list-ref",                       IPROC,      OP_LIST_REF,                2,  2,  T_LIST T_NATURAL)
_OPCODE("memq",                           IPROC,      OP_MEMQ,                    2,  2,  T_ANY T_LIST)
_OPCODE("memv",                           IPROC,      OP_MEMV,                    2,  2,  T_ANY T_LIST)
_OPCODE("member",                         IPROC,      OP_MEMBER,                  2,  2,  T_ANY T_LIST)
_OPCODE("assq",                           IPROC,      OP_ASSQ,                    2,  2,  T_ANY T_LIST)
_OPCODE("assv",                           IPROC,      OP_ASSV,                    2,  2,  T_ANY T_LIST)
_OPCODE("assoc",                          IPROC,      OP_ASSOC,                   2,  2,  T_ANY T_LIST)

_OPCODE("number?",                        IPROC,      OP_NUMBER_P,                1,  1,  0)
_OPCODE("integer?",                       IPROC,      OP_INTEGER_P,               0,  0,  0)
_OPCODE("rational?",                      IPROC,      OP_RATIONAL_P,              0,  0,  0)
_OPCODE("real?",                          IPROC,      OP_REAL_P,                  0,  0,  0)
_OPCODE("complex?",                       IPROC,      OP_COMPLEX_P,               0,  0,  0)
_OPCODE("exact?",                         IPROC,      OP_EXACT_P,                 0,  0,  0)
_OPCODE("inexact?",                       IPROC,      OP_INEXACT_P,               0,  0,  0)
_OPCODE("zero?",                          IPROC,      OP_ZERO_P,                  0,  0,  0)
_OPCODE("negative?",                      IPROC,      OP_NEGATIVE_P,              0,  0,  0)
_OPCODE("positive?",                      IPROC,      OP_POSITIVE_P,              0,  0,  0)
_OPCODE("odd?",                           IPROC,      OP_ODD_P,                   1,  1,  T_INTEGER)
_OPCODE("even?",                          IPROC,      OP_EVEN_P,                  1,  1,  T_INTEGER)
_OPCODE("<",                              IPROC,      OP_LP,                      2,  0xFFFF, T_REAL)
_OPCODE("<=",                             IPROC,      OP_LEP,                     2,  0xFFFF, T_REAL)
_OPCODE(">",                              IPROC,      OP_GP,                      2,  0xFFFF, T_REAL)
_OPCODE(">=",                             IPROC,      OP_GEP,                     2,  0xFFFF, T_REAL)
_OPCODE("=",                              IPROC,      OP_EP,                      2,  0xFFFF, T_NUMBER)
_OPCODE("+",                              IPROC,      OP_ADD,                     0,  0xFFFF, T_NUMBER)
_OPCODE("-",                              IPROC,      OP_SUB,                     1,  0xFFFF, T_NUMBER)
_OPCODE("*",                              IPROC,      OP_MULTI,                   0,  0xFFFF, T_NUMBER)
_OPCODE("/",                              IPROC,      OP_DIV,                     1,  0xFFFF, T_NUMBER)
_OPCODE("abs",                            IPROC,      OP_ABS,                     0,  0,  0)
_OPCODE("quotient",                       IPROC,      OP_QUOTIENT,                0,  0,  0)
_OPCODE("remainder",                      IPROC,      OP_REMAINDER,               0,  0,  0)
_OPCODE("modulo",                         IPROC,      OP_MODULO,                  0,  0,  0)
_OPCODE("gcd",                            IPROC,      OP_GCD,                     0,  0,  0)
_OPCODE("lcm",                            IPROC,      OP_LCM,                     0,  0,  0)
_OPCODE("expt",                           IPROC,      OP_EXPT,                    0,  0,  0)
_OPCODE("sqrt",                           IPROC,      OP_SQRT,                    0,  0,  0)
_OPCODE("numerator",                      IPROC,      OP_NUMERATOR,               0,  0,  0)
_OPCODE("denominator",                    IPROC,      OP_DENOMINATOR,             0,  0,  0)
_OPCODE("rationalize",                    IPROC,      OP_RATIONALIZE,             0,  0,  0)
_OPCODE("floor",                          IPROC,      OP_FLOOR,                   0,  0,  0)
_OPCODE("ceiling",                        IPROC,      OP_CEILING,                 0,  0,  0)
_OPCODE("truncate",                       IPROC,      OP_TRUNCATE,                0,  0,  0)
_OPCODE("round",                          IPROC,      OP_ROUND,                   0,  0,  0)
_OPCODE("inexact->exact",                 IPROC,      OP_INEXACT_TO_EXACT,        0,  0,  0)
_OPCODE("exact->inexact",                 IPROC,      OP_EXACT_TO_INEXACT,        0,  0,  0)
_OPCODE("max",                            IPROC,      OP_MAX,                     0,  0,  0)
_OPCODE("min",                            IPROC,      OP_MIN,                     0,  0,  0)
_OPCODE("sin",                            IPROC,      OP_SIN,                     0,  0,  0)
_OPCODE("cos",                            IPROC,      OP_COS,                     0,  0,  0)
_OPCODE("tan",                            IPROC,      OP_TAN,                     0,  0,  0)
_OPCODE("asin",                           IPROC,      OP_ASIN,                    0,  0,  0)
_OPCODE("acos",                           IPROC,      OP_ACOS,                    0,  0,  0)
_OPCODE("atan",                           IPROC,      OP_ATAN,                    0,  0,  0)
_OPCODE("exp",                            IPROC,      OP_EXP,                     0,  0,  0)
_OPCODE("log",                            IPROC,      OP_LOG,                     0,  0,  0)
_OPCODE("make-rectangular",               IPROC,      OP_MAKE_RECTANGULAR,        0,  0,  0)
_OPCODE("make-polar",                     IPROC,      OP_MAKE_POLAR,              0,  0,  0)
_OPCODE("real-part",                      IPROC,      OP_REAL_PART,               0,  0,  0)
_OPCODE("imag-part",                      IPROC,      OP_IMAG_PART,               0,  0,  0)
_OPCODE("magnitude",                      IPROC,      OP_MAGNITUDE,               0,  0,  0)
_OPCODE("angle",                          IPROC,      OP_ANGLE,                   0,  0,  0)
_OPCODE("number->string",                 IPROC,      OP_NUMBER_TO_STRING,        0,  0,  0)
_OPCODE("integer->char",                  IPROC,      OP_STRING_TO_CHAR,          0,  0,  0)
