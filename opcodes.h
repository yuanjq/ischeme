_OPCODE(op_func0, NULL,                             0,          OP_REPL_READ,               0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_REPL_EVAL,               0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_REPL_PRINT,              0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_REPL_LOOP,               0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_EVAL,                    0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_EVAL_OPC,                0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_EVAL_ARGS,               0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_EVAL_LIST,               0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_APPLY,                   0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_ERROR,                   0,  0,  0)
_OPCODE(op_func0, "define",                         SYNTAX,     OP_DEF0,                    2,  2,  0)
_OPCODE(op_func0, NULL,                             0,          OP_DEF1,                    0,  0,  0)
_OPCODE(op_func0, "lambda",                         SYNTAX,     OP_LAMBDA,                  2,  0xFFFF, 0)
_OPCODE(op_func0, "do",                             SYNTAX,     OP_DO,                      0,  0,  0)
_OPCODE(op_func0, "let",                            SYNTAX,     OP_LET0,                    2,  0xFFFF, 0)
_OPCODE(op_func0, NULL,                             0,          OP_LET1,                    0,  0, 0)
_OPCODE(op_func0, NULL,                             0,          OP_LET2,                    0,  0, 0)
_OPCODE(op_func0, "let*",                           SYNTAX,     OP_LETSEQ0,                 2,  0xFFFF, 0)
_OPCODE(op_func0, NULL,                             0,          OP_LETSEQ1,                 0,  0, 0)
_OPCODE(op_func0, NULL,                             0,          OP_LETSEQ2,                 0,  0, 0)
_OPCODE(op_func0, "letrec",                         SYNTAX,     OP_LETREC,                  0,  0,  0)
_OPCODE(op_func0, "set!",                           SYNTAX,     OP_SET,                     0,  0,  0)
_OPCODE(op_func0, "if",                             SYNTAX,     OP_IF0,                     2,  3,  0)
_OPCODE(op_func0, NULL,                             0,          OP_IF1,                     0,  0,  0)
_OPCODE(op_func0, "cond",                           SYNTAX,     OP_COND,                    0,  0,  0)
_OPCODE(op_func0, "case",                           SYNTAX,     OP_CASE,                    0,  0,  0)
_OPCODE(op_func0, "and",                            SYNTAX,     OP_AND,                     0,  0,  0)
_OPCODE(op_func0, "or",                             SYNTAX,     OP_OR,                      0,  0,  0)
_OPCODE(op_func0, "begin",                          SYNTAX,     OP_BEGIN,                   0,  0,  0)
_OPCODE(op_func0, "define-syntax",                  SYNTAX,     OP_DEF_SYNTAX0,             2,  2,  0)
_OPCODE(op_func0, NULL,                             0,          OP_DEF_SYNTAX1,             0,  0,  0)
_OPCODE(op_func0, NULL,                             0,          OP_DEF_SYNTAX2,             0,  0,  0)
_OPCODE(op_func0, "let-syntax",                     SYNTAX,     OP_LET_SYNTAX,              0,  0,  0)
_OPCODE(op_func0, "letrec-syntax",                  SYNTAX,     OP_LETREC_SYNTAX,           0,  0,  0)
_OPCODE(op_func0, "syntax-rules",                   SYNTAX,     OP_SYNTAX_RULES,            0,  0xFFFF,  0)
_OPCODE(op_func0, "quote",                          SYNTAX,     OP_QUOTE,                   1,  1,  0)
_OPCODE(op_func0, "unquote",                        SYNTAX,     OP_UNQUOTE,                 0,  0,  0)
_OPCODE(op_func0, "quasiquote",                     SYNTAX,     OP_QUASIQUOTE,              0,  0,  0)
_OPCODE(op_func0, "unquote-splicing",               SYNTAX,     OP_UNQUOTE_SPLICING,        0,  0,  0)
_OPCODE(op_func0, "delay",                          SYNTAX,     OP_DELAY,                   0,  0,  0)

_OPCODE(op_func0, "map",                            IPROC,      OP_MAP,                     0,  0,  0)
_OPCODE(op_func0, "for-each",                       IPROC,      OP_FOREACH,                 0,  0,  0)
_OPCODE(op_func0, "eval",                           IPROC,      OP_PEVAL,                   0,  0,  0)
_OPCODE(op_func0, "apply",                          IPROC,      OP_PAPPLY,                  0,  0,  0)
_OPCODE(op_func0, "force",                          IPROC,      OP_FORCE,                   0,  0,  0)
_OPCODE(op_func0, "call-with-current-continuation", IPROC,      OP_CALLCC,                 1,  1,  T_PROC)
_OPCODE(op_func0, "values",                         IPROC,      OP_VALUES,                  0,  0,  0)
_OPCODE(op_func0, "call-with-values",               IPROC,      OP_CALL_WITH_VALUES,        0,  0,  0)
_OPCODE(op_func0, "dynamic-wind",                   IPROC,      OP_DYNAMIC_WIND,            0,  0,  0)
_OPCODE(op_func0, "scheme-report-environment",      IPROC,      OP_SCHEME_REPORT_ENV,       0,  0,  0)
_OPCODE(op_func0, "null-environment",               IPROC,      OP_NULL_ENV,                0,  0,  0)
_OPCODE(op_func0, "interaction-environment ",       IPROC,      OP_INTERACTION_ENV,         0,  0,  0)

_OPCODE(op_func1, "load",                           IPROC,      OP_LOAD,                    0,  0,  0)
_OPCODE(op_func1, "transcript-on",                  IPROC,      OP_TRANSCRIPT_ON,           0,  0,  0)
_OPCODE(op_func1, "transcript-off",                 IPROC,      OP_TRANSCRIPT_OFF,          0,  0,  0)
_OPCODE(op_func1, "display",                        IPROC,      OP_DISPLAY,                 1,  2,  T_ANY T_OUTPORT)
_OPCODE(op_func1, "newline",                        IPROC,      OP_NEWLINE,                 0,  0,  0)
_OPCODE(op_func1, "read",                           IPROC,      OP_READ,                    0,  0,  0)
_OPCODE(op_func1, "write",                          IPROC,      OP_WRITE,                   0,  0,  0)
_OPCODE(op_func1, "read-char",                      IPROC,      OP_READ_CHAR,               0,  0,  0)
_OPCODE(op_func1, "write-char",                     IPROC,      OP_WRITE_CHAR,              0,  0,  0)
_OPCODE(op_func1, "peek-char",                      IPROC,      OP_PEEK_CHAR,               0,  0,  0)
_OPCODE(op_func1, "char-ready?",                    IPROC,      OP_CHAR_READY_P,            0,  0,  0)
_OPCODE(op_func1, "eof-object?",                    IPROC,      OP_EOF_OBJECT_P,            0,  0,  0)
_OPCODE(op_func1, "open-input-file",                IPROC,      OP_OPEN_INPUT_FILE,         0,  0,  0)
_OPCODE(op_func1, "open-output-file",               IPROC,      OP_OPEN_OUTPUT_FILE,        0,  0,  0)
_OPCODE(op_func1, "close-input-port",               IPROC,      OP_CLOSE_INPUT_PORT,        0,  0,  0)
_OPCODE(op_func1, "close-output-port",              IPROC,      OP_CLOSE_OUTPUT_PORT,       0,  0,  0)
_OPCODE(op_func1, "port?",                          IPROC,      OP_PORT_P,                  0,  0,  0)
_OPCODE(op_func1, "input-port?",                    IPROC,      OP_INPUT_PORT_P,            0,  0,  0)
_OPCODE(op_func1, "output-port?",                   IPROC,      OP_OUTPUT_PORT_P,           0,  0,  0)
_OPCODE(op_func1, "current-input-port",             IPROC,      OP_CURR_INPUT_PORT,         0,  0,  0)
_OPCODE(op_func1, "current-output-port",            IPROC,      OP_CURR_OUTPUT_PORT,        0,  0,  0)
_OPCODE(op_func1, "call-with-input-file",           IPROC,      OP_CALL_WITH_INPUT_FILE,    0,  0,  0)
_OPCODE(op_func1, "call-with-output-file",          IPROC,      OP_CALL_WITH_OUTPUT_FILE,   0,  0,  0)
_OPCODE(op_func1, "with-input-from-file",           IPROC,      OP_WITH_INPUT_FROM_FILE,    0,  0,  0)
_OPCODE(op_func1, "with-output-to-file",            IPROC,      OP_WITH_OUTPUT_TO_FILE,     0,  0,  0)

_OPCODE(op_func2, "not",                            IPROC,      OP_NOT,                     0,  0,  0)
_OPCODE(op_func2, "null?",                          IPROC,      OP_NULL_P,                  0,  0,  0)
_OPCODE(op_func2, "boolean?",                       IPROC,      OP_BOOLEAN_P,               0,  0,  0)
_OPCODE(op_func2, "symbol?",                        IPROC,      OP_SYMBOL_P,                0,  0,  0)
_OPCODE(op_func2, "pair?",                          IPROC,      OP_PAIR_P,                  0,  0,  0)
_OPCODE(op_func2, "procedure?",                     IPROC,      OP_PROCEDURE_P,             0,  0,  0)
_OPCODE(op_func2, "eq?",                            IPROC,      OP_EQ_P,                    0,  0,  0)
_OPCODE(op_func2, "eqv?",                           IPROC,      OP_EQV_P,                   0,  0,  0)
_OPCODE(op_func2, "equal?",                         IPROC,      OP_EQUAL_P,                 0,  0,  0)

_OPCODE(op_func2, "char?",                          IPROC,      OP_CHAR_P,                  0,  0,  0)
_OPCODE(op_func2, "char=?",                         IPROC,      OP_CHAR_EP,                 0,  0,  0)
_OPCODE(op_func2, "char<?",                         IPROC,      OP_CHAR_LP,                 0,  0,  0)
_OPCODE(op_func2, "char<=?",                        IPROC,      OP_CHAR_LEP,                0,  0,  0)
_OPCODE(op_func2, "char>?",                         IPROC,      OP_CHAR_GP,                 0,  0,  0)
_OPCODE(op_func2, "char>=?",                        IPROC,      OP_CHAR_GEP,                0,  0,  0)
_OPCODE(op_func2, "char-ci=?",                      IPROC,      OP_CHAR_CI_EP,              0,  0,  0)
_OPCODE(op_func2, "char-ci<?",                      IPROC,      OP_CHAR_CI_LP,              0,  0,  0)
_OPCODE(op_func2, "char-ci<=?",                     IPROC,      OP_CHAR_CI_LEP,             0,  0,  0)
_OPCODE(op_func2, "char-ci>?",                      IPROC,      OP_CHAR_CI_GP,              0,  0,  0)
_OPCODE(op_func2, "char-ci>=?",                     IPROC,      OP_CHAR_CI_GEP,             0,  0,  0)
_OPCODE(op_func2, "char-alphabetic?",               IPROC,      OP_CHAR_ALPHA_P,            0,  0,  0)
_OPCODE(op_func2, "char-numeric?",                  IPROC,      OP_CHAR_NUMBER_P,           0,  0,  0)
_OPCODE(op_func2, "char-whitespace?",               IPROC,      OP_CHAR_SPACE_P,            0,  0,  0)
_OPCODE(op_func2, "char-upper-case?",               IPROC,      OP_CHAR_UPPER_P,            0,  0,  0)
_OPCODE(op_func2, "char-lower-case?",               IPROC,      OP_CHAR_LOWER_P,            0,  0,  0)

_OPCODE(op_func2, "string?",                        IPROC,      OP_STRING_P,                0,  0,  0)
_OPCODE(op_func2, "string=?",                       IPROC,      OP_STRING_EP,               0,  0,  0)
_OPCODE(op_func2, "string<?",                       IPROC,      OP_STRING_LP,               0,  0,  0)
_OPCODE(op_func2, "string<=?",                      IPROC,      OP_STRING_LEP,              0,  0,  0)
_OPCODE(op_func2, "string>?",                       IPROC,      OP_STRING_GP,               0,  0,  0)
_OPCODE(op_func2, "string>=?",                      IPROC,      OP_STRING_GEP,              0,  0,  0)
_OPCODE(op_func2, "string-ci=?",                    IPROC,      OP_STRING_CI_EP,            0,  0,  0)
_OPCODE(op_func2, "string-ci<?",                    IPROC,      OP_STRING_CI_LP,            0,  0,  0)
_OPCODE(op_func2, "string-ci<=?",                   IPROC,      OP_STRING_CI_LEP,           0,  0,  0)
_OPCODE(op_func2, "string-ci>?",                    IPROC,      OP_STRING_CI_GP,            0,  0,  0)
_OPCODE(op_func2, "string-ci>=?",                   IPROC,      OP_STRING_CI_GEP,           0,  0,  0)

_OPCODE(op_func2, "vector?",                        IPROC,      OP_VECTOR_P,                0,  0,  0)
_OPCODE(op_func2, "vector",                         IPROC,      OP_VECTOR,                  0,  0,  0)
_OPCODE(op_func2, "make-vector",                    IPROC,      OP_MAKE_VECTOR,             0,  0,  0)
_OPCODE(op_func2, "vector-length",                  IPROC,      OP_VECTOR_LENGTH,           0,  0,  0)
_OPCODE(op_func2, "vector-ref",                     IPROC,      OP_VECTOR_REF,              0,  0,  0)
_OPCODE(op_func2, "vector-set!",                    IPROC,      OP_VECTOR_SET,              0,  0,  0)
_OPCODE(op_func2, "vector-fill!",                   IPROC,      OP_VECTOR_FILL,             0,  0,  0)
_OPCODE(op_func2, "vector->list",                   IPROC,      OP_VECTOR_TO_LIST,          0,  0,  0)
_OPCODE(op_func2, "list?",                          IPROC,      OP_LIST_P,                  0,  0,  0)
_OPCODE(op_func2, "list",                           IPROC,      OP_LIST,                    0,  0,  0)
_OPCODE(op_func2, "list->string",                   IPROC,      OP_LIST_TO_STRING,          0,  0,  0)
_OPCODE(op_func2, "list->vector",                   IPROC,      OP_LIST_TO_VECTOR,          0,  0,  0)
_OPCODE(op_func2, "symbol->string",                 IPROC,      OP_SYMBOL_STRING,           0,  0,  0)
_OPCODE(op_func2, "string",                         IPROC,      OP_STRING,                  0,  0,  0)
_OPCODE(op_func2, "make-string",                    IPROC,      OP_MAKE_STRING,             0,  0,  0)
_OPCODE(op_func2, "string-length",                  IPROC,      OP_STRING_LENGTH,           0,  0,  0)
_OPCODE(op_func2, "string-ref",                     IPROC,      OP_STRING_REF,              0,  0,  0)
_OPCODE(op_func2, "string-set!",                    IPROC,      OP_STRING_SET,              0,  0,  0)
_OPCODE(op_func2, "substring",                      IPROC,      OP_SUBSTRING,               0,  0,  0)
_OPCODE(op_func2, "string-append",                  IPROC,      OP_STRING_APPEND,           0,  0,  0)
_OPCODE(op_func2, "string-copy",                    IPROC,      OP_STRING_COPY,             0,  0,  0)
_OPCODE(op_func2, "string-fill!",                   IPROC,      OP_STRING_FILL,             0,  0,  0)
_OPCODE(op_func2, "string->symbol",                 IPROC,      OP_STRING_TO_SYMBOL,        0,  0,  0)
_OPCODE(op_func2, "string->list",                   IPROC,      OP_STRING_TO_LIST,          0,  0,  0)
_OPCODE(op_func2, "string->number",                 IPROC,      OP_STRING_TO_NUMBER,        0,  0,  0)
_OPCODE(op_func2, "char->integer",                  IPROC,      OP_CHAR_TO_INTEGER,         0,  0,  0)
_OPCODE(op_func2, "char-upcase",                    IPROC,      OP_CHAR_UPCASE,             0,  0,  0)
_OPCODE(op_func2, "char-downcase",                  IPROC,      OP_CHAR_DOWNCASE,           0,  0,  0)

_OPCODE(op_func2, "cons",                           IPROC,      OP_CONS,                    0,  0,  0)
_OPCODE(op_func2, "car",                            IPROC,      OP_CAR,                     0,  0,  0)
_OPCODE(op_func2, "cdr",                            IPROC,      OP_CDR,                     0,  0,  0)
_OPCODE(op_func2, "set-car!",                       IPROC,      OP_SET_CAR,                 0,  0,  0)
_OPCODE(op_func2, "set-cdr!",                       IPROC,      OP_SET_CDR,                 0,  0,  0)
_OPCODE(op_func2, "length",                         IPROC,      OP_LENGTH,                  0,  0,  0)
_OPCODE(op_func2, "append",                         IPROC,      OP_APPEND,                  0,  0,  0)
_OPCODE(op_func2, "reverse",                        IPROC,      OP_REVERSE,                 0,  0,  0)
_OPCODE(op_func2, "list-tail",                      IPROC,      OP_LIST_TAIL,               0,  0,  0)
_OPCODE(op_func2, "list-ref",                       IPROC,      OP_LIST_REF,                0,  0,  0)
_OPCODE(op_func2, "memq",                           IPROC,      OP_MEMQ,                    0,  0,  0)
_OPCODE(op_func2, "memv",                           IPROC,      OP_MEMV,                    0,  0,  0)
_OPCODE(op_func2, "member",                         IPROC,      OP_MEMBER,                  0,  0,  0)
_OPCODE(op_func2, "assq",                           IPROC,      OP_ASSQ,                    0,  0,  0)
_OPCODE(op_func2, "assv",                           IPROC,      OP_ASSV,                    0,  0,  0)
_OPCODE(op_func2, "assoc",                          IPROC,      OP_ASSOC,                   0,  0,  0)

_OPCODE(op_func3, "number?",                        IPROC,      OP_NUMBER_P,                0,  0,  0)
_OPCODE(op_func3, "integer?",                       IPROC,      OP_INTEGER_P,               0,  0,  0)
_OPCODE(op_func3, "rational?",                      IPROC,      OP_RATIONAL_P,              0,  0,  0)
_OPCODE(op_func3, "real?",                          IPROC,      OP_REAL_P,                  0,  0,  0)
_OPCODE(op_func3, "complex?",                       IPROC,      OP_COMPLEX_P,               0,  0,  0)
_OPCODE(op_func3, "exact?",                         IPROC,      OP_EXACT_P,                 0,  0,  0)
_OPCODE(op_func3, "inexact?",                       IPROC,      OP_INEXACT_P,               0,  0,  0)
_OPCODE(op_func3, "zero?",                          IPROC,      OP_ZERO_P,                  0,  0,  0)
_OPCODE(op_func3, "negative?",                      IPROC,      OP_NEGATIVE_P,              0,  0,  0)
_OPCODE(op_func3, "positive?",                      IPROC,      OP_POSITIVE_P,              0,  0,  0)
_OPCODE(op_func3, "odd?",                           IPROC,      OP_ODD_P,                   0,  0,  0)
_OPCODE(op_func3, "even?",                          IPROC,      OP_EVEN_P,                  0,  0,  0)
_OPCODE(op_func3, "<",                              IPROC,      OP_LP,                      0,  0,  0)
_OPCODE(op_func3, "<=",                             IPROC,      OP_LEP,                     0,  0,  0)
_OPCODE(op_func3, ">",                              IPROC,      OP_GP,                      0,  0,  0)
_OPCODE(op_func3, ">=",                             IPROC,      OP_GEP,                     0,  0,  0)
_OPCODE(op_func3, "=",                              IPROC,      OP_EP,                      0,  0,  0)
_OPCODE(op_func3, "+",                              IPROC,      OP_ADD,                     0,  0xFFFF, T_NUMBER)
_OPCODE(op_func3, "-",                              IPROC,      OP_SUB,                     0,  0xFFFF, T_NUMBER)
_OPCODE(op_func3, "*",                              IPROC,      OP_MULTI,                   0,  0xFFFF, T_NUMBER)
_OPCODE(op_func3, "/",                              IPROC,      OP_DIV,                     1,  0xFFFF, T_NUMBER)
_OPCODE(op_func3, "abs",                            IPROC,      OP_ABS,                     0,  0,  0)
_OPCODE(op_func3, "quotient",                       IPROC,      OP_QUOTIENT,                0,  0,  0)
_OPCODE(op_func3, "remainder",                      IPROC,      OP_REMAINDER,               0,  0,  0)
_OPCODE(op_func3, "modulo",                         IPROC,      OP_MODULO,                  0,  0,  0)
_OPCODE(op_func3, "gcd",                            IPROC,      OP_GCD,                     0,  0,  0)
_OPCODE(op_func3, "lcm",                            IPROC,      OP_LCM,                     0,  0,  0)
_OPCODE(op_func3, "expt",                           IPROC,      OP_EXPT,                    0,  0,  0)
_OPCODE(op_func3, "sqrt",                           IPROC,      OP_SQRT,                    0,  0,  0)
_OPCODE(op_func3, "numerator",                      IPROC,      OP_NUMERATOR,               0,  0,  0)
_OPCODE(op_func3, "denominator",                    IPROC,      OP_DENOMINATOR,             0,  0,  0)
_OPCODE(op_func3, "rationalize",                    IPROC,      OP_RATIONALIZE,             0,  0,  0)
_OPCODE(op_func3, "floor",                          IPROC,      OP_FLOOR,                   0,  0,  0)
_OPCODE(op_func3, "ceiling",                        IPROC,      OP_CEILING,                 0,  0,  0)
_OPCODE(op_func3, "truncate",                       IPROC,      OP_TRUNCATE,                0,  0,  0)
_OPCODE(op_func3, "round",                          IPROC,      OP_ROUND,                   0,  0,  0)
_OPCODE(op_func3, "inexact->exact",                 IPROC,      OP_INEXACT_TO_EXACT,        0,  0,  0)
_OPCODE(op_func3, "exact->inexact",                 IPROC,      OP_EXACT_TO_INEXACT,        0,  0,  0)
_OPCODE(op_func3, "max",                            IPROC,      OP_MAX,                     0,  0,  0)
_OPCODE(op_func3, "min",                            IPROC,      OP_MIN,                     0,  0,  0)
_OPCODE(op_func3, "sin",                            IPROC,      OP_SIN,                     0,  0,  0)
_OPCODE(op_func3, "cos",                            IPROC,      OP_COS,                     0,  0,  0)
_OPCODE(op_func3, "tan",                            IPROC,      OP_TAN,                     0,  0,  0)
_OPCODE(op_func3, "asin",                           IPROC,      OP_ASIN,                    0,  0,  0)
_OPCODE(op_func3, "acos",                           IPROC,      OP_ACOS,                    0,  0,  0)
_OPCODE(op_func3, "atan",                           IPROC,      OP_ATAN,                    0,  0,  0)
_OPCODE(op_func3, "exp",                            IPROC,      OP_EXP,                     0,  0,  0)
_OPCODE(op_func3, "log",                            IPROC,      OP_LOG,                     0,  0,  0)
_OPCODE(op_func3, "make-rectangular",               IPROC,      OP_MAKE_RECTANGULAR,        0,  0,  0)
_OPCODE(op_func3, "make-polar",                     IPROC,      OP_MAKE_POLAR,              0,  0,  0)
_OPCODE(op_func3, "real-part",                      IPROC,      OP_REAL_PART,               0,  0,  0)
_OPCODE(op_func3, "imag-part",                      IPROC,      OP_IMAG_PART,               0,  0,  0)
_OPCODE(op_func3, "magnitude",                      IPROC,      OP_MAGNITUDE,               0,  0,  0)
_OPCODE(op_func3, "angle",                          IPROC,      OP_ANGLE,                   0,  0,  0)
_OPCODE(op_func3, "number->string",                 IPROC,      OP_NUMBER_TO_STRING,        0,  0,  0)
_OPCODE(op_func3, "integer->char",                  IPROC,      OP_STRING_TO_CHAR,          0,  0,  0)
