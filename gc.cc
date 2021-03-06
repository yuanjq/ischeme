#include <string.h>
#include "cell.h"
#include "ischeme.h"

//#define GC_DEBUG
#define is_valid_object(c)          (c && (c)->ptrtag == POINTER_TAG)
#define is_marked(c)                cell_markedp(c)

#ifdef GC_DEBUG
#include <sys/time.h>
int g_gc_total = 0;
#endif

struct SegFreeList {
  uint size;
  SegFreeList *next;
};

struct Segment {
  uint size;
  SegFreeList *free_list;
  Segment *next;
  uchar *data;
};

static uint segment_total_size(Segment *seg) {
  uint total_size = 0;
  for (; seg; seg=seg->next)
    total_size += seg->size;
  return total_size;
}

Segment *cell_mk_segment(uint size) {
    Segment *seg;
    SegFreeList *list, *next;

    if (size < ISC_SEG_SIZE) {
        size = ISC_SEG_SIZE;
    }
    size = segment_align_size(size);
    seg = (Segment *)cell_malloc(size);
    if (!seg) return NULL;
    seg->size = size;
    seg->data = (uchar*)seg + segment_align(S(Segment));
    list = seg->free_list = (SegFreeList*) seg->data;
    seg->next = NULL;
    next = (SegFreeList*) ((uchar*)list + segment_align(S(SegFreeList)));
    list->size = 0;
    list->next = next;
    next->size = size - segment_align(S(Segment)) - segment_align(S(SegFreeList));
    next->next = NULL;
    return seg;
}

static bool cell_grow_segment(Cell *ctx, uint size) {
    uint total_size, grow_to;
    Segment *tmp = NULL, *seg = ctx_segments(ctx);

    size = segment_align((uint)((size < ISC_SEG_SIZE) ? ISC_SEG_SIZE : size));
    total_size = segment_total_size(seg);
    grow_to = total_size * ISC_SEG_GROW_RATIO;
    while (total_size + size <= ISC_SEG_MAX_SIZE) {
        if (total_size >= grow_to) {
            break;
        }
        tmp = cell_mk_segment(size);
        tmp->next = seg->next;
        seg->next = tmp;
        total_size += size;
    }
    return tmp != NULL;
}

static void *cell_try_alloc(Cell *ctx, uint size) {
    Segment *seg;
    SegFreeList *ls1, *ls2, *ls3;
    for (seg=ctx_segments(ctx); seg; seg=seg->next) {
        for (ls1=seg->free_list, ls2=ls1->next; ls2; ls1=ls2, ls2=ls2->next) {
            if (ls2->size >= size) {
                ls3 = (SegFreeList*) ((uchar*)ls2 + size);
                if (((uchar*)ls3) + S(SegFreeList) <= ((uchar*)ls2) + ls2->size) {
                    uint sz = ls2->size;
                    SegFreeList *nx = ls2->next;
                    ls3->size = sz - size;
                    ls3->next = nx;
                    ls1->next = ls3;
                } else {
                    ls1->next = ls2->next;
                }
                memset((void*)ls2, 0, size);
                return ls2;
            }
        }
    }
    return NULL;
}

static uint cell_mark(Cell *ctx, Cell *c) {
    if (!is_valid_object(c) || is_marked(c)) {
        return 0;
    }
    uint n = 1;
    cell_markedp(c) = true;
    switch (cell_type(c)) {
    case CHAR:
    case BOOLEAN:
    case STRING:
    case SYMBOL:
    case SYNTAX:
    case IPROC:
    case EPROC:
        break;
    case PORT:
        if ((port_type(c) & PORT_FILE) &&
                port_file_name(c)) {
            n += cell_mark(ctx, port_file_name(c));
        } else if (port_type(c) & PORT_STRING) {
            n += cell_mark(ctx, port_string_src(c));
        }
        break;
    case NUMBER:
        switch (number_type(c)) {
        case NUMBER_FRACTION:
            n += cell_mark(ctx, number_fn_nr(c));
            n += cell_mark(ctx, number_fn_dr(c));
            break;
        case NUMBER_COMPLEX:
            n += cell_mark(ctx, number_cx_rl(c));
            n += cell_mark(ctx, number_cx_im(c));
            break;
        }
        break;
    case PAIR:
    case LIST:
        for (;;) {
            n += cell_mark(ctx, car(c));
            c = cdr(c);
            if (is_pair(c)) {
                if (!is_marked(c)) {
                    cell_markedp(c) = true;
                    n += 1;
                }
            } else {
                n += cell_mark(ctx, c);
                break;
            }
        }
        break;
    case VECTOR: {
            int len = vector_length(c);
            for (int i=0; i<len; i++) {
                n += cell_mark(ctx, vector_data(c)[i]);
            }
        }
        break; 
    case PROC:
        if (proc_name(c)) {
            n += cell_mark(ctx, proc_name(c));
        }
        n += cell_mark(ctx, proc_closure(c));
        break;
    case CLOSURE:
        n += cell_mark(ctx, closure_args(c));
        n += cell_mark(ctx, closure_code(c));
        n += cell_mark(ctx, closure_env(c));
        break;
    case CONTEXT: {
        n += cell_mark(ctx, ctx_global_env(c));
        n += cell_mark(ctx, ctx_r5rs_env(c));
        n += cell_mark(ctx, ctx_null_env(c));
        n += cell_mark(ctx, ctx_symbols(c));
        n += cell_mark(ctx, ctx_stdinport(c));
        n += cell_mark(ctx, ctx_stdoutport(c));
        n += cell_mark(ctx, ctx_stderrport(c));
        n += cell_mark(ctx, ctx_inport(c));
        n += cell_mark(ctx, ctx_outport(c));
        if (ctx_transc_port(ctx)) {
            n += cell_mark(ctx, ctx_transc_port(ctx));
        }
        vector<Cell*> inports = ctx_inports(ctx);
        vector<Cell*>::iterator it;
        for (it = inports.begin(); it != inports.end(); ++it) {
            n += cell_mark(ctx, *it);
        }
        n += cell_mark(ctx, ctx_ret(c));
        n += cell_mark(ctx, ctx_args(c));
        n += cell_mark(ctx, ctx_env(c));
        n += cell_mark(ctx, ctx_code(c));
        n += cell_mark(ctx, ctx_data(c));
        n += cell_mark(ctx, ctx_instructs(c));
        n += cell_mark(ctx, ctx_winds(c));
        for (Preserved *saves=ctx_saves(c); saves; saves=saves->next) {
            n += cell_mark(ctx, *(saves->var));
        }
        break;
    }
    case MACRO:
        n += cell_mark(ctx, macro_matchers(c));
        n += cell_mark(ctx, macro_env(c));
        break;
    case MATCHER:
        if (matcher_name(c)) {
            n += cell_mark(ctx, matcher_name(c));
        }
        if (matcher_value(c)) {
            n += cell_mark(ctx, matcher_value(c));
        }
        break;
    case EXPANDER:
        if (expander_name(c)) {
            n += cell_mark(ctx, expander_name(c));
        }
        if (expander_value(c)) {
            n += cell_mark(ctx, expander_value(c));
        }
        break;
    case CLOSURE_EXPR:
        n += cell_mark(ctx, closure_expr_expr(c));
        n += cell_mark(ctx, closure_expr_env(c));
        break;
    case INSTRUCT:
        n += cell_mark(ctx, instruct_args(c));
        n += cell_mark(ctx, instruct_code(c));
        n += cell_mark(ctx, instruct_data(c));
        n += cell_mark(ctx, instruct_env(c));
        break;
    case CONTINUE:
        n += cell_mark(ctx, continue_ins(c));
        n += cell_mark(ctx, continue_winds(c));
        break;
    case EXCEPTION:
        if (exception_msg(c)) {
            n += cell_mark(ctx, exception_msg(c));
        }
        if (exception_src(c)) {
            n += cell_mark(ctx, exception_src(c));
        }
        if (exception_trg(c)) {
            n += cell_mark(ctx, exception_trg(c));
        }
        break;
    case PROMISE:
        n += cell_mark(ctx, promise_result(c));
        n += cell_mark(ctx, promise_expr(c));
        break;
    case MULTIVAR:
        n += cell_mark(ctx, multivar_var(c));
        break;
    case ENV: {
        map<char*,Cell*,ptrCmp> *mp;
        map<char*,Cell*,ptrCmp>::iterator it;
        for (;;) {
            mp = &env_map(c);
            for (it=mp->begin(); it!=mp->end(); ++it) {
                n += cell_mark(ctx, (*it).second);
            }
            c=env_outer(c);
            if (!is_env(c)) break;
            if (!is_marked(c)) {
                cell_markedp(c) = true;
                n += 1;
            }
        }
        break;
    }
    default:
        return 0;
    }
    return n;
}

static int _sizeof_cell(Cell *c) {
    int s = 0;
    switch (cell_type(c)) {
    case CHAR:
        s = cell_sizeof(chr);
        break;
    case BOOLEAN:
        s = cell_sizeof(bl);
        break;
    case NUMBER:
        s = cell_sizeof(num);
        break;
    case STRING:
    case SYMBOL:
        s = cell_sizeof(str) + string_size(c) + 1;
        break;
    case SYNTAX:
    case IPROC:
        s = cell_sizeof(op);
        break;
    case PAIR:
    case LIST:
    case CONTINUE:
        s = cell_sizeof(pair);
        break;
    case VECTOR:
        s = cell_sizeof(vect) + vector_length(c) * S(Cell*);
        break;
    case PORT:
        if (port_type(c) & PORT_STRING) 
            s = cell_sizeof(port.s);
        else
            s = cell_sizeof(port.f);
        break;
    case PROC:
        s = cell_sizeof(proc);
        break;
    case EPROC:
        s = cell_sizeof(eproc);
        break;
    case CLOSURE:
        s = cell_sizeof(clos);
        break;
    case CONTEXT:
        s = cell_sizeof(ctx);
        break;
    case MACRO:
        s = cell_sizeof(macro);
        break;
    case MATCHER:
        s = cell_sizeof(mt);
        break;
    case EXPANDER:
        s = cell_sizeof(expd);
        break;
    case CLOSURE_EXPR:
        s = cell_sizeof(closexpr);
        break;
    case INSTRUCT:
        s = cell_sizeof(inst);
        break;
    case EXCEPTION:
        s = cell_sizeof(excpt);
        break;
    case PROMISE:
        s = cell_sizeof(proms);
        break;
    case ENV:
        s = cell_sizeof(env);
        break;
    default:
        printf("gc error: invalid type!\n");
        break;
    }
    return segment_align(s);
}

static uint cell_sweep(Cell *ctx, uint *sum_freed, uint *max_freed) {
    uint total = 0, size;
    Segment *seg = ctx_segments(ctx);
    SegFreeList *p, *q, *r;
    *sum_freed = *max_freed = 0;
    for (Segment *it=seg; it; it=it->next) {
        uchar* st = it->data + segment_align(S(SegFreeList));
        uchar* ed = ((uchar*)it) + it->size;
        p = (SegFreeList*)st;
        q = r = it->free_list;
        while ((uchar*)p < ed) {
            if (q && p >= q) {
                for (; q && q<=p; r=q, q=q->next);
            }
            if ((uchar*)p < ((uchar*)r) + r->size) {
                p = (SegFreeList*) ((uchar*)r + segment_align(r->size));
            }
            if (is_valid_object((Cell*)p)) {
                if (!cell_markedp((Cell*)p)) {
                    size = _sizeof_cell((Cell*)p);
                    if (is_env((Cell*)p)) {
                        env_map((Cell*)p).~map<char*,Cell*,ptrCmp>();
                    }
                    cell_ptrtag(((Cell*)p)) = 0;
                    if ((uchar*)p == ((uchar*)r) + r->size && ((uchar*)p) + size == (uchar*)q) {
                        size += q->size;
                        r->size += size;
                        r->next = q->next;
                        q = q->next;
                    } else if ((uchar*)p == ((uchar*)r) + r->size) {
                        r->size += size;
                    } else if (((uchar*)p) + size == (uchar*)q) {
                        r->next = p;
                        p->size = size + q->size;
                        p->next = q->next;
                        r = p;
                        q = p->next;
                    } else {
                        r->next = p;
                        p->next = q;
                        p->size = size;
                        r = p;
                    }
                    if (size > *max_freed) {
                        *max_freed = size;
                    }
                    *sum_freed += size;
                    ++total;
                    p = (SegFreeList*)(((uchar*)p) + segment_align(size));
                } else {
                    cell_markedp(((Cell*)p)) = 0;
                    p = (SegFreeList*)(((uchar*)p) + segment_align(_sizeof_cell(((Cell*)p))));
                }
            } else {
                p = (SegFreeList*)(((uchar*)p) + segment_align(1));
            }
        }
    }
    return total;
}

uint cell_gc(Cell *ctx, uint *max_free) {
    uint sum_free, sum_marked, sum_swept;
#ifdef GC_DEBUG
    struct timeval mst, med, sed;
    gettimeofday(&mst, NULL);
#endif
    sum_marked = cell_mark(ctx, ctx);
#ifdef GC_DEBUG
    gettimeofday(&med, NULL);
#endif
    sum_swept = cell_sweep(ctx, &sum_free, max_free);
#ifdef GC_DEBUG
    gettimeofday(&sed, NULL);
    printf("\n** GC DEBUG **\n");
    printf("total:%d, marked:%d, swept:%d, freed:%d\n", g_gc_total, sum_marked, sum_swept, sum_free);
    printf("marked cost time: %ldms\n", med.tv_sec * 1000 + med.tv_usec / 1000 - mst.tv_sec * 1000 - mst.tv_usec / 1000);
    printf("swept  cost time: %ldms\n", sed.tv_sec * 1000 + sed.tv_usec / 1000 - med.tv_sec * 1000 - med.tv_usec / 1000);
    g_gc_total = g_gc_total - sum_swept;
#endif
    return sum_free;
}

void *cell_alloc(Cell *ctx, uint size) {
    void *res;
    uint max_freed, sum_freed, total_size;

    Segment *seg = ctx_segments(ctx);
    size = segment_align(size);
    res = cell_try_alloc(ctx, size);
    if (!res) {
        sum_freed = cell_gc(ctx, &max_freed);
        total_size = segment_total_size(ctx_segments(ctx));
        if ((max_freed < size || (total_size - sum_freed > total_size * ISC_SEG_GROW_THRESHOLD)) && total_size + size < ISC_SEG_MAX_SIZE) {
            cell_grow_segment(ctx, size);
        }
        res = cell_try_alloc(ctx, size);
        if (!res) {
            // TODO: out of memory 
            printf("no memory!\n");
            exit(-1);
        }
    }
#ifdef GC_DEBUG
    ++g_gc_total;
#endif
    return res;
}
