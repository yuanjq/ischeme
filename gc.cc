#include <string.h>
#include "cell.h"

#define ISC_SEG_NUM                 32
#define ISC_SEG_SIZE                (8*1024*1024)
#define ISC_SEG_MAX_SIZE            (ISC_SEG_NUM*ISC_SEG_SIZE)
#define ISC_SEG_GROW_THRESHOLD      0.75
#define ISC_SEG_REDUCE_THRESHOLD    0.25
#define ISC_SEG_GROW_RATIO          1.5

struct SegFreeList {
  uint size;
  SegFreeList *next;
};

struct Segment {
  uint size;
  SegFreeList *free_list;
  Segment *next;
  char *data;
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
    seg->data = (char*) segment_align(S(seg->data) + (ulong)&(seg->data));
    list = seg->free_list = (SegFreeList*) seg->data;
    seg->next = NULL;
    next = (SegFreeList*) ((char*)list + segment_align(S(SegFreeList)));
    list->size = 0;
    list->next = next;
    next->size = size - segment_align(S(Segment) + S(SegFreeList));
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
                ls3 = (SegFreeList*) ((char*)ls2 + size);
                if (ls3 + S(SegFreeList) <= ls2 + ls2->size) {
                    ls3->size = ls2->size - size;
                    ls3->next = ls2->next;
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

uint cell_gc(Cell *ctx, uint *sum) {
    // TODO: gc
    return 0;
}

void *cell_alloc(Cell *ctx, uint size) {
    void *res;
    uint max_freed, sum_freed, total_size;

    Segment *seg = ctx_segments(ctx);
    size = segment_align(size);
    res = cell_try_alloc(ctx, size);
    if (!res) {
        max_freed = cell_gc(ctx, &sum_freed);
        total_size = segment_total_size(ctx_segments(ctx));
        if ((max_freed < size || (total_size - sum_freed > total_size * ISC_SEG_GROW_THRESHOLD)) && total_size + size < ISC_SEG_MAX_SIZE) {
            cell_grow_segment(ctx, size);
        }
        res = cell_try_alloc(ctx, size);
        if (!res) {
            // TODO: out of memory 
            printf("no memory!\n");
            while(1); 
        }
    }
    return res;
}
