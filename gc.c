#include <string.h>
#include "ischeme.h"

#define SEGMENT_GROW_THRESHOLD      0.75
#define SEGMENT_GROW_RATIO          1.5


static uint segment_total_size(Segment *seg) {
  uint total_size = 0;
  for (; seg; seg=seg->next)
    total_size += seg->size;
  return total_size;
}

Segment *cell_mk_segment(int size, int max_size) {
    Segment *seg;
    SegFreeList *list, *next;

    seg = cell_malloc(segment_align_size(size));
    if (!seg) return NULL;
    seg->size = size;
    seg->max_size = max_size;
    seg->data = (char*) segment_align(S(seg->data) + (uint)&(seg->data));
    list = seg->free_list = (SegFreeList*) seg->data;
    seg->next = NULL;
    next = (SegFreeList*) ((char*)list + segment_align(S(SegFreeList)));
    list->size = 0;
    list->next = next;
    next->size = size - segment_align(S(SegFreeList));
    next->next = NULL;
    return seg;
}

static uint cell_grow_segment(Cell *ctx, uint size) {
    uint cur_size, new_size;
    Segment *tmp, *seg = ctx_segments(ctx);

    cur_size = seg->size;
    new_size = segment_align((uint)(((cur_size > size) ? cur_size : size) * SEGMENT_GROW_RATIO));
    tmp = cell_mk_segment(new_size, seg->max_size);
    tmp->next = seg->next;
    seg->next = tmp;
    return tmp != NULL;
}

static void *cell_try_alloc(Cell *ctx, uint size) {
    Segment *seg;
    SegFreeList *ls1, *ls2, *ls3;
    for (seg=ctx_segments(ctx); seg; seg=seg->next) {
        for (ls1=seg->free_list, ls2=ls1->next; ls2; ls1=ls2, ls2=ls2->next) {
            if (ls2->size >= size) {
                ls3 = (SegFreeList*) ((char*)ls2 + size);
                ls3->size = ls2->size - size;
                ls3->next = ls2->next;
                ls1->next = ls3;
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
        if ((max_freed < size || (total_size - sum_freed > total_size * SEGMENT_GROW_THRESHOLD)) && total_size + size < seg->max_size) {
            cell_grow_segment(ctx, size);
        }
        res = cell_try_alloc(ctx, size);
        if (!res) {
            // TODO: out of memory 
            while(1) printf("no memory.\n");
        }
    }
    return res;
}
