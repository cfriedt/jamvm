/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007
 * Robert Lougher <rob@lougher.org.uk>.
 *
 * This file is part of JamVM.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Must be included first to get configure options */
#include "jam.h"

#ifdef INLINING
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include "hash.h"

#ifdef TRACE
#define TRACE_INLINING(fmt, ...) jam_printf(fmt, ## __VA_ARGS__)
#else
#define TRACE_INLINING(fmt, ...)
#endif

#define HASHTABSZE 1<<10
#define HASH(ptr) codeBlockHash(ptr)
#define COMPARE(ptr1, ptr2, hash1, hash2) ((ptr2 != DELETED) && \
                                           (hash1 == hash2) && codeBlockComp(ptr1, ptr2))
#define PREPARE(ptr) allocCodeBlock(ptr)
#define FOUND(ptr) foundExistingBlock(ptr)
#define SCAVENGE(ptr) ptr == DELETED
#define DELETED ((void*)-1)

/* Global lock for ... */
static VMWaitLock quick_prepare_lock;

#define LABELS_SIZE  256
#define GOTO_START   230
#define GOTO_END     255

#ifdef USE_CACHE
#define HANDLERS     3
#define ENTRY_LABELS 0
#define START_LABELS 3
#define END_LABELS   6
#else
#define HANDLERS     1
#define ENTRY_LABELS 0
#define START_LABELS 1
#define END_LABELS   2
#endif

#define CODE_INCREMENT 128*KB
#define ALIGN(size) ROUND(size, sizeof(CodeBlockHeader))
#define ROUND(size, round) (size + round - 1) / round * round

typedef struct code_block_header {
    int len;
    union {
        int ref_count;
        struct code_block_header *next;
    } u;
} CodeBlockHeader;

static HashTable code_hash_table;

static int replication_threshold;

static int code_size = 0;
static int sys_page_size;
static int code_increment;
static unsigned int max_code_size;
static CodeBlockHeader *code_free_list = NULL;

static char *min_entry_point = (char*)-1;
static char *max_entry_point  = NULL;

int inlining_inited = FALSE;
static int handler_sizes[HANDLERS][LABELS_SIZE];
static char **handler_entry_points[HANDLERS];
static char *goto_start;
static int goto_len;

int compare(const void *pntr1, const void *pntr2) {
    char *v1 = *(char **)pntr1;
    char *v2 = *(char **)pntr2;

    return v1 - v2;
}

char *findNextLabel(char **pntrs, char *pntr) {
    int i = 0;

    for(i = 0; i < LABELS_SIZE; i++)
        if(pntrs[i] > pntr)
            return pntrs[i];

    return NULL;
}

#define SHOWRELOC(args, fmt, ...) do {   \
    if(args->showreloc)                  \
        jam_printf(fmt, ## __VA_ARGS__); \
} while(0)

int checkRelocatability(InitArgs *args) {
    char ***handlers1 = (char ***)executeJava();
    char ***handlers2 = (char ***)executeJava2();
    char *sorted_ends[LABELS_SIZE];
    int i;

    /* Check relocatability of the indirect goto.  This is copied onto the end
       of each super-instruction.  If this is un-relocatable,  inlining is
       disabled. */

    goto_start = handlers1[ENTRY_LABELS][GOTO_START];
    goto_len = handlers1[ENTRY_LABELS][GOTO_END] - goto_start;

    if(!memcmp(goto_start, handlers2[ENTRY_LABELS][GOTO_START], goto_len))
        SHOWRELOC(args, "Goto is relocatable\n");
    else {
        SHOWRELOC(args, "Goto is not relocatable : disabling inlining.\n");
        return FALSE;
    }

    /* Check relocatability of each handler. */

    for(i = 0; i < HANDLERS; i++) {
        int j;

        memcpy(sorted_ends, handlers1[END_LABELS+i], LABELS_SIZE * sizeof(char *));
        qsort(sorted_ends, LABELS_SIZE, sizeof(char *), compare);

        for(j = 0; j < LABELS_SIZE; j++) {
            char *entry = handlers1[ENTRY_LABELS+i][j];
            char *end = handlers1[END_LABELS+i][j];
            int len = end - entry;

            if(entry < min_entry_point)
                min_entry_point = entry;

            if(entry > max_entry_point)
                max_entry_point = entry;

            if(len > 0) {
                char *nearest_end = findNextLabel(sorted_ends, entry);

                if(nearest_end == end)
                    if(!memcmp(entry, handlers2[ENTRY_LABELS+i][j], len)) {
                        SHOWRELOC(args, "Handler %d is relocatable\n", j);
                        handler_sizes[i][j] = len;
                        continue;
                    } else
                        SHOWRELOC(args, "Memcmp failed : handler %d is not relocatable\n", j);
                else
                    SHOWRELOC(args, "Re-ordered end label : handler %d is not relocatable\n", j);
            } else
                SHOWRELOC(args, "End label < entry : handler %d is not relocatable\n", j);

            /* Flag handler as non-relocatable */
            handler_sizes[i][j] = -1;
        }

        handler_entry_points[i] = handlers1[ENTRY_LABELS+i];
    }

    return TRUE;
}

int initialiseInlining(InitArgs *args) {
    int enabled = args->codemem > 0 ? checkRelocatability(args) : FALSE;

    if(enabled) {
        initVMWaitLock(quick_prepare_lock);
        initHashTable(code_hash_table, HASHTABSZE, TRUE);

        sys_page_size = getpagesize();
        max_code_size = ROUND(args->codemem, sys_page_size);
        code_increment = ROUND(CODE_INCREMENT, sys_page_size);

        replication_threshold = args->replication;

printf("sys_page_size %d\n", sys_page_size);
printf("max_code_size %u\n", max_code_size);
printf("code_increment %d\n", code_increment);
    }

    inlining_inited = TRUE;
    return enabled;
}

int codeBlockHash(CodeBlockHeader *block) {
    int hash = 0;
    int len = block->len - sizeof(CodeBlockHeader);
    unsigned char *pntr = (unsigned char *)(block + 1);

    for(; len > 0; len--)
        hash = hash * 37 + *pntr++;

    return hash;
}

int codeBlockComp(CodeBlockHeader *block, CodeBlockHeader *hashed_block) {
    if(block->len != block->len)
        return FALSE;

    return memcmp(block + 1, hashed_block + 1, block->len - sizeof(CodeBlockHeader)) == 0;
}

void addToFreeList(CodeBlockHeader **blocks, int len) {
    CodeBlockHeader *last = NULL;
    CodeBlockHeader **block_pntr = blocks;
    CodeBlockHeader *free_pntr = code_free_list;

    qsort(blocks, len, sizeof(CodeBlockHeader*), compare);

    for(; len--; block_pntr++) {
        for(; free_pntr && free_pntr < *block_pntr; last = free_pntr, free_pntr = free_pntr->u.next);

        if(last) {
            if((char*)last + last->len == (char*)*block_pntr) {
                last->len += (*block_pntr)->len;
                goto out;
            }
            last->u.next = *block_pntr;
        } else
            code_free_list = *block_pntr;

        (*block_pntr)->u.next = free_pntr;
        last = *block_pntr;

out:
        if((char*)last + last->len == (char*)free_pntr) {
            last->u.next = free_pntr->u.next;
            last->len += free_pntr->len;
            free_pntr = last;
        }
    }
}

void freeMethodInlinedInfo(MethodBlock *mb) {
    Instruction *instruction = mb->code;
    CodeBlockHeader **blocks = mb->code;
    QuickPrepareInfo *info;
    int i;

    /* Scan handlers within the method */

    for(i = mb->code_size; i--; instruction++) {
        char *handler = (char*)instruction->handler;
        CodeBlockHeader *block;

        if(handler >= min_entry_point || handler <= max_entry_point) {
            /* Handler is within the program text and so does
               not need freeing.  However, sequences which
               have not been rewritten yet will have associated
               preparation info. */
            if(handler == handler_entry_points[0][OPC_INLINE_REWRITER])
                gcPendingFree(instruction->operand.pntr);

            continue;
        }

        /* The handler is an inlined block */
        block = ((CodeBlockHeader*)handler) - 1;

        if(block->u.ref_count <= 0) {
            /* Either a duplicate block, or a hashed block and this
               is the only reference to it.  Duplicates must be freed
               as this would be a leak.  Hashed blocks potentially
               will be re-used and so we could keep them around.
               However, we free them because it's better to free
               room for a potentially more useful sequence. */

            /* Add onto list to be freed */
            *blocks++ = block;

            if(block->u.ref_count == 0)
                deleteHashEntry(code_hash_table, block, FALSE);
        } else
            block->u.ref_count--;
    }

    if(blocks > (CodeBlockHeader**)mb->code)
        addToFreeList(mb->code, blocks - (CodeBlockHeader**)mb->code);

    for(info = mb->quick_prepare_info; info != NULL;) {
        QuickPrepareInfo *temp = info;
        info = info->next;
        gcPendingFree(temp);
    }
}

CodeBlockHeader *expandCodeMemory(int size) {
    CodeBlockHeader *block;
    int inc = size < code_increment ? code_increment
                                    : ROUND(size, sys_page_size);

    if(code_size + inc > max_code_size) {
        inc = max_code_size - code_size;
        if(inc < size)
            return NULL;
    }

    block = mmap(0, inc, PROT_READ | PROT_WRITE | PROT_EXEC,
                         MAP_PRIVATE | MAP_ANON, -1, 0);

    if(block == MAP_FAILED)
        return NULL;

    block->len = size;
    if(inc != size) {
        CodeBlockHeader *rem = (CodeBlockHeader*)((char*)block + size);

        rem->len = inc - size;
        addToFreeList(&rem, 1);
    }

    code_size += inc;
    return block;
}

CodeBlockHeader *allocCodeMemory(int size) {
    CodeBlockHeader *last = NULL;
    CodeBlockHeader *block = code_free_list;

    for(; block && block->len < size; last = block, block = block->u.next);

    if(block) {
        if(block->len > size) {
            CodeBlockHeader *rem = (CodeBlockHeader*)((char*)block + size);

            rem->len = block->len - size;
            rem->u.next = block->u.next;

            block->len = size;
            block->u.next = rem;
        }
        if(last)
            last->u.next = block->u.next;
        else
            code_free_list = block->u.next;
    } else
        block = expandCodeMemory(size);

    return block;
}
    
CodeBlockHeader *copyCodeBlock(CodeBlockHeader *dest_block, CodeBlockHeader *src_block) {
    int len = src_block->len - sizeof(CodeBlockHeader);

    memcpy(dest_block + 1, src_block + 1, len);
    FLUSH_CACHE(dest_block + 1, len);

    return dest_block;
}

CodeBlockHeader *foundExistingBlock(CodeBlockHeader *block) {
    /* If the number of usages of the block has reached the replication
       threshold duplicate the block */
    if(block->u.ref_count >= replication_threshold) {
        CodeBlockHeader *new_block = allocCodeMemory(block->len);

        if(new_block != NULL) {
            /* Flag block as being a duplicate */
            new_block->u.ref_count = -1;
            return copyCodeBlock(new_block, block);
        }
    }

    /* Either no code memory for duplicate or not reached
       replication threshold. Just increase usage count */
    block->u.ref_count++;

    return block;
}

CodeBlockHeader *allocCodeBlock(CodeBlockHeader *block) {
    CodeBlockHeader *new_block = allocCodeMemory(block->len);

    if(new_block != NULL) {
        new_block->u.ref_count = 0;
        copyCodeBlock(new_block, block);
    }

    return new_block;
}

CodeBlockHeader *findCodeBlock(CodeBlockHeader *block) {
    CodeBlockHeader *hashed_block;

    findHashEntry(code_hash_table, block, hashed_block, TRUE, TRUE, TRUE);

    return hashed_block;
}

void inlineSequence(CodeBlock *info, int start, int len) {
    int code_len = goto_len + sizeof(CodeBlockHeader);
    Instruction *instructions = &info->start[start];
    OpcodeInfo *opcodes = &info->opcodes[start];
    CodeBlockHeader *hashed_block, *block;
    int aligned_len, i;
    char *pntr;

    /* Calculate sequence length */
    for(i = 0; i < len; i++)
        code_len += handler_sizes[opcodes[i].cache_depth][opcodes[i].opcode];

    aligned_len = ALIGN(code_len);

    /* We malloc memory for the block rather than allocating code memory.
       This reduces fragmentation of the code memory in the case where we
       use an existing block and must free the new sequence */
    block = sysMalloc(aligned_len);

    /* Store length at beginning of sequence */
    block->len = aligned_len;
    pntr = (char *)(block + 1);

    /* Concatenate the handler bodies together */
    for(i = 0; i < len; i++) {
        int size = handler_sizes[opcodes[i].cache_depth][opcodes[i].opcode];

        memcpy(pntr, instructions[i].handler, size);
        pntr += size;
    }

    /* Add the dispatch onto the end of the super-instruction */
    memcpy(pntr, goto_start, goto_len);

    /* Pad with zeros up to block length */
    for(pntr += goto_len; code_len < aligned_len; code_len++)
        *pntr++ = 0;

    /* Look up new block in inlined block cache */
    hashed_block = findCodeBlock(block);
    free(block);

    if(hashed_block != NULL) {
        /* Replace first handler with new inlined block */
        instructions[0].handler = hashed_block + 1;
        MBARRIER();

        TRACE_INLINING("InlineSequence start %p (%d) instruction len %d code len %d sequence %p\n",
                       instructions, start, len, code_len, instructions[0].handler);
    }
}

void inlineBlock(CodeBlock *block) {
    int start, len, i;

    for(start = i = 0; i < block->length; i++) {
        int cache_depth = block->opcodes[i].cache_depth;
        int opcode = block->opcodes[i].opcode;
        int op1, op2;

        switch(opcode) {
            case OPC_LDC:
                op1 = OPC_LDC_QUICK;
                op2 = OPC_LDC_W_QUICK;
                break;

            case OPC_GETSTATIC:
                op1 = OPC_GETSTATIC_QUICK;
                op2 = OPC_GETSTATIC2_QUICK;
                break;

             case OPC_PUTSTATIC:
                op1 = OPC_PUTSTATIC_QUICK;
                op2 = OPC_PUTSTATIC2_QUICK;
                break;

            case OPC_GETFIELD:
                op1 = OPC_GETFIELD_QUICK;
                op2 = OPC_GETFIELD2_QUICK;
                break;

            case OPC_PUTFIELD:
                op1 = OPC_PUTFIELD_QUICK;
                op2 = OPC_PUTFIELD2_QUICK;
                break;

            case OPC_NEW: case OPC_ANEWARRAY: case OPC_CHECKCAST:
            case OPC_INVOKESTATIC: case OPC_INVOKEINTERFACE:
            case OPC_INVOKEVIRTUAL: case OPC_INVOKESPECIAL:
            case OPC_MULTIANEWARRAY: case OPC_INSTANCEOF:
                op1 = op2 = GOTO_END;
                break;

            default:
                op1 = op2 = -1;
                break;
        }

        if(op1 > 0) {
            opcode = handler_entry_points[cache_depth][op1] == (char*) block->start[i].handler ? op1 : op2;
            block->opcodes[i].opcode = opcode;
        }

        if(handler_sizes[cache_depth][opcode] == -1) {
            len = i - start;

            if(len > 0)
                inlineSequence(block, start, len);

            start = i + 1;
        }
    }

    len = block->length - start;
    if(len > 0)
        inlineSequence(block, start, len);

    free(block->opcodes);
}

void inlineBlockWrappedOpcode(Instruction *pc, PrepareInfo *prepare_info) {
    OpcodeInfo *info;

    pc->handler = handler_entry_points[0][GOTO_START];
    MBARRIER();

    if(!LOCKWORD_COMPARE_AND_SWAP(&pc->operand, prepare_info, 0))
        return;

    LOCKWORD_WRITE(&pc->operand, prepare_info->operand);
    MBARRIER();

    info = &prepare_info->block.opcodes[prepare_info->block.length-1];
    pc->handler = handler_entry_points[info->cache_depth][info->opcode];

    inlineBlock(&prepare_info->block);
    free(prepare_info);
}

void checkInliningQuickenedInstruction(Instruction *pc, MethodBlock *mb) {
    QuickPrepareInfo *info = mb->quick_prepare_info;

    if(info) {
        QuickPrepareInfo *last = NULL;
        Thread *self = threadSelf();

        disableSuspend(self);
        lockVMWaitLock(quick_prepare_lock, self);

        for(; info && info->quickened != pc; last = info, info = info->next);

        if(info) {
            if(last)
                last->next = info->next;
            else
                mb->quick_prepare_info = info->next;
        }

        unlockVMWaitLock(quick_prepare_lock, self);
        enableSuspend(self);

        if(info) {
            inlineBlock(&info->block);
            free(info);
        }
    }
}
#endif
