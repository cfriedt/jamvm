/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
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
#include "inlining.h"

/* To do inlining, we must know which handlers are relocatable.  This
   can be calculated either at runtime or at compile-time as part of
   the build.  Doing it at compile-time saves having a second copy of
   the interpreter and the runtime checks, reducing executable size by
   approx 20-30%.  However, it cannot be done at compile-time when
   cross-compiling (at least without extra effort).
*/
#ifdef RUNTIME_RELOC_CHECKS
static int handler_sizes[HANDLERS][LABELS_SIZE];
static int goto_len;
#else
#include "relocatability.inc"
#endif

//#ifdef TRACEINLINING
#if 0
#define TRACE(fmt, ...) jam_printf(fmt, ## __VA_ARGS__)
#else
#define TRACE(fmt, ...)
#endif

#define HASHTABSZE 1<<10
#define HASH(ptr) codeHash((unsigned char*)(ptr + 1), ptr->code_len)
#define COMPARE(ptr1, ptr2, hash1, hash2) ((ptr2 != DELETED) && \
         (hash1 == hash2) && codeComp((char *)(ptr1 + 1), ptr1->code_len, ptr2))
#define FOUND(ptr1, ptr2) foundExistingBlock(ptr1, ptr2)
#define PREPARE(ptr) newCodeBlock(ptr)
#define SCAVENGE(ptr) ptr == DELETED
#define DELETED ((void*)-1)

/* Global lock protecting handler rewriting */
static VMLock rewrite_lock;

#define CODE_INCREMENT 128*KB
#define ALIGN(size) ROUND(size, sizeof(void*))
#define ROUND(size, round) (size + round - 1) / round * round

typedef struct code_block_header {
    int len;
    int code_len;
    union {
        int ref_count;
        struct code_block_header *next;
    } u;
} CodeBlockHeader;

typedef struct test_code_block {
    int code_len;
    CodeBlock *patchers;
} TestCodeBlock;

static HashTable code_hash_table;

static int replication_threshold;

static int code_size = 0;
static int sys_page_size;
static int code_increment;
static unsigned int max_code_size;
static CodeBlockHeader *code_free_list = NULL;

static char *min_entry_point = (char*)-1;
static char *max_entry_point  = NULL;

static int enabled;
int inlining_inited = FALSE;
static char **handler_entry_points[HANDLERS];
static char **branch[HANDLERS];

static int branch_patch_offsets[HANDLERS][BRANCH_NUM];

static char *goto_start;

char *reason(int reason) {
    switch(reason) {
        case MEMCMP_FAILED:
            return "memory compare failed";

        case END_REORDERED:
            return "end label re-ordered";

        case END_BEFORE_ENTRY:
            return "end label before entry label";
    }
    return "unknown reason";
}

void showRelocatability() {
    int i;

#ifdef RUNTIME_RELOC_CHECKS
    goto_len = calculateRelocatability(handler_sizes);
#endif

    if(goto_len >= 0)
        printf("Dispatch sequence is relocatable\n");
    else
        printf("Dispatch sequence is not relocatable (%s)\n", reason(goto_len));

    for(i = 0; i < HANDLERS; i++) {
        int j;

        printf("Opcodes at depth %d: \n", i);

        for(j = 0; j < LABELS_SIZE; j++) {
            int size = handler_sizes[i][j];

            if(size >= 0)
                printf("%d : is relocatable\n", j);
            else
                printf("%d : is not relocatable (%s)\n", j, reason(size));
        }
    }
}

int checkRelocatability() {
    char ***handlers = (char ***)executeJava();
    int i;

#ifdef RUNTIME_RELOC_CHECKS
    goto_len = calculateRelocatability(handler_sizes);
#endif

    /* Check relocatability of the indirect goto.  This is copied onto the end
       of each super-instruction.  If this is un-relocatable,  inlining is
       disabled. */

    if(goto_len < 0)
        return FALSE;

    goto_start = handlers[ENTRY_LABELS][GOTO_START];

    /* Calculate handler code range within the program text.
       This is used to tell which handlers in a method have
       been rewritten when freeing the method data on class
       unloading */

    for(i = 0; i < HANDLERS; i++) {
        int j;

        for(j = 0; j < LABELS_SIZE; j++) {
            char *entry = handlers[ENTRY_LABELS+i][j];

            if(entry < min_entry_point)
                min_entry_point = entry;

            if(entry > max_entry_point)
                max_entry_point = entry;
        }

        handler_entry_points[i] = handlers[ENTRY_LABELS+i];
//        branch[i] = handlers[BRANCH_LABELS+i];
    }

    for(i = 0; i < HANDLERS; i++) {
        int j;

        for(j = 0; j < BRANCH_NUM; j++) {
            branch_patch_offsets[i][j] = handlers[BRANCH_LABELS+i][j] -
                                  handler_entry_points[i][j + OPC_IFEQ];
        }
    }

    return TRUE;
}

int initialiseInlining(InitArgs *args) {
    enabled = args->codemem > 0 ? checkRelocatability() : FALSE;

    if(enabled) {
        initVMLock(rewrite_lock);
        initHashTable(code_hash_table, HASHTABSZE, TRUE);

        sys_page_size = getpagesize();
        max_code_size = ROUND(args->codemem, sys_page_size);
        code_increment = ROUND(CODE_INCREMENT, sys_page_size);

        replication_threshold = args->replication;
    }

    inlining_inited = TRUE;
    return enabled;
}

int codeHash(unsigned char *pntr, int len) {
    int hash = 0;

    for(; len > 0; len--)
        hash = hash * 37 + *pntr++;

    return hash;
}

int codeComp(char *code_pntr, int code_len, CodeBlockHeader *hashed_block) {
    if(code_len != hashed_block->code_len)
        return FALSE;

    return memcmp(code_pntr, hashed_block + 1, code_len) == 0;
}

int compareLabels(const void *pntr1, const void *pntr2) {
    char *v1 = *(char **)pntr1;
    char *v2 = *(char **)pntr2;

    return v1 - v2;
}

void addToFreeList(CodeBlockHeader **blocks, int len) {
    CodeBlockHeader *last = NULL;
    CodeBlockHeader **block_pntr = blocks;
    CodeBlockHeader *free_pntr = code_free_list;

    if(len > 1)
        qsort(blocks, len, sizeof(CodeBlockHeader*), compareLabels);

    for(; len--; block_pntr++) {
        for(; free_pntr && free_pntr < *block_pntr;
              last = free_pntr, free_pntr = free_pntr->u.next);

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

//    if(!enabled)
        return;

    /* Scan handlers within the method */

    for(i = mb->code_size; i--; instruction++) {
        char *handler = (char*)instruction->handler;
        CodeBlockHeader *block;

        if(handler >= min_entry_point && handler <= max_entry_point) {
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
    int remainder;

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
    if((remainder = inc - size) >= sizeof(CodeBlockHeader)) {
        CodeBlockHeader *rem = (CodeBlockHeader*)((char*)block + size);

        rem->len = remainder;
        addToFreeList(&rem, 1);
    }

    code_size += inc;
    return block;
}

CodeBlockHeader *allocCodeBlock(int code_size) {
    int size = ALIGN(code_size + sizeof(CodeBlockHeader));
    CodeBlockHeader *block = code_free_list;
    CodeBlockHeader *last = NULL;

    /* Search free list for big enough block */
    for(; block && block->len < size;
          last = block, block = block->u.next);

    if(block) {
        int remainder = block->len - size;
        /* Found one.  If not exact fit, need to split. */
        if(remainder >= sizeof(CodeBlockHeader)) {
            CodeBlockHeader *rem = (CodeBlockHeader*)((char*)block + size);

            rem->len = remainder;
            rem->u.next = block->u.next;

            block->len = size;
            block->u.next = rem;
        }

        if(last)
            last->u.next = block->u.next;
        else
            code_free_list = block->u.next;
    } else {
        /* No block big enough.  Need to allocate a new code chunk */
        if((block = expandCodeMemory(size)) == NULL)
            return NULL;
    }

    block->code_len = code_size;
    return block;
}
    
CodeBlockHeader *newCodeBlock(TestCodeBlock *block) {
    CodeBlockHeader *new_block = allocCodeBlock(block->code_len);

    if(new_block != NULL) {
        new_block->u.ref_count = 0;

        memcpy(new_block + 1, block + 1, block->code_len);
        FLUSH_CACHE(new_block + 1, block->code_len);
    }

    return new_block;
}

void patchExternalJumps(TestCodeBlock *test_block, CodeBlockHeader *new_block);

CodeBlockHeader *newDuplicateBlock(TestCodeBlock *test_block) {
    CodeBlockHeader *new_block = allocCodeBlock(test_block->code_len);

    if(new_block != NULL) {
        new_block->u.ref_count = -1;

        memcpy(new_block + 1, test_block + 1, test_block->code_len);

        patchExternalJumps(test_block, new_block);

        FLUSH_CACHE(new_block + 1, test_block->code_len);
    }

    return new_block;
}

CodeBlockHeader *foundExistingBlock(TestCodeBlock *test_block,
                                    CodeBlockHeader *existing_block) {

    /* If the number of usages of the block has reached the replication
       threshold duplicate the block */
    if(existing_block->u.ref_count >= replication_threshold) {
        CodeBlockHeader *dup_block = newDuplicateBlock(test_block);

        if(dup_block != NULL)
            return dup_block;
    }

    /* Either no code memory for duplicate or not reached
       replication threshold. Just increase usage count */
    existing_block->u.ref_count++;

    return existing_block;
}

CodeBlockHeader *findCodeBlock(TestCodeBlock *block) {
    CodeBlockHeader *ret_block;

    lockHashTable(code_hash_table);

    if(block->patchers)
        ret_block = newDuplicateBlock(block);
    else {
        /* Search hash table.  Add if absent, scavenge and locked */
        findHashEntry(code_hash_table, block, ret_block, TRUE, TRUE, FALSE);
    }

    unlockHashTable(code_hash_table);

    return ret_block;
}

int insSeqCodeLen(CodeBlock *block, int start, int len) {
    Instruction *instructions = &block->start[start];
    OpcodeInfo *opcodes = &block->opcodes[start];
    int i, code_len = 0;

    for(i = 0; i < len; i++)
        code_len += handler_sizes[opcodes[i].cache_depth]
                                 [opcodes[i].opcode];

    return code_len;
}

int blockSeqCodeLen(CodeBlock *start, int ins_start, CodeBlock *end, int ins_end) {
    int code_len;

    if(start == end)
        code_len = insSeqCodeLen(start, ins_start, ins_end - ins_start + 1);
    else {
        code_len = insSeqCodeLen(start, ins_start, start->length - ins_start);

        for(start = start->next; start != end; start = start->next)
            code_len += insSeqCodeLen(start, 0, start->length);

        code_len += insSeqCodeLen(end, 0, ins_end + 1);
    }

    return code_len;
}

char *insSeqCodeCopy(char *code_pntr, Instruction *ins_start_pntr, char **map,
                   CodeBlock **patchers, CodeBlock *block, int start, int len) {

    Instruction *instructions = &block->start[start];
    OpcodeInfo *opcodes = &block->opcodes[start];
    int opcode = OPC_NOP, size = 0, depth, i;

    map[instructions - ins_start_pntr] = code_pntr;

    for(i = 0; i < len; i++) {
        code_pntr += size;
        opcode = opcodes[i].opcode;
        depth = opcodes[i].cache_depth;
        size = handler_sizes[depth][opcode];

        memcpy(code_pntr, instructions[i].handler, size);
    }

    if(opcode >= OPC_IFEQ && opcode <= OPC_JSR) {
//        int diff = branch[depth][opcode - OPC_IFEQ] - handler_entry_points[depth][opcode];
        int diff = branch_patch_offsets[depth][opcode - OPC_IFEQ];

        block->u.patch.addr = code_pntr + diff;
        block->u.patch.next = *patchers;
        *patchers = block;
    }

    return code_pntr + size;
}

void *inlineProfiledBlock(Instruction *pc, MethodBlock *mb, int force_inlining);

#define GEN_REL_JMP(target_addr, patch_addr) \
{\
    int offset = target_addr - patch_addr - 5; \
    char *ptr = patch_addr; \
    \
    *ptr++ = 0xe9; \
    *(int*)ptr = offset; \
}

char *blockSeqCodeCopy(MethodBlock *mb, TestCodeBlock *block, CodeBlock *start,
                       int ins_start, CodeBlock *end, int ins_end) {

    char *code_pntr = (char *)(block + 1);
    CodeBlock *patchers, *ext_patchers = NULL;
    Instruction *ins_start_pntr = &start->start[ins_start];
    char *map[end->start - start->start - ins_start + ins_end + 1];

    block->patchers = NULL;

    if(start == end)
        code_pntr = insSeqCodeCopy(code_pntr, ins_start_pntr, map,
                   &block->patchers, start, ins_start, ins_end - ins_start + 1);
    else {
        code_pntr = insSeqCodeCopy(code_pntr, ins_start_pntr, map,
                 &block->patchers, start, ins_start, start->length - ins_start);

        for(start = start->next; start != end; start = start->next)
            code_pntr = insSeqCodeCopy(code_pntr, ins_start_pntr, map,
                                    &block->patchers, start, 0, start->length);

        code_pntr = insSeqCodeCopy(code_pntr, ins_start_pntr, map,
                                   &block->patchers, end, 0, ins_end + 1);
    }

    for(patchers = block->patchers; patchers != NULL;) {
        Instruction *target = patchers->start[patchers->length - 1].operand.pntr;
        CodeBlock *next = patchers->u.patch.next;

        if(target >= ins_start_pntr && target <= end->start) {

//            printf("Generating jump within block\n");
            GEN_REL_JMP(map[target - ins_start_pntr],
                        patchers->u.patch.addr);
        } else {
//            inlineProfiledBlock(target, mb, TRUE);

            patchers->u.patch.next = ext_patchers;
            ext_patchers = patchers;
        }
        patchers = next;
    }

    block->patchers = ext_patchers;
    return code_pntr;
}

void patchExternalJumps(TestCodeBlock *test_block, CodeBlockHeader *new_block) {
    CodeBlock *patchers = test_block->patchers;
    char *test_addr = (char*)(test_block + 1);
    char *new_addr = (char*)(new_block + 1);

    for(; patchers; patchers = patchers->u.patch.next) {
        Instruction *target = patchers->start[patchers->length - 1].operand.pntr;
        char *handler;

        handler = (char*)target->handler;
        if(handler < min_entry_point || handler > max_entry_point) {
            char *addr = patchers->u.patch.addr - test_addr + new_addr;

//            printf("Generating jump to external block %p\n", handler);
            GEN_REL_JMP(handler, addr);
        }
    }
}

#define INUM(mb, block, off) &block->start[off] - (Instruction*)mb->code

void updateSeqStarts(MethodBlock *mb, char *code_pntr, CodeBlock *start, int ins_start,
                     CodeBlock *end, int ins_end) {

    TRACE("Updating start block (%d len %d) %p\n", INUM(mb, start, ins_start),
          start->length - ins_start, code_pntr);

    start->start[ins_start].handler = code_pntr;
    MBARRIER();

    if(start != end) {
        code_pntr += insSeqCodeLen(start, ins_start, start->length - ins_start);

        for(start = start->next; start != end; start = start->next) {
            TRACE("Updating block join (%d len %d) %p\n", INUM(mb, start, 0),
                  start->length, code_pntr);

            start->start->handler = code_pntr;
            MBARRIER();

            code_pntr += insSeqCodeLen(start, 0, start->length);
        }

        TRACE("Updating end block (%d len %d) %p\n", INUM(mb, end, 0),
              ins_end + 1, code_pntr);

        end->start->handler = code_pntr;
        MBARRIER();
    }
}

void inlineSequence(MethodBlock *mb, CodeBlock *start, int ins_start, CodeBlock *end,
                    int ins_end) {
    CodeBlockHeader *hashed_block;
    TestCodeBlock *block;
    int code_len, i;
    char *pntr;

    /* Calculate sequence length */
    code_len = goto_len + blockSeqCodeLen(start, ins_start, end, ins_end);

    block = sysMalloc(code_len + sizeof(TestCodeBlock));

    /* Store length at beginning of sequence */
    block->code_len = code_len;

    /* Concatenate the handler bodies together */
    pntr = blockSeqCodeCopy(mb, block, start, ins_start, end, ins_end);

    /* Add the dispatch onto the end of the super-instruction */
    memcpy(pntr, goto_start, goto_len);

    /* Look up new block in inlined block cache */
    hashed_block = findCodeBlock(block);
    sysFree(block);

    if(hashed_block != NULL) {

        TRACE("%s.%s Inlined sequence %d, %d\n", CLASS_CB(mb->class)->name, mb->name,
              INUM(mb, start, ins_start), INUM(mb, end, ins_end));

        /* Replace first handler with new inlined block */
        updateSeqStarts(mb, (char*)(hashed_block + 1), start, ins_start, end, ins_end);
    }
}

void inlineBlocks(MethodBlock *mb, CodeBlock *start, CodeBlock *end) {
    CodeBlock *terminator = end->next;
    int ins_start = 0;
    CodeBlock *block;

    for(block = start; block != terminator; block = block->next) {
        int i;

        for(i = 0; i < block->length; i++) {
            int cache_depth = block->opcodes[i].cache_depth;
            int opcode = block->opcodes[i].opcode;
            int op1, op2;

            /* The block opcodes contain the "un-quickened" opcode.
               This could have been quickened to one of several quick
               versions. */
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
                /* Match which quickened opcode */
                opcode = handler_entry_points[cache_depth][op1]
                                == (char*) block->start[i].handler ? op1 : op2;
                block->opcodes[i].opcode = opcode;
            }

            /* A non-relocatable opcode ends a sequence */
            if(handler_sizes[cache_depth][opcode] < 0) {
                if(start != block || i > ins_start)
                    if(i != 0)
                        inlineSequence(mb, start, ins_start, block, i - 1);
                    else
                        inlineSequence(mb, start, ins_start, block->prev, block->prev->length - 1);

                if((ins_start = i + 1) == block->length) {
                    ins_start = 0;
                    start = block->next;
                } else
                    start = block;
            }
        }
    }

    /* Inline the remaining sequence */
    if(start != terminator)
        inlineSequence(mb, start, ins_start, end, end->length - 1);

//    sysFree(block->opcodes);
}

void rewriteLock(Thread *self) {
    /* Only disable/enable suspension (slow) if
       we have to block */
    if(!tryLockVMLock(rewrite_lock, self)) {
        disableSuspend(self);
        lockVMLock(rewrite_lock, self);
        enableSuspend(self);
    }
}

void rewriteUnlock(Thread *self) {
    unlockVMLock(rewrite_lock, self);
}

void removeFromProfile(MethodBlock *mb, CodeBlock *block) {
    ProfileInfo *info = block->u.profile.profiled;

    if(info == NULL) {
        OpcodeInfo *info;
        int ins_idx = block->length - 1;
        Instruction *ins = &block->start[ins_idx];
        PrepareInfo *prepare_info = ins->operand.pntr;

//        printf("Unwrapping non-quickened block (start %p)...\n", block->start);

        ins->operand = prepare_info->operand;
        MBARRIER();

        /* Unwrap the original handler */
        info = &block->opcodes[ins_idx];
        ins->handler = handler_entry_points[info->cache_depth][info->opcode];
        return;
    }

    TRACE("Removing block (start %p) from profile...\n", block->start);
    block->start->handler = info->handler;

    if(info->prev)
        info->prev->next = info->next;
    else
        mb->profile_info = info->next;

    if(info->next)
        info->next->prev = info->prev;
}

void inlineBlock(MethodBlock *mb, CodeBlock *block, Thread *self) {
    CodeBlock *start, *end;

    for(start = block; start->prev && (start->prev->u.profile.profiled ||
                                      !start->prev->u.profile.quickened);
                       start = start->prev)
        removeFromProfile(mb, start);

    removeFromProfile(mb, start);

    for(end = block; end->next && (end->next->u.profile.profiled ||
                                   !end->next->u.profile.quickened); )
        removeFromProfile(mb, end = end->next);

    if(start->prev)
        start->prev->next = NULL;
    if(end->next)
        end->next->prev = NULL;

    rewriteUnlock(self);

    TRACE("%s.%s InlineBlock trigger %d start %d end %d\n", CLASS_CB(mb->class)->name, mb->name,
          INUM(mb, block, 0), INUM(mb, start, 0), INUM(mb, end, 0));

    inlineBlocks(mb, start, end);
}

void addToProfile(MethodBlock *mb, CodeBlock *block) {
    ProfileInfo *info = sysMalloc(sizeof(ProfileInfo));
    Thread *self = threadSelf();

    TRACE("Adding block (start %p) to profile\n", block->start);
    info->profile_count = 0;
    info->block = block;

    block->u.profile.profiled = info;

    rewriteLock(self);

    info->prev = NULL;
    if((info->next = mb->profile_info))
       info->next->prev = info;
    mb->profile_info = info;

    rewriteUnlock(self);

    info->handler = block->start->handler;
    block->start->handler = handler_entry_points[0][OPC_PROFILE_REWRITER];
}

void prepareBlock(MethodBlock *mb, CodeBlock *block) {
#if 1
    addToProfile(mb, block);
#else
    inlineBlocks(mb, block, block);
#endif
}

void inlineBlockWrappedOpcode(MethodBlock *mb, Instruction *pc) {
    PrepareInfo *prepare_info = pc->operand.pntr;
    OpcodeInfo *info;
    int i;

    Thread *self = threadSelf();
    rewriteLock(self);

    for(i = 0; i < HANDLERS; i++)
        if(pc->handler == handler_entry_points[i][OPC_INLINE_REWRITER])
            break;

    if(i == HANDLERS) {
        rewriteUnlock(self);
        return;
    }

    pc->handler = handler_entry_points[0][GOTO_START];
    rewriteUnlock(self);

    /* Unwrap the original handler's operand */
    pc->operand = prepare_info->operand;
    MBARRIER();

    /* Unwrap the original handler */
    info = &prepare_info->block->opcodes[prepare_info->block->length-1];
    pc->handler = handler_entry_points[info->cache_depth][info->opcode];

    prepareBlock(mb, prepare_info->block);
    sysFree(prepare_info);
}

/* A method's quick prepare info list holds prepare information for all
   blocks within the method that end with a quickened instruction.  If
   the quickened instruction being executed is in the list we must have
   reached the end of a block and we need to inline it */
void checkInliningQuickenedInstruction(Instruction *pc, MethodBlock *mb) {

    /* As there could be multiple threads executing this method,
       the list must be protected with a lock.  However, the 
       fast case of an empty list doesn't need locking. */
    if(mb->quick_prepare_info) {
        QuickPrepareInfo *info, *last = NULL;

        Thread *self = threadSelf();
        rewriteLock(self);

        /* Search list */
        info = mb->quick_prepare_info;
        for(; info && info->quickened != pc; last = info, info = info->next);

        /* If prepare info found, remove it from the list */
        if(info) {
            if(last)
                last->next = info->next;
            else
                mb->quick_prepare_info = info->next;
        }

        rewriteUnlock(self);

        /* If prepare info found, inline block (no need to
           hold lock) */
        if(info) {
            prepareBlock(mb, info->block);
            sysFree(info);
        }
    }
}

void *inlineProfiledBlock(Instruction *pc, MethodBlock *mb, int force_inlining) {
    ProfileInfo *info, *last = NULL;
    int reached = FALSE;

    Thread *self = threadSelf();
    rewriteLock(self);

    /* Search list */
    info = mb->profile_info;
    for(; info && info->block->start != pc; last = info, info = info->next);

    if(info && (force_inlining || info->profile_count++ >= PROFILE_THRESHOLD)) {
        inlineBlock(mb, info->block, self);
        return NULL;
    }

    rewriteUnlock(self);

//    if(!info)
//        printf("*********** NO BLOCK FOUND..... %p\n", pc);
    return info ? (void*) info->handler : NULL;
}
#endif
