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

#include "inlining.h"

int handler_sizes[HANDLERS][LABELS_SIZE];
int inlining_inited = FALSE;
int goto_len;

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

void calculateRelocatability() {
    char ***handlers1 = (char ***)executeJava();
    char ***handlers2 = (char ***)executeJava2();
    char *sorted_ends[LABELS_SIZE];
    char *goto_start;
    int i;

    /* Check relocatability of the indirect goto.  This is copied onto the end
       of each super-instruction.  If this is un-relocatable,  inlining is
       disabled. */

    goto_start = handlers1[ENTRY_LABELS][GOTO_START];
    goto_len = handlers1[ENTRY_LABELS][GOTO_END] - goto_start;

    if(goto_len <= 0)
        goto_len = END_BEFORE_ENTRY;
    else
        if(memcmp(goto_start, handlers2[ENTRY_LABELS][GOTO_START], goto_len) != 0)
            goto_len = MEMCMP_FAILED;

    /* Check relocatability of each handler. */

    for(i = 0; i < HANDLERS; i++) {
        int j;

        memcpy(sorted_ends, handlers1[END_LABELS+i], LABELS_SIZE * sizeof(char *));
        qsort(sorted_ends, LABELS_SIZE, sizeof(char *), compare);

        for(j = 0; j < LABELS_SIZE; j++) {
            char *entry = handlers1[ENTRY_LABELS+i][j];
            char *end = handlers1[END_LABELS+i][j];
            int len = end - entry;

            if(len > 0) {
                char *nearest_end = findNextLabel(sorted_ends, entry);

                if(nearest_end == end) {
                    if(memcmp(entry, handlers2[ENTRY_LABELS+i][j], len) != 0)
                        len = MEMCMP_FAILED;
                } else
                    len = END_REORDERED;
            } else
                len = END_BEFORE_ENTRY;

            handler_sizes[i][j] = len;
        }
    }
}

char *value2Str(int value, char *buff) {
    switch(value) {
        case MEMCMP_FAILED:
            return "MEMCMP_FAILED";
        case END_REORDERED:
            return "END_REORDERED";
        case END_BEFORE_ENTRY:
            return "END_BEFORE_ENTRY";

        default:
            sprintf(buff, "%d", value);
            return buff;
    }
}
  
void writeIncludeFile() {
    char buff[256];
    FILE *fd;
    int i, j;

    fd = fopen("relocatability.inc", "w");

    fprintf(fd, "static int goto_len = %s;\n", value2Str(goto_len, buff));
    fprintf(fd, "static int handler_sizes[%d][%d] = {\n", HANDLERS, LABELS_SIZE);

    for(i = 0; i < HANDLERS; i++) {
        if(i > 0)
            fprintf(fd, ",\n");
        fprintf(fd, "    {\n");

        for(j = 0; j < LABELS_SIZE - 1; j++)
            fprintf(fd, "        %s,\n", value2Str(handler_sizes[i][j], buff));

        fprintf(fd, "        %s\n    }", value2Str(handler_sizes[i][LABELS_SIZE-1], buff));
    }

    fprintf(fd, "\n};\n");
    fclose(fd);
}

int main() {
    calculateRelocatability();
    writeIncludeFile();
    return 0;
}


/* Stubs for functions called from executeJava */

Object *allocObject(Class *class) {
    return NULL;
}

Object *allocArray(Class *class, int size, int el_size) {
    return NULL;
}

Object *allocTypeArray(int type, int size) {
    return NULL;
}

Object *allocMultiArray(Class *array_class, int dim, intptr_t *count) {
    return NULL;
}

void *sysMalloc(int n) {
    return NULL;
}

Class *findArrayClassFromClassLoader(char *name, Object *loader) {
    return NULL;
}

Class *resolveClass(Class *class, int index, int init) {
    return NULL;
}

MethodBlock *resolveMethod(Class *class, int index) {
    return NULL;
}

MethodBlock *resolveInterfaceMethod(Class *class, int index) {
    return NULL;
}

FieldBlock *resolveField(Class *class, int index) {
    return NULL;
}

uintptr_t resolveSingleConstant(Class *class, int index) {
    return 0;
}

char isInstanceOf(Class *class, Class *test) {
    return 0;
}

char arrayStoreCheck(Class *class, Class *test) {
    return 0;
}

void signalChainedException(char *excep_name, char *excep_mess, Object *cause) {
}

CodePntr findCatchBlock(Class *exception) {
    return NULL;
}

ExecEnv *getExecEnv() {
    return NULL;
}

void exitVM(int status) {
}

void jam_fprintf(FILE *stream, const char *fmt, ...) {
}

void initialiseDirect(InitArgs *args) {
}

void prepare(MethodBlock *mb, const void ***handlers) {
}

void objectLock(Object *ob) {
}

void objectUnlock(Object *ob) {
}

void inlineBlockWrappedOpcode(Instruction *pc) {
}

void checkInliningQuickenedInstruction(Instruction *pc, MethodBlock *mb) {
}
#else
int main() {
    return 0;
}
#endif
