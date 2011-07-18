/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2010, 2011
 * Robert Lougher <rob@jamvm.org.uk>.
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>

#include "jam.h"

static int VM_initing = TRUE;
extern void initialisePlatform();

long long getPhysicalMemory() {
    /* Long longs are used here because with PAE, a 32-bit
       machine can have more than 4GB of physical memory */

    long long num_pages = sysconf(_SC_PHYS_PAGES);
    long long page_size = sysconf(_SC_PAGESIZE);

    return num_pages * page_size;
}

unsigned long clampHeapLimit(long long limit) {
    long long int clamp = MAX(limit, DEFAULT_MIN_HEAP);
    return (unsigned long)MIN(clamp, DEFAULT_MAX_HEAP);
}

/* Setup default values for command line args */

void setDefaultInitArgs(InitArgs *args) {
    long long phys_mem = getPhysicalMemory();

    args->asyncgc = FALSE;

    args->verbosegc    = FALSE;
    args->verbosedll   = FALSE;
    args->verboseclass = FALSE;

    args->trace_jni_sigs = FALSE;
    args->compact_specified = FALSE;

    args->classpath = NULL;
    args->bootpath  = NULL;

    args->java_stack = DEFAULT_STACK;
    args->max_heap   = clampHeapLimit(phys_mem/4);
    args->min_heap   = clampHeapLimit(phys_mem/64);

    args->props_count = 0;

    args->vfprintf = vfprintf;
    args->abort    = abort;
    args->exit     = exit;

#ifdef INLINING
    args->replication_threshold = 10;
    args->profile_threshold     = 10;
    args->branch_patching_dup   = TRUE;
    args->branch_patching       = TRUE;
    args->print_codestats       = FALSE;
    args->join_blocks           = TRUE;
    args->profiling             = TRUE;
    args->codemem               = args->max_heap/4;
#endif
}

int VMInitialising() {
    return VM_initing;
}

int initVM(InitArgs *args) {
    int status;

    /* Perform platform dependent initialisation */
    initialisePlatform();

    /* Initialise the VM modules -- ordering is important! */

    status = initialiseHooks(args) &&
             initialiseProperties(args) &&
             initialiseAlloc(args) &&
             initialiseThreadStage1(args) &&
             initialiseUtf8() &&
             initialiseSymbol() &&
             initialiseClass(args) &&
             initialiseDll(args) &&
             initialiseMonitor() &&
             initialiseString() &&
             initialiseException() &&
             initialiseNatives() &&
             initialiseFrame() &&
             initialiseJNI() &&
             initialiseInterpreter(args) &&
             initialiseThreadStage2(args) &&
             initialiseGC(args);

    VM_initing = FALSE;
    return status;
}

unsigned long parseMemValue(char *str) {
    char *end;
    unsigned long n = strtol(str, &end, 0);

    switch(end[0]) {
        case '\0':
            break;

        case 'M':
        case 'm':
            n *= MB;
            break;

        case 'K':
        case 'k':
            n *= KB;
            break;

        default:
             n = 0;
    } 

    return n;
}

void optError(InitArgs *args, const char *fmt, ...) {
    va_list ap;

    va_start(ap, fmt);
    (*args->vfprintf)(stderr, fmt, ap);
    va_end(ap);
}

int parseCommonOpts(char *string, InitArgs *args, int is_jni) {
    int status = OPT_OK;

    if(strcmp(string, "-Xasyncgc") == 0)
        args->asyncgc = TRUE;

    else if(strncmp(string, "-Xms", 4) == 0 ||
            (!is_jni && strncmp(string, "-ms", 3) == 0)) {

        char *value = string + (string[1] == 'm' ? 3 : 4);
        args->min_heap = parseMemValue(value);

        if(args->min_heap < MIN_HEAP) {
            optError(args, "Invalid minimum heap size: %s (min %dK)\n",
                     string, MIN_HEAP/KB);
            status = OPT_ERROR;
        }

    } else if(strncmp(string, "-Xmx", 4) == 0 ||
              (!is_jni && strncmp(string, "-mx", 3) == 0)) {

        char *value = string + (string[1] == 'm' ? 3 : 4);
        args->max_heap = parseMemValue(value);

        if(args->max_heap < MIN_HEAP) {
            optError(args, "Invalid maximum heap size: %s (min is %dK)\n",
                     string, MIN_HEAP/KB);
            status = OPT_ERROR;
        }

    } else if(strncmp(string, "-Xss", 4) == 0 ||
              (!is_jni && strncmp(string, "-ss", 3) == 0)) {

        char *value = string + (string[1] == 'm' ? 3 : 4);
        args->java_stack = parseMemValue(value);

        if(args->java_stack < MIN_STACK) {
            optError(args, "Invalid Java stack size: %s (min is %dK)\n",
                     string, MIN_STACK/KB);
            status = OPT_ERROR;
        }

    } else if(strncmp(string, "-D", 2) == 0) {
        char *key = strcpy(sysMalloc(strlen(string + 2) + 1), string + 2);
        char *pntr;

        for(pntr = key; *pntr && (*pntr != '='); pntr++);
        if(*pntr)
            *pntr++ = '\0';
        args->commandline_props[args->props_count].key = key;
        args->commandline_props[args->props_count++].value = pntr;

    } else if(strncmp(string, "-Xbootclasspath:", 16) == 0) {

        args->bootpathopt = '\0';
        args->bootpath = string + 16;

    } else if(strncmp(string, "-Xbootclasspath/a:", 18) == 0 ||
              strncmp(string, "-Xbootclasspath/p:", 18) == 0 ||
              strncmp(string, "-Xbootclasspath/c:", 18) == 0 ||
              strncmp(string, "-Xbootclasspath/v:", 18) == 0) {

        args->bootpathopt = string[16];
        args->bootpath = string + 18;

    } else if(strcmp(string, "-Xnocompact") == 0) {
        args->compact_specified = TRUE;
        args->do_compact = FALSE;

    } else if(strcmp(string, "-Xcompactalways") == 0) {
        args->compact_specified = args->do_compact = TRUE;

    } else if(strcmp(string, "-Xtracejnisigs") == 0) {
        args->trace_jni_sigs = TRUE;
#ifdef INLINING
    } else if(strcmp(string, "-Xnoinlining") == 0) {
        /* Turning inlining off is equivalent to setting
           code memory to zero */
        args->codemem = 0;

    } else if(strcmp(string, "-Xnoprofiling") == 0) {
        args->profiling = FALSE;

    } else if(strcmp(string, "-Xnopatching") == 0) {
        args->branch_patching = FALSE;

    } else if(strcmp(string, "-Xnopatchingdup") == 0) {
        args->branch_patching_dup = FALSE;

    } else if(strcmp(string, "-Xnojoinblocks") == 0) {
        args->join_blocks = FALSE;

    } else if(strcmp(string, "-Xcodestats") == 0) {
        args->print_codestats = TRUE;

    } else if(strncmp(string, "-Xprofiling:", 12) == 0) {
        args->profile_threshold = strtol(string + 12, NULL, 0);

    } else if(strncmp(string, "-Xreplication:", 14) == 0) {
        char *pntr = string + 14;

        if(strcmp(pntr, "none") == 0)
            args->replication_threshold = INT_MAX;
        else
            if(strcmp(pntr, "always") == 0)
                args->replication_threshold = 0;
            else
                args->replication_threshold = strtol(pntr, NULL, 0);

    } else if(strncmp(string, "-Xcodemem:", 10) == 0) {
        char *pntr = string + 10;

        args->codemem = strncmp(pntr, "unlimited", 10) == 0 ?
            INT_MAX : parseMemValue(pntr);

    } else if(strcmp(string, "-Xshowreloc") == 0) {
        showRelocatability();
#endif
    /* Compatibility options */
    } else if(strcmp(string,  "-Xcomp")  == 0 ||
              strcmp(string,  "-Xbatch") == 0 ||
              strncmp(string, "-XX:", 4) == 0) {
        /* Ignore */
    } else
        status = OPT_UNREC;

    return status;
}

