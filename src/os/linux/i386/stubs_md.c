/*
 * Copyright (C) 2010 Robert Lougher <rob@jamvm.org.uk>.
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

#include <string.h>

#ifndef GEN_STUBS_INC
#include "jam.h"
#endif

char *convertSig2Simple(char *sig) {
    char *simple_sig = sysMalloc(strlen(sig) * 2);
    char *simple_pntr = simple_sig;
    char *sig_pntr = sig;

    *simple_pntr++ = '(';
    while(*++sig_pntr != ')') {
        *simple_pntr++ = 'I';

        if(*sig_pntr == 'J' || *sig_pntr == 'D')
            *simple_pntr++ = 'I';
        else {
            if(*sig_pntr == '[')
                while(*++sig_pntr == '[');
            if(*sig_pntr == 'L')
                while(*++sig_pntr != ';');
        }
    }

    *simple_pntr++ = ')';

    switch(*++sig_pntr) {
        case 'B':
        case 'Z':
            *simple_pntr++ = 'B';
            break;

        case 'C':
        case 'S':
        case 'J':
        case 'D':
        case 'F':
        case 'V':
            *simple_pntr++ = *sig_pntr;
            break;

        default:
            *simple_pntr++ = 'I';
            break;
    }

    *simple_pntr++ = '\0';
    return sysRealloc(simple_sig, simple_pntr - simple_sig);
}

