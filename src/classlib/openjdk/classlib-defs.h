/*
 * Copyright (C) 2011, 2013 Robert Lougher <rob@jamvm.org.uk>.
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

#define JTHREAD                 512
#define CLASSLIB_CLASS_SPECIAL  JTHREAD

#define CLASSLIB_CLASS_PAD_SIZE 19

#define CLASSLIB_CLASS_EXTRA_FIELDS  \
   Object *protection_domain;        \
   Object *signers;

#define CLASSLIB_THREAD_EXTRA_FIELDS \
    /* NONE */

#define CLASSLIB_CLASSBLOCK_REFS_DO(action, cb, ...) \
    action(cb, protection_domain, ## __VA_ARGS__);   \
    action(cb, signers, ## __VA_ARGS__)

#ifdef JSR292
#define POLY_NAMEID_OFFSET 0

#define ID_none            0
#define ID_invokeGeneric   1
#define ID_invokeBasic     2
#define ID_linkToStatic    3
#define ID_linkToSpecial   4
#define ID_linkToVirtual   5
#define ID_linkToInterface 6

#define mbPolymorphicNameID(mb) (mb->state < POLY_NAMEID_OFFSET ?         \
                                 ID_none : mb->state - POLY_NAMEID_OFFSET)

typedef struct cached_poly_offsets {
    int mem_name_vmtarget;
    int lmda_form_vmentry;
    int mthd_hndl_form;
} CachedPolyOffsets;

#define CLASSLIB_METHOD_ANNOTATIONS(mb, type_name) {                       \
    if(type_name == SYMBOL(sig_java_lang_invoke_LambdaForm_Hidden))        \
        mb->flags |= LAMBDA_HIDDEN;                                        \
    else if(type_name == SYMBOL(sig_java_lang_invoke_LambdaForm_Compiled)) \
        mb->flags |= LAMBDA_COMPILED;                                      \
}
#endif
