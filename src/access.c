/*
 * Copyright (C) 2003, 2004, 2005 Robert Lougher <rob@lougher.demon.co.uk>.
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

#include "jam.h"

int isSameRuntimePackage(Class *class1, Class *class2) {
    if(class1 != class2) {
        ClassBlock *cb1 = CLASS_CB(class1);
        ClassBlock *cb2 = CLASS_CB(class2);

        /* The class loader must match */
        if(cb1->class_loader != cb2->class_loader)
            return FALSE;
        else {
            /* And the package name */

            char *ptr1 = cb1->name;
            char *ptr2 = cb2->name;

            /* Names must match at least up to the last slash
               in each.  Note, we do not need to check for NULLs
               because names _must_ be different (same loader). */

            while(*ptr1++ == *ptr2++);

            for(ptr1--; *ptr1 && *ptr1 != '/'; ptr1++);

            /* Didn't match to last slash in ptr1 */
            if(*ptr1)
                return FALSE;

            for(ptr2--; *ptr2 && *ptr2 != '/'; ptr2++);

            /* Didn't match to last slash in ptr2 */
            if(*ptr2)
                return FALSE;
        }
    }
    return TRUE;
}

int checkClassAccess(Class *class1, Class *class2) {
    ClassBlock *cb1 = CLASS_CB(class1);

    /* We can access it if it is public */
    if(cb1->access_flags & ACC_PUBLIC)
        return TRUE;

    /* Or if they're members of the same runtime package */

    return isSameRuntimePackage(class1, class2);
}

int checkMethodOrFieldAccess(int access_flags, Class *decl_class, Class *class) {

    if(access_flags & ACC_PUBLIC)
        return TRUE;

    if((access_flags & ACC_PROTECTED) && isSubClassOf(decl_class, class))
        return TRUE;

    if(((access_flags & ACC_PROTECTED) ||
       !(access_flags & (ACC_PUBLIC|ACC_PROTECTED|ACC_PRIVATE))) &&
            isSameRuntimePackage(decl_class, class))
        return TRUE;

    return (access_flags & ACC_PRIVATE) && decl_class == class;
}

int checkMethodAccess(MethodBlock *mb, Class *class) {
    return checkMethodOrFieldAccess(mb->access_flags, mb->class, class);
}

int checkFieldAccess(FieldBlock *fb, Class *class) {
    return checkMethodOrFieldAccess(fb->access_flags, fb->class, class);
}
