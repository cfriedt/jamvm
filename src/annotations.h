/*
 * Copyright (C) 2013 Robert Lougher <rob@jamvm.org.uk>.
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

#define classAnnotations(class, name) ({                    \
    ClassBlock *cb = CLASS_CB(class);                       \
    cb->annotations == NULL ? NULL : cb->annotations->name; \
})

#define getClassAnnotationData(_class)    classAnnotations(_class, class)

#define indexedAnnotationData(annotations, name, index) ( \
    annotations == NULL || annotations->name == NULL ?    \
        NULL : annotations->name[index]                   \
)

#define methodAnnotations(mb, name) ({                    \
    ClassBlock *cb = CLASS_CB(mb->class);                 \
    int index = mb - cb->methods;                         \
    indexedAnnotationData(cb->annotations, name, index);  \
})

#define getMethodAnnotationData(mb)     methodAnnotations(mb, method)

#define getMethodParameterAnnotationData(mb) \
    methodAnnotations(mb, method_parameters)

#define getMethodDefaultValueAnnotationData(mb) \
    methodAnnotations(mb, method_default_val)

#define fieldAnnotations(fb, name) ({                    \
    ClassBlock *cb = CLASS_CB(fb->class);                \
    int index = fb - cb->fields;                         \
    indexedAnnotationData(cb->annotations, name, index); \
})

#define getFieldAnnotationData(fb)     fieldAnnotations(fb, field)
