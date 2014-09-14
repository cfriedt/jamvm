/*
 * Copyright (C) 2011, 2014 Robert Lougher <rob@jamvm.org.uk>.
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

#define VMTHREAD     512
#define VMTHROWABLE 1024 

#define CLASSLIB_CLASS_SPECIAL (VMTHREAD | VMTHROWABLE)

#define CLASSLIB_CLASS_PAD char pad[4*sizeof(Object*)];

/* In OpenJDK 9 the class loader and array component type fields have been
   moved to the Java-level class object.  To support this in JamVM, the
   class_loader and component_class fields have been removed from ClassBlock
   and made part of the classlib. */

#define CLASSLIB_CLASS_EXTRA_FIELDS Object *class_loader;

#define CLASSLIB_CLASSBLOCK_REFS_DO(action, cb, ...) \
    action(cb, class_loader, ## __VA_ARGS__)

#define CLASSLIB_ARRAY_CLASS_EXTRA_FIELDS Class *component_class;

#define CLASSLIB_CLASSBLOCK_ARRAY_REFS_DO(action, cb, ...) \
    action(cb, component_class, ## __VA_ARGS__)

#define CLASSLIB_THREAD_EXTRA_FIELDS \
    unsigned short state;
