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

extern void fillInStackTrace(Object *);
extern Object *consNewInstance(Object *reflect_ob, Object *args_array);
extern Object *invokeMethod(Object *reflect_ob, Object *ob, Object *args_array);

extern int stackTraceDepth(Object *thrwble);
extern Object *stackTraceElementAtIndex(Object *thrwble, int index);

extern int initialiseJVMInterface();
extern Object *enclosingMethodInfo(Class *class);
extern Object *getAnnotationsAsArray(AnnotationData *annotations);

extern void *getJMMInterface(int version);
