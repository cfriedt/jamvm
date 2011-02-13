/*
 * Copyright (C) 2011 Robert Lougher <rob@jamvm.org.uk>.
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

#define CLASSLIB_CLASS_SPECIAL   0
#define CLASSLIB_CLASS_PAD_SIZE 19

#define CLASSLIB_CLASS_EXTRA_FIELDS  \
   Object *protection_domain;        \
   Object *signers;

#define CLASSLIB_THREAD_EXTRA_FIELDS \
    /* NONE */

#define CLASSLIB_CLASSBLOCK_REFS_DO(action, cb, ...) \
    action(cb, protection_domain, ## __VA_ARGS__);   \
    action(cb, signers, ## __VA_ARGS__)
