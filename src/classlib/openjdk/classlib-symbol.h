/*
 * Copyright (C) 2010, 2011 Robert Lougher <rob@jamvm.org.uk>.
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

#define CLASSLIB_SYMBOLS_DO(action) \
    action(tid, "tid"), \
    action(add, "add"), \
    action(base, "base"), \
    action(_JI__V, "(JI)V"), \
    action(remove, "remove"), \
    action(invoke, "invoke"), \
    action(array_J, "[J"), \
    action(classes, "classes"), \
    action(dispatch, "dispatch"), \
    action(capacity, "capacity"), \
    action(shutdown, "shutdown"), \
    action(findNative, "findNative"), \
    action(threadStatus, "threadStatus"), \
    action(constantPoolOop, "constantPoolOop"), \
    action(sun_misc_Signal, "sun/misc/Signal"), \
    action(java_lang_Shutdown, "java/lang/Shutdown"), \
    action(___java_lang_Object, "()Ljava/lang/Object;"), \
    action(java_lang_Exception, "java/lang/Exception"), \
    action(uncaughtExceptionHandler, "uncaughtExceptionHandler"), \
    action(java_lang_RuntimeException, "java/lang/RuntimeException"), \
    action(_java_lang_Exception__V, "(Ljava/lang/Exception;)V"), \
    action(sig_java_util_vector, "Ljava/util/Vector;"), \
    action(array_java_lang_Object, "[Ljava/lang/Object;"), \
    action(initializeSystemClass, "initializeSystemClass"), \
    action(sun_reflect_ConstantPool, "sun/reflect/ConstantPool"), \
    action(java_nio_DirectByteBuffer, "java/nio/DirectByteBuffer"), \
    action(sun_reflect_MagicAccessorImpl, "sun/reflect/MagicAccessorImpl"), \
    action(sun_reflect_MethodAccessorImpl, "sun/reflect/MethodAccessorImpl"), \
    action(sun_reflect_ConstructorAccessorImpl, \
           "sun/reflect/ConstructorAccessorImpl"), \
    action(java_security_PrivilegedActionException, \
           "java/security/PrivilegedActionException"), \
    action(sun_reflect_UnsafeStaticFieldAccessorImpl, \
           "sun/reflect/UnsafeStaticFieldAccessorImpl"), \
    action(_java_lang_ClassLoader_java_lang_String__J, \
           "(Ljava/lang/ClassLoader;Ljava/lang/String;)J"), \
    action(_java_lang_ThreadGroup_java_lang_String__V, \
           "(Ljava/lang/ThreadGroup;Ljava/lang/String;)V"), \
    action(_java_lang_ThreadGroup_java_lang_Runnable__V, \
           "(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;)V"), \
    action(java_lang_reflect_field_init_sig, \
           "(Ljava/lang/Class;Ljava/lang/String;" \
           "Ljava/lang/Class;IILjava/lang/String;[B)V"), \
    action(java_lang_reflect_cons_init_sig, \
           "(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;" \
           "IILjava/lang/String;[B[B)V"), \
    action(java_lang_reflect_mthd_invoke_sig, \
           "(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;"), \
    action(java_lang_reflect_mthd_init_sig, \
           "(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;" \
           "Ljava/lang/Class;[Ljava/lang/Class;IILjava/lang/String;[B[B[B)V")

