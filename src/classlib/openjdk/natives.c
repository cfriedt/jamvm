/*
 * Copyright (C) 2010, 2011, 2012 Robert Lougher <rob@jamvm.org.uk>.
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

#ifdef NO_JNI
#error to use classpath, Jam must be compiled with JNI!
#endif

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include "jam.h"
#include "excep.h"
#include "frame.h"
#include "class.h"
#include "symbol.h"
#include "reflect.h"
#include "natives.h"
#include "openjdk.h"

#if 0
//#define TRACE(fmt, ...) jam_printf(fmt, ## __VA_ARGS__)
#define TRACE(fmt, ...) printf(fmt, ## __VA_ARGS__)
#else
#define TRACE(fmt, ...)
#endif

#if 0
#define DEBUG(fmt, ...) printf(fmt, ## __VA_ARGS__)
#else
#define DEBUG(fmt, ...)
#endif

int classlibInitialiseNatives() {
    Class *field_accessor;
    FieldBlock *base_fb = NULL;
    char *dll_path = getBootDllPath();
    char *dll_name = getDllName("java");
    char path[strlen(dll_path) + strlen(dll_name) + 2];

    strcat(strcat(strcpy(path, dll_path), "/"), dll_name);
    sysFree(dll_name);

    if(!resolveDll(path, NULL)) {
        jam_fprintf(stderr, "Error initialising natives: couldn't open "
                            "libjava.so: use -verbose:jni for more "
                            "information\n");
        return FALSE;
    }

    field_accessor = findSystemClass0(SYMBOL(
                         sun_reflect_UnsafeStaticFieldAccessorImpl));

    if(field_accessor != NULL)
        base_fb = findField(field_accessor, SYMBOL(base),
                                            SYMBOL(sig_java_lang_Object));

    if(base_fb == NULL) {
        jam_fprintf(stderr, "Error initialising natives: %s "
                            "missing or malformed\n",
                            SYMBOL(sun_reflect_UnsafeStaticFieldAccessorImpl));
        return FALSE;
    }

    hideFieldFromGC(base_fb);

    return initialiseJVMInterface();
}

uintptr_t *unsafeRegisterNatives(Class *class, MethodBlock *mb,
                                 uintptr_t *ostack) {
    return ostack;
}

uintptr_t *staticFieldOffset(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    *(int64_t *)ostack = 0;
    return ostack + 2;
}

uintptr_t *staticFieldBase(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    FieldBlock *fb = fbFromReflectObject((Object*)ostack[1]);

    *ostack++ = (uintptr_t)&fb->u.static_value;
    return ostack;
}

uintptr_t *putLongAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int64_t value   = *(int64_t *)&ostack[3];
    int64_t *pntr   =  (int64_t *)(uintptr_t)address;

    *pntr = value;
    return ostack;
}

uintptr_t *putIntAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int32_t *pntr   =  (int32_t *)(uintptr_t)address;

    *pntr = ostack[3];
    return ostack;
}

uintptr_t *putShortAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int16_t *pntr   =  (int16_t *)(uintptr_t)address;

    *pntr = ostack[3];
    return ostack;
}

uintptr_t *putByteAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int8_t  *pntr   =  (int8_t  *)(uintptr_t)address;

    *pntr = ostack[3];
    return ostack;
}

uintptr_t *getLongAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int64_t *pntr   =  (int64_t *)(uintptr_t)address;

    *(int64_t *)ostack = *pntr;
    return ostack + 2;
}

uintptr_t *getIntAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int32_t *pntr   =  (int32_t *)(uintptr_t)address;

    *ostack++ = *pntr;
    return ostack;
}

uintptr_t *getShortAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int16_t *pntr   =  (int16_t *)(uintptr_t)address;

    *ostack++ = *pntr;
    return ostack;
}

uintptr_t *getCharAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t  address = *(int64_t  *)&ostack[1];
    uint16_t *pntr   =  (uint16_t *)(uintptr_t)address;

    *ostack++ = *pntr;
    return ostack;
}

uintptr_t *getByteAddress(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];
    int8_t  *pntr   =  (int8_t  *)(uintptr_t)address;

    *ostack++ = *pntr;
    return ostack;
}

uintptr_t *putInt(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t   offset = *(int64_t  *)&ostack[2];
    uint32_t  *pntr  =  (uint32_t *)((char *)ostack[1] + offset);
    uintptr_t value  = ostack[4];

    *pntr = value;
    return ostack;
}

uintptr_t *getObject(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t   offset = *(int64_t   *)&ostack[2];
    uintptr_t *pntr  =  (uintptr_t *)((char *)ostack[1] + offset);

    *ostack++ = *pntr;
    return ostack;
}

uintptr_t *getInt(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t  offset = *(int64_t  *)&ostack[2];
    uint32_t *pntr  =  (uint32_t *)((char *)ostack[1] + offset);

    *ostack++ = *pntr;
    return ostack;
}

uintptr_t *allocateMemory(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t bytes = *(int64_t *)&ostack[1];
    size_t size = bytes;
    void *pntr;

    if(bytes < 0 || bytes != size) {
        signalException(java_lang_IllegalArgumentException, NULL);
        return ostack;
    }

    if((pntr = malloc(size)) == NULL) {
        signalException(java_lang_OutOfMemoryError, NULL);
        return ostack;
    }
   
    *(int64_t *)ostack = (int64_t)(uintptr_t)pntr;
    return ostack + 2;
}

uintptr_t *freeMemory(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    int64_t address = *(int64_t *)&ostack[1];

    free((void *)(uintptr_t)address);
    return ostack;
}

uintptr_t *setMemoryOpenJDK6(Class *class, MethodBlock *mb,
                             uintptr_t *ostack) {

    int64_t address = *(int64_t *)&ostack[1];
    int64_t bytes   = *(int64_t *)&ostack[3];
    int32_t value   = ostack[5];
    void    *pntr   = (void *)(uintptr_t)address;
    size_t  size    = bytes;

    if(bytes < 0 || bytes != size) {
        signalException(java_lang_IllegalArgumentException, NULL);
        return ostack;
    }

    memset(pntr, value, size);
    return ostack;
}

uintptr_t *copyMemoryOpenJDK6(Class *class, MethodBlock *mb,
                              uintptr_t *ostack) {

    int64_t src_addr  = *(int64_t *)&ostack[1];
    int64_t dst_addr  = *(int64_t *)&ostack[3];
    int64_t bytes     = *(int64_t *)&ostack[5];
    void    *src_pntr = (void *)(uintptr_t)src_addr;
    void    *dst_pntr = (void *)(uintptr_t)dst_addr;
    size_t  size      = bytes;

    if(bytes < 0 || bytes != size) {
        signalException(java_lang_IllegalArgumentException, NULL);
        return ostack;
    }

    memcpy(dst_pntr, src_pntr, size);
    return ostack;
}

uintptr_t *setMemoryOpenJDK7(Class *class, MethodBlock *mb,
                             uintptr_t *ostack) {

    Object *base    = (Object *)ostack[1];
    int64_t offset  = *(int64_t *)&ostack[2];
    int64_t bytes   = *(int64_t *)&ostack[4];
    int32_t value   = ostack[6];
    void   *pntr    = (char *)base + offset;
    size_t  size    = bytes;

    if(bytes < 0 || bytes != size) {
        signalException(java_lang_IllegalArgumentException, NULL);
        return ostack;
    }

    memset(pntr, value, size);
    return ostack;
}

uintptr_t *copyMemoryOpenJDK7(Class *class, MethodBlock *mb,
                              uintptr_t *ostack) {

    Object *src_base  = (Object *)ostack[1];
    int64_t src_ofst  = *(int64_t *)&ostack[2];
    Object *dst_base  = (Object *)ostack[4];
    int64_t dst_ofst  = *(int64_t *)&ostack[5];
    int64_t bytes     = *(int64_t *)&ostack[7];
    void   *src_pntr  = (char *)src_base + src_ofst;
    void   *dst_pntr  = (char *)dst_base + dst_ofst;
    size_t  size      = bytes;

    if(bytes < 0 || bytes != size) {
        signalException(java_lang_IllegalArgumentException, NULL);
        return ostack;
    }

    memcpy(dst_pntr, src_pntr, size);
    return ostack;
}

uintptr_t *ensureClassInitialized(Class *class, MethodBlock *mb,
                                  uintptr_t *ostack) {
    initClass((Class*)ostack[1]);
    return ostack;
}

uintptr_t *unsafeDefineClass(Class *clazz, MethodBlock *mb,
                                   uintptr_t *ostack) {
    Object *string   = (Object *)ostack[1];
    Object *array    = (Object *)ostack[2];
    int32_t offset   = ostack[3];
    int32_t data_len = ostack[4];
    Class *class     = NULL;

    TRACE("unsafeDefineClass\n");

    if(array == NULL)
        signalException(java_lang_NullPointerException, NULL);
    else
        if((offset < 0) || (data_len < 0) ||
                           ((offset + data_len) > ARRAY_LEN(array)))
            signalException(java_lang_ArrayIndexOutOfBoundsException, NULL);
        else {
            char *data = ARRAY_DATA(array, char);
            char *name = string ? dots2Slash(String2Utf8(string)) : NULL;

            class = defineClass(name, data, offset, data_len, NULL);
            sysFree(name);

            if(class != NULL)
                linkClass(class);
        }

    *ostack++ = (uintptr_t) class;
    return ostack;
}

uintptr_t *defineClassWithLoaderPD(Class *clazz, MethodBlock *mb,
                                   uintptr_t *ostack) {
    Object *string   = (Object *)ostack[1];
    Object *array    = (Object *)ostack[2];
    int32_t offset   = ostack[3];
    int32_t data_len = ostack[4];
    Object *loader   = (Object *)ostack[5];
    Object *pd       = (Object *)ostack[6];
    Class *class     = NULL;

    if(array == NULL)
        signalException(java_lang_NullPointerException, NULL);
    else
        if((offset < 0) || (data_len < 0) ||
                           ((offset + data_len) > ARRAY_LEN(array)))
            signalException(java_lang_ArrayIndexOutOfBoundsException, NULL);
        else {
            char *data = ARRAY_DATA(array, char);
            char *name = string ? dots2Slash(String2Utf8(string)) : NULL;

            class = defineClass(name, data, offset, data_len, loader);
            sysFree(name);

            if(class != NULL) {
                CLASS_CB(class)->protection_domain = pd;
                linkClass(class);
            }
        }

    *ostack++ = (uintptr_t) class;
    return ostack;
}

uintptr_t *throwException(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    setException((Object *)ostack[1]);
    return ostack;
}

uintptr_t *allocateInstance(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Object *obj = allocObjectClassCheck((Class *)ostack[1]);
    *ostack++ = (uintptr_t)obj;
    return ostack;
}

uintptr_t *addressSize(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    *ostack++ = sizeof(void*);
    return ostack;
}

uintptr_t *pageSize(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    *ostack++ = getpagesize();
    return ostack;
}

uintptr_t *defineAnonymousClass(Class *class, MethodBlock *mb, 
	                        uintptr_t *ostack) {
    Class *host_class = (Class *)ostack[1];
    Object *data = (Object *)ostack[2];
    Object *cp_patches = (Object *)ostack[3];

    TRACE("defineAnonymousClass\n");

    class = parseClass(NULL, ARRAY_DATA(data, char), 0, ARRAY_LEN(data),
                       CLASS_CB(host_class)->class_loader);

    if(class != NULL) {
        int cp_patches_len = cp_patches == NULL ? 0 : ARRAY_LEN(cp_patches);
        ConstantPool *cp = &(CLASS_CB(class)->constant_pool);
        int i;

        for(i = 0; i < cp_patches_len; i++) {
            Object *obj = ARRAY_DATA(cp_patches, Object*)[i];
            if(obj != NULL) {
                int type = CP_TYPE(cp, i);
            
                switch(type) {
                    case CONSTANT_String:
                        CP_INFO(cp, i) = (uintptr_t)obj;
                        CP_TYPE(cp, i) = CONSTANT_ResolvedString;
                        break;

                    default:
                        signalException(java_lang_InternalError,
                                        "defineAnonymousClass: "
                                        "unimplemented patch type");
                }
            }
        }
    }

    *ostack++ = (uintptr_t) class;
    return ostack;
}

uintptr_t *shouldBeInitialized(Class *clazz, MethodBlock *mb,
	                       uintptr_t *ostack) {
    Class *class = (Class *)ostack[1];

    *ostack++ = CLASS_CB(class)->state < CLASS_INITED;
    return ostack;
}

VMMethod sun_misc_unsafe[] = {
    {"registerNatives",        "()V", unsafeRegisterNatives},
    {"objectFieldOffset",      "(Ljava/lang/reflect/Field;)J",
                               objectFieldOffset},
    {"staticFieldOffset",      "(Ljava/lang/reflect/Field;)J",
                               staticFieldOffset},
    {"staticFieldBase",        "(Ljava/lang/reflect/Field;)Ljava/lang/Object;",
                               staticFieldBase},
    {"putLong",                "(JJ)V", putLongAddress},
    {"putDouble",              "(JD)V", putLongAddress},
    {"putInt",                 "(JI)V", putIntAddress},
    {"putFloat",               "(JF)V", putIntAddress},
    {"putShort",               "(JS)V", putShortAddress},
    {"putChar",                "(JC)V", putShortAddress},
    {"putByte",                "(JB)V", putByteAddress},
    {"getLong",                "(J)J", getLongAddress},
    {"getDouble",              "(J)D", getLongAddress},
    {"getInt",                 "(J)I", getIntAddress},
    {"getFloat",               "(J)F", getIntAddress},
    {"getShort",               "(J)S", getShortAddress},
    {"getChar",                "(J)C", getCharAddress},
    {"getByte",                "(J)B", getByteAddress},
    {"compareAndSwapInt",      "(Ljava/lang/Object;JII)Z", compareAndSwapInt},
    {"compareAndSwapLong",     "(Ljava/lang/Object;JJJ)Z", compareAndSwapLong},
    {"compareAndSwapObject",   "(Ljava/lang/Object;JLjava/lang/Object;"
                               "Ljava/lang/Object;)Z", compareAndSwapObject},
    {"putOrderedInt",          "(Ljava/lang/Object;JI)V", putOrderedInt},
    {"putOrderedLong",         "(Ljava/lang/Object;JJ)V", putOrderedLong},
    {"putOrderedObject",       "(Ljava/lang/Object;JLjava/lang/Object;)V",
                               putOrderedObject},
    {"putIntVolatile",         "(Ljava/lang/Object;JI)V", putIntVolatile},
    {"putByteVolatile",        "(Ljava/lang/Object;JB)V", putIntVolatile},
    {"putCharVolatile",        "(Ljava/lang/Object;JC)V", putIntVolatile},
    {"putShortVolatile",       "(Ljava/lang/Object;JS)V", putIntVolatile},
    {"putBooleanVolatile",     "(Ljava/lang/Object;JZ)V", putIntVolatile},
    {"putLongVolatile",        "(Ljava/lang/Object;JJ)V", putOrderedLong},
    {"putObjectVolatile",      "(Ljava/lang/Object;JLjava/lang/Object;)V",
                               putObjectVolatile},
    {"getIntVolatile",         "(Ljava/lang/Object;J)I", getIntVolatile},
    {"getByteVolatile",        "(Ljava/lang/Object;J)B", getIntVolatile},
    {"getCharVolatile",        "(Ljava/lang/Object;J)C", getIntVolatile},
    {"getShortVolatile",       "(Ljava/lang/Object;J)S", getIntVolatile},
    {"getFloatVolatile",       "(Ljava/lang/Object;J)F", getIntVolatile},
    {"getBooleanVolatile",     "(Ljava/lang/Object;J)Z", getIntVolatile},
    {"getLongVolatile",        "(Ljava/lang/Object;J)J", getLongVolatile},
    {"getDoubleVolatile",      "(Ljava/lang/Object;J)D", getLongVolatile},
    {"getObjectVolatile",      "(Ljava/lang/Object;J)Ljava/lang/Object;",
                               getObjectVolatile},
    {"putInt",                 "(Ljava/lang/Object;JI)V", putInt},
    {"putByte",                "(Ljava/lang/Object;JB)V", putInt},
    {"putChar",                "(Ljava/lang/Object;JC)V", putInt},
    {"putShort",               "(Ljava/lang/Object;JS)V", putInt},
    {"putFloat",               "(Ljava/lang/Object;JF)V", putInt},
    {"putBoolean",             "(Ljava/lang/Object;JZ)V", putInt},
    {"putLong",                "(Ljava/lang/Object;JJ)V", putLong},
    {"putDouble",              "(Ljava/lang/Object;JD)V", putLong},
    {"putObject",              "(Ljava/lang/Object;JLjava/lang/Object;)V",
                               putObject},
    {"getInt",                 "(Ljava/lang/Object;J)I", getInt},
    {"getByte",                "(Ljava/lang/Object;J)B", getInt},
    {"getChar",                "(Ljava/lang/Object;J)C", getInt},
    {"getShort",               "(Ljava/lang/Object;J)S", getInt},
    {"getFloat",               "(Ljava/lang/Object;J)F", getInt},
    {"getBoolean",             "(Ljava/lang/Object;J)Z", getInt},
    {"getLong",                "(Ljava/lang/Object;J)J", getLong},
    {"getDouble",              "(Ljava/lang/Object;J)D", getLong},
    {"getObject",              "(Ljava/lang/Object;J)Ljava/lang/Object;",
                               getObject},
    {"setMemory",              "(JJB)V", setMemoryOpenJDK6},
    {"copyMemory",             "(JJJ)V", copyMemoryOpenJDK6},
    {"setMemory",              "(Ljava/lang/Object;JJB)V", setMemoryOpenJDK7},
    {"copyMemory",             "(Ljava/lang/Object;JLjava/lang/Object;JJ)V",
                               copyMemoryOpenJDK7},
    {"arrayBaseOffset",        NULL, arrayBaseOffset},
    {"arrayIndexScale",        NULL, arrayIndexScale},
    {"unpark",                 NULL, unpark},
    {"park",                   NULL, park},
    {"allocateMemory",         NULL, allocateMemory},
    {"freeMemory",             NULL, freeMemory},
    {"ensureClassInitialized", NULL, ensureClassInitialized},
    {"defineClass",            "(Ljava/lang/String;[BII"
                                "Ljava/lang/ClassLoader;"
                                "Ljava/security/ProtectionDomain;"
                               ")Ljava/lang/Class;",
                               defineClassWithLoaderPD},
    {"defineClass",            "(Ljava/lang/String;[BII"
                               ")Ljava/lang/Class;",
                               unsafeDefineClass},
    {"throwException",         NULL, throwException},
    {"allocateInstance",       NULL, allocateInstance},
    {"addressSize",            NULL, addressSize},
    {"pageSize",               NULL, pageSize},
    {"defineAnonymousClass",   NULL, defineAnonymousClass},
    {"shouldBeInitialized",    NULL, shouldBeInitialized},
    {NULL,                     NULL, NULL}
};

#define static extern
static Class *cons_reflect_class, *method_reflect_class;
static Class *field_reflect_class;

static MethodBlock *cons_init_mb, *fld_init_mb, *mthd_init_mb;

static int cons_slot_offset, cons_class_offset, cons_param_offset;
static int mthd_slot_offset, mthd_class_offset, mthd_ret_offset,
           mthd_param_offset;
static int fld_slot_offset, fld_class_offset;
#undef static

static int mem_name_clazz_offset, mem_name_name_offset,
           mem_name_type_offset, mem_name_flags_offset,
           mem_name_vmtarget_offset, mem_name_vmindex_offset;
       
static int mthd_type_ptypes_offset, mthd_type_rtype_offset;
static int mthd_hndl_form_offset;
static int lmda_form_vmentry_offset;
static int call_site_target_offset;

static MethodBlock *MHN_linkMethod_mb;
static MethodBlock *MHN_linkCallSite_mb;
static MethodBlock *MHN_findMethodType_mb;
static MethodBlock *MHN_linkMethodHandleConstant_mb;

uintptr_t *invokeRegisterNatives(Class *class, MethodBlock *mb,
                                 uintptr_t *ostack) {
    Class *member_name;
    FieldBlock *clazz_fb, *name_fb, *type_fb, *flags_fb;
    FieldBlock *vmtarget_fb, *vmindex_fb;
    Class *method_type;
    FieldBlock *ptypes_fb, *rtype_fb;
    Class *method_handle;
    FieldBlock *form_fb;
    Class *lambda_form;
    FieldBlock *vmentry_fb;
    Class *mthd_hndl_natives;
    Class *call_site;
    FieldBlock *target_fb;
    
    TRACE("invokeRegisterNatives\n");
    
    member_name = findSystemClass0(SYMBOL(java_lang_invoke_MemberName));

    if(member_name == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: can't find "
                            "java.lang.invoke.MemberName\n");
        exitVM(1);
    }

    clazz_fb = findField(member_name, SYMBOL(clazz),
                                        SYMBOL(sig_java_lang_Class));

    name_fb = findField(member_name, SYMBOL(name),
                                        SYMBOL(sig_java_lang_String));

    type_fb = findField(member_name, SYMBOL(type),
                                        SYMBOL(sig_java_lang_Object));

    flags_fb = findField(member_name, SYMBOL(flags), SYMBOL(I));

    vmtarget_fb = findField(member_name, SYMBOL(vmtarget), SYMBOL(J));

    vmindex_fb = findField(member_name, SYMBOL(vmindex), SYMBOL(J));

    if(clazz_fb == NULL || name_fb == NULL || type_fb == NULL
                        || vmtarget_fb == NULL || vmindex_fb == NULL
    	                || flags_fb == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: Expected fields missing"
                            " in java.lang.invoke.MemberName\n");
        exitVM(1);
    }

    mem_name_clazz_offset = clazz_fb->u.offset;
    mem_name_name_offset = name_fb->u.offset;
    mem_name_type_offset = type_fb->u.offset;
    mem_name_flags_offset = flags_fb->u.offset;
    mem_name_vmtarget_offset = vmtarget_fb->u.offset;
    mem_name_vmindex_offset = vmindex_fb->u.offset;
    
    method_type = findSystemClass0(SYMBOL(java_lang_invoke_MethodType));

    if(method_type == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: can't find "
                            "java.lang.invoke.MethodType\n");
        exitVM(1);
    }

    ptypes_fb = findField(method_type, SYMBOL(ptypes),
                                       SYMBOL(array_java_lang_Class));

    rtype_fb = findField(method_type, SYMBOL(rtype),
                                      SYMBOL(sig_java_lang_Class));

    if(ptypes_fb == NULL || rtype_fb == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: Expected fields missing"
                            " in java.lang.invoke.MethodType\n");
        exitVM(1);
    }

    mthd_type_ptypes_offset = ptypes_fb->u.offset;
    mthd_type_rtype_offset = rtype_fb->u.offset;

    method_handle = findSystemClass0(SYMBOL(java_lang_invoke_MethodHandle));

    if(method_handle == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: can't find "
                            "java.lang.invoke.MethodHandle\n");
        exitVM(1);
    }

    form_fb = findField(method_handle, SYMBOL(form),
                                       SYMBOL(sig_java_lang_invoke_LambdaForm));

    if(form_fb == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: Expected fields missing"
                            " in java.lang.invoke.MethodHandle\n");
        exitVM(1);
    }

    mthd_hndl_form_offset = form_fb->u.offset;

    lambda_form = findSystemClass0(SYMBOL(java_lang_invoke_LambdaForm));

    if(lambda_form == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: can't find "
                            "java.lang.invoke.LambdaForm\n");
        exitVM(1);
    }

    vmentry_fb = findField(lambda_form, SYMBOL(vmentry),
                           SYMBOL(sig_java_lang_invoke_MemberName));

    if(vmentry_fb == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: Expected fields missing"
                            " in java.lang.invoke.LambdaForm\n");
        exitVM(1);
    }

    lmda_form_vmentry_offset = vmentry_fb->u.offset;

    mthd_hndl_natives = findSystemClass0(
                            SYMBOL(java_lang_invoke_MethodHandleNatives));

    if(mthd_hndl_natives == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: can't find "
                            "java.lang.invoke.MethodHandleNatives\n");
        exitVM(1);
    }

    MHN_linkMethod_mb =
         findMethod(mthd_hndl_natives, SYMBOL(linkMethod),
                    SYMBOL(java_lang_invoke_MHN_linkMethod_sig));

    MHN_findMethodType_mb =
         findMethod(mthd_hndl_natives, SYMBOL(findMethodHandleType),
                    SYMBOL(java_lang_invoke_MHN_findMethodType_sig));

    MHN_linkCallSite_mb =
         findMethod(mthd_hndl_natives, SYMBOL(linkCallSite),
                    SYMBOL(java_lang_invoke_MHN_linkCallSite_sig));

    MHN_linkMethodHandleConstant_mb =
         findMethod(mthd_hndl_natives, SYMBOL(linkMethodHandleConstant),
                    SYMBOL(java_lang_invoke_MHN_linkMethodHandleConstant_sig));

    if(MHN_linkMethod_mb == NULL || MHN_linkMethodHandleConstant_mb == NULL ||
       MHN_linkCallSite_mb == NULL || MHN_findMethodType_mb == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: Expected method missing"
                            " in java.lang.invoke.MethodHandleNatives\n");
        exitVM(1);
    }

    call_site = findSystemClass0(SYMBOL(java_lang_invoke_CallSite));

    if(call_site == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: can't find "
                            "java.lang.invoke.CallSite\n");
        exitVM(1);
    }

    target_fb = findField(call_site, SYMBOL(target),
                           SYMBOL(sig_java_lang_invoke_MethodHandle));

    if(target_fb == NULL) {
        jam_fprintf(stderr, "invokeRegisterNatives: Expected fields missing"
                            " in java.lang.invoke.CallSite\n");
        exitVM(1);
    }

    call_site_target_offset = target_fb->u.offset;

    return ostack;
}

// (I)I
uintptr_t *getConstant(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    TRACE("getConstant: type %d\n", (int)*ostack);
    *ostack++ = 0;
    return ostack;
}

/* JVM_CONSTANT_MethodHandle subtypes */
#define REF_getField                1
#define REF_getStatic               2
#define REF_putField                3
#define REF_putStatic               4
#define REF_invokeVirtual           5
#define REF_invokeStatic            6
#define REF_invokeSpecial           7
#define REF_newInvokeSpecial        8
#define REF_invokeInterface         9

#define IS_METHOD      0x10000
#define IS_CONSTRUCTOR 0x20000
#define IS_FIELD       0x40000
#define IS_TYPE        0x80000

#define SEARCH_SUPERCLASSES 0x100000
#define SEARCH_INTERFACES   0x200000

#define ALL_KINDS (IS_METHOD | IS_CONSTRUCTOR | IS_FIELD | IS_TYPE)
                
#define REFERENCE_KIND_SHIFT 24
#define REFERENCE_KIND_MASK  (0xf000000 >> REFERENCE_KIND_SHIFT)

int stackOverflowCheck(ExecEnv *ee, char *sp) {
    if(sp > ee->stack_end) {
        if(ee->overflow++) {
            /* Overflow when we're already throwing stack
               overflow.  Stack extension should be enough
               to throw exception, so something's seriously
               gone wrong - abort the VM! */
            jam_fprintf(stderr, "Fatal stack overflow!  Aborting VM.\n");
            exitVM(1);
        }
        ee->stack_end += STACK_RED_ZONE_SIZE;
        signalException(java_lang_StackOverflowError, NULL);
        return TRUE;
    }

    return FALSE;
}

void executePolyMethod(Object *ob, MethodBlock *mb, uintptr_t *lvars) {
    if(mb->access_flags & ACC_NATIVE)
        (*mb->native_invoker)(mb->class, mb, lvars);
    else {
        ExecEnv *ee = getExecEnv();
        Frame *last = ee->last_frame->prev;
        Frame *dummy = (Frame*)(lvars + mb->max_locals);
        Frame *new_frame = dummy + 1;
        uintptr_t *new_ostack = ALIGN_OSTACK(new_frame + 1);

        if(stackOverflowCheck(ee, (char *)(new_ostack + mb->max_stack)))
            return;

        dummy->prev = last;
        dummy->mb = NULL;
        dummy->ostack = (uintptr_t *)new_frame;

        new_frame->mb = mb;
        new_frame->lvars = lvars;
        new_frame->ostack = new_ostack;
        new_frame->prev = dummy;

        ee->last_frame = new_frame;

        if(mb->access_flags & ACC_SYNCHRONIZED)
            objectLock(ob ? ob : mb->class);

        executeJava();

        if(mb->access_flags & ACC_SYNCHRONIZED)
            objectUnlock(ob ? ob : mb->class);
    }
}

int sigRetSlotSize(char *sig) {
    int len = strlen(sig);

    if(sig[len-2] != ')')
        return 1;

    switch(sig[len-1]) {
        case 'V':
            return 0;
        case 'J':
        case 'D':
            return 2;
        default:
            return 1;
    }
}

uintptr_t *invokeBasic(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Object *method_handle = (Object*)ostack[0];
    Object *form = INST_DATA(method_handle, Object*, mthd_hndl_form_offset);
    Object *vmentry = INST_DATA(form, Object*, lmda_form_vmentry_offset);
    MethodBlock *vmtarget = INST_DATA(vmentry, MethodBlock*, 
    	                              mem_name_vmtarget_offset);

    executePolyMethod(NULL, vmtarget, ostack);

    ostack += mb->native_extra_arg;
    return ostack;
}

uintptr_t *linkToSpecial(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Object *mem_name = (Object*)ostack[mb->args_count-1];
    MethodBlock *vmtarget = INST_DATA(mem_name, MethodBlock*,
    	                              mem_name_vmtarget_offset);

    executePolyMethod(NULL, vmtarget, ostack);

    ostack += mb->native_extra_arg;
    return ostack;
}

uintptr_t *linkToVirtual(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Object *this = (Object*)ostack[0];
    Object *mem_name = (Object*)ostack[mb->args_count-1];
    MethodBlock *vmtarget = INST_DATA(mem_name, MethodBlock*,
    	                              mem_name_vmtarget_offset);

    vmtarget = lookupVirtualMethod(this, vmtarget);
    if(vmtarget != NULL)
        executePolyMethod(this, vmtarget, ostack);

    ostack += mb->native_extra_arg;
    return ostack;
}

uintptr_t *invokeGeneric(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    signalException(java_lang_InternalError, "should not reach here");
    return ostack;
}

// (Ljava/lang/invoke/MemberName;Ljava/lang/Object;)V
uintptr_t *initMemberName(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Object *mname = (Object*)ostack[0];
    Object *target = (Object*)ostack[1];

    TRACE("initMemberName\n");

    if(target->class == method_reflect_class) {
        Class *decl_class = INST_DATA(target, Class*, mthd_class_offset);
        int slot = INST_DATA(target, int, mthd_slot_offset);
        MethodBlock *mb = &(CLASS_CB(decl_class)->methods[slot]);
        int flags = mb->access_flags | IS_METHOD;

        flags |= (mb->access_flags & ACC_STATIC ? REF_invokeStatic
                                                : REF_invokeVirtual)
                  << REFERENCE_KIND_SHIFT;

        INST_DATA(mname, Class*, mem_name_clazz_offset) = mb->class;
        INST_DATA(mname, int, mem_name_flags_offset) = flags;
        INST_DATA(mname, MethodBlock*, mem_name_vmtarget_offset) = mb;

   } else if(target->class == cons_reflect_class) {
        signalException(java_lang_InternalError, "initMemberName: cons unimplemented");
   } else if(target->class == field_reflect_class) {
        Class *decl_class = INST_DATA(target, Class*, fld_class_offset);
        int slot = INST_DATA(target, int, fld_slot_offset);
        FieldBlock *fb = &(CLASS_CB(decl_class)->fields[slot]);
        int flags = fb->access_flags | IS_FIELD;

        flags |= (fb->access_flags & ACC_STATIC ? REF_getStatic
                                                : REF_getField)
                  << REFERENCE_KIND_SHIFT;

        INST_DATA(mname, Class*, mem_name_clazz_offset) = fb->class;
        INST_DATA(mname, int, mem_name_flags_offset) = flags;
        INST_DATA(mname, FieldBlock*, mem_name_vmtarget_offset) = fb;
   } else
        signalException(java_lang_InternalError,
                        "initMemberName: unimplemented target");

    return ostack;
}

// (Ljava/lang/invoke/MemberName;)V
uintptr_t *expandMemberName(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Object *mname = (Object*)ostack[0];

    Class *clazz = INST_DATA(mname, Class*, mem_name_clazz_offset);
    Object *name_str = INST_DATA(mname, Object*, mem_name_name_offset);
    Object *type = INST_DATA(mname, Object*, mem_name_type_offset);
    int flags = INST_DATA(mname, int, mem_name_flags_offset);

    TRACE("expandMemberName\n");
    DEBUG("mname %p\n", mname);

    DEBUG("clazz %p name_str %p type %p flags %x\n", clazz, name_str, type, flags);

    signalException(java_lang_InternalError, "expandMemberName: unimplemented");
    return ostack;
}

uintptr_t *MH_objectFieldOffset(Class *class, MethodBlock *mb,
                                uintptr_t *ostack) {

    Object *mname = (Object*)ostack[0];
    FieldBlock *fb = INST_DATA(mname, FieldBlock*, mem_name_vmtarget_offset);

    TRACE("MH_objectFieldOffset\n");

    *(long long *)ostack = fb->u.offset;
    return ostack + 2;
}

uintptr_t *MH_staticFieldOffset(Class *class, MethodBlock *mb,
                                uintptr_t *ostack) {

    Object *mname = (Object*)ostack[0];
    FieldBlock *fb = INST_DATA(mname, FieldBlock*, mem_name_vmtarget_offset);

    TRACE("MH_staticFieldOffset\n");

    *(long long*)ostack = (uintptr_t)&fb->u.static_value;
    return ostack + 2;
}

uintptr_t *MH_staticFieldBase(Class *class, MethodBlock *mb,
	                      uintptr_t *ostack) {

    TRACE("MH_staticFieldBase\n");

    *ostack++ = 0;
    return ostack;
}

uintptr_t *getMemberVMInfo(Class *class, MethodBlock *mb, uintptr_t *ostack) {

    TRACE("getMemberVMInfo\n");
    signalException(java_lang_InternalError, "getMemberVMInfo: unimplemented");
    return ostack;
}

uintptr_t *setCallSiteTargetNormal(Class *class, MethodBlock *mb, 
	                           uintptr_t *ostack) {

    Object *call_site = (Object *)ostack[0];
    Object *target = (Object *)ostack[1];

    TRACE("setCallSiteTargetNormal\n");

    INST_DATA(call_site, Object*, call_site_target_offset) = target;
    return ostack;
}

uintptr_t *setCallSiteTargetVolatile(Class *class, MethodBlock *mb,
	                             uintptr_t *ostack) {

    Object *call_site = (Object *)ostack[0];
    Object *target = (Object *)ostack[1];

    TRACE("setCallSiteTargetVolatile\n");

    INST_DATA(call_site, Object*, call_site_target_offset) = target;
    return ostack;
}

// (Ljava/lang/Class;Ljava/lang/String;Ljava/lang/String;ILjava/lang/Class;I
//  [Ljava/lang/invoke/MemberName;)I
uintptr_t *getMembers(Class *class, MethodBlock *mb, uintptr_t *ostack) {
    Class *clazz = (Class *)ostack[0];
    Object *match_name = (Object *)ostack[1];
    Object *match_sig = (Object *)ostack[2];
    int match_flags = ostack[3];
    Class *caller = (Class *)ostack[4];
    int skip = ostack[5];
    Object *results = (Object *)ostack[6];
    char *name_sym = NULL, *sig_sym = NULL;

    int search_super = (match_flags & SEARCH_SUPERCLASSES) != 0;
    int search_intf = (match_flags & SEARCH_INTERFACES) != 0;
    int local = !(search_super || search_intf);

    int rlen = ARRAY_LEN(results);
    Object **rpntr = ARRAY_DATA(results, Object*);

    ClassBlock *cb = CLASS_CB(clazz);

    TRACE("getMembers\n");

    if(match_name != NULL) {
        char *str = String2Utf8(match_name);
        name_sym = findUtf8(str);
        sysFree(str);
        if(name_sym == NULL)
            goto no_match;
    }

    if(match_sig != NULL) {
        char *str = String2Utf8(match_sig);
        sig_sym = findUtf8(str);
        sysFree(str);
        if(sig_sym == NULL)
            goto no_match;
    }

    if(match_flags & IS_FIELD)
        goto unimplemented;

    if(!local)
        goto unimplemented;

    if(match_flags & (IS_METHOD | IS_CONSTRUCTOR)) {
        int i, j = 0;

        for(i = cb->methods_count-1; i >= 0; i--) {
            MethodBlock *mb = &cb->methods[i];

            if(mb->name == SYMBOL(class_init))
                continue;
            if(mb->name == SYMBOL(object_init))
                continue;
            if(skip-- > 0)
                continue;

            if(j++ < rlen) {
                Object *mname = *rpntr++;
                int flags = mb->access_flags | IS_METHOD;

                flags |= (mb->access_flags & ACC_STATIC ? REF_invokeStatic
                                                        : REF_invokeVirtual)
                          << REFERENCE_KIND_SHIFT;

                INST_DATA(mname, int, mem_name_flags_offset) = flags;
                INST_DATA(mname, Class*, mem_name_clazz_offset) = mb->class;
                INST_DATA(mname, Object*, mem_name_name_offset) =
                                 findInternedString(createString(mb->name));
                INST_DATA(mname, Object*, mem_name_type_offset) =
                                 createString(mb->type);
                INST_DATA(mname, uintptr_t, mem_name_vmtarget_offset) = mb;
            }
        }

        *ostack++ = j;
        return ostack;
    }

unimplemented:
    signalException(java_lang_InternalError, "getMembers: unimplemented");

no_match:
    *ostack++ = 0;
    return ostack;
}

// (I[Ljava/lang/Object;)I
uintptr_t *getNamedCon(Class *class, MethodBlock *mb, uintptr_t *ostack) {

    TRACE("getNamedCon\n");
    signalException(java_lang_InternalError, "getNamedCon: unimplemented");
    return ostack;
}

// ([Ljava/lang/Object;)Ljava/lang/Object;
uintptr_t *invokeNonExact(Class *class, MethodBlock *mb, uintptr_t *ostack) {

    TRACE("invoke\n");
    signalChainedExceptionName("java/lang/UnsupportedOperationException",
                               "invoke cannot be invoked reflectively",
                               NULL);
    return ostack;
}

// ([Ljava/lang/Object;)Ljava/lang/Object;
uintptr_t *invokeExact(Class *class, MethodBlock *mb, uintptr_t *ostack) {

    TRACE("invokeExact\n");
    signalChainedExceptionName("java/lang/UnsupportedOperationException",
                               "invokeExact cannot be invoked reflectively",
                               NULL);
    return ostack;
}

int class2Signature(Class *class, char *buff[], int pos, int *buff_len)  {
    ClassBlock *cb = CLASS_CB(class);
    int rem, len, name_len;

    if(IS_PRIMITIVE(cb))
        len = 2;
    else {
        name_len = strlen(cb->name);
        len = name_len + (IS_ARRAY(cb) ? 1 : 3);
    }
            
    rem = *buff_len - pos - len;
    if(rem < 0)
        *buff = sysRealloc(*buff, *buff_len += -rem + 128);

    if(IS_PRIMITIVE(cb))
        (*buff)[pos++] = primClass2TypeChar(class);
     else {
        if(!IS_ARRAY(cb))
            (*buff)[pos++] = 'L';

        memcpy(*buff + pos, cb->name, name_len);
        pos += name_len;

        if(!IS_ARRAY(cb))
            (*buff)[pos++] = ';';
    }

    (*buff)[pos] = '\0';
    return pos;
}

char *type2Signature(Object *type, int add_if_absent) {
    char *sig, *found;

    if(IS_CLASS(type)) {
        int buff_len = 0;
        sig = NULL;
        class2Signature(type, &sig, 0, &buff_len);
    } else {
        char *type_classname = CLASS_CB(type->class)->name;
         
        if(type_classname == SYMBOL(java_lang_String))
            sig = String2Utf8(type);
        else
            if(type_classname == SYMBOL(java_lang_invoke_MethodType)) {
                Object *ptypes_array = INST_DATA(type, Object *,
                                                 mthd_type_ptypes_offset);
                Class *rtype = INST_DATA(type, Class *,
                                         mthd_type_rtype_offset);
                Object **ptypes = ARRAY_DATA(ptypes_array, Object*);
                int num_ptypes = ARRAY_LEN(ptypes_array);
                int i, pos, buff_len = 128;

                sig = sysMalloc(128);
                sig[0] = '(';

                for(i = 0, pos = 1; i < num_ptypes; i++)
                    pos = class2Signature(ptypes[i], &sig, pos, &buff_len);

                sig[pos++] = ')';
                class2Signature(rtype, &sig, pos, &buff_len);
            } else {
                signalException(java_lang_InternalError,
                                "unrecognised type");
                return NULL;
            }
    }

    sig = sysRealloc(sig, strlen(sig) + 1);
    if((found = findHashedUtf8(sig, add_if_absent)) != sig)
        sysFree(sig);

    return found;
}

#define ID_invokeGeneric   0
#define ID_invokeBasic     1
#define ID_linkToVirtual   2
#define ID_linkToStatic    3
#define ID_linkToSpecial   4
#define ID_linkToInterface 5

#define isStaticPolymorphicSig(id) (id >= ID_linkToVirtual)

int polymorphicNameID(Class *clazz, char *name) {
    if(CLASS_CB(clazz)->name == SYMBOL(java_lang_invoke_MethodHandle)) {
        if(name == SYMBOL(invoke) || name == SYMBOL(invokeExact))
            return ID_invokeGeneric;
        else if(name == SYMBOL(invokeBasic))
            return ID_invokeBasic;
        else if(name == SYMBOL(linkToVirtual))
            return ID_linkToVirtual;
        else if(name == SYMBOL(linkToStatic))
            return ID_linkToStatic;
        else if(name == SYMBOL(linkToSpecial))
            return ID_linkToSpecial;
        else if(name == SYMBOL(linkToInterface))
            return ID_linkToInterface;
    }

    return -1;
}

NativeMethod polymorphicID2Invoker(int id) {
    switch(id) {
        case ID_invokeBasic:
            return &invokeBasic;
        case ID_linkToSpecial:
        case ID_linkToStatic:
            return &linkToSpecial;
        case ID_linkToVirtual:
        case ID_linkToInterface:
            return &linkToVirtual;
        case ID_invokeGeneric:
            return &invokeGeneric;
    }
}

Object *findMethodHandleType(char *type, Class *accessing_class) {
    Object *method_type, *ptypes;
    char *signature, *sig;
    Class *rtype;

    signature = sig = sysMalloc(strlen(type) + 1);
    strcpy(sig, type);

    ptypes = convertSig2ClassArray(&sig, accessing_class);

    sig += 1;
    rtype = convertSigElement2Class(&sig, accessing_class);

    sysFree(signature);
    if(ptypes == NULL || rtype == NULL)
        return NULL;

    // XXX invokedynamic executed but Java-level stuff hasn't
    // been initialised - find better way to fix this
    if(MHN_findMethodType_mb == NULL)
        invokeRegisterNatives(NULL, NULL, NULL);

    method_type = *(Object**)executeStaticMethod(MHN_findMethodType_mb->class,
                                                 MHN_findMethodType_mb, rtype,
                                                 ptypes);

    if(exceptionOccurred())
        return NULL;

    return method_type;
}

Object *resolveMethodType(Class *class, int cp_index) {
    ConstantPool *cp = &(CLASS_CB(class)->constant_pool);
    Object *mt = NULL;

retry:
    switch(CP_TYPE(cp, cp_index)) {
        case CONSTANT_Locked:
            goto retry;

        case CONSTANT_ResolvedMethodType:
            mt = (Object *)CP_INFO(cp, cp_index);
            break;

        case CONSTANT_MethodType: {
            char *methodtype;
            int type_idx = CP_METHOD_TYPE(cp, cp_index);

            if(CP_TYPE(cp, cp_index) != CONSTANT_MethodType)
                goto retry;

            methodtype = CP_UTF8(cp, type_idx);
            mt = findMethodHandleType(methodtype, class);
            if(mt == NULL)
                return NULL;

            CP_TYPE(cp, cp_index) = CONSTANT_Locked;
            MBARRIER();
            CP_INFO(cp, cp_index) = (uintptr_t)mt;
            MBARRIER();
            CP_TYPE(cp, cp_index) = CONSTANT_ResolvedMethodType;

            break;
        }
    }

    return mt;
}

Object *findMethodHandleConstant(Class *class, int ref_kind,
                                 Class *defining_class,
                                 char *methodname, char *type) {

    // XXX type could be a method or field signature
    // this works for method sig, but not for field
    Object *name_str = findInternedString(createString(methodname));
    Object *method_type = findMethodHandleType(type, class);
    Object *mh;

#if 0
    if(methodname != '(') {
        signalException(java_lang_InternalError,
                        "findMethodHandleConstant: unimplemented");
        return NULL;
    }
#endif

    mh = *(Object**)executeStaticMethod(MHN_linkMethodHandleConstant_mb->class,
                                        MHN_linkMethodHandleConstant_mb,
                                        class, ref_kind, defining_class,
                                        name_str, method_type);

    if(exceptionOccurred())
        return NULL;

   return mh;
}

Object *resolveMethodHandle(Class *class, int cp_index) {
    ConstantPool *cp = &(CLASS_CB(class)->constant_pool);
    Object *mh = NULL;

retry:
    switch(CP_TYPE(cp, cp_index)) {
        case CONSTANT_Locked:
            goto retry;

        case CONSTANT_ResolvedMethodHandle:
            mh = (Object *)CP_INFO(cp, cp_index);
            break;

        case CONSTANT_MethodHandle: {
            Class *resolved_class;
            int cl_idx, name_type_idx;
            char *methodname, *methodtype;
            int ref_idx = CP_METHOD_HANDLE_REF(cp, cp_index);
            int ref_kind = CP_METHOD_HANDLE_KIND(cp, cp_index);

            if(CP_TYPE(cp, cp_index) != CONSTANT_MethodHandle)
                goto retry;

            cl_idx = CP_METHOD_CLASS(cp, ref_idx);
            name_type_idx = CP_METHOD_NAME_TYPE(cp, ref_idx);
            methodname = CP_UTF8(cp, CP_NAME_TYPE_NAME(cp, name_type_idx));
            methodtype = CP_UTF8(cp, CP_NAME_TYPE_TYPE(cp, name_type_idx));

            resolved_class = resolveClass(class, cl_idx, TRUE, FALSE);
            if(resolved_class == NULL)
                return NULL;

            mh = findMethodHandleConstant(class, ref_kind, resolved_class,
                                          methodname, methodtype);

            if(mh == NULL)
                return NULL;

            CP_TYPE(cp, cp_index) = CONSTANT_Locked;
            MBARRIER();
            CP_INFO(cp, cp_index) = (uintptr_t)mh;
            MBARRIER();
            CP_TYPE(cp, cp_index) = CONSTANT_ResolvedMethodHandle;

            break;
        }
    }

    return mh;
}

int cpType2PrimIdx(int type) {
    switch(type) {
        case CONSTANT_Integer:
            return PRIM_IDX_INT;
        case CONSTANT_Float:
            return PRIM_IDX_FLOAT;
        case CONSTANT_Long:
            return PRIM_IDX_LONG;
        case CONSTANT_Double:
            return PRIM_IDX_DOUBLE;
        default:
            return -1;
    }
}

PolyMethodBlock *findInvokeDynamicInvoker(Class *class, char *methodname,
                                          char *type, int boot_mthd_idx) {
    Object *boot_mthd;
    Object *method_type;
    Object *member_name;
    Object *appendix_box;
    PolyMethodBlock *pmb;
    Object *args_array = NULL;
    ClassBlock *cb = CLASS_CB(class);
    ConstantPool *cp = &cb->constant_pool;
    Class *obj_array_class = findArrayClass("[Ljava/lang/Object;");
    Object *name_str = findInternedString(createString(methodname));
    int mthd_idx = BOOTSTRAP_METHOD_REF(cb->bootstrap_methods, boot_mthd_idx);
    int args = BOOTSTRAP_METHOD_ARG_COUNT(cb->bootstrap_methods, boot_mthd_idx);

    if(args != 0) {
        Object **args_data;
        int i;

        args_array = allocArray(obj_array_class, args, sizeof(Object*));
        if(args_array == NULL)
            return NULL;

        args_data = ARRAY_DATA(args_array, Object*);

        for(i = 0; i < args; i++) {
            int idx = BOOTSTRAP_METHOD_ARG(cb->bootstrap_methods,
                                           boot_mthd_idx, i);
            int prim_idx = cpType2PrimIdx(CP_TYPE(cp, idx));

            if(prim_idx != -1)
                args_data[i] = createWrapperObject(prim_idx,
                                                   &CP_INFO(cp, idx),
                                                   REF_SRC_FIELD);
            else
                args_data[i] = (Object*)resolveSingleConstant(class, idx);

            if(args_data[i] == NULL)
                return NULL;
        }
    }

    appendix_box = allocArray(obj_array_class, 1, sizeof(Object*));
    if(appendix_box == NULL)
        return NULL;

    method_type = findMethodHandleType(type, class);
    if(method_type == NULL)
        return NULL;

    boot_mthd = resolveMethodHandle(class, mthd_idx);
    if(boot_mthd == NULL)
        return NULL;

    member_name = *(Object**)executeStaticMethod(MHN_linkCallSite_mb->class,
                                                 MHN_linkCallSite_mb,
                                                 class, boot_mthd, name_str,
                                                 method_type, args_array,
                                                 appendix_box);

   // XXX Check for and intercept LinkageErrors

    if(exceptionOccurred())
        return NULL;

    pmb = sysMalloc(sizeof(PolyMethodBlock));
    pmb->appendix = ARRAY_DATA(appendix_box, Object*)[0];
    pmb->mb = INST_DATA(member_name, MethodBlock*, mem_name_vmtarget_offset);

    return pmb;
}

PolyMethodBlock *resolveInvokeDynamic(Class *class, int cp_index) {
    ConstantPool *cp = &(CLASS_CB(class)->constant_pool);
    PolyMethodBlock *pmb = NULL;

retry:
    switch(CP_TYPE(cp, cp_index)) {
        case CONSTANT_Locked:
            goto retry;

        case CONSTANT_ResolvedInvokeDynamic:
            pmb = (PolyMethodBlock *)CP_INFO(cp, cp_index);
            break;

        case CONSTANT_InvokeDynamic: {
            char *methodname, *methodtype;
            int boot_mthd_idx = CP_INVDYN_BOOT_MTHD(cp, cp_index);
            int name_type_idx = CP_INVDYN_NAME_TYPE(cp, cp_index);

            if(CP_TYPE(cp, cp_index) != CONSTANT_InvokeDynamic)
                goto retry;

            methodname = CP_UTF8(cp, CP_NAME_TYPE_NAME(cp, name_type_idx));
            methodtype = CP_UTF8(cp, CP_NAME_TYPE_TYPE(cp, name_type_idx));

            pmb = findInvokeDynamicInvoker(class, methodname, methodtype,
                                           boot_mthd_idx);

            if(pmb == NULL)
                return NULL;

            CP_TYPE(cp, cp_index) = CONSTANT_Locked;
            MBARRIER();
            CP_INFO(cp, cp_index) = (uintptr_t)pmb;
            MBARRIER();
            CP_TYPE(cp, cp_index) = CONSTANT_ResolvedInvokeDynamic;

            break;
        }
    }

    return pmb;
}

PolyMethodBlock *findMethodHandleInvoker(Class *class, Class *accessing_class,
                                         char *methodname, char *type) {

    Object *name_str = findInternedString(createString(methodname));
    Class *obj_array_class = findArrayClass("[Ljava/lang/Object;");
    PolyMethodBlock *pmb;
    Object *appendix_box;
    Object *member_name;
    Object *method_type;

    if(name_str == NULL || obj_array_class == NULL)
        return NULL;

    appendix_box = allocArray(obj_array_class, 1, sizeof(Object*));
    if(appendix_box == NULL)
        return NULL;

    method_type = findMethodHandleType(type, accessing_class);
    if(method_type == NULL)
        return NULL;

    member_name = *(Object**)executeStaticMethod(MHN_linkMethod_mb->class,
                                             MHN_linkMethod_mb,
                                             accessing_class,
                                             REF_invokeVirtual,
                                             class,
                                             name_str,
                                             method_type,
                                             appendix_box);

    if(exceptionOccurred())
        return NULL;

    pmb = sysMalloc(sizeof(PolyMethodBlock));
    pmb->appendix = ARRAY_DATA(appendix_box, Object*)[0];
    pmb->mb = INST_DATA(member_name, MethodBlock*, mem_name_vmtarget_offset);

    return pmb;
}

MethodBlock *lookupPolymorphicMethod(Class *class, Class *accessing_class,
                                     char *methodname, char *type) {

    int id = polymorphicNameID(class, methodname);
    MethodBlock *mb;

    if(id == -1 || id == ID_invokeGeneric)
        return NULL;

    mb = sysMalloc(sizeof(MethodBlock));
    memset(mb, 0, sizeof(MethodBlock));

    mb->class = class;
    mb->name = methodname;
    mb->type = type;
    mb->args_count = sigArgsCount(type);
    mb->access_flags = ACC_PUBLIC | ACC_PRIVATE | ACC_NATIVE;

    if(isStaticPolymorphicSig(id))
        mb->access_flags |= ACC_STATIC;
    else
        mb->args_count++;

    mb->max_locals = mb->args_count;
    mb->native_extra_arg = sigRetSlotSize(type);
    mb->native_invoker = polymorphicID2Invoker(id);

    return mb;
}

int isPolymorphicRef(Class *class, int cp_index) {
    ConstantPool *cp = &(CLASS_CB(class)->constant_pool);

retry:
    switch(CP_TYPE(cp, cp_index)) {
        case CONSTANT_Locked:
            goto retry;

        case CONSTANT_ResolvedPolyMethod:
            return TRUE;

        case CONSTANT_Methodref: {
            char *methodname;
            Class *resolved_class;
            int cl_idx = CP_METHOD_CLASS(cp, cp_index);
            int name_type_idx = CP_METHOD_NAME_TYPE(cp, cp_index);

            if(CP_TYPE(cp, cp_index) != CONSTANT_Methodref)
                goto retry;

            if(CP_TYPE(cp, cl_idx) != CONSTANT_ResolvedClass)
                return FALSE;

            methodname = CP_UTF8(cp, CP_NAME_TYPE_NAME(cp, name_type_idx));
            resolved_class = (Class*)CP_INFO(cp, cl_idx);

            return polymorphicNameID(resolved_class, methodname) ==
                       ID_invokeGeneric;
        }
    }

    return FALSE;
}

PolyMethodBlock *resolvePolyMethod(Class *class, int cp_index) {
    ConstantPool *cp = &(CLASS_CB(class)->constant_pool);
    PolyMethodBlock *pmb = NULL;

retry:
    switch(CP_TYPE(cp, cp_index)) {
        case CONSTANT_Locked:
            goto retry;

        case CONSTANT_ResolvedPolyMethod:
            pmb = (PolyMethodBlock *)CP_INFO(cp, cp_index);
            break;

        case CONSTANT_Methodref: {
            char *methodname, *methodtype;
            int cl_idx = CP_METHOD_CLASS(cp, cp_index);
            int name_type_idx = CP_METHOD_NAME_TYPE(cp, cp_index);

            if(CP_TYPE(cp, cp_index) != CONSTANT_Methodref)
                goto retry;

            methodname = CP_UTF8(cp, CP_NAME_TYPE_NAME(cp, name_type_idx));
            methodtype = CP_UTF8(cp, CP_NAME_TYPE_TYPE(cp, name_type_idx));

            pmb = findMethodHandleInvoker((Class*)CP_INFO(cp, cl_idx),
                                          class, methodname, methodtype);

            if(pmb == NULL)
                return NULL;

            CP_TYPE(cp, cp_index) = CONSTANT_Locked;
            MBARRIER();
            CP_INFO(cp, cp_index) = (uintptr_t)pmb;
            MBARRIER();
            CP_TYPE(cp, cp_index) = CONSTANT_ResolvedPolyMethod;

            break;
        }
    }

    return pmb;
}

// (Ljava/lang/invoke/MemberName;Ljava/lang/Class;)V
uintptr_t *resolveMemberName(Class *class, MethodBlock *native_mb,
                             uintptr_t *ostack) {

    Object *mname = (Object*)ostack[0];
    char *name_utf, *name_sym, *type_sym;
    Object *name_str, *type;
    Class *clazz;
    int ref_kind;
    int name_id;
    int flags;

    TRACE("resolveMemberName\n");

    clazz = INST_DATA(mname, Class*, mem_name_clazz_offset);
    name_str = INST_DATA(mname, Object*, mem_name_name_offset);
    type = INST_DATA(mname, Object*, mem_name_type_offset);
    flags = INST_DATA(mname, int, mem_name_flags_offset);
    ref_kind = (flags >> REFERENCE_KIND_SHIFT) & REFERENCE_KIND_MASK;

    if(clazz == NULL || name_str == NULL || type == NULL) {
        signalException(java_lang_IllegalArgumentException, NULL);
        return ostack;
    }

    name_utf = String2Utf8(name_str);
    name_sym = findUtf8(name_utf);
    sysFree(name_utf);

    if(name_sym == NULL || name_sym == SYMBOL(class_init))
        goto throw_excep;

    name_id = polymorphicNameID(clazz, name_sym);
    type_sym = type2Signature(type, name_id != -1);
    if(type_sym == NULL)
        goto throw_excep;

    switch(flags & ALL_KINDS) {
        case IS_METHOD: {
            MethodBlock *mb;

            if(IS_INTERFACE(CLASS_CB(clazz)))
                mb = lookupInterfaceMethod(clazz, name_sym, type_sym);
            else {
                mb = lookupMethod(clazz, name_sym, type_sym);
                if(mb == NULL)
                    mb = lookupPolymorphicMethod(clazz, class, name_sym,
                                                 type_sym);
            }

            if(mb == NULL)
                goto throw_excep;

            flags |= mb->access_flags;
            INST_DATA(mname, int, mem_name_flags_offset) = flags;
            INST_DATA(mname, uintptr_t, mem_name_vmtarget_offset) = mb;
            break;
        }
        case IS_CONSTRUCTOR: {
            MethodBlock *mb;

            mb = findMethod(clazz, name_sym, type_sym);
            if(mb == NULL)
                goto throw_excep;

            INST_DATA(mname, uintptr_t, mem_name_vmtarget_offset) = mb;
            break;
        }
        case IS_FIELD: {
            FieldBlock *fb;

            fb = lookupField(clazz, name_sym, type_sym);
            if(fb == NULL)
                goto throw_excep;

            flags |= fb->access_flags;
            INST_DATA(mname, int, mem_name_flags_offset) = flags;
            INST_DATA(mname, FieldBlock*, mem_name_vmtarget_offset) = fb;
            break;
        }

        default:
            goto throw_excep;
    }

    *ostack++ = (uintptr_t)mname;
    return ostack;

throw_excep:
    switch(flags & ALL_KINDS) {
        case IS_METHOD:
        case IS_CONSTRUCTOR:
            signalException(java_lang_NoSuchMethodError, NULL);
            break;

        case IS_FIELD:
            signalException(java_lang_NoSuchFieldError, NULL);
            break;

        default:
            signalException(java_lang_InternalError, "resolveMemberName");
            break;
    }
    return ostack;
}

VMMethod method_handle_natives[] = {
    {"registerNatives",            "()V", invokeRegisterNatives},
    {"getConstant",                "(I)I", getConstant},
    {"resolve",                    "(Ljava/lang/invoke/MemberName;"
                                   "Ljava/lang/Class;)"
                                   "Ljava/lang/invoke/MemberName;",
                                   resolveMemberName},
    {"init",                       "(Ljava/lang/invoke/MemberName;"
                                   "Ljava/lang/Object;)V", initMemberName},
    {"expand",                     "(Ljava/lang/invoke/MemberName;)V",
                                   expandMemberName},
    {"getMembers",                 "(Ljava/lang/Class;Ljava/lang/String;"
                                   "Ljava/lang/String;ILjava/lang/Class;I"
                                   "[Ljava/lang/invoke/MemberName;)I",
                                   getMembers},
    {"getNamedCon",                "(I[Ljava/lang/Object;)I", getNamedCon},
    {"objectFieldOffset",          "(Ljava/lang/invoke/MemberName;)J",
                                   MH_objectFieldOffset},
    {"staticFieldOffset",          "(Ljava/lang/invoke/MemberName;)J",
                                   MH_staticFieldOffset},
    {"staticFieldBase",            "(Ljava/lang/invoke/MemberName;)"
                                   "Ljava/lang/Object;", MH_staticFieldBase},
    {"getMemberVMInfo",            "(Ljava/lang/invoke/MemberName;)"
                                   "Ljava/lang/Object;", getMemberVMInfo},
    {"setCallSiteTargetNormal",    "(Ljava/lang/invoke/CallSite;"
                                   "Ljava/lang/invoke/MethodHandle;)V",
                                   setCallSiteTargetNormal},
    {"setCallSiteTargetVolatile",  "(Ljava/lang/invoke/CallSite;"
                                   "Ljava/lang/invoke/MethodHandle;)V",
                                   setCallSiteTargetVolatile},
    {NULL,                         NULL, NULL}
};

VMMethod method_handle[] = {
    {"invoke",                 "([Ljava/lang/Object;)Ljava/lang/Object;",
                               invokeNonExact},
    {"invokeExact",            "([Ljava/lang/Object;)Ljava/lang/Object;",
                               invokeExact},
    {NULL,                     NULL, NULL}
};

VMClass native_methods[] = {
    {"java/lang/invoke/MethodHandleNatives",      method_handle_natives},
    {"java/lang/invoke/MethodHandle",             method_handle},
    {"sun/misc/Unsafe",                           sun_misc_unsafe},
    {NULL,                                        NULL}
};
