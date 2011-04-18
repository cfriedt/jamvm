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

#include <string.h>

#include "jam.h"
#include "excep.h"
#include "symbol.h"
#include "reflect.h"

static Class *cons_reflect_class, *method_reflect_class;
static Class *field_reflect_class;

static MethodBlock *cons_init_mb, *fld_init_mb, *mthd_init_mb;

static int cons_slot_offset, cons_class_offset, cons_param_offset;
static int mthd_slot_offset, mthd_class_offset, mthd_ret_offset,
           mthd_param_offset;
static int fld_slot_offset, fld_class_offset;

/* Accessed from frame.c */
MethodBlock *mthd_invoke_mb;

int classlibInitReflection() {
    Class *cons_ref_cls, *mthd_ref_cls, *fld_ref_cls;

    FieldBlock *cons_slot_fb, *cons_class_fb, *cons_param_fb;
    FieldBlock *mthd_slot_fb, *mthd_class_fb, *mthd_ret_fb, *mthd_param_fb;
    FieldBlock *fld_slot_fb, *fld_class_fb;

    cons_ref_cls = findSystemClass(SYMBOL(java_lang_reflect_Constructor));
    mthd_ref_cls = findSystemClass(SYMBOL(java_lang_reflect_Method));
    fld_ref_cls = findSystemClass(SYMBOL(java_lang_reflect_Field));

    if(!cons_ref_cls || !mthd_ref_cls || !fld_ref_cls)
        return FALSE;

    cons_slot_fb  = findField(cons_ref_cls, SYMBOL(slot), SYMBOL(I));
    cons_class_fb = findField(cons_ref_cls, SYMBOL(clazz),
                                            SYMBOL(sig_java_lang_Class));
    cons_param_fb = findField(cons_ref_cls, SYMBOL(parameterTypes),
                                            SYMBOL(array_java_lang_Class));

    mthd_slot_fb  = findField(mthd_ref_cls, SYMBOL(slot), SYMBOL(I));
    mthd_class_fb = findField(mthd_ref_cls, SYMBOL(clazz),
                                            SYMBOL(sig_java_lang_Class));
    mthd_ret_fb   = findField(mthd_ref_cls, SYMBOL(returnType),
                                            SYMBOL(sig_java_lang_Class));
    mthd_param_fb = findField(mthd_ref_cls, SYMBOL(parameterTypes),
                                            SYMBOL(array_java_lang_Class));

    fld_slot_fb   = findField(fld_ref_cls,  SYMBOL(slot), SYMBOL(I));
    fld_class_fb  = findField(fld_ref_cls,  SYMBOL(clazz),
                                            SYMBOL(sig_java_lang_Class));

    fld_init_mb = findMethod(fld_ref_cls, SYMBOL(object_init),
                             SYMBOL(java_lang_reflect_field_init_sig));

    cons_init_mb = findMethod(cons_ref_cls, SYMBOL(object_init),
                              SYMBOL(java_lang_reflect_cons_init_sig));

    mthd_init_mb = findMethod(mthd_ref_cls, SYMBOL(object_init),
                              SYMBOL(java_lang_reflect_mthd_init_sig));

    mthd_invoke_mb = findMethod(mthd_ref_cls, SYMBOL(invoke),
                                SYMBOL(java_lang_reflect_mthd_invoke_sig));

    if(!fld_init_mb    || !cons_init_mb  || !mthd_init_mb  ||
       !cons_slot_fb   || !cons_class_fb || !cons_param_fb ||
       !mthd_slot_fb   || !mthd_class_fb || !mthd_ret_fb   ||
       !mthd_param_fb  || !fld_slot_fb   || !fld_class_fb  ||
       !mthd_invoke_mb)  {
        /* Find Field/Method doesn't throw an exception... */
        signalException(java_lang_InternalError,
                        "Expected reflection method/field doesn't exist");
        return FALSE;
    }

    cons_slot_offset = cons_slot_fb->u.offset; 
    cons_class_offset = cons_class_fb->u.offset; 
    cons_param_offset = cons_param_fb->u.offset; 
    mthd_slot_offset = mthd_slot_fb->u.offset; 
    mthd_class_offset = mthd_class_fb->u.offset; 
    mthd_ret_offset = mthd_ret_fb->u.offset; 
    mthd_param_offset = mthd_param_fb->u.offset; 
    fld_slot_offset = fld_slot_fb->u.offset; 
    fld_class_offset = fld_class_fb->u.offset; 

    registerStaticClassRefLocked(&cons_reflect_class, cons_ref_cls);
    registerStaticClassRefLocked(&method_reflect_class, mthd_ref_cls);
    registerStaticClassRefLocked(&field_reflect_class, fld_ref_cls);

    return TRUE;
}

Object *getAnnotationsAsArray(AnnotationData *annotations) {
    Object *array;

    if(annotations == NULL)
        return NULL;

    if((array = allocTypeArray(T_BYTE, annotations->len)) == NULL)
        return NULL;

    memcpy(ARRAY_DATA(array, void*), annotations->data, annotations->len);

    return array;
}

Object *classlibCreateConstructorObject(MethodBlock *mb) {
    AnnotationData *annotations = mb->annotations == NULL ? NULL
                                         : mb->annotations->annotations;
    AnnotationData *parameters = mb->annotations == NULL ? NULL
                                         : mb->annotations->parameters;
    Object *reflect_ob;

    if((reflect_ob = allocObject(cons_reflect_class)) == NULL)
        return NULL;

    executeMethod(reflect_ob, cons_init_mb,
        mb->class,
        getMethodParameterTypes(mb),
        getMethodExceptionTypes(mb),
        mb->access_flags,
        mb - CLASS_CB(mb->class)->methods,
        mb->signature == NULL ? NULL
                      : findInternedString(createString(mb->signature)),
        getAnnotationsAsArray(annotations),
        getAnnotationsAsArray(parameters));

    return reflect_ob;
}

Object *classlibCreateMethodObject(MethodBlock *mb) {
    AnnotationData *annotations = mb->annotations == NULL ? NULL
                                         : mb->annotations->annotations;
    AnnotationData *dft_val = mb->annotations == NULL ? NULL
                                         : mb->annotations->dft_val;
    AnnotationData *parameters = mb->annotations == NULL ? NULL
                                         : mb->annotations->parameters;
    Object *reflect_ob;

    if((reflect_ob = allocObject(method_reflect_class)) == NULL)
        return NULL;

    executeMethod(reflect_ob, mthd_init_mb,
        mb->class,
        findInternedString(createString(mb->name)),
        getMethodParameterTypes(mb),
        getMethodReturnType(mb),
        getMethodExceptionTypes(mb),
        mb->access_flags,
        mb - CLASS_CB(mb->class)->methods,
        mb->signature == NULL ? NULL
                      : findInternedString(createString(mb->signature)),
        getAnnotationsAsArray(annotations),
        getAnnotationsAsArray(parameters),
        getAnnotationsAsArray(dft_val));

    return reflect_ob;
}

Object *classlibCreateFieldObject(FieldBlock *fb) {
    Object *reflect_ob;

    if((reflect_ob = allocObject(field_reflect_class)) == NULL)
        return NULL;

    executeMethod(reflect_ob, fld_init_mb,
        fb->class,
        findInternedString(createString(fb->name)),
        getFieldType(fb),
        fb->access_flags,
        fb - CLASS_CB(fb->class)->fields,
        fb->signature == NULL ? NULL
                      : findInternedString(createString(fb->signature)),
        getAnnotationsAsArray(fb->annotations));
 
    return reflect_ob;
}

Object *enclosingMethodInfo(Class *class) {
    ClassBlock *cb = CLASS_CB(class);

    if(cb->enclosing_class && cb->enclosing_method) {
        Class *enc_class = resolveClass(class, cb->enclosing_class,
                                        TRUE, FALSE);

        if(enc_class != NULL) {
            Class *ary_class = findArrayClass(SYMBOL(array_java_lang_Object));

            if(ary_class != NULL) {
                Object *array = allocArray(ary_class, 3, sizeof(Object*));

                if(array != NULL) {
                    ConstantPool *cp = &cb->constant_pool;
                    char *methodname = CP_UTF8(cp, CP_NAME_TYPE_NAME(cp,
                                                       cb->enclosing_method));
                    char *methodtype = CP_UTF8(cp, CP_NAME_TYPE_TYPE(cp,
                                                       cb->enclosing_method));
                    Object *name = createString(methodname);
                    Object *type = createString(methodtype);

                    if(name != NULL && type != NULL) {
                        ARRAY_DATA(array, Object*)[0] = enc_class;
                        ARRAY_DATA(array, Object*)[1] = name;
                        ARRAY_DATA(array, Object*)[2] = type;

                        return array;
                    }
                }
            }
        }
    }

    return NULL;
}

Object *consNewInstance(Object *reflect_ob, Object *args_array) {
    Object *params = INST_DATA(reflect_ob, Object*, cons_param_offset);
    Class *decl_class = INST_DATA(reflect_ob, Class*, cons_class_offset);
    int slot = INST_DATA(reflect_ob, int, cons_slot_offset);
    MethodBlock *mb = &(CLASS_CB(decl_class)->methods[slot]);

    return constructorConstruct(mb, args_array, params, TRUE, 0);
}

Object *invokeMethod(Object *reflect_ob, Object *ob, Object *args_array) {
    Object *ret = INST_DATA(reflect_ob, Object*, mthd_ret_offset);
    Object *params = INST_DATA(reflect_ob, Object*, mthd_param_offset);
    Class *decl_class = INST_DATA(reflect_ob, Class*, mthd_class_offset);
    int slot = INST_DATA(reflect_ob, int, mthd_slot_offset);
    MethodBlock *mb = &(CLASS_CB(decl_class)->methods[slot]);

    return methodInvoke(ob, mb, args_array, ret, params, TRUE, 0);
}

int typeNo2PrimTypeIndex(int type_no) {
    static char type_map[] = {PRIM_IDX_BOOLEAN, PRIM_IDX_CHAR,
                              PRIM_IDX_FLOAT, PRIM_IDX_DOUBLE,
                              PRIM_IDX_BYTE, PRIM_IDX_SHORT,
                              PRIM_IDX_INT, PRIM_IDX_LONG};

    return type_map[type_no - T_BOOLEAN];
}

int primTypeIndex2Size(int prim_idx) {
    return prim_idx < PRIM_IDX_INT ? prim_idx < PRIM_IDX_CHAR ? 1 : 2
                                   : prim_idx < PRIM_IDX_LONG ? 4 : 8;
}

int widenPrimitiveElement(int src_idx, int dst_idx, void *src_addr,
                          void *dst_addr) {
    u4 widened;

    if(src_idx < PRIM_IDX_INT) {
        if(dst_idx < PRIM_IDX_INT) {
            if(src_idx != dst_idx) {
                if(src_idx != PRIM_IDX_BYTE || dst_idx != PRIM_IDX_SHORT)
                    goto error;
                *(signed short*)dst_addr = *(signed char*)src_addr;
                return TRUE;
            }
            
            if(src_idx < PRIM_IDX_CHAR)
                *(char*)dst_addr = *(char*)src_addr;
            else
                *(short*)dst_addr = *(short*)src_addr;
            return TRUE;
        }

        widened = src_idx < PRIM_IDX_CHAR ? *(signed char*)src_addr : 
                   src_idx == PRIM_IDX_SHORT ? *(signed short*)src_addr
                                             : *(unsigned short*)src_addr;
        src_addr = &widened;
    }

    if(widenPrimitiveValue(src_idx, dst_idx, src_addr, dst_addr,
                           REF_SRC_FIELD | REF_DST_FIELD))
        return TRUE;

error:
    signalException(java_lang_IllegalArgumentException, "can't widen");
    return FALSE;
}

/* Reflection access from JNI */

MethodBlock *classlibMbFromReflectObject(Object *reflect_ob) {
    int is_cons = reflect_ob->class == cons_reflect_class;
    int slot_offset = is_cons ? cons_slot_offset : mthd_slot_offset;
    int class_offset = is_cons ? cons_class_offset : mthd_class_offset;

    Class *decl_class = INST_DATA(reflect_ob, Class*, class_offset);
    int slot = INST_DATA(reflect_ob, int, slot_offset);

    return &(CLASS_CB(decl_class)->methods[slot]);
}

FieldBlock *classlibFbFromReflectObject(Object *reflect_ob) {
    Class *decl_class = INST_DATA(reflect_ob, Class*, fld_class_offset);
    int slot = INST_DATA(reflect_ob, int, fld_slot_offset);

    return &(CLASS_CB(decl_class)->fields[slot]);
}

