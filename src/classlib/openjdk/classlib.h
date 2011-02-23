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


/* Thread */

extern char classLibInitJavaThread(Thread *thread, Object *jlthread,
                                   Object *name, Object *group,
                                   char is_daemon, int priority);

extern Object *classLibThreadPreInit(Class *thread_class,
                                     Class *thrdGrp_class);

#define classLibCreateJavaThread(thread, jThread) \
    /* NOTHING TO DO */ TRUE

#define classLibMarkThreadTerminated(jThread) \
    /* NOTHING TO DO */

#define classLibThreadIdName() SYMBOL(tid)
#define classLibAddThreadName() SYMBOL(add)
#define classLibThreadNameType() SYMBOL(array_C)
#define classLibRemoveThreadName() SYMBOL(remove)
#define classLibExceptionHandlerName() SYMBOL(uncaughtExceptionHandler)

#define classLibJThread2Thread(jThread) \
    findThreadById(jThread2ThreadId(jThread))

extern int classlibGetThreadState(Thread *thread);
extern void classlibSetThreadState(Thread *thread, int state);
extern void classlibThreadName2Buff(Object *jThread, char *buffer,
                                    int buff_len);

extern int classlibInitialiseSignals();
extern void classlibSignalThread(Thread *self);

/* Class */

extern void classlibInitialiseClass();
extern void classlibCacheClassLoaderFields(Class *loader_class);
extern HashTable *classlibLoaderTable(Object *class_loader);
extern HashTable *classlibCreateLoaderTable(Object *class_loader);
extern Object *classlibBootPackage(PackageEntry *entry);
extern Object *classlibBootPackages(PackageEntry *entry);
extern Class *classlibBootPackagesArrayClass();
extern char *classlibBootClassPathOpt(char *cmdlne_bcp, char bootpathopt);
extern char *classlibDefaultBootClassPath();
extern char *classlibDefaultEndorsedDirs();
extern char *classlibDefaultExtDirs();

extern void classlibNewLibraryUnloader(Object *class_loader, void *entry);


/* Reflection */

extern int classlibInitReflection();
extern Object *classlibCreateConstructorObject(MethodBlock *mb);
extern Object *classlibCreateMethodObject(MethodBlock *mb);
extern Object *classlibCreateFieldObject(FieldBlock *fb);
extern MethodBlock *classlibMbFromReflectObject(Object *reflect_ob);
extern FieldBlock *classlibFbFromReflectObject(Object *reflect_ob);


/* DLL */

extern int classlibInitialiseDll();
extern char *classlibDefaultBootDllPath();
extern void *classlibLookupLoadedDlls(char *name, Object *loader);


/* JNI */

extern int classlibInitialiseJNI();
extern Object *classlibNewDirectByteBuffer(void *addr, long long capacity);
extern void *classlibGetDirectBufferAddress(Object *buff);
extern long long classlibGetDirectBufferCapacity(Object *buff);
extern Object *classlibCheckIfOnLoad(Frame *last);


/* Properties */

#define classlibAddDefaultProperties(properties) \
    /* NOTHING TO DO */

extern char *classlibDefaultJavaHome();

/* Access */

extern int classlibAccessCheck(Class *class1, Class *class2);

/* Natives */

extern int classlibInitialiseNatives();

/* Excep */

extern void classlibInitialiseException(Class *throw_class);

/* Frame */

extern int classlibInitialiseFrame();
extern Frame *classlibGetCallerFrame(Frame *last, int depth);
extern int classlibIsSkippedReflectFrame(Frame *frame);

/* Shutdown */

extern void classlibVMShutdown();

/* Alloc */

#define classlibMarkSpecial(ob, mark) \
    /* NOTHING TO DO */

#define classlibHandleUnmarkedSpecial(ob) \
    /* NOTHING TO DO */

