SUBDIRS = os
DIST_SUBDIRS = os arch

bin_PROGRAMS = jamvm

lib_LTLIBRARIES = libjvm.la
noinst_LTLIBRARIES = libcore.la

libcore_la_SOURCES = alloc.c alloc.h cast.c class.c dll.c excep.c execute.c \
                     frame.h hash.c hash.h interp.c jam.h jni.c lock.c lock.h \
                     natives.c reflect.c resolve.c sig.h string.c thread.c \
                     thread.h utf8.c zip.c zip.h properties.c direct.c \
                     interp.h interp-direct.h interp-indirect.h natives.h \
                     dll_ffi.c access.c frame.c jni.h init.c hooks.c

jamvm_SOURCES = jam.c
libjvm_la_SOURCES =

jamvm_LDADD = libcore.la
libjvm_la_LIBADD = libcore.la
libcore_la_LIBADD = os/@os@/@arch@/libnative.la os/@os@/libos.la

AM_CPPFLAGS = -DINSTALL_DIR=\"${prefix}\" -DCLASSPATH_INSTALL_DIR=\"${with_classpath_install_dir}\"

DISTCLEANFILES = arch.h
