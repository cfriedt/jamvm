SUBDIRS = os
DIST_SUBDIRS = os arch

bin_PROGRAMS = jamvm
include_HEADERS = jni.h

lib_LTLIBRARIES = libjvm.la
noinst_LTLIBRARIES = libcore.la

libcore_la_SOURCES = alloc.c alloc.h cast.c class.c dll.c excep.c execute.c \
                     frame.h hash.c hash.h interp.c jam.h jni.c lock.c lock.h \
                     natives.c reflect.c resolve.c sig.h string.c thread.c \
                     thread.h utf8.c zip.c zip.h properties.c direct.c \
                     interp.h interp-direct.h interp-indirect.h natives.h \
                     dll_ffi.c access.c frame.c init.c hooks.c class.h

jamvm_SOURCES = jam.c
libjvm_la_SOURCES =

jamvm_LDADD = libcore.la
libjvm_la_LIBADD = libcore.la
libcore_la_LIBADD = os/@os@/@arch@/libnative.la os/@os@/libos.la

DISTCLEANFILES = arch.h
