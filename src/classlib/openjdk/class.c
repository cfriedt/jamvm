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

#include <string.h>

#include "jam.h"
#include "hash.h"
#include "class.h"
#include "symbol.h"

/* Cached offset of classes field in java.lang.ClassLoader objects */
int ldr_classes_offset;

#define CLASS_INITSZE 1<<8

void classlibCacheClassLoaderFields(Class *loader_class) {
    FieldBlock *ldr_fb = findField(loader_class, SYMBOL(classes),
                                                 SYMBOL(sig_java_util_vector));

    if(ldr_fb == NULL) {
        jam_fprintf(stderr, "Expected \"classes\" field missing in "
                            "java.lang.ClassLoader\n");
        exitVM(1);
    }

    hideFieldFromGC(ldr_fb);

    ldr_classes_offset = ldr_fb->u.offset;
}

HashTable *classlibLoaderTable(Object *class_loader) {
    void *pntr = INST_DATA(class_loader, void*, ldr_classes_offset);

    if(isObject(pntr))
        return NULL;

    return pntr;
}

HashTable *classlibCreateLoaderTable(Object *class_loader) {
    HashTable *table = sysMalloc(sizeof(HashTable));

    initHashTable((*table), CLASS_INITSZE, TRUE);
    INST_DATA(class_loader, HashTable*, ldr_classes_offset) = table;

    return table;
}

Object *classlibBootPackage(PackageEntry *package_entry) {
    char *entry = getBootClassPathEntry(package_entry->index);
    return createString(entry);
}

Object *classlibBootPackages(PackageEntry *package_entry) {
    char *name = package_entry->name;
    char padded[strlen(name) + 2];
    Object *string;
    
    strcat(strcpy(padded, name), " ");
    string = createString(padded);

    return string;
}

Class *classlibBootPackagesArrayClass() {
    return findArrayClass(SYMBOL(array_java_lang_String));
}

/* Add a library unloader object to the class loader for the
   library contained within entry.  The library has an unload
   function, which will be called from the unloader finalizer
   when the class loader is garbage collected */
void classlibNewLibraryUnloader(Object *class_loader, void *entry) {
}

char *classlibDefaultBootClassPath() {
    static char *entries[] = {"lib/resources.jar",
                              "lib/rt.jar",
                              "lib/sunrsasign.jar",
                              "lib/jsse.jar",
                              "lib/jce.jar",
                              "lib/charsets.jar",
                              "classes",
                              NULL};
    char *java_home = getJavaHome();
    char *path, *pntr;
    int i, j, len = 0;

    for(i = 0; entries[i] != NULL; i++)
        len += strlen(entries[i]);

    if(i == 0)
        return "";

    pntr = path = sysMalloc(len + i * (strlen(java_home) + 2));

    for(j = 0; j < i - 1; j++)
        pntr += sprintf(pntr, "%s/%s:", java_home, entries[j]);

    sprintf(pntr, "%s/%s", java_home, entries[j]);
    
    return path;
}

char *classlibDefaultExtDirs() {
    char *java_home = getJavaHome();
    char *ext_dirs = sysMalloc(strlen(java_home) + 
                               sizeof("/lib/ext:/usr/java/packages/lib/ext"));

    return strcat(strcpy(ext_dirs, java_home),
                  "/lib/ext:/usr/java/packages/lib/ext");
}

char *classlibDefaultEndorsedDirs() {
    char *java_home = getJavaHome();
    char *endorsed_dirs = sysMalloc(strlen(java_home) +
                                    sizeof("/lib/endorsed"));

    return strcat(strcpy(endorsed_dirs, java_home), "/lib/endorsed");
}

char *classlibBootClassPathOpt(char *cmdlne_bcp, char bootpathopt) {
    char *bootpath;

    switch(bootpathopt) {
        case 'a':
        case 'p': {
            char *default_bcp = classlibDefaultBootClassPath();

            bootpath = sysMalloc(strlen(default_bcp) + strlen(cmdlne_bcp) + 2);
            if(bootpathopt == 'a')
                strcat(strcat(strcpy(bootpath, default_bcp), ":"), cmdlne_bcp);
            else
                strcat(strcat(strcpy(bootpath, cmdlne_bcp), ":"), default_bcp);
            break;
        }

        default:
            bootpath = sysMalloc(strlen(cmdlne_bcp) + 1);
            strcpy(bootpath, cmdlne_bcp);
    }           

    return bootpath;
}

void classlibInitialiseClass() {
}

