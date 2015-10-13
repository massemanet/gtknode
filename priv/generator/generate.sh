#!/bin/bash

function incdir() {
    local BASE=gtk+-2.0
    pkg-config --cflags $BASE | sed -n -e "s/^.*-I\([^ ]*include\/$1\).*$/\1/p"
}

function canonic() {
    readlink -e $1
}

function gennifer() {
    local PREFIX=$1
    shift
    local INCLUDES="$@"
    local WHITELIST=$GEN_DIR/${PREFIX}_white.txt
    local WHITES=$(echo $INCLUDES | tr " " "\n" | grep -f $WHITELIST)
    cp $GEN_DIR/${PREFIX}_predefined.defs $TMP/${PREFIX}_generated.defs
    python $GEN_DIR/h2def.py $INCLUDES >> $TMP/${PREFIX}_generated.defs
    python $GEN_DIR/h2def.py $WHITES > $TMP/${PREFIX}_white_generated.defs
}

function genfiles() {
    PREFIX=$1
    echo \
        $PREFIX \
        $TMP/${PREFIX}_generated.defs \
        $TMP/${PREFIX}_white_generated.defs \
        $GEN_DIR/${PREFIX}_black.txt \
        $TMP/${PREFIX}_generated.h \
        $TMP/${PREFIX}_funcs.html \
        $TMP/${PREFIX}_crap_funcs.txt \
        $TMP/${PREFIX}_crap_types.txt
}

PKG_CONFIG_PATH=/usr/local/Cellar/libxml2/2.9.2/lib/pkgconfig
GEN_DIR=$(canonic $(dirname $BASH_SOURCE))
C_SRC_DIR=$(canonic $GEN_DIR/../c_src)
EBIN_DIR=$(canonic $GEN_DIR/../../ebin)
TMP=$GEN_DIR/build
GTK_VERSION=$(pkg-config --modversion gtk+-2.0)
GTKDOCLINKS=no

[ -d $TMP ] && rm -rf $TMP
mkdir $TMP

G_INCLUDES=$(echo $(incdir glib-2.0)/*/*.h)
GTK_INCLUDES=$(echo $(incdir gtk-2.0)/gtk/*.h)
GDK_INCLUDES=$(echo $(incdir gtk-2.0)/gdk/*.h)
GDK_PIXBUF_INCLUDES=$(echo $(incdir gdk-pixbuf-2.0)/*/*.h)

gennifer g   $G_INCLUDES
gennifer gdk $GDK_INCLUDES $GDK_PIXBUF_INCLUDES
gennifer gtk $GTK_INCLUDES

erl -noinput -pa $EBIN_DIR \
    -run generator go \
    $GTKDOCLINKS $GTK_VERSION $C_SRC_DIR/gtknode_structs.c \
    $(genfiles g) $(genfiles gdk) $(genfiles gtk) \
    -s erlang halt
