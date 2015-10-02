$(ERL) -noinput -run generator go "@GTKDOCLINKS@" \
        "$(GTKVSN)" "$(STRUCTS)" \
        g $(G_I) $(G_W_I) $(srcdir)/g_black.txt \
        g_generated.h g_funcs.html \
        g_crap_funcs.txt g_crap_types.txt \
        gdk $(GDK_I) $(GDK_W_I) $(srcdir)/gdk_black.txt \
        gdk_generated.h gdk_funcs.html \
        gdk_crap_funcs.txt gdk_crap_types.txt \
        gtk $(GTK_I) $(GTK_W_I) $(srcdir)/gtk_black.txt \
        gtk_generated.h gtk_funcs.html \
        gtk_crap_funcs.txt gtk_crap_types.txt \
        -s erlang halt
