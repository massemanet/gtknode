#include "gtknode.h"
#include <string.h>

gboolean  GN_glade_init(int arity, ei_x_buff *XBUF, char *B, int *I){
  char *xml_filename;
  
  if ( ! gn_check_arity(XBUF, 1, arity) ) 
    return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &xml_filename) ) 
    return FALSE; /* free */
  
  if ( gn_glade_init(xml_filename) ) {
    gn_put_void(XBUF);
    free(xml_filename);
    return TRUE;
  }else{
    gn_enc_1_error(XBUF, "glade_init_failed");
    free(xml_filename);
    return FALSE;
  }
}

gboolean GN_value_get(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  gboolean v_boolean;
  gchar* v_string;
  long long ll;
  gdouble v_double;
  
  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;

  if ( ! G_IS_VALUE(object) ) {
    gn_put_atom(XBUF,"NULL");
    return TRUE;
  }

  switch (G_VALUE_TYPE(object)) {
  case G_TYPE_BOOLEAN:
    v_boolean = g_value_get_boolean(object);
    gn_put_boolean(XBUF,v_boolean);
    break;
  case G_TYPE_INT:
    ll = (long long) g_value_get_int(object);
    gn_put_longlong(XBUF, ll);
    break;
  case G_TYPE_UINT:
    ll = (long long) g_value_get_uint(object);
    gn_put_longlong(XBUF, ll);
    break;
  case G_TYPE_LONG:
    ll = (long long) g_value_get_long(object);
    gn_put_longlong(XBUF, ll);
    break;
  case G_TYPE_ULONG:
    ll = (long long) g_value_get_ulong(object);
    gn_put_longlong(XBUF, ll);
    break;
  case G_TYPE_INT64:
    ll = (long long) g_value_get_int64(object);
    gn_put_longlong(XBUF, ll);
    break;
  case G_TYPE_UINT64:
    ll = (long long) g_value_get_uint64(object);
    gn_put_longlong(XBUF, ll);
    break;
  case G_TYPE_FLOAT:
    v_double = (double) g_value_get_float(object);
    gn_put_double(XBUF, v_double);
  case G_TYPE_DOUBLE:
    v_double = g_value_get_double(object);
    gn_put_double(XBUF,v_double);
    break;
  case G_TYPE_STRING:
    v_string = (gchar*) g_value_get_string(object);
    gn_put_string(XBUF, v_string);
    break;
  default:
    gn_put_atom(XBUF,"NULL");
  }
  return TRUE;
}

gboolean GN_value_set(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;
  int type, len;

  gboolean v_boolean;
  gchar v_atom[MAXATOMLEN+1];
  gchar* v_gchar;
  gint64 v_gint64;
  gdouble v_double;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;

  if ( G_IS_VALUE(object) ) g_value_unset(object);

  ei_get_type(B,I,&type,&len);	/* can't fail */
  switch (type) {
  case ERL_ATOM_EXT:
    if ( ! ei_decode_boolean(B, I, (int*)&v_boolean) ){
      g_value_init(object,G_TYPE_BOOLEAN);
      g_value_set_boolean(object, v_boolean);
    } else if ( gn_get_arg_gchar_fix(XBUF, B, I, v_atom) ){
      g_value_init(object,G_TYPE_STRING);
      g_value_set_string(object,v_atom);
    } else {
      return FALSE;
    }
    break;
  case ERL_SMALL_INTEGER_EXT:
  case ERL_INTEGER_EXT:
  case ERL_SMALL_BIG_EXT:
  case ERL_LARGE_BIG_EXT:
    if ( ! gn_get_arg_gint64(XBUF, B, I, &v_gint64) ) return FALSE;
    g_value_init(object,G_TYPE_INT64);
    g_value_set_int64(object, v_gint64);
    break;
  case ERL_FLOAT_EXT:
    if ( ! gn_get_arg_gdouble(XBUF, B, I, &v_double) ) return FALSE;
      g_value_init(object,G_TYPE_DOUBLE);
    g_value_set_double(object, v_double);
    break;
  case ERL_NIL_EXT:
  case ERL_STRING_EXT:
    if ( ! gn_get_arg_gchar(XBUF, B, I, &v_gchar) ) return FALSE;
    g_value_init(object,G_TYPE_STRING);
    g_value_set_string(object,v_gchar);
    free(v_gchar);
    break;
  default:
    gn_enc_1_error(XBUF, "bad_erl_type");
    return FALSE;
    break;
  }
  gn_put_void(XBUF);
  return TRUE;
}

gboolean GN_value_unset(int ARI, ei_x_buff *XBUF, char *B, int *I){

  GValue* object;

  /* no return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "GValue", (void**)&object) ) return FALSE;
  if ( G_IS_VALUE(object) ) g_value_unset(object);
  gn_put_void(XBUF);
  return TRUE;
}

gboolean GN_tree_view_get_selected(int ARI, ei_x_buff *XBUF, char *B, int *I){
  GtkTreeView *tv;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  gchar* path;
  
  if ( ! gn_check_arity(XBUF, 1, ARI) ) 
    return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_TREE_VIEW, (GObject**)&tv) ) 
    return FALSE;
  
  model = gtk_tree_view_get_model(tv);
  selection = gtk_tree_view_get_selection(tv);
  
  GList* list = gtk_tree_selection_get_selected_rows(selection, &model);
  
  gn_wrap_reply("ok", XBUF);
  
  while ( list ) {
    path = gtk_tree_path_to_string( (GtkTreePath*) list->data);
    g_assert( ! ei_x_encode_list_header(XBUF, 1) );
    g_assert( ! ei_x_encode_string(XBUF, path) );
    g_free(path);
    list = list->next;
  }
  g_list_free(list);
  g_assert( ! ei_x_encode_empty_list(XBUF));
  return TRUE;
}  

gboolean GN_pango_layout_set_text(int ARI, ei_x_buff *XBUF, char *B, int *I){
  char* text;
  char* descr_str;
  PangoLayout* layout;
  PangoFontDescription* descr;

  /* no return value */
  
  if ( ! gn_check_arity(XBUF, 3, ARI) ) 
    return FALSE;
  if ( ! gn_get_arg_struct(XBUF, B, I, "PangoLayout", (void**)&layout) ) 
    return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &text) )  /* free */
    return FALSE;
  if ( ! gn_get_arg_gchar(XBUF, B, I, &descr_str) ) { /* free */
    free(text);
    return FALSE;
  }
  
  descr = pango_font_description_from_string(descr_str);
  pango_layout_set_font_description(layout, descr);
  pango_layout_set_text(layout,text,(int)strlen(text));
  pango_font_description_free(descr);
  
  gn_put_void(XBUF);
  free(text);
  free(descr_str);
  return TRUE;
}

gboolean GN_widget_get_attr(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  gchar attr[MAXATOMLEN+1];
  GtkWidget* widget;
  
  if ( ! gn_check_arity(XBUF, 2, ARI) ) return FALSE;
  if ( ! gn_get_arg_object(XBUF, B, I, GTK_TYPE_WIDGET, (GObject**)&widget) ) 
    return FALSE;
  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, attr) ) return FALSE;
  
  if ( strcmp("window",attr) == 0 ){
    gn_put_object(XBUF,(GObject*) widget->window);
  }else if ( strcmp("x",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.x);
  }else if ( strcmp("y",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.y);
  }else if ( strcmp("width",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.width);
  }else if ( strcmp("height",attr) == 0 ) {
    gn_put_longlong(XBUF,(long long)widget->allocation.height);
  }else{
    gn_enc_2_error(XBUF, "no_such_attr");
    ei_x_encode_atom(XBUF, attr);
    return FALSE;
  }
  return TRUE;
}
