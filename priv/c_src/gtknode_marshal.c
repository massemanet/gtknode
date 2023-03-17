#include "gtknode.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

static gboolean gn_get_arg_gtype(ei_x_buff *XBUF, char *B, int *I, GType *gt);

static void hash_init() {
  extern GHashTable* ghash;
  if ( ! ghash ) ghash = g_hash_table_new(g_str_hash, g_str_equal);
}

void hash_insert(gchar* key, void* value) {
  extern GHashTable* ghash;
  hash_init();
  g_hash_table_insert(ghash, (gpointer) g_strdup(key), (gpointer) value);
}

void* hash_lookup(gchar* key) {
  extern GHashTable* ghash;
  hash_init();
  return (void*) g_hash_table_lookup(ghash, (gconstpointer) key);
}
static void ghFunc(gpointer key,gpointer value,gpointer user_data){
  g_message("hash_foreach %s %d", (gchar*) key, value);
}
static void hash_printall() {
  extern GHashTable* ghash;
  hash_init();
  g_hash_table_foreach(ghash, ghFunc, NULL);
}

/***************************************************************************/
static char* get_type_name(const gchar* name) {
  char c;
  static char str[100];
  int i = 1;
  int ii = 1;

  str[0] = tolower(name[0]);
  while ( c = name[i++] ){
    if ( isupper(c) ) {
      str[ii++] = '_';
      str[ii++] = tolower(c);
    }else{
      str[ii++] = c;
    }
  }
  str[ii] = 0;
  return strcat(str,"_get_type");
}

GType gn_GType_from_name(const gchar* name) {
  extern GModule *gmod;
  GType (*func)(void);
  GType gtp;
  char* tname;

  /* is this type registered? */
  if ( (gtp = g_type_from_name(name) ) )
    return gtp;
  /* try to instantiate the type */
  tname = get_type_name(name);
  if ( g_module_symbol(gmod, tname, (gpointer *)&func ) )
    if ( (gtp = (*func)() ) )
      return gtp;
  /* no dice... */
  return (GType)NULL;
}
/***************************************************************************/
/* wrap all answers in this */
/* {{self(),reply|signal|error|handshake},...} */
/***************************************************************************/

void gn_wrap_ans(char* tag, ei_x_buff* xbuf) {
  extern ei_cnode ec;
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_pid(xbuf, ei_self(&ec));
  ei_x_encode_atom(xbuf, tag);
}

void gn_wrap_reply(char* tag, ei_x_buff* xbuf) {
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_atom(xbuf, tag);
}
/***************************************************************************/
/* error tuple builders */
/***************************************************************************/
/* {{Node,error}, {Err,...}} */
void gn_enc_2_error(ei_x_buff *xbuf, char *err) {
  gn_wrap_reply("error", xbuf);
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_atom(xbuf, err);
}

/* {{Node,error}, Err} */
void gn_enc_1_error(ei_x_buff *xbuf, char *err) {
  gn_wrap_reply("error", xbuf);
  ei_x_encode_atom(xbuf, err);
}

/***************************************************************************/
/* marshal return values from gtk calls */
/* {{Node,reply}, ...} */
/***************************************************************************/
void gn_put_tuple(ei_x_buff *xbuf, int N) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_tuple_header(xbuf,N));
}

void gn_put_void(ei_x_buff *xbuf) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_atom(xbuf, "void") );
}

void gn_put_pid(ei_x_buff *xbuf, erlang_pid *p) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_pid(xbuf, p) );
}

void gn_put_boolean(ei_x_buff *xbuf, int i) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_boolean(xbuf, i) );
}

void gn_put_double(ei_x_buff *xbuf, double d) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_double(xbuf, d) );
}

void gn_put_longlong(ei_x_buff *xbuf, long long l) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_longlong(xbuf, l) );
}

void gn_put_ulonglong(ei_x_buff *xbuf, unsigned long long l) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_ulonglong(xbuf, l) );
}

void gn_put_atom(ei_x_buff *xbuf, char *p) {
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_atom(xbuf, p) );
}

void gn_put_string(ei_x_buff *xbuf, char *p) {
  gn_wrap_reply("ok", xbuf);
  if ( p == NULL ) {
    ei_x_encode_string(xbuf, "");
  }else{
    ei_x_encode_string(xbuf, p);
  }
}

void gn_store_obj(gchar *bf, GObject *w) {

  sprintf(bf,"%d", (long)w);
  if ( ! hash_lookup(bf) ) {
    /* ref all objs that are sent to erl to avoid garbing */
    g_object_ref(w);
    hash_insert(bf, (void*)w);
  }
}

void gn_put_object(ei_x_buff *xbuf, GObject *w) {
  gchar bf[24];

  if ( G_IS_OBJECT(w) ){
    gn_store_obj(bf,w);
  } else if ( w == 0 ) {
    memcpy(bf,"NULL",5);
  } else {
    g_critical("put_object called with non-object");
  }
  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_atom(xbuf, bf));
}


/* if GLib decides to free a struct we're hosed */
/* i think one could rig up some freeing callback and */
/*remove the pointer from the hash, but i'm not sure how */
void gn_put_struct(ei_x_buff *xbuf, char *type, void *struct_p) {
  gchar key[200];
  gchar name[24];

  g_assert( (sprintf(name,"%d", (long)struct_p) < sizeof(name)) );
  g_assert( (strlen(type)+sizeof(name)+4) < sizeof(key) );
  memcpy(key, type, strlen(type)+1);
  strcat(key, "-");
  strcat(key, name);

  g_assert( hash_lookup(key) == NULL );
  hash_insert(key, struct_p);

  gn_wrap_reply("ok", xbuf);
  g_assert( ! ei_x_encode_atom(xbuf, name));
}


void gn_put_enum(ei_x_buff *xbuf, char* type_name, gint enum_val) {
  char buf[100];
  gn_wrap_reply("ok", xbuf);
  g_assert( gn_get_enum_name(type_name, enum_val, buf) );
  g_assert( ! ei_x_encode_atom(xbuf, buf));
}

void gn_put_flags(ei_x_buff *xbuf, char* type_name, guint flags) {
  GType gtyp;
  GFlagsClass* fclass;
  GFlagsValue* fval;

  gn_wrap_reply("ok", xbuf);
  g_assert( (gtyp = gn_GType_from_name(type_name)));
  g_assert(G_TYPE_IS_FLAGS(gtyp));
  fclass = g_type_class_ref(gtyp);
  while (flags) {
    fval = g_flags_get_first_value(fclass,flags);
    flags = flags & ~fval->value;
    g_assert( ! ei_x_encode_list_header(xbuf, 1) );
    g_assert( ! ei_x_encode_atom(xbuf, fval->value_name) );
  }
  g_assert( ! ei_x_encode_empty_list(xbuf));
  g_type_class_unref(fclass);
}


/***************************************************************************/
/* marshalling erlang -> gtk */
/***************************************************************************/
/*****************************************************************************/
gboolean gn_get_arg_pid(ei_x_buff *xbuf, char *B, int *I, erlang_pid *pid){
  if ( ! ei_decode_pid(B ,I, pid) ) return TRUE;
  gn_enc_1_error(xbuf, "bad_pid");
  return FALSE;
}

gboolean gn_get_arg_gboolean(ei_x_buff *xbuf, char *B, int *I, gboolean *a) {
  if ( ! ei_decode_boolean(B, I, a) ) return TRUE;
  gn_enc_1_error(xbuf, "bad_boolean");
  return FALSE;
}

gboolean gn_get_arg_gchar_fix(ei_x_buff *xbuf, char *B, int *I, char *a){
  if ( ! ei_decode_atom(B, I, a) ) return TRUE;
  gn_enc_1_error(xbuf, "bad_atom");
  return FALSE;
}

/* if gn_get_arg_(g)char returns TRUE, *a must be freed */
gboolean gn_get_arg_char(ei_x_buff *xbuf, char *B, int *I, char **a){
  gn_get_arg_gchar(xbuf,B,I,a);
}
gboolean gn_get_arg_gchar(ei_x_buff *xbuf, char *B, int *I, gchar **a) {
  int type, size, dummy;

  if ( ei_get_type(B, I, &type, &size) ) {
    gn_enc_1_error(xbuf, "bad_type");
    return FALSE;
  }

  *a = (gchar *)malloc(size+1);

  if ( ! ei_decode_string(B, I, *a) ) return TRUE;

  free(*a);
  gn_enc_1_error(xbuf, "bad_string");
  return FALSE;
}

gboolean gn_get_arg_int(ei_x_buff *xbuf, char *B, int *I, int *a) {
  gint64 ll;

  if ( ! gn_get_arg_gint64(xbuf, B, I, &ll) ) return FALSE;
  *a = ll;
  return TRUE;
}
gboolean gn_get_arg_gint(ei_x_buff *xbuf, char *B, int *I, gint *a) {
  gint64 ll;

  if ( ! gn_get_arg_gint64(xbuf, B, I, &ll) ) return FALSE;
  *a = ll;
  return TRUE;
}
gboolean gn_get_arg_glong(ei_x_buff *xbuf, char *B, int *I, glong *a) {
  gint64 ll;

  if ( ! gn_get_arg_gint64(xbuf, B, I, &ll) ) return FALSE;
  *a = ll;
  return TRUE;
}
gboolean gn_get_arg_gint64(ei_x_buff *xbuf, char *B, int *I, gint64 *a) {
  long long ll;
  if ( ! ei_decode_longlong(B ,I, &ll) ) {
    *a = ll;
    return TRUE;
  }
  gn_enc_1_error(xbuf, "bad_longlong");
  return FALSE;
}

gboolean gn_get_arg_guint16(ei_x_buff *xbuf, char *B, int *I, guint16 *a) {
  guint gui;
  if ( ! gn_get_arg_guint(xbuf,B,I,&gui) ) return FALSE;
  *a = gui;
  return TRUE;
}
gboolean gn_get_arg_guint32(ei_x_buff *xbuf, char *B, int *I, guint32 *a) {
  guint gui;
  if ( ! gn_get_arg_guint(xbuf,B,I,&gui) ) return FALSE;
  *a = gui;
  return TRUE;
}
gboolean gn_get_arg_guint(ei_x_buff *xbuf, char *B, int *I, guint *a) {
  unsigned long long ull;
  if ( ! ei_decode_ulonglong(B, I, &ull) ) {
    *a = ull;
    return TRUE;
  }
  gn_enc_1_error(xbuf, "bad_uint");
  return FALSE;
}

gboolean gn_get_arg_gfloat(ei_x_buff *xbuf, char *B, int *I, gfloat *a) {
  gdouble dbl;
  if ( ! gn_get_arg_gdouble(xbuf, B, I, &dbl) ) return FALSE;
  *a = dbl;
  return TRUE;
}
gboolean gn_get_arg_gdouble(ei_x_buff *xbuf, char *B, int *I, gdouble *a) {
  if ( ! ei_decode_double(B, I, a) ) return TRUE;
  gn_enc_1_error(xbuf, "bad_double");
  return FALSE;
}

gboolean gn_get_arg_enum(ei_x_buff *xbuf, char *B, int *I, gchar* ec, gint *i) {
  gchar enum_name[MAXATOMLEN+1];
  if ( ! gn_get_arg_gchar_fix(xbuf, B, I, enum_name) ) return FALSE;
  if ( ! gn_get_enum_val(xbuf, ec, enum_name, i) ) return FALSE;
  return TRUE;
}

gboolean gn_get_arg_flags(ei_x_buff *xbuf, char *B, int *I,
                          gchar* type_name, gint *flags) {
  GType gtyp;
  GFlagsClass* fclass;
  GFlagsValue* fval;
  char flag_name[MAXATOMLEN+1];
  int list_len;

  *flags = 0;
  if ( (list_len = gn_get_list(xbuf,B,I)) < 0 ) return FALSE;
  if ( ! (gtyp = gn_GType_from_name(type_name))) {
    gn_enc_2_error(xbuf, "bad_flag_type");
    ei_x_encode_atom(xbuf, type_name);
    return FALSE;
  }
  if ( ! G_TYPE_IS_FLAGS(gtyp)) {
    gn_enc_2_error(xbuf, "bad_type_flag");
    ei_x_encode_atom(xbuf, type_name);
    return FALSE;
  }
  fclass = g_type_class_ref(gtyp);
  while ( list_len-- ) {
    if ( ei_decode_atom(B, I, flag_name) ) {
      gn_enc_2_error(xbuf, "bad_flag_atom");
      ei_x_encode_atom(xbuf, type_name);
      g_type_class_unref(fclass);
      return FALSE;
    }
    fval = g_flags_get_value_by_name(fclass, flag_name);
    *flags = *flags | fval->value;
  }
  g_type_class_unref(fclass);
  return TRUE;
}

/* if get_arg_list returns TRUE, *list must be freed */
gboolean gn_get_arg_list(ei_x_buff *XBUF, char *B, int *I,
                         gchar* type_name, void** list) {
  GType gtyp;
  GType* tlist;
  char tname[MAXATOMLEN+1];
  int list_len, i;

  if ( strcmp(type_name,"GType") != 0 ) { /* only GType* yet */
    gn_enc_1_error(XBUF, "bad_list_type");
    return FALSE;
  }

  if ( (list_len = gn_get_list(XBUF,B,I)) < 0 ) return FALSE;

  tlist = g_new(GType, list_len);

  for (i = 0; i < list_len; i++) {
    if ( ! gn_get_arg_gtype(XBUF, B, I, &gtyp )) {
      g_free(tlist);
      return FALSE;
    }
    tlist[i] = gtyp;
  }
  *list = (void*)tlist;
  return TRUE;
}

static gboolean gn_get_arg_gtype(ei_x_buff *XBUF, char *B, int *I, GType *gt){
  gchar type_str[MAXATOMLEN+1];

  if ( ! gn_get_arg_gchar_fix(XBUF, B, I, type_str) ) return FALSE;
  if ( strcmp(type_str,"string") == 0 ){
    *gt = G_TYPE_STRING;
  } else if(strcmp(type_str,"integer") == 0) {
    *gt = G_TYPE_INT64;
  } else if(strcmp(type_str,"float") == 0) {
    *gt = G_TYPE_DOUBLE;
  } else {
    gn_enc_2_error(XBUF, "not_value_type");
    ei_x_encode_atom(XBUF, type_str);
    return FALSE;
  }
  return TRUE;
}

gboolean gn_get_arg_object(ei_x_buff *xbuf, char *B, int *I,
                           GType type, GObject** go) {

  char object_name[MAXATOMLEN+1];

  if ( ei_decode_atom(B, I, object_name) ) {
    gn_enc_1_error(xbuf, "bad_object_atom");
    return FALSE;
  }
  if ( strcmp(object_name,"NULL") == 0 ) {
    *go = NULL;
  }else if ( ! gn_check_object(xbuf, object_name, type, go) ) {
    return FALSE;
  }
  return TRUE;
}


gboolean gn_get_arg_struct(ei_x_buff *xbuf, char *B, int *I,
                           char* type, void** pp) {

  char str_name[MAXATOMLEN+1];

  if ( ei_decode_atom(B, I, str_name) ) {
    gn_enc_1_error(xbuf, "bad_struct_atom");
    return FALSE;
  }
  if ( strcmp(str_name,"NULL") == 0 ) {
    *pp = NULL;
  }else if ( ! gn_check_struct(xbuf, str_name, type, pp) ) {
    return FALSE;
  }
  return TRUE;
}

gboolean gn_check_object(ei_x_buff *xbuf, gchar* object_name,
                         GType type, GObject** object) {
  void* vp;
  GType GT;

  if ( (vp = hash_lookup(object_name)) ) {
    /* dynamic object */
  }else if ( (vp = gn_check_widget_name(object_name))) {
    /* named widget */
  }else{
    gn_enc_2_error(xbuf, "bad_object");
    ei_x_encode_atom(xbuf, object_name);
    return FALSE;
  }

  *object = G_OBJECT(vp);
  if ( G_TYPE_CHECK_INSTANCE_TYPE ((*object), type) ) return TRUE;

  GT = G_TYPE_FROM_INSTANCE(*object);
  gn_enc_2_error(xbuf, "bad_type");
  ei_x_encode_atom(xbuf, g_type_name(GT));
  return FALSE;
}

gboolean gn_check_struct(ei_x_buff *xbuf,
                         char* struct_name, char* struct_type,void** pp) {
  extern GModule *gmod;
  gchar con_str[200] = "gn_construct_";
  gchar key[200];
  void* (*constr)(void);

  memcpy(key,struct_type,strlen(struct_type)+1);
  strcat(key,"-");
  strcat(key,struct_name);
  if ( (*pp = hash_lookup(key)) ) {
    return TRUE;
  }else{
    strcat(con_str,struct_type);
    g_module_symbol(gmod, con_str, (gpointer *)&constr);
    if ( ! constr ) {
      gn_enc_2_error(xbuf, "bad_constructor");
      ei_x_encode_atom(xbuf, con_str);
      return FALSE;
    }
    *pp = (*constr)();
    hash_insert(key,*pp);
    return TRUE;
  }
}

gboolean gn_check_arity(ei_x_buff *xbuf, int a1, int a2) {
  if ( a1 == a2 ) return TRUE;

  gn_enc_2_error(xbuf, "bad_arity");
  ei_x_encode_long(xbuf, (long) a2);
  return FALSE;
}

/***************************************************************************/
gint gn_get_list(ei_x_buff *xbuf, char *B, int *I) {
  int ari;
  if ( ! ei_decode_list_header(B, I, &ari) ) return ari;
  gn_enc_1_error(xbuf, "bad_list");
  return -1;
}

gint gn_get_tuple(ei_x_buff *xbuf, char *B, int *I) {
  int ari;
  if ( ! ei_decode_tuple_header(B, I, &ari) ) return ari;
  gn_enc_1_error(xbuf, "bad_tuple");
  return -1;
}

/***************************************************************************/

static GEnumClass* get_enum_class(const gchar *type_name) {
  GType gtyp;

  if ( ! (gtyp = gn_GType_from_name(type_name)))
    return NULL;
  if ( ! G_TYPE_IS_ENUM(gtyp))
    return NULL;

  return g_type_class_ref(gtyp);
}

gboolean gn_get_enum_val(ei_x_buff *xbuf,
                         const gchar *type_name, gchar *enum_name, gint *i) {
  GEnumClass* eclass;
  GEnumValue* eval;

  if ( ! (eclass = get_enum_class(type_name)) ) {
    gn_enc_2_error(xbuf, "bad_enum_type");
    ei_x_encode_atom(xbuf, type_name);
    return FALSE;
  }

  eval = g_enum_get_value_by_name(eclass, enum_name);
  g_type_class_unref(eclass);
  if ( ! eval ) {
    gn_enc_2_error(xbuf, "bad_enum_name");
    ei_x_encode_atom(xbuf, enum_name);
    return FALSE;
  }
  *i = eval->value;
  return TRUE;
}

gboolean gn_get_enum_name(const gchar *type_name, gint i, gchar *enum_name) {
  GEnumClass* eclass;
  GEnumValue *eval;

  if ( ! (eclass = get_enum_class(type_name)) )
    return FALSE;

  eval = g_enum_get_value(eclass, i);
  g_type_class_unref(eclass);

  if ( ! eval )
    return FALSE;

  memcpy(enum_name, eval->value_name, strlen(eval->value_name)+1);
  return TRUE;
}

/***************************************************************************/
/*  {{Pid,signal},{Widget,EvType}}  */
/***************************************************************************/
void gn_send_signal(const char *widgetname, const char *evtyp) {
  ei_x_buff xbuf;

  ei_x_new_with_version(&xbuf);
  gn_wrap_ans("signal", &xbuf);
  ei_x_encode_tuple_header(&xbuf,2);
  ei_x_encode_atom(&xbuf, widgetname);
  ei_x_encode_atom(&xbuf, evtyp);
  gn_send(&xbuf);
  ei_x_free(&xbuf);
}
/***************************************************************************/
