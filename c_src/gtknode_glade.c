#include "gtknode.h"

gboolean gn_glade_init(char *filename) {
  const char *rootnode = NULL;
  extern GladeXML *xml;
  
  if ( ! (xml = glade_xml_new(filename, rootnode, NULL)) ) 
    return FALSE;
  glade_xml_signal_autoconnect(xml);
  return TRUE;
}

void erl(GtkWidget *widget){
  const char *widgetname;
  
  GSignalInvocationHint *ihint = g_signal_get_invocation_hint(widget);
  const gchar *signal_name = g_signal_name(ihint->signal_id);
  if ( ! (widgetname = glade_get_widget_name(widget)) )
    widgetname = "UNKNOWN";
  gn_send_signal(widgetname, signal_name);
}

GtkWidget* gn_check_widget_name(char* widget_name) {
  extern GladeXML *xml;
  return glade_xml_get_widget(xml, widget_name);
}
