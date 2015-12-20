# gtknode

*Yet Another GUI framework for Erlang*

## DESIGN GOALS

* GUI separated from application by message passing
* distributed (application and GUI can run on different machines)
* centered around a GUI builder
* small volume of (hand-written) code
* pre-documented

##  ARCHITECTURE

a c-node (a daemon that implements the Erlang distribution protocol)
that instantiates the GUI from a configuration file. the c-node sends
messages to the Erlang node when the user interacts with the GUI; the
Erlang application changes the state of the GUI by sending messages to
widgets in the c-node. the widgets should look like Erlang processes
with registered names. the protocol should look something like this.

CnodePid ! {aWidget,dosomething,Args}       % erlang->cNode
ApplicationPid ! {reply, Val}               % cNode->Erlang
ApplicationPid ! {signal,aWidget,eventType} % cNode->erlang

in this example aWidget is the name given to a widget in the
configration file. it can also be thought of as the registered name of
the process implementing the widget in the c-node.
  the c-node is responsible for garbage-collecting all temporary data.

##  IMPLEMENTATION

###  REFERENCE

the c-node is started thus;

```
gtknode node host regname cookie cnode-name otp-version
```

when started, gtknode will connect to it's application by sending a
handshake message to ``{node@host, regname}``.

the messsage looks like this;

```erlang
{{GtkPid,handshake}, []}
```

the Erlang application sends messages to the gtknode using
``GtkPid``. messages look like this;

```erlang
list({'Gtk_function', [Args]})
```

E.g., if we have a ``GtkButton`` widget, named b1, and we want to use
these functions;

```c
const gchar* gtk_button_get_label (GtkButton *button);
void         gtk_button_set_label (GtkButton *button, const gchar *label);
```

we could send this;

```erlang
GtkPid ! [{'Gtk_button_set_label',[b1,"foo"]},{'Gtk_button_get_label',[b1]}].
```

and we would receive this;

```erlang
{{GtkPid,reply}, [{ok,void},{ok,"foo"}]}
```

signals are sent from gtknode if the signal handler for a specified
signal-widget combination is set to ``gn_sighandler``. the signals look
like this;

```erlang
{{GtkPid, signal}, {atom(WidgetName),atom(SignalName)}}
```

E.g., if we delete the GtkWindow named window1 we'll get this signal

```erlang
{{GtkPid, signal},{window1,'GDK_DELETE'}}
```

given that we've requested it, of course.

##  EXAMPLES

the file ``src/gtknode.erl`` implements a controller/middleman for the
gtknode, it's quite instructive. it is recommended to use this instead of
working directly against the c-node.

the file examples/simple/simple.erl implements the Erlang side of a
GUI for a simple 'top' application. the GUI is specified in
``examples/simple/simple.glade``.

##  GETTING IT

[http://github.com/massemanet/gtknode/](http://github.com/massemanet/gtknode/)

##  STATUS

stable since 2008

