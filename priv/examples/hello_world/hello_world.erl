%%%-------------------------------------------------------------------
%%% File    : hello_world.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : gtknode "hello world" program
%%%
%%% Created : 21 Nov 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(hello_world).
-export([start/0,stop/0]).
-import(filename,[dirname/1,join/1]).

start() -> 
    case whereis(?MODULE) of
	undefined -> spawn(fun init/0);
	_ -> already_started
    end.

stop() -> ?MODULE ! quit.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    gtknode:start(?MODULE),
    GladeFile = join([dirname(code:which(?MODULE)),?MODULE])++".glade",
    g('GN_glade_init',[GladeFile]),
    loop().

loop() ->
    receive
	{?MODULE,{signal,{button_ok,clicked}}}     -> change_text(),loop();
        {?MODULE,{signal,{button_quit,'clicked'}}} -> quit();
	{?MODULE,{signal,{window,'delete-event'}}} -> quit();
	quit                                       -> quit();
	X -> io:fwrite("got ~p~n",[X]),loop()
    end.

change_text() ->
    g('Gtk_label_set_text',[label,new_text()]).

new_text() ->
    case g('Gtk_label_get_text',[label]) of
        "HELLO WORLD" -> "hello world";
        _ -> "HELLO WORLD"
    end.

quit() -> gtknode:stop(?MODULE).

g(C,As) -> g([{C,As}]).
g(CAs) ->  gtknode:cmd(?MODULE,CAs).
