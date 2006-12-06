%%%-------------------------------------------------------------------
%%% File    : hello.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 27 Nov 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(hello).

-export([start/0]).

-define(G(C,A),gtknode:cmd(hello,C,A)).

start() ->
    gtknode:start(hello),
    Win = ?G('Gtk_window_new',['GTK_WINDOW_TOPLEVEL']), 
    But = ?G('Gtk_button_new_with_label',["butt"]), 
    ?G('Gtk_container_add',[Win,But]), 
    ?G('Gtk_widget_show',[Win]), 
    ?G('Gtk_widget_show',[But]), 
    ?G('GN_signal_connect',[But,clicked]),
    ?G('GN_signal_connect',[Win,destroy]),
    loop(Win,But).

loop(Win,But) ->
    receive 
        {hello,{signal,{But,clicked}}} -> 
            io:fwrite("~p~n",[clicked]),
            loop(Win,But);
        {hello,{signal,{Win,destroy}}}  -> 
            gtknode:stop(hello)
    end.
