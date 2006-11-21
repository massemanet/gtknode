%%%-------------------------------------------------------------------
%%% File    : treeview.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : treeview demo
%%%
%%% Created : 21 Nov 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(treeview).
-export([start/0,stop/0]).
-import(filename,[dirname/1,join/1]).
-record(ld,{}).

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
    loop(#ld{}).

loop(LD) ->
    receive
	{?MODULE,{signal,{button_quit,'clicked'}}}     ->quit();
	{?MODULE,{signal,{window,'delete-event'}}}     ->quit();
	quit                                           ->quit();
	X -> io:fwrite("got ~p~n",[X]),loop(LD)
    end.

quit() -> gtknode:stop(?MODULE).

g(C,As) -> g([{C,As}]).
g(CAs) ->
    case gtknode:cmd(?MODULE,CAs) of
        [{ok,Rep}] -> Rep;
	Reps -> 
            case [R || {error,R} <- Reps] of
                [] -> ok;
                Es -> throw({errors,Es})
            end
    end.
