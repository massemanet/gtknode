%%%-------------------------------------------------------------------
%%% File    : sherk.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 14 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk).

-export([go/0]).
-export([log/2]).
-export([loop/1]).

-import(filename,[dirname/1,join/1]).
-import(lists,[foreach/2,member/2]).

-record(ld,{targets,monitor}).

-define(LOG(T), sherk:log(process_info(self()),T)).
-define(LOOP(X), ?MODULE:loop(X)).

go() -> spawn_link(fun init/0).

init() ->
    %% list of all known nodes
    Targets = sherk_aquire:known_nodes(),
    foreach(fun(N)->erlang:monitor_node(N,true) end, Targets),

    %% start the GUI and load the glade file
    gtknode:start(sherk),
    f('GN_glade_init',[join([dirname(code:which(sherk)),"sherk.glade"])]),

    %% check the trc source
    check_file(),
    %% set default trc destination
    f('Gtk_file_chooser_set_current_folder',[aq_filechoose,"/tmp"]),

    %% init nodes treeview
    NodeList = init_list_store([string]),
    init_tree_view(aq_treeview,NodeList,[{0,"Nodes"}]),
    update_treeview(aq_treeview,[[atom_to_list(T)]||T<-Targets]),
    set_selection_mode(aq_treeview,'GTK_SELECTION_MULTIPLE'),

    %% init calls treeview
    CallList = init_list_store([string,integer,integer]),
    init_tree_view(call_treeview,CallList,[{0,"MFA"},{1,"calls"},{2,"%"}]),

    %% init procs treeview
    PerfList = init_list_store([string,string,integer,integer]),
    init_tree_view(perf_treeview,PerfList,[{1,"tag"},{2,"us"},{3,"%"}]),


    loop(#ld{targets=Targets}).

loop(LD) ->
    Monitor = LD#ld.monitor,
    receive
	%% user bored
	quit                                 -> ok;
	{sherk,{signal,{quit,_}}}            -> ok;
	{sherk,{signal,{main_window,_}}}     -> ok;

	%% user wants stuff hidden
	{sherk,{signal,{call_window,_}}}     -> hide(call_window),?LOOP(LD);
	{sherk,{signal,{aq_window,_}}}       -> hide_aq_window(),?LOOP(LD);
	{sherk,{signal,{show_aquire,_}}}     -> toggle_aq_window(),?LOOP(LD);

	%% user curious
	{sherk,{signal,{about,_}}}           -> show(about_window),?LOOP(LD);
	{sherk,{signal,{about_window,_}}}    -> hide(about_window),?LOOP(LD);
	{sherk,{signal,{about_ok,_}}}        -> hide(about_window),?LOOP(LD);

	%% user changed directory
	{sherk,{signal,{main_filechoose,_}}} -> check_file(),?LOOP(LD);
        
        %% user is trying to aquire data
	{sherk,{signal,{aq_filechoose,_}}}   -> aq_check(),?LOOP(LD);
        {sherk,{signal,{aq_treeview,_}}}     -> aq_check(),?LOOP(LD);
        {sherk,{signal,{aq_time_entry,_}}}   -> aq_check(),?LOOP(LD);
	{sherk,{signal,{aq_go_button,_}}}    -> ?LOOP(aq_go(LD));
	{sherk,{signal,{aq_stop_button,_}}}  -> ?LOOP(aq_stop(LD));

	%% let's go
	{sherk,{signal,{go_button,_}}}       -> perf(LD),?LOOP(LD);

	%% inspect process
	{sherk,{signal,{perf_treeview,_}}}   -> call(LD),?LOOP(LD);

        %% a target node went away
        {nodedown, Node}                     -> ?LOOP(update_targets(LD,Node));

        %% aquire proc crashed
        {'DOWN',Monitor,_,_,Info}            -> ?LOOP(do_aq_stop(LD,Info));
        
	%% user doing some wierd stuff
	X                                    -> ?LOG([{received,X}]),?LOOP(LD)
    end.

check_file() ->
    try 
	Dir = f('Gtk_file_chooser_get_filename',[main_filechoose]),
	sherk_tab:check_file(Dir),
	f('Gtk_widget_set_sensitive',[go_button,true])
    catch 
	_:_ -> f('Gtk_widget_set_sensitive',[go_button,false])
    end.

aq_go(LD) -> 
    f('Gtk_widget_set_sensitive',[aq_radiobutton_call,false]),
    f('Gtk_widget_set_sensitive',[aq_radiobutton_proc,false]),
    f('Gtk_widget_set_sensitive',[aq_filechoose,false]),
    f('Gtk_widget_set_sensitive',[aq_time_entry,false]),
    f('Gtk_widget_set_sensitive',[aq_treeview,false]),
    f('Gtk_widget_set_sensitive',[aq_go_button,false]),
    f('Gtk_widget_set_sensitive',[aq_stop_button,true]),
    Time = aq_get_time(),
    Flags = aq_get_flags(),
    RTPs = aq_get_rtps(Flags),
    Procs = all,
    Targs = aq_get_nodes(),
    Cookie = erlang:get_cookie(),
    Dest = {file,aq_get_dest(),0,"/tmp"},
    ?LOG([{time,Time},
          {flags,Flags},
          {rTPs,RTPs},
          {procs,Procs},
          {targs,Targs},
          {cookie,Cookie},
          {dest,Dest}]),
    P = sherk_aquire:go(Time,Flags,RTPs,Procs,Targs,Cookie,Dest),
    LD#ld{monitor=erlang:monitor(process,P)}.

aq_stop(LD) -> 
    sherk_aquire:stop(),
    do_aq_stop(LD,aq_stop_wait(LD#ld.monitor)).

aq_stop_wait(Monitor) ->
    receive
        {sherk_aquire, done} -> erlang:demonitor(Monitor),done;
        {'DOWN',Monitor,_Type,_Object,Info} -> Info
    end.

do_aq_stop(LD,Reason) -> 
    ?LOG([{aqure_finshed,Reason}]),
    f('Gtk_widget_set_sensitive',[aq_radiobutton_call,true]),
    f('Gtk_widget_set_sensitive',[aq_radiobutton_proc,true]),
    f('Gtk_widget_set_sensitive',[aq_filechoose,true]),
    f('Gtk_widget_set_sensitive',[aq_time_entry,true]),
    f('Gtk_widget_set_sensitive',[aq_treeview,true]),
    f('Gtk_widget_set_sensitive',[aq_stop_button,false]),    
    aq_check(),
    LD#ld{monitor=undefined}.

aq_check() ->
    try
        Dir = f('Gtk_file_chooser_get_filename',[aq_filechoose]),
        sherk_aquire:check_dir(Dir),
        [_|_] = get_selected_data(aq_treeview,0),
        list_to_integer(f('Gtk_entry_get_text',[aq_time_entry])),
    	f('Gtk_widget_set_sensitive',[aq_go_button,true])
    catch 
	_:_ -> f('Gtk_widget_set_sensitive',[aq_go_button,false])
    end.

aq_get_time() ->
    1000*list_to_integer(f('Gtk_entry_get_text',[aq_time_entry])).

aq_get_flags() ->
    case f('Gtk_toggle_button_get_active',[aq_radiobutton_proc]) of
        true -> proc_flags();
        false -> call_flags()
    end.

aq_get_rtps(Flags) ->
    case member(call,Flags) of
        true -> [{'_','_'}];
        false -> []
    end.

aq_get_nodes() ->
    [list_to_atom(N) || N<-get_selected_data(aq_treeview,0)].

aq_get_dest() ->
    f('Gtk_file_chooser_get_filename',[aq_filechoose]).

update_targets(LD, BadNode) ->
    Ts = LD#ld.targets--[BadNode],
    update_treeview(aq_treeview,[[atom_to_list(T)]||T<-Ts]),
    LD#ld{targets=Ts}.

proc_flags() ->
    ['procs','running','garbage_collection',
     'timestamp','cpu_timestamp','set_on_spawn'].

call_flags() ->
    ['call','return_to','arity'|proc_flags()].

perf(_) ->
    f('Gtk_widget_set_sensitive',[go_button,false]),
    File = f('Gtk_file_chooser_get_filename',[main_filechoose]),
    sherk_tab:assert(File),
    List = sherk_list:go(perf),
    update_treeview(perf_treeview,List),
    f('Gtk_widget_set_sensitive',[go_button,true]).

call(_) ->
    show(call_window),
    [Pid] = get_selected_data(perf_treeview,0),
    List = sherk_list:go({call,Pid}),
    update_treeview(call_treeview,List).

update_treeview(View,List) ->
    Model = f('Gtk_tree_view_get_model',[View]),
    hide(View),
    list_clear(Model),
    list_insert(Model, List),
    show(View).

set_selection_mode(View,Mode) ->
    Sel = f('Gtk_tree_view_get_selection',[View]),
    f('Gtk_tree_selection_set_mode',[Sel,Mode]).

get_selected_data(View,Col) ->
    Model = f('Gtk_tree_view_get_model',[View]),
    get_data(Model,Col,f('GN_tree_view_get_selected',[View])).

get_data(_Model,_Col,[]) -> [];
get_data(Model,Col,[Path|Paths]) ->
    f('Gtk_tree_model_get_iter_from_string',[Model,daddy,Path]),
    f('GN_value_unset',[val]),
    f('Gtk_tree_model_get_value',[Model,daddy,Col,val]),
    [f('GN_value_get',[val])|get_data(Model,Col,Paths)].

show_aq_window() ->
    show(aq_window),
    f('Gtk_check_menu_item_set_active',[show_aquire,true]).
hide_aq_window() ->
    hide(aq_window),
    f('Gtk_check_menu_item_set_active',[show_aquire,false]).
toggle_aq_window() ->
    case f('Gtk_check_menu_item_get_active',[show_aquire]) of
	true -> show_aq_window();
	false -> hide_aq_window()
    end.

show(W) -> f('Gtk_widget_show',[W]).
hide(W) -> f('Gtk_widget_hide',[W]).

list_clear(List) -> f('Gtk_list_store_clear', [List]).

init_list_store(Cols)->
    f('Gtk_list_store_newv',[length(Cols),Cols]).

list_insert(_,[]) -> ok;
list_insert(Store,[H|T]) -> 
    f('Gtk_list_store_append',[Store,iter]),
    list_insert_row(Store,H,0),
    list_insert(Store,T).

list_insert_row(_Store,[],_) -> ok;
list_insert_row(Store,[I|Is],N) ->
    f('GN_value_set',[val,I]),
    f('Gtk_list_store_set_value',[Store,iter,N,val]),
    list_insert_row(Store,Is,N+1).

init_tree_view(TreeView,Model,Cols) ->
    f('Gtk_tree_view_set_model',[TreeView,Model]),
    foreach(fun({DC,T})->init_tree_view_column(TreeView,DC,T) end,Cols).

init_tree_view_column(TreeView,DataCol,Title) ->
    Col = f('Gtk_tree_view_column_new',[]),
    Renderer = f('Gtk_cell_renderer_text_new',[]),
    f('Gtk_tree_view_column_pack_start',[Col,Renderer,false]),
    f('Gtk_tree_view_column_set_title', [Col,Title]),
    f('Gtk_tree_view_column_set_resizable',[Col,true]),
    f('Gtk_tree_view_column_add_attribute',[Col,Renderer,"text",DataCol]),
    f('Gtk_tree_view_append_column',[TreeView,Col]).

tree_insert(_Store,_Path,[]) -> ok;
tree_insert(Store,Path,[{Str,SubList}|T]) -> 
    tree_insert_str(Store,Path,Str),
    tree_insert(Store,[0|Path],SubList),
    tree_insert(Store,[hd(Path)+1|tl(Path)],T);
tree_insert(Store,Path,[Str|T]) ->
    tree_insert_str(Store,Path,Str),
    tree_insert(Store,[hd(Path)+1|tl(Path)],T).

tree_insert_str(Store,Path,Str) ->
    f('GN_value_set',[val,Str]),
    case Path of
	[I] -> 
	    f('Gtk_tree_store_insert',[Store,iter,'NULL',I]);
	[I|D] -> 
	    f('Gtk_tree_model_get_iter_from_string',[Store,daddy,pth(D)]),
	    f('Gtk_tree_store_insert',[Store,iter,daddy,I])
    end,
    f('Gtk_tree_store_set_value', [Store,iter,0,val]).

pth(P) -> pth(P,[]).
pth([I],O) -> [$0+I|O];
pth([H|T],O) -> pth(T,[$:,$0+H|O]).

f(C,As) ->
    sherk ! {self(),[{C,As}]},
    receive 
	{sherk,{reply,[{ok,Rep}]}} -> Rep; 
	{sherk,{reply,[{error,Rep}]}} -> {error,C,Rep}
    end.

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,Term) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++Term).
