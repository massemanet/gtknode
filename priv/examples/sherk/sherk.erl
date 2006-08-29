%%%-------------------------------------------------------------------
%%% File    : sherk.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 14 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk).

-export([go/0]).
-export([log/3]).

-import(filename,[dirname/1,join/1]).
-import(lists,[foreach/2]).
-record(ld,{perf_list,call_list,perf_selection}).


go() -> spawn_link(fun init/0).

init() ->
    gtknode:start(sherk),
    f('GN_glade_init',[join([dirname(code:which(sherk)),"sherk.glade"])]),
    check_file(),
    CallList = init_list_store([string,integer,integer]),
    init_tree_view(call_treeview,CallList,[{0,"MFA"},{1,"calls"},{2,"%"}]),
    PerfList = init_list_store([string,string,integer,integer]),
    init_tree_view(perf_treeview,PerfList,[{1,"tag"},{2,"us"},{3,"%"}]),
    loop(#ld{call_list=CallList,
	     perf_list=PerfList,
	     perf_selection=f('Gtk_tree_view_get_selection',[perf_treeview])}).

loop(LD) ->
    receive
	%% user bored
	quit -> ok;
	{sherk,{signal,{quit,_}}} -> ok;
	{sherk,{signal,{perf_window,_}}} -> ok;
	{sherk,{signal,{call_window,_}}} -> hide(call_window),loop(LD);
	
	%% user curious
	{sherk,{signal,{about,_}}} -> show(about),loop(LD);
	{sherk,{signal,{about_ok,_}}} -> hide(about),loop(LD);

	%% user changed directory
	{sherk,{signal,{filechooserbutton,_}}} -> check_file(),loop(LD);
	
	%% let's go
	{sherk,{signal,{go_button,_}}} -> perf(LD), loop(LD);

	%% inspect process
	{sherk,{signal,{perf_treeview,_}}} -> call(LD),loop(LD);

	%% user doing some wierd stuff
	X -> io:fwrite("erlang proc received ~p~n",[X]),loop(LD)
    end.

check_file() ->
    try 
	Dir = f('Gtk_file_chooser_get_filename',[filechooserbutton]),
	sherk_tab:check_file(Dir),
	f('Gtk_widget_set_sensitive',[go_button,true])
    catch 
	_:_ -> f('Gtk_widget_set_sensitive',[go_button,false])
    end.

perf(#ld{perf_list=Store}) ->
    f('Gtk_widget_set_sensitive',[go_button,false]),
    File = f('Gtk_file_chooser_get_filename',[filechooserbutton]),
    sherk_tab:assert(File),
    List = sherk_list:go(perf),
    update_treeview(perf_treeview,Store,List),
    f('Gtk_widget_set_sensitive',[go_button,true]).

call(#ld{perf_list=Model, perf_selection=Sel, call_list=Store}) ->
    show(call_window),
    Pid = get_selected_data(Sel,Model,0),
    List = sherk_list:go({call,Pid}),
    update_treeview(call_treeview,Store,List).

update_treeview(View,Store,List) ->
    hide(View),
    list_clear(Store),
    list_insert(Store, List),
    show(View).

get_selected_data(Sel,Model,Col) ->
    f('GN_tree_selection_get_selected',[Sel,iter]),
    f('GN_value_unset',[val]),
    f('Gtk_tree_model_get_value',[Model,iter,Col,val]),
    f('GN_value_get',[val]).

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
	{sherk,{reply,[{error,Rep}]}} -> {error,C,Rep}; 
	X -> exit({what,X}) 
    end.

log(ProcInfo,_Sev,Term) ->
    error_logger:info_report([Term|[{in,CF}||{current_function,CF}<-ProcInfo]]).
