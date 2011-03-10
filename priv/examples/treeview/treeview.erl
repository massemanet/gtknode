%%%-------------------------------------------------------------------
%%% File    : treeview.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : treeview demo
%%%
%%% Created : 21 Nov 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(treeview).
-export([start/0,stop/0]).
-export([list_insert/2,tree_insert/2]).

-import(filename,[dirname/1,join/1]).
-import(lists,[flatten/1,foldl/3]).
-record(ld,{col_list1,col_list2,col_tree1,col_tree2,tree_store,list_store}).

list_data() ->
    [[1,"one"],
     [2,"two"]].

tree_data() ->
    [{["one",1],
      [{["sub1",666],
        [{["sub2_1",667],
          []},
         {["sub2_2",668],
          []}]}]},
     {["two",2],[]}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    LD = #ld{col_list1=init_tree_view_column(0,"list_1"),
             col_list2=init_tree_view_column(1,"list_1"),
             col_tree1=init_tree_view_column(0,"tree_1"),
             col_tree2=init_tree_view_column(1,"tree_1"),
             tree_store=new_tree_store([string,integer]),
             list_store=new_list_store([integer,string])},
    loop(tree_view(LD)).

loop(LD) ->
    receive
        {?MODULE,{signal,{radiobutton_list,toggled}}}-> loop(tree_view(LD));
        {?MODULE,{signal,{button_insert,clicked}}}   -> loop(insert(LD));
        {?MODULE,{signal,{window,'delete-event'}}}   -> quit();
        {?MODULE,{signal,{button_quit,'clicked'}}}   -> quit();
        quit                                         -> quit();
        X -> io:fwrite("got ~p~n",[X]),loop(LD)
    end.

quit() -> gtknode:stop(?MODULE).

insert(LD) ->
    case g('Gtk_toggle_button_get_active',[radiobutton_list]) of
        true -> list_insert(LD#ld.list_store,list_data());
        false-> tree_insert(LD#ld.tree_store,tree_data())
    end,
    LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tree_view(LD) ->
    {Store,Cols} = get_store_data(LD),
    g('Gtk_tree_view_set_model',[treeview,Store]),
    remove_cols(),
    [g('Gtk_tree_view_append_column',[treeview,C]) || C <- Cols],
    LD.

remove_cols() ->
    case g('Gtk_tree_view_get_column',[treeview,0]) of
        'NULL' -> ok;
        Col -> g('Gtk_tree_view_remove_column',[treeview,Col]),remove_cols()
    end.

get_store_data(LD) ->
    case g('Gtk_toggle_button_get_active',[radiobutton_list]) of
        true -> {LD#ld.list_store,[LD#ld.col_list1,LD#ld.col_list2]};
        false-> {LD#ld.tree_store,[LD#ld.col_tree1,LD#ld.col_tree2]}
    end.

init_tree_view_column(DataCol,Title) ->
    Col = g('Gtk_tree_view_column_new',[]),
    Renderer = g('Gtk_cell_renderer_text_new',[]),
    g('Gtk_tree_view_column_pack_start',[Col,Renderer,false]),
    g('Gtk_tree_view_column_set_title', [Col,Title]),
    g('Gtk_tree_view_column_set_resizable',[Col,true]),
    g('Gtk_tree_view_column_add_attribute',[Col,Renderer,"text",DataCol]),
    Col.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_tree_store(Cols)->
    g('Gtk_tree_store_newv',[length(Cols),Cols]).

tree_insert(Store,Rows) ->
    g(flatten(tree_insert(Store,[0],Rows))).

tree_insert(_Store,_Path,[]) -> [];
tree_insert(Store,Path,[{Row,SubTree}|T]) ->
    [update_iter(Path, Store),
     tree_insert_row(Store,0,Row),
     tree_insert(Store,[0|Path],SubTree),
     tree_insert(Store,[hd(Path)+1|tl(Path)],T)].

tree_insert_row(_Store,_N,[]) -> [];
tree_insert_row(Store,N,[Col|Cols]) ->
    [tree_insert_val(Store,N,Col),
     tree_insert_row(Store,N+1,Cols)].

tree_insert_val(Store,N,Val) ->
    [{'GN_value_set',[val,Val]},
     {'Gtk_tree_store_set_value', [Store,iter,N,val]}].

update_iter([I], Store) ->
    {'Gtk_tree_store_insert',[Store,iter,'NULL',I]};
update_iter([I|D], Store) ->
    [{'Gtk_tree_model_get_iter_from_string',[Store,daddy,pth(D)]},
     {'Gtk_tree_store_insert',[Store,iter,daddy,I]}].

pth(P) -> pth(P,[]).
pth([I],O) -> [$0+I|O];
pth([H|T],O) -> pth(T,[$:,$0+H|O]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_list_store(Cols)->
    g('Gtk_list_store_newv',[length(Cols),Cols]).

list_insert(Store,Rows) ->
    Row_f = fun() -> {'Gtk_list_store_append',[Store,iter]} end,
    Col_f = fun(Row) -> foldl(fun col_f/2, {0,Store,[]}, Row) end,
    g(flatten([[Row_f()|element(3,Col_f(Row))] || Row<-Rows])).

col_f(Val,{N,Store,O}) ->
    {N+1,Store,
     [{'GN_value_set',[val,Val]},
      {'Gtk_list_store_set_value',[Store,iter,N,val]}|O]}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
g(C,As) -> g([{C,As}]).
g(CAs) -> gtknode:cmd(?MODULE,CAs).
