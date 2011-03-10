%%%-------------------------------------------------------------------
%%% File    : top.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created :  9 Aug 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(top).

-export([start/0,stop/0]).

-import(filename,[join/1,dirname/1]).

-record(st, {statusbar_ctxt,treeview}).
-record(treeview,{store,cols=[]}).
-record(col,{title,attr,data_col,type}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    case whereis(?MODULE) of
        undefined -> spawn(fun init/0);
        _ -> already_started
    end.

stop() -> ?MODULE ! quit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    %% start the c-node and it's port handler
    gtknode:start(?MODULE),

    %% load the glade file into the c-node
    GladeFile = join([dirname(code:which(?MODULE)),?MODULE])++".glade",
    ssnd([],'GN_glade_init',[GladeFile]),

    loop(init_gui()).

init_gui() ->
    treeview_init(state_init(#st{})).

state_init(St) ->
    %% init the status bar
    Id = ssnd(statusbar1,'Gtk_statusbar_get_context_id',["state"]),
    ssnd(statusbar1,'Gtk_statusbar_push',[Id,"connected"]),
    state_disc(St#st{statusbar_ctxt = Id}).

treeview_init(St) ->
    %% the tree view columns
    Cols = [#col{title="Proc",attr="text",data_col=0,type=string},
            #col{title="Size",attr="text",data_col=1,type=integer},
            #col{title="Msgq",attr="text",data_col=2,type=integer},
            #col{title="Reds",attr="text",data_col=3,type=integer}],
    lists:foreach(fun(C) -> treeview_column(C) end, Cols),

    %% create the model (a list_store)
    LS = ssnd([],'Gtk_list_store_newv',[length(Cols),[C#col.type||C<-Cols]]),

    %% associate the model with the view
    ssnd(treeview1,'Gtk_tree_view_set_model',[LS]),

    St#st{treeview=#treeview{cols = Cols,
                             store = LS}}.

treeview_column(#col{title=Title,attr=Attr,data_col=Col}) ->
    %% create a tree view column
    TreeViewCol = ssnd([],'Gtk_tree_view_column_new',[]),
    TextRend = ssnd([],'Gtk_cell_renderer_text_new',[]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_pack_start',[TextRend,false]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_set_title',[Title]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_add_attribute',[TextRend,Attr,Col]),
    ssnd(TreeViewCol,'Gtk_tree_view_column_set_resizable',[true]),
    ssnd(treeview1,'Gtk_tree_view_append_column',[TreeViewCol]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(St) ->
    receive
        %% user deleted top window
        {?MODULE,{signal,{window1,_}}}    -> quit();
        %% user selected quit menu item
        {?MODULE,{signal,{quit1,_}}}      -> quit();
        %% user selected  connect menu item
        {?MODULE,{signal,{connect,_}}}    -> loop(conn(St));
        %% user selected  disconnect menu item
        {?MODULE,{signal,{disconnect,_}}} -> loop(disc(St));
        %% user selected about menu item
        {?MODULE,{signal,{about1,_}}}     -> loop(show_about(St));
        %% user clicked ok in about dialog
        {?MODULE,{signal,{dialogb,_}}}    -> loop(hide_about(St));
        %% user deleted about dialog
        {?MODULE,{signal,{dialog1,_}}}    -> loop(hide_about(St));
        %% we got data from the top_top process
        {data,Data}                       -> loop(update(St,Data));
        %% quit from the erlang shell
        quit                              -> quit();
        %% log other signals
        X                                 -> io:fwrite("got ~p~n",[X]),loop(St)
    end.

quit() -> gtknode:stop(?MODULE).
conn(St) -> do_connect(),state_conn(St).
disc(St) -> do_disconnect(),state_disc(St).
hide_about(St) -> ssnd(dialog1,'Gtk_widget_hide',[]),St.
show_about(St) -> ssnd(dialog1,'Gtk_widget_show',[]),St.
update(St,Data) ->
    ssnd(treeview1,'Gtk_widget_freeze_child_notify',[]),
    clear(St#st.treeview),
    populate(St#st.treeview,Data),
    ssnd(treeview1,'Gtk_widget_thaw_child_notify',[]),
    St.
state_disc(St) ->
    ssnd(statusbar1,'Gtk_statusbar_push',[St#st.statusbar_ctxt,"disconnected"]),
    ssnd(connect,'Gtk_widget_set_sensitive',[true]),
    ssnd(disconnect,'Gtk_widget_set_sensitive',[false]),
    St.
state_conn(St) ->
    ssnd(statusbar1,'Gtk_statusbar_pop',[St#st.statusbar_ctxt]),
    ssnd(connect,'Gtk_widget_set_sensitive',[false]),
    ssnd(disconnect,'Gtk_widget_set_sensitive',[true]),
    St.

do_connect() -> top_top:assert(self()).
do_disconnect() -> top_top:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear(#treeview{store=LS}) ->
    ssnd(LS,'Gtk_list_store_clear',[]).

populate(_TV,[]) -> ok;
populate(TV=#treeview{store=LS,cols=Cols},[RowData|Data]) ->
    ssnd(LS,'Gtk_list_store_append',[gtkTreeIter]),
    populate_list_row(LS,Cols,RowData),
    populate(TV,Data).

populate_list_row(_LS,[],[]) -> ok;
populate_list_row(LS,[Col|Cols],[Data|Datas]) ->
    ssnd(gval,'GN_value_set',[Data]),
    ssnd(LS,'Gtk_list_store_set_value',[gtkTreeIter,Col#col.data_col,gval]),
    populate_list_row(LS,Cols,Datas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ssnd([],Cmd,Args) -> snd(Cmd,Args);
ssnd(Wid,Cmd,Args) -> snd(Cmd,[Wid|Args]).

snd(Cmd, Args) ->
    ?MODULE ! {self(),[{Cmd,Args}]},
    receive
        {?MODULE,{reply,[{ok,Rep}]}} -> Rep;
        {?MODULE,{reply,[{error,Rep}]}} -> erlang:error({Cmd,Args,Rep})
    end.
