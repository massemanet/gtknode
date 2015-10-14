%%%-------------------------------------------------------------------
%%% File    : generate.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description : generates C code that gtkNode_cnode can call.
%%%               each function marshals args and calls one gtk function.
%%%               reads the .defs files from h2def.py
%%%
%%% Created : 27 Oct 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(generator).

-export([go/1]).
-import(filename,[dirname/1,basename/1]).
-import(lists,[foreach/2,foldl/3,reverse/1,sort/1]).

-record(func, {black=no,cname,pref,ret,const=no,paras=[]}).
-record(type, {what,cname,gtype,etype,cast}).

go([Root, Vsn, StructsFile | Files]) ->
  ets_new(types),
  ets_new(funcs),
  ets_new(bogus, [{keypos, 1}]),
  get_doc_links(Root),
  do_types_loop(Files), % populate the type table
  lists(),
  structs(StructsFile),
  basic_types(),
  R = generate_wrappers_loop(Vsn, Files),
  io:fwrite("~w ~p~n~n", [?MODULE, R]).

doc_header(FD)->
  io:fwrite(FD,"<html><head>"
            "<title>Functions wrapped by gtknode</title><body>"
            "<h2>Functions wrapped by gtknode</h2>",[]).
doc_footer(FD)->
  io:fwrite(FD,"</body></html>",[]).

get_doc_links("no")->
  %% don't put links in the html files
  ok;
get_doc_links("yes")->
  %% don't put links in the html files
  io:fwrite("configure flag --enable-gtkdoclinks takes an argument",[]);
get_doc_links(Root)->
  %% put in links to some place like these;
  %% "/usr/share/doc/libgtk2.0-doc",
  %% "http://developer.gnome.org/doc/API/2.0"
  F = fun(D) -> put_links(D,Root) end,
  try foreach(F, ["gtk","glib","gdk","gobject","gdk-pixbuf"])
  catch _:R -> io:fwrite("doc_links failed:~p~n",[R])
  end.

put_links(D,Root) ->
  Rt = string_join([Root,D,"ix01.html"],"/"),
  doc_links(Rt,get_ix(Rt)).

get_ix("http://"++_ = Doc) -> get_ix_http(Doc);
get_ix("file:/"++Doc) -> get_ix_file(Doc);
get_ix("/"++_ = Doc) -> get_ix_file(Doc).

get_ix_file(Doc) ->
  {ok,Bin} = file:read_file(Doc),
  binary_to_list(Bin).

get_ix_http(Doc) ->
  case os:getenv("http_proxy") of
    false -> ok;
    Proxy ->
      %% [http://][username:password@]proxyhost:port[/]
      case string:tokens(Proxy,"/")  of
        ["http:",Prx] -> ok;
        [Prx] -> ok
      end,
      case string:tokens(Prx,"@") of
        [_,Px] -> throw(proxy_authentication_not_supported);
        [Px] -> ok
      end,
      [H,P]  = string:tokens(Px,":"),
      httpc:set_options([{proxy,{{H,list_to_integer(P)},[]}}])
  end,
  ReqPid = spawn(fun() -> exit(httpc:request(Doc)) end),
  ReqMon = erlang:monitor(process,ReqPid),
  receive
    {'DOWN',ReqMon,_,_,{ok,{_,_,S}}} -> S;
    {'DOWN',ReqMon,_,_,I} -> throw({bad_http_reply,I})
  after
    9999 -> erlang:demonitor(ReqMon), exit(ReqPid,kill), throw(http_timeout)
  end.

doc_links(Root,S)->
  RE = "href=\"[A-Za-z-]+.html#id[0-9]+\">[a-z_]+ \\(\\)</a>",
  {match,Ms} = re:run(S,RE),
  io:fwrite("got ~p links for ~s~n",[length(Ms),basename(dirname(Root))]),
  foreach(fun({St,Le})-> do_doc_link(Root,string:substr(S,St,Le)) end, Ms).

do_doc_link(URL,S) ->
  case string:tokens(S,"=>< ,\"") of
    ["href",Href,"g"++Func,"()","/a"] ->
      put("G"++Func,"<a href=\""++dirname(URL)++"/"++Href++"\">");
    _ ->
      ok
  end.

do_types_loop([]) ->
  ok;
do_types_loop([_Prefix, FuncDefs, _FuncDefsWhiteList, _FuncDefsBlackList,
               _FuncWrappersCode, _WrappedFuncsList,
               _ErrorFuncsList, _ErrorTypesList| Tail]) ->
  do_types(FuncDefs),
  do_types_loop(Tail).

generate_wrappers_loop(Vsn, Files) ->
  gw_loop(Vsn, Files, []).

gw_loop(_Vsn, [], Results) -> Results;
gw_loop(Vsn, [Prefix, FuncDefs,
              FuncDefsWhiteList,FuncDefsBlackList,
              FuncWrappersCode, WrappedFuncsList,
              ErrorFuncsList,ErrorTypesList | Tail], Results) ->
  Res = generate_wrappers(Vsn, Prefix, FuncDefs,
                          FuncDefsWhiteList, FuncDefsBlackList,
                          FuncWrappersCode, WrappedFuncsList, ErrorFuncsList,
                          ErrorTypesList),
  gw_loop(Vsn, Tail, [Res | Results]).

generate_wrappers(Vsn, Prefix, _FuncDefs,
                  FuncDefsWhiteList, FuncDefsBlackList,
                  FuncWrappersCode, WrappedFuncsList,
                  ErrorFuncsList, ErrorTypesList) ->
  do_funcs(FuncDefsWhiteList), %populate the funcs table
  do_black(FuncDefsBlackList), %de-populate the funcs table
  {ok,FDc} = file:open(FuncWrappersCode, [write]),
  {ok,FDok} = file:open(WrappedFuncsList, [write]),
  {ok,FDcrap} = file:open(ErrorFuncsList, [write]),
  {ok,FDtypes} = file:open(ErrorTypesList, [write]),
  doc_header(FDok),
  vsn(Vsn, [FDc, FDok, FDcrap, FDtypes]),
  FRWAs = ets:match(funcs, {func, no, '$1', Prefix, '$2', '$3', '$4'}),
  foreach(fun(FRWA) -> gen_one(FDc, FDcrap, FDok, Prefix, FRWA) end, FRWAs),
  log_types(FDtypes),
  doc_footer(FDok),
  {Prefix, length(FRWAs), ets:lookup(bogus, good), ets:lookup(bogus, bad)}.

vsn(_, []) -> ok;
vsn(Vsn, [FD | FDs]) ->
  io:fwrite(FD, "/* GTK version: ~s */~n", [Vsn]),
  vsn(Vsn, FDs).

gen_one(FDc, FDcrap, FDok, Prefix, [Func,Ret,Const,Args]) ->
  unstick(),
  emit_head(Prefix, Func),
  emit_argdefs(Args),
  emit_retdef(Ret,Const),
  emit_ari_chk(Args),
  emit_arg_chk(Args),
  emit_call(Func, Ret, Args),
  emit_free(Args),
  emit_return(Ret),
  emit_final(),
  unstick(FDc,FDcrap,FDok).

emit_head(Prefix, Func) ->
  estick({UFunc = upcase_1st(Func), rem_pref(Func, Prefix)}),
  stick("/*******************************/~n"
        "gboolean ~s(int ARI, ei_x_buff *XBUF, char *B, int *I){~n~n", [UFunc]).

emit_argdefs([]) -> stick("~n",[]);
emit_argdefs([{Typ,Name,_Doc}|Args]) ->
  stick("  ~s ~s;~n", [Typ,Name]),
  emit_argdefs(Args).

emit_retdef(Ret,Const) ->
  case what_group(Ret) of
    object -> stick("  GObject* R;~n~n",[]);
    struct -> stick("  ~s R;~n~n", [Ret]);
    none -> stick("  /* no return value */~n~n",[]);
    crap -> stick("  ~s R;~n~n", ["CRAP_"++Ret]);
    list ->
      mark_as_crap("list_"++Ret),
      stick("  ~s R;~n~n", ["CRAP_list_"++Ret]);
    _ ->
      case Const of
        yes -> stick("  const ~s R; /* return value */~n~n", [Ret]);
        no -> stick("  ~s R; /* return value */~n~n", [Ret])
      end
  end.

emit_ari_chk(Args) ->
  stick("  if ( ! gn_check_arity(XBUF, ~p, ARI) )"
        " return FALSE;~n", [length(Args)]).

emit_arg_chk([]) -> ok;
emit_arg_chk([{Typ,Name,_Doc}|Args]) ->
  estick(Name),
  case Kind = what_group(Typ) of
    crap ->
      stick("  if ( ! gn_get_arg_CRAP(XBUF, B, I, ~p, &~s) )"
            " return FALSE;~n",
            [Typ,Name]);
    list ->
      stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
            " return FALSE;~n",
            [Kind,unstar(Typ),"(void**)",Name]);
    struct ->
      stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
            " return FALSE;~n",
            [Kind,unstar(Typ),"(void**)",Name]);
    basic ->
      stick("  if ( ! gn_get_arg_~s(XBUF, B, I, &~s) )"
            " return FALSE;~n",
            [unstar(Typ),Name]);
    flags ->
      stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
            " return FALSE;~n",
            [Kind,unstar(Typ),"(gint*)",Name]);
    enum ->
      stick("  if ( ! gn_get_arg_~s(XBUF, B, I, ~p, ~s&~s) )"
            " return FALSE;~n",
            [Kind,unstar(Typ),"(gint*)",Name]);
    object ->
      stick("  if ( ! gn_get_arg_~w(XBUF, B, I, ~s, ~s&~s) )"
            " return FALSE;~n",
            [Kind,what_gtype(unstar(Typ)),"(GObject**)",Name])
  end,
  emit_arg_chk(Args).

emit_call(Func, Ret, Args) ->
  case what_group(Ret) of
    none -> stick("  ~s(",[Func]);
    object -> stick("  R = (GObject*)~s(",[Func]);
    list -> stick("  R = (~s)~s(", ["CRAP_"++Ret,Func]);
    crap -> stick("  R = (~s)~s(", ["CRAP_"++Ret,Func]);
    _ -> stick("  R = ~s(", [Func])
  end,
  emit_call_arg(Args).

emit_call_arg([{_,Name,_}|As]) -> stick("~s", [Name]),emit_call_args(As);
emit_call_arg(A) -> emit_call_args(A).

emit_call_args([]) -> stick(");~n",[]);
emit_call_args([{_,Name,_}|Args]) ->
  stick(", ~s", [Name]),
  emit_call_args(Args).

emit_free([]) -> ok;
emit_free([{Type,Name,_}|Args]) ->
  case {Type,what_group(Type)} of
    {_,list} -> stick("  g_free(~s);~n", [Name]);
    {"gchar*",_}-> stick("  free(~s);~n", [Name]);
    {"char*",_}-> stick("  free(~s);~n", [Name]);
    _ -> ok
  end,
  emit_free(Args).

emit_return(Ret) ->
  case what_group(Ret) of
    basic ->
      {ET,Cast} = what_etype(Ret),
      stick("  gn_put_~w(XBUF,(~s)R);~n", [ET,Cast]);
    none -> stick("  gn_put_void(XBUF);~n", []);
    object -> stick("  gn_put_object(XBUF,R);~n", []);
    struct -> stick("  gn_put_struct(XBUF,~p,(void*)R);~n", [unstar(Ret)]);
    crap -> stick("  gn_put_CRAP(XBUF,~p,R);~n", [Ret]);
    Kind -> stick("  gn_put_~w(XBUF,~p,R);~n", [Kind, Ret])
  end.

emit_final() ->
  stick("  return TRUE;~n}~n",[]).

what_gtype(This) ->
  [#type{what=object,gtype=Type}] = ets:lookup(types,This),
  Type.

what_etype(Basic) ->
  [#type{what=basic,etype=Type,cast=Cast}] = ets:lookup(types,Basic),
  {Type,Cast}.

what_group([]) -> none;
what_group("none") -> none;
what_group(Typ) ->
  case {ets:lookup(types,Typ),ets:lookup(types,unstar(Typ))} of
    {[#type{what = object}],_} -> object;
    {[],[#type{what = object}]} -> object;
    {[#type{what = basic}],_} -> basic;
    {[#type{what = enum}],_} -> enum;
    {[#type{what = flags}],_} -> flags;
    {[#type{what = list}],_} -> list;
    {_,[#type{what = struct}]} -> struct;
    _ -> mark_as_crap(Typ)
  end.

mark_as_crap(Typ) ->
  put(crap,crap),
  ets_upd(bogus,{type,Typ}),
  crap.

estick(X) ->
  case get(efunc) of
    undefined -> put(efunc, {X,[]});
    {F,As} -> put(efunc,{F,[X|As]})
  end.

stick(Form, Args) ->
  S = io_lib:fwrite(Form, Args),
  case get(cfunc) of
    undefined -> put(cfunc, S);
    SS -> put(cfunc, SS++S)
  end.

unstick() -> erase(cfunc), erase(efunc), erase(crap).
unstick(FD1,FD2,FDok) ->
  case get(crap) of
    crap -> ets_upd(bogus,bad),cunstick(FD2);
    _ -> ets_upd(bogus,good),cunstick(FD1), eunstick(FDok)
  end,
  unstick().

cunstick(FD) -> io:fwrite(FD, "~s", [lists:flatten(get(cfunc))]).

eunstick(FD) ->
  {{Cname,_Ename},As} = get(efunc),
  case get(Cname) of
    undefined -> io:fwrite(FD, "<br>~s(",[Cname]);
    Link -> io:fwrite(FD, "<br>~s~s</a>(",[Link,Cname])
  end,
  eunstick_args(FD,As),
  io:fwrite(FD, ")~n",[]).

eunstick_args(_FD,[]) -> ok;
eunstick_args(FD,As) ->
  [A1|AT] = reverse(As),
  io:fwrite(FD,"~s",[upcase_1st(A1)]),
  foreach(fun(A)->io:fwrite(FD,",~s",[upcase_1st(A)]) end, AT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_black(FN) ->
  bio:string(FN, fun do_black/2, nil).

do_black(Str,_State) ->
  case ets:lookup(funcs,Str) of
    [] -> ok;
    [Rec] -> ets:insert(funcs, Rec#func{black=yes})
  end.

do_funcs(FN) ->
  io:fwrite("~p~n", [FN]),
  bio:string(FN, fun do_funcl/2, nil).

do_funcl("",State) ->
  State;
do_funcl(";"++_,State) ->
  State;
do_funcl(")", bogus) ->
  nil;
do_funcl(")", {_,Data}) ->
  ets:insert(funcs,Data),
  nil;
do_funcl("(define-function "++_, nil) ->
  {func,#func{}};
do_funcl("(define-method "++_, nil) ->
  {meth,#func{}};
do_funcl("  (parameters", {Flag,Func}) ->
  {para, {Flag, Func}};
do_funcl(_, bogus) ->
  bogus;
do_funcl("  (varargs #t)", _) ->
  bogus;
do_funcl("  )", {para, {Flag, Func}}) ->
  {Flag, Func};
%%do_funcl("   )", {para, {Flag, Func}}) ->
%%    {Flag, Func};
do_funcl(Str, {para, {Flag, Func}}) ->
  OP = Func#func.paras,
  case string:tokens(Str,"'()\" ") of
    ["const-"++Para, Name|_Doc] ->
      {para, {Flag, Func#func{paras=OP++[{Para,Name,const}]}}};
    [Para, Name|_Doc] ->
      {para, {Flag, Func#func{paras=OP++[{Para,Name,no}]}}}
  end;
do_funcl("  (c-name \""++C, {Flag,Func}) ->
  {Flag,Func#func{cname=trnc(2,C), pref=pref(C)}};
do_funcl("  (return-type \""++C, {Flag,Func}) ->
  case trnc(2,C) of
    "const-"++R -> {Flag,Func#func{ret=R,const=yes}};
    R -> {Flag,Func#func{ret=R}}
  end;
do_funcl("  (of-object \""++C, {meth,Func}) ->
%%%    {meth,Func#func{this=trnc(2,C)}};
  {meth,Func#func{paras=[{trnc(2,C)++"*","object",no}]}};
do_funcl(_,State) ->
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lists() ->
  Lists = ["GType*"],
  foreach(fun ins_list/1, Lists).

ins_list(L) -> ets:insert(types,#type{what=list,cname=L,cast=""}).

structs(StructsFile) ->
  Structs = bio:string(StructsFile, fun do_structs/2, []),
  foreach(fun ins_struct/1, Structs).
do_structs(Str, Acc) ->
  case re:run(Str,"gn_construct_.*\\(") of
    nomatch -> Acc;
    {match,[{St,Le}]} -> [string:substr(Str,St+14,Le-14)|Acc]
  end.
ins_struct(S) -> ets:insert(types,#type{what=struct,cname=S,cast=""}).

basic_types() ->
  Basic = [{boolean, "int", ["gboolean","boolean"]},
           {string, "char*", ["gchar*","char*"]},
           {double, "double", ["gdouble","gfloat"]},
           {longlong, "long long", ["gint64","gint","int","size_t"]},
           {ulonglong, "unsigned long long",
            ["guint","guint8","guint16","guint32"]},
           {void,none,["void"]}],
  foreach(fun ins_basic/1, Basic).
ins_basic({ET,Cast,Bs}) ->
  foreach(fun(B) -> ins_basic(B,ET,Cast) end, Bs).
ins_basic(B,ET,Cast) ->
  ets:insert(types,#type{what=basic,cname=B,etype=ET,cast=Cast}).

do_types(FN) ->
  io:fwrite("~p~n", [FN]),
  bio:string(FN, fun do_typel/2, nil).

do_typel("",State) ->
  State;
do_typel(";"++_,State) ->
  State;
do_typel(")", {_,Data}) ->
  ets:insert(types,Data),
  nil;
do_typel("(define-boxed "++_, nil) ->
  {define_boxed,#type{what=boxed}};
do_typel("(define-enum "++_, nil) ->
  {define_enum,#type{what=enum}};
do_typel("(define-flags "++_, nil) ->
  {define_flags,#type{what=flags}};
do_typel("(define-object "++_, nil) ->
  {define_object,#type{what=object}};
do_typel("(define-pointer "++_, nil) ->
  {define_pointer,#type{what=pointer}};
do_typel("  (c-name \""++C, {Flag,Type}) ->
  {Flag,Type#type{cname=trnc(2,C)}};
do_typel("  (gtype-id \""++C, {Flag,Type}) ->
  {Flag,Type#type{gtype=trnc(2,C),cast=cast(trnc(2,C))}};
do_typel(_,State) ->
  State.

cast(C) ->
  [Prefix,"TYPE"|Toks] = string:tokens(C,"_"),
  string_join([Prefix|Toks], "_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
upcase_1st(Str) -> [hd(Str)+$A-$a|tl(Str)].

rem_pref([X|Func], [X|Prefix]) -> rem_pref(Func, Prefix);
rem_pref([$_|Func], []) -> Func.

pref(C) -> hd(string:tokens(C,"_")).

unstar(Str) ->
  case reverse(Str) of
    "*"++X -> reverse(X);
    _ -> Str
  end.

trnc(N,C) -> lists:sublist(C, length(C)-N).

string_join([Prefix|Toks], Sep) ->
  foldl(fun(Tok,O) -> O++Sep++Tok end, Prefix, Toks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_types(FD) ->
  F = fun(E) -> io:fwrite(FD, "~4w - ~s~n", E) end,
  foreach(F,reverse(sort(ets:match(bogus,{{type,'$2'},'$1'})))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_new(Tab) ->
  catch ets:delete(Tab),
  ets_new(Tab, [{keypos,3}]).
ets_new(Tab, Opt) ->
  catch ets:delete(Tab),
  ets:new(Tab,[named_table,ordered_set]++Opt).
ets_upd(Tab, Key) ->
  case catch ets:update_counter(Tab, Key, 1) of
    {'EXIT',_} -> ets:insert(Tab, {Key,1});
    _ -> ok
  end.
