%%%----------------------------------------------------------------------
%%% File    : shrerk_scan.erl
%%% Author  : Mats Cronqvist <etxmacr@avc386>
%%% Purpose : 
%%% Created : 27 Feb 2006 by Mats Cronqvist <etxmacr@avc386>
%%%----------------------------------------------------------------------

-module(sherk_scan).
-author('etxmacr@avc386').

-include_lib("kernel/include/file.hrl").

-export([action/5]).
-export([write_msg/3]).

-import(lists,[member/2,reverse/1,keysearch/3,map/2]).

-define(LOG(S,T), sherk:log([process_info(self()),S,T])).

-record(state, {seq=0, hits=0, cbs, pattern, out, min, max, eof = false}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(FileName, Patt, CBs, Min, Max) ->
    panEts:new(?MODULE),
    {ok, FD} = file:open(FileName, [read, raw, binary]),
    State = #state{pattern=Patt,cbs=cbs(CBs),min=Min,max=Max},
    St = file_action(FD, State),
    file:close(FD),
    {hits, St#state.hits}.

-define(CHUNKSIZE, 1024).
file_action(FD, Stat) ->
    file_action(make_chunk(FD, <<>>), FD, Stat).

file_action(_, _FD, #state{eof = true} = Stat) -> 
    do(end_of_trace, Stat);
file_action(eof, _FD, Stat) -> 
    do(end_of_trace, Stat);
file_action({Term, Rest}, FD, Stat) ->
    file_action(make_chunk(FD, Rest), FD, do(Term, Stat)).
make_chunk(_FD, eof) -> eof;
make_chunk(FD, <<_Op, Size:32, Tal/binary>> = Bin) ->
    case Tal of
	<<Term:Size/binary, Tail/binary>> -> {binary_to_term(Term), Tail};
	_ -> make_chunk(FD, get_more_bytes(FD, Bin))
    end;
make_chunk(FD, Bin) -> make_chunk(FD, get_more_bytes(FD, Bin)).
get_more_bytes(FD, Rest) ->
    case file:read(FD, ?CHUNKSIZE) of
	{ok, Bin} -> <<Rest/binary, Bin/binary>>;
	_ -> eof
    end.

%%% CBs - CB|list(CB)
%%% CB - atom(M)|{atom(M),term(Init)}
cbs([]) -> [];
cbs([CB|T]) ->
    try [to_cb(CB)|cbs(T)]
    catch _:_ -> [{?MODULE,write_msg,[]}]
    end;
cbs(Term) -> cbs([Term]).

to_cb({Mod,Term}) -> is_cb(Mod), {Mod,go,Term};
to_cb(Mod) -> to_cb({Mod,initial}).

is_cb(M) -> true = member({go,3},M:module_info(exports)).

do(end_of_trace, State) -> 
    do_do(end_of_trace, State);
do(_Mess, #state{max = Max, seq = Seq} = Stat) when Max < Seq -> 
    Stat#state{eof=true};
do(Mess, #state{min = Min, seq = Seq} = Stat) when Seq < Min -> 
    case mass(Mess) of
	[] -> Stat;
	_ -> Stat#state{seq = Seq+1}
    end;
do(Mess, Stat) ->
    case mass(Mess) of
	[] -> Stat;
	Ms -> do_do(Ms, Stat)
    end.

do_do(end_of_trace = Ms, #state{cbs=CBs} = State) ->
    State#state{cbs=do_safe_cbs(CBs, Ms, State, [])};
do_do(Mess, #state{pattern=Patt, cbs=CBs, seq=Seq} = State) ->
    case grep(Patt, Mess) of
	false -> State#state{seq = Seq+1};
	true -> 
	    State#state{seq = Seq+1, 
			hits = State#state.hits+1, 
			cbs=do_safe_cbs(CBs, Mess, State, [])}
    end.

do_safe_cbs([], _Mess, _, O) -> reverse(O);
do_safe_cbs([{M, F, State}|CBs], Mess, #state{seq=Seq} = St, O) ->
    do_safe_cbs(CBs, Mess, St, [{M,F,safe_cb(M,F,Mess,Seq,State)}|O]).

safe_cb(M, F, Ms, Seq, Internal) ->
    M:F(Ms, Seq, Internal).

write_msg(Msg,Seq,_) ->
    io:fwrite("~.9.0w~w~~n",[Seq,Msg]).


grep('',_) -> true;
grep(P,T) when list(P) ->
    case grp(P, T) of
	[] -> true;
	_ -> false
    end;
grep(P, T) -> grep([P], T).

grp([], _) -> [];
grp(P, []) -> P;
grp(P, Fun) when function(Fun) -> grp(P, list_to_atom(erlang:fun_to_list(Fun)));
grp(P, Port) when port(Port) -> grp(P, list_to_atom(erlang:port_to_list(Port)));
grp(P, Rf) when reference(Rf) -> grp(P, list_to_atom(erlang:ref_to_list(Rf)));
grp(P, Pid) when pid(Pid) -> grp(P, list_to_atom(pid_to_list(Pid)));
grp(P, T) when tuple(T) -> 
    case lists:member(T,P) of
	true -> grp(P--[T], []);
	false -> grp(P,tuple_to_list(T))
    end;
grp(P, L) when list(L) -> 
    case lists:member(L, P) of
	true -> grp(P--[L], []);
	false -> grp(grp(P, hd(L)), tl(L))
    end;
grp(P, T) -> grp(P--[T], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% format is {Tag, PI, Data, Timestamp}
%%% PI is {pid(), Info} 
%%% Info is atom()|tuple()
%%% Timestamp is no_time|now()
%%% Data is;
%%%
%%% send,                         {PI2, Msg}
%%% send_to_non_existing_process, {PI2, Msg}
%%% 'receive',                    Message
%%% call,                         {M,F,A}
%%% return_to,                    {M,F,A}
%%% return_from,                  {{M,F,A}, ReturnValue}
%%% spawn,                        {PI2, {M,F,A}}
%%% exit,                         Reason
%%% link,                         PI2
%%% unlink,                       PI2
%%% getting_linked,               PI2
%%% getting_unlinked,             PI2
%%% register                      registered_name
%%% unregister                    registered_name
%%% in,                           {M,F,A}
%%% out,                          {M,F,A}
%%% gc_start,                     Info
%%% gc_end,                       Info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mass(end_of_trace = T) ->  T;
mass({I, _} = R) when I==tabi; I==traci; I==sysi -> ets_ins(R), [];
mass({porti, Port, Info}) -> handle_porti(Port, Info), [];
mass({proci, Pid, Info}) -> handle_proci(Pid, Info), [];
mass({trace, A, B, C}) -> mass(A, B, C, no_time);
mass({trace, A, B, C, D}) -> mass(A, B, {C, D}, no_time);
mass({trace_ts, A, B, C, TS}) -> mass(A ,B, C, TS);
mass({trace_ts, A, B, C, D, TS}) -> mass(A, B, {C, D}, TS);
mass(X) -> ?LOG(error, {unrec_msg, X}), [].

mass(Pid, T=send, X, TS) ->                       mass_send(Pid,T,X,TS);
mass(Pid, T=send_to_non_existing_process, X,TS) ->mass_send(Pid,T,X,TS);
mass(Pid, T='receive', Message, TS) ->              {T,pi(Pid),Message,TS};
mass(Pid, T=call, MFA, TS) ->                       {T,pi(Pid),MFA,TS};
mass(Pid, T=return_to, MFA, TS) ->                  {T,pi(Pid),MFA,TS};
mass(Pid, T=return_from, {MFA,R}, TS) ->            {T,pi(Pid),{MFA,R},TS};
mass(Pid, T=spawn, {P2, MFA}, TS) ->hps(Pid,P2,MFA),{T,pi(Pid),{pi(P2),MFA},TS};
mass(Pid, T=exit, Reason, TS) ->                    {T,pi(Pid),Reason,TS};
mass(Pid, T=link, Pd, TS) ->                        {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=unlink, Pd, TS) when pid(Pd) ->         {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=unlink, _Crap, TS) ->                   {T,pi(Pid),unknown,TS};
mass(Pid, T=getting_linked, Pd, TS) ->              {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=getting_unlinked, Pd, TS) ->            {T,pi(Pid),pi(Pd),TS};
mass(Pid, T=register, Name, TS) ->    hpr(Pid,Name),{T,pi(Pid),Name,TS};
mass(Pid, T=unregister, Name, TS) ->                {T,pi(Pid),Name,TS};
mass(Pid, T=in, MFA, TS) ->                         {T,pi(Pid),MFA,TS};
mass(Pid, T=out, MFA, TS) ->                        {T,pi(Pid),MFA,TS};
mass(Pid, T=gc_start, Info, TS) ->                  {T,pi(Pid),Info,TS};
mass(Pid, T=gc_end, Info, TS) ->                    {T,pi(Pid),Info,TS}.

mass_send(Pid, T, {Msg, To}, TS) when pid(To); port(To) -> 
    {T,pi(Pid),{pi(To), Msg},TS};
mass_send(Pid, T, {Msg, To}, TS) when atom(To) -> 
    {T,pi(Pid),{{find_pid(To),To}, Msg},TS};
mass_send(Pid, T, {Msg, {To,Node}}, TS) when atom(To), Node==node(Pid) -> 
    {T,pi(Pid),{{find_pid(To),To}, Msg},TS};
mass_send(Pid, T, {Msg, {To,Node}}, TS) when atom(To), atom(Node) -> 
    {T,pi(Pid),{{remote,{To,Node}}, Msg},TS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pi(file_driver) -> {trace,file_driver};
pi(Port) when port(Port) ->
    case ets_lup({Port, name}) of
	[] -> {Port, unknown};
	Name -> {Port, Name}
    end;
pi(Pid) when pid(Pid) ->
    case ets_lup({Pid, registered_name}) of
	[] -> 
	    case ets_lup({Pid, initial_call}) of
		[] -> {Pid, unknown};
		IC -> {Pid, IC}
	    end;
	Reg -> {Pid, Reg}
    end;
pi(X) ->
    ?LOG(error,{this_is_not_a_pid,X}),
    {X,X}.

find_pid(Reg) -> ets_lup({registered_name, Reg}).
	
handle_porti(Port, Info) ->
    {value,{name,Name}} = keysearch(name,1,Info),
    [Hd|_] = string:tokens(Name," "),
    Tl = reverse(hd(string:tokens(reverse(Hd),"/"))),
    ets_ins({{Port,name},list_to_atom(Tl)}).

handle_proci(Pid, [Reg, MFA, Mem]) ->
    ets_ins({{Pid, memory}, Mem}),
    ets_ins({{Pid, initial_call}, MFA}),
    case Reg of 
	[] -> ets_ins({{Pid, registered_name}, MFA});
	_ -> 
	    ets_ins({{Pid, registered_name}, Reg}),
	    ets_ins({{registered_name, Reg}, Pid}) %real registered name
    end.

hpr(Pid, Reg) ->
    ets_ins({{Pid, registered_name}, Reg}),
    ets_ins({{registered_name, Reg}, Pid}).	%real registered name

hps(ParentPid, Pid, {M, F, As}) ->
    ets_ins({{Pid, parent}, pi(ParentPid)}),
    ets_ins({{Pid, initial_call}, {M, F, length(As)}}),
    ets_ins({{Pid, registered_name}, {M, F, length(As)}}),
    case catch mangle_ic({M, F, As}) of
	ok -> ok;
	{'EXIT', _} -> ok;
	Reg -> ets_ins({{Pid, registered_name}, Reg})
    end.

mangle_ic(MFA) ->
    case MFA of
	{proc_lib,init_p,[_,_,M,F,A]} -> 
	    {proc_lib, trans_init(M,F,A)};
	{file,file,[_,FileName,_]} ->
	    {file, {atomize(FileName)}};
	{dets,do_open_file,[Tab,_FileName,_,_,_,_,_,_Ram,_,_,_]} ->
	    {dets, {Tab}};
	{application_master,start_it,[_,{state,_,ApplD,_,_,_},_,_]} ->
	    {appl_data,App,_,_,_,_,_,_,_} = ApplD,
	    {application_master, {App}};
	{erlang,apply,[Fun,[]]} when function(Fun) -> 
	    funi(Fun);
	_ -> 
	    panOpt:mangle_ic(MFA)
    end.

atomize(FileName) ->
    list_to_atom(hd(reverse(string:tokens(FileName, "/")))).

funi(Fun) ->
    case erlang:fun_info(Fun, module) of
	{_, rpc} ->
	    case erlang:fun_info(Fun, env) of
		{_, [_, _, Pid, A, _F, _M]} when pid(Pid), list(A) ->
		    {rpc, {call_dummy, node(Pid)}};
		{_, [_, Pid, A, F, M]} ->
		    {rpc, {call, node(Pid)}, {M, F, length(A)}};
		{_, [Pid, A, F, M]} when pid(Pid), list(A) ->
		    {rpc, {cast, node(Pid)}, {M, F, length(A)}};
		_X -> 
		    {rpc}
	    end;
	{_, Mod} -> 
	    case erlang:fun_info(Fun, pid) of
		{_, Pid} when pid(Pid) -> {'fun', {Mod, node(Pid)}};
		{_, X} -> {'fun', {Mod, X}}
	    end
    end.

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) ->
    {gen_server,Module};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) ->
    {gen_server,Module};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) ->
    {gen_fsm,Module};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
    {gen_fsm,Module};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event};
trans_init(M,F,A) ->
    {M,F,length(A)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ets_ins(Rec) -> ets_ins(?MODULE, Rec).
ets_ins(Tab, Rec) -> ets:insert(Tab, Rec).
ets_lup(Key) -> ets_lup(?MODULE, Key).
ets_lup(Tab, Key) ->
    case catch ets:lookup(Tab, Key) of
	{'EXIT', _} ->[];
	[{Key, R}] -> R;
	R -> R
    end.
