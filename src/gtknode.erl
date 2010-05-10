%%%-------------------------------------------------------------------
%%% File    : gtk_node.erl
%%% Author  : Mats Cronqvist <qthmacr@mwux005>
%%% Description : 
%%%
%%% Created : 22 Nov 2004 by Mats Cronqvist <qthmacr@mwux005>
%%%-------------------------------------------------------------------
-module(gtknode).
-export([start/1,stop/1]).
-export([debug/0,debug/1,debug/2]).
-export([cmd/2,cmd/3]).

%%-export([recv/0]).
%%-export([glade/1,widget_get_attr/1,new_gvalue/2]).

-import(filename, [join/1,dirname/1,basename/1]).

-record(st,{gtk_port=[],client_pid=[],handler_pid=[],gtk_pid=[],name=[]}).

-define(BORED, 5000).
-define(LOG(T), log(process_info(self()),T)).

%%%-------------------------------------------------------------------
%%% runs in client process
%%%-------------------------------------------------------------------
start(Name) -> 
  case whereis(Name) of
    undefined ->    
      Self = self(),
      Pid = spawn_link(fun() -> init(Self, Name) end),
      receive 
	started -> Pid;
	quit -> ok
      end;
    _ -> 
      {already_started,Name}
  end.

stop(Pid) when is_pid(Pid) -> 
  case is_process_alive(Pid) of
    true -> Pid ! quit;
    false -> {not_running,Pid}
  end;
stop(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> {not_running,Name};
    Pid -> stop(Pid)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% for debugging, run gtknode:debug and start gtknode from a shell thusly;
%%% gtknode <erlnodename> <host> gtknode_dbg <cookie> nod <erl_dist_version>
%%% E.g. bin/gtknode foo mwlx084 gtknode_dbg cki nod 11
%%% send messages to the gtknode with gtknode:debug/2, thusly;
%%% gtknode:debug('GN_glade_init',["gladefile.glade"]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug(Cmd,Args) -> debug([{Cmd,Args}]).
debug(CmdArgs) ->
  catch debug(),
  gtknode_dbgH ! {cmd, CmdArgs},
  ok.
debug() ->
  case whereis(gtknode_dbg) of
    undefined -> spawn(fun initDBG/0);
    _ -> erlang:error({already_started,gtknode_dbg})
  end.

initDBG() ->
  process_flag(trap_exit,true), 
  register(gtknode_dbg, self()),
  Handler = spawn_link(fun initDBGH/0),
  waiting_handshake(#st{handler_pid=Handler, name=gtknode_dbg}).    

initDBGH() ->
  register(gtknode_dbgH, self()),
  loopDBGH().

loopDBGH() ->
  receive 
    {gtknode_dbg, {signal, Sig}} ->
      ?LOG({signal,Sig}),loopDBGH();
    {gtknode_dbg, {reply, Rep}} ->
      ?LOG({reply,Rep}),loopDBGH();
    {cmd,quit} ->
      gtknode_dbg ! quit;
    {cmd, Cmd} ->
      gtknode_dbg ! {self(),Cmd},loopDBGH()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cmd(GUI,C,As) -> cmd(GUI,[{C,As}]).

cmd(_GUI,[]) -> 
  {'EXIT',{foobar,St}} = (catch erlang:error(foobar)), ?LOG({empty,St});
cmd(GUI,CAs) ->
  GUI ! {self(),CAs},
  receive 
    {GUI,{reply,Reps}} -> filter_reps(Reps,CAs)
  end.

filter_reps([{ok,Rep}],[_]) -> Rep;
filter_reps([{ok,_}|Reps],[_|CAs]) -> filter_reps(Reps,CAs);
filter_reps([{error,R}|_],[CA|_]) -> exit({gtknode_error,{R,CA}}).

%%%-------------------------------------------------------------------
%%% implements the gtkNode middleman process
%%% links to the gtkNode and to the signal handler
%%% the signal handler is the process from which the start function is called
%%%
%%% has a message API
%%% receives from the client;
%%%   {ClientPid, list({atom(Command),list(Args)})}
%%% receives from the port;
%%%   {port(Port),PortData}
%%% receives from the gtkNode;
%%%    {GtkPid, {atom(handshake|signal|reply|error),Data}
%%% from anywhere;
%%%    atom(quit) | EXIT signals
%%%-------------------------------------------------------------------
init(Client, Name) ->
  process_flag(trap_exit,true), 
  register(Name, self()),
  Client ! started,
  Port = start_gtknode(Name),
  waiting_handshake(#st{handler_pid=Client, name=Name, gtk_port=Port}).

start_gtknode(Name) ->
  open_port({spawn,make_cmd(Name)},[stderr_to_stdout,exit_status]).

make_cmd(Name) ->
  ErlDistributionVsn="13",
  [Node,Host] = string:tokens(atom_to_list(node()),"@"),
  RegName = atom_to_list(Name),
  Cookie = atom_to_list(erlang:get_cookie()),
  NN = atom_to_list(?MODULE)++"_"++atom_to_list(Name),
  string:join([exe(),Node,Host,RegName,Cookie,NN,ErlDistributionVsn]," ").

exe() ->
  take_first(fun exe/1,
             [S || S <- [os:getenv("GTKNODE_BIN")], is_list(S)] ++
             [join([my_path(),priv,bin,gtknode]),
              join([my_path(),c_src,gtknode])]).

exe(S) ->
  case os:find_executable(basename(S),dirname(S)) of
    false -> exit(nah);
    Exe -> Exe
  end.

my_path() ->
  dirname(dirname(code:which(?MODULE))).

take_first(_,[]) -> exit({take_first,nothing});
take_first(Fun,[H|T]) ->
  try Fun(H)
  catch _:_ -> take_first(Fun,T)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the states; waiting_handshake, idle, waiting_reply
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting_handshake(St = #st{gtk_port=Port}) ->
  receive
    {{GtkPid,handshake}, []} -> 
      link(GtkPid),
      idle(St#st{gtk_pid=GtkPid});
    {Port,PortData} ->			%from the port
      waiting_handshake(handle_portdata(St, PortData));
    {'EXIT',Port,Reason} ->                 %port died, us too
      die({port_died,Reason});
    quit -> 
      die(quitting)
  after 
    ?BORED -> waiting_handshake(bored(waiting_handshake,St))
  end.

idle(St = #st{gtk_pid=GtkPid, gtk_port=Port, handler_pid=HandPid}) ->
  receive
    {{GtkPid, signal}, Sig} ->
      %%from gtknode
      HandPid ! {St#st.name, {signal, Sig}},
      idle(St);
    {Pid,CmdArgs} when pid(Pid) ->
      %%from client
      GtkPid ! CmdArgs,
      waiting(St#st{client_pid = Pid},CmdArgs,[]);
    {Port,PortData} ->
      %%from the port
      idle(handle_portdata(St, PortData));
    {'EXIT',HandPid,Reason} ->
      %%handler died
      die({handler_died,Reason});
    {'EXIT',Port,Reason} ->
      %%port died, us too
      die({port_died,Reason});
    quit -> 
      die(quitting)
  end.

waiting(St = #st{gtk_pid=GtkPid, gtk_port=Port},CmdArgs,OldReps) ->
  receive
    {{GtkPid,reply}, Ans}->			%from gtknode
      case length(Reps=OldReps++Ans)-length(CmdArgs) of
	0 ->
	  St#st.client_pid ! {St#st.name, {reply,Reps}},
	  idle(St#st{client_pid = []});
	_ ->
	  waiting(St,CmdArgs,Reps)
      end;
    {Port,{data,PortData}} ->		%from the port
      waiting(handle_portdata(St, PortData),CmdArgs,OldReps);
    {'EXIT',Port,Reason} ->                 %port died, us too
      die({port_died,{Reason,CmdArgs}});
    quit -> 
      die(quitting)
  after 
    ?BORED -> waiting(bored(waiting,St),CmdArgs,OldReps)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_portdata(St, PortData) ->
  case PortData of
    {data, Str} -> ?LOG({portdata, Str});
    _ -> ?LOG({portdata,PortData})
  end,
  St.

bored(State,St) ->
  ?LOG([{bored,State}, {state,St}, {msgs,process_info(self(),messages)}]),
  St.

die(quitting) -> ok;
die(Reason) ->
  process_flag(trap_exit,false),     
  exit({dying,Reason}).

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
  error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).
