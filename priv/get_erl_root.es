#!/usr/bin/env escript
%% -*- erlang -*-
main(_) ->
  io:fwrite("~s~n",[code:root_dir()]).
