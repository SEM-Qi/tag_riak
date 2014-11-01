-module(tag_riak_sup).
-compile(export_all).
% -export([start_link/3, init/1]).
-behaviour(supervisor).


start_link() ->
  supervisor:start_link({local, tag_riak_sup},?MODULE, []).


init([]) ->
  MaxRestart = 5,
  MaxTime = 3000,
  {ok, {{simple_one_for_one, MaxRestart, MaxTime},
          [{tag_riak_serv,
            {tag_riak_serv,start_link,[]},
            transient, 5000, worker, [tag_riak_serv]}]}}.




%%% Need to make a redundancy plan. When super goes down, started with the info from refserver? or just force to reconnect?