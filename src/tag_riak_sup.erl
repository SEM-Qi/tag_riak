%% ------------------------------------------------------------------
%% tag_riak_sup is an OTP supervisor. It starts the worker tag_riak_serv dynamically as requested. 
%% It supervises each server and will terminate a child by pid reference when requested
%% ------------------------------------------------------------------

-module(tag_riak_sup).
-export([start_link/0, init/1]).
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

