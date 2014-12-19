%% ------------------------------------------------------------------
%% tag_riak_top is an OTP supervisor that sits at the top of the supervisor tree. 
%% It starts all permanent processes in its init stage and supervises themn. 
%% These processes are registered and should never die (i.e. will be restarted). 
%% ------------------------------------------------------------------

-module(tag_riak_top).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local, tag_riak_top}, ?MODULE, []).

init([]) ->
  MaxRestart = 6,
  MaxTime = 3000,
  {ok, {{one_for_one, MaxRestart, MaxTime},
          [{tag_riak_refserv,
             {tag_riak_refserv, start_link, []},
             permanent,
             5000, 
             worker,
             [tag_riak_refserv]},
            {tag_riak_sup,
             {tag_riak_sup, start_link, []},
             permanent,
             5000, 
             supervisor,
             [tag_riak_sup]}]}}.