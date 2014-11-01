-module(tag_riak_top).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%Start tag_war_top as tag_riak:startlink().
%

%% Could also make this name global (this level only, in a multi node situation maybe?
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
             {tag_riak_sup, start_link, []},  % A = Get list of connections from refServer
             permanent,
             5000, 
             supervisor,
             [tag_riak_sup]}]}}.