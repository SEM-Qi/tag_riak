-module(tag_riak_serv).
-behaviour(gen_server).
-compile(export_all).
% -export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%          code_change/3, terminate/2]).


%Where Pid is the pid of the requesting process (hopefully)
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, Pid} = riakc_pb_socket:start_link("greedo.skip.chalmers.se", 8087),
  {ok, Pid}.

%DO Call/Cast Handling here

handle_call(update_taglist, _From, SocketPid) ->
	Taglist = jiffy:encode([<<"theifs">>,<<"thekilling">>,<<"them">>,
     <<"themasterstoryteller">>,<<"themselves">>,
     <<"thenextgreatbaker">>,<<"theprofit">>,<<"therapydisco">>,
     <<"theseStreets">>,<<"theshatteredmind">>,
     <<"theshinsekai">>,<<"thesimplethingstour">>,<<"thesis">>,
     <<"thestrain">>,<<"thesuitelife">>,<<"thewaylifegoes">>,
     <<"thework">>,<<"theythinktheyre21">>,<<"theywack">>,
     <<"thingsToDoBeforeIDie">>,<<"thingstodobeforeidie">>,
     <<"thisisasadmoment">>,<<"thisisntover">>,<<"porn">>]),
	% {ok, Taglist} = riakc_pb_socket:list_buckets(Pid),
	{reply, Taglist, SocketPid};

handle_call({gettag, Tag}, _From, SocketPid) ->
  {Numtags1, Cotags1, TweetText1} = getTagBySec(1, Tag, SocketPid),
  {Numtags2, Cotags2, TweetText2} = getTagBySec(2, Tag, SocketPid),
  {Numtags3, Cotags3, TweetText3} = getTagBySec(3, Tag, SocketPid),
  {Numtags4, Cotags4, TweetText4} = getTagBySec(4, Tag, SocketPid),
  {Numtags5, Cotags5, TweetText5} = getTagBySec(5, Tag, SocketPid),
  {Numtags6, Cotags6, TweetText6} = getTagBySec(6, Tag, SocketPid),
  {Numtags7, Cotags7, TweetText7} = getTagBySec(7, Tag, SocketPid),
  {Numtags8, Cotags8, TweetText8} = getTagBySec(8, Tag, SocketPid),
  {Numtags9, Cotags9, TweetText9} = getTagBySec(9, Tag, SocketPid),
  {Numtags10, Cotags10, TweetText10} = getTagBySec(10, Tag, SocketPid),
  AllCotags = Cotags10 ++ Cotags1 ++ Cotags2 ++ 
              Cotags3 ++ Cotags4 ++ Cotags5 ++ 
              Cotags6 ++ Cotags7 ++ Cotags8 ++ Cotags9,

Response = jiffy:encode({[{<<"tag">>, Tag},
  {<<"cotags">>, AllCotags},
  {<<"distribution">>, 
    [{[{<<"numtags">>, Numtags1}, {<<"tweets">>, TweetText1}]},
        {[{<<"numtags">>, Numtags2}, {<<"tweets">>, TweetText2}]},
        {[{<<"numtags">>, Numtags3}, {<<"tweets">>, TweetText3}]},
        {[{<<"numtags">>, Numtags4}, {<<"tweets">>, TweetText4}]},
        {[{<<"numtags">>, Numtags5}, {<<"tweets">>, TweetText5}]},
        {[{<<"numtags">>, Numtags6}, {<<"tweets">>, TweetText6}]},
        {[{<<"numtags">>, Numtags7}, {<<"tweets">>, TweetText7}]},
        {[{<<"numtags">>, Numtags8}, {<<"tweets">>, TweetText8}]},
        {[{<<"numtags">>, Numtags9}, {<<"tweets">>, TweetText9}]},
        {[{<<"numtags">>, Numtags10}, {<<"tweets">>, TweetText10}]}]}]}),

{reply, Response, SocketPid}.

% {[{foo, bar}]}

% {[{<<"tag">>, <<"tag">>},
%   {<<"cotags">>, [<<"cotags">>, <<"cotags">>]},
%   {<<"distribution">>, 
%     [{[{<<"numtags">>, 1}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 2}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 3}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 4}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 5}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 6}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 7}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 8}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 9}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]},
%         {[{<<"numtags">>, 10}, {<<"tweets">>, [<<"tweets1">>, <<"tweets2">>]}]}]}]},
% {
%     "tag": "string",
%     "cotags":["string"],
%     "distribution": [
%         {
%             "numtags":"integer",
%             "tweets":["string"]
%         }]
% }

% {ok, List} = riakc_pb_socket:list_keys(SocketPid, Tag),
% NumTag = length(List),
% Result = jiffy:encode({[{Tag, NumTag}]}),
% % {ok, Taglist} = riakc_pb_socket:list_buckets(Pid),
% {reply, Result, SocketPid}.

handle_cast(terminate, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%Call - Start a server under tag_riak_sup supervision

testoldTimeStamp(Second) ->
  1414506936992434 - ((Second * 120) * 1000000).
oldTimeStamp(Second) ->
  {Mega, Secs, Micro} = erlang:now(),
  Mega*1000*1000*1000*1000 + ((Secs - (Second *120)) * 1000 * 1000) + Micro.

getTagBySec(Second, Tag, SocketPid) ->
  {ok, {_,Keys,_,_}} = riakc_pb_socket:get_index_range(
          SocketPid,
          Tag, %% bucket name
          {integer_index, "timestamp"}, %% index name
          testoldTimeStamp(Second), testoldTimeStamp(Second-1) %% origin timestamp should eventually have some logic attached
        ),

  TagObjs = lists:map(fun(Key) -> {ok, Obj} = riakc_pb_socket:get(SocketPid, Tag, Key), Obj end, Keys),
  Allofthecotags = lists:foldl(fun(Object, AllCotags) -> Binary = riakc_obj:get_value(Object), 
                    {Cotags, _} = binary_to_term(Binary), Cotags ++ AllCotags end, [], TagObjs).
  {length(Keys), Allofthecotags, [<<"dummy tweet text">>]}.
