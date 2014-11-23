PROJECT = tag_riak
DEPS = riak_pb riakc jiffy 
dep_riak_pb = git https://github.com/basho/riak_pb 1.4.1.1
dep_riakc = git https://github.com/basho/riak-erlang-client 1.4.1
dep_jiffy = git https://github.com/davisp/jiffy 0.13.1
include erlang.mk