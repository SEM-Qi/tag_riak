PROJECT = tag_riak
DEPS = riakc jiffy 
dep_riakc = git https://github.com/basho/riak-erlang-client
dep_jiffy = git https://github.com/davisp/jiffy 0.13.1
include erlang.mk