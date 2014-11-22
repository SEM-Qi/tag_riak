# Server to Riak Interface

## This is Team QI's Riak interface layer, sitting between the data server and Riak.

### Installation

* Please note. This installation procedure may not have been implemented yet. This is only valid for unix-like systems. You will need `make` and `git` on your system.

Start by cloning the repository:

	git clone https://github.com/SEM-Qi/tag_riak.git

Go into the 'tw_data_server` directory.

	cd PATH_TO_DIR/tag_riak

Once there type `make` into the command line.

There should be a long output as the project builds. Once it is finished enter the following to start up the program in an erlang shell.

	./_rel/tag_riak_release/bin/tag_riak_release console

When you make changes you can run `make` again to rebuild.

### Current API calls

	tag_riak:connect().  

	%% Returns a Pid of a server that will handle actual riak operations

	tag_riak:tag_attack(Pid, Tag).  

	%% Where Pid is the Pid returned in connect() and the tag is the the tag (in binary string form) you would like info about. Returns JSON object with tag distribution data

	tag_riak:update_taglist(Pid).  

	%% Where Pid is the Pid returned in connect(). Returns JSON object with list of tags

	tag_riak:test_post(Pid).

	%% Where Pid is the Pid returned in connect(). Handles posting data, returns ok if successful


## Dependencies

### [riakc](https://github.com/basho/riak-erlang-client)

An erlang riak client.

## Author

* Team QI

