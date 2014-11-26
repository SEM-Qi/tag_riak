{application,tag_riak,
             [{description,"DB interface between server and Riak"},
              {vsn,"0.1"},
              {modules,[tag_riak,tag_riak_refserv,tag_riak_serv,tag_riak_sup,
                        tag_riak_top]},
              {registered,[]},
              {applications,[kernel,stdlib,jiffy,riakc]},
              {mod,{tag_riak,[]}},
              {env,[]}]}.
