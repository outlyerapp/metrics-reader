-type metric_type() :: counter | histogram | gauge | meter | spiral.

-type gen_server_startlink_ret() :: {ok, pid()}
                                  | ignore
                                  | {error, {already_started, pid()}}
                                  | {error, any()}.

-type histogram() :: [map()].
-type tag() :: {string(), any()}.
-type tags() :: [tag()].
-type timestamp() :: pos_integer().
-type counter() :: {integer(), timestamp()}.
