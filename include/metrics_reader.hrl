-type metric_type() :: counter | histogram | gauge | meter | spiral.

-type gen_server_startlink_ret() :: {ok, pid()}
                                  | ignore
                                  | {error, {already_started, pid()}}
                                  | {error, any()}.

