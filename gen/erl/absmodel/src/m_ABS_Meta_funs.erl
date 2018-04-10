-module(m_ABS_Meta_funs).
-compile(export_all).
-include_lib("../include/abs_types.hrl").

'f_reflect'(Cog=#cog{ref=CogRef},V_a_0,Stack)->
    receive
        {stop_world, CogRef} ->
            cog:process_is_blocked_for_gc(Cog, self()),
            cog:process_is_runnable(Cog,self()),
            task:wait_for_token(Cog,lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack);
        die_prematurely ->
            task:send_notifications(killed_by_the_clock),
            exit(killed_by_the_clock)
    after 0 -> ok end,
    builtin.

'f_getProductLine'(Cog=#cog{ref=CogRef},Stack)->
    receive
        {stop_world, CogRef} ->
            cog:process_is_blocked_for_gc(Cog, self()),
            cog:process_is_runnable(Cog,self()),
            task:wait_for_token(Cog,lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack);
        die_prematurely ->
            task:send_notifications(killed_by_the_clock),
            exit(killed_by_the_clock)
    after 0 -> ok end,
    builtin.

