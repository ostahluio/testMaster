-module(m_ABS_DC_funs).
-compile(export_all).
-include_lib("../include/abs_types.hrl").

'f_sumDivsN'(Cog=#cog{ref=CogRef},V_consumeds_0,V_totals_0,V_n_0,Stack)->
    receive
        {stop_world, CogRef} ->
            cog:process_is_blocked_for_gc(Cog, self()),
            cog:process_is_runnable(Cog,self()),
            task:wait_for_token(Cog,lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack);
        die_prematurely ->
            task:send_notifications(killed_by_the_clock),
            exit(killed_by_the_clock)
    after 0 -> ok end,
    case ((cmp:eq(V_n_0,0)) or (m_ABS_StdLib_funs:f_isEmpty(Cog,V_consumeds_0,Stack))) or (m_ABS_StdLib_funs:f_isEmpty(Cog,V_totals_0,Stack)) of
        true -> 0;
        false -> ( rationals:add( rationals:rdiv(( rationals:mul(m_ABS_StdLib_funs:f_head(Cog,V_consumeds_0,Stack),100)) ,m_ABS_StdLib_funs:f_head(Cog,V_totals_0,Stack)) ,m_ABS_DC_funs:f_sumDivsN(Cog,m_ABS_StdLib_funs:f_tail(Cog,V_consumeds_0,Stack),m_ABS_StdLib_funs:f_tail(Cog,V_totals_0,Stack),(V_n_0 - 1) ,Stack))) 
    end.

'f_averageDivsN'(Cog=#cog{ref=CogRef},V_consumeds_0,V_totals_0,V_length_0,Stack)->
    receive
        {stop_world, CogRef} ->
            cog:process_is_blocked_for_gc(Cog, self()),
            cog:process_is_runnable(Cog,self()),
            task:wait_for_token(Cog,lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack);
        die_prematurely ->
            task:send_notifications(killed_by_the_clock),
            exit(killed_by_the_clock)
    after 0 -> ok end,
    (fun (V_mins_0)->case cmp:eq(V_mins_0,0) of
        true -> 0;
        false ->  rationals:rdiv(m_ABS_DC_funs:f_sumDivsN(Cog,V_consumeds_0,V_totals_0,V_mins_0,Stack),V_mins_0) 
    end end(m_ABS_StdLib_funs:f_min(Cog,V_length_0,m_ABS_StdLib_funs:f_min(Cog,m_ABS_StdLib_funs:f_length(Cog,V_consumeds_0,Stack),m_ABS_StdLib_funs:f_length(Cog,V_totals_0,Stack),Stack),Stack))).

'f_thisDC'(Cog=#cog{ref=CogRef},Stack)->
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

'f_finvalue'(Cog=#cog{ref=CogRef},V_data_0,Stack)->
    receive
        {stop_world, CogRef} ->
            cog:process_is_blocked_for_gc(Cog, self()),
            cog:process_is_runnable(Cog,self()),
            task:wait_for_token(Cog,lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack);
        die_prematurely ->
            task:send_notifications(killed_by_the_clock),
            exit(killed_by_the_clock)
    after 0 -> ok end,
    begin
        case V_data_0 of 
            {dataFin,V_res_0}->V_res_0;
            _ -> exit(dataPatternMatchFailException)
        end
    end.

