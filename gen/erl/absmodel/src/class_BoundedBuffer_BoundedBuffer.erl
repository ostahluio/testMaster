-module(class_BoundedBuffer_BoundedBuffer).
-include_lib("../include/abs_types.hrl").
-behaviour(object).
-export([get_val_internal/2,set_val_internal/3,init_internal/0,get_all_state/1]).
-compile(export_all).

exported() -> #{  }.

'init'(O=#object{class=class_BoundedBuffer_BoundedBuffer=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},[P_maxElements,Stack])->
    put(vars, #{}),
    set(O,'maxElements',P_maxElements),
     %% frontend/examples/BoundedBuffer.abs:28--28
    set(O,'buffer',[]),
     %% frontend/examples/BoundedBuffer.abs:29--29
    set(O,'numElements',0),
    O.
 %% frontend/examples/BoundedBuffer.abs:31
 %% frontend/examples/BoundedBuffer.abs:31
'm_append'(O=#object{class=class_BoundedBuffer_BoundedBuffer=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_d_0,Stack)->
    put(vars, #{ 'this' => O,
 'd' => V_d_0 }),
    try
         %% frontend/examples/BoundedBuffer.abs:32--32
        task:release_token(Cog,waiting_poll),
        (fun Poll ([])->
            receive check -> 
                case cmp:lt(get(O,'numElements'),get(O,'maxElements')) of
                    true -> cog:process_poll_is_ready(Cog, self(), get(process_info)), Poll([]);
                    false -> cog:process_poll_is_not_ready(Cog, self(), get(process_info)),  Poll([])
                end;
            wait -> Poll([]);
            token -> ok;
            {stop_world, _Sender} -> % only happens when stop_world and release_token cross
                Poll([]);
            {get_references, Sender} ->
                cog:submit_references(Sender, gc:extract_references([O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
                Poll([]);
            die_prematurely ->
                task:send_notifications(killed_by_the_clock),
                exit(killed_by_the_clock)
        end end)
    ([]),
     %% frontend/examples/BoundedBuffer.abs:33--33
    set(O,'buffer',m_ABS_StdLib_funs:f_appendright(Cog,get(O,'buffer'),maps:get('d', get(vars)),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% frontend/examples/BoundedBuffer.abs:34--34
    set(O,'numElements',(get(O,'numElements') + 1) )
catch
    _:Exception ->
        io:format(standard_error, "Uncaught ~s in method append and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
        object:die(O, Exception), exit(Exception)
end.
 %% frontend/examples/BoundedBuffer.abs:37
 %% frontend/examples/BoundedBuffer.abs:37
'm_remove'(O=#object{class=class_BoundedBuffer_BoundedBuffer=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% frontend/examples/BoundedBuffer.abs:38--38
        put(vars, (get(vars))#{'d' => 0}),
         %% frontend/examples/BoundedBuffer.abs:39--39
        task:release_token(Cog,waiting_poll),
        (fun Poll ([])->
            receive check -> 
                case cmp:gt(get(O,'numElements'),0) of
                    true -> cog:process_poll_is_ready(Cog, self(), get(process_info)), Poll([]);
                    false -> cog:process_poll_is_not_ready(Cog, self(), get(process_info)),  Poll([])
                end;
            wait -> Poll([]);
            token -> ok;
            {stop_world, _Sender} -> % only happens when stop_world and release_token cross
                Poll([]);
            {get_references, Sender} ->
                cog:submit_references(Sender, gc:extract_references([O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
                Poll([]);
            die_prematurely ->
                task:send_notifications(killed_by_the_clock),
                exit(killed_by_the_clock)
        end end)
    ([]),
     %% frontend/examples/BoundedBuffer.abs:40--40
    put(vars, (get(vars))#{'d' := m_ABS_StdLib_funs:f_head(Cog,get(O,'buffer'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
     %% frontend/examples/BoundedBuffer.abs:41--41
    set(O,'buffer',m_ABS_StdLib_funs:f_tail(Cog,get(O,'buffer'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% frontend/examples/BoundedBuffer.abs:42--42
    set(O,'numElements',(get(O,'numElements') - 1) ),
     %% frontend/examples/BoundedBuffer.abs:43--43
    maps:get('d', get(vars))
catch
    _:Exception ->
        io:format(standard_error, "Uncaught ~s in method remove and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
        object:die(O, Exception), exit(Exception)
end.
'set'(O=#object{class=class_BoundedBuffer_BoundedBuffer=C,ref=Ref,cog=Cog},Var,Val)->
    object:set_field_value(O, Var, Val).

'get'(O=#object{class=class_BoundedBuffer_BoundedBuffer=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Var)->
    object:get_field_value(O,Var).

-record(state,{'maxElements'=null,'buffer'=null,'numElements'=null}).
'init_internal'()->
    #state{}.

 %% frontend/examples/BoundedBuffer.abs:25
'get_val_internal'(#state{'maxElements'=G},'maxElements')->
    G;
 %% frontend/examples/BoundedBuffer.abs:28
'get_val_internal'(#state{'buffer'=G},'buffer')->
    G;
 %% frontend/examples/BoundedBuffer.abs:29
'get_val_internal'(#state{'numElements'=G},'numElements')->
    G;
'get_val_internal'(_,_)->
    %% Invalid return value; handled by HTTP API when querying for non-existant field.
    %% Will never occur in generated code.
    none.

 %% frontend/examples/BoundedBuffer.abs:25
'set_val_internal'(S,'maxElements',V)->
    S#state{'maxElements'=V};
 %% frontend/examples/BoundedBuffer.abs:28
'set_val_internal'(S,'buffer',V)->
    S#state{'buffer'=V};
 %% frontend/examples/BoundedBuffer.abs:29
'set_val_internal'(S,'numElements',V)->
    S#state{'numElements'=V}.

'get_all_state'(S)->
    [
        { 'maxElements', S#state.maxElements }
        , { 'buffer', S#state.buffer }
        , { 'numElements', S#state.numElements }
    ].
