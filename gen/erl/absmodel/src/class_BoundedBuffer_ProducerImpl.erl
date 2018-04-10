-module(class_BoundedBuffer_ProducerImpl).
-include_lib("../include/abs_types.hrl").
-behaviour(object).
-export([get_val_internal/2,set_val_internal/3,init_internal/0,get_all_state/1]).
-compile(export_all).

exported() -> #{  }.

'init'(O=#object{class=class_BoundedBuffer_ProducerImpl=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},[P_b,Stack])->
    put(vars, #{}),
    set(O,'b',P_b),
    O.
 %% frontend/examples/BoundedBuffer.abs:49
 %% frontend/examples/BoundedBuffer.abs:49
'm_produce'(O=#object{class=class_BoundedBuffer_ProducerImpl=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_amount_0,Stack)->
    put(vars, #{ 'this' => O,
 'amount' => V_amount_0 }),
    try
         %% frontend/examples/BoundedBuffer.abs:50--50
        case cmp:gt(maps:get('amount', get(vars)),0) of
            true ->  %% frontend/examples/BoundedBuffer.abs:51--51
            T_1 = future:start(get(O,'b'),'m_append',[maps:get('amount', get(vars)),[]],#process_info{method= <<"append"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
            T_1,
             %% frontend/examples/BoundedBuffer.abs:52--52
            T_2 = future:start(O,'m_produce',[(maps:get('amount', get(vars)) - 1) ,[]],#process_info{method= <<"produce"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
            T_2;
            false ->         ok
        end
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method produce and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
'set'(O=#object{class=class_BoundedBuffer_ProducerImpl=C,ref=Ref,cog=Cog},Var,Val)->
    object:set_field_value(O, Var, Val).

'get'(O=#object{class=class_BoundedBuffer_ProducerImpl=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Var)->
    object:get_field_value(O,Var).

-record(state,{'b'=null}).
'init_internal'()->
    #state{}.

 %% frontend/examples/BoundedBuffer.abs:47
'get_val_internal'(#state{'b'=G},'b')->
    G;
'get_val_internal'(_,_)->
    %% Invalid return value; handled by HTTP API when querying for non-existant field.
    %% Will never occur in generated code.
    none.

 %% frontend/examples/BoundedBuffer.abs:47
'set_val_internal'(S,'b',V)->
    S#state{'b'=V}.

'get_all_state'(S)->
    [
        { 'b', S#state.b }
    ].
