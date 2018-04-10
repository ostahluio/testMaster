-module(m_BoundedBuffer).
-behaviour(application).
-include_lib("../include/abs_types.hrl").
-export([main/1]).
%% Application callbacks
-export([start/2, stop/1]).

'main'(Cog=#cog{ref=CogRef})->
    put(vars, #{}),
    O = void,
    %% FIXME: thisDC() won't work in main block (DC should be in Cog state)
    DC = object:new(cog:start(),class_ABS_DC_DeploymentComponent, [<<"Initial DC">>,dataEmptyMap,[]],Cog,[O]),
    cog_monitor:new_dc(DC),
    Stack = [DC],
     %% frontend/examples/BoundedBuffer.abs:69--69
    put(vars, (get(vars))#{'buff' => null}),
     %% frontend/examples/BoundedBuffer.abs:70--70
    put(vars, (get(vars))#{'p1' => null}),
     %% frontend/examples/BoundedBuffer.abs:71--71
    put(vars, (get(vars))#{'p2' => null}),
     %% frontend/examples/BoundedBuffer.abs:72--72
    put(vars, (get(vars))#{'c1' => null}),
     %% frontend/examples/BoundedBuffer.abs:73--73
    put(vars, (get(vars))#{'c2' => null}),
     %% frontend/examples/BoundedBuffer.abs:74--74
    put(vars, (get(vars))#{'buff' := object:new(cog:start(DC),class_BoundedBuffer_BoundedBuffer,[5,[]],Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
     %% frontend/examples/BoundedBuffer.abs:75--75
    put(vars, (get(vars))#{'p1' := object:new(cog:start(DC),class_BoundedBuffer_ProducerImpl,[maps:get('buff', get(vars)),[]],Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
     %% frontend/examples/BoundedBuffer.abs:76--76
    put(vars, (get(vars))#{'c1' := object:new(cog:start(DC),class_BoundedBuffer_ConsumerImpl,[maps:get('buff', get(vars)),[]],Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
     %% frontend/examples/BoundedBuffer.abs:77--77
    put(vars, (get(vars))#{'p2' := object:new(cog:start(DC),class_BoundedBuffer_ProducerImpl,[maps:get('buff', get(vars)),[]],Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
     %% frontend/examples/BoundedBuffer.abs:78--78
    put(vars, (get(vars))#{'c2' := object:new(cog:start(DC),class_BoundedBuffer_ConsumerImpl,[maps:get('buff', get(vars)),[]],Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
     %% frontend/examples/BoundedBuffer.abs:79--79
    T_1 = future:start(maps:get('p1', get(vars)),'m_produce',[10,[]],#process_info{method= <<"produce"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
    T_1,
     %% frontend/examples/BoundedBuffer.abs:80--80
    T_2 = future:start(maps:get('c1', get(vars)),'m_consume',[10,[]],#process_info{method= <<"consume"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
    T_2,
     %% frontend/examples/BoundedBuffer.abs:81--81
    T_3 = future:start(maps:get('p2', get(vars)),'m_produce',[10,[]],#process_info{method= <<"produce"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
    T_3,
     %% frontend/examples/BoundedBuffer.abs:82--82
    T_4 = future:start(maps:get('c2', get(vars)),'m_consume',[10,[]],#process_info{method= <<"consume"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
    T_4.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    runtime:start_link([m_BoundedBuffer]).

stop(_State) ->
    ok.
