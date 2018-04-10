-module(class_ABS_DC_DeploymentComponent).
-include_lib("../include/abs_types.hrl").
-behaviour(object).
-export([get_val_internal/2,set_val_internal/3,init_internal/0,get_all_state/1]).
-compile(export_all).

exported() -> #{  }.

'init'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},[P_description,P_initconfig,Stack])->
    put(vars, #{}),
    set(O,'description',P_description),
    set(O,'initconfig',P_initconfig),
     %% abs/lang/abslang.abs:969--969
    set(O,'cpuhistory',[]),
     %% abs/lang/abslang.abs:970--970
    set(O,'cpuhistorytotal',[]),
     %% abs/lang/abslang.abs:971--971
    set(O,'bwhistory',[]),
     %% abs/lang/abslang.abs:972--972
    set(O,'bwhistorytotal',[]),
     %% abs/lang/abslang.abs:973--973
    set(O,'memoryhistory',[]),
     %% abs/lang/abslang.abs:974--974
    set(O,'memoryhistorytotal',[]),
     %% abs/lang/abslang.abs:978--978
    set(O,'cpuconsumed',0),
     %% abs/lang/abslang.abs:979--979
    set(O,'bwconsumed',0),
     %% abs/lang/abslang.abs:980--980
    set(O,'memoryconsumed',0),
     %% abs/lang/abslang.abs:984--984
    set(O,'creationTime',m_ABS_StdLib_funs:f_now(Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% abs/lang/abslang.abs:985--985
    set(O,'shutdownTime',dataNothing),
     %% abs/lang/abslang.abs:989--989
    set(O,'initialized',false),
     %% abs/lang/abslang.abs:992--992
    set(O,'is_shutdown',false),
     %% abs/lang/abslang.abs:994--995
    set(O,'cpu',begin
        case m_ABS_StdLib_funs:f_lookup(Cog,get(O,'initconfig'),dataSpeed,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]) of 
            dataNothing->dataInfRat;
            {dataJust,V_v_0}->{ dataFin,V_v_0};
            _ -> exit(dataPatternMatchFailException)
        end
    end),
     %% abs/lang/abslang.abs:996--996
    set(O,'cpunext',get(O,'cpu')),
     %% abs/lang/abslang.abs:997--998
    set(O,'bw',begin
        case m_ABS_StdLib_funs:f_lookup(Cog,get(O,'initconfig'),dataBandwidth,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]) of 
            dataNothing->dataInfRat;
            {dataJust,V_v_1}->{ dataFin,V_v_1};
            _ -> exit(dataPatternMatchFailException)
        end
    end),
     %% abs/lang/abslang.abs:999--999
    set(O,'bwnext',get(O,'bw')),
     %% abs/lang/abslang.abs:1000--1001
    set(O,'memory',begin
        case m_ABS_StdLib_funs:f_lookup(Cog,get(O,'initconfig'),dataMemory,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]) of 
            dataNothing->dataInfRat;
            {dataJust,V_m_0}->{ dataFin,V_m_0};
            _ -> exit(dataPatternMatchFailException)
        end
    end),
     %% abs/lang/abslang.abs:1002--1002
    set(O,'memorynext',get(O,'memory')),
     %% abs/lang/abslang.abs:1003--1005
    set(O,'paymentInterval',begin
        case m_ABS_StdLib_funs:f_lookup(Cog,get(O,'initconfig'),dataPaymentInterval,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]) of 
            dataNothing->1;
            {dataJust,V_n_0}->builtin:truncate(Cog,V_n_0);
            _ -> exit(dataPatternMatchFailException)
        end
    end),
     %% abs/lang/abslang.abs:1006--1006
    set(O,'costPerInterval',m_ABS_StdLib_funs:f_lookupDefault(Cog,get(O,'initconfig'),dataCostPerInterval, rationals:rdiv(0,1) ,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% abs/lang/abslang.abs:1007--1007
    set(O,'startupDuration',m_ABS_StdLib_funs:f_lookupDefault(Cog,get(O,'initconfig'),dataStartupduration, rationals:rdiv(0,1) ,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% abs/lang/abslang.abs:1008--1008
    set(O,'shutdownDuration',m_ABS_StdLib_funs:f_lookupDefault(Cog,get(O,'initconfig'),dataShutdownduration, rationals:rdiv(0,1) ,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% abs/lang/abslang.abs:1009--1009
    set(O,'numberOfCores',m_ABS_StdLib_funs:f_lookupDefault(Cog,get(O,'initconfig'),dataCores, rationals:rdiv(1,1) ,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),
     %% abs/lang/abslang.abs:1011--1011
    set(O,'cloudprovider',null),
     %% abs/lang/abslang.abs:1013--1013
    set(O,'initialized',true),
    O.
 %% abs/lang/abslang.abs:1015
 %% abs/lang/abslang.abs:1015
'm_load'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_rtype_0,V_periods_0,Stack)->
    put(vars, #{ 'this' => O,
 'rtype' => V_rtype_0,
 'periods' => V_periods_0 }),
    try
         %% abs/lang/abslang.abs:1016--1016
        put(vars, (get(vars))#{'result' => 0}),
         %% abs/lang/abslang.abs:1017--1017
        case maps:get('rtype', get(vars)) of
            dataSpeed->
                 %% abs/lang/abslang.abs:1019--1019
                case (not cmp:eq(get(O,'cpu'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1020--1020
                    put(vars, (get(vars))#{'result' := m_ABS_DC_funs:f_averageDivsN(Cog,get(O,'cpuhistory'),get(O,'cpuhistorytotal'),maps:get('periods', get(vars)),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])});
                    false ->                 ok
                end;
            dataBandwidth->
                 %% abs/lang/abslang.abs:1024--1024
                case (not cmp:eq(get(O,'bw'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1025--1025
                    put(vars, (get(vars))#{'result' := m_ABS_DC_funs:f_averageDivsN(Cog,get(O,'bwhistory'),get(O,'bwhistorytotal'),maps:get('periods', get(vars)),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])});
                    false ->                 ok
                end;
            dataMemory->
                 %% abs/lang/abslang.abs:1029--1029
                case (not cmp:eq(get(O,'memory'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1030--1030
                    put(vars, (get(vars))#{'result' := m_ABS_DC_funs:f_averageDivsN(Cog,get(O,'memoryhistory'),get(O,'memoryhistorytotal'),maps:get('periods', get(vars)),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])});
                    false ->                 ok
                end;
            _->
                 %% abs/lang/abslang.abs:1017--1033
                throw(dataPatternMatchFailException)
        end,
         %% abs/lang/abslang.abs:1034--1034
        maps:get('result', get(vars))
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method load and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1036
 %% abs/lang/abslang.abs:1036
'm_total'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_rtype_0,Stack)->
    put(vars, #{ 'this' => O,
 'rtype' => V_rtype_0 }),
    try
         %% abs/lang/abslang.abs:1037--1037
        put(vars, (get(vars))#{'result' => dataInfRat}),
         %% abs/lang/abslang.abs:1038--1038
        case maps:get('rtype', get(vars)) of
            dataSpeed->
                 %% abs/lang/abslang.abs:1039--1039
                put(vars, (get(vars))#{'result' := get(O,'cpu')});
            dataBandwidth->
                 %% abs/lang/abslang.abs:1040--1040
                put(vars, (get(vars))#{'result' := get(O,'bw')});
            dataMemory->
                 %% abs/lang/abslang.abs:1041--1041
                put(vars, (get(vars))#{'result' := get(O,'memory')});
            _->
                 %% abs/lang/abslang.abs:1038--1042
                throw(dataPatternMatchFailException)
        end,
         %% abs/lang/abslang.abs:1043--1043
        maps:get('result', get(vars))
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method total and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1049
 %% abs/lang/abslang.abs:1049
'm_transfer'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_target_0,V_amount_0,V_rtype_0,Stack)->
    put(vars, #{ 'this' => O,
 'target' => V_target_0,
 'amount' => V_amount_0,
 'rtype' => V_rtype_0 }),
    try
         %% abs/lang/abslang.abs:1050--1050
        T_1 = (fun() -> case O of
            null -> throw(dataNullPointerException);
            #object{ref=ObjRef,class=T,cog=Cog} ->
                case is_process_alive(ObjRef) of
                    true ->
                        Vars=get(vars),
                        Result=T:'m_decrementResources'(O,maps:get('amount', get(vars)),maps:get('rtype', get(vars)),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
                        put(vars, Vars),
                        Result;
                    false -> throw(dataObjectDeadException)
                end;
            _ ->
                TempFuture = future:start(O,'m_decrementResources',[maps:get('amount', get(vars)),maps:get('rtype', get(vars)),[]],#process_info{method= <<"decrementResources"/utf8>>},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
                future:get_blocking(TempFuture, Cog, [O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])
        end end)(),
        T_1,
         %% abs/lang/abslang.abs:1051--1051
        T_2 = future:start(maps:get('target', get(vars)),'m_incrementResources',[maps:get('amount', get(vars)),maps:get('rtype', get(vars)),[]],#process_info{method= <<"incrementResources"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
        T_2
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method transfer and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1054
 %% abs/lang/abslang.abs:1054
'm_decrementResources'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_amount_0,V_rtype_0,Stack)->
    put(vars, #{ 'this' => O,
 'amount' => V_amount_0,
 'rtype' => V_rtype_0 }),
    try
         %% abs/lang/abslang.abs:1055--1055
        case maps:get('rtype', get(vars)) of
            dataSpeed->
                 %% abs/lang/abslang.abs:1057--1057
                case (not cmp:eq(get(O,'cpunext'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1058--1058
                    case cmp:ge(m_ABS_DC_funs:f_finvalue(Cog,get(O,'cpunext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars))) of
                        true -> ok;
                        false -> io:format(standard_error, "Assertion failure at abs/lang/abslang.abs:1058~n", []), throw(dataAssertionFailException)
                    end,
                     %% abs/lang/abslang.abs:1059--1059
                    set(O,'cpunext',{ dataFin,( rationals:sub(m_ABS_DC_funs:f_finvalue(Cog,get(O,'cpunext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars)))) });
                    false ->                 ok
                end;
            dataBandwidth->
                 %% abs/lang/abslang.abs:1062--1062
                case (not cmp:eq(get(O,'bwnext'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1063--1063
                    case cmp:ge(m_ABS_DC_funs:f_finvalue(Cog,get(O,'bwnext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars))) of
                        true -> ok;
                        false -> io:format(standard_error, "Assertion failure at abs/lang/abslang.abs:1063~n", []), throw(dataAssertionFailException)
                    end,
                     %% abs/lang/abslang.abs:1064--1064
                    set(O,'bwnext',{ dataFin,( rationals:sub(m_ABS_DC_funs:f_finvalue(Cog,get(O,'bwnext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars)))) });
                    false ->                 ok
                end;
            dataMemory->
                 %% abs/lang/abslang.abs:1066--1066
                case (not cmp:eq(get(O,'memorynext'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1067--1067
                    case cmp:ge(m_ABS_DC_funs:f_finvalue(Cog,get(O,'memorynext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars))) of
                        true -> ok;
                        false -> io:format(standard_error, "Assertion failure at abs/lang/abslang.abs:1067~n", []), throw(dataAssertionFailException)
                    end,
                     %% abs/lang/abslang.abs:1068--1068
                    set(O,'memorynext',{ dataFin,( rationals:sub(m_ABS_DC_funs:f_finvalue(Cog,get(O,'memorynext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars)))) });
                    false ->                 ok
                end;
            _->
                 %% abs/lang/abslang.abs:1055--1070
                throw(dataPatternMatchFailException)
        end
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method decrementResources and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1072
 %% abs/lang/abslang.abs:1072
'm_incrementResources'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_amount_0,V_rtype_0,Stack)->
    put(vars, #{ 'this' => O,
 'amount' => V_amount_0,
 'rtype' => V_rtype_0 }),
    try
         %% abs/lang/abslang.abs:1073--1073
        case maps:get('rtype', get(vars)) of
            dataSpeed->
                 %% abs/lang/abslang.abs:1075--1075
                case (not cmp:eq(get(O,'cpunext'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1076--1076
                    set(O,'cpunext',{ dataFin,( rationals:add(m_ABS_DC_funs:f_finvalue(Cog,get(O,'cpunext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars)))) });
                    false ->                 ok
                end;
            dataBandwidth->
                 %% abs/lang/abslang.abs:1079--1079
                case (not cmp:eq(get(O,'bwnext'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1080--1080
                    set(O,'bwnext',{ dataFin,( rationals:add(m_ABS_DC_funs:f_finvalue(Cog,get(O,'bwnext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars)))) });
                    false ->                 ok
                end;
            dataMemory->
                 %% abs/lang/abslang.abs:1082--1082
                case (not cmp:eq(get(O,'memorynext'),dataInfRat)) of
                    true ->  %% abs/lang/abslang.abs:1083--1083
                    set(O,'memorynext',{ dataFin,( rationals:add(m_ABS_DC_funs:f_finvalue(Cog,get(O,'memorynext'),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),maps:get('amount', get(vars)))) });
                    false ->                 ok
                end;
            _->
                 %% abs/lang/abslang.abs:1073--1085
                throw(dataPatternMatchFailException)
        end
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method incrementResources and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1087
 %% abs/lang/abslang.abs:1087
'm_setProvider'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},V_provider_0,Stack)->
    put(vars, #{ 'this' => O,
 'provider' => V_provider_0 }),
    try
         %% abs/lang/abslang.abs:1088--1088
        set(O,'cloudprovider',maps:get('provider', get(vars)))
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method setProvider and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1090
 %% abs/lang/abslang.abs:1090
'm_convertToDC'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1090--1090
        O
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method convertToDC and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1092
 %% abs/lang/abslang.abs:1092
'm_getProvider'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1092--1092
        get(O,'cloudprovider')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getProvider and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1093
 %% abs/lang/abslang.abs:1093
'm_acquire'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1094--1094
        put(vars, (get(vars))#{'result' => true}),
         %% abs/lang/abslang.abs:1095--1095
        case (not cmp:eq(get(O,'cloudprovider'),null)) of
            true ->  %% abs/lang/abslang.abs:1096--1096
            put(vars, (get(vars))#{'tmp158995547' => future:start(get(O,'cloudprovider'),'m_acquireInstance',[O,[]],#process_info{method= <<"acquireInstance"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
             %% abs/lang/abslang.abs:1096--1096
            future:await(maps:get('tmp158995547', get(vars)), Cog, [O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
            ok,
             %% abs/lang/abslang.abs:1096--1096
            put(vars, (get(vars))#{'result' := future:get_blocking(maps:get('tmp158995547', get(vars)), Cog, [O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])});
            false ->         ok
        end,
         %% abs/lang/abslang.abs:1098--1098
        maps:get('result', get(vars))
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method acquire and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1100
 %% abs/lang/abslang.abs:1100
'm_release'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1101--1101
        put(vars, (get(vars))#{'result' => true}),
         %% abs/lang/abslang.abs:1102--1102
        case (not cmp:eq(get(O,'cloudprovider'),null)) of
            true ->  %% abs/lang/abslang.abs:1103--1103
            put(vars, (get(vars))#{'tmp1624702420' => future:start(get(O,'cloudprovider'),'m_releaseInstance',[O,[]],#process_info{method= <<"releaseInstance"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
             %% abs/lang/abslang.abs:1103--1103
            future:await(maps:get('tmp1624702420', get(vars)), Cog, [O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
            ok,
             %% abs/lang/abslang.abs:1103--1103
            put(vars, (get(vars))#{'result' := future:get_blocking(maps:get('tmp1624702420', get(vars)), Cog, [O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])});
            false ->         ok
        end,
         %% abs/lang/abslang.abs:1105--1105
        maps:get('result', get(vars))
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method release and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1108
 %% abs/lang/abslang.abs:1108
'm_shutdown'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1109--1109
        case not (get(O,'is_shutdown')) of
            true ->  %% abs/lang/abslang.abs:1110--1110
            set(O,'is_shutdown',true),
             %% abs/lang/abslang.abs:1111--1111
            set(O,'shutdownTime',{ dataJust,m_ABS_StdLib_funs:f_now(Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])}),
             %% abs/lang/abslang.abs:1112--1112
            case (not cmp:eq(get(O,'cloudprovider'),null)) of
                true ->  %% abs/lang/abslang.abs:1113--1113
                T_1 = future:start(get(O,'cloudprovider'),'m_internalShutdownInstance',[O,[]],#process_info{method= <<"internalShutdownInstance"/utf8>>, creation={dataTime,builtin:currentms(Cog)}, proc_deadline=dataInfDuration},Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),
                T_1;
                false ->             ok
            end;
            false ->         ok
        end,
         %% abs/lang/abslang.abs:1116--1116
        true
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method shutdown and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1119
 %% abs/lang/abslang.abs:1119
'm_getName'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1119--1119
        get(O,'description')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getName and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1120
 %% abs/lang/abslang.abs:1120
'm_getCreationTime'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1120--1120
        get(O,'creationTime')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getCreationTime and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1121
 %% abs/lang/abslang.abs:1121
'm_getStartupDuration'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1121--1121
        get(O,'startupDuration')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getStartupDuration and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1122
 %% abs/lang/abslang.abs:1122
'm_getShutdownDuration'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1122--1122
        get(O,'shutdownDuration')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getShutdownDuration and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1123
 %% abs/lang/abslang.abs:1123
'm_getPaymentInterval'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1123--1123
        get(O,'paymentInterval')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getPaymentInterval and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1124
 %% abs/lang/abslang.abs:1124
'm_getCostPerInterval'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1124--1124
        get(O,'costPerInterval')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getCostPerInterval and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1126
 %% abs/lang/abslang.abs:1126
'm_getAccumulatedCost'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1127--1127
        put(vars, (get(vars))#{'result' => 0}),
         %% abs/lang/abslang.abs:1128--1128
        case (cmp:gt(get(O,'costPerInterval'),0)) and (cmp:gt(get(O,'paymentInterval'),0)) of
            true ->  %% abs/lang/abslang.abs:1129--1129
            put(vars, (get(vars))#{'nIntervals' => builtin:truncate(Cog, rationals:rdiv(builtin:truncate(Cog,m_ABS_StdLib_funs:f_timeDifference(Cog,get(O,'creationTime'),m_ABS_StdLib_funs:f_now(Cog,[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack]),[O,DC|lists:map(fun({_, X}) -> X end, maps:to_list(get(vars))) ++ Stack])),get(O,'paymentInterval')) )}),
             %% abs/lang/abslang.abs:1130--1130
            put(vars, (get(vars))#{'result' := ( rationals:mul(maps:get('nIntervals', get(vars)),get(O,'costPerInterval'))) });
            false ->         ok
        end,
         %% abs/lang/abslang.abs:1132--1132
        maps:get('result', get(vars))
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getAccumulatedCost and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
 %% abs/lang/abslang.abs:1135
 %% abs/lang/abslang.abs:1135
'm_getNumberOfCores'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Stack)->
    put(vars, #{ 'this' => O }),
    try
         %% abs/lang/abslang.abs:1135--1135
        get(O,'numberOfCores')
    catch
        _:Exception ->
            io:format(standard_error, "Uncaught ~s in method getNumberOfCores and no recovery block in class definition, killing object ~s~n", [builtin:toString(Cog, Exception), builtin:toString(Cog, O)]),
            object:die(O, Exception), exit(Exception)
    end.
'set'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog},Var,Val)->
    object:set_field_value(O, Var, Val).

'get'(O=#object{class=class_ABS_DC_DeploymentComponent=C,ref=Ref,cog=Cog=#cog{ref=CogRef,dc=DC}},Var)->
    object:get_field_value(O,Var).

-record(state,{'description'=null,'initconfig'=null,'cpuhistory'=null,'cpuhistorytotal'=null,'bwhistory'=null,'bwhistorytotal'=null,'memoryhistory'=null,'memoryhistorytotal'=null,'cpuconsumed'=null,'bwconsumed'=null,'memoryconsumed'=null,'creationTime'=null,'shutdownTime'=null,'initialized'=null,'is_shutdown'=null,'cpu'=null,'cpunext'=null,'bw'=null,'bwnext'=null,'memory'=null,'memorynext'=null,'paymentInterval'=null,'costPerInterval'=null,'startupDuration'=null,'shutdownDuration'=null,'numberOfCores'=null,'cloudprovider'=null}).
'init_internal'()->
    #state{}.

 %% abs/lang/abslang.abs:958
'get_val_internal'(#state{'description'=G},'description')->
    G;
 %% abs/lang/abslang.abs:958
'get_val_internal'(#state{'initconfig'=G},'initconfig')->
    G;
 %% abs/lang/abslang.abs:969
'get_val_internal'(#state{'cpuhistory'=G},'cpuhistory')->
    G;
 %% abs/lang/abslang.abs:970
'get_val_internal'(#state{'cpuhistorytotal'=G},'cpuhistorytotal')->
    G;
 %% abs/lang/abslang.abs:971
'get_val_internal'(#state{'bwhistory'=G},'bwhistory')->
    G;
 %% abs/lang/abslang.abs:972
'get_val_internal'(#state{'bwhistorytotal'=G},'bwhistorytotal')->
    G;
 %% abs/lang/abslang.abs:973
'get_val_internal'(#state{'memoryhistory'=G},'memoryhistory')->
    G;
 %% abs/lang/abslang.abs:974
'get_val_internal'(#state{'memoryhistorytotal'=G},'memoryhistorytotal')->
    G;
 %% abs/lang/abslang.abs:978
'get_val_internal'(#state{'cpuconsumed'=G},'cpuconsumed')->
    G;
 %% abs/lang/abslang.abs:979
'get_val_internal'(#state{'bwconsumed'=G},'bwconsumed')->
    G;
 %% abs/lang/abslang.abs:980
'get_val_internal'(#state{'memoryconsumed'=G},'memoryconsumed')->
    G;
 %% abs/lang/abslang.abs:984
'get_val_internal'(#state{'creationTime'=G},'creationTime')->
    G;
 %% abs/lang/abslang.abs:985
'get_val_internal'(#state{'shutdownTime'=G},'shutdownTime')->
    G;
 %% abs/lang/abslang.abs:989
'get_val_internal'(#state{'initialized'=G},'initialized')->
    G;
 %% abs/lang/abslang.abs:992
'get_val_internal'(#state{'is_shutdown'=G},'is_shutdown')->
    G;
 %% abs/lang/abslang.abs:994
'get_val_internal'(#state{'cpu'=G},'cpu')->
    G;
 %% abs/lang/abslang.abs:996
'get_val_internal'(#state{'cpunext'=G},'cpunext')->
    G;
 %% abs/lang/abslang.abs:997
'get_val_internal'(#state{'bw'=G},'bw')->
    G;
 %% abs/lang/abslang.abs:999
'get_val_internal'(#state{'bwnext'=G},'bwnext')->
    G;
 %% abs/lang/abslang.abs:1000
'get_val_internal'(#state{'memory'=G},'memory')->
    G;
 %% abs/lang/abslang.abs:1002
'get_val_internal'(#state{'memorynext'=G},'memorynext')->
    G;
 %% abs/lang/abslang.abs:1003
'get_val_internal'(#state{'paymentInterval'=G},'paymentInterval')->
    G;
 %% abs/lang/abslang.abs:1006
'get_val_internal'(#state{'costPerInterval'=G},'costPerInterval')->
    G;
 %% abs/lang/abslang.abs:1007
'get_val_internal'(#state{'startupDuration'=G},'startupDuration')->
    G;
 %% abs/lang/abslang.abs:1008
'get_val_internal'(#state{'shutdownDuration'=G},'shutdownDuration')->
    G;
 %% abs/lang/abslang.abs:1009
'get_val_internal'(#state{'numberOfCores'=G},'numberOfCores')->
    G;
 %% abs/lang/abslang.abs:1011
'get_val_internal'(#state{'cloudprovider'=G},'cloudprovider')->
    G;
'get_val_internal'(_,_)->
    %% Invalid return value; handled by HTTP API when querying for non-existant field.
    %% Will never occur in generated code.
    none.

 %% abs/lang/abslang.abs:958
'set_val_internal'(S,'description',V)->
    S#state{'description'=V};
 %% abs/lang/abslang.abs:958
'set_val_internal'(S,'initconfig',V)->
    S#state{'initconfig'=V};
 %% abs/lang/abslang.abs:969
'set_val_internal'(S,'cpuhistory',V)->
    S#state{'cpuhistory'=V};
 %% abs/lang/abslang.abs:970
'set_val_internal'(S,'cpuhistorytotal',V)->
    S#state{'cpuhistorytotal'=V};
 %% abs/lang/abslang.abs:971
'set_val_internal'(S,'bwhistory',V)->
    S#state{'bwhistory'=V};
 %% abs/lang/abslang.abs:972
'set_val_internal'(S,'bwhistorytotal',V)->
    S#state{'bwhistorytotal'=V};
 %% abs/lang/abslang.abs:973
'set_val_internal'(S,'memoryhistory',V)->
    S#state{'memoryhistory'=V};
 %% abs/lang/abslang.abs:974
'set_val_internal'(S,'memoryhistorytotal',V)->
    S#state{'memoryhistorytotal'=V};
 %% abs/lang/abslang.abs:978
'set_val_internal'(S,'cpuconsumed',V)->
    S#state{'cpuconsumed'=V};
 %% abs/lang/abslang.abs:979
'set_val_internal'(S,'bwconsumed',V)->
    S#state{'bwconsumed'=V};
 %% abs/lang/abslang.abs:980
'set_val_internal'(S,'memoryconsumed',V)->
    S#state{'memoryconsumed'=V};
 %% abs/lang/abslang.abs:984
'set_val_internal'(S,'creationTime',V)->
    S#state{'creationTime'=V};
 %% abs/lang/abslang.abs:985
'set_val_internal'(S,'shutdownTime',V)->
    S#state{'shutdownTime'=V};
 %% abs/lang/abslang.abs:989
'set_val_internal'(S,'initialized',V)->
    S#state{'initialized'=V};
 %% abs/lang/abslang.abs:992
'set_val_internal'(S,'is_shutdown',V)->
    S#state{'is_shutdown'=V};
 %% abs/lang/abslang.abs:994
'set_val_internal'(S,'cpu',V)->
    S#state{'cpu'=V};
 %% abs/lang/abslang.abs:996
'set_val_internal'(S,'cpunext',V)->
    S#state{'cpunext'=V};
 %% abs/lang/abslang.abs:997
'set_val_internal'(S,'bw',V)->
    S#state{'bw'=V};
 %% abs/lang/abslang.abs:999
'set_val_internal'(S,'bwnext',V)->
    S#state{'bwnext'=V};
 %% abs/lang/abslang.abs:1000
'set_val_internal'(S,'memory',V)->
    S#state{'memory'=V};
 %% abs/lang/abslang.abs:1002
'set_val_internal'(S,'memorynext',V)->
    S#state{'memorynext'=V};
 %% abs/lang/abslang.abs:1003
'set_val_internal'(S,'paymentInterval',V)->
    S#state{'paymentInterval'=V};
 %% abs/lang/abslang.abs:1006
'set_val_internal'(S,'costPerInterval',V)->
    S#state{'costPerInterval'=V};
 %% abs/lang/abslang.abs:1007
'set_val_internal'(S,'startupDuration',V)->
    S#state{'startupDuration'=V};
 %% abs/lang/abslang.abs:1008
'set_val_internal'(S,'shutdownDuration',V)->
    S#state{'shutdownDuration'=V};
 %% abs/lang/abslang.abs:1009
'set_val_internal'(S,'numberOfCores',V)->
    S#state{'numberOfCores'=V};
 %% abs/lang/abslang.abs:1011
'set_val_internal'(S,'cloudprovider',V)->
    S#state{'cloudprovider'=V}.

'get_all_state'(S)->
    [
        { 'description', S#state.description }
        , { 'initconfig', S#state.initconfig }
        , { 'cpuhistory', S#state.cpuhistory }
        , { 'cpuhistorytotal', S#state.cpuhistorytotal }
        , { 'bwhistory', S#state.bwhistory }
        , { 'bwhistorytotal', S#state.bwhistorytotal }
        , { 'memoryhistory', S#state.memoryhistory }
        , { 'memoryhistorytotal', S#state.memoryhistorytotal }
        , { 'cpuconsumed', S#state.cpuconsumed }
        , { 'bwconsumed', S#state.bwconsumed }
        , { 'memoryconsumed', S#state.memoryconsumed }
        , { 'creationTime', S#state.creationTime }
        , { 'shutdownTime', S#state.shutdownTime }
        , { 'initialized', S#state.initialized }
        , { 'is_shutdown', S#state.is_shutdown }
        , { 'cpu', S#state.cpu }
        , { 'cpunext', S#state.cpunext }
        , { 'bw', S#state.bw }
        , { 'bwnext', S#state.bwnext }
        , { 'memory', S#state.memory }
        , { 'memorynext', S#state.memorynext }
        , { 'paymentInterval', S#state.paymentInterval }
        , { 'costPerInterval', S#state.costPerInterval }
        , { 'startupDuration', S#state.startupDuration }
        , { 'shutdownDuration', S#state.shutdownDuration }
        , { 'numberOfCores', S#state.numberOfCores }
        , { 'cloudprovider', S#state.cloudprovider }
    ].
