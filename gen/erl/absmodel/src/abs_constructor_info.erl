%%This file is licensed under the terms of the Modified BSD License.
-module(abs_constructor_info).
-compile(export_all).
-include_lib("../include/abs_types.hrl").


to_json(Abs=[dataJust | _]) -> #{
    <<"fromJust"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs))
};
to_json(Abs=[dataLeft | _]) -> #{
    <<"left"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs))
};
to_json(Abs=[dataRight | _]) -> #{
    <<"right"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs))
};
to_json(Abs=[dataPair | _]) -> #{
    <<"fst"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs)),
    <<"snd"/utf8>> => modelapi_v2:abs_to_json(lists:nth(3, Abs))
};
to_json(Abs=[dataTriple | _]) -> #{
    <<"fstT"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs)),
    <<"sndT"/utf8>> => modelapi_v2:abs_to_json(lists:nth(3, Abs)),
    <<"trdT"/utf8>> => modelapi_v2:abs_to_json(lists:nth(4, Abs))
};
to_json(Abs=[dataCons | _]) -> #{
    <<"head"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs)),
    <<"tail"/utf8>> => modelapi_v2:abs_to_json(lists:nth(3, Abs))
};
to_json(Abs=[dataTime | _]) -> #{
    <<"timeValue"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs))
};
to_json(Abs=[dataDuration | _]) -> #{
    <<"durationValue"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs))
};
to_json(Abs=[dataFin | _]) -> #{
    <<"finvalue"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs))
};
to_json(Abs=[dataTestsuites | _]) -> #{
    <<"isNamed"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs)),
    <<"ttss"/utf8>> => modelapi_v2:abs_to_json(lists:nth(3, Abs))
};
to_json(Abs=[dataTestsuite | _]) -> #{
    <<"name"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs)),
    <<"tcs"/utf8>> => modelapi_v2:abs_to_json(lists:nth(3, Abs))
};
to_json(Abs=[dataTestcase | _]) -> #{
    <<"tcname"/utf8>> => modelapi_v2:abs_to_json(lists:nth(2, Abs)),
    <<"tcclassname"/utf8>> => modelapi_v2:abs_to_json(lists:nth(3, Abs)),
    <<"result"/utf8>> => modelapi_v2:abs_to_json(lists:nth(4, Abs)),
    <<"stdout"/utf8>> => modelapi_v2:abs_to_json(lists:nth(5, Abs)),
    <<"stderr"/utf8>> => modelapi_v2:abs_to_json(lists:nth(6, Abs))
};
to_json(Abs) -> builtin:toString(null, list_to_tuple(Abs)).
