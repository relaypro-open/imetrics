-module(varz).

-export([get/3]).

get(SessionId, _Headers, _ReqBody) ->
    RespHeaders = "Content-Type: text/plain\r\n\r\n",
    mod_esi:deliver(SessionId, RespHeaders),
    try
        case imetrics:get() of
            [] ->
                mod_esi:deliver(SessionId, "\n");
            Data ->
                lists:foreach(
                    fun
                        ({Name, Value}) when is_number(Value) ->
                            VStr = strnum(Value),
                            mod_esi:deliver(SessionId, [<<Name/binary, " ">>, VStr, "\n"]);
                        ({Name, Map}) when is_map(Map) ->
                            MIo = serialize_proplist(maps:to_list(Map)),
                            mod_esi:deliver(SessionId, [<<Name/binary, " ">>, MIo, "\n"]);
                        ({Name, MappedValues}) when is_list(MappedValues) ->
                            MIo = serialize_proplist(MappedValues),
                            mod_esi:deliver(SessionId, [<<Name/binary, " ">>, MIo, "\n"])
                    end, Data)
        end
    catch Class:Reason ->
        io:format("exception ~p:~p~n", [Class, Reason]),
        io:format("~p~n", [erlang:get_stacktrace()]),
        ok
    end.

serialize_proplist(List) ->
    MIo = lists:map(
            fun({Key, Value}) when is_number(Value); Value =:= 'NaN' ->
                    VStr = strnum(Value),
                    [Key, ":", VStr]
            end, List),
    string:join(MIo, " ").

strnum('NaN') -> "NaN";
strnum(N) when is_integer(N) -> integer_to_list(N);
strnum(N) when is_float(N) -> float_to_list(N, [{decimals, 6}, compact]).
