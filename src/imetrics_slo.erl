-module(imetrics_slo).

-export([svr_ref/1, uid_name/1,
         get/3, add/3, add/4, put/4, dump/1, dump/2, info/1, foldl_dump/3]).

-define(CATCH_KNOWN_EXC(X), try
                                X
                            catch
                                exit:{noproc,{gen_server,call, _}} ->
                                    {error, unregistered_slo};
                                error:badarg ->
                                    {error, {badarg, check_ets}}
                            end).

svr_ref(UIdName) ->
    list_to_atom("imetrics_slo_" ++ atom_to_list(UIdName)).

uid_name(SvrRef) ->
    case atom_to_list(SvrRef) of
        "imetrics_slo_" ++ UIdNameStr ->
            list_to_atom(UIdNameStr)
    end.

get(UIdName, UId, Key) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:get(svr_ref(UIdName), imetrics_utils:bin(UId), imetrics_utils:bin(Key))
       end
      ).

add(UIdName, UId, Key) ->
    add(UIdName, UId, Key, 1).

add(UIdName, UId, Key, Val) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:add(svr_ref(UIdName), imetrics_utils:bin(UId), imetrics_utils:bin(Key), Val)
       end
      ).

put(UIdName, UId, Key, Val) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:put(svr_ref(UIdName), imetrics_utils:bin(UId), imetrics_utils:bin(Key), Val)
       end
      ).

dump(UIdName) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:dump(svr_ref(UIdName))
       end
      ).

dump(UIdName, UId) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:dump(svr_ref(UIdName), imetrics_utils:bin(UId))
       end
      ).

info(UIdName) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:info(svr_ref(UIdName))
       end
      ).

foldl_dump(UIdName, F, A) ->
    ?CATCH_KNOWN_EXC(
       begin
           icount:foldl_dump(svr_ref(UIdName), F, A)
       end
      ).

