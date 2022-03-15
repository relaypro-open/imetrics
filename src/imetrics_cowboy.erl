-module(imetrics_cowboy).

-export([metrics_callback/1, add_tag/3, add_tags/2, ignore_request/1]).

cowboy_responses_metric() ->
    application:get_env(imetrics, cowboy_responses_metric, cowboy_responses).

cowboy_error_responses_metric() ->
    application:get_env(imetrics, cowboy_error_responses_metric, cowboy_error_responses).

metrics_callback(#{user_data := #{'_imetrics_ignore_request' := true}}) ->
    ignore_request;
metrics_callback(Metrics) ->
    StatusCode = maps:get(resp_status, Metrics),
    UserData = maps:get(user_data, Metrics, #{}),

    SanitizedUserData = maps:remove('_imetrics_ignore_request', UserData),
    Tags = #{
        code => imetrics_utils:bin(StatusCode)
    },
    NewTags = maps:merge(SanitizedUserData, Tags),

    case maps:get(reason, Metrics) of
        normal -> ok;
        switch_protocol -> ok;
        Tuple when is_tuple(Tuple) ->
            Reason = element(1, Tuple),
            imetrics:add(cowboy_error_responses_metric(), NewTags#{ reason => imetrics_utils:bin(Reason) })
    end,

    imetrics:add(cowboy_responses_metric(), NewTags).

add_tag(Req, Name, Value) when is_atom(Name) ->
    cowboy_req:cast(
        {set_options, #{metrics_user_data => #{Name => imetrics_utils:bin(Value)}}}, Req
    ),
    Req.

add_tags(Req, Tags) when is_map(Tags) ->
    maps:fold(fun(Name, Value, Acc) -> add_tag(Acc, Name, Value) end, Req, Tags).

ignore_request(Req) ->
    cowboy_req:cast(
        {set_options, #{metrics_user_data => #{'_imetrics_ignore_request' => true}}}, Req
    ),
    Req.
