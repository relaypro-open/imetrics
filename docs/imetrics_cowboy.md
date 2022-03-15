# imetrics_cowboy

The `imetrics_cowboy` module keeps track of metrics related to HTTP requests and
responses made through the Cowboy web server. It makes use of the [`cowboy_metrics_h` stream handler](https://ninenines.eu/docs/en/cowboy/2.8/manual/cowboy_metrics_h/),
which is included with Cowboy itself. It is configured in the call to `cowboy:start_clear/3`,
or `cowboy:start_tls/3` as such:

```erlang
Dispatch = cowboy_router:compile([ ... ]),
cowboy:start_clear(
    HandlerModule,
    [{port, Port}],
    #{
        % your router configuration:
        env => #{dispatch => Dispatch},

        % additional ProtocolOpts configurations
        % ...

        % make sure to include `cowboy_stream_h` as the last stream handler
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h],
        metrics_callback => fun imetrics_cowboy:metrics_callback/1
    }
)
```

Once you've done this, the following counters will be automatically populated:

-   `cowboy_responses` - the number of responses. Tagged with `code="{HTTP_STATUS_CODE}"`
-   `cowboy_error_responses` - the number of responses where the cowboy handler encountered an error. Tagged with:
    -   `code="{HTTP_STATUS_CODE}"`
    -   `reason="{REASON}"` where **REASON** is one of `[internal_error, socket_error, stream_error, connection_error, stop]`
        [(see `cowboy_stream`'s `reason()` type)](https://ninenines.eu/docs/en/cowboy/2.8/manual/cowboy_stream/#_reason)

## Custom tags

You can also provide custom tags for your metrics, by using the `imetrics_cowboy:add_tag/3`
and `imetrics_cowboy:add_tags/2` functions. For example, if you'd like to tag the handler
responsible for handling a specific request, just call:

```erlang
imetrics_cowboy:add_tag(Req, handler, handler_name_here),
% or
imetrics_cowboy:add_tags(Req, #{ handler => handler_name_here }).
```

## Ignoring certain requests

If one particular endpoint is requested frequently, you can ignore those requests by calling
`imetrics_cowboy:ignore_request(Req)`. This will cause that request to be entirely ignored by
the metrics callback.
