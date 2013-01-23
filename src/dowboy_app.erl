-module(dowboy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
                {'_', [
                       {'_', dowboy_heatmap_handler, []}
                      ]}
               ],
    {ok, _} = cowboy:start_listener(my_http_listener, 100,
                                    cowboy_tcp_transport, [{port, 8080}],
                                    cowboy_http_protocol, [{dispatch, Dispatch}]
                                   ),
    dowboy_sup:start_link().

stop(_State) ->
    ok.
