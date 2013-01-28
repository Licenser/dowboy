%% Feel free to use, reuse and abuse the code in this file.

-module(dowboy_sum_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
%% HTML code taken from misultin's example file.
<<"<html>
<head>
<title>List</title>
</head>
<body onLoad='list_tracer()'>
<textarea id='t' style='width: 800px; height: 150px;'>
erlang*:::global-function-entry
{
  self->funcall_entry_ts[copyinstr(arg1)] = vtimestamp;
}
erlang*:::function-return
{
  @time[copyinstr(arg1)] = sum((vtimestamp - self->funcall_entry_ts[copyinstr(arg1)] ) / 1000);
}</textarea><button onclick='b()'>Run</button><br/>
<table>
<thead>
<tr><td>Function</td><td>Time</td></tr>
</thead>
<tbody id='tab'>
</tbody>
</table>
<script>
/* On load we create our web socket (or flash socket if your browser doesn't support it ) and
   send the d script we wish to be tracing. This extremely powerful and *insecure*. */
socket = undefined;
times = {};
function b() {
  socket.send(document.getElementById('t').value);
}

function list_tracer() {

    //Global vars

    if ('MozWebSocket' in window) {
		WebSocket = MozWebSocket;
	}
    socket = new WebSocket(window.location.href.replace(/^http/, 'ws'));

    /* The only messages we recieve should contain contain the dtrace aggregation data we requested
       on connection. */
    socket.onmessage = function(message){
        var message = JSON.parse(message.data);
        var tab = document.getElementById('tab');
        tab.innerHTML = '';
	    for ( key in message ) {
           if (times[key]) {
             times[key] = times[key] + message[key];
           } else {
             times[key] = + message[key];
           }
	    }
        var s = [];
        for (k in times) {
          s.push([k, times[k]]);
        };
        s.sort(function(a, b) {return b[1] - a[1]});
        s.forEach(function(e) {
           var tr = '<tr><td>' + e[0] + '</td><td>' + e[1] + '</td></tr>';
           tab.innerHTML = tab.innerHTML + tr;
        });

	};

}

</script>
</body>
</html>">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

websocket_init(_Any, Req, []) ->
	timer:send_interval(1000, tick),
	Req2 = cowboy_http_req:compact(Req),
	{ok, Req2, undefined, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    %% We create a new handler
    {ok, Handle} = case State of
                       undefined ->
                           erltrace:open();
                       {Old} ->
                           %% But we want to make sure that any old one is closed first.
                           erltrace:stop(Old),
                           erltrace:open()
                   end,
    %% We've to confert cowboys binary to a list.
    Msg1 = binary_to_list(Msg),
    ok = erltrace:compile(Handle, Msg1),
    ok = erltrace:go(Handle),
    io:format("SCRIPT> ~s~n", [Msg]),
	{ok, Req, {Msg1, Handle}};

websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

websocket_info(tick, Req, {Msg, Handle} = State) ->
     case erltrace:walk(Handle) of
         {ok, R} ->
             JSON = [{list_to_binary(Call), V} || {_, [Call], V} <- R],
             {reply, {text, jsx:encode(JSON)}, Req, State, hibernate};
         ok ->
             {ok, Req, {Msg, Handle}};
         Other ->
             io:format("Error: ~p", [Other]),
             try
                 erltrace:stop(Handle)
             catch
                 _:_ ->
                     ok
             end,
             {ok, Handle1} = erltrace:open(),
             erltrace:compile(Handle1, Msg),
             erltrace:go(Handle1),
             {ok, Req, {Msg, Handle1}}
     end;

websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, {_, Handle}) ->
    erltrace:stop(Handle),
	ok;

websocket_terminate(_Reason, _Req, _State) ->
	ok.
