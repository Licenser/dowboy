%% Feel free to use, reuse and abuse the code in this file.

-module(dowboy_heatmap_handler).
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
</head>
<body onLoad='heat_tracer()'>
<canvas id='canvas' width='1024' height='512'></canvas>
<script>
/* On load we create our web socket (or flash socket if your browser doesn't support it ) and
   send the d script we wish to be tracing. This extremely powerful and *insecure*. */
function heat_tracer() {

    //Global vars
    setup();

    if ('MozWebSocket' in window) {
		WebSocket = MozWebSocket;
	}
    var socket = new WebSocket(window.location.href.replace(/^http/, 'ws'));

    socket.onopen = function() {
	    var dscript = \"syscall:::entry\\n{\\nself->syscall_entry_ts[probefunc] = vtimestamp;\\n}\\nsyscall:::return\\n/self->syscall_entry_ts[probefunc]/\\n{\\n\\n@time[probefunc] = lquantize((vtimestamp - self->syscall_entry_ts[probefunc] ) / 1000, 0, 63, 2);\\nself->syscall_entry_ts[probefunc] = 0;\\n}\";
	    socket.send(dscript);
	};


    /* The only messages we recieve should contain contain the dtrace aggregation data we requested
       on connection. */
    socket.onmessage = function(message){
        var message = JSON.parse(message.data);
	    draw(message);

	    /* for ( key in message ) {
	       val = message[key];
	       console.log( 'key: ' + key + ', interval: ' + val[0][0] + '-' + val[0][1], ', count ' + val[1] );
	       }
	    */
	};

}


/* Take the aggregation data and update the heatmap */
function draw(message) {

    /* Latest data goes in the right most column, initialize it */
    var syscalls_by_latency = [];
    for ( var index = 0; index < 32; index++ ) {
	syscalls_by_latency[index] = 0;
    }

    /* Presently we have the latency for each system call quantized in our message. Merge the data
       such that we have all the system call latency quantized together. This gives us the number
       of syscalls made with latencies in each particular band. */
    for ( var syscall in message ) {
	var val = message[syscall];
	for ( result_index in val ) {
	    var latency_start = val[result_index][0][0];
	    var count =  val[result_index][1];
	    /* The d script we're using lquantizes from 0 to 63 in steps of two. So dividing by 2
	       tells us which row this result belongs in */
	    syscalls_by_latency[Math.floor(latency_start/2)] += count;
	}
    }
    /* We just created a new column, shift the console to the left and add it. */
    console_columns.shift();
    console_columns.push(syscalls_by_latency);
    drawArray(console_columns);
}



/* Draw the columns and rows that map up the heatmap on to the canvas element */
function drawArray(console_columns) {
    var canvas = document.getElementById('canvas');
    if (canvas.getContext) {
	var ctx = canvas.getContext('2d');
	for ( var column_index in console_columns ) {
	    var column = console_columns[column_index];
	    for ( var entry_index in column ) {
		entry = column[entry_index];

		/* Were using a logarithmic scale for the brightness. This was all arrived at by
		   trial and error and found to work well on my Mac.  In the future this
		   could all be adjustable with controls */
		var red_value = 0;
		if ( entry != 0 ) {
		    red_value = Math.floor(Math.log(entry)/Math.log(2));
		}
		//console.log(red_value);
		ctx.fillStyle = 'rgb(' + (red_value * 25) + ',0,0)';
		ctx.fillRect(column_index*16, 496-(entry_index*16), 16, 16);
	    }
	}
    }
}


/* The heatmap is is really a 64x32 grid. Initialize the array which contains the grid data. */
function setup() {
    console_columns = [];
    for ( var column_index = 0; column_index < 64; column_index++ ) {
	var column = [];
	for ( var entry_index = 0; entry_index < 32; entry_index++ ) {
	    column[entry_index] = 0;
	}
	console_columns.push(column);
    }

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
    {ok, Handle} = case State of
                       undefined ->
                           erltrace:open();
                       {Old} ->
                           erltrace:close(Old),
                           erltrace:open()
                   end,
    Msg1 = binary_to_list(Msg),
    erltrace:compile(Handle, Msg1),
    erltrace:go(Handle),
    io:format("SCRIPT> ~s~n", [Msg]),
	{ok, Req, {Msg1, Handle}};

websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

websocket_info(tick, Req, {Msg, Handle} = State) ->
     case erltrace:walk(Handle) of
         {ok, R} ->
             JSON = [{list_to_binary(Call),[ [[S, E], V]|| {{S, E}, V} <- Vs]}|| {lquantize, [Call], Vs} <- R],
             {reply, {text, jsx:encode(JSON)}, Req, State, hibernate};
         Other ->
             case Other of
                 {error, E} ->
                     io:format("Error: ~p", [E]);
                 _ ->
                     ok
             end,
             try
                 erltrace:stop(Handle)
             catch
                 _:_ ->
                     ok
             end,
             {ok, Handle1} = erltrace:open(),
             erltrace:compile(Handle1, Msg),
             {ok, Req, {Msg, Handle1}}
     end;

websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.


websocket_terminate(_Reason, _Req, {_, Handle}) ->
    erltrace:stop(Handle),
	ok;

websocket_terminate(_Reason, _Req, _State) ->
	ok.
