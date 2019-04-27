-module(rooster_web).

-export([start/1, stop/0, loop/2]).

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun(Req) -> ?MODULE:loop(Req, DocRoot) end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

loop({ReqMod, _} = Req, _DocRoot) ->
  try
    Response = rooster_dispatcher:match_route(rooster_adapter:request(Req)),
    ReqMod:respond(rooster_adapter:server_response(Response), Req)
  catch
    Type:What ->
      log_error(Type, What),
      ReqMod:respond({500, [{"Content-Type", "application/json"}], request_fail_msg()})
  end.

request_fail_msg() ->
  rooster_json:encode(#{message => <<"Internal server error">>}).

log_error(Type, What) ->
  Report = ["web request failed",
            {type, Type},
            {what, What},
            {trace, erlang:get_stacktrace()}],
  error_logger:error_report(Report).

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

stop() ->
  mochiweb_http:stop(?MODULE).