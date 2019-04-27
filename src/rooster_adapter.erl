-module(rooster_adapter).

-export([config/1, state/1, middleware/1, route_response/1, server_response/1, request/1, nested_route/1, with_middleware/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

base_headers() ->
  [{"Content-type", "application/json"}].

config(Conf) ->
  Default = #{ip          => {0, 0, 0, 0},
              port        => 8080,
              static_path => ["priv", "www"],
              ssl         => {ssl, false},
              ssl_opts    => {ssl_opts, []}},
  maps:merge(Default, Conf).

state(State) ->
  Default = #{routes       => [],
              middleware   => []},
  maps:merge(Default, State).

middleware(Middleware) ->
  Default = #{name  => default,
              enter => fun(ReqResp) -> ReqResp end,
              leave => fun(ReqResp) -> ReqResp end},
  maps:merge(Default, Middleware).

server_response({Status, Response, Header}) ->
  Headers = base_headers() ++ Header,
  {Status, Headers, rooster_json:encode(Response)}.

route_response({Status, Resp}) ->
  {Status, Resp, []};
route_response(Response) ->
  Response.

request({ReqMod, _} = Req) ->
  #{path          => ReqMod:get(path, Req),
    method        => ReqMod:get(method, Req),
    headers       => ReqMod:get(headers, Req),
    body          => rooster_json:decode(ReqMod:recv_body(Req)),
    qs            => ReqMod:parse_qs(Req),
    cookies       => ReqMod:parse_cookie(Req),
    params        => [],
    authorization => ReqMod:get_header_value('Authorization', Req)}.

nested_route([]) -> [];
nested_route([{Method, Path, Fn, Middleware}|T]) ->
  [{Method, Path, Fn, Middleware}] ++ nested_route(T);
nested_route([{Method, Path, Fn}|T]) when erlang:is_function(Fn) =:= true ->
  [{Method, Path, Fn, []}] ++ nested_route(T);
nested_route([{Method, Fn, Middleware}|T]) ->
  [{Method, "", Fn, Middleware}] ++ nested_route(T);
nested_route([{Method, Fn}|T]) ->
  [{Method, "", Fn, []}] ++ nested_route(T).

with_middleware({Method, Path, Fn}) ->
  {Method, Path, Fn, []};
with_middleware(Route) -> Route.
