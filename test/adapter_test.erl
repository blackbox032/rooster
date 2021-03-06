-module(adapter_test).

-include_lib("eunit/include/eunit.hrl").

config_adapter_test() ->
  Expected = #{ip          => {0, 0, 0, 0},
               port        => 8080,
               static_path => ["priv", "www"],
               ssl         => {ssl, false},
               ssl_opts    => {ssl_opts, []}},
  ?assertEqual(Expected, rooster_adapter:config(#{})).

state_adapter_test() ->
  Expected = #{routes     => [],
               middleware => []},
  ?assertEqual(Expected, rooster_adapter:state(#{})).

middleware_test() ->
  Result = rooster_adapter:middleware(#{name => m_test}),
  ?assertEqual(m_test, maps:get(name, Result)),
  ?assert(is_function(maps:get(enter, Result))),
  ?assert(is_function(maps:get(leave, Result))).

server_response_test() ->
  Result = rooster_adapter:server_response({200, #{}, [{"authorization","foo"}]}),
  ?assertEqual({200,
                [{"Content-type", "application/json"},
                 {"authorization","foo"}],
                <<"{}">>}, Result).

route_response_test() ->
  Result = rooster_adapter:route_response({200, #{}}),
  ?assertEqual({200, #{}, []}, Result).

route_response_with_header_test() ->
  Result = rooster_adapter:route_response({200, #{}, [{"foo", "bar"}]}),
  ?assertEqual({200, #{}, [{"foo", "bar"}]}, Result).

base_headers_test() ->
  ?assertEqual([{"Content-type", "application/json"}], rooster_adapter:base_headers()).

nested_route_test() ->
  Fn = fun() -> 1 end,
  Nested = [{'GET', Fn, [test]},
            {'POST', Fn},
            {'GET', "/permissions", Fn},
            {'GET', "/health", Fn, [test]}],
  Expected = [{'GET', "", Fn, [test]},
              {'POST',"", Fn, []},
              {'GET', "/permissions", Fn, []},
              {'GET', "/health", Fn, [test]}],
  Result = rooster_adapter:nested_route(Nested),
  ?assertEqual(Expected, Result).

with_middleware_test() ->
  Route = {'GET', "/foo", foo},
  Result = rooster_adapter:with_middleware(Route),
  ?assertEqual({'GET', "/foo", foo, []}, Result).

with_middleware_valid_test() ->
  Route = {'GET', "/foo", foo, []},
  Result = rooster_adapter:with_middleware(Route),
  ?assertEqual(Route, Result).
