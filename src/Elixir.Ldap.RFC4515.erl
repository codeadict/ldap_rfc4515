-module('Elixir.Ldap.RFC4515').

-export([to_eldap/1]).

-spec to_eldap(binary() | list()) -> {ok, ldap_rfc4515:eldap_filter()} | {error, any()}.
to_eldap(String) ->
  ldap_rfc4515:to_eldap(String).
