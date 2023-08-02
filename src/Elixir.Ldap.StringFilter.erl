-module('Elixir.Ldap.StringFilter').

-export([parse/1]).

parse(String) ->
  ldap_string_filters:parse(String).
