-module('Elixir.Ldap.RFC4515').

-export([to_eldap/1, sigil_F/2]).

-spec to_eldap(binary() | list()) -> {ok, ldap_rfc4515:eldap_filter()} | {error, any()}.
to_eldap(String) ->
  ldap_rfc4515:to_eldap(String).

%% @doc Elixir sigil to parse LDAP filters (RFC4515) using the
%%  `~F"(&(objectClass=user)(objectClass=top)(objectClass=person))"` syntax.
%% @private
-spec sigil_F(binary() | list(), [char()]) ->
               {ok, ldap_rfc4515:eldap_filter()} | {error, any()}.
sigil_F(String, _Opts) ->
  to_eldap(String).
