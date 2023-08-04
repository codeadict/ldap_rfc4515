# This file is only used as a dependency.
# Use rebar3 instead for compiling, running tests, etc.
defmodule Ldap.RFC4515.MixProject do
  use Mix.Project

  {:ok, [app]} = :file.consult("src/ldap_rfc4515.app.src")
  {:application, :ldap_rfc4515, props} = app
  @props Keyword.take(props, [:applications, :description, :env, :mod, :vsn])

  def application do
    @props
  end

  def project do
    [
      app: :ldap_rfc4515,
      version: to_string(application()[:vsn]),
      language: :erlang
    ]
  end
end
