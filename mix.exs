defmodule Lexthink.Mixfile do
  use Mix.Project

  def project do
    [ app: :lexthink,
      version: "0.0.1",
      compilers: [:protobuffs, :elixir, :app],
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ mod: { Lexthink, [] },
      env: [{:timeout, 30 * 1000}] ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
    { :mix_protobuffs, "~> 0.9.0", git: "git://github.com/taybin/mix_protobuffs.git", branch: "test_branch"},
    { :protobuffs, "~> 0.8.0", git: "git://github.com/basho/erlang_protobuffs.git" }
    ]
  end
end
