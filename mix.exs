defmodule Protean.MixProject do
  use Mix.Project

  @name "Protean"
  @version "0.1.0"
  @source_url "https://github.com/zachallaun/protean"

  def project do
    [
      app: :protean,
      name: @name,
      version: @version,
      source_url: @source_url,
      package: package(),
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      dialyzer: dialyzer(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        t: :test,
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  def application, do: application(Mix.env())

  defp application(:prod) do
    [
      mod: {Protean.Application, []},
      extra_applications: [:logger, :crypto]
    ]
  end

  defp application(_) do
    [
      mod: {Protean.Application, []},
      extra_applications: [:logger, :crypto, :phoenix_pubsub]
    ]
  end

  defp package do
    [
      description: description(),
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url
      }
    ]
  end

  defp deps do
    [
      {:phoenix_pubsub, "~> 2.0", optional: true},

      # dev/test
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false},
      {:dialyxir, "~> 1.2", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.15", only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def aliases do
    [
      t: "coveralls"
    ]
  end

  defp description do
    """
    Library for managing stateful interaction and side-effects with state machines and
    statecharts.
    """
  end

  defp dialyzer do
    [
      plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
      flags: [
        :underspecs,
        :extra_return,
        :missing_return
      ]
    ]
  end

  defp docs do
    [
      main: @name,
      source_url: @source_url,
      extras: [
        # "docs/guides/introduction.livemd": [title: "Introduction"]
      ],
      groups_for_extras: [
        # Guides: Path.wildcard("docs/guides/*")
      ],
      groups_for_modules: [
        Core: [
          Protean.Action,
          Protean.Builder,
          Protean.Context,
          Protean.Guard,
          Protean.MachineConfig,
          Protean.Transition
        ],
        Mechanics: [
          Protean.Interpreter,
          Protean.MachineConfig,
          Protean.Machinery,
          Protean.Node,
          Protean.Supervisor
        ]
      ],
      groups_for_functions: [
        "Callback Actions": &(&1[:type] == :callback_action),
        "Inline Actions": &(&1[:type] == :inline_action)
      ]
    ]
  end
end
