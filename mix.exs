defmodule Protean.MixProject do
  use Mix.Project

  @name "Protean"
  @version "0.1.0-alpha.1"
  @source_url "https://github.com/zachallaun/protean"

  def project do
    [
      app: :protean,
      name: @name,
      version: @version,
      source_url: @source_url,
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger, :crypto]
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
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.28.4", only: :dev, runtime: false},
      {:excoveralls, "~> 0.10", only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp description do
    """
    Protean is a currently-experimental library for managing changes in state and side-effects
    through the use of finite state machines and statecharts.
    """
  end

  defp docs do
    [
      main: @name,
      source_url: @source_url,
      output: "docs/ex_doc",
      extras: [
        # "docs/guides/introduction.livemd": [title: "Introduction"]
      ],
      groups_for_extras: [
        # Guides: Path.wildcard("docs/guides/*")
      ],
      groups_for_modules: [
        "Core Concepts": [
          Protean.MachineConfig,
          Protean.Context,
          Protean.Transition,
          Protean.Action,
          Protean.Guard
        ],
        Mechanics: [Protean.Interpreter, Protean.Machinery, Protean.Node, Protean.Supervisor]
      ],
      groups_for_functions: [
        "Callback Actions": &(&1[:type] == :callback_action),
        "Inline Actions": &(&1[:type] == :inline_action)
      ]
    ]
  end
end
