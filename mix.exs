defmodule Protean.MixProject do
  use Mix.Project

  @name "Protean"
  @version "0.0.2"
  @source_url "https://github.com/zachallaun/protean"

  def project do
    [
      app: :protean,
      version: @version,
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs(),
      source_url: @source_url,
      name: @name
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
      {:ex_doc, "~> 0.28.4", only: :dev, runtime: false}
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
      # source_ref: "v#{@version}",
      extras: [
        # "guides/introduction.livemd": [title: "Introduction"]
      ],
      groups_for_extras: [
        # Guides: Path.wildcard("guides/*")
      ]
    ]
  end
end
