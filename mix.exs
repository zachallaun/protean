defmodule Protean.MixProject do
  use Mix.Project

  def project do
    [
      app: :protean,
      version: "0.0.1",
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      name: "Protean",
      source_url: "https://github.com/zachallaun/protean",
      description: description()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.6", only: :dev, runtime: false},
      {:ex_doc, "~> 0.28.4", only: :dev, runtime: false}
    ]
  end

  defp description do
    """
    Protean is a currently-experimental library for managing changes in state and side-effects
    through the use of finite state machines and statecharts.
    """
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/zachallaun/protean"
      }
    ]
  end
end
