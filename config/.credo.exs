%{
  configs: [
    %{
      name: "default",
      files: %{
        included: ["mix.exs", "lib/"]
      },
      checks: [
        {Credo.Check.Readability.ModuleDoc, false}
      ]
    }
  ]
}
