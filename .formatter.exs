# Used by "mix format"
inputs =
  Enum.flat_map(
    ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
    &Path.wildcard(&1, match_dot: true)
  )

locals_without_parens: [defmachine: 1]

[
  inputs: inputs -- ["lib/protean/utilities.ex"],
  locals_without_parens: locals_without_parens,
  export: [
    locals_without_parens: locals_without_parens
  ]
]
