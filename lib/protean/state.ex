defmodule Protean.State do
  @moduledoc false

  @type t :: %{
          value: atom() | nonempty_list(atom()),
          event: {String.t(), term()},
          actions: list(term())
        }

  defstruct [:value, :event, :actions]
end
