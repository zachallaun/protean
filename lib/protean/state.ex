defmodule Protean.State do
  @moduledoc false

  alias __MODULE__

  defstruct [:value, :event, :actions]

  @type t() :: %State{
          value: state_value(),
          event: {String.t(), term()} | nil,
          actions: list(term())
        }

  @type state_value() :: atom()
end
