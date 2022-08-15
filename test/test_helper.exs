ExUnit.start(capture_log: true)

# Ensure that all Protean-managed processes have been cleaned up. This should be useful in
# catching process-leak bugs.
ExUnit.after_suite(fn _ ->
  n_alive = length(Protean.ProcessManager.which_children())
  n_registered = Protean.ProcessManager.count_registered()

  unless n_alive + n_registered == 0 do
    require Logger

    Logger.warn(
      "Machines still alive or registered:\n\tAlive: #{n_alive}\n\tRegistered: #{n_registered}"
    )

    exit(:failure)
  end
end)
