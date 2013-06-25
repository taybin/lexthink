defmodule Lexthink do
  use Application.Behaviour

  @type json_term() :: :null | boolean | number | binary | HashDict | [json_term]

  def start(_type, _args) do
    Lexthink.Supervisor.start_link()
  end

  #@equiv add_pool(any(), pos_integer(), [{address, "localhost"}, {port, 28015}, {database, <<"test">>}])
  @spec add_pool(any, pos_integer) :: :ok
  def add_pool(ref, n_workers) when n_workers > 0 do
      add_pool(ref, n_workers, [])
  end

  @doc """
  Start a pool of connections to a database.
  """
  #@spec add_pool(any, pos_integer, [connect_options()]) -> ok.
  def add_pool(ref, n_workers, opts) when n_workers > 0 do
      :ok = Lexthink.Server.add_pool(ref)
      {:ok, sup_pid} = :supervisor.start_child(Lexthink.Supervisor,
          {{Lexthink.Worker.Supervisor, ref}, {Lexthink.Worker.Supervisor, :start_link, []},
              :permanent, 5000, :supervisor, [Lexthink.Worker.Supervisor]})
      Enum.each 1..n_workers, fn _ -> {:ok, _} = :supervisor.start_child(sup_pid, [ref, opts]) end
  end

  @doc """
  Stop a pool of connections.
  """
  @spec remove_pool(any) :: :ok
  def remove_pool(ref) do
      case :supervisor.terminate_child(Lexthink.Supervisor, {Lexthink.Worker.Supervisor, ref}) do
          :ok ->
              :supervisor.delete_child(Lexthink.Supervisor, {Lexthink.Worker.Supervisor, ref})
          {:error, reason} -> {:error, reason}
      end
  end

  @doc """
  Change all connections in pool to use database for queries.
  """
  @spec use(any, binary) :: :ok
  def use(ref, db) when is_binary(db) do
      worker_pids = Lexthink.Server.get_all_workers(ref)
      Enum.map(worker_pids, fn pid -> Lexthink.Worker.use(pid, db) end)
  end

  @spec run(:term, any) :: :response
  def run(term, ref) do
    worker_pid = Lexthink.Server.get_worker(ref)
    Lexthink.Worker.query(worker_pid, term)
  end


  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Lexthink.Supervisor.start_link
  end
end
