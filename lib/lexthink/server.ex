defmodule Lexthink.Server do
  use GenServer.Behaviour

  defrecord State, pools: []

  @spec start_link() :: {:ok, pid}
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  @spec stop() :: :stopped
  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  @spec add_pool(any) :: :ok
  def add_pool(ref) do
    :gen_server.cast(__MODULE__, {:add_pool, ref})
  end

  @spec remove_pool(any) :: :ok
  def remove_pool(ref) do
    :gen_server.cast(__MODULE__, {:remove_pool, ref})
  end

  @spec add_worker(any, pid) :: :ok
  def add_worker(ref, pid) do
    :gen_server.cast(__MODULE__, {:add_worker, ref, pid})
  end

  @spec get_worker(any) :: pid
  def get_worker(ref) do
    workers = get_all_workers(ref)
    {_, _, micro} = :erlang.now()
    random = rem(1 + micro, length(workers))
    Enum.at(workers, random)
  end

  @spec get_all_workers(any) :: [pid]
  def get_all_workers(ref) do
    :ets.lookup_element(__MODULE__, {:pool, ref}, 2)
  end

  # Callbacks
  @spec init([]) :: {:ok, :state}
  def init([]) do
    {:ok, State.new}
  end

  @spec handle_call(any, {pid, any}, :state) :: {:stop, :normal, :stopped, :state} |
                                                {:reply, :ignored, :state}
  def handle_call(:stop, _from, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :ignored, state}
  end

  @spec handle_cast(any, :state) :: {:noreply, :state}
  def handle_cast({:add_pool, ref}, state) do
    :true = :ets.insert_new(__MODULE__, {{state.pool, Ref}, []})
    {:noreply, state.pools([ref | state.pool])}
  end

  def handle_cast({:remove_pool, ref}, state) do
    :true = :ets.delete(__MODULE__, {:pool, ref})
    {:noreply, state.pools(List.delete(state.pools, ref))}
  end

  def handle_cast({:add_worker, ref, pid}, state) do
    workers = :ets.lookup_element(__MODULE__, {:pool, ref}, 2)
    :true = :ets.insert(__MODULE__, {{:pool, ref}, [pid|workers]})
    _ = :erlang.monitor(:process, pid)
    {:noreply, state}
  end

  def handle_cast(_request, state) do
    {:noreply, state}
  end

  @spec handle_info(any, :state) :: {:noreply, :state}
  def handle_info({'DOWN', _, :process, pid, _}, state) do
    Enum.each(state.pools, fn(ref) ->
      workers = :ets.lookup_element(__MODULE__, {:pool, ref}, 2)
      if pid in workers do
        true = :ets.insert(__MODULE__, {{:pool, ref},
              List.delete(workers, pid)})
      end
    end)
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

end
