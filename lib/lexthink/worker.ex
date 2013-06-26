defmodule Lexthink.Worker do
  use GenServer.Behaviour

  defrecord State, socket: nil, database: nil, token: 1

  defp __RETHINKDB_VERSION__, do: 0x723081e1 # v_02 magic number from ql2.proto

  @spec start_link(any, list) :: any
  def start_link(ref, opts) do
      {:ok, pid} = :gen_server.start_link(__MODULE__, [opts], [])
      Lexthink.Server.add_worker(ref, pid)
      {:ok, pid}
  end

  @spec use(pid, binary) :: :ok
  def use(pid, name) when is_binary(name) do
      :gen_server.cast(pid, {:use, name})
  end

  #@spec query(pid, :term) ::
  def query(pid, query) do
      timeout = :application.get_env(:lexthink, :timeout, 30000)
      :gen_server.call(pid, {:query, query}, timeout)
  end

  @spec init([{atom, any}]) :: {:ok, tuple}
  def init([opts]) do
      host = Keyword.get(opts, :address, {127,0,0,1})
      port = Keyword.get(opts, :port, 28015)
      database = Keyword.get(opts, :database, "test")
      auth_key = Keyword.get(opts, :auth_key, "")
      {:ok, socket} = :gen_tcp.connect(host, port, [:binary, {:packet, 0}, {:active, :false}])
      :ok = login(auth_key, socket)
      state = State.new(socket: socket, database: database)
      {:ok, state}
  end

  #-spec handle_call(tuple(), pid(), #state{}) -> {reply, ok | lethink:response(), #state{}}.
  def handle_call({:query, term}, _from, state) do
      query = :query.new(type: :'START',
                         query: term,
                         token: state.token,
                         global_optargs: Ql2.Util.global_db(state.database))
      reply = send_and_recv(query, state.socket)
      {:reply, reply, state.token(state.token + 1)}
  end

  #-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
  def handle_cast({:use, name}, state) do
      {:noreply, state.database(name)}
  end

  #-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
  def handle_info(info, state) do
      IO.write("Info: #{info}")
      {:noreply, state}
  end

  #-spec terminate(any(), #state{}) -> ok.
  def terminate(reason, state) do
      IO.write("terminating: #{reason}")
      :gen_tcp.close(state.socket)
      :ok
  end

#-spec send_and_recv(#query{}, port()) -> lethink:response().
  defp send_and_recv(query, socket) do
      send(query, socket)
      response = recv(socket)
      handle_response(Ql2.decode_response(response))
  end

  #-spec send(#query{}, port()) -> any().
  defp send(query, socket) do
      iolist = Ql2.encode_query(query)
      length = iolist_size(iolist)
      :gen_tcp.send(socket, [<<length :: [size(32), little]>>, iolist])
  end

  #-spec recv(port()) -> any().
  defp recv(socket) do
      {:ok, responseLength} = :gen_tcp.recv(socket, 4)
      {:ok, response} = :gen_tcp.recv(socket, :binary.decode_unsigned(responseLength, :little))
      response
  end

  #-spec handle_response(#response{}) -> lethink:response().
  defp handle_response(:response[type: :'SUCCESS_ATOM', response: [datum]]) do
      {:ok, Ql2.Util.datum_value(datum)}
  end
  defp handle_response(:response[type: :'SUCCESS_SEQUENCE', response: data]) do
      {:ok, lc d inlist data, do: Ql2.Util.datum_value(d)}
  end
  defp handle_response(:response[type: :'SUCCESS_PARTIAL', response: [datum]]) do
      {:ok, Ql2.Util.datum_value(datum)}
  end

  defp handle_response(:response[type: :'CLIENT_ERROR', response: [datum]] = response) do
      errorMsg = Ql2.Util.datum_value(datum)
      {:error, errorMsg, response.type, response.backtrace}
  end
  defp handle_response(:response[type: :'COMPILE_ERROR', response: [datum]] = response) do
      errorMsg = Ql2.Util.datum_value(datum)
      {:error, errorMsg, response.type, response.backtrace}
  end
  defp handle_response(:response[type: :'RUNTIME_ERROR', response: [datum]] = response) do
      errorMsg = Ql2.Util.datum_value(datum)
      {:error, errorMsg, response.type, response.backtrace}
  end

  @spec login(binary, port) :: :ok | {:error, binary}
  defp login(auth_key, socket) do
      key_length = iolist_size(auth_key)
      :ok = :gen_tcp.send(socket, :binary.encode_unsigned(__RETHINKDB_VERSION__, :little))
      :ok = :gen_tcp.send(socket, [<<key_length :: [size(32), little]>>, auth_key])
      {:ok, response} = read_until_null(socket)
      case response == <<"SUCCESS",0>> do
          :true -> :ok;
          :false ->
              :io.fwrite("Error: ~s~n", [response])
              {:error, response}
      end
  end

  @spec read_until_null(port) :: {:ok, binary}
  defp read_until_null(socket) do
    read_until_null(socket, [])
  end

  @spec read_until_null(port, list) :: {:ok, binary}
  defp read_until_null(socket, acc) do
    {:ok, response} = :gen_tcp.recv(socket, 0)
    result = [acc, response]
    case is_null_terminated(response) do
      :true -> {:ok, iolist_to_binary(result)}
      :false -> read_until_null(socket, result)
    end
  end

  @spec is_null_terminated(binary) :: boolean
  defp is_null_terminated(b) do
      :binary.at(b, iolist_size(b) - 1) == 0
  end
end
