defmodule Lexthink.AST do

  @type build_result :: :term | {:error, binary}

  @spec db_create(binary) :: build_result
  def db_create(name), do: :term.new(type: :'DB_CREATE', args: expr(name))

  @spec db_drop(binary) :: build_result
  def db_drop(name), do: :term.new(type: :'DB_DROP', args: expr(name))

  @spec db_list() :: build_result
  def db_list(), do: :term.new(type: :'DB_LIST')

  #@spec table_create(binary, [lethink:table_options]) :: build_result
  def table_create(name, options) do
    optargs = lc opt inlist options, do: table_option_term(opt)
    :term.new(type: :'TABLE_CREATE', args: expr(name), optargs: optargs)
  end

  #@spec table_create(:term, binary, [lethink:table_options]) :: build_result
  def table_create(:term[type: :'DB'] = db, name, options) do
    optargs = lc opt inlist options, do: table_option_term(opt)
    :term.new(type: :'TABLE_CREATE', args: [db, expr(name)], optargs: optargs)
  end

  #@private
  #-spec table_option_term(lethink:table_options()) -> #term_assocpair{}.
  defp table_option_term({atom, value}) when is_atom(atom) do
      term_assocpair(atom_to_binary(atom, :utf8), value)
  end

  @spec table_drop(binary) :: build_result
  def table_drop(name) do
    :term.new(type: :'TABLE_DROP', args: expr(name))
  end

  @spec table_drop(:term, binary) :: build_result
  def table_drop(:term[ type: :'DB' ] = db, name) do
    :term.new(type: :'TABLE_DROP', args: [db, expr(name)])
  end

  @spec table_list() :: build_result
  def table_list(), do: :term.new(type: :'TABLE_LIST')

  @spec table_list(:term) :: build_result
  def table_list(:term[type: :'DB'] = db), do: :term.new(type: :'TABLE_LIST', args: db)

  #%% @doc Specify a DB.  Must be first operation in query list
  #%% Optional if a default database has been specified via
  #%% @see lethink:use/2
  @spec db(binary) :: build_result
  def db(name), do: :term.new(type: :'DB', args: expr(name))

  @spec table(binary) :: build_result
  def table(name), do: table([], name, :false)

  @spec table(binary, [] | :term) :: build_result
  def table(name, :term[type: :'DB'] = db), do: table(name, :false, db)

  @spec table(binary, boolean, [] | :term) :: build_result
  def table(name, use_outdated, []) when is_binary(name) do
    optargs = [term_assocpair("use_outdated", use_outdated)]
    :term.new(type: :'TABLE', args: expr(name), optargs: optargs)
  end
  def table(name, use_outdated, :term[type: :'DB'] = db) when is_binary(name) do
    optargs = [term_assocpair("use_outdated", use_outdated)]
    :term.new(type: :'TABLE', args: [db, expr(name)], optargs: optargs)
  end

  #@spec insert([:document], :term) :: build_result
  def insert(:term[type: :'TABLE'] = table, data) do
      :io.fwrite("~p~n", [data])
      :term.new(type: :'INSERT', args: [table] ++ lc d inlist data, do: expr(d))
  end

  #@spec insert([#document{}], [lethink:insert_options()], #term{}) -> build_result().
  def insert(:term[type: :'TABLE'] = table, data, options) do
    args = [table] ++ lc d inlist data, do: expr(d)
    optargs = lc opt inlist options, do: insert_option_term(opt)
    :term.new(type: :'INSERT', args: args, optargs: optargs)
  end

  #%% @private
  #-spec insert_option_term(lethink:insert_options()) -> #term_assocpair{}.
  defp insert_option_term({:upsert, value}) do
    term_assocpair(atom_to_binary(:upsert, :utf8), value)
  end

  @spec get(binary | number, :term) :: build_result
  def get(:term[type: :'TABLE'] = table, key) when is_binary(key) or is_number(key) do
    :term.new(type: :'GET', args: [table | expr(key)])
  end

  @spec update(:term, HashDict | fun) :: build_result
  def update(selection, data) do
    :term.new(type: :'UPDATE', args: [selection | func_wrap(data)])
  end

  @spec row() :: build_result
  def row(), do: :term.new(type: :'IMPLICIT_VAR')

  @spec getattr(binary, :term) :: build_result
  def getattr(term, attr) do
    term.new(type: :'GETATTR', args: [term | expr(attr)])
  end

  #%% Math and Logic Operations

  @spec add(:term, number | binary) :: build_result
  def add(term, value) do
    :term.new(type: :'ADD', args: [term | expr(value)])
  end

  @spec sub(:term, number) :: build_result
  def sub(term, value) do
    :term.new(type: :'SUB', args: [term | expr(value)])
  end

  @spec mul(:term, number) :: build_result
  def mul(term, value) do
    :term.new(type: :'MUL', args: [term | expr(value)])
  end

  @spec div(:term, number) :: build_result
  def div(term, value) do
    :term.new(type: :'DIV', args: [term | expr(value)])
  end

  @spec mod(:term, number) :: build_result
  def mod(term, value) do
    :term.new(type: :'MOD', args: [term | expr(value)])
  end

  @spec and_(:term, number) :: build_result
  def and_(term, value) do
    :term.new(type: :'AND', args: [term | expr(value)])
  end

  @spec or_(:term, number) :: build_result
  def or_(term, value) do
    :term.new(type: :'OR', args: [term | expr(value)])
  end

  @spec eq(:term, number) :: build_result
  def eq(term, value) do
    :term.new(type: :'EQ', args: [term | expr(value)])
  end

  @spec ne(:term, number) :: build_result
  def ne(term, value) do
    :term.new(type: :'NE', args: [term | expr(value)])
  end

  @spec gt(:term, number) :: build_result
  def gt(term, value) do
    :term.new(type: :'GT', args: [term | expr(value)])
  end

  @spec ge(:term, number) :: build_result
  def ge(term, value) do
    :term.new(type: :'GE', args: [term | expr(value)])
  end

  @spec lt(:term, number) :: build_result
  def lt(term, value) do
    :term.new(type: :'LT', args: [term | expr(value)])
  end

  @spec le(:term, number) :: build_result
  def le(term, value) do
    :term.new(type: :'LE', args: [term | expr(value)])
  end

  @spec not_(:term) :: build_result
  def not_(term) do
    :term.new(type: :'NOT', args: [term])
  end

  #-spec expr(lethink:keyvalue() | lethink:json_term() | fun() | #term{} | #term_assocpair{}) -> #term{} | #term_assocpair{}.
  def expr(item) when is_record(item, :term), do: Item;
  def expr(item) when is_record(item, :term_assocpair), do: Item;
  def expr(doc) when is_record(doc, HashDict) do
    optargs = Enum.map(doc, function(expr/1))
    :term.new(type: :'MAKE_OBJ', optargs: optargs)
  end
  def expr({key, value}), do: term_assocpair(key, value)
  def expr(items) when is_list(items) do
    make_array(items)
  end
  def expr(func) when is_function(func), do: func(func)
  def expr(value), do: :term.new(type: :'DATUM', datum: datum(value))

  defp make_array(items) when is_list(items) do
    args = lc i inlist items, do: expr(i)
    :term.new(type: :'MAKE_ARRAY', args: args)
  end

  # @private
  # @doc create Datums from the four basic types.  Arrays and objects
  # are created via MAKE_ARRAY and MAKE_OBJ on the server since it's
  # cheaper that way.
  @spec datum(:null | boolean | number | binary) :: :datum
  defp datum(:null), do: :datum.new(type: :'R_NULL')
  defp datum(v) when is_boolean(v), do: :datum.new(type: :'R_BOOL', r_bool: v)
  defp datum(v) when is_number(v), do: :datum.new(type: :'R_NUM', r_num: v)
  defp datum(v) when is_binary(v), do: :datum.new(type: :'R_STR', r_str: v)

  @spec var(integer) :: :term
  def var(n), do: :term.new(type: :'VAR', args: expr(n))

  @spec func(fun) :: :term
  def func(func) do
      {_, arity} = :erlang.fun_info(func, :arity)
      arg_count_list = :lists.seq(1, arity)
      func_args = lc n inlist arg_count_list, do: var(n)
      :term.new(type: :'FUNC', args: [make_array(arg_count_list) | expr(apply(func, func_args))])
  end

  #-spec term_assocpair(binary(), any()) -> #term_assocpair{}.
  defp term_assocpair(key, value) do
    :term_assocpair.new(key: key, val: expr(value))
  end

  defp func_wrap(data) do
      value = expr(data)
      case ivar_scan(value) do
          true -> func(fn(_) -> value end)
          false -> value
      end
  end

  # Scan for IMPLICIT_VAR or JS
  @spec ivar_scan(any) :: boolean
  defp ivar_scan(:term[type: :'IMPLICIT_VAR']), do: :true
  defp ivar_scan(term) when is_record(term, :term) do
      case is_list(term.args) do
          true -> Enum.any?(term.args, function(ivar_scan/1))
          false -> ivar_scan(term.args)
      end
      or
      case is_list(term.optargs) do
          true -> Enum.any?(term.optargs, function(ivar_scan/1))
          false -> ivar_scan(term.args)
      end
  end
  defp ivar_scan(term_pair) when is_record(term_pair, :term_assocpair) do
      ivar_scan(term_pair)
  end
  defp ivar_scan(_), do: :false

end
