defmodule Lexthink.AST do
  @typep datum_arg :: :null | boolean | number | binary
  @typep expr_arg :: Dict.t | {any, any} | [expr_arg] | fun | atom | :term.t | :term_assocpair.t | datum_arg
  @typep key_arg :: binary | number | atom

  @spec db_create(binary) :: :term.t
  def db_create(name), do: :term.new(type: :'DB_CREATE', args: expr(name))

  @spec db_drop(binary) :: :term.t
  def db_drop(name), do: :term.new(type: :'DB_DROP', args: expr(name))

  @spec db_list() :: :term.t
  def db_list(), do: :term.new(type: :'DB_LIST')

  @spec table_create(binary) :: :term.t
  def table_create(name) when is_binary(name) do
    table_create(name, [])
  end

  @spec table_create(binary | :term.t, Keyword.t | binary) :: :term.t
  def table_create(name, options) when is_binary(name) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE_CREATE', args: expr(name), optargs: optargs)
  end

  def table_create(db, name) when is_binary(name) do
    table_create(db, name, [])
  end

  @spec table_create(:term.t, binary, Keyword.t) :: :term.t
  def table_create(db, name, options) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE_CREATE', args: [db, expr(name)], optargs: optargs)
  end

  @spec table_drop(binary) :: :term.t
  def table_drop(name) do
    :term.new(type: :'TABLE_DROP', args: expr(name))
  end

  @spec table_drop(:term.t, binary) :: :term.t
  def table_drop(db, name) do
    :term.new(type: :'TABLE_DROP', args: [db, expr(name)])
  end

  @spec table_list() :: :term.t
  def table_list(), do: :term.new(type: :'TABLE_LIST')

  @spec table_list(:term.t) :: :term.t
  def table_list(db) do
    :term.new(type: :'TABLE_LIST', args: db)
  end

  #%% @doc Specify a DB.  Must be first operation in query list
  #%% Optional if a default database has been specified via
  #%% @see lethink:use/2
  @spec db(binary) :: :term.t
  def db(name), do: :term.new(type: :'DB', args: expr(name))

  @spec table(binary) :: :term.t
  def table(name), do: table(name, [])

  @spec table(:term.t | binary, binary | Keyword.t) :: :term.t
  def table(db, name) when is_record(db, :term), do: table(db, name, [])

  def table(name, options) when is_binary(name) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE', args: expr(name), optargs: optargs)
  end

  @spec table(:term.t, binary, Keyword.t) :: :term.t
  def table(db, name, options) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE', args: [db, expr(name)], optargs: optargs)
  end

  @spec insert(:term.t, Dict.t | [Dict.t]) :: :term.t
  def insert(table, data) when is_record(table, :term) do
    insert(table, data, [])
  end

  @spec insert(:term.t, Dict.t | [Dict.t], Keyword.t) :: :term.t
  def insert(table, data, options) when is_record(table, :term) do
    args = [table, expr(data)]
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'INSERT', args: args, optargs: optargs)
  end

  @spec get(:term.t, key_arg) :: :term.t
  def get(table, key) when is_record(table, :term) and (is_binary(key) or is_number(key)) do
    args = [table, expr(key)]
    :term.new(type: :'GET', args: args)
  end

  @spec get_all(:term.t, key_arg, Keyword.t) :: :term.t
  def get_all(table, key, options // []) when is_record(table, :term) do
    args = [table, expr(key)]
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'GET_ALL', args: args, optargs: optargs)
  end

  @spec between(:term.t, key_arg, key_arg, Keyword.t) :: :term.t
  def between(:term[] = selection, lower_key, upper_key, options // []) do
    args = [selection, expr(lower_key), expr(upper_key)]
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'BETWEEN', args: args, optargs: optargs)
  end

  @spec update(:term.t, Dict.t | fun, Keyword.t) :: :term.t
  def update(selection, data, options // []) do
    args = [selection, func_wrap(data)]
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'UPDATE', args: args, optargs: optargs)
  end

  @spec replace(:term.t, Dict.t | fun, Keyword.t) :: :term.t
  def replace(selection, data, options // []) do
    args = [selection, func_wrap(data)]
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'REPLACE', args: args, optargs: optargs)
  end

  @spec delete(:term.t, Keyword.t) :: :term.t
  def delete(selection, options // []) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'DELETE', optargs: optargs)
  end

  @spec row() :: :term.t
  def row(), do: :term.new(type: :'IMPLICIT_VAR')

  @spec getattr(:term.t, binary) :: :term.t
  def getattr(term, attr) do
    args = [term, expr(attr)]
    :term.new(type: :'GETATTR', args: args)
  end

  #%% Math and Logic Operations

  @spec add(:term.t, number | binary) :: :term.t
  def add(term, value) do
    :term.new(type: :'ADD', args: [term, expr(value)])
  end

  @spec sub(:term.t, number) :: :term.t
  def sub(term, value) do
    :term.new(type: :'SUB', args: [term, expr(value)])
  end

  @spec mul(:term.t, number) :: :term.t
  def mul(term, value) do
    :term.new(type: :'MUL', args: [term, expr(value)])
  end

  @spec div(:term.t, number) :: :term.t
  def div(term, value) do
    :term.new(type: :'DIV', args: [term, expr(value)])
  end

  @spec mod(:term.t, number) :: :term.t
  def mod(term, value) do
    :term.new(type: :'MOD', args: [term, expr(value)])
  end

  @spec and_(:term.t, number) :: :term.t
  def and_(term, value) do
    :term.new(type: :'AND', args: [term, expr(value)])
  end

  @spec or_(:term.t, number) :: :term.t
  def or_(term, value) do
    :term.new(type: :'OR', args: [term, expr(value)])
  end

  @spec eq(:term.t, number) :: :term.t
  def eq(term, value) do
    :term.new(type: :'EQ', args: [term, expr(value)])
  end

  @spec ne(:term.t, number) :: :term.t
  def ne(term, value) do
    :term.new(type: :'NE', args: [term, expr(value)])
  end

  @spec gt(:term.t, number) :: :term.t
  def gt(term, value) do
    :term.new(type: :'GT', args: [term, expr(value)])
  end

  @spec ge(:term.t, number) :: :term.t
  def ge(term, value) do
    :term.new(type: :'GE', args: [term, expr(value)])
  end

  @spec lt(:term.t, number) :: :term.t
  def lt(term, value) do
    :term.new(type: :'LT', args: [term, expr(value)])
  end

  @spec le(:term.t, number) :: :term.t
  def le(term, value) do
    :term.new(type: :'LE', args: [term, expr(value)])
  end

  @spec not_(:term.t) :: :term.t
  def not_(term) do
    :term.new(type: :'NOT', args: [term])
  end

  @spec expr(expr_arg) :: :term.t | :term_assocpair.t
  def expr(item) when is_record(item, :term), do: item
  def expr(item) when is_record(item, :term_assocpair), do: item
  def expr(doc) when is_record(doc, HashDict) do
    optargs = Enum.map(doc, expr(&1))
    :term.new(type: :'MAKE_OBJ', optargs: optargs)
  end
  def expr({key, value}), do: build_term_assocpair(key, value)
  def expr(items) when is_list(items) do
    make_array(items)
  end
  def expr(f) when is_function(f), do: func(f)
  def expr(a) when is_atom(a), do: expr(atom_to_binary(a))
  def expr(value), do: :term.new(type: :'DATUM', datum: datum(value))

  @spec make_array([expr_arg]) :: :term.t
  def make_array(items) when is_list(items) do
    args = lc i inlist items, do: expr(i)
    :term.new(type: :'MAKE_ARRAY', args: args)
  end

  # @private
  # @doc create Datums from the four basic types.  Arrays and objects
  # are created via MAKE_ARRAY and MAKE_OBJ on the server since it's
  # cheaper that way.
  @spec datum(datum_arg) :: :datum.t
  defp datum(:null), do: :datum.new(type: :'R_NULL')
  defp datum(v) when is_boolean(v), do: :datum.new(type: :'R_BOOL', r_bool: v)
  defp datum(v) when is_number(v), do: :datum.new(type: :'R_NUM', r_num: v)
  defp datum(v) when is_binary(v), do: :datum.new(type: :'R_STR', r_str: v)

  @spec var(integer) :: :term.t
  def var(n), do: :term.new(type: :'VAR', args: expr(n))

  @spec func(fun) :: :term.t
  def func(func) do
      {_, arity} = :erlang.fun_info(func, :arity)
      arg_count_list = :lists.seq(1, arity)
      func_args = lc n inlist arg_count_list, do: var(n)
      args = [make_array(arg_count_list), expr(apply(func, func_args))]
      :term.new(type: :'FUNC', args: args)
  end

  @private
  @spec func_wrap(expr_arg) :: :term.t | :term_assocpair.t
  defp func_wrap(data) do
    value = expr(data)
    case ivar_scan(value) do
      true -> func(fn(_) -> value end)
      false -> value
    end
  end

  # Scan for IMPLICIT_VAR or JS
  @private
  @spec ivar_scan(:term.t | :term_assocpair.t | [:term.t] | [:term_assocpair.t]) :: boolean
  defp ivar_scan(:term[type: :'IMPLICIT_VAR']), do: :true
  defp ivar_scan(list) when is_list(list), do: Enum.any?(list, ivar_scan(&1))
  defp ivar_scan(term) when is_record(term, :term) do
    ivar_scan(term.args) or ivar_scan(term.optargs)
  end
  defp ivar_scan(term_pair) when is_record(term_pair, :term_assocpair) do
    ivar_scan(term_pair.val)
  end
  defp ivar_scan(_), do: :false

  @private
  @spec build_term_assocpair(binary | atom, expr_arg) :: :term_assocpair.t
  defp build_term_assocpair(key, value) when is_atom(key) do
    build_term_assocpair(atom_to_binary(key, :utf8), value)
  end
  defp build_term_assocpair(key, value) when is_binary(key) do
    :term_assocpair.new(key: key, val: expr(value))
  end

  @private
  @spec option_term({atom | binary, atom | binary}) :: :term_assocpair.t
  defp option_term({key, value}) when is_atom(value) do
    option_term({key, atom_to_binary(value, :utf8)})
  end
  defp option_term({key, value}) when is_atom(key) do
    option_term({atom_to_binary(key, :utf8), value})
  end
  defp option_term({key, value}) do
    build_term_assocpair(key, value)
  end
end
