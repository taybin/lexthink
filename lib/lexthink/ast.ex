defmodule Lexthink.AST do
  @typep json_term :: :null | boolean | number | binary | HashDict | [json_term]
  @typep ql_term :: tuple

  @spec db_create(binary) :: ql_term
  def db_create(name), do: :term.new(type: :'DB_CREATE', args: expr(name))

  @spec db_drop(binary) :: ql_term
  def db_drop(name), do: :term.new(type: :'DB_DROP', args: expr(name))

  @spec db_list() :: ql_term
  def db_list(), do: :term.new(type: :'DB_LIST')

  @spec table_create(binary) :: ql_term
  def table_create(name) when is_binary(name) do
    table_create(name, [])
  end

  @spec table_create(binary | ql_term, Keyword | binary) :: ql_term
  def table_create(name, options) when is_binary(name) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE_CREATE', args: expr(name), optargs: optargs)
  end

  def table_create(db, name) when is_binary(name) do
    table_create(db, name, [])
  end

  @spec table_create(ql_term, binary, Keyword) :: ql_term
  def table_create(:term = db, name, options) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE_CREATE', args: [db, expr(name)], optargs: optargs)
  end

  @spec table_drop(binary) :: ql_term
  def table_drop(name) do
    :term.new(type: :'TABLE_DROP', args: expr(name))
  end

  @spec table_drop(ql_term, binary) :: ql_term
  def table_drop(:term = db, name) do
    :term.new(type: :'TABLE_DROP', args: [db, expr(name)])
  end

  @spec table_list() :: ql_term
  def table_list(), do: :term.new(type: :'TABLE_LIST')

  @spec table_list(ql_term) :: ql_term
  def table_list(:term = db) do
    :term.new(type: :'TABLE_LIST', args: db)
  end

  #%% @doc Specify a DB.  Must be first operation in query list
  #%% Optional if a default database has been specified via
  #%% @see lethink:use/2
  @spec db(binary) :: ql_term
  def db(name), do: :term.new(type: :'DB', args: expr(name))

  @spec table(binary) :: ql_term
  def table(name), do: table(name, [])

  @spec table(ql_term | binary, binary | Keyword) :: ql_term
  def table(:term = db, name), do: table(db, name, [])

  def table(name, options) when is_binary(name) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE', args: expr(name), optargs: optargs)
  end

  @spec table(ql_term, binary, Keyword) :: ql_term
  def table(:term = db, name, options) do
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'TABLE', args: [db, expr(name)], optargs: optargs)
  end

  @spec insert(ql_term, HashDict, Keyword) :: ql_term
  def insert(:term = table, data, options // []) do
    args = [table, lc d inlist data, do: expr(d)]
    optargs = lc opt inlist options, do: option_term(opt)
    :term.new(type: :'INSERT', args: args, optargs: optargs)
  end

  @spec get(binary | number, ql_term) :: ql_term
  def get(:term = table, key) when is_binary(key) or is_number(key) do
    :term.new(type: :'GET', args: [table, expr(key)])
  end

  @spec update(ql_term, HashDict | fun) :: ql_term
  def update(selection, data) do
    :term.new(type: :'UPDATE', args: [selection, func_wrap(data)])
  end

  @spec row() :: ql_term
  def row(), do: :term.new(type: :'IMPLICIT_VAR')

  @spec getattr(ql_term, binary) :: ql_term
  def getattr(term, attr) do
    term.new(type: :'GETATTR', args: [term, expr(attr)])
  end

  #%% Math and Logic Operations

  @spec add(ql_term, number | binary) :: ql_term
  def add(term, value) do
    :term.new(type: :'ADD', args: [term, expr(value)])
  end

  @spec sub(ql_term, number) :: ql_term
  def sub(term, value) do
    :term.new(type: :'SUB', args: [term, expr(value)])
  end

  @spec mul(tuple, number) :: tuple
  def mul(:term = ql_term, value) do
    :term.new(type: :'MUL', args: [ql_term, expr(value)])
  end

  @spec div(ql_term, number) :: ql_term
  def div(term, value) do
    :term.new(type: :'DIV', args: [term, expr(value)])
  end

  @spec mod(ql_term, number) :: ql_term
  def mod(term, value) do
    :term.new(type: :'MOD', args: [term, expr(value)])
  end

  @spec and_(ql_term, number) :: ql_term
  def and_(term, value) do
    :term.new(type: :'AND', args: [term, expr(value)])
  end

  @spec or_(ql_term, number) :: ql_term
  def or_(term, value) do
    :term.new(type: :'OR', args: [term, expr(value)])
  end

  @spec eq(ql_term, number) :: ql_term
  def eq(term, value) do
    :term.new(type: :'EQ', args: [term, expr(value)])
  end

  @spec ne(ql_term, number) :: ql_term
  def ne(term, value) do
    :term.new(type: :'NE', args: [term, expr(value)])
  end

  @spec gt(ql_term, number) :: ql_term
  def gt(term, value) do
    :term.new(type: :'GT', args: [term, expr(value)])
  end

  @spec ge(ql_term, number) :: ql_term
  def ge(term, value) do
    :term.new(type: :'GE', args: [term, expr(value)])
  end

  @spec lt(ql_term, number) :: ql_term
  def lt(term, value) do
    :term.new(type: :'LT', args: [term, expr(value)])
  end

  @spec le(ql_term, number) :: ql_term
  def le(term, value) do
    :term.new(type: :'LE', args: [term, expr(value)])
  end

  @spec not_(ql_term) :: ql_term
  def not_(term) do
    :term.new(type: :'NOT', args: [term])
  end

  @spec expr(HashDict | {any, any} | list | fun | ql_term) :: ql_term
  def expr(item) when is_record(item, :term), do: Item
  def expr(item) when is_record(item, :term_assocpair), do: Item
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

  @spec make_array(list) :: ql_term
  def make_array(items) when is_list(items) do
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

  @spec var(integer) :: ql_term
  def var(n), do: :term.new(type: :'VAR', args: expr(n))

  @spec func(fun) :: ql_term
  def func(func) do
      {_, arity} = :erlang.fun_info(func, :arity)
      arg_count_list = :lists.seq(1, arity)
      func_args = lc n inlist arg_count_list, do: var(n)
      args = [make_array(arg_count_list), expr(apply(func, func_args))]
      :term.new(type: :'FUNC', args: args)
  end

  @private
  defp func_wrap(data) do
      value = expr(data)
      case ivar_scan(value) do
          true -> func(fn(_) -> value end)
          false -> value
      end
  end

  # Scan for IMPLICIT_VAR or JS
  @private
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

  @private
  @spec term_assocpair(binary | atom, any) :: :term_assocpair
  defp term_assocpair(key, value) when is_atom(key) do
    term_assocpair(atom_to_binary(key, :utf8), value)
  end
  defp term_assocpair(key, value) when is_binary(key) do
    :term_assocpair.new(key: key, val: expr(value))
  end

  @private
  @spec option_term({atom | binary, atom | binary}) :: :term_assocpair
  defp option_term({key, value}) when is_atom(value) do
    option_term({key, atom_to_binary(value, :utf8)})
  end
  defp option_term({key, value}) when is_atom(key) do
    option_term({atom_to_binary(key, :utf8), value})
  end
  defp option_term({key, value}) when is_binary(key) and is_binary(value) do
    term_assocpair(key, value)
  end
end
