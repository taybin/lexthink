defmodule Ql2.Util do

  @spec datum_value(:datum) :: Lexthink.json_term
  def datum_value(:datum[ type: :'R_NULL' ]) do
    :null
  end
  def datum_value(:datum[ type: :'R_BOOL', r_bool: bool ]) do
    bool
  end
  def datum_value(:datum[ type: :'R_NUM', r_num: num ]) do
    num
  end
  def datum_value(:datum[ type: :'R_STR', r_str: str ]) do
    list_to_binary(str)
  end
  def datum_value(:datum[ type: :'R_ARRAY', r_array: array ]) do
    lc d inlist array, do: datum_value(d)
  end
  def datum_value(:datum[ type: :'R_OBJECT', r_object: object ]) do
    keyvalues = lc o inlist object, do: datum_assocpair_tuple(o)
    HashDict.new(keyvalues)
  end

  @spec global_db(binary) :: :query_assocpair
  def global_db(value) do
    :query_assocpair.new(key: "db",
                         val: :term.new(type: :'DB', args: Lexthink.AST.expr(value)))
  end

  @spec datum_assocpair_tuple(:datum_assocpair) :: {:binary, :any}
  defp datum_assocpair_tuple(datum_assocpair) do
      {list_to_binary(datum_assocpair.key), datum_value(datum_assocpair.val)}
  end
end
