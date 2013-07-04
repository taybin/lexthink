defimpl Access, for: :term do
  @doc """
  Access the given field in a row
  """

  def access(term, field) do
    Lexthink.AST.get_field(term, field)
  end
end
