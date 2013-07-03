defimpl Access, for: :term do
  @doc """
  Access the given key in a row
  """

  def access(term, attr) do
    Lexthink.AST.getattr(term, attr)
  end
end
