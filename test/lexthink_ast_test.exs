Code.require_file "test_helper.exs", __DIR__

defmodule LexthinkTest.AST do
  import Lexthink.AST
  use ExUnit.Case

  test "the truth" do
    assert(true)
  end

  test "db" do
    assert is_record(db("test"), :term)
  end

  test "table" do
    assert is_record(table("table"), :term)
    assert is_record(table("table", Keyword.new), :term)
    assert is_record(table(:term.new, "table"), :term)
    assert is_record(table(:term.new, "table", Keyword.new), :term)
  end

#func_test() ->
#    ?assertMatch(#term{}, lethink_ast:func(fun(_N) -> 1 end)).
#
#insert_test() ->
#    Doc = #document{ keyvalues = [{<<"a">>, 1}]},
#    ?assertMatch({error, _}, lethink_ast:insert(Doc, #term{ type = 'TABLE' })),
#    #term{ type = 'INSERT' } = lethink_ast:insert([Doc], #term{ type = 'TABLE' }).
#
#is_json_test() ->
#    true = lethink_ast:is_json(null),
#    true = lethink_ast:is_json(1),
#    true = lethink_ast:is_json(1.1),
#    true = lethink_ast:is_json(<<"test">>),
#    true = lethink_ast:is_json({<<"key">>, false}),
#    true = lethink_ast:is_json(#document{}),
#    false = lethink_ast:is_json({key, false}),
#    false = lethink_ast:is_json({[]}),
#    false = lethink_ast:is_json({row}),
#    false = lethink_ast:is_json({<<"test">>}).
#
#ivar_scan_test() ->
#    ImpVarTerm = #term{ type = 'IMPLICIT_VAR' },
#    true = lethink_ast:ivar_scan(ImpVarTerm),
#    true = lethink_ast:ivar_scan(#term{ args = ImpVarTerm}),
#    true = lethink_ast:ivar_scan(#term{ args = [ImpVarTerm]}),
#    false = lethink_ast:ivar_scan(true).
#
#expr_test() ->
#    #term{ type = 'MAKE_OBJ' } = lethink_ast:expr(#document{ keyvalues = [{<<"a">>, 1}]}),
#    #term{ type = 'DATUM' } = lethink_ast:expr(5),
#    #term{ type = 'FUNC' } = lethink_ast:expr(fun(X) -> [X] end).
#
#func_wrap_test() ->
#    #term{ type = 'MAKE_ARRAY' } = lethink_ast:func_wrap([1,2,3]),
#    #term{ type = 'DATUM' } = lethink_ast:func_wrap(<<"test">>),
#    #term{ type = 'FUNC' } = lethink_ast:func_wrap([{row}]),
#    #term{ type = 'FUNC' } = lethink_ast:func_wrap(fun(X) -> [X] end).
end
