Lexthink
=======

An elixir driver for [rethinkdb](http://rethinkdb.com).

Use
---

```
> Lexthink.start()
> import Lexthink.AST
> Lexthink.add_pool(:my_db_pool, 5, :address, "localhost", :port, 28015, :database, "test")
> db_create("superheroes") |> Lexthink.run(:my_db_pool)
> db_list
> db_drop
> db("superheroes") |> table_create("marvel")
> db("superheroes") |> table_create("marvel", :primary_key, "name")
> db("superheroes") |> table_list
> db("superheroes") |> table_drop("marvel")
> Lexthink.use(:my_db_pool, "superheroes")
> h = [HashDict.new({:id, 5}, {:name, "batman"}, {"rich", :true}, {:cars, [1,2,3]}),
       HashDict.new({"id", 6}, {"name", "robin"}, {:rich, :false}, {"cars", :null})]
> table("marvel") |> insert(h)
> table("marvel") |> get(5)
> table("marvel") |> update(HashDict.new(age: row["age"] |> add(1)))
> table("marvel") |> update(fn x -> x[:age] |> add(1) end)
> table("dc") |> index_create("parental_planets", fn hero -> [hero[:mother], hero[:father]] end)
```
