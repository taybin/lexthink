Lexthink
=======

An elixir driver for [rethinkdb](http://rethinkdb.com).

Use
---

```elixir
> # Start service and set connect options
> Lexthink.start()
> import Lexthink.AST
> Lexthink.add_pool(:my_db_pool, 5, address: "localhost", port: 28015, database: "test")
>
> # Database options
> db_create("superheroes") |> Lexthink.run(:my_db_pool)
> db_list |> Lexthink.run(:my_db_pool)
> db_drop |> Lexthink.run(:my_db_pool)
>
> # Table options
> db("superheroes") |> table_create("marvel") |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_create("marvel", :primary_key, "name") |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_list |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_drop("marvel") |> Lexthink.run(:my_db_pool)
>
> # Set default pool database
> Lexthink.use(:my_db_pool, "superheroes")
>
> # Data
> h = [HashDict.new({:id, 5}, {:name, "batman"}, {"rich", :true}, {:cars, [1,2,3]}),
       HashDict.new({"id", 6}, {"name", "robin"}, {:rich, :false}, {"cars", :null})]
> table("marvel") |> insert(h) |> Lexthink.run(:my_db_pool)
> table("marvel") |> get(5) |> Lexthink.run(:my_db_pool)
> table("marvel") |> update(HashDict.new(age: row["age"] |> add(1))) |> Lexthink.run(:my_db_pool)
> table("marvel") |> update(fn x -> x[:age] |> add(1) end) |> Lexthink.run(:my_db_pool)
> table("dc") |> index_create("parental_planets", fn hero -> [hero[:mother], hero[:father]] end) |> Lexthink.run(:my_db_pool)
```
