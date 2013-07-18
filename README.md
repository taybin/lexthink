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
> db_list |> Lexthink.run(:my_db_pool)
> db_drop |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_create("marvel") |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_create("marvel", :primary_key, "name") |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_list |> Lexthink.run(:my_db_pool)
> db("superheroes") |> table_drop("marvel") |> Lexthink.run(:my_db_pool)
> Lexthink.use(:my_db_pool, "superheroes")
> h = [HashDict.new({:id, 5}, {:name, "batman"}, {"rich", :true}, {:cars, [1,2,3]}),
       HashDict.new({"id", 6}, {"name", "robin"}, {:rich, :false}, {"cars", :null})]
> table("marvel") |> insert(h) |> Lexthink.run(:my_db_pool)
> table("marvel") |> get(5) |> Lexthink.run(:my_db_pool)
```
