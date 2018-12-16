In memory cache on Mnesia and Erlang.

Instruction:

1. Go to the source directory of the app.

2. Build app with run `make`.

3. Run app with `make run`.

4. For creating table and some test data - run `mnesia_cache_app:init_db().`.

5. Api:
    - `mnesia_cache_app:get(key1).` -> `{ok, value1}`
    - `mnesia_cache_app:set(key1, val1).` -> Add Key-Value pair for 60 seconds by default.
    - `mnesia_cache_app:set(key1, val1, [{ttl, <time_in_seconds>}]).` -> 
        Add Key-Value pair for `<time_in_seconds>` seconds.`
    - `mnesia_cache_app:set(key1, val1, [{ttl, 0|infinity}]).` -> adds pair which should not be deleted.