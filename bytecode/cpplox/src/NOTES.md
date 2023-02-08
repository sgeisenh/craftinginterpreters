Some notes on progress in Crafting Interpreters part 2.

# Current Stopping Place

Finished the book.

Some outstanding bugs:

- Produces different bytecode for the `method_call` benchmark.
- Fails the `method_call` benchmark.

Figure out a better overall approach for hash tables.
Cache `ObjString *` hash values.
