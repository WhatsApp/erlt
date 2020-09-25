# Record to Struct parse transform

Issues that have come up in recent discussions is migrating old erl1
libraries to erlT and interfacing erl1 and erlT. One problem with this
is that typically on the erl1 side records will be used while on the
erlT side structs will be used. Structs will most likely be
implemented using maps while erl1 records are just tagged tuples.

It is of course possible in a FFI interface to convert the structs <->
records by copying. While this may be an acceptable solution during
the development of Modern Erlang it is probably not such a good
solution in production systems.

## The Parse Transform

The parse transforms uses the record definitions and when you do
something with record, create/read/update, it transforms them to
operations on the corresponding map operations. It assumes the field
names are the same in the struct and reocrd, I don't know if this will
be problematic.

It is quite easy for the author of the library to set up for the erl1
side, just fix an .hrl with the record definitions and teelling the
compiler to use the parse transform. You can trol which records you
won't to convert.

Pro:

* Easy to set up for the library, basically one include file.  Makes

* The data look "right" both ways, the erlT sees structs while the
  erl1 side sees records, at least to work with.

Con:

* While it hides the structs in erl1 by using records when you
  actually look at the data you see the maps. This could be confusing.

## Code

There are two files, the parse transform record_to_struct.er and rtos.erl which is a simple example with some code showing the basic operations. If compile rtos.erl with the 'P' options you can the erlang code it generates.

It should be handle a lot of code  but it is still very much a POC.
