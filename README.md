gen_java
========

### A library for Erlang/Java Interoperability via Erlang's rpc module

This project generates two artifacts:
* a .jar file that includes all you need to build your own java project that can respond to erlang's `rpc:call/4` function
* an erlang dependency that provides a `gen_java` behaviour that you can implement.

TODO: not a behavior yet, just a gen server.

## Java Face

Here's what you need to know on the java side of things

If you want to expose a java method to erlang, it needs to be public,
static and have argument and return types from the OTP JInterface
library.


### Maven

TODO: How to pull in the maven dependency to your project.

You can't, we're not published anywhere yet

## Erlang

**Subject to change**

Right now you just start gen_java with `gen_java:start_link("/path/to/jar", "actual-jar-file.jar")`

Then, when you want to call your java function: `gen_java:call('com.whatever.package.Class', 'methodName', ['arg', <<"other arg">>])`

easy peasy
