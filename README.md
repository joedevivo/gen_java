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

For reference, the JInterface package reference can be found
[here](http://www.erlang.org/doc/apps/jinterface/java/com/ericsson/otp/erlang/package-summary.html)


### Maven

TODO: How to pull in the maven dependency to your project.

You can't, we're not published anywhere yet

## Erlang

**Subject to change**

I'm going to try and make this as painless for the user as possible. I
think we can do better than this, but for now it works as follows:

1. Start gen_java with `gen_java:start_link("/path/to/jar", "actual-jar-file.jar")`

2. to call your java function: `gen_java:call('com.whatever.package.Class', 'methodName', ['arg', <<"other arg">>])`

Your Erlang wrapper to the java function will have the same dialyzer return type as `rpc:call/4`
TODO: Provide an example of this

easy peasy
