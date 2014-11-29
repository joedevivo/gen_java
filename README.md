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

### Configure gen_java in your sys.config

```erlang
{gen_java, [
    {modules, [
        {my_module, [
            {jar, "/path/to/jar/file.jar"}
        ]}
    ]}
]}
```

### Create your `my_module`

```erlang
-module(my_module).

-compile({parse_transform, gen_java_parse_transform}).
```

### Start your gen_java server

#### On your own

Start gen_java with `my_module:start_link()` or `my_module:start()`

#### Or from a supervisor

```erlang
{my_module,
    {my_module, start_link, []},
    permanent, 5000, worker, [my_module]},
```

#### Call Java Methods!

`my_module:call('com.whatever.package.Class', 'methodName', ['arg', <<"other arg">>])` will return the value you want!

#### Optional: Add a convenience wrapper function

```erlang
method_name(Atom, Binary) ->
    call('com.whatever.package.Class', 'methodName', [Atom, Binary]).
```

`my_module:call/3` will have the same dialyzer return type as `rpc:call/4`
TODO: Provide an example of this

easy peasy

### Provided Java Functions:

```erlang
%% @doc java.lang.System.getProperites()
-spec system_properties() -> [{atom(), binary()}].
java:system_properties/0

%% @doc java.lang.System.getenv()
-spec system_env() -> [{binary(), binary()}].
java:system_env/0

%% @doc command line $JAVA_OPTS. (e.g. -Xmx512m)
-spec input_arguments() -> binary().
java:input_arguments/0

```