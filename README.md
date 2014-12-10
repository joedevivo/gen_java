gen_java
========

### A library for Erlang/Java Interoperability via Erlang's rpc module

This project generates two artifacts:
* a .jar file that includes all you need to build your own java project that can respond to erlang's `rpc:call/4` function
* an erlang dependency that provides a `gen_java` server that you can implement via parse transform.

## Java Face

Here's what you need to know on the java side of things

If you want to expose a java method to erlang, it needs to be public,
static and have argument and return types from the OTP JInterface
library.

For reference, the JInterface package reference can be found
[here](http://www.erlang.org/doc/apps/jinterface/java/com/ericsson/otp/erlang/package-summary.html)


### Maven

You should be able to pull from Maven Central pretty soon with this:

```xml
<dependency>
    <groupId>com.devivo</groupId>
    <artifactId>gen_java</artifactId>
    <version>0.1.0</version>
</dependency>
```

## Erlang

### Configure gen_java in your sys.config

```erlang
{gen_java, [
    {modules, [
        {my_module, [
            {jar, "/path/to/jar/file.jar"},
            %% Default thread count is 10.
            %% this is how big the java thread pool is
            {thread_count, 10}
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

Start gen_java with `my_module:start_link()` or `my_module:start()`. Those functions are free with the parse transform, so don't worry about it.

#### Or from a supervisor

```erlang
{my_module,
    {my_module, start_link, []},
    permanent, 5000, worker, [my_module]},
```

#### Optional: Init Hollaback

Don't call it a callback, 'cause it's not a behaviour. You can add an
`init/1` hollaback to your module to do some initialization of your
JVM node. The only trick is that you have to use `rpc:call/4` since
the `gen_server` isn't started yet.

```erlang
-spec init(atom()) -> ok.
init(Nodename) ->
    rpc:call(Nodename, 'com.yourcompany.package', 'init', [<<SomeState>>]).
```

#### Call Java Methods!

`my_module:call('com.whatever.package.Class', 'methodName', ['arg', <<"other arg">>])` will return the value you want!

Worried about waiting forever? There's a `call/4` that allows you to specify a timeout.

#### Optional: Add a convenience wrapper function

```erlang
-spec method_name(atom(), binary()) -> my_return_type() | gen_java:badrpc().
method_name(Atom, Binary) ->
    call('com.whatever.package.Class', 'methodName', [Atom, Binary]).
```

`my_module:call/3` will have the same dialyzer return type as `rpc:call/4`

For fun, We provide `gen_java:badrpc()` for you to use in addition to
your expected return type. This will let you just append ` |
gen_java:badrpc()` to your spec and then fuggedaboudit.


easy peasy

### Provided Java Functions:

```erlang
%% @doc java.lang.System.getProperties()
-spec system_properties() -> [{atom(), binary()}].
java:system_properties/0

%% @doc java.lang.System.getenv()
-spec system_env() -> [{binary(), binary()}].
java:system_env/0

%% @doc command line $JAVA_OPTS. (e.g. -Xmx512m)
-spec input_arguments() -> binary().
java:input_arguments/0

```
