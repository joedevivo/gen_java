package com.devivo.gen_java;

import com.ericsson.otp.erlang.*;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ErlangServer {
    public static void main(String[] stringArgs) throws Exception {
        int threadPoolSize = 10;

        // TODO: better argument parsing?
        String nodename = stringArgs[0];
        String cookie = stringArgs[1];

        System.out.println("Starting OTP Node '" + nodename + "' with cookie " + cookie);

        // RPC Cache
        Map<ErlangFunctionCacheKey, Method> RPCCache = new HashMap<ErlangFunctionCacheKey, Method>();

        // The Key needs to be an Erlang MFA with the A being classes, not objects
        // because it's not a particular call, but all calls

        // `erlang:link/1` and `erlang:node/0` are special cases because they require understanding of our OtpMBox.
        // so these lines below aren't really possible
        // RPCCache.put(new ErlangFunctionCacheKey("erlang", "link", OtpErlangPid.class), null);
        // RPCCache.put(new ErlangFunctionCacheKey("erlang", "node"), null);

        // Let's add our example `erlang:abs/1` to the function cache manually
        // we have to add it twice, once per each argument signature
        RPCCache.put(
                new ErlangFunctionCacheKey("erlang", "abs", OtpErlangDouble.class),
                Erlang.class.getMethod("abs", OtpErlangDouble.class));
        RPCCache.put(
                new ErlangFunctionCacheKey("erlang", "abs", OtpErlangLong.class),
                Erlang.class.getMethod("abs", OtpErlangLong.class));

        // wrapper for java.util.System.getProperties()
        RPCCache.put(
                new ErlangFunctionCacheKey("java", "system_properties"),
                Java.class.getMethod("system_properties"));

        RPCCache.put(
                new ErlangFunctionCacheKey("java", "system_env"),
                Java.class.getMethod("system_env"));

        RPCCache.put(
                new ErlangFunctionCacheKey("java", "input_arguments"),
                Java.class.getMethod("input_arguments"));

        OtpNode self = new OtpNode(nodename, cookie);
        System.out.println("Started node: " + self.node());

        // Thread Pool Size
        // It's important that the first two output lines are about the node starting, which is why this is here
        String size = stringArgs[2];
        try {
            threadPoolSize = Integer.parseInt(size);
        } catch (NumberFormatException e) {
            System.out.println("ThreadSize '" + size + "' could not be parsed into an integer, using default");
        }

        ExecutorService pool = Executors.newFixedThreadPool(threadPoolSize);

        boolean keepGoing = true;

        // rex is the erlang "Remote EXecution server"
        OtpMbox rex = self.createMbox("rex");
        while(keepGoing) {
            // rex.receive is a blocking call, so just hang out here until one shows up
            OtpErlangObject o = rex.receive();

            // Process the incoming message.
            // If it's the OtpErlangAtom 'stop', we'll kill the loop
            if (o instanceof OtpErlangAtom) {
                OtpErlangAtom a = (OtpErlangAtom)o;
                String atom = a.atomValue();
                if (atom.equals("stop")) {
                    keepGoing = false;
                    System.out.println("Rex received 'stop' message, shutting down.");

                    // Let's clean up gracefully
                    rex.exit("Rex received 'stop' message, shutting down.");
                    self.closeMbox(rex);
                    self.close();
                    System.exit(0);
                } else {
                    System.out.println("Unexpected atom received by rex server: " + atom);
                }
            // If it's not, we'll let the contructor of ErlangRemoteProcedureCallMessage
            // do all the dirty work
            } else {
                ErlangRemoteProcedureCallMessage msg = null;

                try {
                    msg = new ErlangRemoteProcedureCallMessage(rex, o);
                } catch (ErlangRemoteException erlE) {
                    erlE.send(rex);
                } catch (Exception e) {
                    // System.out.println is picked up by the erlang gen_server
                    // that is monitoring this java process

                    // Unfortunately for us, if this exception is thrown, we don't have enough
                    // information about where the rpc:call came from, so we'll just log this and
                    // rely on rpc:call's timeout arg to eventually return a timeout error

                    // However, if it were a legit rpc:call, the above message would have
                    // processed correctly.
                    System.out.println("Rex received '"
                        + o.toString()
                        + "' but didn't know how to process it. Exception: "
                        + e.getMessage());
                    // We should not continue processing
                }

                // Now if msg == null, we've got a bad message and should just give up!
                // Only well structured messages can even be responeded to
                if(msg != null) {
                    if(msg.getMFA().match("erlang", "link", 1)) {
                        // erlang:link/1 is a special and required case.
                        // It should only happen once, so let's not get all worried about
                        // sending it to the thread pool
                        OtpErlangPid pid = (OtpErlangPid)(msg.getMFA().getArgs().elementAt(0));
                        try {
                            rex.link(pid);
                            msg.send(new OtpErlangAtom(true));
                        } catch (OtpErlangExit oee) {
                            System.out.println("erlang:link/1 failed: " + oee.reason());
                            msg.send(oee.reason());
                        }
                    } else if(msg.getMFA().match("erlang", "node", 0)) {
                        // erlang:node/0 is a special and required case
                        msg.send(new OtpErlangAtom(nodename));
                    } else if(RPCCache.containsKey(msg.getMFA().getKey())) {
                        // This is where we check the cache
                        Method m = RPCCache.get(msg.getMFA().getKey());
                        msg.setMethod(m);

                        pool.execute(msg);

                    } else {
                        // This means it's not in the cache, we should try and find it
                        // and add it.
                        Method m = find(msg.getMFA().getKey());
                        if (m != null) {
                            RPCCache.put(msg.getMFA().getKey(), m);
                            msg.setMethod(m);
                            pool.execute(msg);
                        } else {
                            System.out.println("Bad RPC: " + msg.getMFA().getKey().toString());
                            // we couldn't add it, be nice and send a badrpc error back
                            msg.send(msg.toErlangBadRPC());
                        }
                    }
                }
            }
        }
        System.exit(0);
    }

    /**
     * Looks up method for the cache. Assumes key.getModule() is a loaded java class.
     * @param key
     * @return
     */
    public static Method find(ErlangFunctionCacheKey key) {
        try {
            Class c = Class.forName(key.getModule());
            return c.getDeclaredMethod(key.getFunction(), key.getArguments());
        } catch (ClassNotFoundException cnfe) {
            return null;
        } catch (NoSuchMethodException nsme) {
            return null;
        } catch (SecurityException se) {
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    // For debugging if we need it
    public static void print(Map<ErlangFunctionCacheKey, Method> RPCCache) {
        Iterator<Map.Entry<ErlangFunctionCacheKey, Method>> it = RPCCache.entrySet().iterator();
        while(it.hasNext()) {
            Map.Entry<ErlangFunctionCacheKey, Method> i = it.next();
            System.out.println("Key hashCode: " + i.getKey().hashCode());
            System.out.println(
                    "Entry: { " + i.getKey().toString() + ", " + i.getValue().toString() + " }" );
        }
    }
    /*
    class Handler implements Runnable {
        private final ErlangRemoteProcedureCallMessage h_msg;
        private final OtpMbox h_rex;
        private final Method h_m;
        private final OtpErlangObject[] h_args;

        Handler(ErlangRemoteProcedureCallMessage msg, OtpMbox r, Method m, OtpErlangObject[] args) {
            this.h_msg = msg;
            this.h_rex = r;
            this.h_m = m;
            this.h_args = args;
        }

        public void run() {
            h_msg.send(h_rex, (OtpErlangObject) h_m.invoke(null, h_args));
        }
    }
    */
}
