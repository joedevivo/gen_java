package com.devivo.gen_java;

import com.ericsson.otp.erlang.*;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class ErlangServer {
    public static void main(String[] stringArgs) throws Exception {
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

        OtpNode self = new OtpNode(nodename, cookie);
        System.out.println("Started node: " + self.node());
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
                    System.exit(0);
                } else {
                    System.out.println("Unexpected atom received by rex server: " + atom);
                }
            // If it's not, we'll let the contructor of ErlangRemoteProcedureCallMessage
            // do all the dirty work
            } else {
                ErlangRemoteProcedureCallMessage msg = null;

                try {
                    msg = new ErlangRemoteProcedureCallMessage(o);
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
                        OtpErlangPid pid = (OtpErlangPid)(msg.getMFA().getArgs().elementAt(0));
                        try {
                            rex.link(pid);
                            msg.send(rex, new OtpErlangAtom(true));
                        } catch (OtpErlangExit oee) {
                            System.out.println("erlang:link/1 failed: " + oee.reason());
                            msg.send(rex, oee.reason());
                        }
                    } else if(msg.getMFA().match("erlang", "node", 0)) {
                        // erlang:node/0 is a special and required case
                        msg.send(rex, new OtpErlangAtom(nodename));
                    } else if(RPCCache.containsKey(msg.getMFA().getKey())) {
                        // This is where we check the cache
                        Method m = RPCCache.get(msg.getMFA().getKey());
                        msg.send(rex, (OtpErlangObject) m.invoke(null, msg.getMFA().getArgs().elements()));
                    } else {
                        // This means it's not in the cache, we should try and find it
                        // and add it.
                        Method m = find(msg.getMFA().getKey());
                        if (m != null) {
                            RPCCache.put(msg.getMFA().getKey(), m);
                            msg.send(rex, (OtpErlangObject) m.invoke(null, msg.getMFA().getArgs().elements()));
                        } else {
                            System.out.println("Bad RPC: " + msg.getMFA().getKey().toString());
                            // we couldn't add it, be nice and send a badrpc error back
                            msg.send(rex, msg.toErlangBadRPC());
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
}
