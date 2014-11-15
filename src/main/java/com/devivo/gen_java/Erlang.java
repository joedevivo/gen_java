package com.devivo.gen_java;

import com.ericsson.otp.erlang.*;

/**
 * This class exists as an example of how to write your own erlang functions
 *
 * We're modeling the erlang bif `erlang:abs/1`
 *
 * We can add more to this class over time.
 */
public class Erlang {

    public static OtpErlangDouble abs(OtpErlangDouble d) throws Exception {
        double jd = d.doubleValue();
        return new OtpErlangDouble(Math.abs(jd));
    }

    public static OtpErlangLong abs(OtpErlangLong l) throws Exception {
        long jl = l.longValue();
        return new OtpErlangLong(Math.abs(jl));
    }
}
