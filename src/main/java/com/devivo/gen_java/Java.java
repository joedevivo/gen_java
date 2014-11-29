package com.devivo.gen_java;

import com.ericsson.otp.erlang.*;

import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.List;

public class Java {

    public static OtpErlangList system_properties() {
        List<OtpErlangTuple> l = new ArrayList<OtpErlangTuple>();
        Iterator<Map.Entry<Object, Object>> it = System.getProperties().entrySet().iterator();
        while(it.hasNext()) {
            Map.Entry<Object, Object> i = it.next();
            OtpErlangObject[] elems = new OtpErlangObject[2];
            elems[0] = new OtpErlangAtom(i.getKey().toString());
            elems[1] = new OtpErlangBinary(i.getValue().toString().getBytes());
            OtpErlangTuple t = new OtpErlangTuple(elems);
            l.add(t);
        }
        return new OtpErlangList(l.toArray(new OtpErlangObject[0]));
    }

    public static OtpErlangList system_env() {
        List<OtpErlangTuple> l = new ArrayList<OtpErlangTuple>();
        Iterator<Map.Entry<String, String>> it = System.getenv().entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, String> i = it.next();
            OtpErlangObject[] elems = new OtpErlangObject[2];
            elems[0] = new OtpErlangBinary(i.getKey().getBytes());
            elems[1] = new OtpErlangBinary(i.getValue().getBytes());
            OtpErlangTuple t = new OtpErlangTuple(elems);
            l.add(t);
        }
        return new OtpErlangList(l.toArray(new OtpErlangObject[0]));
    }

    public static OtpErlangBinary input_arguments() {
        RuntimeMXBean mxBean = ManagementFactory.getRuntimeMXBean();
        Iterator<String> it = mxBean.getInputArguments().iterator();
        String ret = "";
        if(it.hasNext()) {
            ret = it.next();
            while (it.hasNext()) {
                ret += " " + it.next();
            }
        }
        return new OtpErlangBinary(ret.getBytes());
    }
}
