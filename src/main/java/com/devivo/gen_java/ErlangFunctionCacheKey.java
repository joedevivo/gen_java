package com.devivo.gen_java;

import java.util.Arrays;

public class ErlangFunctionCacheKey {
    private String module;
    private String function;
    private Class[] arguments;

    public ErlangFunctionCacheKey(String module, String function, Class... arguments) {
        this.module = module;
        this.function = function;
        this.arguments = arguments;
    }

    public String getModule() {
        return module;
    }

    public String getFunction() {
        return function;
    }

    public Class[] getArguments() {
        return arguments;
    }

    @Override
    public boolean equals(Object obj) {
        if(obj instanceof ErlangFunctionCacheKey) {
            ErlangFunctionCacheKey that = (ErlangFunctionCacheKey)obj;
            return that.getModule().equals(this.getModule()) &&
                   that.getFunction().equals(this.getFunction()) &&
                    Arrays.equals(that.getArguments(), this.getArguments());
        } else {
            return false;
        }
    }

    // Returns a unqiue name for hashing mostly
    @Override
    public String toString() {
        String args = "";
        for(Class c : arguments) {
            args += "::" + c.toString();
        }
        return this.module + ":" + this.function + args;
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }
}
