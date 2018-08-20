package is.yaks.rest.foo;

import com.google.gson.annotations.Expose;

public class Foo {

    @Expose
    String bar;

    public Foo() {
        bar = "Foo! Bar!";
    }

    @Override
    public String toString() {
        return "Foo value: " + bar;
    }
}
