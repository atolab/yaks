package com.huawei.camera;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Listener;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.Yaks;

public class Analytic implements Listener<byte[]> {

    private Yaks yaks;
    private Access access;

    public Analytic() {
    }

    public void init() {
        // Instantiate a Yaks Java API using a socket to connect the Yaks instance on 128.10.0.2:8448
        this.yaks = Yaks.getInstance("is.yaks.YaksSocketImpl", Camera.class.getClassLoader(), "host=128.10.0.2",
                "port=8448");

        this.access = yaks.createAccess(Path.ofString("//com.huawei/shenzen/"), 1024, Encoding.JSON);

        access.subscribe(Selector.ofString("//com.huawei/shenzen/videoapps/camera/**"), this);

        access.eval(Selector.ofString("//com.huawei/shenzen/mlapps/pics_db"), p -> match_func(p));
    }

    @Override
    public void onData(Path key, byte[] videoChunk) {
        // process the video chunk and store its key along with information allowing matching with a picture
    }

    private Object match_func(Path p) {
        // get the picture image vector to match
        String personImgVector = p.getQuery().get("pic");

        return findMatchingVideoChunk(personImgVector);
    }

    public String[] findMatchingVideoChunk(String personImgVector) {
        // find the keys of a video chunks matching with the picture
        String[] result = { "//com.huawei/shenzen/videoapps/camera/123/chunk-1531492128",
                "//com.huawei/shenzen/videoapps/camera/123/chunk-1531492135",
                "//com.huawei/shenzen/videoapps/camera/456/chunk-1531492159" };
        return result;
    }

    public void run() {
        while (true) {
            try {

                Thread.sleep(100);
            } catch (InterruptedException e) {
                // ignore
            }
        }
    }

    public static void main(String[] args) {
        Analytic a = new Analytic();

        try {
            a.init();
        } catch (Exception e) {
            System.err.println("Error at Yaks initialization: " + e);
        }

        a.run();

    }

}
