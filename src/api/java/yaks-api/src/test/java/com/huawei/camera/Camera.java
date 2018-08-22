package com.huawei.camera;

import java.util.concurrent.ExecutionException;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.Yaks;

public class Camera {
    private String id;

    private Yaks yaks;
    private Access access;

    public Camera(String id) {
        this.id = id;
    }

    public void init() throws InterruptedException, ExecutionException {
        // Instantiate a Yaks Java API using a socket to connect the Yaks
        // instance on 128.10.0.1:8448
        this.yaks = Yaks.getInstance("is.yaks.YaksSocketImpl", Camera.class.getClassLoader(), "host=128.10.0.1",
                "port=8448");

        this.access = yaks.createAccess(Path.ofString("//com.huawei/shenzen/videoapps/camera/" + id), 1024,
                Encoding.JSON);
    }

    public void run() {
        while (true) {
            // assuming getVideoChunk() blocks until a videoChunk is available
            byte[] chunk = getVideoChunk();

            access.put(Selector.ofString("//com.huawei/shenzen/videoapps/camera/" + id + "/chunk-" + getDate()), chunk);
        }
    }

    private String getDate() {
        return Long.toString(System.currentTimeMillis());

    }

    private byte[] getVideoChunk() {
        // TODO get the videoChunk from the camera device
        return null;
    }

    public static void main(String[] args) {
        Camera c = new Camera(args[0]);

        try {
            c.init();
        } catch (Exception e) {
            System.err.println("Error at Yaks initialization: " + e);
        }

        c.run();
    }
}
