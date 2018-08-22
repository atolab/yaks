package com.huawei.camera;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Yaks;

public class UserApp {

    public static void main(String[] args) throws Exception {
        // Instantiate a Yaks Java API using a socket to connect the Yaks instance on 128.10.0.3:8448
        Yaks yaks = Yaks.getInstance("is.yaks.YaksSocketImpl", Camera.class.getClassLoader(), "host=128.10.0.3",
                "port=8448");

        Access access = yaks.createAccess(Path.ofString("//com.huawei/shenzen/"), 1024, Encoding.JSON);

        String personImgVector = args[0];

        String[] matchingVideoChunksKeys = access
                .get(Path.ofString("//com.huawei/shenzen/mlapps/pics_db?pic=" + personImgVector), String[].class);

        for (String key : matchingVideoChunksKeys) {
            byte[] videoChunk = access.get(Path.ofString(key), byte[].class);
            if (videoChunk != null) {
                processVideoChunk(videoChunk);
            }
        }
    }

    private static void processVideoChunk(byte[] videoChunk) {
        // display the video Chunk
    }

}
