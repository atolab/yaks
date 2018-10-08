from yaks_api import api
import sys


def obs(kvs):
    print('Called OBSERVER KVS: {}'.format(kvs))


def main():
    print('creating api')
    y = api.YAKS(sys.argv[1])
    # y = api.YAKS('127.0.0.1')
    print('>> Create storage')
    #input()
    # storage = y.create_storage('//fos')
    print('>> Create access')
    #input()
    access = y.create_access('//afos')

    # sid = access.subscribe('//fos/example/*', obs)
    #s = 'Ly9hZm9zLzAvNTM3MTJkZjI5NjQ5NGEyMWJlMmU4MGVlZDAwZmY5Y2UuLnsiaW8iOiBbXSwgIm5l\ndHdvcmsiOiBbeyJhdmFpbGFibGUiOiBmYWxzZSwgImludGZfbmFtZSI6ICJlbnMzMyIsICJ0eXBl\nIjogImV0aGVybmV0IiwgImluZnRfY29uZmlndXJhdGlvbiI6IHsiaXB2Nl9hZGRyZXNzIjogImZl\nODA6OjIwYzoyOWZmOmZlNDQ6OTZmOSVlbnMzMyIsICJpcHY2X25ldG1hc2siOiAiZmZmZjpmZmZm\nOmZmZmY6ZmZmZjo6IiwgImlwdjRfZ2F0ZXdheSI6ICIxOTIuMTY4LjEuMSIsICJpcHY0X2FkZHJl\nc3MiOiAiMTkyLjE2OC4xLjEwNiIsICJpcHY0X25ldG1hc2siOiAiMjU1LjI1NS4yNTUuMCJ9LCAi\naW50Zl9tYWNfYWRkcmVzcyI6ICIwMDowYzoyOTo0NDo5NjpmOSIsICJpbnRmX3NwZWVkIjogMTAw\nMCwgImRlZmF1bHRfZ3ciOiB0cnVlfSwgeyJhdmFpbGFibGUiOiB0cnVlLCAiaW50Zl9uYW1lIjog\nInZpcmJyMCIsICJ0eXBlIjogInZpcnR1YWwgYnJpZGdlIiwgImluZnRfY29uZmlndXJhdGlvbiI6\nIHsiaXB2Nl9hZGRyZXNzIjogIiIsICJpcHY2X25ldG1hc2siOiAiIiwgImlwdjRfZ2F0ZXdheSI6\nICIiLCAiaXB2NF9hZGRyZXNzIjogIjE5Mi4xNjguMTIyLjEiLCAiaXB2NF9uZXRtYXNrIjogIjI1\nNS4yNTUuMjU1LjAifSwgImludGZfbWFjX2FkZHJlc3MiOiAiNTI6NTQ6MDA6NTA6NTc6NTEiLCAi\naW50Zl9zcGVlZCI6IDAsICJkZWZhdWx0X2d3IjogZmFsc2V9LCB7ImF2YWlsYWJsZSI6IHRydWUs\nICJpbnRmX25hbWUiOiAibHhkYnIwIiwgInR5cGUiOiAiY29udGFpbmVyIGJyaWRnZSIsICJpbmZ0\nX2NvbmZpZ3VyYXRpb24iOiB7ImlwdjZfYWRkcmVzcyI6ICJmZTgwOjo1MGFlOjQ2ZmY6ZmVjNjpk\nMDlkJWx4ZGJyMCIsICJpcHY2X25ldG1hc2siOiAiZmZmZjpmZmZmOmZmZmY6ZmZmZjo6IiwgImlw\ndjRfZ2F0ZXdheSI6ICIiLCAiaXB2NF9hZGRyZXNzIjogIjEwLjE3OC4yMzAuMSIsICJpcHY0X25l\ndG1hc2siOiAiMjU1LjI1NS4yNTUuMCJ9LCAiaW50Zl9tYWNfYWRkcmVzcyI6ICI1MjphZTo0Njpj\nNjpkMDo5ZCIsICJpbnRmX3NwZWVkIjogMCwgImRlZmF1bHRfZ3ciOiBmYWxzZX0sIHsiYXZhaWxh\nYmxlIjogdHJ1ZSwgImludGZfbmFtZSI6ICJicjAiLCAidHlwZSI6ICJicmlkZ2UiLCAiaW5mdF9j\nb25maWd1cmF0aW9uIjogeyJpcHY2X2FkZHJlc3MiOiAiZmU4MDo6MjBjOjI5ZmY6ZmU0NDo5NjAz\nJWJyMCIsICJpcHY2X25ldG1hc2siOiAiZmZmZjpmZmZmOmZmZmY6ZmZmZjo6IiwgImlwdjRfZ2F0\nZXdheSI6ICIiLCAiaXB2NF9hZGRyZXNzIjogIjE5Mi4xNjguMS4xNjAiLCAiaXB2NF9uZXRtYXNr\nIjogIjI1NS4yNTUuMjU1LjAifSwgImludGZfbWFjX2FkZHJlc3MiOiAiMDA6MGM6Mjk6NDQ6OTY6\nMDMiLCAiaW50Zl9zcGVlZCI6IDAsICJkZWZhdWx0X2d3IjogZmFsc2V9LCB7ImF2YWlsYWJsZSI6\nIHRydWUsICJpbnRmX25hbWUiOiAiZW5zMzgiLCAidHlwZSI6ICJldGhlcm5ldCIsICJpbmZ0X2Nv\nbmZpZ3VyYXRpb24iOiB7ImlwdjZfYWRkcmVzcyI6ICJmZTgwOjoyMGM6MjlmZjpmZTQ0Ojk2MDMl\nZW5zMzgiLCAiaXB2Nl9uZXRtYXNrIjogImZmZmY6ZmZmZjpmZmZmOmZmZmY6OiIsICJpcHY0X2dh\ndGV3YXkiOiAiIiwgImlwdjRfYWRkcmVzcyI6ICIiLCAiaXB2NF9uZXRtYXNrIjogIiJ9LCAiaW50\nZl9tYWNfYWRkcmVzcyI6ICIwMDowYzoyOTo0NDo5NjowMyIsICJpbnRmX3NwZWVkIjogMTAwMCwg\nImRlZmF1bHRfZ3ciOiBmYWxzZX0sIHsiYXZhaWxhYmxlIjogdHJ1ZSwgImludGZfbmFtZSI6ICJ2\naXJicjAtbmljIiwgInR5cGUiOiAidmlydHVhbCBicmlkZ2UiLCAiaW5mdF9jb25maWd1cmF0aW9u\nIjogeyJpcHY2X2FkZHJlc3MiOiAiIiwgImlwdjZfbmV0bWFzayI6ICIiLCAiaXB2NF9nYXRld2F5\nIjogIiIsICJpcHY0X2FkZHJlc3MiOiAiIiwgImlwdjRfbmV0bWFzayI6ICIifSwgImludGZfbWFj\nX2FkZHJlc3MiOiAiNTI6NTQ6MDA6NTA6NTc6NTEiLCAiaW50Zl9zcGVlZCI6IDEwLCAiZGVmYXVs\ndF9ndyI6IGZhbHNlfSwgeyJhdmFpbGFibGUiOiB0cnVlLCAiaW50Zl9uYW1lIjogImxvIiwgInR5\ncGUiOiAibG9vcGJhY2siLCAiaW5mdF9jb25maWd1cmF0aW9uIjogeyJpcHY2X2FkZHJlc3MiOiAi\nOjoxIiwgImlwdjZfbmV0bWFzayI6ICJmZmZmOmZmZmY6ZmZmZjpmZmZmOmZmZmY6ZmZmZjpmZmZm\nOmZmZmYiLCAiaXB2NF9nYXRld2F5IjogIiIsICJpcHY0X2FkZHJlc3MiOiAiMTI3LjAuMC4xIiwg\nImlwdjRfbmV0bWFzayI6ICIyNTUuMC4wLjAifSwgImludGZfbWFjX2FkZHJlc3MiOiAiMDA6MDA6\nMDA6MDA6MDA6MDAiLCAiaW50Zl9zcGVlZCI6IDAsICJkZWZhdWx0X2d3IjogZmFsc2V9XSwgImRp\nc2tzIjogW3siZGltZW5zaW9uIjogMTUuNjIyOTYyOTUxNjYwMTU2LCAibW91bnRfcG9pbnQiOiAi\nLyIsICJsb2NhbF9hZGRyZXNzIjogIi9kZXYvc2RhMSIsICJmaWxlc3lzdGVtIjogImV4dDQifSwg\neyJkaW1lbnNpb24iOiAwLjA1MzM0NDcyNjU2MjUsICJtb3VudF9wb2ludCI6ICIvc25hcC9seGQv\nODQxNSIsICJsb2NhbF9hZGRyZXNzIjogIi9kZXYvbG9vcDAiLCAiZmlsZXN5c3RlbSI6ICJzcXVh\nc2hmcyJ9LCB7ImRpbWVuc2lvbiI6IDAuMDg0OTYwOTM3NSwgIm1vdW50X3BvaW50IjogIi9zbmFw\nL2NvcmUvNTE0NSIsICJsb2NhbF9hZGRyZXNzIjogIi9kZXYvbG9vcDEiLCAiZmlsZXN5c3RlbSI6\nICJzcXVhc2hmcyJ9LCB7ImRpbWVuc2lvbiI6IDAuMDg1OTM3NSwgIm1vdW50X3BvaW50IjogIi9z\nbmFwL2NvcmUvNTMyOCIsICJsb2NhbF9hZGRyZXNzIjogIi9kZXYvbG9vcDMiLCAiZmlsZXN5c3Rl\nbSI6ICJzcXVhc2hmcyJ9LCB7ImRpbWVuc2lvbiI6IDAuMDg0OTYwOTM3NSwgIm1vdW50X3BvaW50\nIjogIi9zbmFwL2NvcmUvNDkxNyIsICJsb2NhbF9hZGRyZXNzIjogIi9kZXYvbG9vcDQiLCAiZmls\nZXN5c3RlbSI6ICJzcXVhc2hmcyJ9LCB7ImRpbWVuc2lvbiI6IDAuMDYyMDExNzE4NzUsICJtb3Vu\ndF9wb2ludCI6ICIvc25hcC9seGQvODg4MiIsICJsb2NhbF9hZGRyZXNzIjogIi9kZXYvbG9vcDIi\nLCAiZmlsZXN5c3RlbSI6ICJzcXVhc2hmcyJ9XSwgImNwdSI6IFt7ImZyZXF1ZW5jeSI6IDMwOTUu\nMDY1LCAiYXJjaCI6ICJ4ODZfNjQiLCAibW9kZWwiOiAiSW50ZWwoUikgQ29yZShUTSkgaTUtNzI2\nN1UgQ1BVIEAgMy4xMEdIeiJ9LCB7ImZyZXF1ZW5jeSI6IDMwOTUuMDY1LCAiYXJjaCI6ICJ4ODZf\nNjQiLCAibW9kZWwiOiAiSW50ZWwoUikgQ29yZShUTSkgaTUtNzI2N1UgQ1BVIEAgMy4xMEdIeiJ9\nXSwgIm5hbWUiOiAidWJ1bnR1LWRldiIsICJyYW0iOiB7InNpemUiOiAzOTMzLjY4NzV9LCAib3Mi\nOiAibGludXgiLCAiYWNjZWxlcmF0b3IiOiBbXSwgInV1aWQiOiAiNTM3MTJkZjI5NjQ5NGEyMWJl\nMmU4MGVlZDAwZmY5Y2UifQ==\n'
    print('>> Put Tuple')
    #input()
    s = '{"io": [], "network": [{"available": false, "intf_name": "ens33", "type": "ethernet", "inft_configuration": {"ipv6_address": "fe80::20c:29ff:fe44:96f9%ens33", "ipv6_netmask": "ffff:ffff:ffff:ffff::", "ipv4_gateway": "192.168.1.1", "ipv4_address": "192.168.1.106", "ipv4_netmask": "255.255.255.0"}, "intf_mac_address": "00:0c:29:44:96:f9", "intf_speed": 1000, "default_gw": true}, {"available": true, "intf_name": "virbr0", "type": "virtual bridge", "inft_configuration": {"ipv6_address": "", "ipv6_netmask": "", "ipv4_gateway": "", "ipv4_address": "192.168.122.1", "ipv4_netmask": "255.255.255.0"}, "intf_mac_address": "52:54:00:50:57:51", "intf_speed": 0, "default_gw": false}, {"available": true, "intf_name": "lxdbr0", "type": "container bridge", "inft_configuration": {"ipv6_address": "fe80::50ae:46ff:fec6:d09d%lxdbr0", "ipv6_netmask": "ffff:ffff:ffff:ffff::", "ipv4_gateway": "", "ipv4_address": "10.178.230.1", "ipv4_netmask": "255.255.255.0"}, "intf_mac_address": "52:ae:46:c6:d0:9d", "intf_speed": 0, "default_gw": false}, {"available": true, "intf_name": "br0", "type": "bridge", "inft_configuration": {"ipv6_address": "fe80::20c:29ff:fe44:9603%br0", "ipv6_netmask": "ffff:ffff:ffff:ffff::", "ipv4_gateway": "", "ipv4_address": "192.168.1.160", "ipv4_netmask": "255.255.255.0"}, "intf_mac_address": "00:0c:29:44:96:03", "intf_speed": 0, "default_gw": false}, {"available": true, "intf_name": "ens38", "type": "ethernet", "inft_configuration": {"ipv6_address": "fe80::20c:29ff:fe44:9603%ens38", "ipv6_netmask": "ffff:ffff:ffff:ffff::", "ipv4_gateway": "", "ipv4_address": "", "ipv4_netmask": ""}, "intf_mac_address": "00:0c:29:44:96:03", "intf_speed": 1000, "default_gw": false}, {"available": true, "intf_name": "virbr0-nic", "type": "virtual bridge", "inft_configuration": {"ipv6_address": "", "ipv6_netmask": "", "ipv4_gateway": "", "ipv4_address": "", "ipv4_netmask": ""}, "intf_mac_address": "52:54:00:50:57:51", "intf_speed": 10, "default_gw": false}, {"available": true, "intf_name": "lo", "type": "loopback", "inft_configuration": {"ipv6_address": "::1", "ipv6_netmask": "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "ipv4_gateway": "", "ipv4_address": "127.0.0.1", "ipv4_netmask": "255.0.0.0"}, "intf_mac_address": "00:00:00:00:00:00", "intf_speed": 0, "default_gw": false}], "disks": [{"dimension": 15.622962951660156, "mount_point": "/", "local_address": "/dev/sda1", "filesystem": "ext4"}, {"dimension": 0.0533447265625, "mount_point": "/snap/lxd/8415", "local_address": "/dev/loop0", "filesystem": "squashfs"}, {"dimension": 0.0849609375, "mount_point": "/snap/core/5145", "local_address": "/dev/loop1", "filesystem": "squashfs"}, {"dimension": 0.0859375, "mount_point": "/snap/core/5328", "local_address": "/dev/loop3", "filesystem": "squashfs"}, {"dimension": 0.0849609375, "mount_point": "/snap/core/4917", "local_address": "/dev/loop4", "filesystem": "squashfs"}, {"dimension": 0.06201171875, "mount_point": "/snap/lxd/8882", "local_address": "/dev/loop2", "filesystem": "squashfs"}], "cpu": [{"frequency": 3095.065, "arch": "x86_64", "model": "Intel(R) Core(TM) i5-7267U CPU @ 3.10GHz"}, {"frequency": 3095.065, "arch": "x86_64", "model": "Intel(R) Core(TM) i5-7267U CPU @ 3.10GHz"}], "name": "ubuntu-dev", "ram": {"size": 3933.6875}, "os": "linux", "accelerator": [], "uuid": "53712df296494a21be2e80eed00ff9ce"}'
    access.put('//afos/0/53712df296494a21be2e80eed00ff9ce', s)
    access.put('//afos/0/53712df296494a21be2e80eed00ff9ce', s*2)
    access.put('//afos/0/53712df296494a21be2e80eed00ff9ce', s*3)
    access.put('//afos/0/53712df296494a21be2e80eed00ff9ce', s*4)
    access.put('//afos/0/53712df296494a21be2e80eed00ff9ce', s*5)

    sid = access.subscribe('//afos/0/**', lambda x: print('OBS {}'.format(x)))

    print('>> Remove Tuple')
    access.remove('//afos/0/53712df296494a21be2e80eed00ff9ce')


    access.unsubscribe(sid)
    print('>> Dispose Access')
    #input()
    access.dispose()

    print('>> Dispose Storage')
    #input()
    # storage.dispose()

    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
