from yaks_api import api
import sys


def obs(kvs):
    print('Called OBSERVER KVS: {}'.format(kvs))


def main():
    print('creating api')
    y = api.YAKS(sys.argv[1])
    # y = api.YAKS('127.0.0.1')
    print('>> Create access')
    input()
    access = y.create_access('//fos')

    sid = access.subscribe('//fos/example/*', obs)

    print('>> Unsubscribe and Dispose')
    input()
    access.unsubscribe(sid)
    access.dispose()
    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
