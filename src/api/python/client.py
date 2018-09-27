from yaks_api import api
import sys


def obs(kvs):
    print('Called OBSERVER KVS: {}'.format(kvs))


def main():
    print('creating api')
    #y = api.YAKS(sys.argv[1])
    y = api.YAKS('127.0.0.1')
    print('>> Create storage')
    input()
    #storage = y.create_storage('//fos')
    print('>> Create access')
    input()
    access = y.create_access('//fos')

    sid = access.subscribe('//fos/example/*', obs)

    print('>> Put Tuple')
    input()
    access.put('//fos/example/one', 'hello!')

    print('>> Unsubscribe and Put Tuple')
    input()
    access.unsubscribe(sid)
    access.put('//fos/example/two', 'hello2!')

    print('>> Put Tuple')
    input()
    access.put('//fos/example/three', 'hello3!')

    print('>> Get Tuple')
    input()
    print('GET: {}'.format(access.get('//fos/example/one')))

    print('>> Get Tuple')
    input()
    print('GET: {}'.format(access.get('//fos/example')))

    print('>> Get Tuple')
    input()
    print('GET: {}'.format(access.get('//fos/example/*')))

    print('>> Dispose Access')
    input()
    access.dispose()

    print('>> Dispose Storage')
    input()
    #storage.dispose()

    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
