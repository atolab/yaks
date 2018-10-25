from yaks_api import api
import sys
import json


def obs(kvs):
    print('Called OBSERVER KVS: {}'.format(kvs))


def main():
    print('creating api')
    y = api.YAKS(sys.argv[1])
    print('>> Create storage')
    input()
    #storage = y.create_storage('//fos')
    print('>> Create access and subscription')
    input()
    access = y.create_access('//fos')

    sid = access.subscribe('//fos/example/*', obs)

    print('>> Put Tuple')
    input()
    access.put('//fos/example/one', 'hello!')

    print('>> Put Tuple')
    input()
    access.put('//fos/example/two', 'hello2!')

    print('>> Put Tuple')
    input()
    access.put('//fos/example/three', 'hello3!')

    print('>> Put Tuple JSON as RAW')
    input()
    d = json.dumps({'this': 'is', 'a': 'json'})
    access.put('//fos/example/four', d)

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
    if sid:
        access.unsubscribe(sid)
    access.dispose()

    print('>> Dispose Storage')
    input()
    #storage.dispose()

    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
