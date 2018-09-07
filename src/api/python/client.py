from yaks_api import api
import sys


def main():
    print('creating api')
    y = api.YAKS(sys.argv[1])
    print('Creating storage')
    input()
    storage = y.create_storage('//fos')
    print('Creating access')
    input()
    access = y.create_access('//fos')

    print('dispose storage')
    input()
    storage.dispose()

    print('get')
    input()
    access.get('//fos/blabla')

    print('put')
    input()
    access.put('//fos/blabla/one', 'hello!')

    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
