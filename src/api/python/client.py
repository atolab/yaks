from yaks_api import api
import sys


def main():
    print('creating api')
    y = api.YAKS(sys.argv[1])
    print('>> Create storage')
    input()
    storage = y.create_storage('//fos')
    print('>> Create access')
    input()
    access = y.create_access('//fos')

    print('>> Put Tuple')
    input()
    access.put('//fos/blabla/one', 'hello!')
    
    print('>> Get Tuple')
    input()
    access.get('//fos/blabla')

    print('>> Dispose Access')
    input()
    access.dispose()
    

    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
