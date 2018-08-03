import requests 
import json
from random import randint
from time import time
from colorama import Fore
from colorama import Style
import sys


def main(times, ip, port):

    SERVER = f'http://{ip}:{port}/'

    uri = SERVER+'yaks/access?path=/afos/0/1/&cacheSize=100'
    resp = requests.post(uri)
    access_id = resp.cookies.get('is.yaks.access')
    #print('Access created with id {}\n'.format(access_id))
    i = 0
    tries = times
    #input('Press enter to get {} times all the values and calculate response time'.format(tries))

    resp_times = []
    successed = 0
    failed = 0 
    while i < tries:
        cookies = {'is.yaks.access':access_id}
        uri = SERVER + 'afos/0/1/data-{}'.format(i)
        #resp = requests.get(uri,cookies=cookies)
        #data = resp.text
        #print('Data is {}\n'.format(data))
        try:
            starttime = time()
            resp = requests.get(uri,cookies=cookies)
            timetaken = time() - starttime
            resp_times.append(timetaken)
            successed = successed+1
            print(f'[{Fore.GREEN}SUCCESS{Style.RESET_ALL}] Run {i}')
        except:
            failed = failed + 1
            print(f'[{Fore.RED}FAILED{Style.RESET_ALL}] Run {i}')
        finally:
            i = i + 1

    print('Successful: {} Failed: {}'.format(successed,failed))
    print('Results Total: {} Min: {} Max: {} Avg:{}'.format(sum(resp_times),min(resp_times),max(resp_times),sum(resp_times)/len(resp_times)))

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print(f'[Usage] {sys.argv[0]} <tries> <server> <port>')
    else:
        main(int(sys.argv[1]),sys.argv[2],sys.argv[3])
