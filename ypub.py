import requests 
import json
from random import randint
from time import time
from colorama import Fore
from colorama import Style
import sys
import statistics


def main(times, ip, port):

    SERVER = f'http://{ip}:{port}/'
    #print ("Creating storage /afos/0/1/\n")

    uri = SERVER+'yaks/storages?path=/afos/0/1/&yaks.backend=memory'
    resp = requests.post(uri)
    storageid = resp.cookies.get('is.yaks.storage')
    #print('Storage created with id {}\n'.format(storageid))

    #input('Press enter to create access')

    uri = SERVER+'yaks/access?path=/afos/0/1/&cacheSize=100'
    resp = requests.post(uri)
    access_id = resp.cookies.get('is.yaks.access')
    #print('Access created with id {}\n'.format(access_id))
    i = 0
    tries = times
    #input('Press enter to send {} values and calculate response time'.format(tries))
    successed = 0
    failed = 0 
    resp_times = []
    while i < tries:
        #print('Press enter to put a random value to /afos/0/1/data-{}'.format(i))
        value = {'value': randint(0, 65535)}
        cookies = {'is.yaks.access':access_id}
        uri = SERVER + 'afos/0/1/data-{}'.format(i)
        try:
            starttime = time()
            resp = requests.put(uri,data=json.dumps(value),cookies=cookies)
            timetaken = time() - starttime
            resp_times.append(timetaken)
            successed = successed+1
            print(f'[{Fore.GREEN}SUCCESS{Style.RESET_ALL}] Run {i}')
        except:
            failed = failed + 1
            print(f'[{Fore.RED}FAILED{Style.RESET_ALL}] Run {i}')
        finally:
            i = i + 1
    print('Results:')
    print('Successful: {} Failed: {}'.format(successed,failed))
    print('Total: {} \nMin: {} \nMax: {} \nAvg: {}'.format(sum(resp_times),min(resp_times),max(resp_times),statistics.mean(resp_times)))
    print(f'Variance: {statistics.variance(resp_times)}')
    print(f'Std Deviation: {statistics.stdev(resp_times)}')

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print(f'[Usage] {sys.argv[0]} <tries> <server> <port>')
    else:
        main(int(sys.argv[1]),sys.argv[2],sys.argv[3])
