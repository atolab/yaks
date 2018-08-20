import requests
import json
from random import randint
from time import time
from colorama import Fore
from colorama import Style
import sys
import statistics
import scipy.io


def main(times, ip, port):

    SERVER = 'http://{}:{}/'.format(ip, port)

    token = time()

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
    results = []
    while i < tries:
        cookies = {'is.yaks.access': access_id}
        uri = SERVER + 'afos/0/1/data-{}'.format(i)
        #resp = requests.get(uri,cookies=cookies)
        #data = resp.text
        #print('Data is {}\n'.format(data))
        try:
            starttime = time()
            resp = requests.get(uri, cookies=cookies)
            timetaken = time() - starttime
            resp_times.append(timetaken)
            successed = successed+1
            results.append(0)
            print('[{}SUCCESS{}] Run {}'.format(
                Fore.GREEN, Style.RESET_ALL, i))
        except:
            failed = failed + 1
            results.append(-1)
            print('[{}FAILED{}] Run {}'.format(Fore.RED, Style.RESET_ALL, i))
        finally:
            i = i + 1

    print('Results:')
    print('Successful: {} Failed: {}'.format(successed, failed))
    print('Success Rate: {}%'.format((float(successed)/float(tries))*100))
    print('Total: {} \nMin: {} \nMax: {} \nAvg: {}'.format(sum(resp_times),
                                                           min(resp_times), max(resp_times), statistics.mean(resp_times)))
    print('Variance: {}'.format(statistics.variance(resp_times)))
    print('Std Deviation: {}'.format(statistics.stdev(resp_times)))
    print('Saving results into MAT file -> get_results-{}.mat'.format(token))
    data = {
        'get_total_tries': tries,
        'get_response_times': resp_times,
        'get_results': results
    }
    scipy.io.savemat('get_results-{}.mat'.format(token), data)


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print('[Usage] {} <tries> <server> <port>'.format(sys.argv[0]))
    else:
        main(int(sys.argv[1]), sys.argv[2], sys.argv[3])
