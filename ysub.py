from yaks_api import YAKS
from time import time
from colorama import Fore
from colorama import Style
import sys
import statistics
import scipy.io


def main(times, ip, port):
    y = YAKS(ip, int(port))

    token = time()
    access = y.create_access('//afos/0/1')
    i = 0
    tries = times

    resp_times = []
    successed = 0
    failed = 0
    results = []
    while i < tries:
        key = '//afos/0/1/data-{}'.format(i)
        starttime = time()
        resp = access.get(key)
        if len(resp)>0:
            timetaken = time() - starttime
            resp_times.append(timetaken)
            successed = successed+1
            results.append(0)
            print('[{}SUCCESS{}] Run {}'.format(
                Fore.GREEN, Style.RESET_ALL, i))
        else:
            failed = failed + 1
            results.append(-1)
            print('[{}FAILED{}] Run {}'.format(Fore.RED, Style.RESET_ALL, i))
        i = i + 1

    access.dispose()
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
