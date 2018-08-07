import requests 
import json
from random import randint
from time import time
from colorama import Fore
from colorama import Style
import sys
import statistics
import scipy.io




def main(ip, port):

    SERVER = 'http://{}:{}/'.format(ip,port)
    token = time()
    #print ("Creating storage /afos/0/1/\n")

    input("Press for create a group")

    uri = SERVER+'yaks/group?name=test1'
    cookies = {
            'yaks.rws': "[\"/these/*\",\"/those/*\"]",
            'yaks.rs': "[\"/that_ones/*\"]",
            'yaks.ws': "[]",
            }
    resp = requests.put(uri,cookies=cookies)
    print(resp.cookies)
    groupid = resp.cookies.get('is.yaks.group.id')

    input("Press for create a user")

    uri = SERVER+'yaks/user?name=user&group='+groupid
    cookies = {
            'yaks.user.pwd': "hello"
            }
    resp = requests.put(uri,cookies=cookies)
    print(resp.cookies)
    userid = resp.cookies.get('is.yaks.user.id')


    input("Press for authenticate the user")

    uri = SERVER+'yaks/authenticate?name=user'
    cookies = {
            'yaks.user.pwd': "hello"
            }
    resp = requests.post(uri,cookies=cookies)
    print(resp.cookies)
    token = resp.cookies.get('is.yaks.user.token')

    input("Press for authenticate the access (no rights creation will fail)")
    uri = SERVER+'yaks/access?path=/prova&cacheSize=100'
    cookies = {
            'is.yaks.user.token': token
            }
    resp = requests.post(uri,cookies=cookies)
    print(resp.cookies)
    access_id = resp.cookies.get('is.yaks.access')


    input("Press for authenticate the access (with rights creation will be ok)")
    uri = SERVER+'yaks/access?path=/these/data&cacheSize=100'
    cookies = {
            'is.yaks.user.token': token
            }
    resp = requests.post(uri,cookies=cookies)
    print(resp.cookies)
    access_id = resp.cookies.get('is.yaks.access')

    
    input("Press enter to exit")
    exit(0)

    uri = SERVER+'yaks/storages?path=/afos/0/1/&yaks.backend=memory'
    resp = requests.post(uri)
    storageid = resp.cookies.get('is.yaks.storage')
    #print('Storage created with id {}\n'.format(storageid))

    #input('Press enter to create access')

    uri = SERVER+'yaks/access?path=/afos/0/1/&cacheSize=100'
    resp = requests.post(uri)
    access_id = resp.cookies.get('is.yaks.access')
    # #print('Access created with id {}\n'.format(access_id))
    # i = 0
    # tries = times
    # #input('Press enter to send {} values and calculate response time'.format(tries))
    # successed = 0
    # failed = 0 
    # results = []
    # resp_times = []
    # while i < tries:
    #     #print('Press enter to put a random value to /afos/0/1/data-{}'.format(i))
    #     value = {'value': randint(0, 65535)}
    #     cookies = {'is.yaks.access':access_id}
    #     uri = SERVER + 'afos/0/1/data-{}'.format(i)
    #     try:
    #         starttime = time()
    #         resp = requests.put(uri,data=json.dumps(value),cookies=cookies)
    #         timetaken = time() - starttime
    #         resp_times.append(timetaken)
    #         successed = successed+1
    #         results.append(0)
    #         print(f'[{Fore.GREEN}SUCCESS{Style.RESET_ALL}] Run {i}')
    #     except:
    #         failed = failed + 1
    #         results.append(-1)
    #         print(f'[{Fore.RED}FAILED{Style.RESET_ALL}] Run {i}')
    #     finally:
    #         i = i + 1
    # print('Results:')
    # print('Successful: {} Failed: {}'.format(successed,failed))
    # print('Success Rate: {}%'.format((float(successed)/float(tries))*100))
    # print('Total: {} \nMin: {} \nMax: {} \nAvg: {}'.format(sum(resp_times),min(resp_times),max(resp_times),statistics.mean(resp_times)))
    # print('Variance: {}'.format(statistics.variance(resp_times)))
    # print('Std Deviation: {}'.format(statistics.stdev(resp_times)))
    # print('Saving results into MAT file -> put_results-{}.mat'.format(token))

    # data = {
    #     'put_total_tries':tries,
    #     'put_response_times': resp_times,
    #     'put_results':results    
    # }
    # scipy.io.savemat('put_results-{}.mat'.format(token),data)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print('[Usage] {} <server> <port>'.format(sys.argv[0]))
    else:
        main(sys.argv[1],sys.argv[2])
