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

    input("press enter for a put (authorized)")
    value = {'value': randint(0, 65535)}
    cookies = {
            'is.yaks.user.token': token,
            'is.yaks.access':access_id
            }
    uri = SERVER + 'these/data'
    resp = requests.put(uri,data=json.dumps(value),cookies=cookies)
    print(resp)


    input("press enter for a get(authorized)")
    cookies = {
            'is.yaks.user.token': token,
            'is.yaks.access':access_id
            }
    uri = SERVER + 'these/data'
    resp = requests.get(uri,cookies=cookies)
    print(resp.text)

    input("Press for authenticate a new access (with read only rights)")
    uri = SERVER+'yaks/access?path=/that_ones/data&cacheSize=100'
    cookies = {
            'is.yaks.user.token': token
            }
    resp = requests.post(uri,cookies=cookies)
    print(resp.cookies)
    access_id = resp.cookies.get('is.yaks.access')

    input("press enter for a put (unauthorized)")
    value = {'value': randint(0, 65535)}
    cookies = {
            'is.yaks.user.token': token,
            'is.yaks.access':access_id
            }
    uri = SERVER + 'that_ones/data'
    resp = requests.put(uri,data=json.dumps(value),cookies=cookies)
    print(resp)


    input("press enter for a get(authorized)")
    cookies = {
            'is.yaks.user.token': token,
            'is.yaks.access':access_id
            }
    uri = SERVER + 'that_ones/data'
    resp = requests.get(uri,cookies=cookies)
    print(resp.text)




    input("press enter for a put (unauthorized - access is in another path)")
    value = {'value': randint(0, 65535)}
    cookies = {
            'is.yaks.user.token': token,
            'is.yaks.access':access_id
            }
    uri = SERVER + 'this/data'
    resp = requests.put(uri,data=json.dumps(value),cookies=cookies)
    print(resp)


    input("press enter for a get(unauthorized)")
    cookies = {
            'is.yaks.user.token': token,
            'is.yaks.access':access_id
            }
    uri = SERVER + 'this/data'
    resp = requests.get(uri,cookies=cookies)
    print(resp.text)


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


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print('[Usage] {} <server> <port>'.format(sys.argv[0]))
    else:
        main(sys.argv[1],sys.argv[2])
