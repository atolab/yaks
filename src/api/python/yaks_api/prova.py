import api
import messages
import time


print('creating api')
y = api.YAKS('127.0.0.1')
print('Creating access')
y.create_access('//fos')
while True:
    time.sleep(1)
