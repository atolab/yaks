# Copyright (c) 2018 ADLINK Technology Inc.
#
# See the NOTICE file(s) distributed with this work for additional
# information regarding copyright ownership.
#
# This program and the accompanying materials are made available under the
# terms of the LGPL 2.1 which is available at
# https://github.com/atolab/yaks/blob/master/LICENSE
# Contributors: Gabriele Baldoni, ADLINK Technology Inc. - API

#!/usr/bin/env python3

from setuptools import setup

setup(
    name='yaks_api',
    version='0.0.1',
    author='ADLINK_ATO',
    packages=['yaks_api'],
    install_requires=['hexdump'],
    include_package_data=True
)
