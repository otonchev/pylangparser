#!/usr/bin/env python

from distutils.core import setup

import sys
import os

from pylangparser import __version__ as version
from pylangparser import __author__ as author
from pylangparser import __author_email__ as author_email

modules = ["pylangparser",]

setup(
    name = "pylangparser",
    version = version,
    description = "Python language parsing module",
    author = author,
    author_email = author_email,
    url = "https://github.com/otonchev/pylangparser",
    download_url = "https://github.com/otonchev/pylangparser/archive/master.zip",
    license = "LGPLv2",
    py_modules = modules,
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: Developers',
        'Intended Audience :: Information Technology',
        'License :: OSI Approved :: LGPLv2',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.0',
        'Programming Language :: Python :: 3.1',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        ]
    )
