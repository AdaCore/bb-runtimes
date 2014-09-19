#!/usr/bin/env python

import os
import sys
import struct
import termios
import select
import getopt

class IOError(Exception):
    """Serial IO error"""
    pass

verbose=0
speed=termios.B57600
flag_run=0

options,argv = getopt.getopt(sys.argv[1:], 'vs:r')

allspeeds = {'9600': termios.B9600,
             '57600': termios.B57600,
             '115200' : termios.B115200 }

for opt, arg in options:
    if opt in ('-v'):
        verbose=1
    elif opt in ('-r'):
        flag_run=1
    elif opt in ('-s'):
        if arg in allspeeds:
            speed=allspeeds[arg]
        else:
            print 'Unknown speed: ', arg
            sys.exit(1)

if not len(argv) in (1,2):
    print 'Usage: ' + sys.argv[0] + ' [-s speed] device file'
    sys.exit(1)

dev=argv[0]
fd=os.open(dev,os.O_RDWR);

caps = termios.tcgetattr(fd)
caps[3] = caps[3] & ~(termios.ECHO | termios.ICANON) # lflags
caps[4] = speed
caps[5] = speed
termios.tcsetattr(fd, termios.TCSADRAIN, caps)

def readwait(l):
    res=''
    for i in range(l):
        i,o,e=select.select([fd],[],[],0.1)
        if len(i) == 0:
            break
        res = res + os.read(fd,1)
    return res

def flushserial():
    while 1:
        i,o,e=select.select([fd],[],[],0.2)
        if len(i) == 0:
            break
        c = os.read(fd,1)
        sys.stdout.write(c)
    sys.stdout.flush()

print 'Syncing...'
os.write(fd, '\n')
res = readwait(7)
if res != '\r\nMON> ':
    print 'cannot get prompt, got "', res, '"'
    sys.exit(1)
os.write(fd,'load\n')
res = readwait(6)
if res != 'load\r\n':
    print 'cannot get echo, got "', res, '"'
    sys.exit(1)
res = readwait(19)
if res != 'Waiting for srec.\r\n':
    print 'cannot get wait message, got "', res, '"'
    sys.exit(1)

print 'Sending srec...'

for l in open(argv[1]):
    if verbose:
        print l.rstrip(),
    os.write(fd,l)
    i,o,e=select.select([fd],[],[],1.0)
    if len(i) == 0:
        print ' Timeout!'
        break
    c = os.read(fd,1)
    if c == '+':
        sys.stdout.write('+')
        sys.stdout.flush()
        if verbose:
            print
    elif c == '.':
        print '.'
        break
    else:
        print ' Error!'
        # Flush input
        sys.stdout.write (c)
        flushserial()
        sys.exit(1)

res = readwait(5)
if res != 'MON> ':
    print 'cannot get prompt, got "', res, '"'
    sys.exit(1)

def echo():
    while 1:
        i,o,e=select.select([fd],[],[])
        for s in i:
            c = os.read(s,1)
            if ord(c) == 26: # ^Z
                return
            sys.stdout.write(c)
        sys.stdout.flush()
if flag_run:
    os.write(fd,'go\n')
    res = readwait(4)
    if res != 'go\r\n':
        print 'cannot get echo, got "', res, '"'
        sys.exit(1)
    print '# Echoing...'
    echo()
    flushserial()
else:
    print 'Done'
