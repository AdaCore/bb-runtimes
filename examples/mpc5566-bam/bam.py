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

verbose=1
speed=termios.B57600

options,argv = getopt.getopt(sys.argv[1:], 'vs:')

allspeeds = {'9600': termios.B9600,
             '57600': termios.B57600,
             '115200' : termios.B115200 }

for opt, arg in options:
    if opt in ('-v'):
        verbose=1
    elif opt in ('-s'):
        if arg in allspeeds:
            speed=allspeeds[arg]
        else:
            print 'Unknown speed: ', arg
            sys.exit(1)

if not len(argv) in (1,2):
    print 'Usage: ' + sys.argv[0] + ' [-s speed] device [file]'
    sys.exit(1)

dev=argv[0]
fd=os.open(dev,os.O_RDWR);

def set_baud(br):
    caps = termios.tcgetattr(fd)
    caps[2] = termios.CS8 | termios.CREAD | termios.CLOCAL;
    caps[3] = caps[3] & ~(termios.ECHO | termios.ICANON) # lflags
    caps[4] = br
    caps[5] = br
    termios.tcsetattr(fd, termios.TCSADRAIN, caps)

def send_byte(c):
#    print hex(ord(c))
    if os.write(fd,c) != 1:
        print 'Failed to send a char'
        raise IOError
    else:
        ec = os.read(fd,1)
        if ec != c:
            print 'Wrong echo'
            raise IOError


def send_str(s):
    for b in s[:]:
        send_byte(b)

if len(argv) == 2:
    bfile=open(argv[1],"rb")
    buf=bfile.read()
    bfile.close()

    set_baud(termios.B9600)
    # Password
    if verbose:
        print 'Sending password'
    send_str('\xfe\xed\xfa\xce\xca\xfe\xbe\xef')

    # Load address
    if verbose:
        print 'Sending start address'
    send_str('\x40\x00\x00\x00')

    # Length
    if verbose:
        print 'Sending length'
    send_str(struct.pack('>I',len(buf)))

    # Content
    if verbose:
        print 'Sending binary'
    send_str(buf)

set_baud(speed)
if verbose:
    print 'Terminal...'

def terminal():
    while 1:
        i,o,e=select.select([stdin, fd],[],[])
        for s in i:
#            print '[',s,':',
            c = os.read(s,1)
#            print hex(ord(c)),']'
            if s == fd:
                if ord(c) == 26: # ^Z
                    print '[Rebooted]'
#                   return
                sys.stdout.write(c)
                sys.stdout.flush()
            elif s == stdin:
                os.write(fd,c)

try:
    stdin = sys.stdin.fileno()
    oldSettings = termios.tcgetattr(stdin)
    new = list(oldSettings)
    new[3] = new[3] & ~termios.ECHO # lflags
    new[3] = new[3] & ~termios.ICANON # lflags
    new[3] = new[3] | termios.ISIG
    termios.tcsetattr(stdin, termios.TCSADRAIN, new)

    terminal()

    # Flush input
    print 'Flushing'
    while 1:
        i,o,e=select.select([fd],[],[],0.1)
        if len(i) == 0:
            break
        c = os.read(fd,1)
        sys.stdout.write(c)
    sys.stdout.flush

finally:
    termios.tcsetattr(stdin, termios.TCSADRAIN, oldSettings)
    print

