import sys
import time

while True:
    #time.sleep(1)
    line = sys.stdin.readline()
    sys.stdout.write('echo: %s\n' % line)
    sys.stdout.flush()