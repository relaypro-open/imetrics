from __future__ import division
import sys
from pprint import pprint as pp
import requests
import re
import string
import operator
import getopt
import time

class hist():
    def __init__(self, data):
        self.mi = float(data['m'])
        self.ma = float(data['M'])
        self.num = int(data['n'])
        self.data = data
        del self.data['m']
        del self.data['M']
        del self.data['n']
        data2 = {}
        for key, value in self.data.iteritems():
            data2[int(key)] = int(value)
        self.data = data2
        return

    def subtract(self, h1):
        for key, value in h1.data.iteritems():
            try:
                IntKey = int(key)
                self.data[IntKey] = self.data[IntKey] - h1.data[IntKey]
            except:
                continue
        return

    def __str__(self):
        max_width = 80
        val_max = max(self.data.iteritems(), key=operator.itemgetter(1))[1]
        s = ""
        for x in range(0, self.num+2):
            bucket_count = 0
            if val_max > 0:
                bucket_count = int(round((max_width * self.data.get(x, 0)) / val_max))
            bar = '#' * bucket_count
            bucket_desc = (x-1) * (self.ma-self.mi) / self.num
            if x == 0:
                bucket_desc = float('nan')
            elif x == self.num+1:
                bucket_desc = float('nan')
            line = '% 8.2f %s\n' % (bucket_desc, bar)
            s = s + line
        return s

class hist_print():
    def __init__(self):
        return

    def get_imetrics(self, name, use_interval, interval):
        r = requests.get('http://localhost:8085/imetrics/varz:get')
        match = re.search('^'+name+' (.*)$', r.text, flags=re.MULTILINE)
        hist_line = match.group(1)
        split = string.split(hist_line)
        data = {}
        for item in split:
            match = re.search('^(.*):(.*)$', item, flags=0)
            data[match.group(1)] = match.group(2)
        h = hist(data)

        if use_interval:
            time.sleep(interval)
            h1none, h2 = self.get_imetrics(name, False, interval)
            return (h, h2)

        return (None, h)


if __name__ == '__main__':
    try:
        arglist, args = getopt.getopt(
                sys.argv[
                    2:], "i:", [
                        "interval="])
    except:
        print "Invalid Option!"
        exit(1)

    use_interval = False
    interval = 1.0

    for (field, val) in arglist:
        if field in ("-i", "--interval"):
            use_interval = True
            interval = float(val)

    hp = hist_print()
    name = sys.argv[1]
    h1, h2 = hp.get_imetrics(name, use_interval, interval)
    if h1 is not None:
        h2.subtract(h1)
    print '%s' % str(h2)
