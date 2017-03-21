from __future__ import division
import sys
from pprint import pprint as pp
import requests
import re
import string
import operator

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

    def __str__(self):
        max_width = 80
        val_max = max(self.data.iteritems(), key=operator.itemgetter(1))[1]
        s = ""
        for x in range(0, self.num+2):
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

    def get_imetrics(self, name):
        r = requests.get('http://localhost:8085/imetrics/varz:get')
        match = re.search('^'+name+' (.*)$', r.text, flags=re.MULTILINE)
        hist_line = match.group(1)
        split = string.split(hist_line)
        data = {}
        for item in split:
            match = re.search('^(.*):(.*)$', item, flags=0)
            data[match.group(1)] = match.group(2)
        h = hist(data)
        return h


if __name__ == '__main__':
    hp = hist_print()
    name = sys.argv[1]
    my_hist = hp.get_imetrics(name)
    print '%s' % str(my_hist)
