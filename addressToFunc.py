import gdb

def parseforks (path):
    f = open(path, "r")
    lines  = f.read().splitlines()
    addrs = list (map (getForkAddr, lines))
    for addr in addrs:
        print(addr)
        gdb.execute('info symbol ' + addr)
    pass

def getForkAddr(s):
    l = s.split(',')
    return (l[1][1:])
