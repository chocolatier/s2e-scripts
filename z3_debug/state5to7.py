from z3 import *

arr = Array('arr', IntSort(), BitVecSort(8))

s = Solver()

# RestrictAscii stuff
for i in range(0,3):
    index = Int('i_%d' % i)
    s.add(index == i)
    s.add(UGE(arr[index],40), ULE(arr[index],176))

x = Select(arr,0)
s.add(False == (0x0 == (0xff & (ZeroExt(32,x) & ZeroExt(32,x)))))
s.add(Not (SignExt(32,x) <= 0x2f))
s.add(Not (SignExt(32,x) < 0x39))
    
x = Select(arr,1)
s.add(Not (SignExt(32,x) <= 0x2f))
s.add(Not (SignExt(32,x) < 0x39))
    
x = Select(arr,2)
s.add(Not (SignExt(32,x) <= 0x2f))
s.add(Not (SignExt(32,x) < 0x39))

print(s.to_smt2())           
print(s.check())
print(s.model())
