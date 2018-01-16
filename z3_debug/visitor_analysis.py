# Based on the visitor example at examples/python/visitor.py
from __future__ import print_function
from z3 import *

def depth(e):
    if e.children():
        return 1 + max([depth(x) for x in e.children()])
    else:
        return 0

def visitor(e, seen):
    if e in seen:
        return
    seen[e] = True
    # print (depth(e))
    # print (e.sexpr())
    if (not is_and(e)):
        yield e
    else:
        for ch in e.children():
            for e in visitor(ch, seen):
                yield e
        return

f = open("singledigit.smt2").read()
expr = parse_smt2_string(f);

seen = {}
for e in visitor(expr, seen):
    if is_const(e) and e.decl().kind() == Z3_OP_UNINTERPRETED:
        print("Variable", e.sexpr())
    else:
        print(e.sexpr())
