# Assumes the formulas are all quantifier free.
from __future__ import print_function
import functools
from collections import defaultdict
from z3 import *
import pprint

def depth(e):
    if e.children():
        return 1 + max([depth(x) for x in e.children()])
    else:
        return 0

# Based on the visitor example at examples/python/visitor.py
def get_discrete_constraints(e, seen):
    if e in seen:
        return
    seen[e] = True
    if (not is_and(e)):
        yield e
    else:
        for ch in e.children():
            for e in get_discrete_constraints(ch, seen):
                yield e
        return

f = open("query4.smt2").read()
expr = parse_smt2_string(f);

# Generates ids for expression unique upto constants.
# TODO: Instead of nested lists of decl maybe use primes + FTA?
# To ensure uniqueness each symbol would need to be assigned a
# sequence of primes to account for depth and context.
def generate_expr_id(e):
    return [e.decl()] + [generate_expr_id(ch) for ch in e.children()]

classified = defaultdict(lambda: [])

# Assumption: Similar constraint checks will result in identical constraints.
def classify(gen):
    for x in gen:
        i = generate_expr_id(x)
        classified[str(i)] = classified[str(i)] + [x]

seen2 = {}

# SMT is purely funcitonal -> constants are functions with no application
# Also treat selection of different positions in a bitvec array as a constant
# Opertaing under the assumption that select statements will only be generated
# when reading a byte from the symbolic input.
def get_constants(e):
    if (e.num_args() == 0 or is_app_of(e, Z3_OP_SELECT)):
        return e
    else:
        return [get_constants(x) for x in e.children()]

# Returns the index the first select operator it encounters is reading from
def find_select_index(expr):
    if is_app_of(expr, Z3_OP_SELECT):
        return expr.children()[1]
    elif expr.children():
        for x in expr.children():
            i = find_select_index(x)
            # (i != -1) kept getting interpreted as a boolean expression and
            # throwing an error
            if (not i == -1):
                return i
        return -1
    else:
        return -1

partitioned = defaultdict(lambda: [])

# Partitions the constraints based on which byte they are applied to.
# Opertaing under the assumption that select statements will only be generated
# when reading a byte from the symbolic input.
def partition(gen):
    for x in gen:
        print (x.sexpr())
        i = find_select_index(x)
        partitioned[i] = partitioned[i] + [x]

# classify(get_discrete_constraints(expr, seen2))
partition(get_discrete_constraints(expr, seen2))
pp = pprint.PrettyPrinter(depth=99)
pp.pprint(partitioned)
