from z3 import *

# f = open("4_simplification_attempt.smt2").read()
f =  open("sign_extend.smt2").read()
expr = parse_smt2_string(f)

simplified_expr = simplify(expr)

# print (expr)
#
# print ("-------------------------------------------------------------------")

print(simplified_expr.sexpr())

g = open("pprinted_query4.smt2", 'w')

g.write(str(expr.sexpr()))
g.close();
