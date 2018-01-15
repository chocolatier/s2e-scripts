from z3 import *

f = open("minimal.smt2").read()

expr = parse_smt2_string(f)

simplified_expr = simplify(expr)

# print (expr)
#
# print ("-------------------------------------------------------------------")

print(simplified_expr.sexpr())

g = open("simplified_query4.smt2", 'w')

g.write(str(simplified_expr.sexpr()))
g.close();
