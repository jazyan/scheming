# A limited Scheme interpreter written in Python
# Lots of code from http://norvig.com/lispy.html.
# Mostly an exercise for me to understand how interpreters work :)

# NOTE: lists are defined by (list (1 2 3)) instead of '(1 2 3)

import operator as op

binop = {'+': op.add, '-': op.sub, '/': op.div, '*': op.mul, '%': op.mod,
        '<': op.lt, '<=': op.le, 'eq?': op.eq, 'neq?': op.ne, '>': op.ge,
        '>=': op.gt, 'cons': lambda x, y: [x] + y}

unop = {'car': lambda x: x[0], 'cdr': lambda x: x[1:]}

# the environment: needed for variables
class Env(dict):
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms, args))
        self.outer = outer
    def find(self, var):
        return self if var in self else self.outer.find(var)

# one-line lexer
def tokenize(s):
    return s.replace("(", " ( ").replace(")", " ) ").split()

# creates an AST in the form of a list of lists
def parse(t):
    if len(t) == 0:
        raise SyntaxError('unexpected EOF')
    token = t.pop(0)
    if token == '(':
        L = []
        while t[0] != ')':
            L.append(parse(t))
        t.pop(0)
        return L
    elif token == ')':
        raise SyntaxError('unexpected EOF')
    else:
        return atom(token)

# types
def atom(token):
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return str(token)

# the meat of the interpreter
def interpret(t, env = Env()):
    if type(t) == str:                  # if it is a symbol
        return env.find(t)[t] #NOTE do not yet understand this
    elif type(t) != list:               # if it is a constant
        return t
    elif t[0] in binop:                 # check the binops
        (_, exp1, exp2) = t
        e1 = interpret(exp1, env)
        e2 = interpret(exp2, env)
        return binop[t[0]](e1, e2)
    elif t[0] in unop:                  # check the unops
        (_, exp) = t
        e = interpret(exp, env)
        return unop[t[0]](e)
    elif t[0] == 'list':                # making list a unop does not work
        (_, exp) = t
        return list(exp)
    elif t[0] == 'quote':
        (_, exp) = t
        return exp
    elif t[0] == 'cond':
        (_, test, result, alt) = t
        return interpret((result if eval(test, env) else alt), env)
    elif t[0] == 'define':
        (_, var, exp) = t
        env[var] = interpret(exp, env)
    elif t[0] == 'lambda':
        (_, var, exp) = t
        return lambda *args: interpret(exp, Env(var, args, env))
    else:
    # the below (as I understand) is for calling user-defined functions
        exps = [interpret(exp, env) for exp in t]
        proc = exps.pop(0)
        return proc(*exps)

# creating a REPL
while True:
    val = interpret(parse(tokenize(raw_input('jlis.py> '))))
    if val is not None: print val
