import operator as op

binop = {'+': op.add, '-': op.sub, '/': op.div, '*': op.mul, '%': op.mod,
        '<': op.lt, '<=': op.le, 'eq?': op.eq, 'neq?': op.ne, '>': op.ge,
        '>=': op.gt, 'cons': lambda x, y: [x] + y}

unop = {'car': lambda x: x[0], 'cdr': lambda x: x[1:]}

class Env(dict):
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms, args))
        self.outer = outer
    def find(self, var):
        print self, var
        return self if var in self else self.outer.find(var)

def tokenize(s):
    return s.replace("(", " ( ").replace(")", " ) ").split()

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

def interpret(t, env = Env()):
    if type(t) == str:
        print "hello"
        return env.find(t)[t] #NOTE do not understand this
    elif type(t) != list:
        return t
    elif t[0] in binop:
        (_, exp1, exp2) = t
        e1 = interpret(exp1, env)
        e2 = interpret(exp2, env)
        return binop[t[0]](e1, e2)
    elif t[0] in unop:
        (_, exp) = t
        e = interpret(exp, env)
        return unop[t[0]](e)
    elif t[0] == 'list':
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
        exps = [interpret(exp, env) for exp in t]
        proc = exps.pop(0)
        return proc(*exps)

def atom(token):
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return str(token)
'''
print "TOKENS!", tokenize(z)
print "PARSE!", parse(tokenize(z))
print "ANSWER!", interpret(parse(tokenize(z)))
'''

while True:
    val = interpret(parse(tokenize(raw_input('jlis.py> '))))
    if val is not None: print val
