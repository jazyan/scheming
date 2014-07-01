# TODO: multi-line reading
import operator as op
z = raw_input()

binop = {'+': op.add, '-': op.sub, '/': op.div, '*': op.mul, '%': op.mod,
        '<': op.lt, '<=': op.le, 'eq?': op.eq, 'neq?': op.ne, '>': op.ge,
        '>=': op.gt}

# string -> token
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

def calc(t):
    if type(t) == list:
        token = t.pop(0)
    else:
        token = t
    if type(token) == str:
        if token in binop:
            return binop[token](calc(t[0]), calc(t[1]))
        # only works for numbers
        elif token == "quote" or token == '\'':
            return t[0]
        elif token == "cons":
            if (type(t[1]) == list):
                li = calc(t[1])
                li.insert(0, calc(t[0]))
            else:
                li = [calc(t[0])]
                li.append(calc(t[1]))
            return li
        # TODO does not account for (car '(1 2 3)), for which returns '
        # also accepts types other than list
        elif token == "car":
            return t[0][0]
        elif token == "cdr":
            li = t[0]
            li.pop(0)
            return li
        else:
            return token
    elif type(token) == list:
        return calc(token)
    else:
        return token

def atom(token):
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            #TODO: remove double quotes
            return str(token)

print "TOKENS!", tokenize(z)
print "PARSE!", parse(tokenize(z))
print "ANSWER!", calc(parse(tokenize(z)))
