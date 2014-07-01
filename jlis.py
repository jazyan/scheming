z = raw_input()

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
        if token == '+':
            return (calc(t[0]) + calc(t[1]))
        elif token == '-':
            return (calc(t[0]) - calc(t[1]))
        elif token == '/':
            return (calc(t[0]) / calc(t[1]))
        elif token == '*':
            return (calc(t[0]) * calc(t[1]))
        else:
            return SyntaxError("only these functions for now")
    elif type(token) == list:
        return calc(token)
    else:
        try: return int(token)
        except ValueError: return SyntaxError("only calculator for now")

def atom(token):
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return str(token)

print "answer!", calc(parse(tokenize(z)))
