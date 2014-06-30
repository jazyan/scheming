z = raw_input()

# string -> token
def tokenize(s):
    return s.replace("(", " ( ").replace(")", " ) ").split()

print tokenize(z)
