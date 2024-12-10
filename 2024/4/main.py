f = open("input")
s = f.read().splitlines()

def inside(x, y):
    return  y >= 0 and y < len(s) and x >= 0 and x < len(s[y])

def N(x, y): return x, y - 1
def S(x, y): return x, y + 1
def W(x, y): return x - 1, y
def E(x, y): return x + 1, y
def NW(x, y): return N(*W(x, y))
def NE(x, y): return N(*E(x, y))
def SW(x, y): return S(*W(x, y))
def SE(x, y): return S(*E(x, y))

def check(x, y, c): return inside(x, y) and (s[y][x] == c)

# Solution 1
def checkPattern1(x, y, pattern, update):
    for c in pattern:
        if not check(x, y, c): return False
        x, y = update(x, y)
    return True

updates = [N, S, W, E, NW, NE, SW, SE]

num = 0
for y in range(len(s)):
    for x in range(len(s[y])):
        for update in updates:
            if checkPattern1(x, y, "XMAS", update):
                num = num + 1

print("Solution 1: ", num)

# Solution 2
def checkPattern2(x, y):
    if not check(x, y, 'A'): return False
    
    x, y = NW(x, y)
    if not check(x, y, 'M'):
        if not check(x, y, 'S'): return False
        x, y = SE(*SE(x, y))
        if not check(x, y, 'M'): return False
    else:
        x, y = SE(*SE(x, y))
        if not check(x, y, 'S'): return False
    x, y = NW(x, y)

    x, y = NE(x, y)
    if not check(x, y, 'M'):
        if not check(x, y, 'S'): return False
        x, y = SW(*SW(x, y))
        if not check(x, y, 'M'): return False
    else:
        x, y = SW(*SW(x, y))
        if not check(x, y, 'S'): return False
    x, y = NE(x, y)

    return True
        
num = 0
for y in range(len(s)):
    for x in range(len(s[y])):
        if checkPattern2(x, y):
            num = num + 1

print("Solution 2: ", num)