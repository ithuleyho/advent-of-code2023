# filename = "test"
filename = "day03-part1"

with open(f"inputs/{filename}") as f:
    lines = f.readlines()

lines = list(map(lambda line: "." + line.strip() + ".", lines))
d = len(lines[0]) * "."
lines.insert(0, d)
lines.append(d)

def isNextToSymbol(lines, i, j):
    if not lines[i][j].isdigit():
        return False

    for y in range(3):
        for x in range(3):
            c = lines[i + y - 1][j + x - 1] 
            if not(c.isdigit() or c == '.'):
                return True

    return False

def pprint(lines):
    for line in lines:
        for c in line:
            print(c, end="")          
        print()

acc = ""
s = 0
nextToSymbol = False

for i in range(len(lines)):
    for j in range(len(lines[0])):
        c = lines[i][j]
        if c.isdigit():
            acc += c
            nextToSymbol |= isNextToSymbol(lines, i, j)
        else:
            if nextToSymbol:
                s += int(acc)
            nextToSymbol = False
            acc = ""
   

print("part 1:", s)

def findNeighborGear(lines, i, j):
    if not lines[i][j].isdigit():
        return False

    for y in range(3):
        for x in range(3):
            c = lines[i + y - 1][j + x - 1] 
            if c == '*':
                return (i + y - 1, j + x - 1)

    return False
    
gears = {}
acc = ""
nextToGear = False

for i in range(len(lines)):
    for j in range(len(lines[0])):
        c = lines[i][j]
        if c.isdigit():
            acc += c
            res = findNeighborGear(lines, i, j)
            if res:
                nextToGear = res
        else:
            if nextToGear:
                if nextToGear not in gears:
                    gears[nextToGear] = []
                gears[nextToGear].append(int(acc))
            nextToGear = False
            acc = ""
s = 0
for val in gears.values():
    if len(val) == 2:
        s += val[0] * val[1]
print(s)
