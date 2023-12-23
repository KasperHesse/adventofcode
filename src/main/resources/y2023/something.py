
from collections import defaultdict

lines = []
with open("d22.txt") as f:
    lines = f.readlines()
    
    
brick = []
#for line in inp.split("\n"):
for line in lines:
    a,b = line.split("~")
    a = list(map(int, a.split(",")))
    b = list(map(int, b.split(",")))
    brick.append((a,b))

n = len(brick)

brick.sort(key=lambda x: x[0][2])

highest = defaultdict(lambda:(0,-1))
bad = set()
graph = [[] for i in range(n)]
for idx,b in enumerate(brick):
    mxh = -1
    support_set = set()
    for x in range(b[0][0], b[1][0]+1):
        for y in range(b[0][1], b[1][1]+1):
            if highest[x,y][0] + 1 > mxh:
                mxh = highest[x,y][0] + 1
                support_set = {highest[x,y][1]}
            elif highest[x,y][0] + 1 == mxh:
                support_set.add(highest[x,y][1])
    
    for x in support_set:
        if x != -1:
            graph[x].append(idx)

    if len(support_set) == 1:
        bad.add(support_set.pop())
    
    fall = b[0][2] - mxh
    if fall > 0:
        b[0][2] -= fall
        b[1][2] -= fall

    for x in range(b[0][0], b[1][0]+1):
        for y in range(b[0][1], b[1][1]+1):
            highest[x,y] = (b[1][2], idx)

print(len(brick)-len(bad)-1) # -1 for "ground" brick

def count(idx, graph):
    indeg = [0 for __ in range(n)]
    for j in range(n):
        for i in graph[j]:
            indeg[i] += 1
    q = [idx]
    count = -1
    while len(q) > 0:
        count += 1
        x = q.pop()
        for i in graph[x]:
            indeg[i] -= 1
            if indeg[i] == 0:
                q.append(i)

    return count

print(sum(count(x, graph) for x in range(n)))
