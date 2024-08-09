


f = open("../input", "r")
contents = f.readlines()
#print(contents)

i = 0 

x1 = 1
y1 = 1

x2 = 1
y2 = 1

been = {}

def frog1(x,y):
 global been
 been[str(x) + "x" + str(y)] = True

def frog2(x,y):
 global been
 been[str(x) + "x" + str(y)] = True

frog1(x1,y1) 
frog2(x2,y2) 

#print("iterating over contents\n")

def santa():
  global i,x1,y1
  c = contents[0][i]
  if c == '^':
   y1 = y1 - 1
   frog1(x1,y1)   
  if c == 'v': 
   y1 = y1 + 1
   frog1(x1,y1)   
  if c == '<':
   x1 = x1 - 1
   frog1(x1,y1)
  if c == '>':
   x1 = x1 + 1
   frog1(x1,y1)
  else: 
   pass
  # next char
  i = i + 1 


def robo():
  global i,x2,y2
  c = contents[0][i]
  if c == '^':
   y2 = y2 - 1
   frog2(x2,y2)   
  if c == 'v': 
   y2 = y2 + 1
   frog2(x2,y2)   
  if c == '<':
   x2 = x2 - 1
   frog2(x2,y2)
  if c == '>':
   x2 = x2 + 1
   frog2(x2,y2)
  else: 
   pass
  # next char
  i = i + 1 

def loop():
  global i
  i = 0
  lenc = len(contents[0])
  while i < lenc:
   santa()
   if i < lenc:
    robo()

# run loop
loop()

# count keys in dict
print(len(been))




