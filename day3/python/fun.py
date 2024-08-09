


f = open("../input", "r")
contents = f.readlines()
# print(contents)

x = 1
y = 1
been = {}

def frog(x,y):
 global been
 been[str(x) + "x" + str(y)] = True

frog(x,y) 


#print("iterating over contents\n")

for i in contents[0]:
  # print("i = " + str(i) + "\n")
  if i == '^':
   y = y - 1
   frog(x,y)   
  if i == 'v': 
   y = y + 1
   frog(x,y)   
  if i == '<':
   x = x - 1
   frog(x,y)
  if i == '>':
   x = x + 1
   frog(x,y)
  else: 
   pass

# print(been)
# count keys in dict
print(len(been))




