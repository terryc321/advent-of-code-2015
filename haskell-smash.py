
# generate the directories for day6 .. day25 for dayX/aoc2015dayX
# where X from 6 to 25

import os
import subprocess
current_dir = os.getcwd()

for x in range(6,26):
    dir = "day" + str(x)
    hask = "aoc2015day" + str(x)
    os.chdir(dir)
    #subprocess.run(["stack", "new" , hask])
    subprocess.run(["cp", "-v" , ""])
    os.chdir(current_dir)

    
   
