# python plot-lactotroph.py screen  curves
# python plot-lactotroph.py screen  peaks
# python plot-lactotroph.py fig.png curves


import matplotlib.pyplot as plt
import numpy as np
import sys
import subprocess as sp


out = sp.Popen(["./lactotroph-model"]+sys.argv[2:], stdout = sp.PIPE).communicate()[0]

fig = plt.figure()

if sys.argv[2]=="peaks":
   peaks =  np.fromstring(out, dtype=int, sep='\r\n') 
   plt.hist(peaks/1000*0.01)
else:
   curves = np.stack([np.fromstring( x, dtype=np.float64, sep=',') for x in  out.split()],axis=0)
   plt.plot(curves[:,0]/1000, curves[:,1])

if sys.argv[1]=="screen":
    plt.show()
else:
    fig.savefig(sys.argv[1])



