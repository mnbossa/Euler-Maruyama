# python plot-pituitary.py screen 1 curves
# python plot-pituitary.py screen 1 peaks
# python plot-pituitary.py fig.png 1 curves
# A) --gCa 1   --gSK 3 --kc 0.05
# B) --gSK 2   --gK 4  --gCa 3
# C) --gSK 1.5 --gK 2  --gCa 2
# D) --gSK 3   --gK 0  --gCa 3
# E) --gCa 2.5 --gK 1  --gSK 0.5

# python plot-pituitary.py screen 1 --simTime 90000 curves --gCa 1.1002976287630477 --gK 3.2398755933668673 --gSK 2.6308369648429095 --gKir 0.6889231657480497 --gBK 0.6235830102910045 --gA 23.88520250181707 --gL 0.24274716086400577 --kc 0.16697537643047153 --noise 5.887695689604206

import matplotlib.pyplot as plt
import numpy as np
import sys
import subprocess as sp

out = sp.Popen(["dist/build/pituitary/pituitary"]+sys.argv[3:], stdout = sp.PIPE).communicate()[0]

i = int(sys.argv[2])

def plt_show():
  if sys.argv[1]=="screen":
    plt.show()
  else:
    fig.savefig(sys.argv[1])

if sys.argv[3]=="peaks":
   peaks =  np.stack([np.fromstring( x, dtype=np.float64, sep=',') for x in  out.split()],axis=0)
   if peaks.shape[0] == 1:
     print(peaks[0]) 
   else:
     fig = plt.figure()
     plt.hist(peaks[:,i-1]/1000)
     plt_show()
else:
   fig = plt.figure()
   curves = np.stack([np.fromstring( x, dtype=np.float64, sep=',') for x in  out.split()],axis=0)
   plt.plot(curves[:,0]/1000, curves[:,i])
   plt_show()

