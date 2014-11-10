import numpy as np
from matplotlib import pyplot as plt

from matplotlib import rc
# rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']})
rc('text', usetex=True)
plt.rcParams['text.latex.preamble']=\
                [r"\usepackage{amssymb, amsmath}"]

plt.rc('text', usetex=True)
plt.rc('font', **{'family': 'serif', 'size': 16})

data = np.loadtxt("train_number_net_0006.held_out.csv", delimiter=",")
data.sort(axis=0)
data = data[1:, 1]

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(range(1, len(data) + 1), 
        data, 
        linewidth=2, 
        marker='o')
ax.set_xlabel("Iteration")
ax.set_ylabel(r"$\mathbb{E} \log p(D_{\text{test}})$")
ax.set_ylim(top=1000)
ax.set_xlim(right=20)

# line at 0
[lo, hi] = ax.get_xlim()
ax.plot([lo, hi], [0, 0], 'k--')

fig.savefig("train_number_net_0006.held_out.pdf")

plt.show()
