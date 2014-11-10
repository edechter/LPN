

import numpy as np
from matplotlib import pyplot as plt
import pandas as pd

ec2_data = np.loadtxt("./ec2_data.csv", delimiter=",")

df = pd.DataFrame(ec2_data, columns = ["nExamples", "nPreds", "Acc"])
df = df[df["nExamples"] <600]

grouped = df.groupby(["nPreds", "nExamples"])



grouped_mean = grouped.mean()
grouped_std = grouped.std()
grouped_max = grouped.max()
grouped_min = grouped.min()

fig = plt.figure()
ax = fig.add_subplot(111)
for npred in [1,2,3,4]:
    ax.hold(True)
    ax.bar(grouped_max.loc[npred].index.values+10*npred - 30, 
           grouped_max.loc[npred].values, 
           width=10,
                # yerr=grouped_std.loc[npred].values/10,
           label="%d Predicates"%npred,
           color=[.2*i for i in [npred, npred, npred]],
           linewidth=2
    )
    ax.set_ylim([0, 1])

plt.legend()
    


