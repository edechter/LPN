
import numpy as np

frac_first_19=0.75
total_count = 4000
exponential_decay_constant=1/2.

if __name__=="__main__":
    xs = np.random.geometric(p=exponential_decay_constant, 
                             size=np.floor(total_count*frac_first_19))
    xs = xs[xs<100]
    xs = xs[xs > 0]
    
    ys = np.random.choice(range(1, 100), size=np.floor(total_count-xs.size))

    zs = np.concatenate([xs, ys])
    
    (cnts, bins) = np.histogram(zs, range(1, 101))
    print cnts, bins
    
    


