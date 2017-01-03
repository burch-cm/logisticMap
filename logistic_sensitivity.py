# -*- coding: utf-8 -*-
"""
Created on Tue Jan  3 16:41:23 2017

@author: kitt burch
"""
#%%
def logistic_map(r, init = 0.5, steps = 100):
    x_t = [init]
    for i in range(0, steps + 1):
        x_t.append(r * x_t[i] * (1 - x_t[i]))
    return(x_t)
#%%
x1 = logistic_map(r = 3.8, init = 0.5, steps = 1000)
x2 = logistic_map(r = 3.8000001, init = 0.5, steps = 1000)
#%%
import matplotlib.pyplot as plt
# look at the last 100 values of each
x1_plt = x1[-100:]
x2_plt = x2[-100:]
index = list(range(1, len(x1_plt) + 1)) # this is a cool trick
plt.plot(index, x1_plt, label = "r = 3.8")
plt.plot(index, x2_plt, label = "r = 3.8000001")
plt.xlabel("iteration")
plt.ylabel("x-value")
plt.legend()
plt.figure(figsize = (10, 5))