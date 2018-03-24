# Reference File for Basic Plotting
# You'll see similarities here to Matlab plotting with this library
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt

# generating 100 standard normally distributed random numbers as a NumPy ndarray
np.random.seed(1000)
y = np.random.standard_normal(100)

x = range(len(y))
plt.figure(1)
plt.plot(x,y)

#below is same as above b/c plot takes index values as the respective x values
plt.figure(2)
plt.plot(y)

#Don't forget that you can methods to this as such below on line 21
plt.figure(figsize=(7,4))
plt.plot(y.cumsum())
plt.title('Basic Plotting 101')
plt.xlabel('x-axis')
plt.ylabel('y-axis')
plt.grid(True)
plt.axis('tight')
plt.xlim(-1,20)
plt.ylim(-3,1)


#LaTeX is excellent for text rendering

plt.show()


