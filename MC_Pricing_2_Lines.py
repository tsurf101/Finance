# Monte Carlo Options Pricing in Two Lines as created by Dr Tom Starke
from matplotlib.pyplot import *
from numpy import cumprod, random, sqrt, mean
from numpy import *

# There is 252 trading days in a year and Vol is assumed to be 20 here
#Change 1,000 below to 10,000 in order to make your simulation more accurate.

k = cumprod(1 + random.randn(1000, 252) * 0.2 / sqrt(252), 1) * 100
# This is already producing our geometric brownian motion
for i in k:
    plot(i)

show()

# You can switch between a put and call option by using ">" or "<" sign below
print(mean((k[:, -1] - 100) * ((k[:, -1] - 100) > 0)))
