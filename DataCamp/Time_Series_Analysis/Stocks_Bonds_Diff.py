import pandas
import matplotlib

#Import data sets(10 year bond and S&P500) prices using Fred
#Merge the data frames into one -> stocks_and_bonds

# Compute percent change using pct_change()
returns = stocks_and_bonds.pct_change()

# Compute correlation using corr()
correlation = returns['SP500'].corr(returns['US10Y'])
print("Correlation of stocks and interest rates: ", correlation)

# Make scatter plot
matplotlib.pyplot.scatter(returns['SP500'], returns['US10Y'])
matplotlib.pyplot.show()
