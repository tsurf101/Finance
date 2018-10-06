from fredapi import Fred

fred = Fred(api_key='')
data = fred.get_series('SP500')
print(data)
