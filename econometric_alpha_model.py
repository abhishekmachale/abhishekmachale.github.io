"""
Econometric Alpha Forecasting Model
Predicts S&P 500 returns using macroeconomic indicators.
"""

import pandas as pd
import statsmodels.api as sm
import yfinance as yf

# Download data
sp500 = yf.download('^GSPC', start='2013-01-01', end='2024-12-31', interval='1mo')
unemployment = yf.download('UNRATE', start='2013-01-01', end='2024-12-31', interval='1mo')
cpi = yf.download('CPIAUCSL', start='2013-01-01', end='2024-12-31', interval='1mo')

# Merge datasets
df = pd.DataFrame()
df['S&P500'] = sp500['Adj Close']
df['Unemployment'] = unemployment['Adj Close']
df['CPI'] = cpi['Adj Close']
df.dropna(inplace=True)

# Create return column
df['Returns'] = df['S&P500'].pct_change().shift(-1)
df.dropna(inplace=True)

# Run regression
X = sm.add_constant(df[['Unemployment', 'CPI']])
y = df['Returns']
model = sm.OLS(y, X).fit()
print(model.summary())

# Predict next month's return
df['Predicted'] = model.predict(X)
print(df[['Returns', 'Predicted']].tail())
