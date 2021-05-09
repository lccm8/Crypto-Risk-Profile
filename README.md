# Crypto-Risk-Portfolio
Risk profile analysis of a tradin bot deployed in a crypto exchange

<em> Risk profile.R </em>

This file will load trading order data from BSZ012.csv and build a daily P&L table, by symbol (instrument), including closed P&L, net floating P&L, cost and total P&L. An Excel file is also created for validation.

<BR>
<span style="text-decoration: underline;"> Analysis: </span>
 
Firstly, the code displays an equity chart, a return histogram and a table of basic statistics parameters for analysis. Following that, risk metrics are displayed (Alpha & Beta to BTCUSD and S&P500, optimal F, max drawdown, VaR, CVaR and Sharpe ratio).

Based on findings, I suggested a hedge of 50% of the total volume using BTCUSD. A simulation is built, a validation file is created and results are presented (risk metrics and return chart):
- Max drawdown reduced from 0.26 to 0.18
- Sharpe ratio improved to 0.049 from 0.046


Make sure you install all packages before!
  
  
  
