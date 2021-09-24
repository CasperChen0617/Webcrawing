# 回傳投資組合下的報酬率

# 使用jupyter notebook 要加：
import seaborn as sns
import numpy as np
import json
import pandas as pd
import requests
# %matplotlib inline


def StockReturn(stock, sharpe_smooth_interval, sharpediff_upper_quantile, sharpediff_lower_quantile, sharpediff_delay):
    mean = stock['close'].pct_change().rolling(252).mean()
    std = stock['close'].pct_change().rolling(252).std()
    sr = mean / std * (252**0.5)
    srsma = sr.rolling(sharpe_smooth_interval).mean()

    srsmadiff = srsma.diff() * 100
    ub = srsmadiff.quantile(sharpediff_upper_quantile)
    lb = srsmadiff.quantile(sharpediff_lower_quantile)

    # srsmadiff.shift(d)代表幾天前的夏普
    buy = ((srsmadiff.shift(sharpediff_delay) < lb) & (srsmadiff > ub))
    sell = ((srsmadiff.shift(sharpediff_delay) > ub) & (srsmadiff < lb))

    hold = pd.Series(np.nan, index=buy.index)
    hold[buy] = 1
    hold[sell] = 0
    hold.ffill(inplace=True)

    adj = stock['close'][buy.index]

    eq = (adj.pct_change().shift(-1)+1).fillna(1)[hold == 1].cumprod()
    if len(eq) > 0:
        return eq.iloc[-1]
    else:
        return 1


# 暴力搜尋
def Search(stock):
    maxeq = 0
    ma_smooth = 0
    negative_sign = 0
    positive_sign = 0
    detect_lag = 0
    for a in range(0, 240, 20):
        for b in np.arange(0.3, 1, 0.03):
            for c in np.arange(0, 0.7, 0.03):
                for d in range(0, 180, 20):
                    eq = StockReturn(stock, a, b, c, d)
                    if maxeq < eq:
                        maxeq = eq
                        ma_smooth = a
                        negative_sign = b
                        positive_sign = c
                        detect_lag = d
    print(maxeq, ma_smooth, negative_sign, positive_sign, detect_lag)
