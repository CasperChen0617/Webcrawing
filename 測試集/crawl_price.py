# 用function將程式碼打包，方便我們多次使用，這行的意思就是宣告一個可呼叫的程式區塊，叫做crawl_price(stock_id)


def crawl_price(stock_id):

    import requests
    import pandas as pd
    import json

    url = "https://query1.finance.yahoo.com/v8/finance/chart/"+stock_id + \
        "?period1=0&period2=1549258857&interval=1d&events=history&=hP2rOschxO0"

    res = requests.get(url)
    data = json.loads(res.text)
# 用dataframe取股價各值
    df = pd.DataFrame(data['chart']['result'][0]['indicators']['quote'][0], index=pd.to_datetime(
        data['chart']['result'][0]['timestamp'], unit='s'), columns=['open', 'high', 'low', 'close', 'volume'])
# 將小時拔掉！！！！！！！！！！！我debug de了3小時，注意看
    df.index = df.index.normalize()
# 日期索引值欄位名稱設定
    df = df.rename_axis("Date")
    # df.index.dt.strftime('%y-%m-%d')
# 開高低收設定
    # df=df.rename_axis("stock_price", axis="columns")
    return df
