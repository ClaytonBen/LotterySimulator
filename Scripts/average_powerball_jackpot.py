import pandas as pd

df = pd.read_html("https://www.texaslottery.com/export/sites/lottery/Games/Powerball/Estimated_Jackpot.html")


dod = pd.concat(df)

results_df = dod[["Draw Date","Estimated CVO for Advertised Jackpot"]]

results_df.dtypes