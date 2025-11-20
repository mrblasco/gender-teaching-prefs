import pyarrow.dataset as ds
import pandas as pd

dataset_path = 'data/filtered_papers_full_uniq.parquet'

data = ds.dataset(dataset_path, format= "parquet")
table = data.to_table()
df = table.to_pandas()

df_non_missing = df[df["Atyp_10pct_Z"].notna()]
df_non_missing.to_csv("data/sciscinet_atyp_non_missing.csv", index=False)
