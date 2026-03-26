import pandas as pd
from pyarrow.parquet import ParquetFile
import pyarrow.parquet as pq
import pyarrow as pa 

#['paperid', 'doi', 'year', 'date', 'doctype', 'cited_by_count', 'is_retracted', 'reference_count', 'citation_count', 'C3', 'C5', 'C10', 'disruption', 'Atyp_Median_Z', 'Atyp_10pct_Z', 'Atyp_Pairs', 'WSB_mu', 'WSB_sigma', 'WSB_Cinf', 'SB_B', 'SB_T', 'team_size', 'institution_count', 'patent_count', 'newsfeed_count', 'nct_count', 'nih_count', 'nsf_count']


test = 'output'

if test == 'input_top_rows':
    col_subset = ['paperid', 'doi']
    file_path = "/Volumes/LaCie/SciSciNet/sciscinet_papers.parquet"
    pf = ParquetFile(file_path) 
    first_ten_rows = next(pf.iter_batches(batch_size = 100)) 
    df = pa.Table.from_batches([first_ten_rows]).to_pandas() 
    print(df)

if test == 'output':
    output_path = 'data/filtered_papers.parquet'
    table = pq.read_table(output_path)
    df_check = pd.read_parquet(output_path)
    print(df_check.head(10)) 
    print(f"Number of rows: {len(df_check)}")

    # Count missing / non-missing
    total_rows = len(df_check)
    non_missing = df_check.notna().sum()
    missing = df_check.isna().sum()
    percent_non_missing = (non_missing / total_rows * 100).round(2)
    
    print("🔍 Non-missing values per column:")
    print(pd.DataFrame({
        "non_missing": non_missing,
        "missing": missing,
        "percent_non_missing": percent_non_missing
    }))
