import argparse
import pyarrow.dataset as ds
import pandas as pd
import os
import time
import json
import random

# file_path = "/Volumes/LaCie/SciSciNet/sciscinet_papers.parquet"
# doi_file = "data/doi_list_shuf1000.csv"
# doi_column_name = "doi"

def load_doi_list(path):
    ext = os.path.splitext(path)[1].lower()
    if ext == ".csv":
        df = pd.read_csv(path, on_bad_lines='skip')
        if df.shape[1] == 1:
            return df.iloc[:, 0].dropna().unique().tolist()
        elif "doi" in df.columns:
            return df["doi"].dropna().unique().tolist()
        else:
            raise ValueError("CSV must have a single column or a 'doi' column")
    elif ext == ".json":
        with open(path, "r") as f:
            data = json.load(f)
        if isinstance(data, dict):
            # If JSON is like {"dois": ["..."]}
            return list(data.values())[0]
        elif isinstance(data, list):
            return data
        else:
            raise ValueError("Unsupported JSON format for DOI list")
    else:
        raise ValueError("Unsupported file type: must be .csv or .json")

def normalise_doi(dois: list):
    normalized_dois = []
    for d in dois:
        d = str(d).strip()
        if not d.startswith("http://") and not d.startswith("https://"):
            d = f"https://doi.org/{d}"
        normalized_dois.append(d)
    return normalized_dois

def main(dataset_path, doi_file, doi_column, output_path=None):
    t0 = time.time()
    doi_list_raw = load_doi_list(doi_file)
    doi_list = normalise_doi(doi_list_raw)
    t1 = time.time()
    print(f"✅ Loaded and normalised {len(doi_list)} DOIs in {t1 - t0:.4f} seconds")

    print("\nSample DOIs from DOI list:")
    for doi in random.sample(doi_list, min(5, len(doi_list))):
        print(doi)

    t2 = time.time()
    dataset = ds.dataset(dataset_path, format="parquet")
    t3 = time.time()
    print(f"✅ Loaded dataset in {t3 - t2:.4f} seconds")

    t4 = time.time()
    # TODO: if doi_column not in columns, return error
    scanner = dataset.scanner(
        columns = ['paperid', 'doi', 'Atyp_10pct_Z'],
        filter = ds.field(doi_column).isin(doi_list)
    )
    t5 = time.time()
    print(f"✅ Built filter expression in {t5 - t4:.4f} seconds")

    t6 = time.time()
    table = scanner.to_table()
    t7 = time.time()
    print(f"✅ Converted filtered dataset to PyArrow table in {t7 - t6:.4f} seconds")

    t8 = time.time()
    df = table.to_pandas()
    t9 = time.time()
    print(f"✅ Converted table to pandas DataFrame in {t9 - t8:.4f} seconds")

    total_time = t9 - t0
    print(f"\n✅ Total time: {total_time:.4f} seconds")
    print(f"✅ Found {len(df):,} matching rows")


    if output_path:
        t10 = time.time()
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        df.to_parquet(output_path, index=False)
        t11 = time.time()
        print(f"✅ Saved filtered DataFrame to {output_path} in {t11 - t10:.4f} seconds")

    total_time = t11 - t0 if output_path else t9 - t0
    print(f"\n✅ Total time: {total_time:.4f} seconds")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Filter a parquet dataset by DOI list")
    parser.add_argument("--dataset", required=True, help="Path to Parquet dataset")
    parser.add_argument("--doi_file", required=True, help="Path to CSV or JSON containing DOI list")
    parser.add_argument("--doi_column", default="doi", help="Column name in DOI file (default: doi)")
    parser.add_argument("--output", default="filtered_papers.parquet", help="Optional output path to save filtered DataFrame (Parquet recommended)")
    args = parser.parse_args()

    main(args.dataset, args.doi_file, args.doi_column, args.output)