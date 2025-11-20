import json
import pandas as pd
import re
import gzip
from tqdm import tqdm

JSON_FILE = "data/syll_doi.json.gz"
CSV_FILE = "data/sciscinet_atyp_non_missing.csv"
OUTPUT_FILE = "matched_output.csv"


def normalize_doi(doi: str) -> str:
    if not isinstance(doi, str):
        return None
    doi = doi.strip().lower()
    doi = re.sub(r"^https?://(dx\.)?doi\.org/", "", doi)
    doi = re.sub(r"^doi\.org/", "", doi)
    return doi


def main():
    print("Loading CSV (creating DOI → Atyp lookup)...")
    df = pd.read_csv(CSV_FILE)
    df["clean_doi"] = df["doi"].astype(str).apply(normalize_doi)
    atyp_lookup = dict(zip(df["clean_doi"], df["Atyp_10pct_Z"]))

    print("Opening JSON and writing output (matches only, clean DOI only)...")
    with gzip.open(JSON_FILE, "rt", encoding="utf-8") as f, open(OUTPUT_FILE, "w") as out:
        out.write("ID,clean_DOI,Atyp_10pct_Z\n")

        first_char = f.read(1)

        if first_char == "[":
            # JSON array mode
            buffer = first_char + f.read()
            entries = json.loads(buffer)
            for entry in tqdm(entries, desc="Processing entries"):
                entry_id = entry["id"]
                for raw_doi in entry["dois"]:
                    clean = normalize_doi(raw_doi)
                    atyp = atyp_lookup.get(clean)
                    if atyp is not None:
                        out.write(f"{entry_id},{clean},{round(atyp, 2)}\n")

        else:
            # Newline-delimited JSON mode
            f.seek(0)
            for line in tqdm(f, desc="Processing entries"):
                line = line.strip()
                if not line:
                    continue
                entry = json.loads(line)
                entry_id = entry["id"]
                for raw_doi in entry["dois"]:
                    clean = normalize_doi(raw_doi)
                    atyp = atyp_lookup.get(clean)
                    if atyp is not None:
                        out.write(f"{entry_id},{clean},{round(atyp, 2)}\n")

    print(f"Done! Output written to: {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
