#!/usr/bin/env python3
import os
import glob
import json
import argparse
from tqdm import tqdm

def combine_folder(folder_path, recursive=False):
    """
    Load & concatenate all JSON arrays in folder_path/*.json
    If recursive=True, globs **/*.json instead.
    """
    pattern = '**/*.json' if recursive else '*.json'
    files = glob.glob(os.path.join(folder_path, pattern), recursive=recursive)
    combined = []
    for fp in tqdm(files, desc=f"Combining {os.path.basename(folder_path)}"):
        try:
            with open(fp, 'r', encoding='utf-8') as f:
                data = json.load(f)
            if isinstance(data, list):
                combined.extend(data)
            else:
                combined.append(data)
        except Exception as e:
            print(f"⚠️  Skipping {fp}: {e}")
    return combined

def process_combiner(input_dir=None, output_dir=None):
    os.makedirs(output_dir, exist_ok=True)

    # 1) metadata (non-recursive)
    md_in = os.path.join(input_dir, 'metadata')
    md_all = combine_folder(md_in, recursive=False)
    with open(os.path.join(output_dir, 'bill_metadata.json'), 'w', encoding='utf-8') as f:
        json.dump(md_all, f, indent=2)
    print(f"Wrote {len(md_all)} metadata records → bill_metadata.json")

    # 2) sponsors (non-recursive)
    sp_in = os.path.join(input_dir, 'sponsors')
    sp_all = combine_folder(sp_in, recursive=False)
    with open(os.path.join(output_dir, 'sponsors.json'), 'w', encoding='utf-8') as f:
        json.dump(sp_all, f, indent=2)
    print(f"Wrote {len(sp_all)} sponsor records → sponsors.json")

    # 3) history (non-recursive)
    hist_in = os.path.join(input_dir, 'history')
    hist_all = combine_folder(hist_in, recursive=False)
    with open(os.path.join(output_dir, 'bill_history.json'), 'w', encoding='utf-8') as f:
        json.dump(hist_all, f, indent=2)
    print(f"Wrote {len(hist_all)} history records → bill_history.json")

    # 4) votes (non-recursive)
    votes_in = os.path.join(input_dir, 'votes')
    votes_all = combine_folder(votes_in, recursive=False)
    with open(os.path.join(output_dir, 'votes.json'), 'w', encoding='utf-8') as f:
        json.dump(votes_all, f, indent=2)
    print(f"Wrote {len(votes_all)} vote records → votes.json")

if __name__ == '__main__':
    process_combiner(input_dir='intermediate', output_dir='output')
