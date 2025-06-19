import os, json, argparse, sys
from argparse import RawDescriptionHelpFormatter

def load_json(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        return json.load(f)

def match(rec, filters):
    return all(filters[k] is None or str(rec.get(k,'')).lower() == filters[k].lower()
               for k in filters)

def main():
    p = argparse.ArgumentParser(
        description="Query bill data by uuid, state, session, state_bill_id",
        formatter_class=RawDescriptionHelpFormatter,
        epilog=r"""
Examples:
  # Query by state, session, and bill id
  python code\query.py --state NV --session 70th1999 --state_bill_id AB444 > result.json

  # Query by UUID only and dump to file
  python code\query.py --uuid NV70th1999AB444 > result.json
"""
    )
    p.add_argument("--uuid")
    p.add_argument("--state")
    p.add_argument("--session")
    p.add_argument("--state_bill_id")
    args = p.parse_args()
    filters = {
        "uuid": args.uuid,
        "state": args.state,
        "session": args.session,
        "state_bill_id": args.state_bill_id
    }

    # define input vs. output dirs
    input_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "output"))
    outdir    = os.path.join(input_dir, "query")

    # load all four datasets from input_dir
    meta     = load_json(os.path.join(input_dir, "bill_metadata.json"))
    history  = load_json(os.path.join(input_dir, "bill_history.json"))
    sponsors = load_json(os.path.join(input_dir, "sponsors.json"))
    votes    = load_json(os.path.join(input_dir, "votes.json"))

    # find single metadata record
    md = next((r for r in meta if match(r, filters)), None) or {}
    # find single history entry
    hist = next((r.get("bill_history") for r in history if match(r, filters)), {})

    # flatten sponsors list
    sp = [
        s
        for r in sponsors if match(r, filters)
        for s in r.get("sponsors", [])
    ]
    # flatten votes list
    vt = [
        v
        for r in votes if match(r, filters)
        for v in r.get("votes", [])
    ]

    out = {
        "metadata": md,
        "bill_history": hist,
        "sponsors": sp,
        "votes": vt
    }

    # write to file named by args
    if args.uuid:
        fname = f"{args.uuid}.json"
    else:
        fname = f"{args.state}{args.session}{args.state_bill_id}.json"
    os.makedirs(outdir, exist_ok=True)
    outfile = os.path.join(outdir, fname)
    with open(outfile, 'w', encoding='utf-8') as f:
        json.dump(out, f, indent=2)

    print(f"Wrote output to {outfile}")

if __name__ == "__main__":
    main()
