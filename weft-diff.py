#!/usr/bin/env python3
"""Semantic diff of Swift vs Haskell weft analysis outputs."""

import json
import sys


def sort_key(x):
    if isinstance(x, dict):
        return json.dumps(x, sort_keys=True)
    return str(x)


def sorted_val(v):
    if isinstance(v, list):
        normed = [sorted_val(x) for x in v]
        return sorted(normed, key=sort_key)
    if isinstance(v, dict):
        return {k: sorted_val(v2) for k, v2 in sorted(v.items())}
    return v


def fmt(v):
    if isinstance(v, list):
        return "[" + ", ".join(fmt(x) for x in sorted(v, key=sort_key)) + "]"
    if isinstance(v, dict):
        return json.dumps(v, sort_keys=True)
    return str(v)


def diff_entities(swift_list, hask_list, key_field, section_name):
    swift_map = {e[key_field]: e for e in swift_list}
    hask_map = {e[key_field]: e for e in hask_list}
    all_keys = sorted(set(swift_map) | set(hask_map))

    mismatches = []
    for k in all_keys:
        if k not in swift_map:
            mismatches.append((k, "  only in Haskell"))
            continue
        if k not in hask_map:
            mismatches.append((k, "  only in Swift"))
            continue

        s = swift_map[k]
        h = hask_map[k]
        fields = sorted(set(s) | set(h))
        field_diffs = []
        for f in fields:
            if f == key_field:
                continue
            sv = sorted_val(s.get(f))
            hv = sorted_val(h.get(f))
            if sv != hv:
                field_diffs.append(
                    f"    {f}:  Swift={fmt(s.get(f))}  Haskell={fmt(h.get(f))}"
                )
        if field_diffs:
            mismatches.append((k, "\n".join(field_diffs)))

    if not mismatches:
        print(f"{section_name}: all match")
    else:
        print(
            f"{section_name} ({len(mismatches)} mismatch{'es' if len(mismatches) != 1 else ''}):"
        )
        for k, detail in mismatches:
            print(f'  "{k}":')
            print(detail)
    print()


def diff_deps(swift_deps, hask_deps):
    all_keys = sorted(set(swift_deps) | set(hask_deps))
    mismatches = []
    for k in all_keys:
        if k not in swift_deps:
            mismatches.append((k, "    only in Haskell"))
            continue
        if k not in hask_deps:
            mismatches.append((k, "    only in Swift"))
            continue
        sv = sorted(swift_deps[k])
        hv = sorted(hask_deps[k])
        if sv != hv:
            mismatches.append(
                (k, f"    Swift={fmt(swift_deps[k])}  Haskell={fmt(hask_deps[k])}")
            )

    if not mismatches:
        print("dependencies: all match")
    else:
        print(
            f"dependencies ({len(mismatches)} mismatch{'es' if len(mismatches) != 1 else ''}):"
        )
        for k, detail in mismatches:
            print(f'  "{k}":')
            print(detail)
    print()


def main():
    swift_path, hask_path = sys.argv[1], sys.argv[2]
    with open(swift_path) as f:
        swift = json.load(f)
    with open(hask_path) as f:
        hask = json.load(f)

    diff_entities(swift.get("signals", []), hask.get("signals", []), "name", "signals")
    diff_entities(swift.get("bundles", []), hask.get("bundles", []), "name", "bundles")
    diff_entities(
        swift.get("swatches", []), hask.get("swatches", []), "backend", "swatches"
    )
    diff_deps(swift.get("dependencies", {}), hask.get("dependencies", {}))


if __name__ == "__main__":
    main()
