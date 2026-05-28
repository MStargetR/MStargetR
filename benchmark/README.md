# MStargetR vs MetaboAnalyst 6.0 — Benchmark

Reproducible benchmark of **MStargetR** against **MetaboAnalyst 6.0**
(`MetaboAnalystR` v4.2.0 engine, run in Docker) covering **feature parity**
and **runtime/memory performance**.

This directory is excluded from the R package build (`^benchmark$` in
`.Rbuildignore`). All result artifacts are written to the external data folder,
**not** the repo.

## What gets compared

| | MStargetR | MetaboAnalyst |
|---|---|---|
| Vendor `.wiff` → mzML → peak table | ✅ (`msConvertR`+`PeakForgeR`) | ❌ |
| ComBat / QC-RLSC batch correction | ✅ | ✅ (head-to-head) |
| QCRFSC (random-forest) correction | ✅ | ❌ |
| PCA | ✅ | ✅ (head-to-head) |
| Pathway/enrichment, ROC, untargeted | ❌ | ✅ |

Because MetaboAnalyst cannot read `.wiff`, the **head-to-head starts from a
shared peak-intensity table** that MStargetR generates; the **full-pipeline
scaling** curve is MStargetR-only.

## Prerequisites

- R with `MStargetR` installed, plus host benchmark deps:
  `install.packages(c("bench","peakRAM","digest","ggplot2","vegan","RhpcBLASctl"))`
  (`qcrlscR` and `sva` for the correction methods).
- **Docker Desktop** running (for MetaboAnalystR, and for the MStargetR vendor
  front end in the full-pipeline scaling).

## Data folder

Set via `MSTARGETR_BENCH_DIR` (defaults to
`%USERPROFILE%/OneDrive - Murdoch University/Desktop/MStargetR_Benchmark`).
Must contain `raw_data/` with the real `.wiff` + `.wiff.scan` files. Results
land in `<bench_dir>/results/`.

## Build the MetaboAnalyst image (once)

```sh
docker build -t mstargetr-bench/metaboanalystr:4.2.0 benchmark/docker
```

## Run order

```sh
# 1. Shared peak table from the real .wiff files (needs Docker+Skyline).
#    Use MSTARGETR_ALLOW_EXAMPLE=1 to smoke-test downstream with bundled data.
MSTARGETR_RUN_PIPELINE=1 Rscript benchmark/01_make_shared_table.R

# 2. Per-stage head-to-head timing + memory at base size.
Rscript benchmark/02_headtohead.R

# 3. Output equivalence (ComBat / QC-RLSC / RSD / PCA).
Rscript benchmark/04_equivalence.R

# 4. Scaling sweeps (expensive — run in background). MSTARGETR_BENCH_QUICK=1
#    for a tiny validation grid.
Rscript benchmark/03_scaling_shared.R
MSTARGETR_RUN_PIPELINE=1 Rscript benchmark/03_scaling_fullpipe.R

# 5. Knit the report.
Rscript -e 'rmarkdown::render("benchmark/05_report.Rmd",
  output_file = file.path(Sys.getenv("MSTARGETR_BENCH_DIR"), "results", "benchmark_report.html"))'
```

## Smoke test the MA runner

```sh
# tiny CSV in the bench dir, then:
docker run --rm -v "<bench_dir>:/data" mstargetr-bench/metaboanalystr:4.2.0 \
  Rscript /data/ma_runner.R --stage=read_sanity \
  --input=/data/shared_table.csv --out=/data/ma_smoke.csv
```

Expect `STAGE_TIME_S=` / `STAGE_PEAK_MB=` on stdout. If a MetaboAnalystR call
errors, `ma_runner.R` prints that function's argument names — use them to align
the stage call to the pinned image version (signatures drift across releases).

## Outputs (`<bench_dir>/results/`)

| File | Content |
|---|---|
| `shared_table_hash.txt` | input provenance + sha256 |
| `bm_headtohead.csv` | per-stage wall-clock + peak RAM, both engines |
| `bm_scaling.csv` + `bm_scaling_*.png`, `bm_memory.png` | scaling curves |
| `bm_res.csv`, `../bm_linear.png`, `../bm_loess.png` | full-pipeline scaling |
| `bm_fullpipe_stages.csv` | msConvertR/PeakForgeR/qcCheckR breakdown |
| `bm_equivalence.csv` + `equivalence_plots/*.png` | numeric agreement |
| `benchmark_report.html` | knitted report |
