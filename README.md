# DV495_EFLR_Agency
# Dissertation: Women's Agency as a Conversion Factor in Economic Development  This repository contains all code and documentation for my MSc dissertation investigating whether increases in women's agency shape the extent to which local economic opportunities reduce multidimensional poverty in Ethiopia.  

# Women's Agency as a Conversion Factor (Ethiopia)

This repository contains code for my MSc dissertation investigating whether women's agency
shapes the extent to which local economic opportunities reduce multidimensional poverty.

**Data are not included** due to DHS licensing and sensitivity. The repo contains only code.

## Methods
- Staggered DiD (Callaway & Sant'Anna, 2021) for the effect of the Revised Family Code on agency.
- 2SLS interaction model to test whether agency amplifies the poverty-reducing effect of local economic activity (proxied by night-time lights).
- Agency index (Alkire–Foster approach with Kabeer-inspired domains).
- MPI (OPHI/UNDP methodology).

## Structure
See `/code/r` and `/code/stata` for parallel pipelines; `/data_raw` and `/data_work` are ignored by Git; outputs in `/outputs`.

## Reproducibility
- R: `renv` for package management.
- Stata: versioned do-files with centralized paths in `code/stata/00_setup.do`.

## How to run (local)
1. Place DHS and NTL files in `data_raw/` (see docs/notes for expected filenames).
2. In R: run `code/r/01_build_empowerment_index.R`, then subsequent scripts in order.
3. In Stata: run `do code/stata/01_clean_dhs.do` etc. after `00_setup.do`.
