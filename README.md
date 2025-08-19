# Research01: Competitive Advantage Analysis

## Overview
This repository contains the code and data for analyzing competitive advantage in construction enterprises using NCA (Necessary Condition Analysis) and QCA (Qualitative Comparative Analysis) methods.

## Project Structure

```
.
├── data/                    # Data files
│   ├── raw.csv             # Original data
│   └── rev01-06.csv        # Revised versions
├── results/                 # Analysis results
│   ├── NCA results.csv    # NCA analysis results
│   └── bottleneck*.csv    # Bottleneck analysis
├── Python_scripts/          # Python analysis scripts
│   ├── run_analysis.py    # Main entry point
│   └── taoyan_*.py        # NCA analysis implementations
├── R_scripts/              # R analysis scripts
│   ├── NCA_analysis.R     # R-based NCA analysis
│   └── qca_*.R           # QCA analysis scripts
└── md文件/                 # Documentation (Chinese)
```

## Quick Start

### Python Analysis
```bash
cd Python_scripts
python run_analysis.py
```

### R Analysis
```r
source("R_scripts/run_r_analysis.R")
```

## Requirements

### Python
- pandas
- numpy
- scipy

### R
- NCA package
- QCA package
- dplyr, readr, tidyr

## Key Findings
The analysis reveals that all 9 condition variables are non-necessary conditions for high competitive advantage, supporting the configurational theory that competitive advantage emerges from combinations of conditions rather than single necessary conditions.

## Data
- Based on 961 observations from Chinese construction enterprises (2014-2023)
- Variables include revenue diversity, dynamic capabilities, ESG performance, etc.

## Methods
- NCA: Following Dul (2016) and Taoyan et al. (2024) methodology
- QCA: fsQCA for exploring sufficient configurations

## Citation
If you use this code or data, please cite the relevant research papers.

## License
Research use only

## Contact
For questions about the code or analysis, please open an issue.