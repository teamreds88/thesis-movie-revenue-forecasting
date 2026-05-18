# Movie Revenue Forecasting Using Pre-Release Signals & NLP

![Python](https://img.shields.io/badge/Python-3.12-blue?logo=python)
![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r)
![Jupyter](https://img.shields.io/badge/Jupyter-Notebook-orange?logo=jupyter)
![scikit-learn](https://img.shields.io/badge/scikit--learn-1.5.1-F7931E?logo=scikit-learn)
![HuggingFace](https://img.shields.io/badge/HuggingFace-Transformers-yellow?logo=huggingface)
![tidyverse](https://img.shields.io/badge/R-tidyverse-1E90FF)
![ggplot2](https://img.shields.io/badge/R-ggplot2-green)
![XGBoost](https://img.shields.io/badge/XGBoost-2.x-red)

---

## Overview

Predicting box office revenue before a film releases is a high-stakes problem for content acquisition teams: decisions worth tens of millions of euros are made on incomplete information. This project builds a machine learning pipeline that forecasts a movie's opening weekend revenue using only signals available **before** the theatrical release date — no post-release data is used anywhere in the model.

The core hypothesis is that audience sentiment expressed in YouTube trailer comments, combined with structural features such as star power, distributor strength, and release timing, can meaningfully predict opening weekend performance. The project was completed as an MSc thesis in partnership with a European media company, giving access to proprietary box office data for model training and evaluation.

---

## Key Results

> **37% reduction in Mean Absolute Error (MAE) compared to a baseline OLS regression model.**

| Model | MAE (relative to baseline) |
|-------|---------------------------|
| Baseline OLS Regression | 1.00× |
| LASSO Regression | ~0.85× |
| Random Forest | ~0.75× |
| **XGBoost** | **0.63×** |

XGBoost with BERT-derived sentiment features achieved the best performance. Accumulated Local Effects (ALE) plots and permutation feature importance were used to interpret which signals drove predictions.

---

## Methodology

### Data Sources
- **YouTube trailer comments** — scraped via the YouTube Data API v3 from official movie trailers, filtered to English-language comments published before the film's release date
- **Public movie metadata** — box office distributor rankings, cast and director data, release schedules (scraped from [The Numbers](https://www.the-numbers.com))
- **Proprietary box office data** — internal revenue figures provided by the media company partner (not included in this repository)

### Python Pipeline (notebooks run in order)

| Step | Notebook | Description |
|------|----------|-------------|
| 1 | `01_scraping_trailer_comments.ipynb` | Collect pre-release YouTube comments via the Data API v3, filter by language and date |
| 2 | `02_weak_labelling_vader.ipynb` | Apply VADER sentiment scores to generate weak labels for BERT fine-tuning training data |
| 3 | `03_holiday_control.ipynb` | Engineer a binary holiday-release flag using the `holidays` package |
| 4 | `04_vader_testing.ipynb` | Exploratory calibration of VADER on movie comment language and emoji handling |
| 5 | `05_bert_finetuning.ipynb` | Fine-tune `bert-base-uncased` on weakly-labelled comments using Optuna for hyperparameter search; generate sentiment scores for the full comment corpus |

### R Pipeline (scripts run in order)

| Step | Script | Description |
|------|--------|-------------|
| 6 | `06_scraping_control.R` | Scrape distributor market share and annual movie release tables from The Numbers |
| 7 | `07_modelling_and_evaluation.R` | Feature engineering, model training (LASSO, Random Forest, XGBoost), evaluation, ALE plots, and feature importance analysis |

---

## Repository Structure

```
thesis-movie-revenue-forecasting/
├── README.md
├── requirements.txt          # Python dependencies
├── r_requirements.txt        # R package list
├── data/
│   ├── README.md             # Dataset descriptions, sources, and exclusions
│   ├── all_movies_cleaned_combined.csv
│   ├── movies_20XX_subset_clean.csv (× 6 years)
│   ├── distributors_20XX.csv (× 6 years)
│   ├── top_1000_grossing_actors.csv
│   ├── top_1000_grossing_directors.csv
│   └── youtube_comments/
│       ├── batch_1_with_ids.csv … batch_17_with_ids.csv
│       ├── comments_corrected_21_22.csv
│       ├── comments_corrected_23_24.csv
│       └── video_ids_with_links.xlsx
├── python/
│   ├── 01_scraping_trailer_comments.ipynb
│   ├── 02_weak_labelling_vader.ipynb
│   ├── 03_holiday_control.ipynb
│   ├── 04_vader_testing.ipynb
│   └── 05_bert_finetuning.ipynb
├── R/
│   ├── 06_scraping_control.R
│   └── 07_modelling_and_evaluation.R
└── results/
    ├── figures/              # ALE plots, feature importance, confusion matrices
    └── metrics/              # Model comparison tables
```

**Why both Python and R?** The scraping and deep learning components (BERT fine-tuning, API calls) were implemented in Python where the ecosystem is stronger. Feature engineering, statistical modelling, and visualisation were done in R using `tidyverse`, `glmnet`, `ranger`, and `xgboost`, which offer mature interfaces for regularised regression and interpretable ML.

---

## How to Run

### Python setup

```bash
# Create and activate a virtual environment
python -m venv venv
source venv/bin/activate   # Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

Set your YouTube Data API key as an environment variable before running the scraping notebook:

```bash
export YOUTUBE_API_KEY=your_api_key_here
```

Run notebooks in order (1 → 5) from the `python/` directory:

```bash
cd python
jupyter notebook
```

### R setup

```r
# Install all required packages
pkgs <- readLines("r_requirements.txt")
pkgs <- pkgs[!grepl("^#|^$", pkgs)]
install.packages(pkgs)
```

Run scripts in order (6 → 7) from the `R/` directory. Update the `setwd("../data")` call at the top of each script if your data is stored elsewhere.

---

## Data

The public data included in this repository covers:
- **YouTube trailer comments** — scraped from publicly accessible YouTube videos using the official Data API
- **Public movie metadata** — release dates, distributor rankings, and star/director power scores derived from public box office databases

**Proprietary data used during the thesis is excluded from this repository.** This includes internal viewership data, territory-level revenue figures, and ticketing data provided by the media company partner. As a result, the R modelling scripts reference files that are not present here — each such file is clearly marked with a `# NOTE: This file is not included in the public repository` comment. Models trained on the full dataset (including proprietary features) may not be fully reproducible from the public data alone.

---

## Tech Stack

| Layer | Tools |
|-------|-------|
| Data collection | YouTube Data API v3, `rvest`, `httr` |
| NLP / Sentiment | VADER (`vaderSentiment`), BERT (`bert-base-uncased` via HuggingFace `transformers`) |
| Hyperparameter search | `optuna` |
| ML models | LASSO (`glmnet`), Random Forest (`ranger`), XGBoost (`xgboost`) |
| Model interpretation | ALE plots (`iml`), permutation importance (`DALEXtra`) |
| Data wrangling | `pandas`, `numpy`, `tidyverse`, `dplyr`, `lubridate` |
| Visualisation | `ggplot2`, `matplotlib`, `seaborn`, `cowplot`, `patchwork` |

---

## Academic Context

**Degree:** MSc Data Science & Marketing Analytics
**Institution:** Erasmus University Rotterdam
**Year:** 2025
**Grade:** 8.0 / 10
**Industry partner:** European media company (anonymised)
