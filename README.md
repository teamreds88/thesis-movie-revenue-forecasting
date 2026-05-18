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

Predicting box office revenue before a film releases is a high-stakes problem for content acquisition teams: decisions worth tens of millions of euros are made on incomplete information. This project builds a machine learning pipeline that forecasts a movie's **opening weekend revenue** using only signals available **before** the theatrical release date — no post-release data is used anywhere in the model.

The core hypothesis is that audience sentiment expressed in YouTube trailer comments, combined with structural features (star power, distributor strength, release timing, competition), can meaningfully predict opening weekend performance. The project was completed as an MSc thesis in partnership with a European media company, giving access to proprietary box office data for model training and evaluation.

---

## Key Results

> **XGBoost with BERT-derived sentiment features achieved a 37% lower MAE than the baseline OLS regression model.**

Adding YouTube comment buzz variables (volume + BERT sentiment) consistently improved every model's predictive accuracy:

| Model | MAE without Buzz | MAE with Buzz | Improvement |
|-------|-----------------|---------------|-------------|
| Linear Regression (baseline) | 133.5% | 120% | −10% |
| Ridge Regression | 126% | 109% | −13% |
| Lasso Regression | 129% | 119% | −8% |
| Elastic Net | 128% | 119% | −7% |
| Random Forest | 123% | 107% | −13% |
| **XGBoost** | **132%** | **110%** | **−17%** |

*MAE expressed as % of true revenue on held-out test set.*

![MAE Comparison](results/figures/mae_buzz_comparison.png)

The chart above shows that every single model improves when buzz variables are added — confirming that pre-release YouTube comment signals carry genuine predictive signal beyond what structural features alone can capture.

---

## BERT Sentiment Classifier

Before sentiment features could enter the forecasting models, a domain-specific sentiment classifier was needed. Generic pre-trained classifiers perform poorly on informal YouTube comment language (slang, emoji, hyperbole). The solution was a two-stage approach:

1. **Weak labelling** — VADER was used to generate noisy sentiment labels at scale across the full comment corpus
2. **BERT fine-tuning** — `bert-base-uncased` was fine-tuned on the weak labels using Optuna hyperparameter search across 8 trials

The resulting classifier achieves strong three-class (Positive / Neutral / Negative) classification performance:

![BERT Confusion Matrix](results/figures/confusion_matrix.png)

The model correctly classifies the large majority of comments in all three classes, with most errors being adjacent-class confusions (e.g. Neutral predicted as Positive) rather than polar errors — which is the best-case failure mode for downstream sentiment aggregation.

---

## Feature Importance

Two complementary importance metrics were computed for the best-performing XGBoost model: **Gain** (how much each feature contributes to splits) and **Permutation Importance** (how much test RMSE degrades when a feature is randomly shuffled).

![XGBoost Feature Importance](results/figures/XGBoost_Importance_Comparison.png)

Both methods agree on the top driver: **number of opening locations** (`opening_locs`) dominates by a large margin — reflecting that wide theatrical distribution is both a signal of studio confidence and a direct amplifier of opening weekend revenue. Beyond distribution scale, **release week**, **run time**, **pandemic era**, and **sequel status** all feature consistently across methods. Star power (`log_star_power_count`) and distributor power appear in the top 10 but have more moderate effects.

The feature importance grid below shows the top 10 drivers across all six model families, confirming that the opening locations signal is not an XGBoost artefact — it appears as the dominant feature consistently:

![Variable Importance Grid](results/figures/variable_importance_grid.png)

---

## ALE Plots — How Features Drive Revenue

Accumulated Local Effects (ALE) plots reveal the **direction and magnitude** of each feature's causal effect on predicted log revenue, averaged across the data distribution. These are model-agnostic and robust to correlated features.

![ALE Plots](results/figures/ALE_Plots_XGBoost_5Features.png)

Key insights from the ALE analysis:

- **Opening Locations** — strong, monotonic positive effect. Each additional opening location increases predicted revenue substantially, with the effect accelerating above ~100 locations. This is the single most impactful feature.
- **Release Week** — non-linear seasonal pattern with clear peaks in weeks 15–20 (spring blockbuster season) and around week 50 (holiday season). The effect oscillates by up to ±0.15 log-revenue units, confirming that release timing matters independently of film quality.
- **Movie Run Time** — generally positive and near-linear above 100 minutes. Longer films (which tend to be prestige or event releases) are associated with higher revenue.
- **Star Power** — log-linear positive effect. The benefit of having a top star rises steeply at low levels (going from zero to one A-list actor matters a lot) and continues to grow but with diminishing returns.
- **Same-Week Competition** — negative effect, especially at 1–2 competing films. Films releasing into a crowded weekend face meaningful revenue penalties; the effect flattens at higher competition counts, suggesting audiences self-select toward preferred films regardless.

---

## Sentiment vs Revenue

The scatter plots below show the raw relationship between the three BERT-derived sentiment proportions and log opening weekend revenue across all movies in the dataset:

![Sentiment Scatter](results/figures/sentiment_scatter_combined_cowplot.png)

Positive sentiment proportion shows a weak positive association with revenue; negative sentiment shows a weak negative association. The relationships are noisy at the individual film level — which is expected, as sentiment is one signal among many — but the aggregate signal is consistent enough to improve model predictions when combined with structural features.

---

## Methodology

### Data Sources
- **YouTube trailer comments** — scraped via the YouTube Data API v3 from official movie trailers, filtered to English-language comments published before the film's release date. ~1.9 million comments collected across 17 batches covering 2019–2024.
- **Public movie metadata** — box office distributor rankings, cast and director data, release schedules (scraped from [The Numbers](https://www.the-numbers.com))
- **Proprietary box office data** — internal revenue figures provided by the media company partner (not included in this repository; see `data/README.md`)

### Python Pipeline (notebooks run in order)

| Step | Notebook | Description |
|------|----------|-------------|
| 1 | `01_scraping_trailer_comments.ipynb` | Collect pre-release YouTube comments via the Data API v3; filter by language and date |
| 2 | `02_weak_labelling_vader.ipynb` | Apply VADER sentiment scores to generate weak labels for BERT training data |
| 3 | `03_holiday_control.ipynb` | Engineer a binary holiday-release flag using the `holidays` package |
| 4 | `04_vader_testing.ipynb` | Exploratory calibration of VADER on movie comment language and emoji handling |
| 5 | `05_bert_finetuning.ipynb` | Fine-tune `bert-base-uncased` on weak labels using Optuna hyperparameter search; generate sentiment scores for the full corpus |

### R Pipeline (scripts run in order)

| Step | Script | Description |
|------|--------|-------------|
| 6 | `06_scraping_control.R` | Scrape distributor market share and annual movie release tables from The Numbers |
| 7 | `07_modelling_and_evaluation.R` | Feature engineering, model training (LASSO, Ridge, Elastic Net, Random Forest, XGBoost), evaluation, ALE plots, and feature importance analysis |

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
    ├── figures/              # ALE plots, feature importance, confusion matrix, MAE chart
    └── metrics/
```

**Why both Python and R?** Scraping and deep learning (BERT, API calls) were implemented in Python where the ecosystem is strongest. Feature engineering, statistical modelling, and visualisation were done in R using `tidyverse`, `glmnet`, `ranger`, and `xgboost`, which offer mature interfaces for regularised regression and interpretable ML.

---

## How to Run

### Python setup

```bash
python -m venv venv
source venv/bin/activate   # Windows: venv\Scripts\activate
pip install -r requirements.txt
```

Set your YouTube Data API key before running the scraping notebook:

```bash
export YOUTUBE_API_KEY=your_api_key_here
```

Run notebooks in order (1 → 5) from the `python/` directory using `jupyter notebook`.

### R setup

```r
pkgs <- readLines("r_requirements.txt")
pkgs <- pkgs[!grepl("^#|^$", pkgs)]
install.packages(pkgs)
```

Run scripts in order (6 → 7) from the `R/` directory. Update the `setwd("../data")` call at the top of each script to point to your local data folder.

---

## Data

The public data included in this repository:
- **YouTube trailer comments** — scraped from publicly accessible YouTube videos using the official Data API (~1.9M comments, 2019–2024)
- **Public movie metadata** — release dates, distributor rankings, star and director power scores from public box office databases

**Proprietary data used during the thesis is excluded from this repository.** This includes internal viewership data, territory-level revenue figures, and ticketing data provided by the media company partner. Each excluded file is clearly marked with a `# NOTE: This file is not included in the public repository` comment in the relevant R scripts. Models trained on the full dataset may not be fully reproducible from the public data alone.

See [`data/README.md`](data/README.md) for a full list of datasets, sources, and exclusions.

---

## Tech Stack

| Layer | Tools |
|-------|-------|
| Data collection | YouTube Data API v3, `rvest`, `httr` |
| NLP / Sentiment | VADER (`vaderSentiment`), BERT (`bert-base-uncased` via HuggingFace `transformers`) |
| Hyperparameter search | `optuna` (8 trials, Bayesian optimisation) |
| ML models | LASSO / Ridge / Elastic Net (`glmnet`), Random Forest (`ranger`), XGBoost (`xgboost`) |
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
