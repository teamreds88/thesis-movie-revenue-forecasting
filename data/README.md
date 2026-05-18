# Data

This folder contains all publicly sourced data used in the thesis pipeline. Proprietary data provided by the media company partner is excluded from this repository.

---

## Files in `data/`

| File | Source | Description |
|------|--------|-------------|
| `all_movies_cleaned_combined.csv` | Derived | Cleaned master list of movie titles and release dates used for scraping |
| `all_titles_post_filtering.csv` | Derived | Movie titles remaining after pre-release eligibility filtering |
| `movies_20XX_subset_clean.csv` (×6, 2019–2024) | [The Numbers](https://www.the-numbers.com) | Annual movie release lists scraped from The Numbers, cleaned |
| `distributors_20XX.csv` (×6, 2019–2024) | [The Numbers](https://www.the-numbers.com) | Annual distributor market share tables scraped from The Numbers |
| `top_1000_grossing_actors.csv` | [The Numbers](https://www.the-numbers.com) | Top 1,000 highest-grossing actors used to construct the star power feature |
| `top_1000_grossing_directors.csv` | [The Numbers](https://www.the-numbers.com) | Top 1,000 highest-grossing directors used to construct the director power feature |

---

## Files in `data/youtube_comments/`

| File | Source | Description |
|------|--------|-------------|
| `batch_1_with_ids.csv` … `batch_17_with_ids.csv` | YouTube Data API v3 | Movie title, release date, distributor, and YouTube trailer video ID for each scraping batch |
| `comments_corrected_21_22.csv` | YouTube Data API v3 | Pre-release English trailer comments scraped for 2021–2022 movies, spell-corrected |
| `comments_corrected_23_24.csv` | YouTube Data API v3 | Pre-release English trailer comments scraped for 2023–2024 movies, spell-corrected |
| `video_ids_with_links.xlsx` | YouTube Data API v3 | Full list of trailer video IDs and YouTube URLs for all movies in the dataset |

### Comment data collection methodology
- Comments were collected using the YouTube Data API v3 (see `python/01_scraping_trailer_comments.ipynb`)
- Only English-language comments published **before the movie's theatrical release date** were retained
- Comments were collected in 17 batches covering movies released 2019–2024
- Spell correction was applied using the `textclean` R package

---

## Excluded data

The following data was used during the thesis but is **not included** in this public repository:

| File | Reason for exclusion |
|------|----------------------|
| `cinemas_data-20250318_Film_Lookup.xlsx` | Proprietary internal data provided by media company partner |
| `super_final_metadata.csv` | Contains internal revenue figures, territory admissions, and booking codes |
| `metadata_with_holiday_flag.csv` | Derived from proprietary internal metadata |
| `super_final_comments.csv` | Too large for GitHub (441 MB) |
| `super_final_comments_labeled.csv` | Too large for GitHub (447 MB); contains BERT sentiment labels |
| `cleaned_new_comments_corrected.csv` | Too large for GitHub (120 MB) |
| `comments_corrected_19_20.csv` | Too large for GitHub (57 MB) |

Models trained on the full dataset (including proprietary features) may not be fully reproducible from the public data alone.
