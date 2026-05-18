################## Load all necessary packages ##################################
library(stringi)
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(tibble)
library(fuzzyjoin)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)
library(pacman)
library(tidytext)
library(dplyr)
library(stringr)
library(textclean)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(writexl)
library(vader)
library(tidyr)
library(corrplot)
library(caret)
library(rvest)
library(purrr)
library(stringdist)
library(randomForest)
library(ranger)
library(xgboost)
library(glmnet)
library(DALEXtra)
library(mlr3)
library(iml)
library(corrplot)
library(knitr)
library(kableExtra)
library(flextable)
library(ggcorrplot)
library(scales)  
library(gridExtra)  
library(cowplot)
library(patchwork)
library(Ckmeans.1d.dp)
library(stargazer)
library(broom)
library(flextable)



# Set working directory
# NOTE: Update this path to your local data folder before running
setwd("../data")  # Assumes R scripts are run from the R/ folder in the repo

# NOTE: This file is not included in the public repository. See README for details.
meta_data <- read_excel("../Sensitive Data/cinemas_data-20250318_Film_Lookup.xlsx")

# check data structure 
str(meta_data)

# change the column name for release date column
colnames(meta_data)[colnames(meta_data) == "Release Date...1"] <- "release_date"

# Get min and max release dates
min(meta_data$release_date, na.rm = TRUE)
max(meta_data$release_date, na.rm = TRUE)

# clean the names of all columns  
meta_data <- meta_data %>%
  janitor::clean_names()  # converts to snake_case like primary_territories_of_origin


# Create a frequency table of primary territories
territory_summary <- as.data.frame(table(meta_data$primary_territories_of_origin))

# Rename columns for clarity
colnames(territory_summary) <- c("Primary_Territory_of_Origin", "Number_of_Movies")

# Order by frequency (descending)
territory_summary <- territory_summary %>%
  arrange(desc(Number_of_Movies))

# View it nicely
View(territory_summary)

# Display as a thesis-friendly table
territory_summary %>% 
  kable(caption = "Primary Territories of Origin by Number of Movies") %>%
  kable_styling(full_width = FALSE)


# filtering movie data from 2019 to 2024
meta_movies_2019_2024 <- meta_data %>%
  filter(year(release_date) >= 2019 & year(release_date) <= 2024) %>%
  distinct() %>%
  arrange(release_date)

# View summary
nrow(meta_movies_2019_2024)
glimpse(meta_movies_2019_2024)


####### exploratory data analysis ###############
# Count number of titles released per year
meta_movies_2019_2024 %>%
  mutate(release_year = year(release_date)) %>%
  count(release_year)


# Create a bar chart of number of titles released per year
meta_movies_2019_2024 %>%
  mutate(Year = year(release_date)) %>%
  count(Year) %>%
  ggplot(aes(x = as.factor(Year), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Number of Movies Released per Year (2019–2024)",
    x = "Year",
    y = "Number of Movies"
  ) +
  theme_minimal()

# checking how many titles have USA as the primary territory of origin 
meta_movies_2019_2024 %>%
  mutate(hollywood = grepl("USA", primary_territories_of_origin, ignore.case = TRUE)) %>%
  count(hollywood)

# filter down dataset to titles with USA as primary territory of origin
hollywood_movies <- meta_movies_2019_2024 %>%
  filter(grepl("USA", primary_territories_of_origin, ignore.case = TRUE))

# Clean column names to remove hidden characters
colnames(hollywood_movies) <- str_replace_all(colnames(hollywood_movies), "[\r\n]", " ")

# Clean column names to consistent snake_case
hollywood_movies <- hollywood_movies %>%
  clean_names()

# Now rename opening weekend and opening week revenue columns
hollywood_movies <- hollywood_movies %>%
  rename(
    opening_weekend_eur = reported_opening_weekend_gross,
    opening_week_eur = reported_opening_week_gross
  )

# Check correlation between opening weekend revenue and opening weeke revenue 
cor(
  hollywood_movies$opening_weekend_eur,
  hollywood_movies$opening_week_eur,
  use = "complete.obs"
)

# scatter plot of opening weekend revenue vs opening week revenue 
ggplot(hollywood_movies, aes(x = opening_weekend_eur, y = opening_week_eur)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Opening Weekend vs. Opening Week Gross (€)",
    x = "Opening Weekend Gross (€)",
    y = "Opening Week Gross (€)"
  ) +
  theme_minimal()

# opening weekend gross as the dependent variable 
# let's check the distribution of opening weekend gross 
ggplot(hollywood_movies, aes(x = opening_weekend_eur)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Opening Weekend Gross (€)",
    x = "Opening Weekend Gross (€)",
    y = "Number of Movies"
  ) +
  theme_minimal()

# distribution of natural log transformed opening weekend revenues 
ggplot(hollywood_movies, aes(x = log(opening_weekend_eur))) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white", alpha = 0.8) +
  labs(
    title = "Log-Transformed Distribution of Opening Weekend Gross (€)",
    x = "Log(Opening Weekend Gross €)",
    y = "Number of Movies"
  ) +
  theme_minimal()

# removing duplicate title column
hollywood_movies <- hollywood_movies %>%
  select(-title_37)

# renaming primary title column for clean data
hollywood_movies <- hollywood_movies %>%
  rename(title = title_2)

# creating search query components for scraping in python
# Create YouTube search query using title, release year, and distributor
hollywood_movies <- hollywood_movies %>%
  mutate(
    release_year = year(release_date),
    youtube_search_query = paste(title, release_year, "official trailer", us_distributor)
  )

# Export the required fields to CSV for scraper in Jupyter Notebook
write.csv(
  hollywood_movies %>%
    select(title, release_date, us_distributor, youtube_search_query),
  "youtube_scraping_master.csv",
  row.names = FALSE
)

# let's start with scraping for the first 10 movies only to see how the scraper works 
head(hollywood_movies, 10) %>%
  write.csv("first_10_movies_for_scraping.csv", row.names = FALSE)

# checking all unique US distributors in the dataset
unique_distributors <- hollywood_movies %>%
  mutate(us_distributor = str_to_lower(str_trim(us_distributor))) %>%
  distinct(us_distributor) %>%
  arrange(us_distributor)

View(unique_distributors)

# let's inspect the scraped reviews for the first 10 movies in the dataset
comments_10_movies <- read.csv("first_10_movies_comments.csv")
str(comments_10_movies)

# rename movie column to title in scraped dataset for uniformity
comments_10_movies <- comments_10_movies %>%
  rename(title = movie)

# add release date to the scraped dataset
comments_10_movies <- comments_10_movies %>%
  left_join(hollywood_movies %>% select(title, release_date), by = "title")

# checking if there are any comments after the release date
comments_10_movies %>%
  filter(as.Date(date) > as.Date(release_date))
summary(as.Date(comments_10_movies$date) > as.Date(comments_10_movies$release_date))


# save the initial hollywood titles file
write.csv(hollywood_movies, "hollywood_pre_controls.csv", row.names = FALSE)


# read in the hollywood movies dataset
hollywood_movies <- read.csv("hollywood_pre_controls.csv")


############## checking the primary genres of the movies in the hollywood dataset 
# Create a frequency table of primary genres
genre_summary <- as.data.frame(table(hollywood_movies$primary_genre))

# Rename columns for clarity
colnames(genre_summary) <- c("Primary_Genre", "Number_of_Movies")

# Order by frequency (descending)
genre_summary <- genre_summary %>%
  arrange(desc(Number_of_Movies))

# View it nicely in RStudio
View(genre_summary)

# Display as a thesis-friendly table
genre_summary %>%
  kable(caption = "Distribution of Movies by Primary Genre") %>%
  kable_styling(full_width = FALSE, position = "center")

########################## creating variables for controls ###########################
####################### 1. Star power ################################################
# Load the top 1000 grossing actors scraped from The Numbers 
top_actors <- read.csv("top_1000_grossing_actors.csv")

# Step 1: Expand the cast column into one row per actor per movie
cast_expanded <- hollywood_movies %>%
  select(title = title, cast) %>%
  mutate(movie_id = row_number()) %>%
  separate_rows(cast, sep = ",\\s*") %>%
  mutate(cast = str_trim(cast))

# Step 2: Clean top actors list and restrict to top 100
top_actor_names <- top_actors %>%
  dplyr::slice(1:100) %>%          # <-- NEW: Select only top 100 actors
  mutate(Name = str_trim(Name)) %>%
  select(Name)

# Step 3: Fuzzy join (Jaro-Winkler similarity, tolerant to small differences)
fuzzy_matches <- stringdist_inner_join(
  cast_expanded, 
  top_actor_names,
  by = c("cast" = "Name"),
  method = "jw",           # Jaro-Winkler method
  max_dist = 0.10          # You can tune this (0.10–0.15 is typical)
)

# Step 4: Count how many top actors appear in each movie's cast
star_counts <- fuzzy_matches %>%
  distinct(movie_id, cast) %>%
  count(movie_id, name = "star_power_count")

# Step 5: Merge the count back to the main dataset
hollywood_movies <- hollywood_movies %>%
  mutate(movie_id = row_number()) %>%
  left_join(star_counts, by = "movie_id") %>%
  mutate(star_power_count = replace_na(star_power_count, 0)) %>%
  select(-movie_id)


####################### 2. Director Power ##############################
# Load the top 1000 grossing actors scraped from The Numbers 
top_directors <- read.csv("top_1000_grossing_directors.csv")

# Prepare director name columns
hollywood_movies <- hollywood_movies %>%
  mutate(director_clean = tolower(trimws(director)))

top_directors <- top_directors %>%
  mutate(name_clean = tolower(trimws(Name)))

# Redo fuzzy join with enforced uniqueness
director_joined <- stringdist_left_join(
  hollywood_movies,
  top_directors,
  by = c("director_clean" = "name_clean"),
  method = "jw",
  max_dist = 0.10,
  distance_col = "jw_distance"
) %>%
  group_by(title, release_date) %>%
  slice_min(jw_distance, with_ties = FALSE) %>%  # Keep only 1 best match
  ungroup() %>%
  mutate(director_power = ifelse(!is.na(Rank) & Rank <= 100, 1, 0)) %>%
  select(title, release_date, director_power)

# Join back to hollywood_movies safely
hollywood_movies <- hollywood_movies %>%
  left_join(director_joined, by = c("title", "release_date")) %>%
  mutate(director_power = replace_na(director_power, 0))

# sanity check: looking for duplicates 
hollywood_movies %>%
  count(title, release_date) %>%
  filter(n > 1) # no duplicates found




####################### 3. Distributor Power ###########################
# Rename the dist column to distributor_name 
hollywood_movies <- hollywood_movies %>%
  rename(distributor_name = dist)

# loading the datasets of distributors scraped from The Numbers 
distributors_2019 <- read.csv("distributors_2019.csv")
distributors_2020 <- read.csv("distributors_2020.csv")
distributors_2021 <- read.csv("distributors_2021.csv")
distributors_2022 <- read.csv("distributors_2022.csv")
distributors_2023 <- read.csv("distributors_2023.csv")
distributors_2024 <- read.csv("distributors_2024.csv")

# rename gross columns in 2023 and 2024 datasets for uniformity 
distributors_2023 <- distributors_2023 %>% rename(Gross = Gross_2023)
distributors_2024 <- distributors_2024 %>% rename(Gross = Gross_2024)



# Step 1: Combine all distributor datasets into one
distributors_all <- bind_rows(
  distributors_2019,
  distributors_2020,
  distributors_2021,
  distributors_2022,
  distributors_2023,
  distributors_2024
)

# Step 2: Identify top 20 distributors by year based on Gross
top_distributors <- distributors_all %>%
  group_by(Year) %>%
  slice_max(order_by = Gross, n = 20) %>%
  ungroup() %>%
  mutate(distributor_clean = tolower(trimws(Distributor)))

# Clean both hollywood titles and distributor datasets first
hollywood_movies <- hollywood_movies %>%
  mutate(us_distributor_clean = tolower(trimws(us_distributor)),
         year = lubridate::year(release_date))

top_distributors <- top_distributors %>%
  mutate(distributor_clean = tolower(trimws(Distributor)))

# Extract distinct distributor names from both datasets
hollywood_dist_names <- hollywood_movies %>%
  distinct(us_distributor_clean) %>%
  arrange(us_distributor_clean)

distributor_dist_names <- top_distributors %>%
  mutate(distributor_clean = tolower(trimws(Distributor))) %>%
  distinct(distributor_clean) %>%
  arrange(distributor_clean)

# Manual fixes for known distributor mismatches in both datasets 
distributor_mapping <- tibble(
  us_distributor_clean = c("disney", "walt disney", "sony", "sony pictures", "searchlight"),
  us_distributor_final = c("walt disney", "walt disney", "sony pictures", "sony pictures", "searchlight pictures")
)


# Standardise distributor names for matching
hollywood_movies <- hollywood_movies %>%
  mutate(year = lubridate::year(release_date)) %>%
  left_join(distributor_mapping, by = "us_distributor_clean") %>%
  mutate(us_distributor_final = coalesce(us_distributor_final, us_distributor_clean))

# Fuzzy join to assign distributor_power
distributor_joined <- stringdist_left_join(
  hollywood_movies,
  top_distributors,
  by = c("us_distributor_final" = "distributor_clean", "year" = "Year"),
  method = "jw",
  max_dist = 0.10,
  distance_col = "dist"
) %>%
  group_by(title, release_date) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(distributor_power = ifelse(!is.na(Rank), 1, 0)) %>%
  select(title, release_date, distributor_power)

# Join result back to hollywood_movies
hollywood_movies <- hollywood_movies %>%
  left_join(distributor_joined, by = c("title", "release_date")) %>%
  mutate(distributor_power = replace_na(distributor_power, 0))


# sanity check again for duplicates
hollywood_movies %>%
  count(title, release_date) %>%
  filter(n > 1) # no duplicates found 

# save the hollywood movies improved dataset 
write.csv(hollywood_movies, "hollywood_improved_with_controls_except_prod_budget.csv", row.names = FALSE)



#################################### CLEANUP PROCESS TO CREATE FINAL DATASET WITH VALID TITLES #################################################
# the cleanup continues until the modelling part to retain only those titles that were:
# 1. Not rescreened
# 2. Had comments enabled on their official trailer 
# 3. Were movies and not documentaries or concert screenings
# 4. Had more than 30 pre-release comments on the official trailer


############ initial checking for rescreened old movies to remove from the dataset using movie titles scraped Box Office Mojo ####################
# Create a slim version of the dataset
title_release_data <- hollywood_movies %>%
  select(title = title, release_date) %>%
  distinct() %>%
  mutate(
    title_clean = title %>%
      # Move ", The" to the beginning
      str_replace("^(.*),\\s+The$", "The \\1") %>%
      str_replace("^(.*),\\s+An$", "An \\1") %>%
      str_replace("^(.*),\\s+A$", "A \\1") %>%
      
      tolower() %>%
      str_remove_all("\\((re|\\d{4})\\)") %>%
      str_replace_all("&", "and") %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_squish()
  )

# 2019 #
# Filter movies released in 2019
movies_2019_subset <- title_release_data %>%
  filter(year(release_date) == 2019)

# View the result
View(movies_2019_subset)

# checking which movies in our dataset are present in the box office mojo scraped data 
# Step 1: Preprocess both title columns (to lowercase and remove punctuation)
clean_titles <- function(title_vec) {
  title_vec %>%
    tolower() %>%
    gsub("[[:punct:]]", "", .) %>%
    trimws()
}


# scrape movies released in 2019 from box office mojo
# Define the URL for 2019 calendar grosses
url_2019 <- "https://www.boxofficemojo.com/year/2019/?grossesOption=calendarGrosses"

# Read and parse the HTML content
page_2019 <- read_html(url_2019)

# Extract the table
movies_2019 <- page_2019 %>%
  html_element("table") %>%
  html_table()

# Clean up column names
colnames(movies_2019) <- make.names(colnames(movies_2019))


# cleaning up titles for better and easier matching
movies_2019_subset <- movies_2019_subset %>%
  mutate(title_clean_match = clean_titles(title_clean))

movies_2019 <- movies_2019 %>%
  mutate(Release_clean = clean_titles(Release))  # 'Release' is the column in BOM

# Perform fuzzy left join
matched_movies <- stringdist_left_join(
  movies_2019_subset,
  movies_2019,
  by = c("title_clean_match" = "Release_clean"),
  method = "jw",       # Jaro-Winkler
  max_dist = 0.10,     # Allowable distance threshold
  distance_col = "dist"
) %>%
  group_by(title) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(in_bom_2019 = !is.na(Release)) %>%
  select(title, release_date, title_clean, Release, in_bom_2019)

# View results
View(matched_movies)

# Create the unmatched dataset for manual inspection to see whether movies actually released in 2019 or not 
unmatched_2019 <- matched_movies %>%
  filter(in_bom_2019 == FALSE) %>%
  select(title, release_date, title_clean)

# changing names of two movies to the correct english names
movies_2019_subset <- movies_2019_subset %>%
  mutate(
    title = case_when(
      title == "Escape Plan 3: Devil's Station" ~ "Escape Plan 3: The Extractors",
      title == "Nomis" ~ "Night Hunter",
      TRUE ~ title
    ),
    title_clean = case_when(
      title_clean == "escape plan 3 devils station" ~ "escape plan 3 the extractors",
      title_clean == "nomis" ~ "night hunter",
      TRUE ~ title_clean
    )
  )


# removing documentaries or rescreenings 
titles_to_flag <- c(
  "apocalypse now",
  "depeche mode: spirits in the forest",
  "the eyes of orson welles",
  "jesus is king",
  "the kill team",
  "league of legends world championship",
  "met opera akhnaten",
  "metallica & san francisco symphony - s&m2",
  "monrovia",
  "roger waters us + them",
  "sea of trees",
  "shakira in concert: el dorado world tour",
  "el silencio de los otros",
  "soundgarden: live from the artists den",
  "true crime"
)

# Use stringdist to compare each dataset title against your reference list
find_similar_titles <- function(dataset_titles, reference_titles, max_distance = 5) {
  matches <- data.frame()
  
  for (ref_title in reference_titles) {
    distances <- stringdist::stringdist(tolower(dataset_titles), tolower(ref_title), method = "jw")
    matched <- dataset_titles[distances < max_distance]
    
    if (length(matched) > 0) {
      temp <- data.frame(
        reference_title = ref_title,
        matched_title = matched,
        distance = distances[distances < max_distance]
      )
      matches <- rbind(matches, temp)
    }
  }
  return(matches)
}


# applying it to my dataset to manually inspect whether the matching worked 
dataset_titles <- movies_2019_subset$title
similar_titles_df <- find_similar_titles(dataset_titles, titles_to_flag, max_distance = 0.3)  # Jaro-Winkler threshold

# View results for manual inspection
View(similar_titles_df)

# removing the 15 old movies from the 2019 hollywood movies subset 
# Titles to remove (as matched/final titles in your subset)
titles_to_remove <- c(
  "Apocalypse Now Final Cut (2019)",
  "Depeche Mode: Spirits In The Forest",
  "Eyes Of Orson Welles, The",
  "Jesus Is King",
  "League Of Legends World Championship 2019",
  "MET Opera: Akhnaten (2019)",
  "Metallica & San Francisco Symphony: S&M2",
  "Monrovia, Indiana",
  "Roger Waters  Us + Them",
  "Sea Of Trees",
  "Shakira In Concert: El Dorado World Tour",
  "Silencio de los Otros, El",
  "Soundgarden: Live From The Artists Den – The IMAX Experience",
  "True Crimes",
  "Kill Team, The (2013)"
)

# Filter them out
movies_2019_subset_cleaned <- movies_2019_subset %>%
  filter(!(title %in% titles_to_remove))

# saving this cleaned 2019 dataset
write.csv(movies_2019_subset_cleaned, "movies_2019_subset_cleaned.csv")


# 2020 #
# Filter movies released in 2020
movies_2020_subset <- title_release_data %>%
  filter(year(release_date) == 2020)

# View the result
View(movies_2020_subset)

# Define the URL for 2020 calendar grosses to scrape from box office mojo
url_2020 <- "https://www.boxofficemojo.com/year/2020/?grossesOption=calendarGrosses"

# Read and parse the HTML content
page_2020 <- read_html(url_2020)

# Extract the table
movies_2020 <- page_2020 %>%
  html_element("table") %>%
  html_table()

# Clean up column names
colnames(movies_2020) <- make.names(colnames(movies_2020))

# cleaning up movie titles
movies_2020_subset <- movies_2020_subset %>%
  mutate(title_clean_match = clean_titles(title_clean))

movies_2020 <- movies_2020 %>%
  mutate(Release_clean = clean_titles(Release))  # 'Release' is the column in BOM

# Perform fuzzy left join
matched_movies <- stringdist_left_join(
  movies_2020_subset,
  movies_2020,
  by = c("title_clean_match" = "Release_clean"),
  method = "jw",       # Jaro-Winkler
  max_dist = 0.10,     # Allowable distance threshold
  distance_col = "dist"
) %>%
  group_by(title) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(in_bom_2020 = !is.na(Release)) %>%
  select(title, release_date, title_clean, Release, in_bom_2020)

# View results
View(matched_movies)

# Create the unmatched dataset for manual inspection
unmatched_2020 <- matched_movies %>%
  filter(in_bom_2020 == FALSE) %>%
  select(title, release_date, title_clean)


# removing old movies or documentaries from the 2020 movie subset 
# Create a vector of exact titles to remove
titles_to_remove_2020 <- c(
  "Angels Fallen",
  "Animal Crackers",
  "Beyond the mountains and hills",
  "David Byrne's American Utopia",
  "Elvis: That's The Way It Is: Special Edition (re)",
  "Hail Satan?",
  "Inception (2010) (re)"
)

# Remove them from movies_2020_subset
movies_2020_subset_clean <- movies_2020_subset %>%
  filter(!(title %in% titles_to_remove_2020))

# save the 2020 cleaned movie names
write.csv(movies_2020_subset_clean, "movies_2020_subset_clean.csv")


# 2021 #
# Filter movies released in 2021
movies_2021_subset <- title_release_data %>%
  filter(year(release_date) == 2021)

# View the result
View(movies_2021_subset)

# Define the URL for 2021 calendar grosses
url_2021 <- "https://www.boxofficemojo.com/year/2021/?grossesOption=calendarGrosses"

# Read and parse the HTML content
page_2021 <- read_html(url_2021)

# Extract the table
movies_2021 <- page_2021 %>%
  html_element("table") %>%
  html_table()

# Clean up column names
colnames(movies_2021) <- make.names(colnames(movies_2021))

# cleaning up movie titles
movies_2021_subset <- movies_2021_subset %>%
  mutate(title_clean_match = clean_titles(title_clean))

movies_2021 <- movies_2021 %>%
  mutate(Release_clean = clean_titles(Release))  # 'Release' is the column in BOM

# Perform fuzzy left join
matched_movies <- stringdist_left_join(
  movies_2021_subset,
  movies_2021,
  by = c("title_clean_match" = "Release_clean"),
  method = "jw",       # Jaro-Winkler
  max_dist = 0.10,     # Allowable distance threshold
  distance_col = "dist"
) %>%
  group_by(title) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(in_bom_2021 = !is.na(Release)) %>%
  select(title, release_date, title_clean, Release, in_bom_2021)

# View results
View(matched_movies)

# Create the unmatched dataset for manual inspection
unmatched_2021 <- matched_movies %>%
  filter(in_bom_2021 == FALSE) %>%
  select(title, release_date, title_clean)

# changing names of two movies to the full name 
movies_2021_subset <- movies_2021_subset %>%
  mutate(title = case_when(
    title == "Paw Patrol" ~ "Paw Patrol: The Movie",
    title == "Rock Dog 2" ~ "Rock Dog 2: Walk Around the Park",
    TRUE ~ title  # leave all others unchanged
  ))


# chainging the release date of the movie "Synchronic" to the correct release date 
# Ensure release_date is a proper Date
movies_2021_subset <- movies_2021_subset %>%
  mutate(release_date = as.Date(release_date)) %>%
  mutate(release_date = if_else(title == "Synchronic", as.Date("2021-06-03"), release_date))

# List of exact titles to remove because they are either rescreenings or they are not "movies"
titles_to_remove <- c(
  "Be Natural: The Untold Story of Alice Guy-Blache",
  "Bon Jovi From Encore Nights",
  "Carnival Of Souls",
  "Doors: Live at the Bowl '68 Special Edition, The",
  "Double Play: James Benning and Richard Linklater",
  "Fat City",
  "Female Trouble",
  "Ferdinand (2017) (re)",
  "Friend, The (2019)",
  "Hustler, The (1961)",
  "Kid, The (Dir. Chaplin)",
  "Little Shop Of Horrors, The (1960)",
  "Lusty Men, The",
  "Night Of The Living Dead (1968)",
  "Sita Sings The Blues",
  "Snowpiercer (2013) (re)",
  "TCM: West Side Story 60th Anniversary"
)

# Remove the titles defined above 
movies_2021_subset <- movies_2021_subset %>%
  filter(!title %in% titles_to_remove)

# save the 2021 cleaned movie names
write.csv(movies_2021_subset, "movies_2021_subset_clean.csv")


# 2022 #
# Filter movies released in 2022
movies_2022_subset <- title_release_data %>%
  filter(year(release_date) == 2022)

# View the result
View(movies_2022_subset)

# Define the URL for 2022 calendar grosses
url_2022 <- "https://www.boxofficemojo.com/year/2022/?grossesOption=calendarGrosses"

# Read and parse the HTML content
page_2022 <- read_html(url_2022)

# Extract the table
movies_2022 <- page_2022 %>%
  html_element("table") %>%
  html_table()

# Clean up column names
colnames(movies_2022) <- make.names(colnames(movies_2022))

# cleaning up movie titles
movies_2022_subset <- movies_2022_subset %>%
  mutate(title_clean_match = clean_titles(title_clean))

movies_2022 <- movies_2022 %>%
  mutate(Release_clean = clean_titles(Release))  # 'Release' is the column in BOM

# Perform fuzzy left join
matched_movies <- stringdist_left_join(
  movies_2022_subset,
  movies_2022,
  by = c("title_clean_match" = "Release_clean"),
  method = "jw",       # Jaro-Winkler
  max_dist = 0.10,     # Allowable distance threshold
  distance_col = "dist"
) %>%
  group_by(title) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(in_bom_2022 = !is.na(Release)) %>%
  select(title, release_date, title_clean, Release, in_bom_2022)

# View results
View(matched_movies)

# Create the unmatched dataset for manual inspection
unmatched_2022 <- matched_movies %>%
  filter(in_bom_2022 == FALSE) %>%
  select(title, release_date, title_clean)

# correcting the names of two movies 
movies_2022_subset <- movies_2022_subset %>%
  mutate(title = case_when(
    title == "Cinderella and the Spellbinder" ~ "Cinderella and the Little Sorcerer",
    title == "Exorcismo de Dios, El" ~ "The Exorcism of God",
    TRUE ~ title
  ))

# removing old titles and those that are not movies i.e. documentaries and concerts 
# Define the titles to remove
titles_to_remove_2022 <- c(
  "12 Angry Men", "All That Jazz", "Anatomy Of A Murder", "Angel (1937)", "Annie Hall",
  "Avatar (2009) (re)", "Band of Angels (re: 2009)", "Barton Fink", "Bell Book and Candle",
  "Bend of the River (re)", "Birds, The (Re 2012)", "Blackboard Jungle (re: 1996)", "Caja, La",
  "Cheyenne Autumn", "Conversation, The (1974) (re)", "Duran Duran - A Hollywood High",
  "E.T. The Extra-Terrestrial (1982) (re: 2022)", "Easy Rider (re)",
  "Effect of Gamma Rays on Man-in-the-Moon Marigolds, The (re 2017)",
  "Faces", "Family Jewels, The", "Fortune Cookie, The", "Gloria (re: 2010)", "Gypsy Moon",
  "Husbands (Re: 2012)", "Jaws (1975)", "Killing Of A Chinese Bookie, The", "Last Picture Show, The",
  "Lighthouse, The (2019) (re)", "Minnie And Moskowitz", "Moby Dick (Re 2011)", "Moonfleet",
  "My Fair Lady (1964)", "New York, New York (1977)", "North By Northwest (1959)",
  "One From The Heart (1982)", "Opening Night (1977) (re)", "Ox-Bow Incident, The (re 05)",
  "Paper Moon", "Punch-Drunk Love (2002) (re)", "Quiet Man, The", "Rancho Notorious (re:2011)",
  "Rio Bravo (1959)", "Searchers, The (re: 2015)", "Sex, Lies and Videotape", "Shadows (1959)",
  "Singin' In The Rain (1952)", "South Pacific (1958)", "Stranger, The (1946)",
  "TCM: An American In Paris", "Tall Men, The (re)", "Tarnished Angels, The", "Tokyo-ga",
  "Torn", "Twenty One Pilots Cinema Experience", "Twin Peaks: The Missing Pieces",
  "Two Rode Together", "Unconquered (re)", "Vanishing Point (1971)",
  "What Ever Happened To Baby Jane? (re: 2004)", "What's Up, Doc?", "Wild at Heart",
  "Wind Across the Everglades", "Witch, The (2016)", "Woman under the Influence, A",
  "Written on the Wind"
)

# Remove them from the dataset
movies_2022_subset <- movies_2022_subset %>%
  filter(!title %in% titles_to_remove_2022)


# save the 2022 cleaned movie names
write.csv(movies_2022_subset, "movies_2022_subset_clean.csv")

# 2023 #
# Filter movies released in 2023
movies_2023_subset <- title_release_data %>%
  filter(year(release_date) == 2023)

# View the result
View(movies_2023_subset)

# Define the URL for 2023 calendar grosses
url_2023 <- "https://www.boxofficemojo.com/year/2023/?grossesOption=calendarGrosses"

# Read and parse the HTML content
page_2023 <- read_html(url_2023)

# Extract the table
movies_2023 <- page_2023 %>%
  html_element("table") %>%
  html_table()

# Clean up column names
colnames(movies_2023) <- make.names(colnames(movies_2023))

# cleaning up movie titles
movies_2023_subset <- movies_2023_subset %>%
  mutate(title_clean_match = clean_titles(title_clean))

movies_2023 <- movies_2023 %>%
  mutate(Release_clean = clean_titles(Release))  # 'Release' is the column in BOM

# Perform fuzzy left join
matched_movies <- stringdist_left_join(
  movies_2023_subset,
  movies_2023,
  by = c("title_clean_match" = "Release_clean"),
  method = "jw",       # Jaro-Winkler
  max_dist = 0.10,     # Allowable distance threshold
  distance_col = "dist"
) %>%
  group_by(title) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(in_bom_2023 = !is.na(Release)) %>%
  select(title, release_date, title_clean, Release, in_bom_2023)

# View results
View(matched_movies)

# Create the unmatched dataset for manual inspection
unmatched_2023 <- matched_movies %>%
  filter(in_bom_2023 == FALSE) %>%
  select(title, release_date, title_clean)

# changing the names of two movies to their english titles 
movies_2023_subset <- movies_2023_subset %>%
  mutate(
    title = case_when(
      title == "Abyzou" ~ "The Offering",
      title == "Misanthrope" ~ "To Catch a Killer",
      TRUE ~ title
    )
  )

# removing old titles or those that are not movies (documentaries, concerts etc)
titles_to_remove_2023 <- c(
  "2001: A Space Odyssey (Re: 2014)", "All the Beauty and the Bloodshed", "Beetlejuice (1988)", 
  "Before You Know It (2014)", "Big Lebowski, The (re)", "Bigger Than Life", "Bitter Victory (1957)", 
  "Blade Runner (1982)", "Bridges of Madison County, The", "Caged (1950)", "Carlos", 
  "Casablanca (1942) (re)", "Citizen Kane (1941)", "Cleopatra (1963)", "Cop Land (1997)", 
  "Cruising (re)", "Cry Baby", "Design for Living (1933)", "Doctor Dolittle", 
  "Dolly Parton ROCKSTAR: The Global First Listen Event", "Esther Newton Made Me Gay", 
  "Exodus (1961)", "Exorcist, The (1973)", "Fantasia (re) '90", "French Connection, The (1971)", 
  "Ghostbusters (re: 2014)", "Godfather Part III, The (1990) (re)", "Godfather, The (re)", 
  "Godfather: Part II (Re: 2014), The", "Gone With The Wind (1939)", 
  "Grand Budapest Hotel, The (2014) (re)", "Grapes of Wrath", "Harold and Maude (re: 2012)", 
  "Hell and High Water (1954) (re)", "How Green Was My Valley", "Hunger Games (2023 Event), The", 
  "I Am Sam", "Interview With the Vampire (1994)", "King Of Comedy (1983)", "Lilith (1964)", 
  "Lost In Translation (2003)", "Lust For Life (1957)", 
  "Machine Gun Kelly: Mainstream Sellout Live From Cleveland", "Magnolia", 
  "Man From Laramie, The", "Man Who Shot Liberty Valance, The", 
  "Metallica M72 World Tour Live from TX #1", "Metallica M72 World Tour Live from TX #2", 
  "Metallica: 72 Seasons - Global Premiere", "Miracle Worker, The", "My Darling Clementine", 
  "Night of the Iguana (1964)", "Once Upon A Time In America (1984)", "Out Of The Past (1947)", 
  "Psycho (1960) (re)", "Pulp Fiction (1994)", "Raging Bull (1980)", "Rear Window (1954)", 
  "Robinson Crusoe (1954)", "Rock Bottom Riser", "Roman Holiday (1953)", "Scarface (1983)", 
  "Seven Women", "Silk Road Rally, The", "Sociedad de la nieve, La", "Stop Making Sense Remastered", 
  "Streetcar Named Desire, A (1951) (re)", "TCM Presents E.T. The Extra-Terrestrial 35th Anniversary", 
  "Ted K", "They Drive by Night", "Thing, The (1982)", "To Live and Die in L.A.", "Tobacco Road", 
  "Torn Curtain (1966)", "Travels with My Aunt (1972) (re)", "Two Weeks In Another Town", 
  "TÁR (documentary)", "Walk the Line (2005) (re) (Spain)", "War and Peace (1956)", 
  "Wendy and Lucy (2008) (re)", "Whole Town's Talking, The", "Young Mr. Lincoln", 
  "Ziggy Stardust: 50th Anniversary Event"
)

# Remove the rows with those titles
movies_2023_subset <- movies_2023_subset %>%
  filter(!title %in% titles_to_remove_2023)

# save the 2023 cleaned movie names
write.csv(movies_2023_subset, "movies_2023_subset_clean.csv")


# 2024 #
# Filter movies released in 2024
movies_2024_subset <- title_release_data %>%
  filter(year(release_date) == 2024)

# View the result
View(movies_2024_subset)

# Define the URL for 2024 calendar grosses
url_2024 <- "https://www.boxofficemojo.com/year/2024/?grossesOption=calendarGrosses"

# Read and parse the HTML content
page_2024 <- read_html(url_2024)

# Extract the table
movies_2024 <- page_2024 %>%
  html_element("table") %>%
  html_table()

# Clean up column names
colnames(movies_2024) <- make.names(colnames(movies_2024))

# cleaning up movie titles
movies_2024_subset <- movies_2024_subset %>%
  mutate(title_clean_match = clean_titles(title_clean))

movies_2024 <- movies_2024 %>%
  mutate(Release_clean = clean_titles(Release))  # 'Release' is the column in BOM

# Perform fuzzy left join
matched_movies <- stringdist_left_join(
  movies_2024_subset,
  movies_2024,
  by = c("title_clean_match" = "Release_clean"),
  method = "jw",       # Jaro-Winkler
  max_dist = 0.10,     # Allowable distance threshold
  distance_col = "dist"
) %>%
  group_by(title) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(in_bom_2024 = !is.na(Release)) %>%
  select(title, release_date, title_clean, Release, in_bom_2024)

# View results
View(matched_movies)

# Create the unmatched dataset for manual inspection
unmatched_2024 <- matched_movies %>%
  filter(in_bom_2024 == FALSE) %>%
  select(title, release_date, title_clean)

# Update movie titles for two movies
movies_2024_subset <- movies_2024_subset %>%
  mutate(title = case_when(
    title == "Gun Monkeys" ~ "Fast Charlie",
    title == "Queen Mary" ~ "Haunting of the Queen Mary",
    TRUE ~ title
  ))

# removing old movies or those that are documentaries 
# Vector of titles to remove
titles_to_remove_2024 <- c(
  "Adventures Of Buckaroo Banzai Across The 8th Dimension",
  "Always (1989)",
  "Anna Karenina (1935)",
  "Another Woman (1988)",
  "Bad And The Beautiful, The (re)",
  "Barefoot Contessa, The",
  "Big Sleep, The (1946)",
  "Bigamist, The (1953)",
  "Birth (2004)",
  "Chase, The (1966)",
  "Children Of Men (2006) (re)",
  "Couple, Un",
  "Dead Poets Society",
  "Dead Reckoning (1947)",
  "Duck Soup",
  "East of Eden",
  "Favourite, The (2018) (re)",
  "Fixed Bayonets!",
  "Forty Guns",
  "Four Horsemen Of The Apocalypse",
  "Fugitive Kind, The",
  "Funny Face (1957)",
  "Giant (re)",
  "Home from the hill",
  "Inland Empire (2006) (re)",
  "Julius Caesar (1953)",
  "Lady From Shanghai, The",
  "Land of the Pharoahs",
  "Laufey's A Night At The Symphony: Hollywood Bowl",
  "Little Women (2019) (re)",
  "Love In The Afternoon",
  "M. Butterfly",
  "Man Who Knew Too Much, The (1956)",
  "Man of the West (1958)",
  "Marnie (1964)",
  "Missouri Breaks, The (1976)",
  "Mutiny On The Bounty (1962)",
  "Nam June Paik: Moon is the Oldest TV",
  "Nuclear Now",
  "On The Waterfront",
  "One from the Heart: Reprise",
  "Paths Of Glory",
  "Pearl Jam - Dark Matter - Global Theatrical Experience - One Night Only",
  "Placebo: This Search for Meaning",
  "Platoon",
  "Private Hell 36",
  "Quiet American, The (1958) (re)",
  "Rebel Without A Cause (1955)",
  "Reflections In A Golden Eye",
  "Repo Man",
  "RoboCop (1987)",
  "Rope (re)",
  "Rosemary's Baby (re: 2015)",
  "Ruggles of Red Gap (re)",
  "Second Civil War, The",
  "Shanghai Express (1932)",
  "Some Came Running (1958)",
  "Something Wild",
  "Splendor In The Grass",
  "Star Is Born, A (1954)",
  "Star Wars: Episode I - Phantom Menace (1999) (re)",
  "Strangers On A Train (1951) (re)",
  "Super/Man: The Christopher Reeve Story",
  "Tetro (2009) (re)",
  "Texas Chainsaw Massacre 50th Anniversary, The",
  "There Will Be Blood (2007) (re)",
  "They Live",
  "This Property Is Condemned",
  "To Catch A Thief (1955)",
  "To Have And Have Not (1944)",
  "To Kill a Mockingbird (re 2010)",
  "Today We Live",
  "Touch Of Evil (1958) (re)",
  "USHER: Rendezvous in Paris",
  "Vidro Fumê",
  "Virgin Suicides (4K Restoration), The",
  "Where The Sidewalk Ends",
  "Wild River",
  "Zabriskie Point"
)

# Remove the specified titles from the 2024 subset
movies_2024_subset <- movies_2024_subset %>%
  filter(!title %in% titles_to_remove_2024)


# save the 2024 cleaned movie names
write.csv(movies_2024_subset, "movies_2024_subset_clean.csv")


############## combining all the cleaned movie names ####################
movies_2019_subset_cleaned <- read.csv("movies_2019_subset_cleaned.csv")
movies_2020_subset_cleaned <- read.csv("movies_2020_subset_clean.csv")
movies_2021_subset_cleaned <- read.csv("movies_2021_subset_clean.csv")
movies_2022_subset_cleaned <- read.csv("movies_2022_subset_clean.csv")
movies_2023_subset_cleaned <- read.csv("movies_2023_subset_clean.csv")
movies_2024_subset_cleaned <- read.csv("movies_2024_subset_clean.csv")


# Combine all cleaned datasets into one
all_movies_cleaned <- bind_rows(
  movies_2019_subset_cleaned,
  movies_2020_subset_cleaned,
  movies_2021_subset_cleaned,
  movies_2022_subset_cleaned,
  movies_2023_subset_cleaned,
  movies_2024_subset_cleaned
) # we have a total of 711 movies now 

# Optional: view the result
glimpse(all_movies_cleaned)  # or use head(all_movies_cleaned)

# Save to CSV if needed
write.csv(all_movies_cleaned, "all_movies_cleaned_combined.csv", row.names = FALSE)


#################################### INITIAL SCRAPING OF COMMENTS IN BATCHES ##############################################
# now that the results for the first 10 movies is satisfactory, let's move towards scraping for the entire dataset
####### batch 1 ###############
batch_1 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(1:50)  # First 50

# Export
write.csv(batch_1, "batch_1_scrape.csv", row.names = FALSE)

# Load the scraped comments for batch 1
comments_batch_1 <- read.csv("comments_batch_1.csv", stringsAsFactors = FALSE)

# Number of comments per movie
comments_batch_1 %>%
  count(title, sort = TRUE)

# Range of comment dates
comments_batch_1 %>%
  group_by(title) %>%
  summarise(
    min_date = min(as.Date(date)),
    max_date = max(as.Date(date)),
    .groups = "drop"
  )


# Titles from original batch
original_titles <- batch_1$title

# Titles with scraped comments
scraped_titles <- unique(comments_batch_1$title)

# Movies with no comments
setdiff(original_titles, scraped_titles)


########### batch 2 ################
batch_2 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(51:100)

# Export to CSV for Python
write.csv(batch_2, "batch_2_scrape.csv", row.names = FALSE)

# loading in the second scraped batch 
comments_batch_2 <- read.csv("comments_batch_2.csv")
scraped_titles_2 <- unique(comments_batch_2$title)

# Titles in batch_2 but not in the scraped comments
missing_titles_2 <- setdiff(batch_2$title, scraped_titles_2)

# Count how many were scraped and how many are missing
n_scraped <- length(scraped_titles_2)
n_total <- nrow(batch_2)
n_missing <- length(missing_titles_2)

cat("✅ Scraped for", n_scraped, "movies\n")
cat("❌ Missing for", n_missing, "movies\n")
cat("📦 Total movies in batch:", n_total, "\n")

# View the missing ones
print(missing_titles_2)


############## batch 3 ##########################
# Create Batch 3: Movies 101 to 150
batch_3 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(101:150)

# Export for Python scraping
write.csv(batch_3, "batch_3_scrape.csv")

# checking the movie IDs for this batch:
movie_ids_3 <- read.csv("batch_3_with_ids.csv")
# for some of the movies like Angry Birds 2, the comments are turned off so could not scrape. look into this later 

# checking the scraped dataset
comments_batch_3 <- read.csv("comments_batch_3.csv")
scraped_titles_3 <- unique(comments_batch_3$title)

missing_titles_3 <- setdiff(batch_3$title, scraped_titles_3)


################### batch 4 ############################
# Create Batch 4: Movies 151 to 200
batch_4 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(151:200)

# Export for Python scraping
write.csv(batch_4, "batch_4_scrape.csv")

# load in scraped comments
comments_batch_4 <- read.csv("comments_batch_4.csv")


################ batch 5 ##############################
# Create Batch 5: Movies 201 to 300
batch_5 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(201:300)

# Export for Python scraping
write.csv(batch_5, "batch_5_scrape.csv")

# load in scraped comments
comments_batch_5 <- read.csv("comments_batch_5.csv")


############### batch 6 #############################
# Create Batch 6: Movies 301 to 400
batch_6 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(301:400)

# Export for Python scraping
write.csv(batch_6, "batch_6_scrape.csv")

# load in scraped comments
comments_batch_6 <- read.csv("comments_batch_6.csv")


##################### batch 7 #######################
# Create Batch 7: Movies 401 to 450
batch_7 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(401:450)

# Export for Python scraping
write.csv(batch_7, "batch_7_scrape.csv")

# load in scraped comments
comments_batch_7 <- read.csv("comments_batch_7.csv")


##################### batch 8 #######################
# Create Batch 8: Movies 451 to 500
batch_8 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(451:500)

# Export for Python scraping
write.csv(batch_8, "batch_8_scrape.csv")

# load in scraped comments
comments_batch_8 <- read.csv("comments_batch_8.csv")


##################### batch 9 #######################
# Create Batch 9: Movies 501 to 550
batch_9 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(501:550)

# Export for Python scraping
write.csv(batch_9, "batch_9_scrape.csv")

# load in scraped comments
comments_batch_9 <- read.csv("comments_batch_9.csv")

##################### batch 10 #######################
# Create Batch 10: Movies 551 to 600
batch_10 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(551:600)

# Export for Python scraping
write.csv(batch_10, "batch_10_scrape.csv")

# load in scraped comments
comments_batch_10 <- read.csv("comments_batch_10.csv")


##################### batch 11 #######################
# Create Batch 11: Movies 601 to 650
batch_11 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(601:650)

# Export for Python scraping
write.csv(batch_11, "batch_11_scrape.csv")

# load in scraped comments
comments_batch_11 <- read.csv("comments_batch_11.csv")

##################### batch 12 #######################
# Create Batch 12: Movies 651 to 700
batch_12 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(651:700)

# Export for Python scraping
write.csv(batch_12, "batch_12_scrape.csv")

# load in scraped comments
comments_batch_12 <- read.csv("comments_batch_12.csv")


##################### batch 13 #######################
# Create Batch 13: Movies 701 to 750
batch_13 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(701:750)

# Export for Python scraping
write.csv(batch_13, "batch_13_scrape.csv")

# load in scraped comments
comments_batch_13 <- read.csv("comments_batch_13.csv")

##################### batch 14 #######################
# Create Batch 14: Movies 751 to 800
batch_14 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(751:800)

# Export for Python scraping
write.csv(batch_14, "batch_14_scrape.csv")

# load in scraped comments
comments_batch_14 <- read.csv("comments_batch_14.csv")


##################### batch 15 #######################
# Create Batch 15: Movies 801 to 850
batch_15 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(801:850)

# Export for Python scraping
write.csv(batch_15, "batch_15_scrape.csv")

# load in scraped comments
comments_batch_15 <- read.csv("comments_batch_15.csv")

##################### batch 16 #######################
# Create Batch 16: Movies 851 to 900
batch_16 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(851:900)

# Export for Python scraping
write.csv(batch_16, "batch_16_scrape.csv")

# load in scraped comments
comments_batch_16 <- read.csv("comments_batch_16.csv")

##################### batch 17 #######################
# Create Batch 17: Movies 901 to 950
batch_17 <- hollywood_movies %>%
  select(title, release_date, us_distributor) %>%
  dplyr::slice(901:950)

# Export for Python scraping
write.csv(batch_17, "batch_17_scrape.csv")

# load in scraped comments
comments_batch_17 <- read.csv("comments_batch_17.csv")


######## inspecting the comments data and cleaning it #############################
# combining the first 10 scraped batches
# Combine all batches into one dataframe
comments_10_batches <- bind_rows(
  comments_batch_1,
  comments_batch_2,
  comments_batch_3,
  comments_batch_4,
  comments_batch_5,
  comments_batch_6,
  comments_batch_7,
  comments_batch_8,
  comments_batch_9,
  comments_batch_10
)

# converts the dates to the right format
comments_10_batches <- comments_10_batches %>%
  mutate(
    date = as.Date(date),
    release_date = as.Date(release_date)
  )

pre_release_comments <- comments_10_batches %>%
  filter(date < release_date)

# How many total and how many pre-release
cat("Total comments:", nrow(comments_10_batches), "\n")
cat("Pre-release comments:", nrow(pre_release_comments), "\n")
cat("Post-release comments filtered out:", nrow(comments_10_batches) - nrow(pre_release_comments), "\n")

# cleaning up the data so that by removing urls so that sample can be scored by VADER (VADER does not need a lot cleaning)
# identifying spam in the comments 
strict_spam_regex <- regex(
  "https?://|www\\.|youtu\\.be|check (my|this)|promo code|earn money|buy now|free trial|dm for collab",
  ignore_case = TRUE
)

comments_flagged <- comments_10_batches %>%
  mutate(is_spam = str_detect(comment, strict_spam_regex))

spam_comments <- filter(comments_flagged, is_spam == TRUE)

# subsetting dataset without spam
comments_no_spam <- comments_10_batches %>%
  anti_join(spam_comments, by = "comment")


# further cleaning to remove empty spaces and mentions etc.
comments_cleaned <- comments_no_spam %>%
  mutate(comment = comment %>%
           
           # Remove URLs (http, https, youtu.be, etc.)
           str_replace_all("(?i)https?://\\S+|www\\.\\S+|youtu\\.be/\\S+", "") %>%
           
           # Remove @mentions (words starting with @ followed by typical username patterns)
           str_replace_all("@\\w+", "") %>%
           
           # Remove hashtags (but not the hash sign itself, only the full tag)
           str_replace_all("#\\w+", "") %>%
           
           # Normalize line breaks, tabs, and excessive spaces
           str_replace_all("[\\r\\n\\t]+", " ") %>%
           str_replace_all(" {2,}", " ") %>%
           
           # Final trim
           str_trim()
  )


# initial checking of movies that might be rescreened based on the year given with the title in brackets
comments_cleaned <- comments_cleaned %>%
  mutate(
    title_year = str_extract(title, "\\((\\d{4})\\)"),
    title_year = as.numeric(str_remove_all(title_year, "[()]")),
    release_year = year(release_date),
    year_diff = release_year - title_year
  )

# Check how many movies have large backward gaps (re-release?)
summary(comments_cleaned$year_diff)

rescreened_movies <- comments_cleaned %>%
  filter(!is.na(year_diff), year_diff > 10) %>%
  distinct(title, release_year, title_year, year_diff) %>%
  arrange(desc(year_diff))

View(rescreened_movies)

# removing rescreened movies because they donot reflect actual pre-release buzz
rescreened_titles <- c(
  "Out Of The Past (1947)", "Singin' In The Rain (1952)", "Rear Window (1954)",
  "Little Shop Of Horrors, The (1960)", "Hustler, The (1961)", "My Fair Lady (1964)",
  "Night Of The Living Dead (1968)", "Conversation, The (1974) (re)",
  "Jaws (1975)", "Opening Night (1977) (re)", "E.T. The Extra-Terrestrial (1982) (re: 2022)",
  "Pulp Fiction (1994)", "Punch–Drunk Love (2002) (re)", "Avatar (2009) (re)"
)

comments_cleaned <- comments_cleaned %>%
  filter(!(title %in% rescreened_titles))


# also checking for and removing rescreenings based on "re" in titles 
comments_cleaned <- comments_cleaned %>%
  mutate(
    is_reboot_labelled = str_detect(title, "(?i)\\(re\\s*\\d{4}\\)")
  )


comments_cleaned %>%
  filter(is_reboot_labelled) %>%
  select(title, release_date) %>%
  distinct() %>%
  View() # this is a very simple initial step to identify rescreenings. after this a thorough search was conducted manually to flag rescreenings to remove them

rescreen_titles <- c("Moby Dick (Re 2011)", "Birds, The (Re 2012)")

comments_cleaned <- comments_cleaned %>%
  filter(!(title %in% rescreen_titles))


# also filter out observations where the comment column is empty
comments_cleaned <- comments_cleaned %>%
  filter(comment != "" & !is.na(comment))


# save dataset 
write.csv(comments_cleaned, "comments_cleaned_no_rescreens.csv", row.names = FALSE)

# read in the comments_cleaned dataset (old dataset)
old_comments_cleaned_10_batches <- read.csv("comments_cleaned_no_rescreens.csv")


################## doing the same cleaning steps as above but for batches 11 to 16 now 
# loading in all the new comments 
comments_batch_11 <- read.csv("comments_batch_11.csv")
comments_batch_12 <- read.csv("comments_batch_12.csv")
comments_batch_13 <- read.csv("comments_batch_13.csv")
comments_batch_14 <- read.csv("comments_batch_14.csv")
comments_batch_15 <- read.csv("comments_batch_15.csv")
comments_batch_16 <- read.csv("comments_batch_16.csv")


# Step 1: Combine batches 11 to 16
comments_new_batches <- bind_rows(
  comments_batch_11,
  comments_batch_12,
  comments_batch_13,
  comments_batch_14,
  comments_batch_15,
  comments_batch_16
)

# Step 2: Convert date formats
comments_new_batches <- comments_new_batches %>%
  mutate(
    date = as.Date(date),
    release_date = as.Date(release_date)
  )

# Step 3: Filter pre-release comments only
pre_release_comments <- comments_new_batches %>%
  filter(date < release_date)

cat("Total comments:", nrow(comments_new_batches), "\n")
cat("Pre-release comments:", nrow(pre_release_comments), "\n")
cat("Post-release comments filtered out:", nrow(comments_new_batches) - nrow(pre_release_comments), "\n")

# Step 4: Flag spam comments
strict_spam_regex <- regex(
  "https?://|www\\.|youtu\\.be|check (my|this)|promo code|earn money|buy now|free trial|dm for collab",
  ignore_case = TRUE
)

comments_flagged <- comments_new_batches %>%
  mutate(is_spam = str_detect(comment, strict_spam_regex))

spam_comments <- filter(comments_flagged, is_spam == TRUE)

# Step 5: Remove spam
comments_no_spam <- comments_new_batches %>%
  anti_join(spam_comments, by = "comment")


# Step 6: Clean text fields
comments_cleaned <- comments_no_spam %>%
  mutate(comment = comment %>%
           
           str_replace_all("(?i)https?://\\S+|www\\.\\S+|youtu\\.be/\\S+", "") %>%
           str_replace_all("@\\w+", "") %>%
           str_replace_all("#\\w+", "") %>%
           str_replace_all("[\\r\\n\\t]+", " ") %>%
           str_replace_all(" {2,}", " ") %>%
           str_trim()
  )

# Step 7: Detect re-screenings based on year in brackets with the title
comments_cleaned <- comments_cleaned %>%
  mutate(
    title_year = str_extract(title, "\\((\\d{4})\\)"),
    title_year = as.numeric(str_remove_all(title_year, "[()]")),
    release_year = year(release_date),
    year_diff = release_year - title_year
  )

rescreened_movies <- comments_cleaned %>%
  filter(!is.na(year_diff), year_diff > 10) %>%
  distinct(title, release_year, title_year, year_diff)

# Remove rescreened titles from the main cleaned dataset
comments_cleaned_filtered <- comments_cleaned %>%
  anti_join(rescreened_movies, by = c("title", "release_year"))

# Confirm reduction
cat("✅ Original rows:", nrow(comments_cleaned), "\n")
cat("✅ After removing re-releases:", nrow(comments_cleaned_filtered), "\n")
cat("✅ Removed:", nrow(comments_cleaned) - nrow(comments_cleaned_filtered), "comments\n")

# checking if there are still any older movies in the dataset 
# Detect any remaining movies with a 4-digit year in parentheses
suspicious_year_titles <- comments_cleaned_filtered %>%
  filter(str_detect(title, "\\(\\d{4}\\)")) %>%
  distinct(title, release_date) %>%
  arrange(title)

# View or inspect the result
View(suspicious_year_titles)

# Optional: print summary
cat("🎬 Movies with (YYYY) still in title after cleaning:", nrow(suspicious_year_titles), "\n")

# removing the only one that is a rescreening 
comments_cleaned_filtered <- comments_cleaned_filtered %>%
  filter(title != "Before You Know It (2014)")

# checking all the titles in the dataset
unique_titles <- comments_cleaned_filtered %>%
  distinct(title) %>%
  arrange(title)

print(unique_titles)

# more re-releases that we missed 
re_titles_missed <- c(
  "2001: A Space Odyssey (Re: 2014)",
  "Big Lebowski, The (re)",
  "Cruising (re)",
  "Godfather: Part II (Re: 2014), The",
  "Harold and Maude (re: 2012)",
  "To Kill a Mockingbird (re 2010)",
  "Ghostbusters (re: 2014)"
)

# Remove these from your cleaned dataset
comments_cleaned_filtered <- comments_cleaned_filtered %>%
  filter(!(title %in% re_titles_missed))

# Confirm
cat("✅ Removed", length(re_titles_missed), "additional re-releases\n")

# Step 10: Filter out empty comments (again, just in case)
comments_cleaned_final <- comments_cleaned_filtered %>%
  filter(comment != "" & !is.na(comment))

# Step 11: save the new data
write.csv(comments_cleaned_final, "comments_cleaned_new_final.csv", row.names = FALSE)

# Optional check
cat("✅ Final cleaned dataset saved with", nrow(comments_cleaned_final), "comments across",
    n_distinct(comments_cleaned_final$title), "unique movies.\n")


################ inspecting movie IDs fethched through the scraper in Python to check for official trailers ###################
# loading datasets with video IDs for inspection
# Generate file names
file_names <- paste0("batch_", 1:17, "_with_ids.csv")

# Load and combine all batches
all_batches <- lapply(file_names, read.csv)
video_ids_combined <- bind_rows(all_batches)

# Optional: Check the combined dataset
glimpse(video_ids_combined)


# changing titles of movies that have been changed above during filtering of old movies during matching 
# read in the cleaned movies dataset 
all_movies_cleaned <- read.csv("all_movies_cleaned_combined.csv")


# now change names of movies in the video_ids_combined dataset to filter out the old ones by matching them with the
# cleaned movies dataset 
video_ids_combined <- video_ids_combined %>%
  mutate(title = case_when(
    title == "Gun Monkeys" ~ "Fast Charlie",
    title == "Queen Mary" ~ "Haunting of the Queen Mary",
    title == "Abyzou" ~ "The Offering",
    title == "Misanthrope" ~ "To Catch a Killer",
    title == "Cinderella and the Spellbinder" ~ "Cinderella and the Little Sorcerer",
    title == "Exorcismo de Dios, El" ~ "The Exorcism of God",
    title == "Paw Patrol" ~ "Paw Patrol: The Movie",
    title == "Rock Dog 2" ~ "Rock Dog 2: Walk Around the Park",
    title == "Escape Plan 3: Devil's Station" ~ "Escape Plan 3: The Extractors",
    title == "Nomis" ~ "Night Hunter",
    TRUE ~ title
  ))


# removing old titles, documentaries and concert videos from the video ID dataset 
video_ids_combined <- video_ids_combined %>%
  semi_join(all_movies_cleaned, by = "title")


# divide the video IDs into years so that we can check in chunks 
video_ids_combined <- video_ids_combined %>%
  mutate(year = year(ymd(release_date)))

video_ids_2019 <- video_ids_combined %>% filter(year == 2019)
video_ids_2020 <- video_ids_combined %>% filter(year == 2020)
video_ids_2021 <- video_ids_combined %>% filter(year == 2021)
video_ids_2022 <- video_ids_combined %>% filter(year == 2022)
video_ids_2023 <- video_ids_combined %>% filter(year == 2023)
video_ids_2024 <- video_ids_combined %>% filter(year == 2024)

# now each of the titles was manually inspected by conducting google searches for further cleaning outlined in the next steps 


############################ FURTHER CLEANUP AND MANUAL INSPECTION OF ALL TITLES ###################################
# 2019 #
# removing some more old movies or documentaries that I missed
titles_to_remove_2019 <- c(
  "Dead Trigger",
  "Aladdin",
  "Soundgarden: Live From The Artists Den - The IMAX Experience",
  "Photograph",
  "Angry Birds Movie 2, The",
  "Amazing Grace",
  "Driven",
  "Abominable (2019)",
  "Apollo",
  "Wanda (1970)"
)

video_ids_2019 <- video_ids_2019 %>%
  filter(!title %in% titles_to_remove_2019)

# creating a dataset of manually inspected corrected video IDs to coincide with official trailers only
corrected_ids_2019 <- tibble::tibble(
  title = c(
    "Vox Lux",
    "Destroyer",
    "Happy Death Day 2U",
    "Lego Movie 2: The Second Part, The",
    "Backtrace",
    "Captain Marvel",
    "Drunk Parents",
    "Destination Wedding",
    "After (2019)",
    "Missing Link",
    "Avengers: Endgame",
    "We Die Young",
    "Godzilla: King Of The Monsters (2019)",
    "Haunting Of Sharon Tate, The",
    "Don't Let Go",
    "Escape Plan 3: The Extractors",
    "Her Smell",
    "Kitchen, The (2019)",
    "Stockholm (2019)",
    "Night Hunter",
    "47 Meters Down: Uncaged",
    "Killerman",
    "Playmobil: The Movie",
    "Skin",
    "Current War, The",
    "Maleficent: Mistress Of Evil",
    "Mutant Blast",
    "Doctor Sleep",
    "Midway",
    "21 Bridges",
    "Star Wars: The Rise Of Skywalker",
    "Spies In Disguise",
    "Jexi"
  ),
  video_id = c(
    "zxdVqr4hmZU",
    "bqHaLUoiWZU",
    "IeXqWDFJZiw",
    "XvHSlHhh1gk",
    "INDleu1aHyE",
    "Z1BCujX3pw8",
    "47RxfLj6KBg",
    "TjXQzRWmb_I",
    "2ZAdcWHuCmY",
    "L8uPXwUR7NE",
    "TcMBFSGVi1c",
    "EflGhkNvWPM",
    "wVDtmouV9kM",
    "I-L0dk6zuyE",
    "RB-_oNDH0d8",
    "VoipQuCpQ9Y",
    "PMlHDNdLGU8",
    "fgit74aVAvM&t=1s",
    "FHzgsiNO6AY",
    "ev6VW0VA8jA",
    "BhOJXUmvLQ8",
    "hIb2_I-dPY8",
    "Tb-E-WUJl44",
    "8uc30b4kZws",
    "2FTxKFsWz60",
    "n0OFH4xpPr4",
    "pH_913fyWrM",
    "2msJTFvhkU4",
    "Z_7eN5iloyk",
    "qaZoSTG10lw",
    "8Qn_spdM5Zg",
    "C5YeOc0N6Ao",
    "EtpBbRsNr-M"
  )
)

# saving this dataset to scrape youtube trailer comments for these new video_ids for the official trailers
write.csv(corrected_ids_2019, "corrected_ids_2019.csv")

# cleaned and final titles 2019
final_titles_2019 <- video_ids_2019

# save final_titles_2019
write.csv(final_titles_2019, "final_titles_2019.csv")


# 2020 #
# removing missed old movies or those that have lesser than 30 comments 
# Define titles to remove
titles_to_remove_2020 <- c(
  "Thriller",
  "Family Romance, LLC",
  "Vault",
  "Scoob!"
)

# Filter them out from the dataset
video_ids_2020 <- video_ids_2020 %>%
  filter(!title %in% titles_to_remove_2020)

# creating a separate dataset of corrected video IDs and titles for 2020 to scrape comments from official trailer only
# Manually create the corrected dataset for 2020
corrected_ids_2020 <- tibble::tibble(
  title = c(
    "Wedding Year, The",
    "Bigger",
    "Gentlemen, The",
    "Brahms: The Boy II",
    "Invisible Man, The (2020)",
    "Wolf Hour, The",
    "Give Me Liberty",
    "Burden (2020)",
    "Ophelia",
    "Tenet",
    "After We Collided",
    "Unhinged",
    "Greenland",
    "Trolls World Tour",
    "Follow Me"
  ),
  video_id = c(
    "TVhVdVj5KVY",
    "UBdLrdX-Ov4",
    "2B0RpUGss2c",
    "A6caADGf8mw",
    "WO_FJdiY9dA",
    "RxZ8FTMlN6w",
    "YR8nVCExVo4",
    "6HDT6u2j2XM",
    "gIA2Fn2q7zY",
    "LdOM0x0XDMo",
    "2SvwX3ux_-8",
    "3xIO18Du5aY",
    "vz-gdEL_ae8",
    "4_DZX7fJ6Yo",
    "HjrE3lFmxEU"
  )
)

# saving this dataset to scrape youtube trailer comments for these new video_ids:
write.csv(corrected_ids_2020, "corrected_ids_2020.csv")

# Remove "Trolls World Tour" from video_ids_2020
video_ids_2020 <- video_ids_2020 %>%
  filter(title != "Trolls World Tour") 


# save finalized titles for 2020
final_titles_2020 <- video_ids_2020
write.csv(final_titles_2020, "final_titles_2020.csv")



# combining corrected titles for 2019 and 2020 for rescraping
# Combine 2019 and 2020 corrected datasets
corrected_ids_19_20 <- bind_rows(corrected_ids_2019, corrected_ids_2020)

# save this to import to python for scraping of comments from official trailer 
write.csv(corrected_ids_19_20, "corrected_ids_19_20.csv")

# read in the corrected_ids_19_20 dataset
corrected_ids_19_20 <- read.csv("corrected_ids_19_20.csv")

# reading in the newly scraped comments from official trailers
# Load the cleaned comments datasets
comments_corrected_19_20 <- read.csv("comments_corrected_19_20.csv", stringsAsFactors = FALSE)
comments_kitchen_2019 <- read.csv("comments_kitchen_2019.csv", stringsAsFactors = FALSE) # these were scraped again because the video ID was wrong for this particular movie 

# Check basic structure and dimensions
str(comments_corrected_19_20)
str(comments_kitchen_2019)

# combine comments for The Kitchen with the rest of the movies
# Combine the datasets
comments_corrected_19_20 <- bind_rows(comments_corrected_19_20, comments_kitchen_2019)

# save full comments_corrected_19_20 dataset
write.csv(comments_corrected_19_20, "comments_corrected_19_20.csv")

# checking which titles could not be scraped for this new batch
# Get unique titles from corrected_ids_19_20
unique_ids_titles_19_20 <- unique(corrected_ids_19_20$title)

# Get unique titles from comments_corrected_19_20
unique_comments_titles_19_20 <- unique(comments_corrected_19_20$title)

# Find titles that are in corrected_ids_19_20 but not in comments_corrected_19_20
missing_titles_19_20 <- setdiff(unique_ids_titles_19_20, unique_comments_titles_19_20)

# Print the results
print("Titles present in corrected_ids_19_20 but not in comments_corrected_19_20:")
print(missing_titles_19_20)


# Get the count of missing titles
print(paste("Number of missing titles:", length(missing_titles_19_20)))

# remove title: Trolls World Tour from movie names cleaned because comments are disabled on trailer
# Remove from all_movies_cleaned
all_movies_cleaned <- all_movies_cleaned %>%
  filter(title != "Trolls World Tour")

# Remove from video_ids_combined
video_ids_combined <- video_ids_combined %>%
  filter(title != "Trolls World Tour")

# save the new updated comments_corrected_19_20 dataset
write.csv(comments_corrected_19_20, "comments_corrected_19_20.csv")


# 2021 #
# removing some titles that are old movies or have comments disabled on the trailer
titles_to_remove_2021 <- c(
  "Extinct",
  "Sweet Thing",
  "Boss Baby: Family Business, The",
  "Seder-Masochism"
)

# Filter out those titles from video_ids_2021
video_ids_2021 <- video_ids_2021 %>%
  filter(!title %in% titles_to_remove_2021)


# creating a new dataset for titles with new video IDs from official trailers only
# Create a data frame with titles and video_ids
corrected_ids_2021 <- data.frame(
  title = c(
    "Fatman",
    "Music (Event)",
    "Spiral (dir. Bousman)",
    "Friendsgiving",
    "Nobody",
    "Synchronic",
    "Marksman, The",
    "F9 The Fast Saga",
    "Black Widow",
    "Gunpowder Milkshake",
    "Suicide Squad, The",
    "To the Stars (dir. Stephens)",
    "Echo Boomers",
    "Barb And Star Go To Vista del Mar",
    "After We Fell (Event)",
    "What Is Life Worth",
    "Copshop",
    "Eternals"
  ),
  video_id = c(
    "Z64XvPERZ50",
    "T3hxL41mVbU",
    "gzy6ORqE9IY",
    "cOnFN2VSVdY",
    "wZti8QKBWPo",
    "fl_kzTQvPVw",
    "clW-4WNIUTo",
    "aSiDu3Ywi8E",
    "ybji16u608U",
    "yxuAroBqt2c",
    "eg5ciqQzmK0",
    "fpp_MnGqTR8",
    "_O_t073kq9s",
    "3EBwBGTlGFQ",
    "NYdNN6C9hfI",
    "OOAemeB9CAw",
    "wgdLO-U2mDQ",
    "x_me3xsvDgk"
  )
)

# save this dataset
write.csv(corrected_ids_2021, "corrected_ids_2021.csv")

# creating and saving the final_titles 2021 dataset
final_titles_2021 <- video_ids_2021
write.csv(final_titles_2021, "final_titles_2021.csv")


# 2022 #
# Titles to remove from video_ids_2022
titles_to_remove_2022 <- c(
  "Arctic Dogs",
  "Beatles: Get Back - The Rooftop Concert, The",
  "Bad Guys, The",
  "Reel Rock 16",
  "Marmaduke",
  "Lightyear",
  "Moonage Daydream",
  "Effect of gamma rays on Man-in-the-Moon Marigolds, The (re 2017)",
  "Cinderella and the Little Sorcerer",
  "Cinderella And The Secret Prince"
)

# Remove the titles
video_ids_2022 <- video_ids_2022 %>%
  filter(!title %in% titles_to_remove_2022)

# Create the corrected_ids_2022 dataset to scrape comments from official trailers only
corrected_ids_2022 <- tibble::tibble(
  title = c(
    "Out of Death",
    "13 Minutes",
    "Swallow",
    "Demonic",
    "Doctor Strange in the Multiverse of Madness",
    "Last Looks",
    "Minions: The Rise Of Gru",
    "Last Seen Alive",
    "Thor: Love And Thunder",
    "Book of Love",
    "Blacklight",
    "Don't Worry Darling",
    "The Exorcism of God",
    "Black Panther: Wakanda Forever",
    "Puss In Boots: The Last Wish",
    "Rock Dog 3 Battle the Beat"
  ),
  video_id = c(
    "ulT5QB_uJL0",
    "11eHcVi9-v8",
    "auVZKcxV7XQ",
    "dUUtdDnxRuY",
    "aWzlQ2N6qqg",
    "7-WUfQwodE8",
    "6DxjJzmYsXo",
    "si-OuJ2bH-E",
    "Go8nTmfrQd8",
    "M_HTbI00qOU",
    "d4K9Rzy2_DA",
    "FgmnKsED-jU",
    "T65_PSo4dLU",
    "_Z3QKkl1WyM",
    "xgZLXyqbYOc",
    "tFmyP-nz7M"
  )
)

# save the dataset 
write.csv(corrected_ids_2022, "corrected_ids_2022.csv")

# join the 2021 and 2022 corrected video IDs
corrected_ids_21_22 <- bind_rows(corrected_ids_2021, corrected_ids_2022)

# save this dataset
write.csv(corrected_ids_21_22, "corrected_ids_21_22.csv")

# read in the corrected_ids_21_22 dataset 
corrected_ids_21_22 <- read.csv("corrected_ids_21_22.csv")

# loading in and inspecting the scraped comments
comments_corrected_21_22 <- read.csv("comments_corrected_21_22.csv")

# checking the titles that haven't been scraped to remove them from the data
# Get unique titles from corrected_ids_21_22
unique_ids_titles_21_22 <- unique(corrected_ids_21_22$title)

# Get unique titles from comments_corrected_21_22
unique_comments_titles_21_22 <- unique(comments_corrected_21_22$title)

# Find titles in corrected_ids_21_22 but not in comments_corrected_21_22
missing_titles_21_22 <- setdiff(unique_ids_titles_21_22, unique_comments_titles_21_22)

# Print results
print("Titles in corrected_ids_21_22 but missing in comments_corrected_21_22:")
print(missing_titles_21_22)

# titles 13 Minutes (2022), Minions: The Rise of Gru (2022), Puss In Boots: The Last Wish (2022) and Rock Dog 3 Battle The Beat (2022) missing here 

# Count of missing titles
print(paste("Number of missing titles:", length(missing_titles_21_22)))

# creating final_titles_2022 dataset
# List of titles to remove
titles_to_remove <- c(
  "13 Minutes",
  "Minions: The Rise of Gru", 
  "Puss In Boots: The Last Wish",
  "Rock Dog 3 Battle The Beat"
)

# Remove titles and create final_titles_2022
final_titles_2022 <- video_ids_2022 %>%
  filter(!title %in% titles_to_remove)

# save the 2022 final titles
write.csv(final_titles_2022, "final_titles_2022.csv")



# 2023 #
# List of titles to remove with reasons (for reference)
titles_to_remove <- c(
  "Titanic 25 Year Anniversary",    # old movie remove title
  "Super Mario Bros. Movie, The",   # comments off
  "Beautiful Disaster (Vertical)",  # trailer has only 20 comments
  "See You on Venus",               # less comments
  "TAYLOR SWIFT THE ERAS TOUR",     # not a movie
  "RENAISSANCE: A FILM BY BEYONCE"  # not a movie
)

# Remove these titles from video_ids_2023
video_ids_2023 <- video_ids_2023 %>%
  filter(!title %in% titles_to_remove)

# create a corrected_ids_2023 dataset to scrape comments from official trailers only
corrected_ids_2023 <- tibble::tibble(
  title = c(
    "Wrong Place",
    "Inspection, The",
    "Ant-Man and the Wasp: Quantumania",
    "To Leslie",
    "Whale, The",
    "Savage Salvation",
    "Till",
    "White Elephant",
    "On the Line (2022)",
    "Guardians of the Galaxy Vol. 3",
    "Fast X",
    "Spider-Man: Across The Spider-Verse",
    "The Offering",
    "Never Rarely Sometimes Always",
    "Indiana Jones and the Dial of Destiny",
    "After Everything (Event)",
    "Blood",
    "To Catch a Killer",
    "Perpetrator",
    "She Came to Me",
    "Marvels, The",
    "Master Gardener",
    "Wish",
    "Hypnotic",
    "Migration",
    "All Fun and Games",
    "It's a Wonderful Knife",
    "Anyone But You"
  ),
  video_id = c(
    "PPJtM3JNN7U",
    "wSeprzQM6gk",
    "ZlNFpri-Y40",
    "D_k63vvm3mU",
    "D30r0CwtIKc",
    "OCp5FkDVMrs",
    "rkQi6GBwmSA",
    "YLuV1bbMEWg",
    "3Oib36GRRu4",
    "u3V5KDHRQvk",
    "32RAq6JzY-w",
    "cqGjhVJWtEg",
    "oMc-iybcRdc",
    "hjw_QTKr2rc",
    "eQfMbSe7F2g",
    "KFO3kw7AZME",
    "MPg6vLdo_AQ",
    "WEUX9HwlF5c",
    "EvkVyZPKhEs",
    "EdfEv2ZwrFs",
    "wS_qbDztgVY",
    "wzuJ9tv1eM0",
    "oyRxxpD3yNw",
    "CDFoQYFBDkE",
    "cQfo0HJhCnE",
    "UPwf19i4RWU",
    "yLdAYrG0xo0",
    "UtjH6Sk7Gxs"
  )
)

# save this dataset
write.csv(corrected_ids_2023, "corrected_ids_2023.csv")

# creating final_titles_2023 dataset
final_titles_2023 <- video_ids_2023 %>%
  filter(!str_detect(title, "Migration"))

# save final titles for 2023
write.csv(final_titles_2023, "final_titles_2023.csv")


# 2024 #
# List of titles to remove with reasons
titles_to_remove <- c(
  "Piper, The (dir. Thoroddsen)",                  # no official trailer
  "Girl in the Backseat, The",                    # no official trailer
  "Adventures Of Buckaroo Banzai Across The 8th Dimension, The",  # old movie
  "Coraline 15th Anniversary",                    # old movie
  "Gracie & Pedro: Pets to the Rescue!",          # trailer has no comments
  "Robots",                                      # no official trailer
  "Moana 2",                                     # comments off on trailer
  "Baby The Rain Must Fall"                       # old movie
)

# Remove these titles from video_ids_2024 to scrape comments from official trailers only
video_ids_2024 <- video_ids_2024 %>%
  filter(!title %in% titles_to_remove)

# Verify the removal
print("Removed titles:")
print(titles_to_remove)

# creating corrected_ids_2024 dataset to scrape comments from official trailers only
corrected_ids_2024 <- tibble::tibble(
  title = c(
    "Poor Things",
    "Beautiful Wedding",
    "On Fire",
    "Bricklayer, The",
    "Sleeping Dogs",
    "Cabrini",
    "Origin",
    "Exorcism, The",
    "Freud's Last Session",
    "Memory",
    "Deadpool & Wolverine",
    "In the Land of Saints and Sinners",
    "Play Dead",
    "Fast Charlie",
    "Reagan",
    "Anora",
    "Terrifier 3",
    "Elevation",
    "Poolman",
    "Kraven the Hunter",
    "Sasquatch Sunset",
    "Babygirl"
  ),
  video_id = c(
    "RlbR5N6veqw",
    "3qDY7cZdqlE",
    "TbmM9Rugu4g",
    "zC96qykLTdQ",
    "jlm1zyy8whg",
    "ZaMlUazXvyY",
    "pAweg5PaMuw",
    "I1lNNd_klK4",
    "-lM65Dm6Ytc",
    "yGw8yw6Mso8",
    "73_1biulkYk",
    "v1_oKW_NDEk",
    "RLJ6Nlr3TQM",
    "kEjexRdeuo0",
    "J_vdTwQP1a8",
    "p1HxTmV5i7c",
    "tk2mkXHN2G8",
    "1coLN43Q3w4",
    "bHFI2GSMoM0",
    "I8gFw4-2RBM",
    "KgfkthLpeXw",
    "9XXoNB0lVGo"
  )
)

# Verify the dataset
print("Number of movies in corrected_ids_2024:")
print(nrow(corrected_ids_2024))  # Should return 22

# save the corrected_ids_2024 dataset
write.csv(corrected_ids_2024, "corrected_ids_2024.csv")


# Stack datasets corrected_ids_2023 and corrected_ids_2024 
corrected_ids_23_24 <- bind_rows(corrected_ids_2023, corrected_ids_2024)

# Verify
print(paste("Total movies:", nrow(corrected_ids_23_24)))
head(corrected_ids_23_24)  # Check first few rows

# save this dataset to scrape the remaining comments
write.csv(corrected_ids_23_24, "corrected_ids_23_24.csv")


# let's inspect what movie comments could not be scraped
comments_corrected_23_24 <- read.csv("comments_corrected_23_24.csv")

# checking the titles that haven't been scraped to remove them from the data
# Get unique titles from corrected_ids_23_24
unique_ids_titles_23_24 <- unique(corrected_ids_23_24$title)

# Get unique titles from comments_corrected_23_24
unique_comments_titles_23_24 <- unique(comments_corrected_23_24$title)

# Find titles in corrected_ids_21_22 but not in comments_corrected_21_22
missing_titles_23_24 <- setdiff(unique_ids_titles_23_24, unique_comments_titles_23_24)

# Print results
print("Titles in corrected_ids_23_24 but missing in comments_corrected_23_24:")
print(missing_titles_23_24)

# title Migration (2023) missing here because comments disabled

# create final_titles_2024 dataset
final_titles_2024 <- video_ids_2024

# save the final titles for 2024
write.csv(final_titles_2024, "final_titles_2024.csv")



# now let's join all the final_titles and see how many movies we have left
# Step 1: Bind all datasets vertically
all_movies_post_filtering <- bind_rows(
  final_titles_2019 %>% mutate(year = 2019),
  final_titles_2020 %>% mutate(year = 2020),
  final_titles_2021 %>% mutate(year = 2021),
  final_titles_2022 %>% mutate(year = 2022), 
  final_titles_2023 %>% mutate(year = 2023),
  final_titles_2024 %>% mutate(year = 2024)
)

# Step 2: Count movies per year and total
count_summary_filtered_movies <- all_movies_post_filtering %>%
  group_by(year) %>%
  summarise(n_movies = n())

total_movies_post_filtering <- sum(count_summary_filtered_movies$n_movies)

# Step 3: Print results
print("Number of movies per year:")
print(count_summary_filtered_movies)
print(paste("Total movies across all years:", total_movies_post_filtering))

# now we are left with a total of 654 titles 

# save the dataset of the names of all the movies post filtering
write.csv(all_movies_post_filtering, "all_titles_post_filtering.csv")


########### read in the dataset of all titles post filtering and start matching them with the meta data #############
# reading in the post-filtered titles 
all_movies_post_filtering <- read.csv("all_titles_post_filtering.csv")


# clean up the hollywood movies dataset by changing the titles that have been changed above
hollywood_movies_updated <- hollywood_movies %>%
  mutate(
    title = case_when(
      # 2019 changes
      title == "Escape Plan 3: Devil's Station" ~ "Escape Plan 3: The Extractors",
      title == "Nomis" ~ "Night Hunter",
      
      # 2021 changes
      title == "Paw Patrol" ~ "Paw Patrol: The Movie",
      title == "Rock Dog 2" ~ "Rock Dog 2: Walk Around the Park",
      
      # 2022 changes
      title == "Cinderella and the Spellbinder" ~ "Cinderella and the Little Sorcerer",
      title == "Exorcismo de Dios, El" ~ "The Exorcism of God",
      
      # 2023 changes
      title == "Abyzou" ~ "The Offering",
      title == "Misanthrope" ~ "To Catch a Killer",
      
      # 2024 changes
      title == "Gun Monkeys" ~ "Fast Charlie",
      title == "Queen Mary" ~ "Haunting of the Queen Mary",
      
      TRUE ~ title  # Default: keep original title if no match
    )
  )

# Save the updated hollywood movies meta dataset
write.csv(hollywood_movies_updated, "hollywood_movies_updated.csv", row.names = FALSE)

# read in the hollywood_movies_updated dataset
hollywood_movies_updated <- read.csv("hollywood_movies_updated.csv")


################################### INSPECTION AND CLEANING OF SCRAPED YOUTUBE TRAILER COMMENTS #####################################

# read in the newly scraped comments and concatinate the comments datasets 
comments_corrected_23_24 <- read.csv("comments_corrected_23_24.csv")
comments_corrected_21_22 <- read.csv("comments_corrected_21_22.csv")
comments_corrected_19_20 <- read.csv("comments_corrected_19_20.csv")

# join the comments corrected datasets
all_comments_corrected <- bind_rows(
  comments_corrected_19_20 %>% mutate(year_range = "2019_2020"),
  comments_corrected_21_22 %>% mutate(year_range = "2021_2022"), 
  comments_corrected_23_24 %>% mutate(year_range = "2023_2024")
)

# Verify the combined dataset
glimpse(all_comments_corrected)  # Check structure
print(paste("Total comments:", nrow(all_comments_corrected)))

# Check counts per period
all_comments_corrected %>% 
  count(year_range) %>% 
  print()

# Save the combined dataset
write.csv(all_comments_corrected, "all_corrected_youtube_comments.csv", row.names = FALSE)

# checking all the unique titles in the all_comments_corrected dataset
unique_titles_count_corrected <- all_comments_corrected %>%
  distinct(title) %>%  # Get unique titles
  nrow()  # Count them

print(paste("Number of unique movie titles:", unique_titles_count_corrected)) # 126 titles here

# removing X and X.1 columns from all_comments_corrected dataset 
all_comments_corrected <- all_comments_corrected %>%
  select(-X, -X.1)

# Verify removal
colnames(all_comments_corrected)  # Should not show X or X.1

# checking and removing post release comments 
# Step 1: Join release dates to comments
all_comments_corrected <- all_comments_corrected %>%
  left_join(
    hollywood_movies_updated %>% 
      select(title, release_date),  # Assuming column is 'release_date'
    by = "title"
  ) %>%
  # Convert dates to Date type (adjust format if needed)
  mutate(
    comment_date = as.Date(date),  # Replace 'date' with your comment timestamp column
    release_date = as.Date(release_date)
  )

# Step 2: Filter comments posted BEFORE release
valid_comments_corrected <- all_comments_corrected %>%
  filter(comment_date < release_date | is.na(release_date))  # Keep if date is missing

# Step 3: Verify results
print(paste("Original comments:", nrow(all_comments_corrected)))
print(paste("Valid pre-release comments:", nrow(valid_comments_corrected)))

# Check for any invalid comments (posted ON or AFTER release)
invalid_comments <- valid_comments_corrected %>% 
  mutate(
    is_pre_release = comment_date < release_date  # Should be TRUE for all rows
  ) %>% 
  filter(!is_pre_release | is.na(is_pre_release))

if (nrow(invalid_comments) > 0) {
  warning(paste("Found", nrow(invalid_comments), "comments posted ON/AFTER release!"))
  print(invalid_comments %>% select(title, comment_date, release_date))
} else {
  print("All comments are confirmed pre-release!")
}


# save the valid new comments corrected dataset (pre-cleaned)
write.csv(valid_comments_corrected, "valid_comments_corrected.csv")

# cleaning the newly scraped comments to remove bot comments and spam (ads etc.)
# Step 1: flag spam using regex
strict_spam_regex <- regex(
  "https?://|www\\.|youtu\\.be|check (my|this)|promo code|earn money|buy now|free trial|dm for collab",
  ignore_case = TRUE
)

valid_comments_flagged <- valid_comments_corrected %>%
  mutate(is_spam = str_detect(comment, strict_spam_regex))

# inspecting spam flagged comments
spam_comments <- valid_comments_flagged %>%
  filter(is_spam == TRUE)

# looks good! removing comments flagged as spam
valid_comments_nospam_corrected <- valid_comments_flagged %>%
  filter(!is_spam)

# further cleaning to remove links, user mentions and hashtags so that comments are cleaner
valid_comments_cleaned_final_corrected <- valid_comments_nospam_corrected %>%
  mutate(comment = comment %>%
           str_replace_all("(?i)https?://\\S+|www\\.\\S+|youtu\\.be/\\S+", "") %>%
           str_replace_all("@\\w+", "") %>%
           str_replace_all("#\\w+", "") %>%
           str_replace_all("[\\r\\n\\t]+", " ") %>%
           str_replace_all(" {2,}", " ") %>%
           str_trim()
  )


# save the cleaned new comments 
write.csv(valid_comments_cleaned_final_corrected, "cleaned_new_comments_corrected.csv")

# read in the cleaned corrected new comments 
valid_comments_cleaned_final_corrected <- read.csv("cleaned_new_comments_corrected.csv")


# now let's check what titles are absent in the newly scraped comments so that those can be fetched from old scrapes 
missing_titles_filtered <- all_movies_post_filtering %>%
  filter(!(title %in% valid_comments_cleaned_final_corrected$title))

# View the missing titles
View(missing_titles_filtered)

# let's check whar titles have been dropped in our cleaning of the new dataset 
# How many unique movie titles before cleaning?
length(unique(all_comments_corrected$title))  # Should be 126

# How many unique movie titles after cleaning?
length(unique(valid_comments_cleaned_final_corrected$title))  # Should be 122

# Find titles that were dropped
dropped_titles <- setdiff(unique(all_comments_corrected$title),
                          unique(valid_comments_cleaned_final_corrected$title))

print(dropped_titles)


# 1. Titles missing in cleaned comments
missing_titles <- setdiff(
  unique(all_movies_post_filtering$title),
  unique(valid_comments_cleaned_final_corrected$title)
)

# 2. Remove the ones you already know were dropped during cleaning
additional_missing_titles <- setdiff(missing_titles, dropped_titles)

# 3. View or inspect them
print(additional_missing_titles)
length(additional_missing_titles)  # Count of truly missing titles


# read in the rest of the old scraped batches of trailer comments 
old_comments_cleaned_10_batches <- read.csv("comments_cleaned_no_rescreens.csv")

old_comments_cleaned_10_to_16 <- read.csv("comments_cleaned_new_final.csv")

# Combine the two old cleaned comment datasets
old_comments_cleaned_all <- bind_rows(old_comments_cleaned_10_batches, old_comments_cleaned_10_to_16)

# Optional: check how many unique titles are in the combined dataset
cat("✅ Combined dataset loaded. Unique movie titles:", n_distinct(old_comments_cleaned_all$title), "\n")

# Check which additional missing titles are NOT present in the old combined dataset
missing_from_old <- setdiff(additional_missing_titles, unique(old_comments_cleaned_all$title))

# Print results
if (length(missing_from_old) == 0) {
  cat("✅ All additional missing titles are present in the old_comments_cleaned_all dataset.\n")
} else {
  cat("⚠️ Some titles are still missing from the old dataset:\n")
  print(missing_from_old)
}


# Check which titles in missing_from_old are already present in the new scraped dataset
already_present_in_new <- intersect(missing_from_old, unique(valid_comments_cleaned_final_corrected$title))

# Check which ones are still missing
still_missing_everywhere <- setdiff(missing_from_old, already_present_in_new)

# Print results
cat("✅ Titles already present in newly scraped comments:\n")
print(already_present_in_new)

cat("\n❌ Titles still missing from both old and new scrapes:\n")
print(still_missing_everywhere)


# first of all pull the comments for the movies that are present in the old scraped data
# Step 1: Identify which titles are available in the old dataset
available_in_old <- intersect(additional_missing_titles, unique(old_comments_cleaned_all$title))

# Step 2: Filter the old_comments_cleaned_all dataset for those titles
recovered_comments_from_old <- old_comments_cleaned_all %>%
  filter(title %in% available_in_old)

# Optional Step 3: View or confirm number of recovered titles
cat("✅ Recovered comments for", n_distinct(recovered_comments_from_old$title), "titles.\n")
View(recovered_comments_from_old)


# save the recovered comments dataset 
write.csv(recovered_comments_from_old, "recovered_comments_from_old.csv")

# read in the recovered comments as well
recovered_comments_from_old <- read.csv("recovered_comments_from_old.csv")


# now it's time to scrape the final set of titles (HOPEFULLY!!!!)
# Create a dataframe with titles and video_ids
final_scrape_titles_df <- data.frame(
  title = c(
    "Serenity", "Spider-Man: Far From Home", "Outpost, The (2020)", "Endless",
    "Force Of Nature (2020)", "Love, Weddings & Other Disasters", "Raya And The Last Dragon",
    "Rock Dog 2: Walk Around the Park", "Smile (2022)", "Operation Fortune: Ruse de Guerre",
    "Killer, The (2023)", "White Bird", "Imaginary", 
    # Skipping "Kung Fu Panda 4"
    "Mothers' Instinct", "Haunting of the Queen Mary", "Crow, The", "Beetlejuice Beetlejuice",
    "Land of Bad", "Daddio", "Speak No Evil", "Reality", "Transformers One", "Never Let Go",
    "Lee", "Rob Peace", "Joker: Folie à Deux", "Wild Robot, The", "In a Violent Nature",
    "Smile 2", "Megalopolis", "Venom: The Last Dance", "Goodrich", "Red One", "Conclave",
    "Here After", "Gladiator II", "Heretic", "Bagman", "Red Right Hand", "Wicked",
    "Lord of the Rings: The War of the Rohirrim", "Sonic The Hedgehog 3", "Here",
    "Ezra", "Mufasa: The Lion King"
  ),
  video_id = c(
    "k3zMlsEK8xA", "Nt9L1jCKGnE", "f4LM9a02q9Q", "qMHpWCN0byw", "d61GX5VoEJc", "7-Lfbu970xo",
    "1VIZ89FEjYI", "UoYB1Zn6lR4", "BcDK7lkzzsU", "WdZ-BWWQcWQ", "5S7FR_HCg9g", "aTTPea6gHh4",
    "8XoNfrgrAGM", 
    # Skipping "Kung Fu Panda 4"
    "4ltU9ooQ8x0", "uZhXePjoVFE", "djSKp_pwmOA", "CoZqL9N6Rx4", "yTFazxfrXVw", "PJrr2amlFyc",
    "dtt_S_vGWM0", "plIUJ-LF7JU", "5a09yJU-mCI", "ZDfRp_ukHDU", "DmFYkiUAAA8", "bugi-UdtjzI",
    "_OKAwz2MsJs", "67vbA5ZJdKQ", "WyXuRmXbS7U", "0HY6QFlBzUY", "pq6mvHZU0fc", "__2bjWbetsA",
    "oQ5yuRhM_yE", "U8XH3W0cMss", "JX9jasdi3ic", "ukhI37_Z9XA", "ukhI37_Z9XA", "O9i2vmFhSSY",
    "slrzCgYIUPM", "R7NNUnMIRAQ", "6COmYeLsz4c", "gCUg6Td5fgQ", "qSu6i2iFMO0", "I_id-SkGU2k",
    "HQDLCWLEHHk", "o17MF9vnabg"
  ),
  stringsAsFactors = FALSE
)

# Save the dataframe as a CSV
write.csv(final_scrape_titles_df, "final_scrape_titles_with_ids.csv", row.names = FALSE)

# read in the newly scraped comments 
comments_final_scrape_titles <- read.csv("comments_final_scrape_titles.csv")

# Count unique titles
unique_titles_count <- n_distinct(comments_final_scrape_titles$title)

# Print the result
cat("🎬 Number of unique movie titles in the final scrape:", unique_titles_count, "\n")

# adding the release date to the newly scraped comments to filter post-release comments
# Make sure release_date is Date format
hollywood_movies_updated <- hollywood_movies_updated %>%
  mutate(release_date = as.Date(release_date))

# Merge release_date into the scraped comments
comments_final_scrape_titles <- comments_final_scrape_titles %>%
  left_join(select(hollywood_movies_updated, title, release_date), by = "title")

# removing post-release comments
# Convert comment date to Date format
comments_final_scrape_titles <- comments_final_scrape_titles %>%
  mutate(date = as.Date(date))

# Filter to only pre-release comments
comments_final_scrape_pre_release <- comments_final_scrape_titles %>%
  filter(!is.na(release_date), date <= release_date)

# checking how many titles remain post filtering
titles_before <- n_distinct(comments_final_scrape_titles$title)
titles_after <- n_distinct(comments_final_scrape_pre_release$title)

cat("🎬 Unique titles before filtering:", titles_before, "\n")
cat("🎯 Unique titles after filtering:", titles_after, "\n")


# cleaning up the comments to remove promotions and spam comments 
# Step 1: Define strict spam regex
strict_spam_regex <- regex(
  "https?://|www\\.|youtu\\.be|check (my|this)|promo code|earn money|buy now|free trial|dm for collab",
  ignore_case = TRUE
)

# Step 2: Flag spam comments
comments_flagged <- comments_final_scrape_pre_release %>%
  mutate(is_spam = str_detect(comment, strict_spam_regex))

# Step 3: Extract spam comments (optional)
spam_comments <- comments_flagged %>%
  filter(is_spam == TRUE)

# Step 4: Remove spam comments
comments_no_spam <- comments_final_scrape_pre_release %>%
  anti_join(spam_comments, by = "comment")

# Step 5: Clean remaining comments
comments_final_scrape_cleaned <- comments_no_spam %>%
  mutate(comment = comment %>%
           str_replace_all("(?i)https?://\\S+|www\\.\\S+|youtu\\.be/\\S+", "") %>%
           str_replace_all("@\\w+", "") %>%
           str_replace_all("#\\w+", "") %>%
           str_replace_all("[\\r\\n\\t]+", " ") %>%
           str_replace_all(" {2,}", " ") %>%
           str_trim()
  )

# Step 6: Check how many unique titles are left
unique_titles_count <- comments_final_scrape_cleaned %>%
  distinct(title) %>%
  nrow()
cat("✅ Unique movie titles remaining:", unique_titles_count, "\n")

# Step 7: Check that all comments are pre-release
# First convert to Date if not already
comments_final_scrape_cleaned <- comments_final_scrape_cleaned %>%
  mutate(
    comment_date = as.Date(date),
    release_date = as.Date(release_date)
  )

# Now check for any post-release comments
post_release_count <- comments_final_scrape_cleaned %>%
  filter(comment_date > release_date) %>%
  nrow()

if (post_release_count == 0) {
  cat("✅ All comments are pre-release.\n")
} else {
  cat("⚠️", post_release_count, "comments are post-release and may need review.\n")
}


###### all the new comments are cleaned and pre-release now 
# Convert release_date to Date format in all three datasets
comments_final_scrape_cleaned <- comments_final_scrape_cleaned %>%
  mutate(
    date = as.Date(date),
    release_date = as.Date(release_date),
    comment_date = as.Date(comment_date)
  )

recovered_comments_from_old <- recovered_comments_from_old %>%
  mutate(
    date = as.Date(date),
    release_date = as.Date(release_date)
  )

valid_comments_cleaned_final_corrected <- valid_comments_cleaned_final_corrected %>%
  mutate(
    date = as.Date(date),
    release_date = as.Date(release_date),
    comment_date = as.Date(comment_date)
  )



# let's join all our final comment datasets
all_cleaned_comments <- bind_rows(
  comments_final_scrape_cleaned,
  recovered_comments_from_old,
  valid_comments_cleaned_final_corrected
)

# removing unnecessary columns from the dataset
all_cleaned_comments <- all_cleaned_comments %>%
  dplyr::select(-comment_date, -X, -title_year, -release_year, -year_diff, 
         -is_reboot_labelled, -year_range, -is_spam)

# renaming the date column to comment_date for clarity
all_cleaned_comments <- all_cleaned_comments %>%
  rename(comment_date = date)

# Count NA values in comment_date
sum(is.na(all_cleaned_comments$comment_date))


# checking how many comments were posted on the release_date 
release_day_comments <- all_cleaned_comments %>%
  filter(comment_date == release_date)

nrow(release_day_comments)
length(unique(release_day_comments$title))


# ensuring ONLY COMMENTS POSTED BEFORE THE RELEASE DATE because we have some comments here that were posted on the release date which may contaminte our sentiment analysis
all_cleaned_comments_only_pre <- all_cleaned_comments %>%
  filter(comment_date < release_date)

# checking how many unique titles we have left now at the end 
length(unique(all_cleaned_comments_only_pre$title))

###### for now we have a total of 634 unique titles before checking for and filtering movies with only a few comments 

# let's check comments per movie 
comment_counts <- all_cleaned_comments_only_pre %>%
  group_by(title) %>%
  summarise(comment_count = n()) %>%
  arrange(comment_count)

# View comment counts for movies with fewest comments
View(comment_counts)

summary(comment_counts$comment_count)


# Keep only titles with 30 or more comments for analysis to be robust 
titles_with_30_or_more <- comment_counts %>%
  filter(comment_count >= 30)

# Step 3: Filter the main dataset based on those titles
all_cleaned_comments_filtered <- all_cleaned_comments_only_pre %>%
  filter(title %in% titles_with_30_or_more$title)

# Optional: Check result
cat("✅ Final dataset has", nrow(all_cleaned_comments_filtered), "comments across", 
    length(unique(all_cleaned_comments_filtered$title)), "titles.\n")


############### save this finalized cleaned comments dataset ########################
write.csv(all_cleaned_comments_filtered, "finalized_comments_dataset.csv")


# read in the finalized comments dataset
finalized_comments_data <- read.csv("finalized_comments_dataset.csv")


################## found out that vader does not account for emojis in R but does in Python. need to test this out with test comments
# purely testing emoji scoring in R
# Emoji-only comments
emoji_only_comments <- tibble::tibble(
  comment = c("😍", "😭", "😡", "😂", "😐", "🔥", "💔", "👍", "💩", "🥳")
)

# Score the comments using VADER
emoji_scores <- vader_df(emoji_only_comments$comment)

# Combine original comments and their scores
emoji_scored_r <- bind_cols(emoji_only_comments, emoji_scores)

# View results
print(emoji_scored_r)

##### vader in R doesn't factor in emojis so VADER will be used in Jupyter notebook instead for weak labelling of comment subset 


###### VADER in Jupyter Notebook is handling the emojis well enough for weak labelling. let's move on and label the comments with vader and the fine tune our bert model
# Drop the 'X' column
finalized_comments_data <- finalized_comments_data[, !(names(finalized_comments_data) %in% "X")]



######################## Extracting a random sample of 80,000 comments to be scored with VADER in Python #######################
# Sample 80,000 rows
set.seed(42)  # for reproducibility
sampled_comments <- finalized_comments_data[sample(nrow(finalized_comments_data), 80000), ]

# Optional: View structure of sampled dataset
str(sampled_comments)


# save this sampled dataset to score with VADER in Jupyter Notebook
write.csv(sampled_comments, "sampled_for_vader.csv")

# inspecting the vader labelled dataset 
vader_labelled_data <- read.csv("vader_labeled_comments_full.csv")

str(vader_labelled_data)

# checking the proportion of sentiment labels 
table(vader_labelled_data$vader_sentiment)
prop.table(table(vader_labelled_data$vader_sentiment)) # proportions of positive, negative and neutral for the VADER labelled subset are rather balanced


# View some positive examples
vader_labelled_data %>%
  filter(vader_sentiment == "positive") %>%
  select(comment, pos, compound) %>%
  slice_sample(n = 5)

# View some neutral examples
vader_labelled_data %>%
  filter(vader_sentiment == "neutral") %>%
  select(comment, neu, compound) %>%
  slice_sample(n = 5)

# View some negative examples
vader_labelled_data %>%
  filter(vader_sentiment == "negative") %>%
  select(comment, neg, compound) %>%
  slice_sample(n = 5)


##################### Data Manipulation and exploration #######################################
################# 1. Pure Movie Meta Data Exploration ###########################
# identifying all the unique titles in our dataset
unique_titles <- unique(finalized_comments_data$title)
print(unique_titles)

# pulling meta data for our titles 
filtered_metadata <- hollywood_movies_updated %>%
  filter(title %in% unique_titles)

# Optional: View how many matched
cat("Number of matched titles:", nrow(filtered_metadata), "\n")

str(filtered_metadata)

# let's first check for NA values 
# Check number of NAs in each column
na_counts <- sapply(filtered_metadata, function(x) sum(is.na(x)))
na_df <- data.frame(Variable = names(na_counts), NA_Count = na_counts)
na_df <- na_df[order(-na_df$NA_Count), ]  # Sort descending by NA count

# Display variables that have at least one NA
na_df[na_df$NA_Count > 0, ]


# checking which titles our target variable (opening_weekend_eur) is NA for 
na_target_rows <- filtered_metadata[is.na(filtered_metadata$opening_weekend_eur), ]

# View the titles with missing target values
na_titles <- na_target_rows$title
print(na_titles)
# a total of 12 titles have NA values for the target variable


# removing those 12 titles that have NA values in the target variable (opening_weekend_eur) 
filtered_final_movies_metadata <- filtered_metadata[!is.na(filtered_metadata$opening_weekend_eur), ]
nrow(filtered_final_movies_metadata)


# let's also check and remove these titles and their associated comments from our finalized comments dataset
# Count how many comments are associated with each of the NA titles
comment_counts_na_titles <- finalized_comments_data %>%
  filter(title %in% na_titles) %>%
  count(title, name = "num_comments")

# View the counts
print(comment_counts_na_titles)

# Now remove all comments for those titles from the comments dataset
finalized_comments_data_no_NA <- finalized_comments_data %>%
  filter(!title %in% na_titles)

unique(finalized_comments_data_no_NA$title) # 575 titles now 

# save this no NA comments dataset now
write.csv(finalized_comments_data_no_NA, "finalized_comments_data_no_NA.csv")

# let's check how many movies do we have per genre
table(filtered_final_movies_metadata$primary_genre)

# there are still 2 documentaries in the data that must be excluded 
documentaries <- filtered_final_movies_metadata %>%
  filter(primary_genre == "Documentary") %>%
  select(title)

print(documentaries)

# Remove documentary titles from metadata
filtered_final_movies_metadata <- filtered_final_movies_metadata[!filtered_final_movies_metadata$title %in% documentaries$title, ]

# now remove these titles and associated comments from the comments dataset as well
finalized_comments_data_clean <- finalized_comments_data_no_NA %>%
  filter(!title %in% documentaries$title)

# now inspect and remove the Rock/Pop Concert title as well
# Check the title with "Rock/Pop Concert" genre
filtered_final_movies_metadata %>%
  filter(primary_genre == "Rock/Pop Concert") %>%
  select(title, primary_genre)

# remove titles both from meta data and also from comments dataset 
# Remove the title from metadata
filtered_final_movies_metadata <- filtered_final_movies_metadata %>%
  filter(title != "GHOST: RITE HERE RITE NOW")

# Remove associated comments from the comments dataset
finalized_comments_data_clean <- finalized_comments_data_clean %>%
  filter(title != "GHOST: RITE HERE RITE NOW")

# checking how many titles left now 
unique(finalized_comments_data_clean$title) # 572 final titles left 

# save both final and clean comments and meta data now (total of 572 titles)
write.csv(finalized_comments_data_clean, "super_final_comments.csv")
write.csv(filtered_final_movies_metadata, "super_final_metadata.csv")


# load them both in because these are the SUPER FINAL DATASETS
# NOTE: super_final_comments.csv is not included in the public repository (>50MB). See README for details.
super_final_comments <- read.csv("super_final_comments.csv")
# NOTE: This file is not included in the public repository. See README for details.
super_final_metadata <- read.csv("../Sensitive Data/super_final_metadata.csv")

# let's check the top 10 movies and bottom 10 movies with the most and least comments 
str(super_final_comments)

# Create frequency table
comment_counts <- as.data.frame(table(super_final_comments$title))

# Rename columns
colnames(comment_counts) <- c("Title", "Number_of_Comments")

# Sort and extract top 10
top_10_movies <- comment_counts %>%
  arrange(desc(Number_of_Comments)) %>%
  slice_head(n = 10)

# View or display nicely
top_10_movies %>%
  kable(caption = "Top 10 Movies with the Most YouTube Comments") %>%
  kable_styling(full_width = FALSE)

# Sort and extract bottom 10
bottom_10_movies <- comment_counts %>%
  arrange(Number_of_Comments) %>%
  slice_head(n = 10)

# Display nicely
bottom_10_movies %>%
  kable(caption = "Bottom 10 Movies with the Fewest YouTube Comments") %>%
  kable_styling(full_width = FALSE)


# now let's check the final distribution of movie genres in our data
table(super_final_metadata$primary_genre) # keep in mind while modelling that some genres are under represented; see what you can do with them


###### checking what the distribution of movies with top directors and distributors looks like
# Convert binary variables to factors
super_final_metadata$distributor_power <- as.factor(super_final_metadata$distributor_power)
super_final_metadata$director_power <- as.factor(super_final_metadata$director_power)

# Check the distribution
table(super_final_metadata$distributor_power)
table(super_final_metadata$director_power)

# Optional: visualize
ggplot(super_final_metadata, aes(x = distributor_power)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Distributor Power", x = "Distributor Power", y = "Count")

ggplot(super_final_metadata, aes(x = director_power)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Distribution of Director Power", x = "Director Power", y = "Count")

########## inspecting star power count
summary(super_final_metadata$star_power_count)


# checking top 10 movies with the highest number of stars in cast
super_final_metadata %>%
  arrange(desc(star_power_count)) %>%
  select(title, release_date, star_power_count, opening_weekend_eur) %>% head(10)


################## let's create proper seasonality variables for our predictive model 
# we created the holiday release flag variable using the "holidays" package in Python for specific portuguese holidays 
# loading in the dataset and checking it
# NOTE: This file is not included in the public repository. See README for details.
super_final_metadata_with_holiday <- read.csv("../Sensitive Data/metadata_with_holiday_flag.csv")


# Convert the holiday flag into a factor with meaningful labels
super_final_metadata_with_holiday$is_holiday_release <- factor(
  super_final_metadata_with_holiday$is_holiday_release,
  levels = c(0, 1),
  labels = c("Non-Holiday", "Holiday")
)

# Check the structure of holiday control
str(super_final_metadata_with_holiday$is_holiday_release)

# check counts of holiday control 
table(super_final_metadata_with_holiday$is_holiday_release)



# Convert release_date to Date format and creating variables called release_month and release_year
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  mutate(
    release_date = as.Date(release_date),
    release_month = factor(
      format(release_date, "%b"),  # "Jan", "Feb", ..., "Dec"
      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      ordered = FALSE
    ),
    release_year = lubridate::year(release_date)
  )




# code the release_year variable into a categorical variable which captures the effects of covid on the box office industry
super_final_metadata_with_holiday$era_group <- case_when(
  super_final_metadata_with_holiday$release_year == 2019 ~ "pre_pandemic",
  super_final_metadata_with_holiday$release_year %in% c(2020, 2021) ~ "pandemic",
  super_final_metadata_with_holiday$release_year %in% c(2022, 2023, 2024) ~ "post_pandemic"
)

# code the era_group variable as a factor
super_final_metadata_with_holiday$era_group <- as.factor(super_final_metadata_with_holiday$era_group)

# check levels to make sure that it has been coded properly
levels(super_final_metadata_with_holiday$era_group)

# check the distribution of movies for the three era groups 
table(super_final_metadata_with_holiday$era_group)


# Convert primary_genre to a factor
super_final_metadata_with_holiday$primary_genre <- as.factor(super_final_metadata_with_holiday$primary_genre)

# check the distribution of primary_genre
table(super_final_metadata_with_holiday$primary_genre)


# Check the levels
levels(super_final_metadata_with_holiday$primary_genre)

# some genres have very few observations. let's club some genres together
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  mutate(genre_grouped = case_when(
    primary_genre == "Action" ~ "Action",
    primary_genre == "Drama" ~ "Drama",
    primary_genre %in% c("Comedy", "Romantic Comedy") ~ "Comedy and Rom Coms",
    primary_genre == "Horror" ~ "Horror",
    primary_genre == "Suspense" ~ "Suspense",
    primary_genre %in% c("Animation", "Family") ~ "Animation and Family",
    primary_genre %in% c("Science Fiction", "Adventure") ~ "Sci-Fi and Adventure",
    primary_genre %in% c("Musical", "Romance", "Western") ~ "Other"
  )) %>%
  mutate(genre_grouped = factor(genre_grouped))

# check the distribution of new genre_grouped variable 
table(super_final_metadata_with_holiday$genre_grouped)

############# coding the rating column properly (these are MPAA ratings)
unique(super_final_metadata_with_holiday$rating)
table(super_final_metadata_with_holiday$rating)

# creating a categorical variable called MPAA_rating
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  mutate(
    MPAA_rating = case_when(
      rating == 6  ~ "G",
      rating == 12 ~ "PG",
      rating == 14 ~ "PG-13",
      rating == 16 ~ "R",
      rating == 18 ~ "NC-17"
    ),
    MPAA_rating = factor(MPAA_rating, levels = c("G", "PG", "PG-13", "R", "NC-17"))
  )

# checking the distribution of the MPAA_rating variable 
table(super_final_metadata_with_holiday$MPAA_rating)


# checking the structure of the dataset 
str(super_final_metadata_with_holiday)



# checking what day most movies are released  on
# 1. Create a new column with the day of the week
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  mutate(
    release_day = lubridate::wday(release_date, week_start = 1),  # Monday = 1
    release_day_name = factor(
      lubridate::wday(release_date, label = TRUE, abbr = FALSE, week_start = 1),
      ordered = FALSE  # ✅ ensure unordered factor
    )
  )

  
# 1. Get distribution counts while preserving factor order
day_counts <- table(super_final_metadata_with_holiday$release_day_name)
release_day_distribution <- data.frame(
  day = names(day_counts),
  n = as.numeric(day_counts),
  stringsAsFactors = FALSE
)
release_day_distribution$percentage <- release_day_distribution$n / sum(release_day_distribution$n) * 100

# Order by factor levels
release_day_distribution <- release_day_distribution %>%
  arrange(match(day, levels(super_final_metadata_with_holiday$release_day_name)))

print(release_day_distribution)

# make proper table 
release_day_distribution %>%
  mutate(`Cumulative %` = cumsum(percentage)) %>%
  flextable() %>%
  set_header_labels(day = "Day of Week",
                    n = "Releases",
                    percentage = "Percentage (%)",
                    `Cumulative %` = "Cumulative (%)") %>%
  colformat_num(j = c("percentage", "Cumulative %"), digits = 2, suffix = "%") %>%
  bold(part = "header") %>%
  bg(i = ~ day == "Thursday", bg = "#E6F2FF") %>%
  autofit()




# Convert distributor_power to factor and check levels
super_final_metadata_with_holiday$distributor_power <- as.factor(super_final_metadata_with_holiday$distributor_power)
levels(super_final_metadata_with_holiday$distributor_power)

# convert director_power into factor and check levels 
super_final_metadata_with_holiday$director_power <- as.factor(super_final_metadata_with_holiday$director_power)
levels(super_final_metadata_with_holiday$director_power)


################# creating some more control variables such as number of movies released in the same week
# creating week of the year variable 
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  mutate(
    release_week = isoweek(release_date)  # 1–53, ISO standard
  )


# creating number of movies released in the same week variable 
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  group_by(year, release_week) %>%
  mutate(num_movies_same_week = n() - 1) %>%
  ungroup()


# cleanup unnecessary columns and save finalized meta data 
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  select(-Unnamed..0, -release_date_20)

names(super_final_metadata_with_holiday)

write.csv(super_final_metadata_with_holiday, "fully_final_metadata.csv")


####### top and bottom 10 movies based on opening_weekend_eur
# Select and arrange top 10 highest opening weekend movies
top_10_movies <- super_final_metadata_with_holiday %>%
  dplyr::select(title, release_date, opening_locs, opening_weekend_eur,
                primary_genre = genre_grouped, run_time, MPAA_rating, n_comments, star_power_count) %>%
  arrange(desc(opening_weekend_eur)) %>%
  slice_head(n = 10)

# Select and arrange bottom 10 lowest opening weekend movies
bottom_10_movies <- super_final_metadata_with_holiday %>%
  dplyr::select(title, release_date, opening_locs, opening_weekend_eur,
                primary_genre = genre_grouped, run_time, MPAA_rating, n_comments, star_power_count) %>%
  arrange(opening_weekend_eur) %>%
  slice_head(n = 10)

# Optional: View tables nicely
print(top_10_movies)
print(bottom_10_movies)

# proper formatting of the tables 
# Top 10 table with renamed columns
top_10_movies_clean <- top_10_movies %>%
  dplyr::rename(
    Title = title,
    `Release Date` = release_date,
    `Opening Locations` = opening_locs,
    `Opening Weekend (Euros)` = opening_weekend_eur,
    `Primary Genre` = primary_genre,
    `Run Time` = run_time,
    `MPAA Rating` = MPAA_rating,
    `Number of Comments` = n_comments, 
    `Number of Star Actors` = star_power_count
  )

# Bottom 10 table with renamed columns
bottom_10_movies_clean <- bottom_10_movies %>%
  dplyr::rename(
    Title = title,
    `Release Date` = release_date,
    `Opening Locations` = opening_locs,
    `Opening Weekend (Euros)` = opening_weekend_eur,
    `Primary Genre` = primary_genre,
    `Run Time` = run_time,
    `MPAA Rating` = MPAA_rating,
    `Number of Comments` = n_comments, 
    `Number of Star Actors` = star_power_count
  )

# Display nicely formatted tables
kable(top_10_movies_clean, caption = "1. Top 10 Movies by Opening Weekend Revenue") %>%
  kable_styling(full_width = FALSE, position = "center")

kable(bottom_10_movies_clean, caption = "2. Bottom 10 Movies by Opening Weekend Revenue") %>%
  kable_styling(full_width = FALSE, position = "center")


# checking top 20 movies with the highest opening weekend
super_final_metadata %>%
  arrange(desc(opening_weekend_eur)) %>%
  select(title, opening_weekend_eur, primary_genre, star_power_count, director_power, distributor_power) %>%
  head(20)




########################## Inspecting Comments Scored By New Fine-Tuned BERT Classifier ###############################
# load in the dataset
# Load the labeled dataset
# NOTE: This file is not included in the public repository (>50MB). See README for details.
super_final_classified_comments <- read.csv("super_final_comments_labeled.csv")

# View the structure of the dataset
str(super_final_classified_comments)

# Preview the first few rows
head(super_final_classified_comments)

# Check sentiment distribution
table(super_final_classified_comments$bert_sentiment)

# Optionally, explore confidence scores
summary(super_final_classified_comments$bert_confidence)


#################### creating buzz volume and valence variables 
### let's create number of pre-release comments per title variable (buzz volume) 
# Step 1: Count comments per title
comment_counts <- super_final_classified_comments %>%
  group_by(title) %>%
  summarise(n_comments = n(), .groups = "drop")

# Step 2: Left join the count to your metadata
super_final_metadata_with_holiday <- super_final_metadata_with_holiday %>%
  left_join(comment_counts, by = "title")


### let's create variables for proportions of positive, negative and neutral comments (buzz valence)
# Calculate sentiment proportions per title
sentiment_proportions <- super_final_classified_comments %>%
  dplyr::group_by(title, bert_sentiment) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(title) %>%
  dplyr::mutate(total = sum(count),
                proportion = count / total) %>%
  dplyr::select(title, bert_sentiment, proportion) %>%
  tidyr::pivot_wider(names_from = bert_sentiment, values_from = proportion, 
                     values_fill = list(proportion = 0))

# rename the proportion columns
sentiment_proportions <- sentiment_proportions %>%
  dplyr::rename(
    prop_neg = negative,
    prop_neut = neutral,
    prop_pos = positive
  )


# add sentiment proportions to the meta data for a complete dataset 
super_duper_final_meta_data_with_sent <- inner_join(super_final_metadata_with_holiday, sentiment_proportions, by = "title")


# log transform the skewed variables, namely opening_weekend_eur, n_comments and star_power_count
super_duper_final_meta_data_with_sent <- super_duper_final_meta_data_with_sent %>%
  mutate(
    log_opening_weekend_eur = log(opening_weekend_eur),
    log_n_comments = log(n_comments),
    log_star_power_count = log1p(star_power_count)
  )




############# adding one last control variable: THE SEQUEL ##############################
# Vector of sequel titles
sequel_titles <- c(
  "Once Upon A Deadpool", 
  "Glass", 
  "Happy Death Day 2U", 
  "Lego Movie 2: The Second Part, The",
  "Avengers: Endgame", 
  "John Wick: Chapter 3 - Parabellum (2019)", 
  "Godzilla: King Of The Monsters (2019)",
  "Dark Phoenix", 
  "Men In Black International", 
  "Annabelle Comes Home", 
  "Spider-Man: Far From Home",
  "Escape Plan 3: The Extractors", 
  "Fast & Furious Presents: Hobbs & Shaw", 
  "Angel Has Fallen",
  "47 Meters Down: Uncaged", 
  "IT Chapter Two", 
  "Rambo: Last Blood", 
  "Maleficent: Mistress Of Evil",
  "Zombieland: Double Tap", 
  "Terminator: Dark Fate", 
  "Doctor Sleep", 
  "Jumanji: The Next Level",
  "Star Wars: The Rise Of Skywalker", 
  "Bad Boys For Life", 
  "Birds Of Prey (And The Fantabulous Emancipation Of One Harley Quinn)",
  "Brahms: The Boy II", 
  "After We Collided", 
  "Bill & Ted Face The Music", 
  "Craft: Legacy, The",
  "Wonder Woman 1984", 
  "Godzilla vs. Kong", 
  "Peter Rabbit 2: The Runaway", 
  "Quiet Place Part II, A",
  "Conjuring: The Devil Made Me Do It, The", 
  "Hitman's Wife's Bodyguard, The", 
  "F9 The Fast Saga",
  "Escape Room: Tournament of Champions", 
  "Space Jam: A New Legacy", 
  "Suicide Squad, The", 
  "Don't Breathe 2",
  "Candyman", 
  "After We Fell (Event)", 
  "Rock Dog 2: Walk Around the Park", 
  "No Time To Die",
  "Venom: Let There Be Carnage", 
  "Halloween Kills", 
  "Addams Family 2, The", 
  "Ghostbusters: Afterlife",
  "Spider-Man: No Way Home", 
  "Matrix Resurrections, The", 
  "King's Man, The", 
  "Scream",
  "Death on the Nile", 
  "Jackass Forever", 
  "Fantastic Beasts: The Secrets of Dumbledore",
  "Doctor Strange in the Multiverse of Madness", 
  "Top Gun Maverick", 
  "Jurassic World Dominion",
  "Thor: Love And Thunder", 
  "After Ever Happy (Event)", 
  "Halloween Ends", 
  "Confess, Fletch",
  "Black Panther: Wakanda Forever", 
  "Avatar: The Way of Water", 
  "Terrifier 2", 
  "Magic Mike's Last Dance",
  "Ant-Man and the Wasp: Quantumania", 
  "Missing (2023)", 
  "Creed III", 
  "Scream VI",
  "Shazam! Fury Of The Gods", 
  "John Wick: Chapter 4", 
  "Evil Dead Rise", 
  "Guardians of the Galaxy Vol. 3",
  "Book Club: The Next Chapter", 
  "Jeepers Creepers Reborn", 
  "Fast X", 
  "Spider-Man: Across The Spider-Verse",
  "Transformers: Rise of the Beasts", 
  "Indiana Jones and the Dial of Destiny", 
  "Insidious: The Red Door",
  "Mission: Impossible - Dead Reckoning Part One", 
  "Meg 2: The Trench", 
  "Equalizer 3, The",
  "Nun II, The", 
  "After Everything (Event)", 
  "Haunting in Venice, A", 
  "Expend4bles",
  "Exorcist: Believer, The", 
  "Saw X", 
  "Marvels, The", 
  "Hunger Games: The Ballad of Songbirds & Snakes, The",
  "Aquaman and the Lost Kingdom", 
  "Dune: Part Two", 
  "Ghostbusters: Frozen Empire",
  "Godzilla x Kong: The New Empire", 
  "First Omen, The", 
  "Kingdom of the Planet of the Apes",
  "Furiosa: A Mad Max Saga", 
  "Bad Boys: Ride or Die", 
  "Quiet Place: Day One, A", 
  "Inside Out 2",
  "Twisters", 
  "Deadpool & Wolverine", 
  "Alien: Romulus", 
  "Hellboy: The Crooked Man",
  "Beetlejuice Beetlejuice", 
  "Joker: Folie à Deux", 
  "Smile 2", 
  "Venom: The Last Dance",
  "Gladiator II", 
  "Lord of the Rings: The War of the Rohirrim", 
  "Mufasa: The Lion King",
  "Sonic The Hedgehog 3"
)

# Create is_sequel variable (1 if title is in the sequel list, 0 otherwise)
super_duper_final_meta_data_with_sent$is_sequel <- ifelse(
  super_duper_final_meta_data_with_sent$title %in% sequel_titles, 1, 0
)

# Convert is_sequel to factor with meaningful labels
super_duper_final_meta_data_with_sent$is_sequel <- factor(
  super_duper_final_meta_data_with_sent$is_sequel,
  levels = c(0, 1),
  labels = c("Not Sequel", "Sequel")
)



################### descriptive table for average opening weekend revenue and number of comments 
# Create summary table
summary_table <- super_duper_final_meta_data_with_sent %>%
  summarise(
    `Mean Opening Weekend (€)` = mean(opening_weekend_eur, na.rm = TRUE),
    `Median Opening Weekend (€)` = median(opening_weekend_eur, na.rm = TRUE),
    `SD Opening Weekend (€)` = sd(opening_weekend_eur, na.rm = TRUE),
    `Min Opening Weekend (€)` = min(opening_weekend_eur, na.rm = TRUE),
    `Max Opening Weekend (€)` = max(opening_weekend_eur, na.rm = TRUE),
    
    `Mean Comment Volume` = mean(n_comments, na.rm = TRUE),
    `Median Comment Volume` = median(n_comments, na.rm = TRUE),
    `SD Comment Volume` = sd(n_comments, na.rm = TRUE),
    `Min Comment Volume` = min(n_comments, na.rm = TRUE),
    `Max Comment Volume` = max(n_comments, na.rm = TRUE)
  )

# View the summary table in a readable format
summary_table %>%
  t() %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("Metric") %>%
  setNames(c("Metric", "Value")) %>%
  print(digits = 5)


####### checking the distributions of numeric variables in the final dataset with buzz volume and valence 
# 1. Histogram for opening weekend EUR
ggplot(super_duper_final_meta_data_with_sent, aes(x = opening_weekend_eur)) +
  geom_histogram(binwidth = 50000, fill = "#0073C2FF", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Opening Weekend Box Office (Euros)",
    x = "Opening Weekend Revenue (Euros)",
    y = "Number of Movies"
  ) +
  theme_minimal()

# 2. Histogram for number of comments 
ggplot(super_duper_final_meta_data_with_sent, aes(x = n_comments)) +
  geom_histogram(binwidth = 10000, fill = "yellow", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Comment Volume",
    x = "Number of Comments per Movie ",
    y = "Number of Movies"
  ) +
  theme_minimal()

# 3. Opening locations
ggplot(super_duper_final_meta_data_with_sent, aes(x = opening_locs)) +
  geom_histogram(fill = "red", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Opening Locations",
    x = "Number of Opening Locations",
    y = "Number of Movies"
  ) +
  theme_minimal()

# 4. number of stars per movie 
ggplot(super_duper_final_meta_data_with_sent, aes(x = star_power_count)) +
  geom_histogram(fill = "green", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Star Power Count",
    x = "Number of Stars per Movie",
    y = "Number of Movies"
  ) +
  theme_minimal()

# 5. run time 
ggplot(super_duper_final_meta_data_with_sent, aes(x = run_time)) +
  geom_histogram(fill = "orange", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Movie Run Times",
    x = "Run Time In Minutes",
    y = "Number of Movies"
  ) +
  theme_minimal()


########### trying to arrange the plots better
base_histogram_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# 1. Opening Weekend Revenue
p1 <- ggplot(super_duper_final_meta_data_with_sent, aes(x = opening_weekend_eur)) +
  geom_histogram(binwidth = 50000, fill = "#0073C2FF", color = "white", alpha = 0.9) +
  labs(title = "Opening Weekend Box Office (€)", x = "Revenue (€)", y = "Number of Movies") +
  scale_x_continuous(labels = comma) +
  base_histogram_theme

# 2. Number of Comments
p2 <- ggplot(super_duper_final_meta_data_with_sent, aes(x = n_comments)) +
  geom_histogram(binwidth = 10000, fill = "#FAD02E", color = "white", alpha = 0.9) +
  labs(title = "Pre-Release Comment Volume", x = "Comments per Movie", y = "Number of Movies") +
  scale_x_continuous(labels = comma) +
  base_histogram_theme

# 3. Opening Locations
p3 <- ggplot(super_duper_final_meta_data_with_sent, aes(x = opening_locs)) +
  geom_histogram(binwidth = 5, fill = "#E74C3C", color = "white", alpha = 0.9) +
  labs(title = "Opening Locations", x = "Number of Locations", y = "Number of Movies") +
  base_histogram_theme

# 4. Star Power Count
p4 <- ggplot(super_duper_final_meta_data_with_sent, aes(x = star_power_count)) +
  geom_histogram(binwidth = 1, fill = "#2ECC71", color = "white", alpha = 0.9) +
  labs(title = "Star Power Count", x = "Number of Top Stars", y = "Number of Movies") +
  base_histogram_theme

# 5. Movie Runtime
p5 <- ggplot(super_duper_final_meta_data_with_sent, aes(x = run_time)) +
  geom_histogram(binwidth = 10, fill = "#F39C12", color = "white", alpha = 0.9) +
  labs(title = "Distribution of Movie Runtime", x = "Run Time (min)", y = "Number of Movies") +
  base_histogram_theme

# arrange the plots in a grid and then save them
multi_plot <- grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
ggsave("all_histograms.png", plot = multi_plot, width = 12, height = 8, dpi = 300)



########### scatter plot of number of comments and target
# Compute correlation between log_n_comments and log_opening_weekend_eur (excluding NAs)
correlation <- cor(
  super_duper_final_meta_data_with_sent$log_n_comments, 
  super_duper_final_meta_data_with_sent$log_opening_weekend_eur, 
  use = "complete.obs",
  method = "pearson"
)


# Create subtitle with correlation
subtitle_text <- paste0(
  "Scatter plot with LOESS trend line (n = 572, r = ", 
  round(correlation, 2), ")"
)

# Plot
n_comments_scatter <- ggplot(super_duper_final_meta_data_with_sent, 
                             aes(x = log_n_comments, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, size = 2, shape = 21, fill = "#0073C2FF", color = "white", stroke = 0.3) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 1) +
  labs(
    title = "Relationship Between Comment Volume and Opening Weekend Revenue",
    subtitle = subtitle_text,
    x = expression("Number of Comments (log scale)"),
    y = expression("Opening Weekend Revenue (log scale)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = ggplot2::margin(b = unit(10, "pt"))),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# save this plot
ggsave("volume_histogram.png", plot = n_comments_scatter, width = 12, height = 8, dpi = 300)





########## box plots for categorical variables vs the target variable
# 1. Genre Box Plot
box_genre <- ggplot(super_duper_final_meta_data_with_sent, 
                    aes(x = genre_grouped, y = log_opening_weekend_eur)) +
  geom_boxplot(fill = "#A6CEE3", color = "#1F78B4") +
  labs(title = "Opening Weekend Revenue by Genre",
       x = "Genre", y = "Log Opening Weekend Revenue") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))

# 2. Director Power
box_director <- ggplot(super_duper_final_meta_data_with_sent, 
                       aes(x = director_power, y = log_opening_weekend_eur)) +
  geom_boxplot(fill = "#FDBF6F", color = "#FF7F00") +
  labs(title = "By Director Power", x = "Director Power", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 3. Distributor Power
box_distributor <- ggplot(super_duper_final_meta_data_with_sent, 
                          aes(x = distributor_power, y = log_opening_weekend_eur)) +
  geom_boxplot(fill = "#B2DF8A", color = "#33A02C") +
  labs(title = "By Distributor Power", x = "Distributor Power", y = "Log Opening Weekend Revenue") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 4. MPAA Rating
box_mpaa <- ggplot(super_duper_final_meta_data_with_sent, 
                   aes(x = MPAA_rating, y = log_opening_weekend_eur)) +
  geom_boxplot(fill = "#FB9A99", color = "#E31A1C") +
  labs(title = "By MPAA Rating", x = "MPAA Rating", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# Combine with patchwork
final_combined <- (box_genre | box_director) / (box_distributor | box_mpaa) +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))
  )

# Save the combined figure
ggsave("boxplots_categorical_vs_logrevenue.png", final_combined, 
       width = 12, height = 8, dpi = 300)


################ correlation plot for numeric variables
# Select numeric variables from your dataset
# Use explicit namespacing to avoid function masking
numeric_vars <- super_duper_final_meta_data_with_sent %>%
  dplyr::select(log_opening_weekend_eur, opening_locs, run_time, log_n_comments, prop_pos, prop_neg, prop_neut, log_star_power_count)

# Compute correlation matrix (use complete.obs to avoid NA issues)
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Plot the correlation matrix
ggcorrplot(cor_matrix,
           method = "circle",         
           type = "lower",            
           lab = TRUE,                
           lab_size = 4,              
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Numeric Variables",
           ggtheme = theme_minimal(base_size = 12)) +  # Larger base font
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )




################# exploration of buzz valence variables for the results section
# Assuming your data is called super_duper_final_meta_data_with_sent
valence_summary <- super_duper_final_meta_data_with_sent %>%
  summarise(
    Positive_Min = min(prop_pos),
    Positive_Median = median(prop_pos),
    Positive_Mean = mean(prop_pos),
    Positive_Max = max(prop_pos),
    Positive_SD = sd(prop_pos),
    
    Neutral_Min = min(prop_neut),
    Neutral_Median = median(prop_neut),
    Neutral_Mean = mean(prop_neut),
    Neutral_Max = max(prop_neut),
    Neutral_SD = sd(prop_neut),
    
    Negative_Min = min(prop_neg),
    Negative_Median = median(prop_neg),
    Negative_Mean = mean(prop_neg),
    Negative_Max = max(prop_neg),
    Negative_SD = sd(prop_neg)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = c("Sentiment", "Metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  dplyr::select(Sentiment, Mean, Median, SD, Min, Max)

# Round and display nicely
valence_summary %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  kable(caption = "Descriptive Statistics for Buzz Valence Variables",
        col.names = c("Sentiment", "Mean", "Median", "SD", "Min", "Max"))


# Positive Sentiment Plot
plot_pos <- ggplot(super_duper_final_meta_data_with_sent, aes(x = prop_pos, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#1f77b4", linetype = "dashed") +
  labs(
    title = "Relationship Between Positive Sentiment and Opening Weekend Revenue",
    x = "Proportion of Positive Comments",
    y = "Log Opening Weekend Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave("positive_sentiment_vs_log_revenue.png", plot = plot_pos, width = 10, height = 6, dpi = 300)

# Neutral Sentiment Plot
plot_neut <- ggplot(super_duper_final_meta_data_with_sent, aes(x = prop_neut, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, color = "#ff7f0e") +
  geom_smooth(method = "lm", se = TRUE, color = "#ff7f0e", linetype = "dashed") +
  labs(
    title = "Relationship Between Neutral Sentiment and Opening Weekend Revenue",
    x = "Proportion of Neutral Comments",
    y = "Log Opening Weekend Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave("neutral_sentiment_vs_log_revenue.png", plot = plot_neut, width = 10, height = 6, dpi = 300)

# Negative Sentiment Plot
plot_neg <- ggplot(super_duper_final_meta_data_with_sent, aes(x = prop_neg, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, color = "#d62728") +
  geom_smooth(method = "lm", se = TRUE, color = "#d62728", linetype = "dashed") +
  labs(
    title = "Relationship Between Negative Sentiment and Opening Weekend Revenue",
    x = "Proportion of Negative Comments",
    y = "Log Opening Weekend Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave("negative_sentiment_vs_log_revenue.png", plot = plot_neg, width = 10, height = 6, dpi = 300)



# Positive Sentiment
plot_pos <- ggplot(super_duper_final_meta_data_with_sent,
                   aes(x = prop_pos, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, size = 2, shape = 21, fill = "#1f77b4", color = "white", stroke = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "#1f77b4", linetype = "dashed") +
  labs(
    title = paste0("Positive Sentiment (r = ", round(cor(super_duper_final_meta_data_with_sent$prop_pos,
                                                         super_duper_final_meta_data_with_sent$log_opening_weekend_eur), 2), ")"),
    x = "Proportion of Positive Comments",
    y = "Log Opening Weekend Revenue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )

# Neutral Sentiment
plot_neut <- ggplot(super_duper_final_meta_data_with_sent,
                    aes(x = prop_neut, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, size = 2, shape = 21, fill = "#ff7f0e", color = "white", stroke = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "#ff7f0e", linetype = "dashed") +
  labs(
    title = paste0("Neutral Sentiment (r = ", round(cor(super_duper_final_meta_data_with_sent$prop_neut,
                                                        super_duper_final_meta_data_with_sent$log_opening_weekend_eur), 2), ")"),
    x = "Proportion of Neutral Comments",
    y = "Log Opening Weekend Revenue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )

# Negative Sentiment
plot_neg <- ggplot(super_duper_final_meta_data_with_sent,
                   aes(x = prop_neg, y = log_opening_weekend_eur)) +
  geom_point(alpha = 0.6, size = 2, shape = 21, fill = "#d62728", color = "white", stroke = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "#d62728", linetype = "dashed") +
  labs(
    title = paste0("Negative Sentiment (r = ", round(cor(super_duper_final_meta_data_with_sent$prop_neg,
                                                         super_duper_final_meta_data_with_sent$log_opening_weekend_eur), 2), ")"),
    x = "Proportion of Negative Comments",
    y = "Log Opening Weekend Revenue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )


# Combine plots into a row
combined_plot <- plot_grid(plot_pos, plot_neut, plot_neg, ncol = 3, align = "h", labels = c("A", "B", "C"))

# Save the combined plot
ggsave("sentiment_scatter_combined_cowplot.png", combined_plot, width = 16, height = 6, dpi = 300)

############ let's divide the data into training and test sets and check the simple linear regression model's performance (NO SENTIMENT VARIABLES!!!!!)
# Set seed for reproducibility
set.seed(42)

# Step 1: Split the data (80% train, 20% test)
split_index <- createDataPartition(super_duper_final_meta_data_with_sent$log_opening_weekend_eur, p = 0.8, list = FALSE)
train_data <- super_duper_final_meta_data_with_sent[split_index, ]
test_data <- super_duper_final_meta_data_with_sent[-split_index, ]

# check the structures of the train and test datasets
str(train_data)
str(test_data)


################ 1. let's build a linear regression model without any buzz
# Step 1: Fit the model on training data
lm_model_no_buzz <- lm(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                             distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                          num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel, data = train_data)


summary(lm_model_no_buzz)

# Step 3: Predict on test data
predictions_lm_no_buzz <- predict(lm_model_no_buzz, newdata = test_data)

# Step 4: Evaluate performance
actuals <- test_data$log_opening_weekend_eur
rmse_lm_no_buzz <- sqrt(mean((predictions_lm_no_buzz - actuals)^2))
mae_lm_no_buzz <- mean(abs(predictions_lm_no_buzz - actuals))
r_squared_lm_no_buzz <- 1 - sum((predictions_lm_no_buzz - actuals)^2) / sum((actuals - mean(actuals))^2)

# Output results
cat("Test Set Evaluation Linear Regression No Buzz:\n")
cat("RMSE:", round(rmse_lm_no_buzz, 4), "\n")
cat("MAE:", round(mae_lm_no_buzz, 4), "\n")
cat("R-squared:", round(r_squared_lm_no_buzz, 4), "\n")


############## 2. Linear Model with buzz volume only
# Step 2: Fit the model on training data
lm_model_vol_only <- lm(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                         distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                         num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel, data = train_data)


summary(lm_model_vol_only)


# Step 3: Predict on test data
predictions_lm_vol_only <- predict(lm_model_vol_only, newdata = test_data)

# Step 4: Evaluate performance
actuals <- test_data$log_opening_weekend_eur
rmse_lm_vol_only <- sqrt(mean((predictions_lm_vol_only - actuals)^2))
mae_lm_vol_only <- mean(abs(predictions_lm_vol_only - actuals))
r_squared_lm_vol_only <- 1 - sum((predictions_lm_vol_only - actuals)^2) / sum((actuals - mean(actuals))^2)

# Output results
cat("Test Set Evaluation Linear Model Volume Only:\n")
cat("RMSE:", round(rmse_lm_vol_only, 4), "\n")
cat("MAE:", round(mae_lm_vol_only, 4), "\n")
cat("R-squared:", round(r_squared_lm_vol_only, 4), "\n")



############## 3. Linear Model with buzz volume and valence 
# Step 2: Fit the model on training data
lm_model_vol_val <- lm(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                          distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                          num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + prop_pos + prop_neg + is_sequel, data = train_data)


summary(lm_model_vol_val)


# Step 3: Predict on test data
predictions_lm_vol_val <- predict(lm_model_vol_val, newdata = test_data)

# Step 4: Evaluate performance
actuals <- test_data$log_opening_weekend_eur
rmse_lm_vol_val <- sqrt(mean((predictions_lm_vol_val - actuals)^2))
mae_lm_vol_val <- mean(abs(predictions_lm_vol_val - actuals))
r_squared_lm_vol_val <- 1 - sum((predictions_lm_vol_val - actuals)^2) / sum((actuals - mean(actuals))^2)

# Output results
cat("Test Set Evaluation Linear Model Volume and Valence:\n")
cat("RMSE:", round(rmse_lm_vol_val, 4), "\n")
cat("MAE:", round(mae_lm_vol_val, 4), "\n")
cat("R-squared:", round(r_squared_lm_vol_val, 4), "\n")



########################### ROBUSTNESS CHECK: MODEL PERFORMANCE WITHOUT OPENING LOCATIONS VARIABLE #############################
###### 1. OLS regression no buzz
# Step 1: Fit the model on training data
lm_model_no_buzz_no_locs <- lm(log_opening_weekend_eur ~ run_time + log_star_power_count + director_power + 
                         distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                         num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel, data = train_data)


summary(lm_model_no_buzz_no_locs)

# Step 3: Predict on test data
predictions_lm_no_buzz_no_locs <- predict(lm_model_no_buzz_no_locs, newdata = test_data)

# Step 4: Evaluate performance
actuals <- test_data$log_opening_weekend_eur
rmse_lm_no_buzz_no_locs <- sqrt(mean((predictions_lm_no_buzz_no_locs - actuals)^2))
mae_lm_no_buzz_no_locs <- mean(abs(predictions_lm_no_buzz_no_locs - actuals))
r_squared_lm_no_buzz_no_locs <- 1 - sum((predictions_lm_no_buzz_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

# Output results
cat("Test Set Evaluation Linear Regression No Buzz and No opening Locations:\n")
cat("RMSE:", round(rmse_lm_no_buzz_no_locs, 4), "\n")
cat("MAE:", round(mae_lm_no_buzz_no_locs, 4), "\n")
cat("R-squared:", round(r_squared_lm_no_buzz_no_locs, 4), "\n")


############## 2. Linear Model with VOLUME ONLY!!!!!
# Step 2: Fit the model on training data
lm_model_vol_only_no_locs <- lm(log_opening_weekend_eur ~ run_time + log_star_power_count + director_power + 
                          distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                          num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel, data = train_data)


summary(lm_model_vol_only_no_locs)

# Step 3: Predict on test data
predictions_lm_vol_only_no_locs <- predict(lm_model_vol_only_no_locs, newdata = test_data)

# Step 4: Evaluate performance
actuals <- test_data$log_opening_weekend_eur
rmse_lm_vol_only_no_locs <- sqrt(mean((predictions_lm_vol_only_no_locs - actuals)^2))
mae_lm_vol_only_no_locs <- mean(abs(predictions_lm_vol_only_no_locs - actuals))
r_squared_lm_vol_only_no_locs <- 1 - sum((predictions_lm_vol_only_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

# Output results
cat("Test Set Evaluation Linear Model Volume Only No Opening Locations:\n")
cat("RMSE:", round(rmse_lm_vol_only_no_locs, 4), "\n")
cat("MAE:", round(mae_lm_vol_only_no_locs, 4), "\n")
cat("R-squared:", round(r_squared_lm_vol_only_no_locs, 4), "\n")

############## 3. Linear Model with VOLUME AND VALENCE!!!!!
# Step 2: Fit the model on training data
lm_model_vol_val_no_locs <- lm(log_opening_weekend_eur ~ run_time + log_star_power_count + director_power + 
                         distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                         num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + prop_pos + prop_neg + is_sequel, data = train_data)


summary(lm_model_vol_val_no_locs)

# Step 3: Predict on test data
predictions_lm_vol_val_no_locs <- predict(lm_model_vol_val_no_locs, newdata = test_data)

# Step 4: Evaluate performance
actuals <- test_data$log_opening_weekend_eur
rmse_lm_vol_val_no_locs <- sqrt(mean((predictions_lm_vol_val_no_locs - actuals)^2))
mae_lm_vol_val_no_locs <- mean(abs(predictions_lm_vol_val_no_locs - actuals))
r_squared_lm_vol_val_no_locs <- 1 - sum((predictions_lm_vol_val_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

# Output results
cat("Test Set Evaluation Linear Model Volume and Valence No Opening Locations:\n")
cat("RMSE:", round(rmse_lm_vol_val_no_locs, 4), "\n")
cat("MAE:", round(mae_lm_vol_val_no_locs, 4), "\n")
cat("R-squared:", round(r_squared_lm_vol_val_no_locs, 4), "\n")


# variable importance for linear regression with buzz volume and valence (no opening locations)
# Extract coefficients and take absolute values
coef_lm_values <- summary(lm_model_vol_val_no_locs)$coefficients
coef_lm_df <- data.frame(
  Variable = rownames(coef_lm_values),
  Coefficient = coef_lm_values[, "Estimate"]
)

# Remove the intercept
coef_lm_df <- coef_lm_df[coef_lm_df$Variable != "(Intercept)", ]

# Compute absolute coefficients for importance
coef_lm_df$Abs_Coefficient <- abs(coef_lm_df$Coefficient)

# Get top 10 most important variables
top_coef_lm_df <- coef_lm_df[order(-coef_lm_df$Abs_Coefficient), ][1:10, ]

# Plot
ggplot(top_coef_lm_df, aes(x = reorder(Variable, Abs_Coefficient), y = Abs_Coefficient)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importances (Linear Regression Without Opening Locations)",
    x = "Variable",
    y = "Absolute Coefficient"
  ) +
  theme_minimal()



###################### building non-linear tree-based machine learning models ################################
################# running a basic random forest without hyper parameter tuning to see whether performance better than linear regression
########## 1. without buzz volume
rf_model_baseline_no_comments <- randomForest(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + era_group + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + is_sequel,
  data = train_data,
  ntree = 500,        # Number of trees
  importance = TRUE   # So you can later view variable importance
)


# predict on test set 
rf_predictions_baseline_no_comments <- predict(rf_model_baseline_no_comments, newdata = test_data)

# evaluate performance of simple random forest baseline 
# Actual values
actuals <- test_data$log_opening_weekend_eur

# Metrics
rmse_rf_baseline_no_comments <- sqrt(mean((rf_predictions_baseline_no_comments - actuals)^2))
mae_rf_baseline_no_comments <- mean(abs(rf_predictions_baseline_no_comments - actuals))
r_squared_rf_baseline_no_comments <- 1 - sum((rf_predictions_baseline_no_comments - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print Results
cat("Untuned Random Forest Performance (without buzz):\n")
cat("RMSE:", round(rmse_rf_baseline_no_comments, 4), "\n")
cat("MAE:", round(mae_rf_baseline_no_comments, 4), "\n")
cat("R-squared:", round(r_squared_rf_baseline_no_comments, 4), "\n")

# checking variable importance plots
varImpPlot(rf_model_baseline_no_comments)


############## 2. with buzz volume
rf_model_baseline_comments_volume <- randomForest(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + era_group + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + log_n_comments + is_sequel,
  data = train_data,
  ntree = 500,        # Number of trees
  importance = TRUE   # So you can later view variable importance
)


# predict on test set 
rf_predictions_baseline_comments_volume <- predict(rf_model_baseline_comments_volume, newdata = test_data)

# evaluate performance of simple random forest baseline 
# Actual values
actuals <- test_data$log_opening_weekend_eur

# Metrics
rmse_rf_baseline_comments_volume <- sqrt(mean((rf_predictions_baseline_comments_volume - actuals)^2))
mae_rf_baseline_comments_volume <- mean(abs(rf_predictions_baseline_comments_volume - actuals))
r_squared_rf_baseline_comments_volume <- 1 - sum((rf_predictions_baseline_comments_volume - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print Results
cat("Untuned Random Forest Performance with volume only:\n")
cat("RMSE:", round(rmse_rf_baseline_comments_volume, 4), "\n")
cat("MAE:", round(mae_rf_baseline_comments_volume, 4), "\n")
cat("R-squared:", round(r_squared_rf_baseline_comments_volume, 4), "\n")

# checking variable importance plots
varImpPlot(rf_model_baseline_comments_volume)


############## 3. with buzz volume and valence
rf_model_baseline_with_sent <- randomForest(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + era_group + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + log_n_comments + prop_pos + prop_neg + is_sequel,
  data = train_data,
  ntree = 500,        # Number of trees
  importance = TRUE   # So you can later view variable importance
)


# predict on test set 
rf_predictions_baseline_with_sent <- predict(rf_model_baseline_with_sent, newdata = test_data)

# evaluate performance of simple random forest baseline 
# Actual values
actuals <- test_data$log_opening_weekend_eur

# Metrics
rmse_rf_baseline_with_sent <- sqrt(mean((rf_predictions_baseline_with_sent - actuals)^2))
mae_rf_baseline_with_sent <- mean(abs(rf_predictions_baseline_with_sent - actuals))
r_squared_rf_baseline_with_sent <- 1 - sum((rf_predictions_baseline_with_sent - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print Results
cat("Untuned Random Forest Performance with volume and valence:\n")
cat("RMSE:", round(rmse_rf_baseline_with_sent, 4), "\n")
cat("MAE:", round(mae_rf_baseline_with_sent, 4), "\n")
cat("R-squared:", round(r_squared_rf_baseline_with_sent, 4), "\n")

# checking variable importance plots
varImpPlot(rf_model_baseline_with_sent)


########## let's use the randomforest package to check the out of bag error as we grow the number of trees

# first identify the optimal number of trees
set.seed(42)
rf_model <- randomForest(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel,
  data = train_data,
  ntree = 1500,
  importance = TRUE
)


# Plot OOB MSE vs. Number of Trees
# Save OOB MSE plot for Random Forest model
png("rf_oob_plot_highres.png", width = 8, height = 6, units = "in", res = 300)


# Generate the plot with improved labels
plot(
  x = 1:rf_model$ntree,
  y = rf_model$mse,
  type = "l",
  lwd = 2,
  col = "darkblue",
  main = "Random Forest Model (No Buzz Variables)",
  sub = "Out-of-Bag Mean Squared Error by Number of Trees",
  xlab = "Number of Trees",
  ylab = "Out-of-Bag Mean Squared Error (MSE)"
)

# Optional: add gridlines for clarity
grid(col = "gray80")

# Close the file device
dev.off()


######## now that we have identified the optimal number of trees as 1500, let's run a random forest model and test it's performance 
########## 1. random forest without buzz 
# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "variance",  # For regression
  min.node.size = c(1, 5, 10, 20)
)

# 10-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# set seed and train model
set.seed(42)

rf_model_no_buzz <- caret::train(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  num.trees = 1500,
  importance = "permutation",
  metric = "RMSE"
)

# evaluate the model to check the best parameters 
print(rf_model_no_buzz)
plot(rf_model_no_buzz)

# Plot with clear labels
plot_rf <- ggplot(rf_model_no_buzz$results, 
                  aes(x = mtry, y = RMSE, color = factor(min.node.size))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Random Forest Model Tuning (No Buzz Variables)",
    subtitle = "10-Fold CV RMSE by mtry and Minimum Node Size",
    x = "Number of Predictors (mtry)",
    y = "RMSE (10-Fold CV)",
    color = "Min Node Size"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(hjust = 0.5)
  )

# Save high-res image
ggsave("rf_tuning_no_buzz_centered.png", plot_rf, width = 8, height = 6, dpi = 300)

# let's predict on the test set and see how well the model generalizes
predictions_rf_no_buzz <- predict(rf_model_no_buzz, newdata = test_data)

# Compute test metrics
actuals <- test_data$log_opening_weekend_eur
rmse_rf_no_buzz <- sqrt(mean((predictions_rf_no_buzz - actuals)^2))
mae_rf_no_buzz <- mean(abs(predictions_rf_no_buzz - actuals))
r_squared_rf_no_buzz <- 1 - sum((predictions_rf_no_buzz - actuals)^2) / sum((actuals - mean(actuals))^2)

cat("Test Set Evaluation RF No Buzz:\n")
cat("RMSE:", round(rmse_rf_no_buzz, 4), "\n")
cat("MAE:", round(mae_rf_no_buzz, 4), "\n")
cat("R-squared:", round(r_squared_rf_no_buzz, 4), "\n")



############## 2. random forest with buzz volume only!!!
# first identify the optimal number of trees
set.seed(42)
rf_model_volume_only <- randomForest(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel,
  data = train_data,
  ntree = 1500,
  importance = TRUE
)

# Plot OOB MSE vs. Number of Trees
plot(rf_model_volume_only)  # For regression, this shows MSE vs trees
# we use this plot to check the optimal number of trees 


# Plot OOB MSE vs. Number of Trees
# Save OOB MSE plot for Random Forest model
png("rf_oob_plot_vol_highres.png", width = 8, height = 6, units = "in", res = 300)


# Generate the plot with improved labels
plot(
  x = 1:rf_model_volume_only$ntree,
  y = rf_model_volume_only$mse,
  type = "l",
  lwd = 2,
  col = "darkblue",
  main = "Random Forest Model (Buzz Volume Only)",
  sub = "Out-of-Bag Mean Squared Error by Number of Trees",
  xlab = "Number of Trees",
  ylab = "Out-of-Bag Mean Squared Error (MSE)"
)

# Optional: add gridlines for clarity
grid(col = "gray80")

# Close the file device
dev.off()



########## 2. random forest with buzz volume only 
# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "variance",  # For regression
  min.node.size = c(1, 5, 10, 20)
)

# 10-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# set seed and train model
set.seed(42)

rf_ranger_model_volume_only <- caret::train(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  num.trees = 1500, # because according to the above plot, error was constant after 500 trees 
  importance = "permutation",
  metric = "RMSE"
)

# evaluate the model to check the best parameters 
print(rf_ranger_model_volume_only)
plot(rf_ranger_model_volume_only)

# Plot with clear labels
plot_rf <- ggplot(rf_ranger_model_volume_only$results, 
                  aes(x = mtry, y = RMSE, color = factor(min.node.size))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Random Forest Model Tuning (Buzz Volume Only)",
    subtitle = "10-Fold CV RMSE by mtry and Minimum Node Size",
    x = "Number of Predictors (mtry)",
    y = "RMSE (10-Fold CV)",
    color = "Min Node Size"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(hjust = 0.5)
  )

# Save high-res image
ggsave("rf_tuning_vol_only_centered.png", plot_rf, width = 8, height = 6, dpi = 300)



# let's predict on the test set and see how well the model generalizes
predictions_rf_volume_only <- predict(rf_ranger_model_volume_only, newdata = test_data)

# Compute test metrics
actuals <- test_data$log_opening_weekend_eur
rmse_rf_volume_only <- sqrt(mean((predictions_rf_volume_only - actuals)^2))
mae_rf_volume_only <- mean(abs(predictions_rf_volume_only - actuals))
r_squared_rf_volume_only <- 1 - sum((predictions_rf_volume_only - actuals)^2) / sum((actuals - mean(actuals))^2)


cat("Test Set Evaluation Random Forest Volume Only:\n")
cat("RMSE:", round(rmse_rf_volume_only, 4), "\n")
cat("MAE:", round(mae_rf_volume_only, 4), "\n")
cat("R-squared:", round(r_squared_rf_volume_only, 4), "\n")


# still linear regression the best model so far. let's try some other machine learning models 

########## 3. random forest with buzz volume and valence
# first identify the optimal number of trees
set.seed(42)
rf_model_volume_valence <- randomForest(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel + prop_pos + prop_neg,
  data = train_data,
  ntree = 1500,
  importance = TRUE
)

# Plot OOB MSE vs. Number of Trees
plot(rf_model_volume_valence)  # For regression, this shows MSE vs trees
# we use this plot to check the optimal number of trees 


# Plot OOB MSE vs. Number of Trees
# Save OOB MSE plot for Random Forest model
png("rf_oob_plot_vol_val_highres.png", width = 8, height = 6, units = "in", res = 300)


# Generate the plot with improved labels
plot(
  x = 1:rf_model_volume_valence$ntree,
  y = rf_model_volume_valence$mse,
  type = "l",
  lwd = 2,
  col = "darkblue",
  main = "Random Forest Model (Buzz Volume and Valence)",
  sub = "Out-of-Bag Mean Squared Error by Number of Trees",
  xlab = "Number of Trees",
  ylab = "Out-of-Bag Mean Squared Error (MSE)"
)

# Optional: add gridlines for clarity
grid(col = "gray80")

# Close the file device
dev.off()


# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "variance",  # For regression
  min.node.size = c(1, 5, 10, 20)
)

# 10-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# set seed and train model
set.seed(42)

rf_ranger_model_with_vol_sent <- caret::train(
  log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + prop_pos + prop_neg + is_sequel,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  num.trees = 1500, # because according to the above plot, error was constant after 500 trees 
  importance = "permutation",
  metric = "RMSE"
)

# evaluate the model to check the best parameters 
print(rf_ranger_model_with_vol_sent)
plot(rf_ranger_model_with_vol_sent)

# Plot with clear labels
plot_rf <- ggplot(rf_ranger_model_with_vol_sent$results, 
                  aes(x = mtry, y = RMSE, color = factor(min.node.size))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Random Forest Model Tuning (Buzz Volume and Valence)",
    subtitle = "10-Fold CV RMSE by mtry and Minimum Node Size",
    x = "Number of Predictors (mtry)",
    y = "RMSE (10-Fold CV)",
    color = "Min Node Size"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(hjust = 0.5)
  )


# Save high-res image
ggsave("rf_tuning_vol_val_centered.png", plot_rf, width = 8, height = 6, dpi = 300)

# let's predict on the test set and see how well the model generalizes
predictions_rf_with_sent <- predict(rf_ranger_model_with_vol_sent, newdata = test_data)

# Compute test metrics
actuals <- test_data$log_opening_weekend_eur
rmse_rf_with_sent <- sqrt(mean((predictions_rf_with_sent - actuals)^2))
mae_rf_with_sent <- mean(abs(predictions_rf_with_sent - actuals))
r_squared_with_sent <- 1 - sum((predictions_rf_with_sent - actuals)^2) / sum((actuals - mean(actuals))^2)


cat("Test Set Evaluation Random Forest Buzz Volume and Valence:\n")
cat("RMSE:", round(rmse_rf_with_sent, 4), "\n")
cat("MAE:", round(mae_rf_with_sent, 4), "\n")
cat("R-squared:", round(r_squared_with_sent, 4), "\n")



######################################### ROBUSTNESS CHECK: RUNNING ALL MODELS WITHOUT WITHOUT OPENING LOCATIONS #################################
########## let's use the randomforest package to check the out of bag error as we grow the number of trees
# first identify the optimal number of trees
set.seed(42)
rf_model_no_locs <- randomForest(
  log_opening_weekend_eur ~ run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel,
  data = train_data,
  ntree = 1500,
  importance = TRUE
)


# Generate the plot with improved labels
plot(
  x = 1:rf_model_no_locs$ntree,
  y = rf_model$mse,
  type = "l",
  lwd = 2,
  col = "darkblue",
  main = "Random Forest Model (No Buzz Variables and Opening Locations)",
  sub = "Out-of-Bag Mean Squared Error by Number of Trees",
  xlab = "Number of Trees",
  ylab = "Out-of-Bag Mean Squared Error (MSE)"
)




######## now that we have identified the optimal number of trees as 1500, let's run a random forest model and test it's performance 
########## 1. random forest without buzz 
# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "variance",  # For regression
  min.node.size = c(1, 5, 10, 20)
)

# 10-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# set seed and train model
set.seed(42)

rf_model_no_buzz_no_locs <- caret::train(
  log_opening_weekend_eur ~ run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  num.trees = 1500,
  importance = "permutation",
  metric = "RMSE"
)

# evaluate the model to check the best parameters 
print(rf_model_no_buzz_no_locs)
plot(rf_model_no_buzz_no_locs)

# let's predict on the test set and see how well the model generalizes
predictions_rf_no_buzz_no_locs <- predict(rf_model_no_buzz_no_locs, newdata = test_data)

# Compute test metrics
actuals <- test_data$log_opening_weekend_eur
rmse_rf_no_buzz_no_locs <- sqrt(mean((predictions_rf_no_buzz_no_locs - actuals)^2))
mae_rf_no_buzz_no_locs <- mean(abs(predictions_rf_no_buzz_no_locs - actuals))
r_squared_rf_no_buzz_no_locs <- 1 - sum((predictions_rf_no_buzz_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

cat("Test Set Evaluation RF No Buzz No Opening Locations:\n")
cat("RMSE:", round(rmse_rf_no_buzz_no_locs, 4), "\n")
cat("MAE:", round(mae_rf_no_buzz_no_locs, 4), "\n")
cat("R-squared:", round(r_squared_rf_no_buzz_no_locs, 4), "\n")



############## 2. random forest with buzz volume only!!!
# first identify the optimal number of trees
set.seed(42)
rf_model_volume_only_no_locs <- randomForest(
  log_opening_weekend_eur ~ run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel,
  data = train_data,
  ntree = 1500,
  importance = TRUE
)

# Plot OOB MSE vs. Number of Trees
plot(rf_model_volume_only_no_locs)  # For regression, this shows MSE vs trees
# we use this plot to check the optimal number of trees 

########## 2. random forest with buzz volume only 
# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "variance",  # For regression
  min.node.size = c(1, 5, 10, 20)
)

# 10-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# set seed and train model
set.seed(42)

rf_ranger_model_volume_only_no_locs <- caret::train(
  log_opening_weekend_eur ~ run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  num.trees = 1500, # because according to the above plot, error was constant after 500 trees 
  importance = "permutation",
  metric = "RMSE"
)

# evaluate the model to check the best parameters 
print(rf_ranger_model_volume_only_no_locs)
plot(rf_ranger_model_volume_only_no_locs)


# let's predict on the test set and see how well the model generalizes
predictions_rf_volume_only_no_locs <- predict(rf_ranger_model_volume_only_no_locs, newdata = test_data)

# Compute test metrics
actuals <- test_data$log_opening_weekend_eur
rmse_rf_volume_only_no_locs <- sqrt(mean((predictions_rf_volume_only_no_locs - actuals)^2))
mae_rf_volume_only_no_locs <- mean(abs(predictions_rf_volume_only_no_locs - actuals))
r_squared_rf_volume_only_no_locs <- 1 - sum((predictions_rf_volume_only_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)


cat("Test Set Evaluation Random Forest Volume Only No Opening Locations:\n")
cat("RMSE:", round(rmse_rf_volume_only_no_locs, 4), "\n")
cat("MAE:", round(mae_rf_volume_only_no_locs, 4), "\n")
cat("R-squared:", round(r_squared_rf_volume_only_no_locs, 4), "\n")

########## 3. random forest with buzz volume and valence
# first identify the optimal number of trees
set.seed(42)
rf_model_volume_valence_no_locs <- randomForest(
  log_opening_weekend_eur ~ run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + is_sequel + prop_pos + prop_neg,
  data = train_data,
  ntree = 1500,
  importance = TRUE
)

# Plot OOB MSE vs. Number of Trees
plot(rf_model_volume_valence_no_locs)  # For regression, this shows MSE vs trees
# we use this plot to check the optimal number of trees 

# Define the tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "variance",  # For regression
  min.node.size = c(1, 5, 10, 20)
)

# 10-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# set seed and train model
set.seed(42)

rf_ranger_model_with_vol_sent_no_locs <- caret::train(
  log_opening_weekend_eur ~ run_time + log_star_power_count +
    director_power + distributor_power + is_holiday_release +
    release_month + release_day_name + release_week +
    num_movies_same_week + genre_grouped + MPAA_rating + era_group + log_n_comments + prop_pos + prop_neg + is_sequel,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  num.trees = 1500, # because according to the above plot, error was constant after 500 trees 
  importance = "permutation",
  metric = "RMSE"
)

# evaluate the model to check the best parameters 
print(rf_ranger_model_with_vol_sent_no_locs)
plot(rf_ranger_model_with_vol_sent_no_locs)

# let's predict on the test set and see how well the model generalizes
predictions_rf_with_sent_no_locs <- predict(rf_ranger_model_with_vol_sent_no_locs, newdata = test_data)

# Compute test metrics
actuals <- test_data$log_opening_weekend_eur
rmse_rf_with_sent_no_locs <- sqrt(mean((predictions_rf_with_sent_no_locs - actuals)^2))
mae_rf_with_sent_no_locs <- mean(abs(predictions_rf_with_sent_no_locs - actuals))
r_squared_with_sent_no_locs <- 1 - sum((predictions_rf_with_sent_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)


cat("Test Set Evaluation Random Forest Buzz Volume and Valence No Opening Locations:\n")
cat("RMSE:", round(rmse_rf_with_sent_no_locs, 4), "\n")
cat("MAE:", round(mae_rf_with_sent_no_locs, 4), "\n")
cat("R-squared:", round(r_squared_with_sent_no_locs, 4), "\n")

# variable importance plot 
var_imp <- varImp(rf_ranger_model_with_vol_sent_no_locs, scale = FALSE)
var_imp_df <- data.frame(
  Variable = rownames(var_imp$importance),
  Importance = var_imp$importance$Overall
)

# Get top 10 most important variables
top_vars <- var_imp_df[order(-var_imp_df$Importance), ][1:10, ]

# Plot using ggplot2
ggplot(top_vars, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importances (Random Forest)",
    x = "Variable",
    y = "Permutation Importance"
  ) +
  theme_minimal()



############################### XGBoost ###################################################
# Step 1: Create a copy of the dataset
xgb_model_data <- super_duper_final_meta_data_with_sent

# Step 2: one-hot encode categorical variables because xgb cannot handle categorical variables 
categorical_vars <- c("is_holiday_release", "release_month", "release_day_name", 
                      "genre_grouped", "MPAA_rating", "era_group")

# Create dummyVars model (formula: target ~ predictors)
dummies_model <- dummyVars(
  formula = ~ is_holiday_release + release_month + release_day_name +
    genre_grouped + MPAA_rating + era_group,
  data = xgb_model_data
)


# Apply dummy variable transformation to the dataset
dummies_data <- predict(dummies_model, newdata = xgb_model_data)


# Convert the result to a dataframe
dummies_data <- as.data.frame(dummies_data)

# Combine with numeric features
xgb_model_data <- cbind(
  xgb_model_data[, !(names(xgb_model_data) %in% c("is_holiday_release", "release_month", "release_day_name",
                                          "genre_grouped", "MPAA_rating", "era_group"))],
  dummies_data
)

# check the structure of the dataset 
str(xgb_model_data)

# converting binary factor variables into numeric 
xgb_model_data$director_power <- as.numeric(xgb_model_data$director_power) - 1
xgb_model_data$distributor_power <- as.numeric(xgb_model_data$distributor_power) - 1
xgb_model_data$is_sequel <- as.numeric(xgb_model_data$is_sequel) - 1

################### 1. XGB WITHOUT BUZZ
# let's remove the irrelevant columns and only keep the ones that we will include in our model
irrelevant_cols_no_buzz <- c("release_date","title", "rating", "distributor_name", "us_distributor", "widest_locs",
                             "opening_wknd_local_currency", "opening_week_local_currency", "cume_gross_local_currency",
                             "opening_week_eur", "reported_cume_gross", "international_territory_total", 
                             "international_studio_cume", "domestic_studio_cume", "worldwide_studio_cume", "opening_weekend_adm",
                             "cume_adm", "release_window_number_of_days_vs_domestic", "cast", "director", "producers", 
                             "primary_genre", "non_primary_genre", "sound_formats", "visual_formats", "academy_awards", 
                             "golden_globes", "languages_of_origin", "primary_territories_of_origin", "non_primary_territories_of_origin", 
                             "booking_title_number", "bafta_awards", "title_global_id", "director_clean", "us_distributor_clean", 
                             "year", "us_distributor_final", "release_year", "release_day", "n_comments", "star_power_count", "opening_weekend_eur", "log_n_comments", 
                             "prop_pos", "prop_neg", "prop_neut")  # adjust these as needed

# Drop them
xgb_model_data_no_buzz <- xgb_model_data[, !(names(xgb_model_data) %in% irrelevant_cols_no_buzz)]


# check the structure of the data to see if there are any non-numeric variables
str(xgb_model_data_no_buzz)

# Keep only numeric columns and the target
xgb_model_data_no_buzz <- xgb_model_data_no_buzz %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(log_opening_weekend_eur, everything())  # Ensure target is first


# split the data into train and test sets 
set.seed(42)  # for reproducibility
split_index <- createDataPartition(xgb_model_data_no_buzz$log_opening_weekend_eur, p = 0.8, list = FALSE)

train_data_xgb_no_buzz <- xgb_model_data_no_buzz[split_index, ]
test_data_xgb_no_buzz  <- xgb_model_data_no_buzz[-split_index, ]

# Separate predictors and target
train_matrix_xgb_no_buzz <- xgb.DMatrix(
  data = as.matrix(train_data_xgb_no_buzz[, -1]),
  label = train_data_xgb_no_buzz$log_opening_weekend_eur
)

test_matrix_xgb_no_buzz <- xgb.DMatrix(
  data = as.matrix(test_data_xgb_no_buzz[, -1]),
  label = test_data_xgb_no_buzz$log_opening_weekend_eur
)


################### XGBoost with grid search 
# defining the grid 
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 4, 6)
min_child_weight_vals <- c(1, 5)
gamma_vals <- c(0, 1)
lambda_vals <- c(1, 5)
alpha_vals <- c(0, 1)

# fixed regularization
subsample <- 0.8
colsample_bytree <- 0.8

results_no_buzz <- data.frame()

set.seed(42)
for (eta in eta_vals) {
  for (depth in max_depth_vals) {
    for (child_weight in min_child_weight_vals) {
      for (gamma in gamma_vals) {
        for (lambda in lambda_vals) {
          for (alpha in alpha_vals) {
            
            cat("Running: eta =", eta, 
                ", depth =", depth, 
                ", min_child_weight =", child_weight,
                ", gamma =", gamma,
                ", lambda =", lambda,
                ", alpha =", alpha, "\n")
            
            params <- list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            
            set.seed(42)
            cv_model <- xgb.cv(
              params = params,
              data = train_matrix_xgb_no_buzz,
              nrounds = 1000,
              nfold = 10,
              early_stopping_rounds = 10,
              verbose = 0
            )
            
            results_no_buzz <- rbind(results_no_buzz, data.frame(
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              best_iteration = cv_model$best_iteration,
              best_rmse = min(cv_model$evaluation_log$test_rmse_mean)
            ))
          }
        }
      }
    }
  }
}

results_no_buzz <- results_no_buzz[order(results_no_buzz$best_rmse), ]
print(results_no_buzz)

# let's train a model on the best hyperparameters, so the ones with the lowest test rmse and then check performance on the holdout test set 
# Define the best parameters
best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 4,
  min_child_weight = 5,
  gamma = 0,
  lambda = 5,
  alpha = 0,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Best number of boosting rounds from CV
best_nrounds <- 914

# train the model
xgb_final_no_buzz <- xgb.train(
  params = best_params,
  data = train_matrix_xgb_no_buzz,
  nrounds = best_nrounds,
  watchlist = list(train = train_matrix_xgb_no_buzz, test = test_matrix_xgb_no_buzz),
  verbose = 1
)


# Predictions
preds_xgb_final_no_buzz <- predict(xgb_final_no_buzz, newdata = test_matrix_xgb_no_buzz)

# Actual values
actuals <- getinfo(test_matrix_xgb_no_buzz, "label")

# Evaluation metrics
rmse_xgb_final_no_buzz <- sqrt(mean((preds_xgb_final_no_buzz - actuals)^2))
mae_xgb_final_no_buzz <- mean(abs(preds_xgb_final_no_buzz - actuals))
r2_xgb_final_no_buzz <- 1 - sum((preds_xgb_final_no_buzz - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print results
cat("🔍 Final XGBoost No Buzz Test Set Evaluation:\n")
cat("RMSE:", round(rmse_xgb_final_no_buzz, 4), "\n")
cat("MAE :", round(mae_xgb_final_no_buzz, 4), "\n")
cat("R²  :", round(r2_xgb_final_no_buzz, 4), "\n")



################################ OPENING THE "BLACK BOX" OF BEST MODEL: XGBOOST WITHOUT BUZZ VOLUME OR VALENCE ################################
####### variable importance plot
# Get importance matrix
importance_matrix <- xgb.importance(model = xgb_final_no_buzz)

# Keep only top 10
top_features <- importance_matrix[1:10, ]

# Plot without clustering
ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Features in XGBoost Model (No Buzz)",
    x = "Feature",
    y = "Relative Gain (Importance)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )


######### permutation feature importance 
# Extract matrix of test predictors and original labels
test_data_matrix <- as.matrix(test_data_xgb_no_buzz[, -1])  # drop target
true_labels <- test_data_xgb_no_buzz$log_opening_weekend_eur

# Create baseline predictions and baseline RMSE
baseline_preds <- predict(xgb_final_no_buzz, newdata = test_data_matrix)
baseline_rmse <- sqrt(mean((baseline_preds - true_labels)^2))

# Initialize results
perm_importance <- data.frame(Feature = character(), RMSE_Increase = numeric(), stringsAsFactors = FALSE)

# Loop over each column (feature)
for (feature_name in colnames(test_data_matrix)) {
  cat("Permuting feature:", feature_name, "\n")
  
  # Make a copy of test data
  permuted_matrix <- test_data_matrix
  
  # Permute current feature
  permuted_matrix[, feature_name] <- sample(permuted_matrix[, feature_name])
  
  # Predict using permuted data
  perm_preds <- predict(xgb_final_no_buzz, newdata = permuted_matrix)
  
  # Compute RMSE with permuted feature
  perm_rmse <- sqrt(mean((perm_preds - true_labels)^2))
  
  # Calculate RMSE increase
  rmse_increase <- perm_rmse - baseline_rmse
  
  # Store
  perm_importance <- rbind(
    perm_importance,
    data.frame(Feature = feature_name, RMSE_Increase = rmse_increase)
  )
}

# Sort by RMSE increase
perm_importance <- perm_importance[order(-perm_importance$RMSE_Increase), ]

# Plot only Top 10
top_n <- 10
ggplot(perm_importance[1:top_n, ], aes(x = reorder(Feature, RMSE_Increase), y = RMSE_Increase)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste("Top 10 Most Important Features in XGBoost Model (Permutation Importance - No Buzz)"),
    x = "Feature",
    y = "Increase in RMSE when Permuted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )


#### aligning and saving permutation feauture importance and variable importance plots 
# --- Plot 1: Gain-based Variable Importance ---
p1 <- ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "XGBoost Variable Importance (Gain)",
    x = "Feature",
    y = "Relative Gain"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )

# --- Plot 2: Permutation Feature Importance ---
top_n <- 10
p2 <- ggplot(perm_importance[1:top_n, ], aes(x = reorder(Feature, RMSE_Increase), y = RMSE_Increase)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Permutation Feature Importance",
    x = "Feature",
    y = "RMSE Increase When Permuted"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )

# Combine plots side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# Save to file
ggsave("XGBoost_Importance_Comparison.png", combined_plot, width = 14, height = 6, dpi = 300)





###### ALE Plots 
# let's first save the feature names that the model used 
xgb_model_features <- xgb_final_no_buzz$feature_names

# This MUST match the model input structure
xgb_ale_data_matrix <- model.matrix(~ . -1, data = train_data_xgb_no_buzz[, -1])  # Remove target

# Convert to data.frame for iml
xgb_ale_data <- as.data.frame(xgb_ale_data_matrix)

# Add any missing columns and reorder
missing_cols <- setdiff(xgb_model_features, colnames(xgb_ale_data))
for (col in missing_cols) {
  xgb_ale_data[[col]] <- 0  # fill with zeros (safe because model never saw these values in this context)
}

# Reorder columns to match model
xgb_ale_data <- xgb_ale_data[, xgb_model_features]

xgb_predict_function <- function(model, newdata) {
  # Ensure it's a data.frame
  newdata <- as.data.frame(newdata)
  
  # Add missing columns
  missing <- setdiff(xgb_model_features, names(newdata))
  for (col in missing) {
    newdata[[col]] <- 0
  }
  
  # Drop extra columns if needed (optional safety check)
  newdata <- newdata[, intersect(xgb_model_features, names(newdata)), drop = FALSE]
  
  # Reorder
  newdata <- newdata[, xgb_model_features, drop = FALSE]
  
  # Now convert to matrix safely
  newdata_matrix <- data.matrix(newdata)  # Better than model.matrix in this case
  
  predict(model, newdata = newdata_matrix)
}

predictor_xgb <- Predictor$new(
  model = xgb_final_no_buzz,
  data = xgb_ale_data,
  y = train_data_xgb_no_buzz$log_opening_weekend_eur,
  predict.function = xgb_predict_function 
)


# 1. ALE Opening Locations  
ale_plot_opening_locs <- FeatureEffect$new(
  predictor = predictor_xgb,
  feature = "opening_locs",
  method = "ale"
)


# Generate and customize ALE plot
# Generate the ggplot object from the iml FeatureEffect object
p <- plot(ale_plot_opening_locs)  # 👈 safest and clearest call

# Redraw the plot with your custom labels
p + 
  labs(
    title = "Accumulated Local Effects (ALE) for Opening Locations",
    x = "Number of Opening Locations",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


# 2. ALE for release_week 
# Generate the ALE object
ale_plot_release_week <- FeatureEffect$new(
  predictor = predictor_xgb,
  feature = "release_week",
  method = "ale"
)

# Extract and customize the ggplot
p_week <- plot(ale_plot_release_week)

# Add custom labels and styling
p_week + 
  labs(
    title = "ALE Plot for Release Week",
    x = "Release Week of the Year",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )


# 3. ALE run_time
# Generate ALE object
ale_plot_run_time <- FeatureEffect$new(
  predictor = predictor_xgb,
  feature = "run_time",
  method = "ale"
)

# Customize and plot
plot(ale_plot_run_time) + 
  labs(
    title = "ALE Plot for Length of Movies",
    x = "Movie Run Time in Minutes",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )



# 4. ALE Star Power Count
ale_plot_star_power <- FeatureEffect$new(
  predictor = predictor_xgb,
  feature = "log_star_power_count",
  method = "ale"
)

# Extract and customize the ggplot
p_star <- plot(ale_plot_star_power)

# Override labels and apply clean theme
p_star + 
  labs(
    title = "ALE Plot for Star Power (log scale)",
    x = "log(Number of Top-Grossing Stars)",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )





# 5. ALE num_movies_same_week
# Generate ALE object
ale_plot_competition <- FeatureEffect$new(
  predictor = predictor_xgb,
  feature = "num_movies_same_week",
  method = "ale"
)

# Customize and plot
plot(ale_plot_competition) + 
  labs(
    title = "ALE Plot for Competition During Release",
    x = "Number of Movies Released in the Same Week",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

###### arranging these ALE plots nicely for the thesis 
p1 <- plot(ale_plot_opening_locs) +
  labs(
    title = "ALE: Opening Locations",
    x = "Number of Opening Locations",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

p2 <- plot(ale_plot_release_week) +
  labs(
    title = "ALE: Release Week",
    x = "Week of the Year",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

p3 <- plot(ale_plot_run_time) +
  labs(
    title = "ALE: Movie Run Time",
    x = "Run Time (minutes)",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

p4 <- plot(ale_plot_star_power) +
  labs(
    title = "ALE: Star Power (log scale)",
    x = "log(Number of Top Stars)",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

p5 <- plot(ale_plot_competition) +
  labs(
    title = "ALE: Same-Week Competition",
    x = "Other Movies Releasing Same Week",
    y = "Effect on log(Opening Weekend Revenue)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"))

# Combine into a 3-column layout with 5 plots
combined_plot <- (p1 | p2 | p3) / (p4 | p5 | plot_spacer())

# Save the combined ALE plot
ggsave("ALE_Plots_XGBoost_5Features.png", combined_plot, width = 16, height = 10, dpi = 300)






################### 2. XGB WITH VOLUME ONLY
# let's remove the irrelevant columns and only keep the ones that we will include in our model
irrelevant_cols_vol_only <- c("release_date","title", "rating", "distributor_name", "us_distributor", "widest_locs",
                             "opening_wknd_local_currency", "opening_week_local_currency", "cume_gross_local_currency",
                             "opening_week_eur", "reported_cume_gross", "international_territory_total", 
                             "international_studio_cume", "domestic_studio_cume", "worldwide_studio_cume", "opening_weekend_adm",
                             "cume_adm", "release_window_number_of_days_vs_domestic", "cast", "director", "producers", 
                             "primary_genre", "non_primary_genre", "sound_formats", "visual_formats", "academy_awards", 
                             "golden_globes", "languages_of_origin", "primary_territories_of_origin", "non_primary_territories_of_origin", 
                             "booking_title_number", "bafta_awards", "title_global_id", "director_clean", "us_distributor_clean", 
                             "year", "us_distributor_final", "release_year", "release_day", "n_comments", "star_power_count", "opening_weekend_eur", 
                             "prop_pos", "prop_neg", "prop_neut")  # adjust these as needed

# Drop them
xgb_model_data_vol_only <- xgb_model_data[, !(names(xgb_model_data) %in% irrelevant_cols_vol_only)]


# check the structure of the data to see if there are any non-numeric variables
str(xgb_model_data_vol_only)

# Keep only numeric columns and the target
xgb_model_data_vol_only <- xgb_model_data_vol_only %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(log_opening_weekend_eur, everything())  # Ensure target is first


# split the data into train and test sets 
set.seed(42)  # for reproducibility
split_index <- createDataPartition(xgb_model_data_vol_only$log_opening_weekend_eur, p = 0.8, list = FALSE)

train_data_xgb_vol_only <- xgb_model_data_vol_only[split_index, ]
test_data_xgb_vol_only  <- xgb_model_data_vol_only[-split_index, ]

# Separate predictors and target
train_matrix_xgb_vol_only <- xgb.DMatrix(
  data = as.matrix(train_data_xgb_vol_only[, -1]),
  label = train_data_xgb_vol_only$log_opening_weekend_eur
)

test_matrix_xgb_vol_only <- xgb.DMatrix(
  data = as.matrix(test_data_xgb_vol_only[, -1]),
  label = test_data_xgb_vol_only$log_opening_weekend_eur
)


################### XGBoost with grid search 
# defining the grid 
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 4, 6)
min_child_weight_vals <- c(1, 5)
gamma_vals <- c(0, 1)
lambda_vals <- c(1, 5)
alpha_vals <- c(0, 1)

# fixed regularization
subsample <- 0.8
colsample_bytree <- 0.8

results_vol_only <- data.frame()

set.seed(42)
for (eta in eta_vals) {
  for (depth in max_depth_vals) {
    for (child_weight in min_child_weight_vals) {
      for (gamma in gamma_vals) {
        for (lambda in lambda_vals) {
          for (alpha in alpha_vals) {
            
            cat("Running: eta =", eta, 
                ", depth =", depth, 
                ", min_child_weight =", child_weight,
                ", gamma =", gamma,
                ", lambda =", lambda,
                ", alpha =", alpha, "\n")
            
            params <- list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            
            set.seed(42)
            cv_model <- xgb.cv(
              params = params,
              data = train_matrix_xgb_vol_only,
              nrounds = 1000,
              nfold = 10,
              early_stopping_rounds = 10,
              verbose = 0
            )
            
            results_vol_only <- rbind(results_vol_only, data.frame(
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              best_iteration = cv_model$best_iteration,
              best_rmse = min(cv_model$evaluation_log$test_rmse_mean)
            ))
          }
        }
      }
    }
  }
}

results_vol_only <- results_vol_only[order(results_vol_only$best_rmse), ]
print(results_vol_only)

# let's train a model on the best hyperparameters, so the ones with the lowest test rmse and then check performance on the holdout test set 
# Define the best parameters
best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.01,
  max_depth = 4,
  min_child_weight = 5,
  gamma = 0,
  lambda = 5,
  alpha = 0,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Best number of boosting rounds from CV
best_nrounds <- 806

# train the model
xgb_final_vol_only <- xgb.train(
  params = best_params,
  data = train_matrix_xgb_vol_only,
  nrounds = best_nrounds,
  watchlist = list(train = train_matrix_xgb_vol_only, test = test_matrix_xgb_vol_only),
  verbose = 1
)


# Predictions
preds_xgb_final_vol_only <- predict(xgb_final_vol_only, newdata = test_matrix_xgb_vol_only)

# Actual values
actuals <- getinfo(test_matrix_xgb_vol_only, "label")

# Evaluation metrics
rmse_xgb_final_vol_only <- sqrt(mean((preds_xgb_final_vol_only - actuals)^2))
mae_xgb_final_vol_only <- mean(abs(preds_xgb_final_vol_only - actuals))
r2_xgb_final_vol_only <- 1 - sum((preds_xgb_final_vol_only - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print results
cat("🔍 Final XGBoost Volume Only Test Set Evaluation:\n")
cat("RMSE:", round(rmse_xgb_final_vol_only, 4), "\n")
cat("MAE :", round(mae_xgb_final_vol_only, 4), "\n")
cat("R²  :", round(r2_xgb_final_vol_only, 4), "\n")




################### 3. XGB WITH BUZZ VOLUME AND VALENCE
# let's remove the irrelevant columns and only keep the ones that we will include in our model
irrelevant_cols_vol_val <- c("release_date","title", "rating", "distributor_name", "us_distributor", "widest_locs",
                     "opening_wknd_local_currency", "opening_week_local_currency", "cume_gross_local_currency",
                     "opening_week_eur", "reported_cume_gross", "international_territory_total", 
                     "international_studio_cume", "domestic_studio_cume", "worldwide_studio_cume", "opening_weekend_adm",
                     "cume_adm", "release_window_number_of_days_vs_domestic", "cast", "director", "producers", 
                     "primary_genre", "non_primary_genre", "sound_formats", "visual_formats", "academy_awards", 
                     "golden_globes", "languages_of_origin", "primary_territories_of_origin", "non_primary_territories_of_origin", 
                     "booking_title_number", "bafta_awards", "title_global_id", "director_clean", "us_distributor_clean", 
                     "year", "us_distributor_final", "release_year", "release_day", "n_comments", "star_power_count", "opening_weekend_eur", "prop_neut")  # adjust these as needed

# Drop them
xgb_model_data_vol_val <- xgb_model_data[, !(names(xgb_model_data) %in% irrelevant_cols_vol_val)]

# Keep only numeric columns and the target
xgb_model_data_vol_val <- xgb_model_data_vol_val %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(log_opening_weekend_eur, everything())  # Ensure target is first

# check the structure of the dataset to ensure it has all the required variables 
str(xgb_model_data_vol_val)


# split the data into train and test sets 
set.seed(42)  # for reproducibility
split_index <- createDataPartition(xgb_model_data_vol_val$log_opening_weekend_eur, p = 0.8, list = FALSE)

train_data_xgb_vol_val <- xgb_model_data_vol_val[split_index, ]
test_data_xgb_vol_val  <- xgb_model_data_vol_val[-split_index, ]

# Separate predictors and target
train_matrix_xgb_vol_val <- xgb.DMatrix(
  data = as.matrix(train_data_xgb_vol_val[, -1]),
  label = train_data_xgb_vol_val$log_opening_weekend_eur
)

test_matrix_xgb_vol_val <- xgb.DMatrix(
  data = as.matrix(test_data_xgb_vol_val[, -1]),
  label = test_data_xgb_vol_val$log_opening_weekend_eur
)

################### XGBoost with grid search 
# defining the grid 
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 4, 6)
min_child_weight_vals <- c(1, 5)
gamma_vals <- c(0, 1)
lambda_vals <- c(1, 5)
alpha_vals <- c(0, 1)

# fixed regularization
subsample <- 0.8
colsample_bytree <- 0.8

results_vol_val <- data.frame()

set.seed(42)
for (eta in eta_vals) {
  for (depth in max_depth_vals) {
    for (child_weight in min_child_weight_vals) {
      for (gamma in gamma_vals) {
        for (lambda in lambda_vals) {
          for (alpha in alpha_vals) {
            
            cat("Running: eta =", eta, 
                ", depth =", depth, 
                ", min_child_weight =", child_weight,
                ", gamma =", gamma,
                ", lambda =", lambda,
                ", alpha =", alpha, "\n")
            
            params <- list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            
            set.seed(42)
            cv_model <- xgb.cv(
              params = params,
              data = train_matrix_xgb_vol_val,
              nrounds = 1000,
              nfold = 10,
              early_stopping_rounds = 10,
              verbose = 0
            )
            
            results_vol_val <- rbind(results_vol_val, data.frame(
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              best_iteration = cv_model$best_iteration,
              best_rmse = min(cv_model$evaluation_log$test_rmse_mean)
            ))
          }
        }
      }
    }
  }
}

results_vol_val <- results_vol_val[order(results_vol_val$best_rmse), ]
print(results_vol_val)

# let's train a model on the best hyperparameters, so the ones with the lowest test rmse and then check performance on the holdout test set 
# Define the best parameters
best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.10,
  max_depth = 6,
  min_child_weight = 5,
  gamma = 1,
  lambda = 5,
  alpha = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Best number of boosting rounds from CV
best_nrounds <- 110

# train the model
xgb_final_vol_val <- xgb.train(
  params = best_params,
  data = train_matrix_xgb_vol_val,
  nrounds = best_nrounds,
  watchlist = list(train = train_matrix_xgb_vol_val, test = test_matrix_xgb_vol_val),
  verbose = 1
)


# Predictions
preds_xgb_final_vol_val <- predict(xgb_final_vol_val, newdata = test_matrix_xgb_vol_val)

# Actual values
actuals <- getinfo(test_matrix_xgb_vol_val, "label")

# Evaluation metrics
rmse_xgb_final_vol_val <- sqrt(mean((preds_xgb_final_vol_val - actuals)^2))
mae_xgb_final_vol_val <- mean(abs(preds_xgb_final_vol_val - actuals))
r2_xgb_final_vol_val <- 1 - sum((preds_xgb_final_vol_val - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print results
cat("🔍 Final XGBoost Volume and Valence Model Test Set Evaluation:\n")
cat("RMSE:", round(rmse_xgb_final_vol_val, 4), "\n")
cat("MAE :", round(mae_xgb_final_vol_val, 4), "\n")
cat("R²  :", round(r2_xgb_final_vol_val, 4), "\n")




############################# ROBUSTNESS CHECK: NO OPENING LOCATIONS ##########################################
################### 1. XGB WITHOUT BUZZ
# let's remove the irrelevant columns and only keep the ones that we will include in our model
irrelevant_cols_no_buzz_no_locs <- c("release_date","title", "rating", "distributor_name", "us_distributor", "widest_locs",
                             "opening_wknd_local_currency", "opening_week_local_currency", "cume_gross_local_currency",
                             "opening_week_eur", "reported_cume_gross", "international_territory_total", 
                             "international_studio_cume", "domestic_studio_cume", "worldwide_studio_cume", "opening_weekend_adm",
                             "cume_adm", "release_window_number_of_days_vs_domestic", "cast", "director", "producers", 
                             "primary_genre", "non_primary_genre", "sound_formats", "visual_formats", "academy_awards", 
                             "golden_globes", "languages_of_origin", "primary_territories_of_origin", "non_primary_territories_of_origin", 
                             "booking_title_number", "bafta_awards", "title_global_id", "director_clean", "us_distributor_clean", 
                             "year", "us_distributor_final", "release_year", "release_day", "n_comments", "star_power_count", "opening_weekend_eur", "log_n_comments", 
                             "prop_pos", "prop_neg", "prop_neut", "opening_locs")  # adjust these as needed

# Drop them
xgb_model_data_no_buzz_no_locs <- xgb_model_data[, !(names(xgb_model_data) %in% irrelevant_cols_no_buzz_no_locs)]


# check the structure of the data to see if there are any non-numeric variables
str(xgb_model_data_no_buzz_no_locs)

# Keep only numeric columns and the target
xgb_model_data_no_buzz_no_locs <- xgb_model_data_no_buzz_no_locs %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(log_opening_weekend_eur, everything())  # Ensure target is first


# split the data into train and test sets 
set.seed(42)  # for reproducibility
split_index <- createDataPartition(xgb_model_data_no_buzz_no_locs$log_opening_weekend_eur, p = 0.8, list = FALSE)

train_data_xgb_no_buzz_no_locs <- xgb_model_data_no_buzz_no_locs[split_index, ]
test_data_xgb_no_buzz_no_locs  <- xgb_model_data_no_buzz_no_locs[-split_index, ]

# Separate predictors and target
train_matrix_xgb_no_buzz_no_locs <- xgb.DMatrix(
  data = as.matrix(train_data_xgb_no_buzz_no_locs[, -1]),
  label = train_data_xgb_no_buzz_no_locs$log_opening_weekend_eur
)

test_matrix_xgb_no_buzz_no_locs <- xgb.DMatrix(
  data = as.matrix(test_data_xgb_no_buzz_no_locs[, -1]),
  label = test_data_xgb_no_buzz_no_locs$log_opening_weekend_eur
)


################### XGBoost with grid search 
# defining the grid 
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 4, 6)
min_child_weight_vals <- c(1, 5)
gamma_vals <- c(0, 1)
lambda_vals <- c(1, 5)
alpha_vals <- c(0, 1)

# fixed regularization
subsample <- 0.8
colsample_bytree <- 0.8

results_no_buzz_no_locs <- data.frame()

set.seed(42)
for (eta in eta_vals) {
  for (depth in max_depth_vals) {
    for (child_weight in min_child_weight_vals) {
      for (gamma in gamma_vals) {
        for (lambda in lambda_vals) {
          for (alpha in alpha_vals) {
            
            cat("Running: eta =", eta, 
                ", depth =", depth, 
                ", min_child_weight =", child_weight,
                ", gamma =", gamma,
                ", lambda =", lambda,
                ", alpha =", alpha, "\n")
            
            params <- list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            
            set.seed(42)
            cv_model <- xgb.cv(
              params = params,
              data = train_matrix_xgb_no_buzz_no_locs,
              nrounds = 1000,
              nfold = 10,
              early_stopping_rounds = 10,
              verbose = 0
            )
            
            results_no_buzz_no_locs <- rbind(results_no_buzz_no_locs, data.frame(
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              best_iteration = cv_model$best_iteration,
              best_rmse = min(cv_model$evaluation_log$test_rmse_mean)
            ))
          }
        }
      }
    }
  }
}

results_no_buzz_no_locs <- results_no_buzz_no_locs[order(results_no_buzz_no_locs$best_rmse), ]
print(results_no_buzz_no_locs)

# let's train a model on the best hyperparameters, so the ones with the lowest test rmse and then check performance on the holdout test set 
# Define the best parameters
best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.10,
  max_depth = 3,
  min_child_weight = 5,
  gamma = 1,
  lambda = 1,
  alpha = 0,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Best number of boosting rounds from CV
best_nrounds <- 84

# train the model
xgb_final_no_buzz_no_locs <- xgb.train(
  params = best_params,
  data = train_matrix_xgb_no_buzz_no_locs,
  nrounds = best_nrounds,
  watchlist = list(train = train_matrix_xgb_no_buzz_no_locs, test = test_matrix_xgb_no_buzz_no_locs),
  verbose = 1
)


# Predictions
preds_xgb_final_no_buzz_no_locs <- predict(xgb_final_no_buzz_no_locs, newdata = test_matrix_xgb_no_buzz_no_locs)

# Actual values
actuals <- getinfo(test_matrix_xgb_no_buzz_no_locs, "label")

# Evaluation metrics
rmse_xgb_final_no_buzz_no_locs <- sqrt(mean((preds_xgb_final_no_buzz_no_locs - actuals)^2))
mae_xgb_final_no_buzz_no_locs <- mean(abs(preds_xgb_final_no_buzz_no_locs - actuals))
r2_xgb_final_no_buzz_no_locs <- 1 - sum((preds_xgb_final_no_buzz_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print results
cat("🔍 Final XGBoost No Buzz Test Set Evaluation No Opening Locations:\n")
cat("RMSE:", round(rmse_xgb_final_no_buzz_no_locs, 4), "\n")
cat("MAE :", round(mae_xgb_final_no_buzz_no_locs, 4), "\n")
cat("R²  :", round(r2_xgb_final_no_buzz_no_locs, 4), "\n")


####### variable importance plot
# Get importance matrix
importance_matrix <- xgb.importance(model = xgb_final_no_buzz_no_locs)

# Keep only top 10
top_features <- importance_matrix[1:10, ]

# Plot without clustering
ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Features in XGBoost Model (No Buzz and No Opening Locations)",
    x = "Feature",
    y = "Relative Gain (Importance)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )


################### 2. XGB WITH VOLUME ONLY
# let's remove the irrelevant columns and only keep the ones that we will include in our model
irrelevant_cols_vol_only_no_locs <- c("release_date","title", "rating", "distributor_name", "us_distributor", "widest_locs",
                              "opening_wknd_local_currency", "opening_week_local_currency", "cume_gross_local_currency",
                              "opening_week_eur", "reported_cume_gross", "international_territory_total", 
                              "international_studio_cume", "domestic_studio_cume", "worldwide_studio_cume", "opening_weekend_adm",
                              "cume_adm", "release_window_number_of_days_vs_domestic", "cast", "director", "producers", 
                              "primary_genre", "non_primary_genre", "sound_formats", "visual_formats", "academy_awards", 
                              "golden_globes", "languages_of_origin", "primary_territories_of_origin", "non_primary_territories_of_origin", 
                              "booking_title_number", "bafta_awards", "title_global_id", "director_clean", "us_distributor_clean", 
                              "year", "us_distributor_final", "release_year", "release_day", "n_comments", "star_power_count", "opening_weekend_eur", 
                              "prop_pos", "prop_neg", "prop_neut", "opening_locs")  # adjust these as needed

# Drop them
xgb_model_data_vol_only_no_locs <- xgb_model_data[, !(names(xgb_model_data) %in% irrelevant_cols_vol_only_no_locs)]


# check the structure of the data to see if there are any non-numeric variables
str(xgb_model_data_vol_only_no_locs)

# Keep only numeric columns and the target
xgb_model_data_vol_only_no_locs <- xgb_model_data_vol_only_no_locs %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(log_opening_weekend_eur, everything())  # Ensure target is first


# split the data into train and test sets 
set.seed(42)  # for reproducibility
split_index <- createDataPartition(xgb_model_data_vol_only_no_locs$log_opening_weekend_eur, p = 0.8, list = FALSE)

train_data_xgb_vol_only_no_locs <- xgb_model_data_vol_only_no_locs[split_index, ]
test_data_xgb_vol_only_no_locs  <- xgb_model_data_vol_only_no_locs[-split_index, ]

# Separate predictors and target
train_matrix_xgb_vol_only_no_locs <- xgb.DMatrix(
  data = as.matrix(train_data_xgb_vol_only_no_locs[, -1]),
  label = train_data_xgb_vol_only_no_locs$log_opening_weekend_eur
)

test_matrix_xgb_vol_only_no_locs <- xgb.DMatrix(
  data = as.matrix(test_data_xgb_vol_only_no_locs[, -1]),
  label = test_data_xgb_vol_only_no_locs$log_opening_weekend_eur
)


################### XGBoost with grid search 
# defining the grid 
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 4, 6)
min_child_weight_vals <- c(1, 5)
gamma_vals <- c(0, 1)
lambda_vals <- c(1, 5)
alpha_vals <- c(0, 1)

# fixed regularization
subsample <- 0.8
colsample_bytree <- 0.8

results_vol_only_no_locs <- data.frame()

set.seed(42)
for (eta in eta_vals) {
  for (depth in max_depth_vals) {
    for (child_weight in min_child_weight_vals) {
      for (gamma in gamma_vals) {
        for (lambda in lambda_vals) {
          for (alpha in alpha_vals) {
            
            cat("Running: eta =", eta, 
                ", depth =", depth, 
                ", min_child_weight =", child_weight,
                ", gamma =", gamma,
                ", lambda =", lambda,
                ", alpha =", alpha, "\n")
            
            params <- list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            
            set.seed(42)
            cv_model <- xgb.cv(
              params = params,
              data = train_matrix_xgb_vol_only_no_locs,
              nrounds = 1000,
              nfold = 10,
              early_stopping_rounds = 10,
              verbose = 0
            )
            
            results_vol_only_no_locs <- rbind(results_vol_only_no_locs, data.frame(
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              best_iteration = cv_model$best_iteration,
              best_rmse = min(cv_model$evaluation_log$test_rmse_mean)
            ))
          }
        }
      }
    }
  }
}

results_vol_only_no_locs <- results_vol_only_no_locs[order(results_vol_only_no_locs$best_rmse), ]
print(results_vol_only_no_locs)

# let's train a model on the best hyperparameters, so the ones with the lowest test rmse and then check performance on the holdout test set 
# Define the best parameters
best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 3,
  min_child_weight = 5,
  gamma = 1,
  lambda = 5,
  alpha = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Best number of boosting rounds from CV
best_nrounds <- 143

# train the model
xgb_final_vol_only_no_locs <- xgb.train(
  params = best_params,
  data = train_matrix_xgb_vol_only_no_locs,
  nrounds = best_nrounds,
  watchlist = list(train = train_matrix_xgb_vol_only_no_locs, test = test_matrix_xgb_vol_only_no_locs),
  verbose = 1
)


# Predictions
preds_xgb_final_vol_only_no_locs <- predict(xgb_final_vol_only_no_locs, newdata = test_matrix_xgb_vol_only_no_locs)

# Actual values
actuals <- getinfo(test_matrix_xgb_vol_only_no_locs, "label")

# Evaluation metrics
rmse_xgb_final_vol_only_no_locs <- sqrt(mean((preds_xgb_final_vol_only_no_locs - actuals)^2))
mae_xgb_final_vol_only_no_locs <- mean(abs(preds_xgb_final_vol_only_no_locs - actuals))
r2_xgb_final_vol_only_no_locs <- 1 - sum((preds_xgb_final_vol_only_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print results
cat("🔍 Final XGBoost Volume Only Test Set Evaluation No Opening Locations:\n")
cat("RMSE:", round(rmse_xgb_final_vol_only_no_locs, 4), "\n")
cat("MAE :", round(mae_xgb_final_vol_only_no_locs, 4), "\n")
cat("R²  :", round(r2_xgb_final_vol_only_no_locs, 4), "\n")




################### 3. XGB WITH BUZZ VOLUME AND VALENCE
# let's remove the irrelevant columns and only keep the ones that we will include in our model
irrelevant_cols_vol_val_no_locs <- c("release_date","title", "rating", "distributor_name", "us_distributor", "widest_locs",
                             "opening_wknd_local_currency", "opening_week_local_currency", "cume_gross_local_currency",
                             "opening_week_eur", "reported_cume_gross", "international_territory_total", 
                             "international_studio_cume", "domestic_studio_cume", "worldwide_studio_cume", "opening_weekend_adm",
                             "cume_adm", "release_window_number_of_days_vs_domestic", "cast", "director", "producers", 
                             "primary_genre", "non_primary_genre", "sound_formats", "visual_formats", "academy_awards", 
                             "golden_globes", "languages_of_origin", "primary_territories_of_origin", "non_primary_territories_of_origin", 
                             "booking_title_number", "bafta_awards", "title_global_id", "director_clean", "us_distributor_clean", 
                             "year", "us_distributor_final", "release_year", "release_day", "n_comments", "star_power_count", "opening_weekend_eur", "prop_neut", "opening_locs")  # adjust these as needed

# Drop them
xgb_model_data_vol_val_no_locs <- xgb_model_data[, !(names(xgb_model_data) %in% irrelevant_cols_vol_val_no_locs)]

# Keep only numeric columns and the target
xgb_model_data_vol_val_no_locs <- xgb_model_data_vol_val_no_locs %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(log_opening_weekend_eur, everything())  # Ensure target is first

# check the structure of the dataset to ensure it has all the required variables 
str(xgb_model_data_vol_val_no_locs)


# split the data into train and test sets 
set.seed(42)  # for reproducibility
split_index <- createDataPartition(xgb_model_data_vol_val_no_locs$log_opening_weekend_eur, p = 0.8, list = FALSE)

train_data_xgb_vol_val_no_locs <- xgb_model_data_vol_val_no_locs[split_index, ]
test_data_xgb_vol_val_no_locs  <- xgb_model_data_vol_val_no_locs[-split_index, ]

# Separate predictors and target
train_matrix_xgb_vol_val_no_locs <- xgb.DMatrix(
  data = as.matrix(train_data_xgb_vol_val_no_locs[, -1]),
  label = train_data_xgb_vol_val_no_locs$log_opening_weekend_eur
)

test_matrix_xgb_vol_val_no_locs <- xgb.DMatrix(
  data = as.matrix(test_data_xgb_vol_val_no_locs[, -1]),
  label = test_data_xgb_vol_val_no_locs$log_opening_weekend_eur
)


################### XGBoost with grid search 
# defining the grid 
eta_vals <- c(0.01, 0.05, 0.1)
max_depth_vals <- c(3, 4, 6)
min_child_weight_vals <- c(1, 5)
gamma_vals <- c(0, 1)
lambda_vals <- c(1, 5)
alpha_vals <- c(0, 1)

# fixed regularization
subsample <- 0.8
colsample_bytree <- 0.8

results_vol_val_no_locs <- data.frame()

set.seed(42)
for (eta in eta_vals) {
  for (depth in max_depth_vals) {
    for (child_weight in min_child_weight_vals) {
      for (gamma in gamma_vals) {
        for (lambda in lambda_vals) {
          for (alpha in alpha_vals) {
            
            cat("Running: eta =", eta, 
                ", depth =", depth, 
                ", min_child_weight =", child_weight,
                ", gamma =", gamma,
                ", lambda =", lambda,
                ", alpha =", alpha, "\n")
            
            params <- list(
              objective = "reg:squarederror",
              eval_metric = "rmse",
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              subsample = subsample,
              colsample_bytree = colsample_bytree
            )
            
            set.seed(42)
            cv_model <- xgb.cv(
              params = params,
              data = train_matrix_xgb_vol_val_no_locs,
              nrounds = 1000,
              nfold = 10,
              early_stopping_rounds = 10,
              verbose = 0
            )
            
            results_vol_val_no_locs <- rbind(results_vol_val_no_locs, data.frame(
              eta = eta,
              max_depth = depth,
              min_child_weight = child_weight,
              gamma = gamma,
              lambda = lambda,
              alpha = alpha,
              best_iteration = cv_model$best_iteration,
              best_rmse = min(cv_model$evaluation_log$test_rmse_mean)
            ))
          }
        }
      }
    }
  }
}

results_vol_val_no_locs <- results_vol_val_no_locs[order(results_vol_val_no_locs$best_rmse), ]
print(results_vol_val_no_locs)

# let's train a model on the best hyperparameters, so the ones with the lowest test rmse and then check performance on the holdout test set 
# Define the best parameters
best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 3,
  min_child_weight = 5,
  gamma = 1,
  lambda = 5,
  alpha = 0,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Best number of boosting rounds from CV
best_nrounds <- 144

# train the model
xgb_final_vol_val_no_locs <- xgb.train(
  params = best_params,
  data = train_matrix_xgb_vol_val_no_locs,
  nrounds = best_nrounds,
  watchlist = list(train = train_matrix_xgb_vol_val_no_locs, test = test_matrix_xgb_vol_val_no_locs),
  verbose = 1
)


# Predictions
preds_xgb_final_vol_val_no_locs <- predict(xgb_final_vol_val_no_locs, newdata = test_matrix_xgb_vol_val_no_locs)

# Actual values
actuals <- getinfo(test_matrix_xgb_vol_val_no_locs, "label")

# Evaluation metrics
rmse_xgb_final_vol_val_no_locs <- sqrt(mean((preds_xgb_final_vol_val_no_locs - actuals)^2))
mae_xgb_final_vol_val_no_locs <- mean(abs(preds_xgb_final_vol_val_no_locs - actuals))
r2_xgb_final_vol_val_no_locs <- 1 - sum((preds_xgb_final_vol_val_no_locs - actuals)^2) / sum((actuals - mean(actuals))^2)

# Print results
cat("🔍 Final XGBoost Volume and Valence Model Test Set Evaluation No Opening Locations:\n")
cat("RMSE:", round(rmse_xgb_final_vol_val_no_locs, 4), "\n")
cat("MAE :", round(mae_xgb_final_vol_val_no_locs, 4), "\n")
cat("R²  :", round(r2_xgb_final_vol_val_no_locs, 4), "\n")

# variable importance
importance_matrix <- xgb.importance(model = xgb_final_vol_val_no_locs)

# Plot top 10 features by 'Gain'
xgb.plot.importance(importance_matrix[1:10, ], top_n = 10, 
                    measure = "Gain", 
                    rel_to_first = TRUE, 
                    xlab = "Relative Importance (Gain)", 
                    main = "Top 10 Variable Importances: XGBoost (No Opening Locations)")


############################ REGULARIZED REGRESSION MODELS ##################################################

##################################### RIDGE REGRESSIONS ########################################################
################ 1. ridge regression without any buzz variables 
# preparing the data for a regularized regression 
# Create model matrix (this will one-hot encode factors automatically)
x_train_no_buzz <- model.matrix(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                          distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                          num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel - 1, data = train_data)  # -1 removes intercept
y_train <- train_data$log_opening_weekend_eur

x_test_no_buzz <- model.matrix(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                         distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                         num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel - 1, data = test_data)
y_test <- test_data$log_opening_weekend_eur


##### lasso showing some issues owing to one category being missing during one-hot encoding. let's try and resolve this 
# Step 1: Add a set flag to both datasets
train_data$set <- "train"
test_data$set <- "test"

# Step 2: Combine train and test
combined_data <- rbind(train_data, test_data)

# Step 3: Select only the predictor columns you want for modeling
# You can adjust this list to match the predictors you've used in Random Forest/XGBoost
predictors_no_buzz <- c("opening_locs", "run_time", "log_star_power_count", "director_power", 
                "distributor_power", "is_holiday_release", "release_month", "release_day_name", 
                "release_week", "num_movies_same_week", "genre_grouped", "MPAA_rating", 
                "era_group", "is_sequel")

# Step 4: Build model matrix on combined data
x_full <- model.matrix(~ . - 1, data = combined_data[, predictors_no_buzz])

# Step 5: Extract aligned train/test matrices from combined model matrix
x_train_no_buzz <- x_full[combined_data$set == "train", ]
x_test_no_buzz <- x_full[combined_data$set == "test", ]

# Step 6: Extract aligned targets
y_train <- train_data$log_opening_weekend_eur
y_test <- test_data$log_opening_weekend_eur





# sanity check: all columns present in train data are also present in test data 
all(colnames(x_train_no_buzz) == colnames(x_test_no_buzz))  # should return TRUE

# Step 1: Fit Ridge Regression with Cross-Validation
set.seed(42)

ridge_cv <- cv.glmnet(
  x = x_train_no_buzz,
  y = y_train,
  alpha = 0,               # Ridge
  nfolds = 10,             # 10-fold CV
  standardize = TRUE
)

# Step 2: View Best Lambda Min
best_lambda_ridge_min <- ridge_cv$lambda.min
cat("Best lambda (Ridge Min):", best_lambda_ridge_min, "\n")

# view best lambda 1se
best_lambda_ridge_1se <- ridge_cv$lambda.1se
cat("Best lambda (Ridge 1se):", best_lambda_ridge_1se, "\n")


# Step 3: Plot CV Error vs. Log(lambda)
plot(ridge_cv)
title("Ridge: Cross-Validation Curve")

# Step 4: Coefficient Path Plot
ridge_model_full <- glmnet(
  x = x_train_no_buzz,
  y = y_train,
  alpha = 0,
  standardize = TRUE
)

plot(ridge_model_full, xvar = "lambda", label = TRUE)
title("Ridge: Coefficient Paths")

# extracting coefficients for the best model with lamda min
ridge_coefs_min_no_buzz <- coef(ridge_cv, s = best_lambda_ridge_min)
print(ridge_coefs_min_no_buzz)

# extracting coefficients for the best model with lamda 1se
ridge_coefs_1se_no_buzz <- coef(ridge_cv, s = best_lambda_ridge_1se)
print(ridge_coefs_1se_no_buzz)


# Step 5: Test Set Evaluation with lambda min
ridge_preds_min_no_buzz <- predict(ridge_cv, s = best_lambda_ridge_min, newx = x_test_no_buzz)

rmse_ridge_min_no_buzz <- sqrt(mean((ridge_preds_min_no_buzz - y_test)^2))
mae_ridge_min_no_buzz <- mean(abs(ridge_preds_min_no_buzz - y_test))
r2_ridge_min_no_buzz <- 1 - sum((ridge_preds_min_no_buzz - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda Min Test Set Evaluation (no buzz):\n")
cat("RMSE:", round(rmse_ridge_min_no_buzz, 4), "\n")
cat("MAE:", round(mae_ridge_min_no_buzz, 4), "\n")
cat("R-squared:", round(r2_ridge_min_no_buzz, 4), "\n")



# Step 5: Test Set Evaluation with lambda min
ridge_preds_1se_no_buzz <- predict(ridge_cv, s = best_lambda_ridge_1se, newx = x_test_no_buzz)

rmse_ridge_1se_no_buzz <- sqrt(mean((ridge_preds_1se_no_buzz - y_test)^2))
mae_ridge_1se_no_buzz <- mean(abs(ridge_preds_1se_no_buzz - y_test))
r2_ridge_1se_no_buzz <- 1 - sum((ridge_preds_1se_no_buzz - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda 1se Test Set Evaluation (no buzz):\n")
cat("RMSE:", round(rmse_ridge_1se_no_buzz, 4), "\n")
cat("MAE:", round(mae_ridge_1se_no_buzz, 4), "\n")
cat("R-squared:", round(r2_ridge_1se_no_buzz, 4), "\n")



################ 2. ridge regression with BUZZ VOLUME ONLY!!!!!
# preparing the data for a regularized regression 
# Create model matrix (this will one-hot encode factors automatically)
x_train_buzz_vol_only <- model.matrix(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                                  distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                                  num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel + log_n_comments - 1, data = train_data)  # -1 removes intercept
y_train <- train_data$log_opening_weekend_eur

x_test_buzz_vol_only <- model.matrix(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                                 distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                                 num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel + log_n_comments - 1, data = test_data)
y_test <- test_data$log_opening_weekend_eur


# You can adjust this list to match the predictors
predictors_buzz_vol_only <- c("opening_locs", "run_time", "log_star_power_count", "director_power", 
                        "distributor_power", "is_holiday_release", "release_month", "release_day_name", 
                        "release_week", "num_movies_same_week", "genre_grouped", "MPAA_rating", 
                        "era_group", "is_sequel", "log_n_comments")

# Step 4: Build model matrix on combined data
x_full_vol_only <- model.matrix(~ . - 1, data = combined_data[, predictors_buzz_vol_only])

# Step 5: Extract aligned train/test matrices from combined model matrix
x_train_vol_only <- x_full_vol_only[combined_data$set == "train", ]
x_test_vol_only <- x_full_vol_only[combined_data$set == "test", ]



# sanity check: all columns present in train data are also present in test data 
all(colnames(x_train_vol_only) == colnames(x_test_vol_only))  # should return TRUE



# Step 1: Fit Ridge Regression with Cross-Validation
set.seed(42)

ridge_cv_vol_only <- cv.glmnet(
  x = x_train_vol_only,
  y = y_train,
  alpha = 0,               # Ridge
  nfolds = 10,             # 10-fold CV
  standardize = TRUE
)

# Step 2: View Best Lambda Min
best_lambda_min_ridge_vol_only <- ridge_cv_vol_only$lambda.min
cat("Best lambda min (Ridge):", best_lambda_min_ridge_vol_only, "\n")

# Step 3: View Best Lambda 1se
best_lambda_1se_ridge_vol_only <- ridge_cv_vol_only$lambda.1se
cat("Best lambda 1se (Ridge):", best_lambda_1se_ridge_vol_only, "\n")


# Step 3: Plot CV Error vs. Log(lambda)
plot(ridge_cv_vol_only)
title("Ridge: Cross-Validation Curve")

# Step 4: Coefficient Path Plot
ridge_model_vol_only <- glmnet(
  x = x_train_vol_only,
  y = y_train,
  alpha = 0,
  standardize = TRUE
)

plot(ridge_model_vol_only, xvar = "lambda", label = TRUE)
title("Ridge: Coefficient Paths")

# extracting coefficients for the best model with lambda min
ridge_coefs_min_vol_only <- coef(ridge_cv_vol_only, s = best_lambda_min_ridge_vol_only)
print(ridge_coefs_min_vol_only)

# extracting coefficients for the best model with lambda 1se
ridge_coefs_1se_vol_only <- coef(ridge_cv_vol_only, s = best_lambda_1se_ridge_vol_only)
print(ridge_coefs_1se_vol_only)




# Step 5: Test Set Evaluation with lambda min
ridge_preds_min_vol_only <- predict(ridge_cv_vol_only, s = best_lambda_min_ridge_vol_only, newx = x_test_vol_only)

rmse_ridge_min_buzz_vol_only <- sqrt(mean((ridge_preds_min_vol_only - y_test)^2))
mae_ridge_min_buzz_vol_only <- mean(abs(ridge_preds_min_vol_only - y_test))
r2_ridge_min_buzz_vol_only <- 1 - sum((ridge_preds_min_vol_only - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda Min Test Set Evaluation (buzz volume only):\n")
cat("RMSE:", round(rmse_ridge_min_buzz_vol_only, 4), "\n")
cat("MAE:", round(mae_ridge_min_buzz_vol_only, 4), "\n")
cat("R-squared:", round(r2_ridge_min_buzz_vol_only, 4), "\n")

# Step 6: Test Set Evaluation with lambda 1se
ridge_preds_1se_vol_only <- predict(ridge_cv_vol_only, s = best_lambda_1se_ridge_vol_only, newx = x_test_vol_only)

rmse_ridge_1se_buzz_vol_only <- sqrt(mean((ridge_preds_1se_vol_only - y_test)^2))
mae_ridge_1se_buzz_vol_only <- mean(abs(ridge_preds_1se_vol_only - y_test))
r2_ridge_1se_buzz_vol_only <- 1 - sum((ridge_preds_1se_vol_only - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda 1se Test Set Evaluation (buzz volume only):\n")
cat("RMSE:", round(rmse_ridge_1se_buzz_vol_only, 4), "\n")
cat("MAE:", round(mae_ridge_1se_buzz_vol_only, 4), "\n")
cat("R-squared:", round(r2_ridge_1se_buzz_vol_only, 4), "\n")




################ 3. ridge regression with BUZZ VOLUME AND VALENCE!!!!!
# preparing the data for a regularized regression 
# Create model matrix (this will one-hot encode factors automatically)
x_train_buzz_vol_val <- model.matrix(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                                        distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                                        num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel + log_n_comments + prop_pos + prop_neg + prop_neut - 1, data = train_data)  # -1 removes intercept
y_train <- train_data$log_opening_weekend_eur

x_test_buzz_vol_val <- model.matrix(log_opening_weekend_eur ~ opening_locs + run_time + log_star_power_count + director_power + 
                                       distributor_power + is_holiday_release + release_month + release_day_name + release_week +
                                       num_movies_same_week + genre_grouped + MPAA_rating + era_group + is_sequel + log_n_comments + prop_pos + prop_neg + prop_neut - 1, data = test_data)
y_test <- test_data$log_opening_weekend_eur


# You can adjust this list to match the predictors
predictors_buzz_vol_val <- c("opening_locs", "run_time", "log_star_power_count", "director_power", 
                              "distributor_power", "is_holiday_release", "release_month", "release_day_name", 
                              "release_week", "num_movies_same_week", "genre_grouped", "MPAA_rating", 
                              "era_group", "is_sequel", "log_n_comments", "prop_pos", "prop_neg")

# Step 4: Build model matrix on combined data
x_full_vol_val <- model.matrix(~ . - 1, data = combined_data[, predictors_buzz_vol_val])

# Step 5: Extract aligned train/test matrices from combined model matrix
x_train_vol_val <- x_full_vol_val[combined_data$set == "train", ]
x_test_vol_val <- x_full_vol_val[combined_data$set == "test", ]



# sanity check: all columns present in train data are also present in test data 
all(colnames(x_train_vol_val) == colnames(x_test_vol_val))  # should return TRUE

# Step 1: Fit Ridge Regression with Cross-Validation
set.seed(42)

ridge_cv_vol_val <- cv.glmnet(
  x = x_train_vol_val,
  y = y_train,
  alpha = 0,               # Ridge
  nfolds = 10,             # 10-fold CV
  standardize = TRUE
)

# Step 2: View Best Lambda Min
best_lambda_min_ridge_vol_val <- ridge_cv_vol_val$lambda.min
cat("Best lambda Min (Ridge):", best_lambda_min_ridge_vol_val, "\n")


# Step 2: View Best Lambda 1se
best_lambda_1se_ridge_vol_val <- ridge_cv_vol_val$lambda.1se
cat("Best lambda 1se (Ridge):", best_lambda_1se_ridge_vol_val, "\n")

# Step 3: Plot CV Error vs. Log(lambda)
# Save improved plot with tighter layout
png("ridge_cv_plot_highres.png", width = 2400, height = 1600, res = 300)

# Set tighter margins: bottom, left, top, right
par(mar = c(5, 5, 4, 4))  # adjust margins if needed

# Plot the CV curve
plot(ridge_cv_vol_val,
     xlab = "Log(Lambda)",
     ylab = "Mean Cross-Validation Error",
     cex.main = 1.4,
     cex.lab = 1.2,
     cex.axis = 1,
     las = 1,
     col = "red",
     pch = 20,
     xaxt = "s")

# Add vertical lines for best lambda and 1se lambda
abline(v = log(best_lambda_min_ridge_vol_val), col = "blue", lty = 2)
abline(v = log(best_lambda_1se_ridge_vol_val), col = "forestgreen", lty = 2)

# Add legend at bottom right with raw lambda values
legend("bottomright",
       legend = c(
         paste0("Min Lambda = ", round(best_lambda_min_ridge_vol_val, 4)),
         paste0("1-SE Lambda = ", round(best_lambda_1se_ridge_vol_val, 4))
       ),
       col = c("blue", "forestgreen"),
       lty = 2,
       cex = 1,
       bty = "n")  # no border

dev.off()

# Step 4: Coefficient Path Plot
ridge_model_vol_val <- glmnet(
  x = x_train_vol_val,
  y = y_train,
  alpha = 0,
  standardize = TRUE
)

plot(ridge_model_vol_val, xvar = "lambda", label = TRUE)
title("Ridge: Coefficient Paths")

# Save as high-resolution PNG
png("ridge_coeff_paths_highres.png", width = 2400, height = 1600, res = 300)


# Plot coefficient paths
plot(ridge_model_vol_val,
     xvar = "lambda",       # x-axis is log(Lambda)
     label = TRUE,          # label some variables
     col = rainbow(20),     # color variety if >10 variables
     lwd = 2,               # thicker lines for visibility
     xaxt = "s")            # show x-axis

dev.off()




# extracting coefficients for the best model with lambda min
ridge_min_coefs_vol_val <- coef(ridge_cv_vol_val, s = best_lambda_min_ridge_vol_val)
print(ridge_min_coefs_vol_val)

# extracting coefficients for the best model with lambda 1se
ridge_1se_coefs_vol_val <- coef(ridge_cv_vol_val, s = best_lambda_1se_ridge_vol_val)
print(ridge_1se_coefs_vol_val)


# Step 5: Test Set Evaluation with lambda min
ridge_min_preds_vol_val <- predict(ridge_cv_vol_val, s = best_lambda_min_ridge_vol_val, newx = x_test_vol_val)

rmse_ridge_min_buzz_vol_val <- sqrt(mean((ridge_min_preds_vol_val - y_test)^2))
mae_ridge_min_buzz_vol_val <- mean(abs(ridge_min_preds_vol_val - y_test))
r2_ridge_min_buzz_vol_val <- 1 - sum((ridge_min_preds_vol_val - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda Min Test Set Evaluation (buzz volume and valence):\n")
cat("RMSE:", round(rmse_ridge_min_buzz_vol_val, 4), "\n")
cat("MAE:", round(mae_ridge_min_buzz_vol_val, 4), "\n")
cat("R-squared:", round(r2_ridge_min_buzz_vol_val, 4), "\n")

# Step 5: Test Set Evaluation with lambda 1se
ridge_1se_preds_buzz_vol_val <- predict(ridge_cv_vol_val, s = best_lambda_1se_ridge_vol_val, newx = x_test_vol_val)

rmse_ridge_1se_buzz_vol_val <- sqrt(mean((ridge_1se_preds_buzz_vol_val - y_test)^2))
mae_ridge_1se_buzz_vol_val <- mean(abs(ridge_1se_preds_buzz_vol_val - y_test))
r2_ridge_1se_buzz_vol_val <- 1 - sum((ridge_1se_preds_buzz_vol_val - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda 1SE Test Set Evaluation (buzz volume and valence):\n")
cat("RMSE:", round(rmse_ridge_1se_buzz_vol_val, 4), "\n")
cat("MAE:", round(mae_ridge_1se_buzz_vol_val, 4), "\n")
cat("R-squared:", round(r2_ridge_1se_buzz_vol_val, 4), "\n")


########################## ROBUSTNESS CHECK: NO OPENING LOCATIONS ###############################################
################ 1. ridge regression without any buzz variables and no opening locations
##### lasso showing some issues owing to one category being missing during one-hot encoding. let's try and resolve this 
# Step 1: Add a set flag to both datasets
train_data$set <- "train"
test_data$set <- "test"

# Step 2: Combine train and test
combined_data <- rbind(train_data, test_data)

# Step 3: Select only the predictor columns you want for modeling
# You can adjust this list to match the predictors you've used in Random Forest/XGBoost
predictors_no_buzz_no_locs <- c("run_time", "log_star_power_count", "director_power", 
                        "distributor_power", "is_holiday_release", "release_month", "release_day_name", 
                        "release_week", "num_movies_same_week", "genre_grouped", "MPAA_rating", 
                        "era_group", "is_sequel")

# Step 4: Build model matrix on combined data
x_full_no_locs <- model.matrix(~ . - 1, data = combined_data[, predictors_no_buzz_no_locs])

# Step 5: Extract aligned train/test matrices from combined model matrix
x_train_no_buzz_no_locs <- x_full_no_locs[combined_data$set == "train", ]
x_test_no_buzz_no_locs <- x_full_no_locs[combined_data$set == "test", ]

# Step 6: Extract aligned targets
y_train <- train_data$log_opening_weekend_eur
y_test <- test_data$log_opening_weekend_eur





# sanity check: all columns present in train data are also present in test data 
all(colnames(x_train_no_buzz_no_locs) == colnames(x_test_no_buzz_no_locs))  # should return TRUE

# Step 1: Fit Ridge Regression with Cross-Validation
set.seed(42)

ridge_cv_no_locs <- cv.glmnet(
  x = x_train_no_buzz_no_locs,
  y = y_train,
  alpha = 0,               # Ridge
  nfolds = 10,             # 10-fold CV
  standardize = TRUE
)

# Step 2: View Best Lambda Min
best_lambda_ridge_min_no_locs <- ridge_cv_no_locs$lambda.min
cat("Best lambda (Ridge Min):", best_lambda_ridge_min_no_locs, "\n")

# view best lambda 1se
best_lambda_ridge_1se_no_locs <- ridge_cv_no_locs$lambda.1se
cat("Best lambda (Ridge 1se):", best_lambda_ridge_1se_no_locs, "\n")


# Step 3: Plot CV Error vs. Log(lambda)
plot(ridge_cv_no_locs)
title("Ridge: Cross-Validation Curve")

# Step 4: Coefficient Path Plot
ridge_model_full_no_locs <- glmnet(
  x = x_train_no_buzz_no_locs,
  y = y_train,
  alpha = 0,
  standardize = TRUE
)

plot(ridge_model_full_no_locs, xvar = "lambda", label = TRUE)
title("Ridge: Coefficient Paths")

# extracting coefficients for the best model with lamda min
ridge_coefs_min_no_buzz_no_locs <- coef(ridge_cv_no_locs, s = best_lambda_ridge_min_no_locs)
print(ridge_coefs_min_no_buzz_no_locs)

# extracting coefficients for the best model with lamda 1se
ridge_coefs_1se_no_buzz_no_locs <- coef(ridge_cv_no_locs, s = best_lambda_ridge_1se_no_locs)
print(ridge_coefs_1se_no_buzz_no_locs)


# Step 5: Test Set Evaluation with lambda min
ridge_preds_min_no_buzz_no_locs <- predict(ridge_cv_no_locs, s = best_lambda_ridge_min_no_locs, newx = x_test_no_buzz_no_locs)

rmse_ridge_min_no_buzz_no_locs <- sqrt(mean((ridge_preds_min_no_buzz_no_locs - y_test)^2))
mae_ridge_min_no_buzz_no_locs <- mean(abs(ridge_preds_min_no_buzz_no_locs - y_test))
r2_ridge_min_no_buzz_no_locs <- 1 - sum((ridge_preds_min_no_buzz_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda Min Test Set Evaluation (no buzz and no opening locations):\n")
cat("RMSE:", round(rmse_ridge_min_no_buzz_no_locs, 4), "\n")
cat("MAE:", round(mae_ridge_min_no_buzz_no_locs, 4), "\n")
cat("R-squared:", round(r2_ridge_min_no_buzz_no_locs, 4), "\n")



# Step 5: Test Set Evaluation with lambda 1SE
ridge_preds_1se_no_buzz_no_locs <- predict(ridge_cv_no_locs, s = best_lambda_ridge_1se_no_locs, newx = x_test_no_buzz_no_locs)

rmse_ridge_1se_no_buzz_no_locs <- sqrt(mean((ridge_preds_1se_no_buzz_no_locs - y_test)^2))
mae_ridge_1se_no_buzz_no_locs <- mean(abs(ridge_preds_1se_no_buzz_no_locs - y_test))
r2_ridge_1se_no_buzz_no_locs <- 1 - sum((ridge_preds_1se_no_buzz_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda 1se Test Set Evaluation (no buzz and no opening locations):\n")
cat("RMSE:", round(rmse_ridge_1se_no_buzz_no_locs, 4), "\n")
cat("MAE:", round(mae_ridge_1se_no_buzz_no_locs, 4), "\n")
cat("R-squared:", round(r2_ridge_1se_no_buzz_no_locs, 4), "\n")



################ 2. ridge regression with BUZZ VOLUME ONLY!!!!!
# preparing the data for a regularized regression 
# You can adjust this list to match the predictors
predictors_buzz_vol_only_no_locs <- c("run_time", "log_star_power_count", "director_power", 
                              "distributor_power", "is_holiday_release", "release_month", "release_day_name", 
                              "release_week", "num_movies_same_week", "genre_grouped", "MPAA_rating", 
                              "era_group", "is_sequel", "log_n_comments")

# Step 4: Build model matrix on combined data
x_full_vol_only_no_locs <- model.matrix(~ . - 1, data = combined_data[, predictors_buzz_vol_only_no_locs])

# Step 5: Extract aligned train/test matrices from combined model matrix
x_train_vol_only_no_locs <- x_full_vol_only_no_locs[combined_data$set == "train", ]
x_test_vol_only_no_locs <- x_full_vol_only_no_locs[combined_data$set == "test", ]



# sanity check: all columns present in train data are also present in test data 
all(colnames(x_train_vol_only_no_locs) == colnames(x_test_vol_only_no_locs))  # should return TRUE



# Step 1: Fit Ridge Regression with Cross-Validation
set.seed(42)

ridge_cv_vol_only_no_locs <- cv.glmnet(
  x = x_train_vol_only_no_locs,
  y = y_train,
  alpha = 0,               # Ridge
  nfolds = 10,             # 10-fold CV
  standardize = TRUE
)

# Step 2: View Best Lambda Min
best_lambda_min_ridge_vol_only_no_locs <- ridge_cv_vol_only_no_locs$lambda.min
cat("Best lambda min (Ridge):", best_lambda_min_ridge_vol_only_no_locs, "\n")

# Step 3: View Best Lambda 1se
best_lambda_1se_ridge_vol_only_no_locs <- ridge_cv_vol_only_no_locs$lambda.1se
cat("Best lambda 1se (Ridge):", best_lambda_1se_ridge_vol_only_no_locs, "\n")


# Step 3: Plot CV Error vs. Log(lambda)
plot(ridge_cv_vol_only_no_locs)
title("Ridge: Cross-Validation Curve")

# Step 4: Coefficient Path Plot
ridge_model_vol_only_no_locs <- glmnet(
  x = x_train_vol_only_no_locs,
  y = y_train,
  alpha = 0,
  standardize = TRUE
)

plot(ridge_model_vol_only_no_locs, xvar = "lambda", label = TRUE)
title("Ridge: Coefficient Paths")

# extracting coefficients for the best model with lambda min
ridge_coefs_min_vol_only_no_locs <- coef(ridge_cv_vol_only_no_locs, s = best_lambda_min_ridge_vol_only_no_locs)
print(ridge_coefs_min_vol_only_no_locs)

# extracting coefficients for the best model with lambda 1se
ridge_coefs_1se_vol_only_no_locs <- coef(ridge_cv_vol_only_no_locs, s = best_lambda_1se_ridge_vol_only_no_locs)
print(ridge_coefs_1se_vol_only_no_locs)




# Step 5: Test Set Evaluation with lambda min
ridge_preds_min_vol_only_no_locs <- predict(ridge_cv_vol_only_no_locs, s = best_lambda_min_ridge_vol_only_no_locs, newx = x_test_vol_only_no_locs)

rmse_ridge_min_buzz_vol_only_no_locs <- sqrt(mean((ridge_preds_min_vol_only_no_locs - y_test)^2))
mae_ridge_min_buzz_vol_only_no_locs <- mean(abs(ridge_preds_min_vol_only_no_locs - y_test))
r2_ridge_min_buzz_vol_only_no_locs <- 1 - sum((ridge_preds_min_vol_only_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda Min Test Set Evaluation (buzz volume only and no opening locations):\n")
cat("RMSE:", round(rmse_ridge_min_buzz_vol_only_no_locs, 4), "\n")
cat("MAE:", round(mae_ridge_min_buzz_vol_only_no_locs, 4), "\n")
cat("R-squared:", round(r2_ridge_min_buzz_vol_only_no_locs, 4), "\n")

# Step 6: Test Set Evaluation with lambda 1se
ridge_preds_1se_vol_only_no_locs <- predict(ridge_cv_vol_only_no_locs, s = best_lambda_1se_ridge_vol_only_no_locs, newx = x_test_vol_only_no_locs)

rmse_ridge_1se_buzz_vol_only_no_locs <- sqrt(mean((ridge_preds_1se_vol_only_no_locs - y_test)^2))
mae_ridge_1se_buzz_vol_only_no_locs <- mean(abs(ridge_preds_1se_vol_only_no_locs - y_test))
r2_ridge_1se_buzz_vol_only_no_locs <- 1 - sum((ridge_preds_1se_vol_only_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda 1se Test Set Evaluation (buzz volume only and no opening locations):\n")
cat("RMSE:", round(rmse_ridge_1se_buzz_vol_only_no_locs, 4), "\n")
cat("MAE:", round(mae_ridge_1se_buzz_vol_only_no_locs, 4), "\n")
cat("R-squared:", round(r2_ridge_1se_buzz_vol_only_no_locs, 4), "\n")




################ 3. ridge regression with BUZZ VOLUME AND VALENCE!!!!!
# preparing the data for a regularized regression 
# You can adjust this list to match the predictors
predictors_buzz_vol_val_no_locs <- c("run_time", "log_star_power_count", "director_power", 
                             "distributor_power", "is_holiday_release", "release_month", "release_day_name", 
                             "release_week", "num_movies_same_week", "genre_grouped", "MPAA_rating", 
                             "era_group", "is_sequel", "log_n_comments", "prop_pos", "prop_neg")

# Step 4: Build model matrix on combined data
x_full_vol_val_no_locs <- model.matrix(~ . - 1, data = combined_data[, predictors_buzz_vol_val_no_locs])

# Step 5: Extract aligned train/test matrices from combined model matrix
x_train_vol_val_no_locs <- x_full_vol_val_no_locs[combined_data$set == "train", ]
x_test_vol_val_no_locs <- x_full_vol_val_no_locs[combined_data$set == "test", ]



# sanity check: all columns present in train data are also present in test data 
all(colnames(x_train_vol_val_no_locs) == colnames(x_test_vol_val_no_locs))  # should return TRUE

# Step 1: Fit Ridge Regression with Cross-Validation
set.seed(42)

ridge_cv_vol_val_no_locs <- cv.glmnet(
  x = x_train_vol_val_no_locs,
  y = y_train,
  alpha = 0,               # Ridge
  nfolds = 10,             # 10-fold CV
  standardize = TRUE
)

# Step 2: View Best Lambda Min
best_lambda_min_ridge_vol_val_no_locs <- ridge_cv_vol_val_no_locs$lambda.min
cat("Best lambda Min (Ridge):", best_lambda_min_ridge_vol_val_no_locs, "\n")


# Step 2: View Best Lambda 1se
best_lambda_1se_ridge_vol_val_no_locs <- ridge_cv_vol_val_no_locs$lambda.1se
cat("Best lambda 1se (Ridge):", best_lambda_1se_ridge_vol_val_no_locs, "\n")

# Step 4: Coefficient Path Plot
ridge_model_vol_val_no_locs <- glmnet(
  x = x_train_vol_val_no_locs,
  y = y_train,
  alpha = 0,
  standardize = TRUE
)

plot(ridge_model_vol_val_no_locs, xvar = "lambda", label = TRUE)
title("Ridge: Coefficient Paths")

# extracting coefficients for the best model with lambda min
ridge_min_coefs_vol_val_no_locs <- coef(ridge_cv_vol_val_no_locs, s = best_lambda_min_ridge_vol_val_no_locs)
print(ridge_min_coefs_vol_val_no_locs)

# extracting coefficients for the best model with lambda 1se
ridge_1se_coefs_vol_val_no_locs <- coef(ridge_cv_vol_val_no_locs, s = best_lambda_1se_ridge_vol_val_no_locs)
print(ridge_1se_coefs_vol_val_no_locs)


# Step 5: Test Set Evaluation with lambda min
ridge_min_preds_vol_val_no_locs <- predict(ridge_cv_vol_val_no_locs, s = best_lambda_min_ridge_vol_val_no_locs, newx = x_test_vol_val_no_locs)

rmse_ridge_min_buzz_vol_val_no_locs <- sqrt(mean((ridge_min_preds_vol_val_no_locs - y_test)^2))
mae_ridge_min_buzz_vol_val_no_locs <- mean(abs(ridge_min_preds_vol_val_no_locs - y_test))
r2_ridge_min_buzz_vol_val_no_locs <- 1 - sum((ridge_min_preds_vol_val_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda Min Test Set Evaluation no opening locations (buzz volume and valence):\n")
cat("RMSE:", round(rmse_ridge_min_buzz_vol_val_no_locs, 4), "\n")
cat("MAE:", round(mae_ridge_min_buzz_vol_val_no_locs, 4), "\n")
cat("R-squared:", round(r2_ridge_min_buzz_vol_val_no_locs, 4), "\n")

# Step 5: Test Set Evaluation with lambda 1se
ridge_1se_preds_buzz_vol_val_no_locs <- predict(ridge_cv_vol_val_no_locs, s = best_lambda_1se_ridge_vol_val_no_locs, newx = x_test_vol_val_no_locs)

rmse_ridge_1se_buzz_vol_val_no_locs <- sqrt(mean((ridge_1se_preds_buzz_vol_val_no_locs - y_test)^2))
mae_ridge_1se_buzz_vol_val_no_locs <- mean(abs(ridge_1se_preds_buzz_vol_val_no_locs - y_test))
r2_ridge_1se_buzz_vol_val_no_locs <- 1 - sum((ridge_1se_preds_buzz_vol_val_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)


cat("Ridge Regression Lambda 1SE Test Set Evaluation no opening locations (buzz volume and valence):\n")
cat("RMSE:", round(rmse_ridge_1se_buzz_vol_val_no_locs, 4), "\n")
cat("MAE:", round(mae_ridge_1se_buzz_vol_val_no_locs, 4), "\n")
cat("R-squared:", round(r2_ridge_1se_buzz_vol_val_no_locs, 4), "\n")

##### variable importance using absolute coefficients 
# Extract coefficients (excluding intercept)
coefs_ridge <- as.matrix(ridge_min_coefs_vol_val_no_locs)
coefs_ridge_df <- data.frame(
  Variable = rownames(coefs_ridge),
  Coefficient = as.numeric(coefs_ridge)
)

# Remove intercept
coefs_ridge_df <- coefs_ridge_df[coefs_ridge_df$Variable != "(Intercept)", ]

# Take absolute value for importance ranking
coefs_ridge_df$Importance <- abs(coefs_ridge_df$Coefficient)

# Get top 10 variables by importance
top_ridge_coefs <- coefs_ridge_df[order(-coefs_ridge_df$Importance), ][1:10, ]

# Plot
ggplot(top_ridge_coefs, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importances (Ridge Regression, Lambda Min)",
    x = "Variable",
    y = "Absolute Coefficient (Importance)"
  ) +
  theme_minimal()



###################################### LASSO REGRESSIONS ###################################################
###################### 1. Lasso WITHOUT any buzz
set.seed(42)

lasso_cv_no_buzz <- cv.glmnet(
  x = x_train_no_buzz,
  y = y_train,
  alpha = 1,               # Lasso regression (L1)
  nfolds = 10,
  standardize = TRUE
)

# View best lambda min
best_lambda_min_lasso_no_buzz <- lasso_cv_no_buzz$lambda.min
cat("Best lambda min (Lasso):", best_lambda_min_lasso_no_buzz, "\n")


# View best lambda 1se
best_lambda_1se_lasso_no_buzz <- lasso_cv_no_buzz$lambda.1se
cat("Best lambda 1se (Lasso):", best_lambda_1se_lasso_no_buzz, "\n")

# Cross-validation error curve
plot(lasso_cv_no_buzz)
title("Lasso: Cross-Validation Curve")

# Coefficient shrinkage path
lasso_model_no_buzz <- glmnet(
  x = x_train_no_buzz,
  y = y_train,
  alpha = 1,
  standardize = TRUE
)

plot(lasso_model_no_buzz, xvar = "lambda", label = TRUE)
title("Lasso: Coefficient Paths")

# Predict using best lambda min
lasso_preds_min_no_buzz <- predict(lasso_cv_no_buzz, s = best_lambda_min_lasso_no_buzz, newx = x_test_no_buzz)

# Evaluation metrics lambda min
rmse_min_lasso_no_buzz <- sqrt(mean((lasso_preds_min_no_buzz - y_test)^2))
mae_min_lasso_no_buzz <- mean(abs(lasso_preds_min_no_buzz - y_test))
r2_min_lasso_no_buzz <- 1 - sum((lasso_preds_min_no_buzz - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda Min Test Set Evaluation (no buzz):\n")
cat("RMSE:", round(rmse_min_lasso_no_buzz, 4), "\n")
cat("MAE:", round(mae_min_lasso_no_buzz, 4), "\n")
cat("R-squared:", round(r2_min_lasso_no_buzz, 4), "\n")

# check the non-zero coefficients lambda min
lasso_min_coefs_no_buzz <- coef(lasso_cv_no_buzz, s = best_lambda_min_lasso_no_buzz)
print(lasso_min_coefs_no_buzz)

# Predict using best lambda 1se
lasso_preds_1se_no_buzz <- predict(lasso_cv_no_buzz, s = best_lambda_1se_lasso_no_buzz, newx = x_test_no_buzz)

# Evaluation metrics lambda 1se
rmse_1se_lasso_no_buzz <- sqrt(mean((lasso_preds_1se_no_buzz - y_test)^2))
mae_1se_lasso_no_buzz <- mean(abs(lasso_preds_1se_no_buzz - y_test))
r2_1se_lasso_no_buzz <- 1 - sum((lasso_preds_1se_no_buzz - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda 1SE Test Set Evaluation (no buzz):\n")
cat("RMSE:", round(rmse_1se_lasso_no_buzz, 4), "\n")
cat("MAE:", round(mae_1se_lasso_no_buzz, 4), "\n")
cat("R-squared:", round(r2_1se_lasso_no_buzz, 4), "\n")

# check the non-zero coefficients lambda 1se
lasso_1se_coefs_no_buzz <- coef(lasso_cv_no_buzz, s = best_lambda_1se_lasso_no_buzz)
print(lasso_1se_coefs_no_buzz)

###################### 2. Lasso with BUZZ VOLUME ONLY!!!!!!!!!!!!!
set.seed(42)

lasso_cv_vol_only <- cv.glmnet(
  x = x_train_vol_only,
  y = y_train,
  alpha = 1,               # Lasso regression (L1)
  nfolds = 10,
  standardize = TRUE
)

# View best lambda
best_lambda_min_lasso_vol_only <- lasso_cv_vol_only$lambda.min
cat("Best lambda min (Lasso):", best_lambda_min_lasso_vol_only, "\n")

# View best lambda
best_lambda_1se_lasso_vol_only <- lasso_cv_vol_only$lambda.1se
cat("Best lambda 1se (Lasso):", best_lambda_1se_lasso_vol_only, "\n")

# Cross-validation error curve
plot(lasso_cv_vol_only)
title("Lasso: Cross-Validation Curve")

# Coefficient shrinkage path
lasso_model_vol_only <- glmnet(
  x = x_train_vol_only,
  y = y_train,
  alpha = 1,
  standardize = TRUE
)

plot(lasso_model_vol_only, xvar = "lambda", label = TRUE)
title("Lasso: Coefficient Paths")

# Predict using best lambda min
lasso_preds_min_vol_only <- predict(lasso_cv_vol_only, s = best_lambda_min_lasso_vol_only, newx = x_test_vol_only)

# Evaluation metrics
rmse_min_lasso_vol_only <- sqrt(mean((lasso_preds_min_vol_only - y_test)^2))
mae_min_lasso_vol_only <- mean(abs(lasso_preds_min_vol_only - y_test))
r2_min_lasso_vol_only <- 1 - sum((lasso_preds_min_vol_only - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda Min Test Set Evaluation (volume only):\n")
cat("RMSE:", round(rmse_min_lasso_vol_only, 4), "\n")
cat("MAE:", round(mae_min_lasso_vol_only, 4), "\n")
cat("R-squared:", round(r2_min_lasso_vol_only, 4), "\n")

# check the non-zero coefficients for lambda min
lasso_min_coefs_vol_only <- coef(lasso_cv_vol_only, s = best_lambda_min_lasso_vol_only)
print(lasso_min_coefs_vol_only)

# Predict using best lambda 1se
lasso_preds_1se_vol_only <- predict(lasso_cv_vol_only, s = best_lambda_1se_lasso_vol_only, newx = x_test_vol_only)

# Evaluation metrics
rmse_1se_lasso_vol_only <- sqrt(mean((lasso_preds_1se_vol_only - y_test)^2))
mae_1se_lasso_vol_only <- mean(abs(lasso_preds_1se_vol_only - y_test))
r2_1se_lasso_vol_only <- 1 - sum((lasso_preds_1se_vol_only - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda 1SE Test Set Evaluation (volume only):\n")
cat("RMSE:", round(rmse_1se_lasso_vol_only, 4), "\n")
cat("MAE:", round(mae_1se_lasso_vol_only, 4), "\n")
cat("R-squared:", round(r2_1se_lasso_vol_only, 4), "\n")

# check the non-zero coefficients for lambda 1se
lasso_1se_coefs_vol_only <- coef(lasso_cv_vol_only, s = best_lambda_1se_lasso_vol_only)
print(lasso_1se_coefs_vol_only)


###################### 3. Lasso with BUZZ VOLUME AND VALENCE!!!!!!!!!!!!!
set.seed(42)

lasso_cv_vol_val <- cv.glmnet(
  x = x_train_vol_val,
  y = y_train,
  alpha = 1,               # Lasso regression (L1)
  nfolds = 10,
  standardize = TRUE
)

# View best lambda min
best_lambda_min_lasso_vol_val <- lasso_cv_vol_val$lambda.min
cat("Best lambda min (Lasso):", best_lambda_min_lasso_vol_val, "\n")

# View best lambda 1se
best_lambda_1se_lasso_vol_val <- lasso_cv_vol_val$lambda.1se
cat("Best lambda 1se (Lasso):", best_lambda_1se_lasso_vol_val, "\n")

# Cross-validation error curve
plot(lasso_cv_vol_val)
title("Lasso: Cross-Validation Curve")

# Step 1: Save high-res plot
png("lasso_cv_error_curve.png", width = 2400, height = 1600, res = 300)

# Set margins: bottom, left, top, right
par(mar = c(5, 5, 4, 4))  # default-ish margins

# Plot Lasso CV curve
plot(lasso_cv_vol_val,
     xlab = "Log(Lambda)",
     ylab = "Mean Cross-Validation Error",
     cex.main = 1.4,
     cex.lab = 1.2,
     cex.axis = 1,
     las = 1,
     col = "red",
     pch = 20,
     xaxt = "s")

# Add vertical lines for lambda.min and lambda.1se
abline(v = log(lasso_cv_vol_val$lambda.min), col = "blue", lty = 2)
abline(v = log(lasso_cv_vol_val$lambda.1se), col = "forestgreen", lty = 2)

# Add legend INSIDE top-left corner of plot with raw lambdas
legend("topleft",
       legend = c(
         paste0("Min Lambda = ", round(lasso_cv_vol_val$lambda.min, 4)),
         paste0("1-SE Lambda = ", round(lasso_cv_vol_val$lambda.1se, 4))
       ),
       col = c("blue", "forestgreen"),
       lty = 2,
       cex = 1,
       bty = "n")  # no box around legend


# Step 6: Save
dev.off()



# Coefficient shrinkage path
lasso_model_vol_val <- glmnet(
  x = x_train_vol_val,
  y = y_train,
  alpha = 1,
  standardize = TRUE
)

plot(lasso_model_vol_val, xvar = "lambda", label = TRUE)
title("Lasso: Coefficient Paths")


# Save as high-resolution PNG
png("lasso_coeff_paths_highres.png", width = 2400, height = 1600, res = 300)


# Plot coefficient paths
plot(lasso_model_vol_val,
     xvar = "lambda",       # x-axis is log(Lambda)
     label = TRUE,          # label some variables
     col = rainbow(20),     # color variety if >10 variables
     lwd = 2,               # thicker lines for visibility
     xaxt = "s")            # show x-axis

dev.off()




# Predict using best lambda min
lasso_min_preds_vol_val <- predict(lasso_cv_vol_val, s = best_lambda_min_lasso_vol_val, newx = x_test_vol_val)

# Evaluation metrics lambda min
rmse_min_lasso_vol_val <- sqrt(mean((lasso_min_preds_vol_val - y_test)^2))
mae_min_lasso_vol_val <- mean(abs(lasso_min_preds_vol_val - y_test))
r2_min_lasso_vol_val <- 1 - sum((lasso_min_preds_vol_val - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda Min Test Set Evaluation (volume and valence):\n")
cat("RMSE:", round(rmse_min_lasso_vol_val, 4), "\n")
cat("MAE:", round(mae_min_lasso_vol_val, 4), "\n")
cat("R-squared:", round(r2_min_lasso_vol_val, 4), "\n")

# check the non-zero coefficients lambda min
lasso_min_coefs_vol_val <- coef(lasso_cv_vol_val, s = best_lambda_min_lasso_vol_val)
print(lasso_min_coefs_vol_val)


# Predict using best lambda 1se
lasso_1se_preds_vol_val <- predict(lasso_cv_vol_val, s = best_lambda_1se_lasso_vol_val, newx = x_test_vol_val)

# Evaluation metrics lambda 1se
rmse_1se_lasso_vol_val <- sqrt(mean((lasso_1se_preds_vol_val - y_test)^2))
mae_1se_lasso_vol_val <- mean(abs(lasso_1se_preds_vol_val - y_test))
r2_1se_lasso_vol_val <- 1 - sum((lasso_1se_preds_vol_val - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda 1SE Test Set Evaluation (volume and valence):\n")
cat("RMSE:", round(rmse_1se_lasso_vol_val, 4), "\n")
cat("MAE:", round(mae_1se_lasso_vol_val, 4), "\n")
cat("R-squared:", round(r2_1se_lasso_vol_val, 4), "\n")

# check the non-zero coefficients lambda 1se
lasso_1se_coefs_vol_val <- coef(lasso_cv_vol_val, s = best_lambda_1se_lasso_vol_val)
print(lasso_1se_coefs_vol_val)


####################### ROBUSTNESS CHECK: NO OPENING LOCATIONS #######################################
###################### 1. Lasso WITHOUT any buzz
set.seed(42)

lasso_cv_no_buzz_no_locs <- cv.glmnet(
  x = x_train_no_buzz_no_locs,
  y = y_train,
  alpha = 1,               # Lasso regression (L1)
  nfolds = 10,
  standardize = TRUE
)

# View best lambda min
best_lambda_min_lasso_no_buzz_no_locs <- lasso_cv_no_buzz_no_locs$lambda.min
cat("Best lambda min (Lasso):", best_lambda_min_lasso_no_buzz_no_locs, "\n")


# View best lambda 1se
best_lambda_1se_lasso_no_buzz_no_locs <- lasso_cv_no_buzz_no_locs$lambda.1se
cat("Best lambda 1se (Lasso):", best_lambda_1se_lasso_no_buzz_no_locs, "\n")

# Cross-validation error curve
plot(lasso_cv_no_buzz_no_locs)
title("Lasso: Cross-Validation Curve")

# Coefficient shrinkage path
lasso_model_no_buzz_no_locs <- glmnet(
  x = x_train_no_buzz_no_locs,
  y = y_train,
  alpha = 1,
  standardize = TRUE
)

plot(lasso_model_no_buzz_no_locs, xvar = "lambda", label = TRUE)
title("Lasso: Coefficient Paths")

# Predict using best lambda min
lasso_preds_min_no_buzz_no_locs <- predict(lasso_cv_no_buzz_no_locs, s = best_lambda_min_lasso_no_buzz_no_locs, newx = x_test_no_buzz_no_locs)

# Evaluation metrics lambda min
rmse_min_lasso_no_buzz_no_locs <- sqrt(mean((lasso_preds_min_no_buzz_no_locs - y_test)^2))
mae_min_lasso_no_buzz_no_locs <- mean(abs(lasso_preds_min_no_buzz_no_locs - y_test))
r2_min_lasso_no_buzz_no_locs <- 1 - sum((lasso_preds_min_no_buzz_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda Min Test Set Evaluation no opening locations (no buzz):\n")
cat("RMSE:", round(rmse_min_lasso_no_buzz_no_locs, 4), "\n")
cat("MAE:", round(mae_min_lasso_no_buzz_no_locs, 4), "\n")
cat("R-squared:", round(r2_min_lasso_no_buzz_no_locs, 4), "\n")

# check the non-zero coefficients lambda min
lasso_min_coefs_no_buzz_no_locs <- coef(lasso_cv_no_buzz_no_locs, s = best_lambda_min_lasso_no_buzz_no_locs)
print(lasso_min_coefs_no_buzz_no_locs)

# Predict using best lambda 1se
lasso_preds_1se_no_buzz_no_locs <- predict(lasso_cv_no_buzz_no_locs, s = best_lambda_1se_lasso_no_buzz_no_locs, newx = x_test_no_buzz_no_locs)

# Evaluation metrics lambda 1se
rmse_1se_lasso_no_buzz_no_locs <- sqrt(mean((lasso_preds_1se_no_buzz_no_locs - y_test)^2))
mae_1se_lasso_no_buzz_no_locs <- mean(abs(lasso_preds_1se_no_buzz_no_locs - y_test))
r2_1se_lasso_no_buzz_no_locs <- 1 - sum((lasso_preds_1se_no_buzz_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda 1SE Test Set Evaluation no opening locations (no buzz):\n")
cat("RMSE:", round(rmse_1se_lasso_no_buzz_no_locs, 4), "\n")
cat("MAE:", round(mae_1se_lasso_no_buzz_no_locs, 4), "\n")
cat("R-squared:", round(r2_1se_lasso_no_buzz_no_locs, 4), "\n")

# check the non-zero coefficients lambda 1se
lasso_1se_coefs_no_buzz_no_locs <- coef(lasso_cv_no_buzz_no_locs, s = best_lambda_1se_lasso_no_buzz_no_locs)
print(lasso_1se_coefs_no_buzz_no_locs)

###################### 2. Lasso with BUZZ VOLUME ONLY!!!!!!!!!!!!!
set.seed(42)

lasso_cv_vol_only_no_locs <- cv.glmnet(
  x = x_train_vol_only_no_locs,
  y = y_train,
  alpha = 1,               # Lasso regression (L1)
  nfolds = 10,
  standardize = TRUE
)

# View best lambda min
best_lambda_min_lasso_vol_only_no_locs <- lasso_cv_vol_only_no_locs$lambda.min
cat("Best lambda min (Lasso):", best_lambda_min_lasso_vol_only_no_locs, "\n")

# View best lambda 1SE
best_lambda_1se_lasso_vol_only_no_locs <- lasso_cv_vol_only_no_locs$lambda.1se
cat("Best lambda 1se (Lasso):", best_lambda_1se_lasso_vol_only_no_locs, "\n")

# Cross-validation error curve
plot(lasso_cv_vol_only_no_locs)
title("Lasso: Cross-Validation Curve")

# Coefficient shrinkage path
lasso_model_vol_only_no_locs <- glmnet(
  x = x_train_vol_only_no_locs,
  y = y_train,
  alpha = 1,
  standardize = TRUE
)

plot(lasso_model_vol_only_no_locs, xvar = "lambda", label = TRUE)
title("Lasso: Coefficient Paths")

# Predict using best lambda min
lasso_preds_min_vol_only_no_locs <- predict(lasso_cv_vol_only_no_locs, s = best_lambda_min_lasso_vol_only_no_locs, newx = x_test_vol_only_no_locs)

# Evaluation metrics
rmse_min_lasso_vol_only_no_locs <- sqrt(mean((lasso_preds_min_vol_only_no_locs - y_test)^2))
mae_min_lasso_vol_only_no_locs <- mean(abs(lasso_preds_min_vol_only_no_locs - y_test))
r2_min_lasso_vol_only_no_locs <- 1 - sum((lasso_preds_min_vol_only_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda Min Test Set Evaluation no opening locations (volume only):\n")
cat("RMSE:", round(rmse_min_lasso_vol_only_no_locs, 4), "\n")
cat("MAE:", round(mae_min_lasso_vol_only_no_locs, 4), "\n")
cat("R-squared:", round(r2_min_lasso_vol_only_no_locs, 4), "\n")

# check the non-zero coefficients for lambda min
lasso_min_coefs_vol_only_no_locs <- coef(lasso_cv_vol_only_no_locs, s = best_lambda_min_lasso_vol_only_no_locs)
print(lasso_min_coefs_vol_only_no_locs)

# Predict using best lambda 1se
lasso_preds_1se_vol_only_no_locs <- predict(lasso_cv_vol_only_no_locs, s = best_lambda_1se_lasso_vol_only_no_locs, newx = x_test_vol_only_no_locs)

# Evaluation metrics
rmse_1se_lasso_vol_only_no_locs <- sqrt(mean((lasso_preds_1se_vol_only_no_locs - y_test)^2))
mae_1se_lasso_vol_only_no_locs <- mean(abs(lasso_preds_1se_vol_only_no_locs - y_test))
r2_1se_lasso_vol_only_no_locs <- 1 - sum((lasso_preds_1se_vol_only_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda 1SE Test Set Evaluation no opening locations (volume only):\n")
cat("RMSE:", round(rmse_1se_lasso_vol_only_no_locs, 4), "\n")
cat("MAE:", round(mae_1se_lasso_vol_only_no_locs, 4), "\n")
cat("R-squared:", round(r2_1se_lasso_vol_only_no_locs, 4), "\n")

# check the non-zero coefficients for lambda 1se
lasso_1se_coefs_vol_only_no_locs <- coef(lasso_cv_vol_only_no_locs, s = best_lambda_1se_lasso_vol_only_no_locs)
print(lasso_1se_coefs_vol_only_no_locs)


###################### 3. Lasso with BUZZ VOLUME AND VALENCE!!!!!!!!!!!!!
set.seed(42)

lasso_cv_vol_val_no_locs <- cv.glmnet(
  x = x_train_vol_val_no_locs,
  y = y_train,
  alpha = 1,               # Lasso regression (L1)
  nfolds = 10,
  standardize = TRUE
)

# View best lambda min
best_lambda_min_lasso_vol_val_no_locs <- lasso_cv_vol_val_no_locs$lambda.min
cat("Best lambda min (Lasso):", best_lambda_min_lasso_vol_val_no_locs, "\n")

# View best lambda 1se
best_lambda_1se_lasso_vol_val_no_locs <- lasso_cv_vol_val_no_locs$lambda.1se
cat("Best lambda 1se (Lasso):", best_lambda_1se_lasso_vol_val_no_locs, "\n")

# Cross-validation error curve
plot(lasso_cv_vol_val_no_locs)
title("Lasso: Cross-Validation Curve")

# Coefficient shrinkage path
lasso_model_vol_val_no_locs <- glmnet(
  x = x_train_vol_val_no_locs,
  y = y_train,
  alpha = 1,
  standardize = TRUE
)

plot(lasso_model_vol_val_no_locs, xvar = "lambda", label = TRUE)
title("Lasso: Coefficient Paths")

# Predict using best lambda min
lasso_min_preds_vol_val_no_locs <- predict(lasso_cv_vol_val_no_locs, s = best_lambda_min_lasso_vol_val_no_locs, newx = x_test_vol_val_no_locs)

# Evaluation metrics lambda min
rmse_min_lasso_vol_val_no_locs <- sqrt(mean((lasso_min_preds_vol_val_no_locs - y_test)^2))
mae_min_lasso_vol_val_no_locs <- mean(abs(lasso_min_preds_vol_val_no_locs - y_test))
r2_min_lasso_vol_val_no_locs <- 1 - sum((lasso_min_preds_vol_val_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda Min Test Set Evaluation no opening locations (volume and valence):\n")
cat("RMSE:", round(rmse_min_lasso_vol_val_no_locs, 4), "\n")
cat("MAE:", round(mae_min_lasso_vol_val_no_locs, 4), "\n")
cat("R-squared:", round(r2_min_lasso_vol_val_no_locs, 4), "\n")

# check the non-zero coefficients lambda min
lasso_min_coefs_vol_val_no_locs <- coef(lasso_cv_vol_val_no_locs, s = best_lambda_min_lasso_vol_val_no_locs)
print(lasso_min_coefs_vol_val_no_locs)


# Predict using best lambda 1se
lasso_1se_preds_vol_val_no_locs <- predict(lasso_cv_vol_val_no_locs, s = best_lambda_1se_lasso_vol_val_no_locs, newx = x_test_vol_val_no_locs)

# Evaluation metrics lambda 1se
rmse_1se_lasso_vol_val_no_locs <- sqrt(mean((lasso_1se_preds_vol_val_no_locs - y_test)^2))
mae_1se_lasso_vol_val_no_locs <- mean(abs(lasso_1se_preds_vol_val_no_locs - y_test))
r2_1se_lasso_vol_val_no_locs <- 1 - sum((lasso_1se_preds_vol_val_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Lasso Regression Lambda 1SE Test Set Evaluation no opening locations (volume and valence):\n")
cat("RMSE:", round(rmse_1se_lasso_vol_val_no_locs, 4), "\n")
cat("MAE:", round(mae_1se_lasso_vol_val_no_locs, 4), "\n")
cat("R-squared:", round(r2_1se_lasso_vol_val_no_locs, 4), "\n")

# check the non-zero coefficients lambda 1se
lasso_1se_coefs_vol_val_no_locs <- coef(lasso_cv_vol_val_no_locs, s = best_lambda_1se_lasso_vol_val_no_locs)
print(lasso_1se_coefs_vol_val_no_locs)

##### variable importance using absolute coefficients  
# Extract coefficients (as matrix)
coefs_lasso <- as.matrix(lasso_min_coefs_vol_val_no_locs)

# Convert to data frame
coefs_lasso_df <- data.frame(
  Variable = rownames(coefs_lasso),
  Coefficient = as.numeric(coefs_lasso)
)

# Remove intercept
coefs_lasso_df <- coefs_lasso_df[coefs_lasso_df$Variable != "(Intercept)", ]

# Keep only non-zero coefficients
coefs_lasso_df <- coefs_lasso_df[coefs_lasso_df$Coefficient != 0, ]

# Calculate importance
coefs_lasso_df$Importance <- abs(coefs_lasso_df$Coefficient)

# Select top 10 by absolute value
top_coefs_lasso <- coefs_lasso_df[order(-coefs_lasso_df$Importance), ][1:10, ]

# Plot
ggplot(top_coefs_lasso, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importances (Lasso Regression, Lambda Min)",
    x = "Variable",
    y = "Absolute Coefficient (Importance)"
  ) +
  theme_minimal()

################################### ELASTIC NET REGRESSIONS ######################################
################## 1. without buzz 
set.seed(42)

elastic_cv_no_buzz <- cv.glmnet(
  x = x_train_no_buzz,
  y = y_train,
  alpha = 0.5,             # Elastic Net = 50% Ridge + 50% Lasso
  nfolds = 10,
  standardize = TRUE
)

# Best lambda Min
best_lambda_min_elastic_no_buzz <- elastic_cv_no_buzz$lambda.min
cat("Best lambda min (Elastic Net):", best_lambda_min_elastic_no_buzz, "\n")

# Best lambda 1se
best_lambda_1se_elastic_no_buzz <- elastic_cv_no_buzz$lambda.1se
cat("Best lambda 1se (Elastic Net):", best_lambda_1se_elastic_no_buzz, "\n")

# CV Error Curve
plot(elastic_cv_no_buzz)
title("Elastic Net: Cross-Validation Curve")

# Coefficient Paths (optional, not alpha-specific but still informative)
elastic_model_no_buzz <- glmnet(
  x = x_train_no_buzz,
  y = y_train,
  alpha = 0.5,
  standardize = TRUE
)

plot(elastic_model_no_buzz, xvar = "lambda", label = TRUE)
title("Elastic Net: Coefficient Paths")

# predict on test data and check performance with lambda min
elastic_min_preds_no_buzz <- predict(elastic_cv_no_buzz, s = best_lambda_min_elastic_no_buzz, newx = x_test_no_buzz)

rmse_min_elastic_no_buzz <- sqrt(mean((elastic_min_preds_no_buzz - y_test)^2))
mae_min_elastic_no_buzz <- mean(abs(elastic_min_preds_no_buzz - y_test))
r2_min_elastic_no_buzz <- 1 - sum((elastic_min_preds_no_buzz - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda Min Test Set Evaluation (no buzz):\n")
cat("RMSE:", round(rmse_min_elastic_no_buzz, 4), "\n")
cat("MAE :", round(mae_min_elastic_no_buzz, 4), "\n")
cat("R²  :", round(r2_min_elastic_no_buzz, 4), "\n")

# check the coefficients lambda min
elastic_min_coefs_no_buzz <- coef(elastic_cv_no_buzz, s = best_lambda_min_elastic_no_buzz)
print(elastic_min_coefs_no_buzz)

# Predict on test data and check performance with lambda 1se
elastic_1se_preds_no_buzz <- predict(elastic_cv_no_buzz, s = best_lambda_1se_elastic_no_buzz, newx = x_test_no_buzz)

rmse_1se_elastic_no_buzz <- sqrt(mean((elastic_1se_preds_no_buzz - y_test)^2))
mae_1se_elastic_no_buzz <- mean(abs(elastic_1se_preds_no_buzz - y_test))
r2_1se_elastic_no_buzz <- 1 - sum((elastic_1se_preds_no_buzz - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda 1SE Test Set Evaluation (no buzz):\n")
cat("RMSE:", round(rmse_1se_elastic_no_buzz, 4), "\n")
cat("MAE :", round(mae_1se_elastic_no_buzz, 4), "\n")
cat("R²  :", round(r2_1se_elastic_no_buzz, 4), "\n")

# Check the coefficients lambda 1se
elastic_1se_coefs_no_buzz <- coef(elastic_cv_no_buzz, s = best_lambda_1se_elastic_no_buzz)
print(elastic_1se_coefs_no_buzz)

################## 2. BUZZ VOLUME ONLY
set.seed(42)

elastic_cv_vol_only <- cv.glmnet(
  x = x_train_vol_only,
  y = y_train,
  alpha = 0.5,             # Elastic Net = 50% Ridge + 50% Lasso
  nfolds = 10,
  standardize = TRUE
)

# Best lambda min
best_lambda_min_elastic_vol_only <- elastic_cv_vol_only$lambda.min
cat("Best lambda min (Elastic Net):", best_lambda_min_elastic_vol_only, "\n")

# Best lambda 1se
best_lambda_1se_elastic_vol_only <- elastic_cv_vol_only$lambda.1se
cat("Best lambda 1se (Elastic Net):", best_lambda_1se_elastic_vol_only, "\n")

# CV Error Curve
plot(elastic_cv_vol_only)
title("Elastic Net: Cross-Validation Curve")

# Coefficient Paths (optional, not alpha-specific but still informative)
elastic_model_vol_only <- glmnet(
  x = x_train_vol_only,
  y = y_train,
  alpha = 0.5,
  standardize = TRUE
)

plot(elastic_model_vol_only, xvar = "lambda", label = TRUE)
title("Elastic Net: Coefficient Paths")

# predict on test data and check performance lambda min
elastic_min_preds_vol_only <- predict(elastic_cv_vol_only, s = best_lambda_min_elastic_vol_only, newx = x_test_vol_only)

rmse_min_elastic_vol_only <- sqrt(mean((elastic_min_preds_vol_only - y_test)^2))
mae_min_elastic_vol_only <- mean(abs(elastic_min_preds_vol_only - y_test))
r2_min_elastic_vol_only <- 1 - sum((elastic_min_preds_vol_only - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda Min Test Set Evaluation (volume only):\n")
cat("RMSE:", round(rmse_min_elastic_vol_only, 4), "\n")
cat("MAE :", round(mae_min_elastic_vol_only, 4), "\n")
cat("R²  :", round(r2_min_elastic_vol_only, 4), "\n")

# check the coefficients lambda min
elastic_min_coefs_vol_only <- coef(elastic_cv_vol_only, s = best_lambda_min_elastic_vol_only)
print(elastic_min_coefs_vol_only)

# Predict on test data and check performance lambda 1se
elastic_1se_preds_vol_only <- predict(elastic_cv_vol_only, s = best_lambda_1se_elastic_vol_only, newx = x_test_vol_only)

rmse_1se_elastic_vol_only <- sqrt(mean((elastic_1se_preds_vol_only - y_test)^2))
mae_1se_elastic_vol_only <- mean(abs(elastic_1se_preds_vol_only - y_test))
r2_1se_elastic_vol_only <- 1 - sum((elastic_1se_preds_vol_only - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda 1SE Test Set Evaluation (volume only):\n")
cat("RMSE:", round(rmse_1se_elastic_vol_only, 4), "\n")
cat("MAE :", round(mae_1se_elastic_vol_only, 4), "\n")
cat("R²  :", round(r2_1se_elastic_vol_only, 4), "\n")

# Check the coefficients lambda 1se
elastic_1se_coefs_vol_only <- coef(elastic_cv_vol_only, s = best_lambda_1se_elastic_vol_only)
print(elastic_1se_coefs_vol_only)

################## 3. BUZZ VOLUME AND VALENCE
set.seed(42)

elastic_cv_vol_val <- cv.glmnet(
  x = x_train_vol_val,
  y = y_train,
  alpha = 0.5,             # Elastic Net = 50% Ridge + 50% Lasso
  nfolds = 10,
  standardize = TRUE
)

# Best lambda min
best_lambda_min_elastic_vol_val <- elastic_cv_vol_val$lambda.min
cat("Best lambda min (Elastic Net):", best_lambda_min_elastic_vol_val, "\n")

# Best lambda 1se
best_lambda_1se_elastic_vol_val <- elastic_cv_vol_val$lambda.1se
cat("Best lambda 1se (Elastic Net):", best_lambda_1se_elastic_vol_val, "\n")

# CV Error Curve
plot(elastic_cv_vol_val)
title("Elastic Net: Cross-Validation Curve")

# Step 1: Save high-res plot
png("elastic_cv_error_curve.png", width = 2400, height = 1600, res = 300)

# Set margins: bottom, left, top, right
par(mar = c(5, 5, 4, 4))  # default-ish margins

# Plot Lasso CV curve
plot(elastic_cv_vol_val,
     xlab = "Log(Lambda)",
     ylab = "Mean Cross-Validation Error",
     cex.main = 1.4,
     cex.lab = 1.2,
     cex.axis = 1,
     las = 1,
     col = "red",
     pch = 20,
     xaxt = "s")

# Add vertical lines for lambda.min and lambda.1se
abline(v = log(elastic_cv_vol_val$lambda.min), col = "blue", lty = 2)
abline(v = log(elastic_cv_vol_val$lambda.1se), col = "forestgreen", lty = 2)

# Add legend INSIDE top-left corner of plot with raw lambdas
legend("topleft",
       legend = c(
         paste0("Min Lambda = ", round(elastic_cv_vol_val$lambda.min, 4)),
         paste0("1-SE Lambda = ", round(elastic_cv_vol_val$lambda.1se, 4))
       ),
       col = c("blue", "forestgreen"),
       lty = 2,
       cex = 1,
       bty = "n")  # no box around legend


# Step 6: Save
dev.off()



# Coefficient Paths (optional, not alpha-specific but still informative)
elastic_model_vol_val <- glmnet(
  x = x_train_vol_val,
  y = y_train,
  alpha = 0.5,
  standardize = TRUE
)

plot(elastic_model_vol_val, xvar = "lambda", label = TRUE)
title("Elastic Net: Coefficient Paths")

# Save as high-resolution PNG
png("elastic_coeff_paths_highres.png", width = 2400, height = 1600, res = 300)


# Plot coefficient paths
plot(elastic_model_vol_val,
     xvar = "lambda",       # x-axis is log(Lambda)
     label = TRUE,          # label some variables
     col = rainbow(20),     # color variety if >10 variables
     lwd = 2,               # thicker lines for visibility
     xaxt = "s")            # show x-axis

dev.off()




# predict on test data and check performance lambda min
elastic_min_preds_vol_val <- predict(elastic_cv_vol_val, s = best_lambda_min_elastic_vol_val, newx = x_test_vol_val)

rmse_min_elastic_vol_val <- sqrt(mean((elastic_min_preds_vol_val - y_test)^2))
mae_min_elastic_vol_val <- mean(abs(elastic_min_preds_vol_val - y_test))
r2_min_elastic_vol_val <- 1 - sum((elastic_min_preds_vol_val - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda Min Test Set Evaluation (volume and valence):\n")
cat("RMSE:", round(rmse_min_elastic_vol_val, 4), "\n")
cat("MAE :", round(mae_min_elastic_vol_val, 4), "\n")
cat("R²  :", round(r2_min_elastic_vol_val, 4), "\n")

# check the coefficients lambda min
elastic_min_coefs_vol_val <- coef(elastic_cv_vol_val, s = best_lambda_min_elastic_vol_val)
print(elastic_min_coefs_vol_val)

# Convert sparse matrix to tidy table
elastic_min_table <- as.matrix(elastic_min_coefs_vol_val) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Coefficient = s1)

# Filter out zero coefficients (optional, to simplify the table)
elastic_min_table <- elastic_min_table[elastic_min_table$Coefficient != 0, ]

# View
print(elastic_min_table)

ft <- flextable(elastic_min_table)
ft <- autofit(ft)

# Export to Word document
save_as_docx(ft, path = "elastic_min_coefs.docx")

# Predict on test data and check performance lambda 1se
elastic_1se_preds_vol_val <- predict(elastic_cv_vol_val, s = best_lambda_1se_elastic_vol_val, newx = x_test_vol_val)

rmse_1se_elastic_vol_val <- sqrt(mean((elastic_1se_preds_vol_val - y_test)^2))
mae_1se_elastic_vol_val <- mean(abs(elastic_1se_preds_vol_val - y_test))
r2_1se_elastic_vol_val <- 1 - sum((elastic_1se_preds_vol_val - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda 1SE Test Set Evaluation (volume and valence):\n")
cat("RMSE:", round(rmse_1se_elastic_vol_val, 4), "\n")
cat("MAE :", round(mae_1se_elastic_vol_val, 4), "\n")
cat("R²  :", round(r2_1se_elastic_vol_val, 4), "\n")

# Check the coefficients lambda 1se
elastic_1se_coefs_vol_val <- coef(elastic_cv_vol_val, s = best_lambda_1se_elastic_vol_val)
print(elastic_1se_coefs_vol_val)



############################## ROBUSTNESS CHECK: NO OPENING LOCATIONS ##########################################
################## 1. without buzz 
set.seed(42)

elastic_cv_no_buzz_no_locs <- cv.glmnet(
  x = x_train_no_buzz_no_locs,
  y = y_train,
  alpha = 0.5,             # Elastic Net = 50% Ridge + 50% Lasso
  nfolds = 10,
  standardize = TRUE
)

# Best lambda Min
best_lambda_min_elastic_no_buzz_no_locs <- elastic_cv_no_buzz_no_locs$lambda.min
cat("Best lambda min (Elastic Net):", best_lambda_min_elastic_no_buzz_no_locs, "\n")

# Best lambda 1se
best_lambda_1se_elastic_no_buzz_no_locs <- elastic_cv_no_buzz_no_locs$lambda.1se
cat("Best lambda 1se (Elastic Net):", best_lambda_1se_elastic_no_buzz_no_locs, "\n")

# CV Error Curve
plot(elastic_cv_no_buzz_no_locs)
title("Elastic Net: Cross-Validation Curve")

# Coefficient Paths (optional, not alpha-specific but still informative)
elastic_model_no_buzz_no_locs <- glmnet(
  x = x_train_no_buzz_no_locs,
  y = y_train,
  alpha = 0.5,
  standardize = TRUE
)

plot(elastic_model_no_buzz_no_locs, xvar = "lambda", label = TRUE)
title("Elastic Net: Coefficient Paths")

# predict on test data and check performance with lambda min
elastic_min_preds_no_buzz_no_locs <- predict(elastic_cv_no_buzz_no_locs, s = best_lambda_min_elastic_no_buzz_no_locs, newx = x_test_no_buzz_no_locs)

rmse_min_elastic_no_buzz_no_locs <- sqrt(mean((elastic_min_preds_no_buzz_no_locs - y_test)^2))
mae_min_elastic_no_buzz_no_locs <- mean(abs(elastic_min_preds_no_buzz_no_locs - y_test))
r2_min_elastic_no_buzz_no_locs <- 1 - sum((elastic_min_preds_no_buzz_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda Min Test Set Evaluation no opening locations (no buzz):\n")
cat("RMSE:", round(rmse_min_elastic_no_buzz_no_locs, 4), "\n")
cat("MAE :", round(mae_min_elastic_no_buzz_no_locs, 4), "\n")
cat("R²  :", round(r2_min_elastic_no_buzz_no_locs, 4), "\n")

# check the coefficients lambda min
elastic_min_coefs_no_buzz_no_locs <- coef(elastic_cv_no_buzz_no_locs, s = best_lambda_min_elastic_no_buzz_no_locs)
print(elastic_min_coefs_no_buzz_no_locs)

# Predict on test data and check performance with lambda 1se
elastic_1se_preds_no_buzz_no_locs <- predict(elastic_cv_no_buzz_no_locs, s = best_lambda_1se_elastic_no_buzz_no_locs, newx = x_test_no_buzz_no_locs)

rmse_1se_elastic_no_buzz_no_locs <- sqrt(mean((elastic_1se_preds_no_buzz_no_locs - y_test)^2))
mae_1se_elastic_no_buzz_no_locs <- mean(abs(elastic_1se_preds_no_buzz_no_locs - y_test))
r2_1se_elastic_no_buzz_no_locs <- 1 - sum((elastic_1se_preds_no_buzz_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda 1SE Test Set Evaluation no opening locations (no buzz):\n")
cat("RMSE:", round(rmse_1se_elastic_no_buzz_no_locs, 4), "\n")
cat("MAE :", round(mae_1se_elastic_no_buzz_no_locs, 4), "\n")
cat("R²  :", round(r2_1se_elastic_no_buzz_no_locs, 4), "\n")

# Check the coefficients lambda 1se
elastic_1se_coefs_no_buzz_no_locs <- coef(elastic_cv_no_buzz_no_locs, s = best_lambda_1se_elastic_no_buzz_no_locs)
print(elastic_1se_coefs_no_buzz_no_locs)

################## 2. BUZZ VOLUME ONLY
set.seed(42)

elastic_cv_vol_only_no_locs <- cv.glmnet(
  x = x_train_vol_only_no_locs,
  y = y_train,
  alpha = 0.5,             # Elastic Net = 50% Ridge + 50% Lasso
  nfolds = 10,
  standardize = TRUE
)

# Best lambda min
best_lambda_min_elastic_vol_only_no_locs <- elastic_cv_vol_only_no_locs$lambda.min
cat("Best lambda min (Elastic Net):", best_lambda_min_elastic_vol_only_no_locs, "\n")

# Best lambda 1se
best_lambda_1se_elastic_vol_only_no_locs <- elastic_cv_vol_only_no_locs$lambda.1se
cat("Best lambda 1se (Elastic Net):", best_lambda_1se_elastic_vol_only_no_locs, "\n")

# CV Error Curve
plot(elastic_cv_vol_only_no_locs)
title("Elastic Net: Cross-Validation Curve")

# Coefficient Paths (optional, not alpha-specific but still informative)
elastic_model_vol_only_no_locs <- glmnet(
  x = x_train_vol_only_no_locs,
  y = y_train,
  alpha = 0.5,
  standardize = TRUE
)

plot(elastic_model_vol_only_no_locs, xvar = "lambda", label = TRUE)
title("Elastic Net: Coefficient Paths")

# predict on test data and check performance lambda min
elastic_min_preds_vol_only_no_locs <- predict(elastic_cv_vol_only_no_locs, s = best_lambda_min_elastic_vol_only_no_locs, newx = x_test_vol_only_no_locs)

rmse_min_elastic_vol_only_no_locs <- sqrt(mean((elastic_min_preds_vol_only_no_locs - y_test)^2))
mae_min_elastic_vol_only_no_locs <- mean(abs(elastic_min_preds_vol_only_no_locs - y_test))
r2_min_elastic_vol_only_no_locs <- 1 - sum((elastic_min_preds_vol_only_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda Min Test Set Evaluation no opening locations (volume only):\n")
cat("RMSE:", round(rmse_min_elastic_vol_only_no_locs, 4), "\n")
cat("MAE :", round(mae_min_elastic_vol_only_no_locs, 4), "\n")
cat("R²  :", round(r2_min_elastic_vol_only_no_locs, 4), "\n")

# check the coefficients lambda min
elastic_min_coefs_vol_only_no_locs <- coef(elastic_cv_vol_only_no_locs, s = best_lambda_min_elastic_vol_only_no_locs)
print(elastic_min_coefs_vol_only_no_locs)

# Predict on test data and check performance lambda 1se
elastic_1se_preds_vol_only_no_locs <- predict(elastic_cv_vol_only_no_locs, s = best_lambda_1se_elastic_vol_only_no_locs, newx = x_test_vol_only_no_locs)

rmse_1se_elastic_vol_only_no_locs <- sqrt(mean((elastic_1se_preds_vol_only_no_locs - y_test)^2))
mae_1se_elastic_vol_only_no_locs <- mean(abs(elastic_1se_preds_vol_only_no_locs - y_test))
r2_1se_elastic_vol_only_no_locs <- 1 - sum((elastic_1se_preds_vol_only_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda 1SE Test Set Evaluation no opening locations (volume only):\n")
cat("RMSE:", round(rmse_1se_elastic_vol_only_no_locs, 4), "\n")
cat("MAE :", round(mae_1se_elastic_vol_only_no_locs, 4), "\n")
cat("R²  :", round(r2_1se_elastic_vol_only_no_locs, 4), "\n")

# Check the coefficients lambda 1se
elastic_1se_coefs_vol_only_no_locs <- coef(elastic_cv_vol_only_no_locs, s = best_lambda_1se_elastic_vol_only_no_locs)
print(elastic_1se_coefs_vol_only_no_locs)

################## 3. BUZZ VOLUME AND VALENCE
set.seed(42)

elastic_cv_vol_val_no_locs <- cv.glmnet(
  x = x_train_vol_val_no_locs,
  y = y_train,
  alpha = 0.5,             # Elastic Net = 50% Ridge + 50% Lasso
  nfolds = 10,
  standardize = TRUE
)

# Best lambda min
best_lambda_min_elastic_vol_val_no_locs <- elastic_cv_vol_val_no_locs$lambda.min
cat("Best lambda min (Elastic Net):", best_lambda_min_elastic_vol_val_no_locs, "\n")

# Best lambda 1se
best_lambda_1se_elastic_vol_val_no_locs <- elastic_cv_vol_val_no_locs$lambda.1se
cat("Best lambda 1se (Elastic Net):", best_lambda_1se_elastic_vol_val_no_locs, "\n")

# CV Error Curve
plot(elastic_cv_vol_val_no_locs)
title("Elastic Net: Cross-Validation Curve")


# Coefficient Paths (optional, not alpha-specific but still informative)
elastic_model_vol_val_no_locs <- glmnet(
  x = x_train_vol_val_no_locs,
  y = y_train,
  alpha = 0.5,
  standardize = TRUE
)

plot(elastic_model_vol_val_no_locs, xvar = "lambda", label = TRUE)
title("Elastic Net: Coefficient Paths")

# predict on test data and check performance lambda min
elastic_min_preds_vol_val_no_locs <- predict(elastic_cv_vol_val_no_locs, s = best_lambda_min_elastic_vol_val_no_locs, newx = x_test_vol_val_no_locs)

rmse_min_elastic_vol_val_no_locs <- sqrt(mean((elastic_min_preds_vol_val_no_locs - y_test)^2))
mae_min_elastic_vol_val_no_locs <- mean(abs(elastic_min_preds_vol_val_no_locs - y_test))
r2_min_elastic_vol_val_no_locs <- 1 - sum((elastic_min_preds_vol_val_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda Min Test Set Evaluation no opening locations (volume and valence):\n")
cat("RMSE:", round(rmse_min_elastic_vol_val_no_locs, 4), "\n")
cat("MAE :", round(mae_min_elastic_vol_val_no_locs, 4), "\n")
cat("R²  :", round(r2_min_elastic_vol_val_no_locs, 4), "\n")

# check the coefficients lambda min
elastic_min_coefs_vol_val_no_locs <- coef(elastic_cv_vol_val_no_locs, s = best_lambda_min_elastic_vol_val_no_locs)
print(elastic_min_coefs_vol_val_no_locs)

# Predict on test data and check performance lambda 1se
elastic_1se_preds_vol_val_no_locs <- predict(elastic_cv_vol_val_no_locs, s = best_lambda_1se_elastic_vol_val_no_locs, newx = x_test_vol_val_no_locs)

rmse_1se_elastic_vol_val_no_locs <- sqrt(mean((elastic_1se_preds_vol_val_no_locs - y_test)^2))
mae_1se_elastic_vol_val_no_locs <- mean(abs(elastic_1se_preds_vol_val_no_locs - y_test))
r2_1se_elastic_vol_val_no_locs <- 1 - sum((elastic_1se_preds_vol_val_no_locs - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Elastic Net Lambda 1SE Test Set Evaluation no opening locations (volume and valence):\n")
cat("RMSE:", round(rmse_1se_elastic_vol_val_no_locs, 4), "\n")
cat("MAE :", round(mae_1se_elastic_vol_val_no_locs, 4), "\n")
cat("R²  :", round(r2_1se_elastic_vol_val_no_locs, 4), "\n")

# Check the coefficients lambda 1se
elastic_1se_coefs_vol_val_no_locs <- coef(elastic_cv_vol_val_no_locs, s = best_lambda_1se_elastic_vol_val_no_locs)
print(elastic_1se_coefs_vol_val_no_locs)

###### variable importance using absolute coefficients
# Convert coefficients to data frame
elastic_coefs <- as.matrix(elastic_min_coefs_vol_val_no_locs)

elastic_df <- data.frame(
  Variable = rownames(elastic_coefs),
  Coefficient = as.numeric(elastic_coefs)
)

# Remove intercept
elastic_df <- elastic_df[elastic_df$Variable != "(Intercept)", ]

# Filter non-zero coefficients
elastic_df <- elastic_df[elastic_df$Coefficient != 0, ]

# Calculate importance
elastic_df$Importance <- abs(elastic_df$Coefficient)

# Select top 10 variables
top_elastic <- elastic_df[order(-elastic_df$Importance), ][1:10, ]

# Plot
ggplot(top_elastic, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Variable Importances (Elastic Net, Lambda Min)",
    x = "Variable",
    y = "Absolute Coefficient (Importance)"
  ) +
  theme_minimal()


############ let's tune the alpha and see if the model performance improves 
# Define alpha values to test (from Ridge to Lasso)
alpha_grid <- seq(0, 1, by = 0.01)

# Store results
cv_results <- list()
mean_cv_errors <- c()
best_lambdas <- c()

set.seed(42)
for (a in alpha_grid) {
  cv_fit <- cv.glmnet(
    x = x_train_vol_val,
    y = y_train,
    alpha = a,
    nfolds = 10,
    standardize = TRUE
  )
  
  cv_results[[as.character(a)]] <- cv_fit
  mean_cv_errors <- c(mean_cv_errors, min(cv_fit$cvm))
  best_lambdas <- c(best_lambdas, cv_fit$lambda.min)
}

# Find best alpha
best_alpha <- alpha_grid[which.min(mean_cv_errors)]
best_model <- cv_results[[as.character(best_alpha)]]

cat("Best alpha:", best_alpha, "\n")
cat("Best lambda:", best_model$lambda.min, "\n")
cat("Lowest CV error (MSE):", min(mean_cv_errors), "\n")

# Step 1: Predict on test data using best model (with best alpha and lambda)
elastic_preds_best <- predict(best_model, s = best_model$lambda.min, newx = x_test_vol_val)

# Step 2: Compute performance metrics
rmse_best <- sqrt(mean((elastic_preds_best - y_test)^2))
mae_best <- mean(abs(elastic_preds_best - y_test))
r2_best <- 1 - sum((elastic_preds_best - y_test)^2) / sum((y_test - mean(y_test))^2)

# Step 3: Print results
cat("Elastic Net (Tuned Alpha) Test Set Evaluation:\n")
cat("RMSE:", round(rmse_best, 4), "\n")
cat("MAE :", round(mae_best, 4), "\n")
cat("R²  :", round(r2_best, 4), "\n")

# Step 4: Extract and print coefficients
elastic_best_coefs <- coef(best_model, s = best_model$lambda.min)
print(elastic_best_coefs)



######### arranging all variable importance plots for the thesis document and saving them ############
custom_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Helper function for coefficient-based models
plot_coeff_importance <- function(df, title_text, fill_color = "steelblue") {
  ggplot(df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = fill_color) +
    coord_flip() +
    labs(title = title_text, x = "Variable", y = "Absolute Coefficient") +
    custom_theme
}

# Linear Regression
lm_plot <- plot_coeff_importance(
  coef_df[order(-coef_df$Abs_Coefficient), ][1:10, ] %>% 
    rename(Importance = Abs_Coefficient),
  "Linear Regression (Top 10)"
)

ridge_plot <- plot_coeff_importance(
  coefs_df[order(-coefs_df$Importance), ][1:10, ],
  "Ridge Regression (Top 10)"
)

lasso_plot <- plot_coeff_importance(
  coefs_df[order(-coefs_df$Importance), ][1:10, ],
  "Lasso Regression (Top 10)",
  fill_color = "orange"
)

elastic_plot <- plot_coeff_importance(
  elastic_df[order(-elastic_df$Importance), ][1:10, ],
  "Elastic Net (Top 10)"
)

rf_plot <- plot_coeff_importance(
  var_imp_df[order(-var_imp_df$Importance), ][1:10, ],
  "Random Forest (Top 10)",
  fill_color = "darkgreen"
)

xgb_df <- importance_matrix[1:10, ]
xgb_df$Feature <- factor(xgb_df$Feature, levels = rev(xgb_df$Feature))  # for proper order

xgb_plot <- ggplot(xgb_df, aes(x = Feature, y = Gain)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "XGBoost (Top 10)",
    x = "Variable",
    y = "Relative Importance (Gain)"
  ) +
  custom_theme

# Combine plots into a grid: 2 rows x 3 columns
combined_plot <- grid.arrange(
  lm_plot, ridge_plot, lasso_plot,
  elastic_plot, rf_plot, xgb_plot,
  ncol = 3,
  top = textGrob("Variable Importance Across Models (No Opening Locations)", gp = gpar(fontsize = 16, fontface = "bold"))
)

# Save high-res output
ggsave("variable_importance_grid.png", plot = combined_plot, width = 14, height = 9, dpi = 400)



# Apply custom theme to each ggplot
custom_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# Linear Regression Plot
p1 <- ggplot(top_coef_lm_df, aes(x = reorder(Variable, Abs_Coefficient), y = Abs_Coefficient)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Linear Regression", x = "Variable", y = "Abs Coefficient") +
  theme_minimal()

# Ridge Regression Plot
p2 <- ggplot(top_ridge_coefs, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Ridge Regression", x = "Variable", y = "Abs Coefficient (λ min)") +
  theme_minimal()

# Lasso Regression Plot
p3 <- ggplot(top_coefs_lasso, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Lasso Regression", x = "Variable", y = "Abs Coefficient (λ min)") +
  theme_minimal()

# Elastic Net Plot
p4 <- ggplot(top_elastic, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "mediumseagreen") +
  coord_flip() +
  labs(title = "Elastic Net", x = "Variable", y = "Abs Coefficient (λ min)") +
  theme_minimal()

# Random Forest Plot
p5 <- ggplot(top_vars, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Random Forest", x = "Variable", y = "Permutation Importance") +
  theme_minimal()

# XGBoost Plot
# Prepare top 10 features from XGBoost importance matrix
xgb_top <- importance_matrix[1:10, ]
xgb_top$Feature <- factor(xgb_top$Feature, levels = rev(xgb_top$Feature))

# Create ggplot version of XGBoost plot
p6 <- ggplot(xgb_top, aes(x = Feature, y = Gain)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "XGBoost",
    x = "Variable",
    y = "Gain Importance"
  ) +
  custom_theme

# Apply theme to each plot
p1 <- p1 + custom_theme
p2 <- p2 + custom_theme
p3 <- p3 + custom_theme
p4 <- p4 + custom_theme
p5 <- p5 + custom_theme

# Arrange plots in a grid
final_grid <- grid.arrange(
  p1, p2, p3,
  p4, p5, p6,
  ncol = 3,
  top = textGrob(
    "Variable Importance Across Models (No Opening Locations)",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)



# Save high-res
ggsave("variable_importance_grid.png", plot = final_grid, width = 14, height = 10, dpi = 300)


######################## Bar Chart For Improvement In Average Absolute Error For Each Model ######################
# Define your data
mae_data <- data.frame(
  Model = rep(c("Linear Regression", "Random Forest", "XGBoost", "Ridge", "Lasso", "Elastic Net"), each = 2),
  Buzz = rep(c("Without Buzz", "With Buzz"), times = 6),
  MAE_Percent = c(
    133.5, 120,
    123, 107,
    132, 110,
    126, 109,
    129, 119,
    128, 119
  )
)

# plot the bar chart
ggplot(mae_data, aes(x = Model, y = MAE_Percent, fill = Buzz)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(MAE_Percent, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Impact of Buzz Variables on Predictive Accuracy",
    subtitle = "Average Absolute Prediction Error Before and After Adding Buzz Variables",
    x = "Model",
    y = "Mean Absolute Error (% of true revenue)",
    fill = "Model Version"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),        # Centered
    plot.subtitle = element_text(hjust = 0.5),                                # Centered
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

# save the plot
# Save as high-resolution PNG
ggsave("mae_buzz_comparison.png",
       width = 10,           # Width in inches
       height = 6,           # Height in inches
       dpi = 300)            # High resolution (300 DPI)

############################ 5 MOVIE PREDICTION COMPARISON #################################################
# Standardize predictor inputs: log_star_power_count and opening_locs
test_data <- test_data %>%
  mutate(
    z_star_power = scale(log_star_power_count)[, 1],
    z_opening_locs = scale(opening_locs)[, 1]
  )


# Compute expected performance as sum of standardized star power and distribution scale
test_data <- test_data %>%
  mutate(
    expected_score = z_star_power + z_opening_locs
  )


# Standardize actual performance (z-score of log opening weekend revenue)
test_data <- test_data %>%
  mutate(
    z_actual_rev = scale(log_opening_weekend_eur)[, 1]
  )

# Deviation shows whether movie outperformed or underperformed vs. expected score
test_data <- test_data %>%
  mutate(
    deviation = z_actual_rev - expected_score
  )

# High Performer – highest opening revenue
high_perf <- test_data %>%
  arrange(desc(opening_weekend_eur)) %>%
  slice(1) %>%
  mutate(type = "High Performer")

# Mid Performer – closest to median actual z-score
mid_perf <- test_data %>%
  arrange(abs(z_actual_rev - median(z_actual_rev, na.rm = TRUE))) %>%
  dplyr::slice(1) %>%
  mutate(type = "Mid Performer")

# Low Performer – lowest opening revenue
low_perf <- test_data %>%
  arrange(opening_weekend_eur) %>%
  dplyr::slice(1) %>%
  mutate(type = "Low Performer")

# Surprising Overachiever – highest positive deviation
surprising_over <- test_data %>%
  arrange(desc(deviation)) %>%
  dplyr::slice(1) %>%
  mutate(type = "Surprising Overachiever")

# Surprising Underperformer – lowest deviation (excluding High Performer)
surprising_under <- test_data %>%
  filter(!(title %in% high_perf$title)) %>%
  arrange(deviation) %>%
  dplyr::slice(1) %>%
  mutate(type = "Surprising Underperformer")

# Drop release_month from all selected rows
high_perf <- dplyr::select(high_perf, -release_month)
mid_perf <- dplyr::select(mid_perf, -release_month)
low_perf <- dplyr::select(low_perf, -release_month)
surprising_over <- dplyr::select(surprising_over, -release_month)
surprising_under <- dplyr::select(surprising_under, -release_month)

# Drop release_day_name from all selected rows
high_perf <- dplyr::select(high_perf, -release_day_name)
mid_perf <- dplyr::select(mid_perf, -release_day_name)
low_perf <- dplyr::select(low_perf, -release_day_name)
surprising_over <- dplyr::select(surprising_over, -release_day_name)
surprising_under <- dplyr::select(surprising_under, -release_day_name)



# Now combine
movie_selection <- bind_rows(
  high_perf, mid_perf, low_perf, surprising_over, surprising_under
)



# Step 6: Combine selections
movie_selection <- bind_rows(
  high_perf, mid_perf, low_perf, surprising_over, surprising_under
)

# Step 7: View relevant columns
movie_selection %>%
  dplyr::select(
    type, title, opening_weekend_eur,
    log_star_power_count, opening_locs,
    expected_score, z_actual_rev, deviation
  )

###### predicting the opening weekend revenues of these 5 movies with the best performing versions of each model
movie_indices <- which(test_data$title %in% movie_selection$title)
print(movie_indices)

# Match titles with correct row numbers
movie_indices_named <- c(
  "Avengers: Endgame" = 7,
  "Paws of Fury: The Legend of Hank" = 72,
  "Echo Boomers" = 41,
  "Creed III" = 71,
  "Transformers One" = 109
)

# Get ordered indices in same order as movie_selection$title
ordered_indices <- movie_indices_named[movie_selection$title]


# Extract model predictions for the 5 selected movies across different models
xgb_preds_df <- data.frame(
  Title = movie_selection$title,
  `Actual (Euros)` = test_data$opening_weekend_eur[ordered_indices],
  `XGBoost (No Buzz)` = exp(preds_xgb_final_no_buzz[ordered_indices]),
  `Elastic Net (Buzz Volume + Valence)` = exp(elastic_min_preds_vol_val[ordered_indices]),
  `Ridge (Buzz Volume + Valence)` = exp(ridge_min_preds_vol_val[ordered_indices]),
  `Lasso (Buzz Volume + Valence)` = exp(lasso_min_preds_vol_val[ordered_indices]),
  `Random Forest (Buzz Volume Only)` = exp(predictions_rf_volume_only[ordered_indices])
)

# Add the type column from movie_selection
xgb_preds_df$type <- movie_selection$type


# give proper column names 
colnames(xgb_preds_df) <- c(
  "Title",
  "Type",
  "Actual (Euros)",
  "XGBoost (No Buzz)",
  "Elastic Net (Buzz Volume + Valence)",
  "Ridge (Buzz Volume + Valence)",
  "Lasso (Buzz Volume + Valence)",
  "Random Forest (Buzz Volume Only)"
)


# Round for readability
xgb_preds_df <- xgb_preds_df %>%
  mutate(across(where(is.numeric), ~ round(., 0)))

# View the final comparison table
print(xgb_preds_df)

# Reorder columns for readability (optional)
xgb_preds_df <- xgb_preds_df %>%
  dplyr::select(Title, everything())

# View updated dataframe
print(xgb_preds_df)


# create a table of these 5 movies with certain deterministic features
# Step 1: Subset the relevant columns for the selected movies
buzz_summary_sample_movies <- test_data[ordered_indices, ] %>%
  dplyr::select(
    "Title" = title,
    "Opening Weekend (Euros)" = opening_weekend_eur,
    "Star Power Count" = star_power_count,
    "Number of Pre-release Comments" = n_comments,
    "Proportion of Positive Comments" = prop_pos,
    "Proportion of Negative Comments" = prop_neg,
    "Number of Opening Locations" = opening_locs, 
    "Director Power" = director_power,
    "Distributor Power" = distributor_power
  )

# View the table
print(buzz_summary_sample_movies)



############################ CHECKING POTENTIAL ENDOGENIETY CONCERNS FOR OPENING LOCATIONS ########################
opening_locs_model <- lm(opening_locs ~ 
                             log_n_comments + 
                             prop_pos + 
                             prop_neg,    # also assuming rating is a factor
                           data = super_duper_final_meta_data_with_sent)


summary(opening_locs_model)

# Tidy the model output
tidy_opening_locs <- tidy(opening_locs_model)

# Round numeric columns
tidy_opening_locs <- tidy_opening_locs %>%
  mutate(across(where(is.numeric), round, 3))

# Add significance stars
tidy_opening_locs <- tidy_opening_locs %>%
  rename(p_value = `p.value`) %>%
  mutate(Significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.1 ~ ".",
    TRUE ~ ""
  ))

# Rename columns for table display
colnames(tidy_opening_locs) <- c("Variable", "Estimate", "Std. Error", "t value", "p-value", "Significance")

# Create flextable
ft_opening_locs <- flextable(tidy_opening_locs) %>%
  set_caption("Table A1: Linear Regression Predicting Number of Opening Locations") %>%
  bold(j = 1, part = "body") %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  fontsize(size = 10, part = "all") %>%
  padding(padding.top = 4, padding.bottom = 4) %>%
  border_remove() %>%
  border_outer() %>%
  border_inner()

# Add to Word document
doc <- read_docx() %>%
  body_add_par("Appendix A", style = "heading 1") %>%
  body_add_par("Linear Regression Summary: Predicting Opening Locations", style = "heading 2") %>%
  body_add_flextable(ft_opening_locs) %>%
  body_add_par("* p < 0.05, ** p < 0.01, *** p < 0.001", style = "Normal")

# Save the Word document
print(doc, target = "opening_locations_regression_output.docx")
