library(rvest)
library(dplyr)
library(purrr)

######### 1. Distributors Data ############################
############# 2024 ####################
# Step 1: Define the URL
url_2024 <- "https://www.the-numbers.com/market/2024/distributors"

# Step 2: Read the page
page_2024 <- read_html(url_2024)

# Step 3: Extract all tables and select the second one
tables_2024 <- page_2024 %>% html_elements("table")
distributors_2024 <- tables_2024[[2]] %>% html_table()

# Step 4: Clean the table
colnames(distributors_2024) <- c("Rank", "Distributor", "Movies", "Gross_2024", "Tickets", "Share")

# Step 5: Clean the numeric values
distributors_2024 <- distributors_2024 %>%
  mutate(
    Gross_2024 = as.numeric(gsub("[$,]", "", Gross_2024)),
    Tickets = as.numeric(gsub(",", "", Tickets)),
    Share = as.numeric(gsub("%", "", Share)),
    Year = 2024
  )

# Step 6: Preview
head(distributors_2024)

# save the data
write.csv(distributors_2024, "distributors_2024.csv", row.names = FALSE)

################## 2023 ##################################
# Step 1: Define the URL
url_2023 <- "https://www.the-numbers.com/market/2023/distributors"

# Step 2: Read the page
page_2023 <- read_html(url_2023)

# Step 3: Extract all tables and select the second one
tables_2023 <- page_2023 %>% html_elements("table")
distributors_2023 <- tables_2023[[2]] %>% html_table()

# Step 4: Clean the table
colnames(distributors_2023) <- c("Rank", "Distributor", "Movies", "Gross_2023", "Tickets", "Share")

# Step 5: Clean the numeric values
distributors_2023 <- distributors_2023 %>%
  mutate(
    Gross_2023 = as.numeric(gsub("[$,]", "", Gross_2023)),
    Tickets = as.numeric(gsub(",", "", Tickets)),
    Share = as.numeric(gsub("%", "", Share)),
    Year = 2023
  )

# Step 6: Preview
head(distributors_2023)

# save the data
write.csv(distributors_2023, "distributors_2023.csv", row.names = FALSE)


############## 2019:2022 ###################################################
# Function to scrape distributor data by year
scrape_distributors_year <- function(year) {
  url <- paste0("https://www.the-numbers.com/market/", year, "/distributors")
  page <- read_html(url)
  
  table <- page %>%
    html_elements("table") %>%
    .[[2]] %>%
    html_table()
  
  # Rename columns generically first
  colnames(table) <- c("Rank", "Distributor", "Movies", "Gross", "Tickets", "Share")
  
  # Clean and tag with year
  table_clean <- table %>%
    mutate(
      Gross = as.numeric(gsub("[$,]", "", Gross)),
      Tickets = as.numeric(gsub(",", "", Tickets)),
      Share = as.numeric(gsub("%", "", Share)),
      Year = year
    )
  
  return(table_clean)
}

# Years to scrape
years <- 2019:2022

# Scrape and save each year
for (yr in years) {
  message("Scraping: ", yr)
  dist_data <- scrape_distributors_year(yr)
  write.csv(dist_data, paste0("distributors_", yr, ".csv"), row.names = FALSE)
}

# checking the scraped data 
distributors_2019 <- read.csv("distributors_2019.csv")
distributors_2020 <- read.csv("distributors_2020.csv")
distributors_2021 <- read.csv("distributors_2021.csv")
distributors_2022 <- read.csv("distributors_2022.csv")


################ 2. Star power ################################
# Function to scrape one page of actor data
scrape_actor_page <- function(url) {
  page <- read_html(url)
  
  rows <- page %>% html_elements("table tr")  # all table rows
  
  data <- rows %>%
    # Skip header
    .[-1] %>%
    map_dfr(function(row) {
      cols <- row %>% html_elements("td") %>% html_text2()
      if (length(cols) == 5) {
        tibble(
          Rank = as.integer(cols[1]),
          Name = cols[2],
          Worldwide_Box_Office = as.numeric(gsub("[$,]", "", cols[3])),
          Movies = as.integer(gsub(",", "", cols[4])),
          Average = as.numeric(gsub("[$,]", "", cols[5]))
        )
      } else {
        NULL  # skip non-data rows
      }
    })
  
  return(data)
}

# List of start ranks
ranks <- seq(1, 1001, by = 100)

# Build full URLs
urls <- c(
  "https://www.the-numbers.com/box-office-star-records/worldwide/lifetime-acting/top-grossing-leading-stars",
  paste0("https://www.the-numbers.com/box-office-star-records/worldwide/lifetime-acting/top-grossing-leading-stars/", ranks[-1])
)

# Scrape each page
actor_data_list <- lapply(urls, scrape_actor_page)

# Combine all
top_actors <- bind_rows(actor_data_list)

# Identify only the first NA row after rank 999 and assign it 1000
top_actors$Rank[which(is.na(top_actors$Rank))[1]] <- 1000

# Then drop any rows with Rank still NA (i.e. 1001–1100)
top_actors <- top_actors %>%
  filter(Rank <= 1000)

# Step 3: View the result
head(top_actors)
tail(top_actors)



# Save
write.csv(top_actors, "top_1000_grossing_actors.csv", row.names = FALSE)


#################### 3. Director ranking ###################################
# Function to scrape a single director ranking page
scrape_director_page <- function(start_rank) {
  if (start_rank == 1) {
    url <- "https://www.the-numbers.com/box-office-star-records/domestic/lifetime-specific-technical-role/director"
  } else {
    url <- paste0("https://www.the-numbers.com/box-office-star-records/domestic/lifetime-specific-technical-role/director/", start_rank)
  }
  
  message("Scraping: ", url)
  
  page <- read_html(url)
  rows <- page %>% html_elements("table tr")  # select table rows
  
  # Skip header and map over rows
  data <- rows[-1] %>%
    map_dfr(function(row) {
      cols <- row %>% html_elements("td") %>% html_text2()
      if (length(cols) == 5) {
        tibble(
          Rank = as.integer(cols[1]),
          Name = cols[2],
          Domestic_Box_Office = as.numeric(gsub("[$,]", "", cols[3])),
          Movies = as.integer(gsub(",", "", cols[4])),
          Average = as.numeric(gsub("[$,]", "", cols[5]))
        )
      } else {
        NULL
      }
    })
  
  return(data)
}

# Define start ranks (1, 101, ..., 1001)
start_ranks <- c(1, seq(101, 1001, 100))

# Run the scraping
director_data_list <- lapply(start_ranks, scrape_director_page)

# Combine and clean
top_directors <- bind_rows(director_data_list) %>%
  arrange(Rank) %>%
  slice(1:1000)

# Save the result
write.csv(top_directors, "top_1000_grossing_directors.csv", row.names = FALSE)

# Preview
View(top_directors)



