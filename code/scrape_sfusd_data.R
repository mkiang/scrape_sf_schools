## Imports ----
library(rvest)
library(tidyverse)
library(fs)
library(here)
library(janitor)

## Constants ----
## Change to TRUE if you want to overwrite the cached data
FORCE_RERUN <- TRUE

## Helper functions ----

## Scrapes the website to get the URL of each school-specific website
## Note that I set a generous timer (~15 seconds on average) so this
## can take a while. You only need to run it once (unless the website changes)
## so be kind and don't lower the max_sleep.
return_school_urls <- function(max_sleep = 30) {
    holder <- NULL
    
    ## URL of each search page containing school URLS
    pages <- c(
        paste0(
            "https://www.sfusd.edu/schools-search?",
            "field_school_type_target_id%5B318%5D=318"
        ),
        paste0(
            "https://www.sfusd.edu/schools-search?",
            "field_schoolname_value=&",
            "field_school_type_target_id%5B318%5D=318&",
            "field_has_before_school_value=All&",
            "field_has_after_school_value=All&",
            "field_school_uniforms_required_value=All&",
            "page=",
            1:7
        )
    )
    
    ## Loop and extract each school URL
    for (u in pages) {
        Sys.sleep(stats::runif(1, 0, max_sleep))
        
        temp_x <- xml2::read_html(u) %>%
            rvest::html_nodes(".school-slat_title a") %>%
            rvest::html_attr("href") %>%
            xml2::url_absolute("https://www.sfusd.edu")
        
        holder <- c(holder, temp_x)
    }
    
    holder
}

## Given a school URL, reads the HTML and extracts things we want
parse_school_website <- function(url) {
    ## Read the HTML
    raw_html <- url %>%
        xml2::read_html()
    
    ## Read each list item (eg enrollment, principal, etc.)
    x <- raw_html %>%
        rvest::html_nodes(".field-item") %>%
        rvest::html_text(trim = TRUE)
    
    ## Read the school name
    hero <- raw_html %>%
        rvest::html_nodes(".hero_title") %>%
        rvest::html_text(trim = TRUE)
    
    ## Extract the last item in every list pair (these are values)
    parsed_values <- c(hero,
                       lapply(strsplit(x, "\n"), function(x)
                           trimws(x[[NROW(x)]])))
    
    ## Extract the first item in every list pair (these are column names)
    value_labels <-
        janitor::make_clean_names(unlist(lapply(strsplit(x, "\n"), function(x)
            x[[1]])))
    
    ## The "about" column changes per school so we rename it
    value_labels[grep("about_", value_labels, fixed = TRUE)] <-
        "about"
    
    names(parsed_values) <- c("school_name", value_labels)
    
    ## Remove random misnamed columns
    dplyr::as_tibble(parsed_values) %>%
        dplyr::select(
            -dplyr::starts_with("x2018_"),
            -dplyr::starts_with("x2017_"),
            -dplyr::starts_with("x018"),
            -dplyr::starts_with("x20108_"),
            -dplyr::starts_with("x201_8_"),
            -dplyr::starts_with("x2019")
        )
}

## Load school URLs ----
if (!fs::file_exists(here::here("data", "school_urls.RDS")) |
    FORCE_RERUN) {
    school_urls <- return_school_urls()
    saveRDS(school_urls, here::here("data", "school_urls.RDS"))
} else {
    school_urls <- readRDS(here::here("data", "school_urls.RDS"))
}

## Loop through and scrap each school ----
if (!fs::file_exists(here::here("data", "sf_elementary_schools.csv")) |
    FORCE_RERUN) {
    schools_df <- NULL
    for (u in school_urls) {
        schools_df <- dplyr::bind_rows(schools_df, parse_school_website(u))
        Sys.sleep(stats::runif(1, 0, 10))
    }
    readr::write_csv(schools_df,
                     here::here("data", "sf_elementary_schools.csv"))
}
