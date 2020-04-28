
wd <- "~/Documents/School/College/Junior Year/Econ 126/OpEd_1_State_Covid_Response/"
data_dir <- paste0(wd, "data/")
library(readr)
library(tibble)
library(dplyr)
library(purrr)

state_map <- tibble::tibble("state_name" = c(state.name, "District of Columbia"),
                            "state_abb" = c(state.abb, "DC"))

getUSCases <- function(active = FALSE) {
  if (active) {
    # removes cases that are more than 1 month old
    # getOldCases <- function(state_, date_) {
    #   old_cases <- us_cases %>%
    #     dplyr::filter(state_name == state_ & date == (as.Date(date_) - 30)) %>%
    #     dplyr::pull(cases) %>%
    #     sum()
    #   deaths <- us_cases %>%
    #     dplyr::filter(state_name == state_ & date == as.Date(date_)) %>%
    #     dplyr::pull(deaths)
    #   return (old_cases + deaths)
    # }
    # old_cases <- mapply(getOldCases, us_cases$state_name, us_cases$date)
    # us_cases[["active_cases"]] <- us_cases$cases - old_cases
    return (readr::read_csv(paste0(data_dir, "clean_cases.csv")))
  }
  cases <- readr::read_csv(paste0(data_dir, "us-counties.csv"))
  # https://www.kaggle.com/fireballbyedimyrnmom/us-counties-covid-19-dataset
  us_cases <- cases %>%
    dplyr::group_by(date, state) %>%
    dplyr::summarize(cases = sum(cases), deaths = sum(deaths)) %>%
    dplyr::left_join(state_map, by = c("state" = "state_name")) %>%
    dplyr::rename("state_name" = "state")
}

getRts <- function() {
  rts <- readr::read_csv(paste0(data_dir, "Rt_by_state.csv"))
  # https://rt.live
  rts <- rts %>%
    dplyr::left_join(state_map, by = c("region" = "state_abb")) %>%
    dplyr::rename("state_abb" = "region")
}

getUSBeds <- function() {
  hosp_beds <- readr::read_csv(paste0(data_dir, "hospital_beds_global_regional_v1.csv"))
  # https://www.kaggle.com/ikiulian/global-hospital-beds-capacity-for-covid19
  # all entries in the 'bed' column are beds per 1000 population 
  # all(us_beds$measure == '1000HAB')
  us_beds <- hosp_beds %>%
    dplyr::filter(country == "US" & !is.na(state)) %>%
    dplyr::mutate(n_beds = round(beds * population / 1000)) %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(total_beds = sum(n_beds)) %>%
    dplyr::rename("state_abb" = state) %>%
    dplyr::left_join(state_map, by = "state_abb")
}

getFirstSAH <- function() {
  regulations <- readr::read_delim(paste0(data_dir, "States Regulations.txt"), delim = "\n")
  # https://www.kff.org/report-section/state-data-and-policy-actions-to-address-coronavirus-sources/
  entries <- c(names(regulations), regulations[[1]])
  states <- rep(NA_character_, length(entries))
  for (i in 1:length(entries)) {
    entry <- entries[[i]]
    if (entry == toupper(entry)) {
      state <- entry
    }
    states[[i]] <- state
  }
  sah_orders <- tibble::tibble("entry" = entries, "state" = states) %>%
    dplyr::filter(grepl("stay at home", tolower(entry)))
  extractSAH <- function(entry_text) {
    dates <- stringr::str_extract_all(entry_text, "\\d/\\d{2}|\\d/\\d")[[1]]
    valid_dates <- dates[grepl("^3|^4", dates)]
    if (length(valid_dates) == 1) {
      return (valid_dates)
    }
    first_alpha_index <- stringr::str_locate(entry_text, "[:alpha:]")
    alpha <- substr(entry_text, first_alpha_index, nchar(entry_text))
    alpha_split <- strsplit(alpha, split = ",")[[1]]
    sah_date <- dates[grep("stay at home", tolower(alpha_split))]
    return (sah_date)
  }
  sah_orders[["date"]] <- purrr::map_chr(sah_orders$entry, extractSAH)
  first_sah <- sah_orders %>%
    dplyr::group_by(state) %>%
    dplyr::summarize(first_sah_date = dplyr::first(date)) %>%
    dplyr::mutate("state_name_lower" = tolower(state)) %>%
    dplyr::left_join(state_map %>%
                       dplyr::mutate(state_name_lower = tolower(state_name)),
                     by = "state_name_lower") %>%
    dplyr::select(-state_name_lower)
}

getPopData <- function() {
  pop_data <- readr::read_csv(paste0(data_dir, "pop_by_age_by_state.csv"))
  # https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html
  # we'll be doing the 2018 estimate
  state_totals <- pop_data %>%
    dplyr::filter(AGE == 999 & SEX == 0) %>%
    dplyr::select(NAME, POPEST2018_CIV) %>%
    dplyr::rename("total_pop" = "POPEST2018_CIV")
  state_by_age <- pop_data %>%
    dplyr::filter(AGE != 999 & SEX == 0 & NAME %in% state_map$state_name) %>%
    dplyr::left_join(state_totals, by = "NAME") %>%
    dplyr::select("NAME", "AGE", "POPEST2018_CIV", "total_pop") %>%
    dplyr::rename("state_name" = "NAME", "age" = "AGE", "pop" = "POPEST2018_CIV") %>%
    dplyr::mutate(group = (age %/% 10) * 10) %>%
    dplyr::group_by(state_name, group) %>%
    dplyr::summarize(group_pop = sum(pop), state_pop = first(total_pop)) %>%
    dplyr::mutate(pop_prop = group_pop / state_pop) %>%
    dplyr::left_join(state_map, by = c("state_name"))
}
