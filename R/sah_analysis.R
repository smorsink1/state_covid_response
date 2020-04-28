
wd <- "~/Documents/School/College/Junior Year/Econ 126/OpEd_1_State_Covid_Response/"
data_dir <- paste0(wd, "data/")
source(paste0(wd, "state_risk_scores.R"))
library(ggplot2)

sah <- getFirstSAH()
sah$first_sah_date <- as.Date(sah$first_sah_date, format = "%m/%d")
# risk_scores_sah <- purrr::map2_dfr(sah$state_abb, sah$first_sah_date, getRiskScores)
# readr::write_csv(risk_scores_sah, paste0(wd, "risk_scores_at_sah.csv"))

risk_scores <- readr::read_csv(paste0(wd, "risk_scores.csv"))
risk_scores_at_sah <- readr::read_csv(paste0(wd, "risk_scores_at_sah.csv"))

risk_scores_upd <- risk_scores %>%
  dplyr::left_join(sah, by = "state_abb") %>%
  dplyr::mutate(sah_in_effect = as.integer(date >= first_sah_date)) %>%
  dplyr::mutate(mean_inv = abs(mean - 366)) %>%
  dplyr::mutate(days_before_sah = first_sah_date - date) %>%
  dplyr::mutate(high_risk = mean <= 100)

#### Num High Risk Days Before SAH ####

getNHRDBS <- function(state_abbrev) {
  state_specific <- risk_scores_upd[risk_scores_upd$state_abb == state_abbrev, ]
  sum(state_specific$sah_in_effect == 0 & state_specific$high_risk == TRUE)
}

n_hrd_before_sah <- sapply(unique(sah$state_abb), getNHRDBS)
hrd_before_sah <- tibble::tibble("state_abb" = unique(sah$state_abb),
                                 "hr_days_pre_sah" = n_hrd_before_sah)

#### Merging with Party Data ####

getGovParty <- function() {
  gov_party <- readxl::read_excel(paste0(data_dir, "gov_party.xlsx"))
  # https://ballotpedia.org/Partisan_composition_of_governors
  state <- strsplit(gov_party$office, split = "of ") %>%
    purrr::map(2) %>%
    unlist()
  gov_party <- gov_party %>%
    dplyr::mutate(state_name = state) %>%
    dplyr::filter(state_name %in% state_map$state_name) %>%
    dplyr::mutate(party = stringr::str_trim(party)) %>%
    dplyr::left_join(state_map, by = "state_name")
  dc_row <- tibble::tibble("office" = "Mayor of District of Columbia",
                           "person" = "Muriel Bowser",
                           "party" = "Democratic",
                           "date_in_office" = "1/2/2015",
                           "state_name" = "District of Columbia",
                           "state_abb" = "DC")
  gov_party <- dplyr::bind_rows(gov_party, dc_row)
}

gov_party <- getGovParty()

hrd_before_sah <- hrd_before_sah %>%
  dplyr::left_join(gov_party, by = "state_abb") %>%
  dplyr::left_join(sah, by = c("state_abb", "state_name"))

#### Plotting ####

# We'll basically be using number of high risk days before SAH order 
# to measure the response to the COVID
# Note: smaller number of days isn't necessarily better (but it probably is)

## Date vs NHRDBS

date_plot <- ggplot(data = hrd_before_sah, aes(x = first_sah_date, y = hr_days_pre_sah)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Date of SAH Order") + ylab("Number of High-Risk Days Before SAH Order") +
  ggtitle("Later SAH Orders Were Overdue")

summary(lm(hr_days_pre_sah ~ first_sah_date, data = hrd_before_sah))
# significantly positive correlation, with a coefficient of 0.913 on slope:
## for every date that passes, number of high-risk days before SAH order increases by almost 1 day
## the fact that it doesn't increase by exactly one day suggests that maybe things are a tiny 
##   bit better in states that issue SAH later, but not nearly enough to justify them waiting

## Party vs NHRDBS

party_colors <- c("Democratic" = "blue", "Republican" = "red")

party_plot <- ggplot(data = hrd_before_sah, aes(x = first_sah_date, y = hr_days_pre_sah)) +
  geom_point(aes(color = party), size = 3, alpha = 0.5) +
  scale_color_manual(values = party_colors) +
  geom_smooth(data = hrd_before_sah[hrd_before_sah$party == "Democratic", ], method = "lm", se = F, col = "blue") + 
  geom_smooth(data = hrd_before_sah[hrd_before_sah$party == "Republican", ], method = "lm", se = F, col = "red")

dem_days_before <- hrd_before_sah$hr_days_pre_sah[hrd_before_sah$party == "Democratic"]
rep_days_before <- hrd_before_sah$hr_days_pre_sah[hrd_before_sah$party == "Republican"]
t.test(dem_days_before, rep_days_before)

party_boxplot <- ggplot(data = hrd_before_sah, aes(x = party, y = hr_days_pre_sah)) +
  geom_boxplot(aes(fill = party)) + 
  scale_fill_manual(values = party_colors) + 
  xlab("Party") + ylab("Number of High-Risk Days Before SAH Order") + 
  ggtitle("Speed of Response by Party")

## Gov Experience vs NHRDBS

gov_first_day <- c("2017-04-10", "2018-12-03", "2015-01-05", "2019-01-07", "2019-01-08",
                   "2017-01-17", "2015-01-02", "2019-01-08", "2019-01-14", "2014-12-01",
                   "2019-01-07", "2019-01-14", "2017-01-09", "2019-01-14", "2016-01-11",
                   "2019-01-02", "2015-01-21", "2019-01-01", "2019-01-07", "2019-01-14",
                   "2018-05-29", "2013-01-07", "2019-01-07", "2017-01-05", "2018-01-16",
                   "2019-01-01", "2011-01-01", "2017-01-01", "2019-01-14", "2019-01-14",
                   "2015-02-18", "2015-01-20", "2015-01-06", "2017-01-24", "2019-01-15",
                   "2015-01-20", "2009-08-11", "2017-01-05", "2018-01-10", "2013-01-16",
                   "2017-01-16", "2019-01-07")
hrd_before_sah$gov_first_day <- as.Date(gov_first_day)

gov_exp_plot <- ggplot(data = hrd_before_sah, aes(x = gov_first_day, y = hr_days_pre_sah)) +
  geom_point() +
  xlab("Governor's First Day in Office") + ylab("Number of High-Risk Days Before SAH Order") +
  ggtitle("Governor Experience Didn't Help")
  
## Cuomo was Overhyped

ny_plot <- ggplot(data = hrd_before_sah, aes(x = first_sah_date, y = hr_days_pre_sah)) +
  geom_point(aes(col = state_abb == "NY"), size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Date of SAH Order") + ylab("Number of High-Risk Days Before SAH Order") +
  ggtitle("NY Response Doesn't Stand Out")





