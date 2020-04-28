
wd <- "~/Documents/School/College/Junior Year/Econ 126/OpEd_1_State_Covid_Response/"
data_dir <- paste0(wd, "data/")
source(paste0(wd, "data_clean.R"))

# https://www.usnews.com/news/health-news/articles/2020-03-30/odds-of-hospitalization-death-with-covid-19-rise-steadily-with-age-study
case_hosp_rate_by_age <- c(0, 0.1, 1, 3.4, 4.5, 8.2, 11.8, 16.6, 18.4)
age_decade <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
hosp_by_age <- tibble::tibble(hosp_prob = case_hosp_rate_by_age / 100,
                              group = age_decade)
# case_hosp_rate is prob of hospitalization given infection (not confirmed case)
# but, assuming people are equally likely to be infected across age,
# we can estimate case_hosp_rate by:
### distributing new cases across ages based on age demographics of the state, 
### calculating number of hospitalizations based on number of cases in each age demographic

cases <- getUSCases(active = TRUE)
cases <- cases[!is.na(cases$state_abb), ]
rts <- getRts()
beds <- getUSBeds()
pop <- getPopData()

pop_w_hosp <- pop %>%
  dplyr::left_join(hosp_by_age, by = "group")

# the risk score is the number of time periods 
# until the number of hospitalizations from COVID
# exceed the number of total hospital beds
# returns the best case (10th percentile R), avg case scenario (mean R), worse case (90th percentile R)
getRiskScores <- function(state_abbrev, date_) {
  getOverrunTF <- function(hosp, beds) hosp > beds
  n_active_cases <- cases %>%
    dplyr::filter(date == date_, state_abb == state_abbrev) %>%
    pull(active_cases)
  n_active_cases_good <- n_active_cases_mean <- n_active_cases_bad <- n_active_cases
  hosp_info <- pop_w_hosp %>%
    dplyr::filter(state_abb == state_abbrev)
  n_beds <- beds %>%
    dplyr::filter(state_abb == state_abbrev) %>%
    pull(total_beds)
  r_good <- rts$lower_90[rts$state_abb == state_abbrev & rts$date == date_]
  r_mean <- rts$mean[rts$state_abb == state_abbrev & rts$date == date_]
  r_bad <- rts$upper_90[rts$state_abb == state_abbrev & rts$date == date_]
  risk_score <- tibble::tibble("good" = 1, "mean" = 1, "bad" = 1)
  i <- 1
  while (i <= 365) {
    n_active_cases_good <- n_active_cases_good * r_good
    n_active_cases_mean <- n_active_cases_mean * r_mean
    n_active_cases_bad <- n_active_cases_bad * r_bad
    n_hosp_good <- sum(n_active_cases_good * hosp_info$pop_prop * hosp_info$hosp_prob)
    n_hosp_mean <- sum(n_active_cases_mean * hosp_info$pop_prop * hosp_info$hosp_prob)
    n_hosp_bad <- sum(n_active_cases_bad * hosp_info$pop_prop * hosp_info$hosp_prob)
    if (!getOverrunTF(n_hosp_good, n_beds)) risk_score$good <- risk_score$good + 1
    if (!getOverrunTF(n_hosp_mean, n_beds)) risk_score$mean <- risk_score$mean + 1
    if (!getOverrunTF(n_hosp_bad, n_beds)) risk_score$bad <- risk_score$bad + 1
    i <- i + 1
  }
  risk_score[["state_abb"]] <- state_abbrev
  risk_score[["date"]] <- date_
  return (risk_score)
}

# risk_scores <- purrr::map2_dfr(cases$state_abb, cases$date, getRiskScores)
# save as csv
# readr::write_csv(risk_scores, paste0(wd, "risk_scores.csv"))


# Assumptions & Notes
## time period: number of cases in state grows by (n_cases * R_t) from one time period to the next
## we're assuming a case hospitalization rate here that varies based on age demographics of the state
## (though hospitalizations are hard to project, we're using infected hosp rate, not case hosp rate)
## we're not accounting for variation in type of hospital bed (ICU vs non-ICU)
## we're not accounting for people in the hospital for non-COVID reasons
## we're not accounting for within-state variation in hospital bed availability

