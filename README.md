# state_covid_response
Accounting for differences in threat posed by COVID-19 in analysis of state-by-state response

## Motivation

This project aims to analyze the state-by-state response to COVID-19 in the US.
It is important to account for the variation in the threat posed by COVID-19 to each state. This variation might come from differences in hospital capacity, differences in number of cases, differences in reproduction potential of the virus, or different age demographics of the population. Arguably, the strength of the US’s state-by-state response lies in the ability to address heterogeneity in COVID-19 risk by state.

## Findings

My findings resoundingly suggest that heterogeneity in state COVID-19 response was not justified by heterogeneity in the risk posed by COVID-19 to states. States that issued Stay at Home orders at later dates were simply under high-risk from COVID-19 for longer than states that issued Stay at Home orders at earlier dates. Despite the theoretical benefits of a state-by-state response, in this case, an early national response would have been much more effective. 

I also challenge the narrative that Democratic governors responded faster, highlighting the need to account for conditions in individual states and test for statistical significance. Though, on average, Democratic governors did respond faster relative to conditions in their state, this difference was not statistically significant. 

Lastly, I find no relationship between governor experience and speed of COVID-19 response.

## Disclaimers

This project presents a methodology for gauging state-by-state response to COVID-19, accounting for the particular conditions of each state. It is true that individual estimates of risk are likely to be wrong, owing to the uncertainties throughout the estimation process. This is why I only use risk scores for the coarse categorization of high-risk vs. low-risk days, and keep the focus on overall trends. I believe this maintains the robustness of my analysis to the nuances in estimation. I caution against reading into specific risk scores.

## Data Credits

For hospital beds: https://www.kaggle.com/ikiulian/global-hospital-beds-capacity-for-covid19
For cases: https://github.com/nytimes/covid-19-data/
For reproduction number: https://rt.live 
For state age demographics: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html
For hospitalizations by age: https://www.usnews.com/news/health-news/articles/2020-03-30/odds-of-hospitalization-death-with-covid-19-rise-steadily-with-age-study
For governors: https://ballotpedia.org/Partisan_composition_of_governors and Wikipedia
