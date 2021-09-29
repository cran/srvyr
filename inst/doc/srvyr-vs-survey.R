## ---- message = FALSE, fig.width = 6------------------------------------------
library(survey)
library(ggplot2)
library(dplyr)

data(api)

out <- apistrat %>%
  mutate(hs_grad_pct = cut(hsg, c(0, 20, 100), include.lowest = TRUE,
                           labels = c("<20%", "20+%"))) %>%
  group_by(stype, hs_grad_pct) %>%
  summarize(api_diff = weighted.mean(api00 - api99, pw),
            n = n())

ggplot(data = out, aes(x = stype, y = api_diff, group = hs_grad_pct, fill = hs_grad_pct)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -1)

## ---- message = FALSE---------------------------------------------------------
library(srvyr)

# simple random sample
srs_design_srvyr <- apisrs %>% as_survey_design(ids = 1, fpc = fpc)

srs_design_survey <- svydesign(ids = ~1, fpc = ~fpc, data = apisrs)

## ---- message = FALSE---------------------------------------------------------
# selecting variables to keep in the survey object (stratified example)
strat_design_srvyr <- apistrat %>%
  as_survey_design(1, strata = stype, fpc = fpc, weight = pw,
                variables = c(stype, starts_with("api")))

strat_design_survey <- svydesign(~1, strata = ~stype, fpc = ~fpc,
                                 variables = ~stype + api99 + api00 + api.stu,
                                 weight = ~pw, data = apistrat)

## ---- message = FALSE---------------------------------------------------------
# simple random sample (again)
srs_design_srvyr2 <- apisrs %>% as_survey(ids = 1, fpc = fpc)

## ---- message = FALSE---------------------------------------------------------
strat_design_srvyr <- strat_design_srvyr %>%
  mutate(api_diff = api00 - api99) %>%
  rename(api_students = api.stu)

strat_design_survey$variables$api_diff <- strat_design_survey$variables$api00 -
  strat_design_survey$variables$api99
names(strat_design_survey$variables)[names(strat_design_survey$variables) == "api.stu"] <- "api_students"

## ---- message = FALSE---------------------------------------------------------
# Using srvyr
out <- strat_design_srvyr %>%
  summarize(api_diff = survey_mean(api_diff, vartype = "ci"))

out

# Using survey
out <- svymean(~api_diff, strat_design_survey)

out
confint(out)

## ---- message = FALSE---------------------------------------------------------
# Using srvyr
strat_design_srvyr %>%
  group_by(stype) %>%
  summarize(api_increase = survey_total(api_diff >= 0),
            api_decrease = survey_total(api_diff < 0))

# Using survey
svyby(~api_diff >= 0, ~stype, strat_design_survey, svytotal)

## ---- message = FALSE---------------------------------------------------------
# Using srvyr
srs_design_srvyr %>%
  group_by(awards) %>%
  summarize(proportion = survey_mean(),
            total = survey_total())

# Using survey
svymean(~awards, srs_design_survey)
svytotal(~awards, srs_design_survey)

## ---- message = FALSE---------------------------------------------------------
# Using srvyr
strat_design_srvyr %>%
  group_by(stype) %>%
  summarize(n = unweighted(n()))

# Using survey
svyby(~api99, ~stype, strat_design_survey, unwtd.count)

## ---- message = FALSE, fig.width = 6------------------------------------------
strat_design <- apistrat %>%
  as_survey_design(strata = stype, fpc = fpc, weight  = pw)

out <- strat_design %>%
  mutate(hs_grad_pct = cut(hsg, c(0, 20, 100), include.lowest = TRUE,
                           labels = c("<20%", "20+%"))) %>%
  group_by(stype, hs_grad_pct) %>%
  summarize(api_diff = survey_mean(api00 - api99, vartype = "ci"),
            n = unweighted(n()))

ggplot(data = out, aes(x = stype, y = api_diff, group = hs_grad_pct, fill = hs_grad_pct,
                       ymax = api_diff_upp, ymin = api_diff_low)) +
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  geom_text(aes(y = 0, label = n), position = position_dodge(width = 0.9), vjust = -1)



## -----------------------------------------------------------------------------
# Set pillar print methods so tibble has more decimal places
old_sigfig <- options("pillar.sigfig")
options("pillar.sigfig" = 5)

# survey default
estimate <- svymean(~api99, strat_design)
confint(estimate)

# srvyr default
strat_design %>%
  summarize(x = survey_mean(api99, vartype = "ci"))

# setting the degrees of freedom so srvyr matches survey default
strat_design %>%
  summarize(x = survey_mean(api99, vartype = "ci", df = Inf)) %>%
  print()

# setting the degrees of freedom so survey matches survey default
confint(estimate, df = degf(strat_design))

# reset significant figures
options("pillar.sigfig" = old_sigfig)

## ---- message = FALSE---------------------------------------------------------
glm <- svyglm(api00 ~ ell + meals + mobility, design = strat_design)
summary(glm)

## ---- message = FALSE---------------------------------------------------------
strat_design %>%
  summarize(prop_api99_over_700 = survey_mean(api99 > 700))

## ---- message = FALSE---------------------------------------------------------
strat_design %>%
  group_by(awards) %>%
  summarize(percentage = 100 * survey_mean())

## ---- message = FALSE---------------------------------------------------------
strat_design %>%
  group_by(api99_above_700 = api99 > 700) %>%
  summarize(api00_mn = survey_mean(api00))

## ---- message = FALSE, eval=FALSE---------------------------------------------
#  # BAD DON'T DO THIS!
#  strat_design %>%
#    group_by(awards) %>%
#    summarize(percentage = 100 * survey_mean(vartype = "var"))
#  # VARIANCE IS WRONG

## ---- message = FALSE---------------------------------------------------------
mean_with_ci <- function(.data, var) {
  summarize(.data, mean = survey_mean({{var}}, vartype = "ci"))
}

srs_design_srvyr <- apisrs %>% as_survey_design(fpc = fpc)

mean_with_ci(srs_design_srvyr, api99)

## -----------------------------------------------------------------------------
# Calculate survey mean for all variables that have names starting with "api"
strat_design %>%
  summarize(across(starts_with("api"), survey_mean))

## -----------------------------------------------------------------------------
# Calculate the proportion that falls into each category of `awards` per `stype`
strat_design %>%
  group_by(stype, awards) %>%
  summarize(prop = survey_prop())

## -----------------------------------------------------------------------------
# Calculate the proportion that falls into each category of both `awards` and `stype`
strat_design %>%
  group_by(interact(stype, awards)) %>%
  summarize(prop = survey_prop())

