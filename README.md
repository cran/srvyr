<!-- README.md is generated from README.Rmd. Please edit that file -->
srvyr
=====

[![Travis-CI Build Status](https://travis-ci.org/gergness/srvyr.svg?branch=master)](https://travis-ci.org/gergness/srvyr) [![Coverage Status](https://img.shields.io/codecov/c/github/gergness/srvyr/master.svg)](https://codecov.io/github/gergness/srvyr?branch=master)

srvyr brings parts of [dplyr's](https://github.com/hadley/dplyr/) syntax to survey analysis, using the [survey](http://cran.r-project.org/package=survey) package.

srvyr focuses on calculating summary statistics from survey data, such as the mean, total or quantile. It allows for the use of many dplyr verbs, such as `summarize`, `group_by`, and `mutate`, the convenience of pipe-able functions, lazyeval's style of non-standard evaluation and more consistent return types than the survey package.

You can try it out (only available on github at the moment):

``` r
devtools::install_github("gergness/srvyr")
```

To create a `tbl_svy` object (the core concept behind the srvyr package), use the function `as_survey_design()` with the bare column names of the names you would use in `survey::svydesign()` object.

``` r
library(survey)
data(api)

dstrata <- apistrat %>%
   as_survey_design(strata = stype, weights = pw)
```

Now many of the dplyr verbs are available.

-   Use `mutate()` if you want to add or modify a variable.

    ``` r
    dstrata <- dstrata %>%
      mutate(api_diff = api00 - api99)
    ```

-   `summarise()` calculates summary statistics such as mean, total, quantile or ratio.

    ``` r
    dstrata %>% 
      summarise(api_diff = survey_mean(api_diff, vartype = "ci")))
    ```

-   Use `group_by()` if you want to summarise by groups.

    ``` r
    dstrata %>% 
      group_by(stype) %>%
      summarise(api_diff = survey_mean(api_diff, vartype = "ci")))
    ```

You can still use functions from the survey package if you'd like to:

``` r
svyglm(api99 ~ stype, dstrata)
```

If you'd like to contribute, please let me know! I started this as a way to learn about R package development, so you'll have to bear with me as I learn, but I would appreciate bug reports, pull requests or other suggestions!
