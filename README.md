
<!-- README.md is generated from README.Rmd. Please edit that file -->
srvyr <img src="tools/logo.png" align="right" height="149" width="149"/>
========================================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/srvyr)](http://cran.r-project.org/web/packages/srvyr) [![Travis-CI Build Status](https://travis-ci.org/gergness/srvyr.svg?branch=master)](https://travis-ci.org/gergness/srvyr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/gergness/srvyr?branch=master&svg=true)](https://ci.appveyor.com/project/gergness/srvyr) [![Coverage Status](https://codecov.io/gh/gergness/srvyr/master.svg)](https://codecov.io/github/gergness/srvyr?branch=master) [![Documentation via pkgdown](tools/pkgdownshield.svg)](http://gdfe.co/srvyr)

srvyr brings parts of [dplyr's](https://github.com/hadley/dplyr/) syntax to survey analysis, using the [survey](https://CRAN.R-project.org/package=survey) package.

srvyr focuses on calculating summary statistics from survey data, such as the mean, total or quantile. It allows for the use of many dplyr verbs, such as `summarize`, `group_by`, and `mutate`, the convenience of pipe-able functions, rlang's style of non-standard evaluation and more consistent return types than the survey package.

You can try it out:

``` r
install.packages("srvyr")
# or for development version
# devtools::install_github("gergness/srvyr")
```

Example usage
-------------

First, describe the variables that define the survey's stucture with the function `as_survey()`with the bare column names of the names that you would use in functions from the survey package like `survey::svydesign()`, `survey::svrepdesign()` or `survey::twophase()`.

``` r
library(srvyr, warn.conflicts = FALSE)
data(api, package = "survey")

dstrata <- apistrat %>%
   as_survey_design(strata = stype, weights = pw)
```

Now many of the dplyr verbs are available.

-   `mutate()` adds or modifies a variable.

``` r
dstrata <- dstrata %>%
  mutate(api_diff = api00 - api99)
```

-   `summarise()` calculates summary statistics such as mean, total, quantile or ratio.

``` r
dstrata %>% 
  summarise(api_diff = survey_mean(api_diff, vartype = "ci"))
#> # A tibble: 1 x 3
#>   api_diff api_diff_low api_diff_upp
#>      <dbl>        <dbl>        <dbl>
#> 1     32.9         28.8         37.0
```

-   `group_by()` and then `summarise()` creates summaries by groups.

``` r
dstrata %>% 
  group_by(stype) %>%
  summarise(api_diff = survey_mean(api_diff, vartype = "ci"))
#> # A tibble: 3 x 4
#>   stype api_diff api_diff_low api_diff_upp
#>   <fct>    <dbl>        <dbl>        <dbl>
#> 1 E        38.6         33.1          44.0
#> 2 H         8.46         1.74         15.2
#> 3 M        26.4         20.4          32.4
```

-   Functions from the survey package are still available:

``` r
my_model <- survey::svyglm(api99 ~ stype, dstrata)
summary(my_model)
#> 
#> Call:
#> svyglm(formula = api99 ~ stype, dstrata)
#> 
#> Survey design:
#> Called via srvyr
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   635.87      13.34  47.669   <2e-16 ***
#> stypeH        -18.51      20.68  -0.895    0.372    
#> stypeM        -25.67      21.42  -1.198    0.232    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 16409.56)
#> 
#> Number of Fisher Scoring iterations: 2
```

What people are saying about srvyr
----------------------------------

> \[srvyr\] lets us use the survey library’s functions within a data analysis pipeline in a familiar way.
>
> -- <cite>Kieran Healy, in [Data Visualization: A practical introduction](http://socviz.co/modeling.html#plots-from-complex-surveys) </cite>

> 1.  Yay!
>
> --<cite>Thomas Lumley, [in the Biased and Inefficent blog](http://notstatschat.tumblr.com/post/161225885311/pipeable-survey-analysis-in-r)</cite>

Contributing
------------

I do appreciate bug reports, suggestions and pull requests! I started this as a way to learn about R package development, and am still learning, so you'll have to bear with me. Please review the [Contributor Code of Conduct](CODE_OF_CONDUCT.md), as all participants are required to abide by its terms.

If you're unfamiliar with contributing to an R package, I recommend the guides provided by Rstudio's tidyverse team, such as Jim Hester's [blog post](https://www.tidyverse.org/articles/2017/08/contributing/) or Hadley Wickham's [R packages book](http://r-pkgs.had.co.nz).
