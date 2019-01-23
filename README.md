# resumable

This is a **prototype** package (designed mainly for internal use) based on [storr](https://github.com/richfitz/storr): Simple object cacher for R by [Rich FitzJohn](https://github.com/richfitz)

This gives a simple function `resumable` which takes [purrr](https://purrr.tidyverse.org/) like functions as input and make that a resumable function. It is actually memoization technique adpoted on arbitrary user defined R function.

This feature helps in web-scrapping activties.

Install this package in R with `devtools::install_github` with the following call:

    devtools::install_github("MadeInR/resumable")
