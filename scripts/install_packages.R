packages <- c(
  "tidyverse", "lubridate", "broom", "fixest", "kableExtra",
  "gridExtra", "patchwork", "ggplot2", "plm", "lmtest", "stargazer"
)
install.packages(setdiff(packages, installed.packages()[, "Package"]), repos = "https://cran.r-project.org")
lapply(packages, library, character.only = TRUE)
