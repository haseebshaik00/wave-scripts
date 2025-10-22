# Use a stable CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Put renv’s library & cache somewhere you definitely own (short paths)
Sys.setenv(
  RENV_PATHS_LIBRARY_ROOT = "C:/R/renv-lib",
  RENV_PATHS_CACHE        = "C:/R/renv-cache",
  RENV_COPY_METHOD        = "R"    # avoid robocopy
)
dir.create("C:/R/renv-lib",   recursive = TRUE, showWarnings = FALSE)
dir.create("C:/R/renv-cache", recursive = TRUE, showWarnings = FALSE)

# Safer on Windows / network drives
options(renv.config.cache.symlinks = FALSE)

# Make sure renv is present, then activate project
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::activate()

# (Optional) if permissions still bite, temporarily bypass cache:
# renv::settings$use.cache(FALSE)

# Install your packages via renv
renv::install(c("dplyr","tidyverse","data.table","stringr","lubridate",
                "openxlsx","here","purrr","tidyr"))

# Lock them in the project
renv::snapshot(prompt = FALSE)