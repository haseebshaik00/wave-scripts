# Summer'25 Wave Scripts

## Initial Setup

Setup-1: Project Setup
- Create project wave-scripts
- Add necessary files/folders, README.md and .gitignore files
- To open the project in RStudio (double-click .Rproj file), wherever saved!

Step-2: `renv` setup
```bash
install.packages("renv")
renv::init()
```
Initialize renv (manages dependencies), this creates .Rprofile, renv/, and renv.lock

Step-3: Install your libraries (only once, they’ll be saved)
```bash
install.packages(c(
  "dplyr", "tidyverse", "data.table",
  "stringr", "lubridate", "openxlsx", "here"
))
renv::snapshot()   # save versions into renv.lock
```

Step-4: Create your R/setup.R file
```bash
# R/setup.R
options(scipen = 999)
library(dplyr)
library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(openxlsx)
library(here)
```

Congrats! You all set!

## Access Project
- Open .Rproj file in RStudio (this auto-activates renv)
```bash
install.packages("renv")
renv::restore()
source("R/setup.R") # Run source("R/setup.R") in console - make sure you are in the project's directory
```
- Now you’ll be in the same environment, no confusion about versions!

### [IMPORTANTS]
- Make sure you run the following commands when adding/removing/updating packages:
```bash
install.packages("packagename")
reNv::snapshot()
```