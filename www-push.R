#!/usr/bin/env Rscript

source("R/load.R")
source("R/process-html.R")
source("R/accounts.R")
source("R/process-latex.R")

dat <- load.data()
www(dat)
www.copy()
