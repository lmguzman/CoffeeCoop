#!/usr/bin/env Rscript
## gdata is faster, but will require remaking read.coffee function.
library(xlsx)

## Flags:
## http://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Flag_of_Norway.svg/20px-Flag_of_Norway.svg.png

source("process-fun.R")

dat <- read.coffee("coffee-database.xlsx")

signup(dat)
www(dat)
www.copy()



