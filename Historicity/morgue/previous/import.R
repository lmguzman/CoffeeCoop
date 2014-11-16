## Import the old database, and set things up.

## Run this from the directory *below* this (the main coffee
## directory).  But probably don't run it, as it was just for
## transitioning data formats.

## The new version.
##
## * Each period has its own sheet (possibly csv files as I don't
##   trust excel much at the moment).
##
## * Each sheet has columns:
##   Name
##   Payment
##   PaymentDate
##   Milk (for coffee)
##   Coffees } empty until accounting time
##   Milks   }

## I also have two other sheets:
## * people:
##   Name
##   Email
##   Gone
##
## * prices:
##   

## I do not have by-period payment data until 2010-11-04, so all
## payments prior to this are lumped into the previous period.

info <- read.csv("previous/coffee-database-info.csv",
                 stringsAsFactors=FALSE)
info$Cash <- as.numeric(sub("$", "", info$Cash, fixed=TRUE))
info$Assets <- as.numeric(sub("$", "", info$Assets, fixed=TRUE))
info[nrow(info)+1,] <- NA
info$Date[nrow(info)] <- "Future"

dat <- read.csv("previous/coffee-database-main.csv",
                stringsAsFactors=FALSE)

## Column matching regular expressions:
re.coffee <- "^Coffee_([0-9]{4}\\.[0-9]{2}\\.[0-9]{2})$"
re.milk <- "^Milk_([0-9]{4}\\.[0-9]{2}\\.[0-9]{2})$"
re.paid <- "^Paid_"
re.paiddate <- "^PaidDate_"

i.coffee <- grep(re.coffee, names(dat))
i.milk <- grep(re.milk, names(dat))
i.paid <- grep(re.paid, names(dat))
i.paiddate <- grep(re.paiddate, names(dat))

## Basic processing:
dat$Gone[is.na(dat$Gone)] <- FALSE
dat[i.coffee][is.na(dat[i.coffee])] <- 0
dat[i.milk][is.na(dat[i.milk])] <- TRUE
dat$Milk_Future[is.na(dat$Milk_Future)] <- TRUE
dat[i.paid][is.na(dat[i.paid])] <- 0

tmp <- lapply(dat[i.paiddate], as.Date, "%d-%b-%y")
sum(is.na(unlist(tmp))) == sum(dat[i.paiddate]=="")
dat[i.paiddate] <- tmp

## Becca had some dates in a really weird format.
dat.b <- read.csv("previous/coffee-database-becca.csv",
                  stringsAsFactors=FALSE)
i.date.b <- grep("^Date", names(dat.b))
i.paid.b <- grep("^Contr", names(dat.b))
dat.b[i.date.b] <- lapply(dat.b[i.date.b], as.Date, "%Y-%m-%d")
dat.b[i.paid.b] <- lapply(dat.b[i.paid.b], function(x)
                          as.numeric(sub("$", "", x, fixed=TRUE)))

## Next, confirm that Becca and my calculations add up...
tot.b <- rowSums(dat.b[i.paid.b], na.rm=TRUE)
names(tot.b) <- dat.b$Name

tot.m <- dat$Paid_Becca
names(tot.m) <- dat$Name

err <- which(tot.b != tot.m[names(tot.b)])
tot.b[names(err)]
tot.m[names(err)]

i <- which(tot.b - tot.m[names(tot.b)] != 0)
tot.b[i]
tot.m[names(tot.b)][i]

all.equal(tot.b, tot.m[names(tot.b)])
all(tot.m[setdiff(names(tot.m), names(tot.b))] == 0)
length(setdiff(dat.b$Name, dat$Name) == 0)

## Set up some output matrices.  These are going to get converted into
## the final version.
dates <- as.character(info$Date)
people <- dat$Name

## Good.  Now, move Becca's format into my format:

## Output space:
m.date <- m.paid <- m.milk <- m.coffee <- 
  matrix(NA, length(people), length(dates),
         dimnames=list(people, dates))
m.paid[] <- 0
for ( i in i.date.b ) {
  j <- sapply(dat.b[[i]], function(x) which(x < dates)[1])
  k <- !is.na(j)
  idx <- cbind(match(dat.b$Name[k], people), j[k])
  m.date[idx] <- as.character(dat.b[[i]][k])
  m.paid[idx] <- m.paid[idx] + dat.b[[i+1]][k]
}

## Check no money was lost:
all.equal(rowSums(m.paid, na.rm=TRUE)[dat.b$Name],
          rowSums(dat.b[i.paid.b], na.rm=TRUE), check.attr=FALSE)

## Now, do the same thing for my cash.
## Also need to track down Milk status (FALSE for everyone prior to
## 2010-11-04) and combine with Becca.

## For my data set, this is slightly easier:
idx <- sub(".+_", "", names(dat)[i.paiddate][-1])
idx <- gsub(".", "-", idx, fixed=TRUE)

## The issue here is overlap.
m.paid.tmp <- m.paid
m.paid.tmp[] <- 0
m.date.tmp <- m.date
m.date.tmp[] <- NA
m.paid.tmp[,idx] <- as.matrix(dat[i.paid[-1]])
m.date.tmp[,idx] <- as.matrix(dat[i.paiddate[-1]])

## Check no overlap:
!any(m.paid.tmp > 0 & m.paid > 0)
i <- m.paid == 0
m.paid[i] <- m.paid.tmp[i]
m.date[i] <- m.date.tmp[i]

## And check that no money has gone missing:
all.equal(rowSums(m.paid), rowSums(dat[i.paid]), check.attr=FALSE)

## Then milk
m.milk[] <- TRUE
idx <- sub(".+_", "", names(dat)[i.milk])
idx <- gsub(".", "-", idx, fixed=TRUE)
m.milk[,idx] <- as.matrix(dat[i.milk])
m.milk[,length(dates)] <- m.milk[,length(dates)-1]

m.coffee[] <- 0
idx <- sub(".+_", "", names(dat)[i.coffee])
idx <- gsub(".", "-", idx, fixed=TRUE)
m.coffee[,idx] <- as.matrix(dat[i.coffee])

m.tea <- m.coffee
m.tea[] <- 0

## Then, build the main sheets.
f <- function(i)
  data.frame(Name=people,
             Payment=m.paid[,i],
             Payment.Date=m.date[,i],
             Milk=m.milk[,i],
             Coffee=m.coffee[,i],
             Tea=m.tea[,i],
             stringsAsFactors=FALSE)
sheets <- lapply(seq_along(dates), f)
names(sheets) <- dates

## Now, let's find when people turn up and trim the sheets down.
seen <- character(0)
gone <- dat$Name[dat$Gone]
gone.date <- rep(NA, length(gone))
names(gone.date) <- gone

## We got the machine on 2009-10-14
start.date <- rep(NA, length(people))
names(start.date) <- people

sheets2 <- sheets
for ( i in seq_along(sheets) ) {
  d <- sheets2[[i]]
  seen.i <- d$Coffee > 0 | d$Payment > 0
  seen <- union(seen, d$Name[seen.i])
  new <- is.na(start.date[seen])
  start.date[names(new[new])] <- i
  exclude.poss <- intersect(seen, intersect(d$Name[d$Coffee == 0], gone))
  if ( length(exclude.poss) > 0 ) {
    future.coffee <- rowSums(m.coffee[exclude.poss,-seq_len(i),drop=FALSE])
    future.paid <- rowSums(m.paid[exclude.poss,-seq_len(i-1),drop=FALSE])
    exclude <- exclude.poss[future.coffee == 0 & future.paid == 0]
    new <- is.na(gone.date[exclude])
    gone.date[names(new[new])] <- dates[i]
  }
  tmp <- d[d$Name %in% seen,]
  tmp <- tmp[order(tmp$Name),]
  sheets2[[i]] <- tmp
  sheets2[[i]]$Tea[sheets2[[i]]$Tea == 0] <- NA
  sheets2[[i]]$Coffee[sheets2[[i]]$Coffee == 0] <- NA
  if ( sum(sheets2[[i]]$Coffee, na.rm=TRUE) != sum(sheets[[i]]$Coffee) )
    stop("Impossible mismatch")
}

## Redo Future so that the order exactly matches previously.
sheets2$Future <- sheets$Future
sheets2[[i]]$Tea[sheets2[[i]]$Tea == 0] <- NA
sheets2[[i]]$Coffee[sheets2[[i]]$Coffee == 0] <- NA
if ( sum(sheets2[[i]]$Coffee, na.rm=TRUE) != sum(sheets[[i]]$Coffee) )
  stop("Impossible mismatch")

start.date <- dates[start.date]

## This is all the information to make the people sheet:
d.people <- data.frame(Name=people,
                       Email=dat$Email,
                       Start=start.date,
                       Gone=dat$Gone,
                       Gone.Date=gone.date[people],
                       stringsAsFactors=FALSE)

write.csv(info, "data/info.csv", row.names=FALSE, na="")
write.csv(d.people, "data/people.csv", row.names=FALSE, na="")
for ( i in names(sheets2) )
  write.csv(sheets2[[i]], sprintf("data/%s.csv", i),
            row.names=FALSE, na="")

rm(list=ls(all=TRUE))
oldwd <- setwd("previous")
library(xlsx)
source("process-fun.R")
prev <- read.coffee("coffee-database.xlsx")
save(prev, file="prev.Rdata")
setwd(oldwd)
rm(list=ls(all=TRUE))

## Confirm that my generated csv sheets match up with the Excel sheets.
library(xlsx)
info.old <- read.xlsx("previous/coffee-database.xlsx", 2,
                      stringsAsFactors=FALSE)
data.old <- read.xlsx("previous/coffee-database.xlsx", "Main",
                      stringsAsFactors=FALSE)
re.paiddate <- "^PaidDate_"
i.paiddate <- grep(re.paiddate, names(data.old))
data.old[i.paiddate] <- lapply(data.old[i.paiddate], as.Date,
                               origin=as.Date("1904-01-01"))

info <- read.csv("previous/coffee-database-info.csv",
                 stringsAsFactors=FALSE)
info$Cash <- as.numeric(sub("$", "", info$Cash, fixed=TRUE))
info$Assets <- as.numeric(sub("$", "", info$Assets, fixed=TRUE))
all.equal(info, info.old)

data <- read.csv("previous/coffee-database-main.csv",
                 stringsAsFactors=FALSE, na.strings="")
data <- data[setdiff(names(data), "X")]
data[i.paiddate] <- lapply(data[i.paiddate], as.Date, "%d-%b-%y")

all.equal(data, data.old)

## Check against new import
rm(list=ls(all=TRUE))
source("R/load.R")
source("R/process-html.R")
dat <- load.coffee()

cols <- setdiff(names(dat$status), "Milk")
load("previous/prev.Rdata")
s.new <- dat$status[cols]
s.old <- prev[match(s.new$Name, prev$Name), cols]
s.old$TotalOut[s.old$Name == "Sally Otto"] <- 0
rownames(s.new) <- rownames(s.old) <- NULL
s.new$LastDate <- as.Date(s.new$LastDate)

## Differences in LastPaid and and LastDate (8 & 9)
all.equal(s.new, s.old)

## LastPaid is really different, but these are just the Becca
## accounting change, as all date differences stem from then:
i <- which(s.new$LastPaid != s.old$LastPaid)
any(is.na(s.new$LastPaid) + is.na(s.old$LastPaid) == 1) # FALSE -- good
s.new[i,]
