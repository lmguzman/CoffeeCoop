read.coffee <- function(filename) {
  info <- read.xlsx(filename, 2, stringsAsFactors=FALSE)
  info$Date <- as.Date(info$Date)
  info$Used <- c(NA, diff(info$Count))
  info$UsedPerDay <- info$Used / c(NA, diff(info$Date))
  
  dat <- read.xlsx(filename, "Main")
  dat <- dat[!is.na(dat$Name),] # Excel sucks
  dat$Name <- as.character(dat$Name)
  dat$Email <- as.character(dat$Email)

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

  ## Excel sucks more than a vacum cleaner.
  i.paiddate.date <- i.paiddate[sapply(dat[i.paiddate], inherits, "Date")]
  if ( length(i.paiddate.date) > 0 )
    dat[i.paiddate.date] <- dat[i.paiddate.date] + 1462
  i.paiddate.int <- setdiff(i.paiddate, i.paiddate.date)
  dat[i.paiddate.int] <- lapply(dat[i.paiddate.int], as.Date,
                                origin=as.Date("1904-01-01"))

  ## Now, let's work out what interval things apply to:
  sub.date <- function(re, i)
    as.Date(gsub("\\.", "-", sub(re, "\\1", names(dat)[i])))
  
  coffee.date <- sub.date(re.coffee, i.coffee)
  milk.date <- sub.date(re.milk, i.milk)

  ## Basic sanity checking:
  if ( !identical(coffee.date, info$Date) )
    stop("Dates must line up")
  ## TODO: This is not sufficient...
  if ( !all(milk.date %in% info$Date) )
    stop("Unknown milk dates")
  if ( length(i.paid) != length(i.paiddate) )
    stop("Paid columns confused")

  ## Compute cost.  First, get a matrix of milk consumption:
  with.milk.m <- matrix(FALSE, nrow(dat), length(i.coffee))
  with.milk.m[,coffee.date %in% milk.date] <- as.matrix(dat[i.milk])

  ## And then a user x date matrix of per-period cost.
  cost.black <- matrix(info$CostBlack, nrow(dat), length(i.coffee),
                       byrow=TRUE)
  cost.milk <- matrix(info$CostBlack + info$CostMilk, nrow(dat),
                      length(i.coffee), byrow=TRUE)
  cost.m <- cost.black
  cost.m[with.milk.m] <- cost.milk[with.milk.m]

  ## And add columns with the main things that we want:
  dat$TotalCoffee <- rowSums(dat[i.coffee])
  dat$TotalIn <- rowSums(dat[i.paid])
  dat$TotalOut <- rowSums(dat[i.coffee] * cost.m)
  dat$Balance <- dat$TotalIn - dat$TotalOut
  dat$LastCoffee <- dat[[max(i.coffee)]]

  dat$Balance[dat$Name == "Sally Otto"] <- 0.00

  last <- suppressWarnings(apply(dat[i.paid] > 0, 1, function(x)
                               max(which(x))))
  last[is.infinite(last)] <- NA
  dat$LastPaid <- as.numeric(dat[cbind(seq_along(last), i.paid[last])])
  dat$LastDate <- as.Date(dat[cbind(seq_along(last), i.paid[last]+1)])

  info$Paid <- NA
  info$Paid[match(coffee.date, info$Date)] <- colSums(dat[i.coffee])
  info$PaidPerDay <- info$Paid / c(NA, diff(info$Date))
  info$Honesty <- info$Paid / info$Used

  i <- match(milk.date, coffee.date)
  info$MilkyCoffee <- NA
  info$MilkyCoffee[i] <- colSums(dat[i.coffee[i]] * dat[i.milk])
  info$TrueMilkCost <- info$MilkOutgoing / info$MilkyCoffee

  ## Try and work out the rolling balances:
  i <- match(sub("Paid_", "Coffee_", names(dat)[i.paid[-length(i.paid)]]),
             names(dat)[i.coffee])
  i[1] <- i[2] - 1
  tmp.out <- t(apply(dat[i.coffee] * cost.m, 1, cumsum))
  tmp.in <- t(apply(dat[i.paid[-length(i.paid)]], 1, cumsum))
  info$Liability <- NA
  info$Liability[i] <- colSums(tmp.in - tmp.out[,i])
  info$Balance <- info$Cash + info$Assets - info$Liability

  attr(dat, "info") <- info

  dat
}

## Signup sheet:
signup <- function(dat) {
  sub <- subset(dat, !Gone, c("Name", "Milk_Future", "Balance"))

  ## Accent translation for people who need it:
  from <- c("Chenard", "Debarre")
  to <- c("Ch\\\\'enard", "D\\\\'ebarre")
  for ( i in seq_along(from) )
    sub$Name <- sub(from[i], to[i], sub$Name)

  sub <- sub[order(sub$Name),]
  
  tmp <- cbind(as.character(sub$Name),
               ifelse(!sub$Milk_Future, "\\checkmark", ""),
               sprintf(ifelse(sub$Balance < 0, "\\textbf{-\\$%2.2f}",
                              "\\$%2.2f"),
                       abs(sub$Balance)),
               "\\\\\\hline")
  str <- paste(apply(tmp, 1, paste, collapse=" & "), collapse="\n")


  template <-
    c("\\begin{longtable}{|l|c|r|X|}\\hline",
      "\\textbf{Name} & \\textbf{No milk} & \\textbf{Balance}",
      "& \\textbf{Drinks Tally}\\endhead\\hline",
      "%s",
      "\\multicolumn{4}{l}{}\\\\[15ex]",
      "\\multicolumn{4}{l}{\\textbf{New members:}}\\\\[1ex]\\hline",
      "\\textbf{Name}&\\textbf{No milk}&&\\textbf{Drinks Tally}\\\\\\hline",
      "%s",
      "\\end{longtable}")
  writeLines(sprintf(paste(template, collapse="\n"),
                     paste(str, collapse="\n"),
                     paste(rep("&&&\\\\\\hline", 20), collapse="\n")),
             "signup/table.tex")
  system("cd signup; pdflatex signup.tex")  
}

www <- function(dat) {
  template <- paste(readLines("www/template.html"), collapse="\n")
  str <- sprintf(template, www.summary(dat), www.balance(dat))
  writeLines(str, "www/index.html")                 
}

td <- function(x) {
  out <- sprintf("<td>%s</td>", x)
  out[is.na(x)] <- "<td />"
  out
}
td.currency <- function(x) {
  out <- sprintf(ifelse(x < 0, '<td class="indebt">-$%2.2f</td>',
                        '<td>$%2.2f</td>'), abs(x))
  out[is.na(x)] <- "<td />"
  out
}
th <- function(x) {
  out <- sprintf("<th>%s</th>", x)
  out[is.na(x)] <- "<th />"
  out
}

www.summary <- function(dat) {
  sprintf2 <- function(fmt, x, ...) {
    ans <- sprintf(fmt, x, ...)
    ans[is.na(x)] <- NA
    ans
  }

  info <- attr(dat, "info")
  tmp <- data.frame(Date=as.character(info$Date, "%Y/%m/%d"),
                    "Coffees made"=info$Count,
                    "This month"=info$Used,
                    "Per day"=sprintf2("%2.2f", info$UsedPerDay),
                    "Paid for"=info$Paid,
                    "Per day"=sprintf2("%2.2f", info$PaidPerDay),
                    Honesty=sprintf2("%2.2f", info$Honesty),
                    "With milk"=info$MilkyCoffee,
                    ## "True milk cost"=sprintf2("%2.2f", info$TrueMilkCost),
                    check.names=FALSE)
  tmp[] <- sapply(tmp, as.character)
  tmp[is.na(tmp)] <- ""

  tmp[] <- lapply(tmp[], td)

  header <- sprintf("<thead><tr>\n%s\n</tr></thead>",
                    paste(th(names(tmp)), collapse="\n"))
  tbody <-
    c("<tbody>",
      sprintf("<tr>\n%s\n</tr>", apply(tmp, 1, paste, collapse="\n")),
      "</tbody>")

  paste(c(header, tbody), collapse="\n")  
}

www.balance <- function(dat) {
  dat <- subset(dat, !Gone)

  dat <- dat[order(dat$Name),]
  
  tmp <- data.frame(Name=dat$Name,
                    Balance=dat$Balance,
                    "Total coffees"=dat$TotalCoffee,
                    "(last period)"=dat$LastCoffee,
                    "Last Paid"=dat$LastPaid,
                    "(date)"=as.character(dat$LastDate),
                    check.names=FALSE,
                    stringsAsFactors=FALSE)

  is.currency <- match(c("Balance", "Last Paid"), names(tmp))
  tmp[,is.currency] <- lapply(tmp[,is.currency], td.currency)
  tmp[,-is.currency] <- lapply(tmp[,-is.currency], td)

  header <- sprintf("<thead><tr>\n%s\n</tr></thead>",
                    paste(th(names(tmp)), collapse="\n"))
  tbody <-
    c("<tbody>",
      sprintf("<tr>\n%s\n</tr>", apply(tmp, 1, paste, collapse="\n")),
      "</tbody>")

  paste(c(header, tbody), collapse="\n")
}

www.copy <- function() {
  system("rsync -avz www/* zoology.ubc.ca:www/coffee")
}
