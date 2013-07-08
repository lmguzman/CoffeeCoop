## This is probably heaps easier to do with RHTML
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
sprintf2 <- function(fmt, x, ...) {
  ans <- sprintf(fmt, x, ...)
  ans[is.na(x)] <- NA
  ans
}
thead <- function(x)
  sprintf("<thead><tr>\n%s\n</tr></thead>",
          paste(th(names(x)), collapse="\n"))
tbody <- function(x)
  c("<tbody>",
    sprintf("<tr>\n%s\n</tr>", apply(x, 1, paste, collapse="\n")),
    "</tbody>")

www.info <- function(info) {
  info <- info[info$Date != "Future",]
  tmp <- data.frame(Date=as.character(info$Date, "%Y/%m/%d"),
                    "Coffees made"=info$Count,
                    "This month"=info$Used,
                    "Per day"=sprintf2("%2.2f", info$UsedPerDay),
                    "Paid for"=info$Paid,
                    "Per day"=sprintf2("%2.2f", info$PaidPerDay),
                    Honesty=sprintf2("%2.2f", info$Honesty),
                    "With milk"=info$MilkyCoffee,
                    ## "True milk cost"=sprintf2("%2.2f", info$TrueMilkCost),
                    Tea=info$Tea,
                    "Per day"=sprintf2("%2.2f", info$TeaPerDay),
                    check.names=FALSE)
  tmp[] <- sapply(tmp, as.character)
  tmp[is.na(tmp)] <- ""
  tmp[] <- lapply(tmp[], td)
  header <- thead(tmp)
  body <- tbody(tmp)
  paste(c(header, body), collapse="\n")
}

www.balance <- function(status) {
  ## TODO: Subset by !Gone?
  tmp <- data.frame(Name=status$Name,
                    Balance=status$Balance,
                    "Total coffees"=status$TotalCoffee,
                    "(last period)"=status$LastCoffee,
                    "Total teas"=status$TotalTea,
                    "(last period)"=status$LastTea,
                    "Last Paid"=status$LastPaid,
                    "(date)"=as.character(status$LastDate),
                    check.names=FALSE, stringsAsFactors=FALSE)
  is.currency <- match(c("Balance", "Last Paid"), names(tmp))
  tmp[,is.currency] <- lapply(tmp[,is.currency], td.currency)
  tmp[,-is.currency] <- lapply(tmp[,-is.currency], td)

  header <- thead(tmp)
  tbody <- tbody(tmp)
  paste(c(header, tbody), collapse="\n")  
}

www <- function(dat) {
  template <- paste(readLines("www/template.html"), collapse="\n")
  str <- sprintf(template,
                 www.info(dat$info),
                 www.balance(dat$status))
  writeLines(str, "www/index.html")                 
}

## Copy the files over to the server.
www.copy <- function() {
  system("rsync -avz www/* zoology.ubc.ca:www/coffee")
}

