## Read in a datasheet of the active co-op users


## convert numbers to text for formatting later

xtable(accounts_formatted,
       align=c("|","l","|","p{5cm}","|","r","|","p{9cm}","|","p{6cm}","|","l","|")
       ) %>%
  print(type='latex',sanitize.text.function=identity,
        tabular.environment="longtable",hline.after=1:nrow(accounts_formatted),floating=FALSE,
        include.rownames=FALSE,
        add.to.row = list(pos = list(0),
                          command = c("\\hline \\endhead ")),
        comment=FALSE) %>%
  cat(file = "../SignupSheet/signuptable.tex")


## this ends with the data all nicely organized into a sheet showing Milk and Coffee consumption.
