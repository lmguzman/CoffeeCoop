## Read in a datasheet of the active co-op users
accounts_active <- read.csv("../SignupSheet/accounts_active.csv", stringsAsFactors = FALSE)

library(dplyr)
library(xtable)
library(magrittr)

## convert numbers to text for formatting later

accounts_formatted <- accounts_active %>%
  mutate(balance_text=sprintf("%.2f", balance),
         balance_text_format=ifelse(balance<0,
                                    paste0("\\textbf{",balance_text,"}"),
                                    balance_text),
         Name=ifelse(balance<0,
                     paste0("\\textbf{",Name,"}"),
                     Name)) %>%
  cbind(data.frame(Coffee="",Milk="")) %>%
  select(Name,balance_text_format,Coffee,Milk,ID) %>%
  set_names(c("Name","balance","\\textbf{Coffee}","\\textbf{Milk}","ID"))

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
