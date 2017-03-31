## The data for the coffee coop is stored in five spreadsheets
## info, which shows how prices of things change over time
## consumption, which tracks who has consumed milk or coffee, and when
## people, with information on every participant of the Coffee Coop 
## payments, which tracks all movements of cash in and out of the Coop
## goods, data on all non-monetary goods the coop produces.



## the formula for a coffee-coop person's balance is calculated as 

## all payments - consumption*cost_when_consumed + all goods - any cash payments to this person.


## first use remake to read in all the latest data
#devtools::install_github("richfitz/remake")
library(remake)
library(dplyr)

dump_environment()

# check somebody's payment history ------------------------------------------------------

people %>%
  left_join(payments) %>%
  filter(grepl("Rich",Printed.Name))


# check consumption history -----------------------------------------------

people %>%
  left_join(consumption) %>%
  filter(grepl("Rich",Printed.Name))

# check somebody's balance ------------------------------------------------

filter(accounts,grepl("Rich",Name))


# cumulative donation -----------------------------------------------------

consumption %>%
  left_join(info) %>%
  filter(CostBlack==0.35) %>%
  summarize(donation=sum(Coffee)*0.1)

