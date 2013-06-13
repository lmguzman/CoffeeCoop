## The data for the coffee coop is stored in five spreadsheets
## info, which shows how prices of things change over time
## consumption, which tracks who has consumed milk or coffee, and when
## people, with information on every participant of the Coffee Coop 
## payments, which tracks all movements of cash in and out of the Coop
## goods, data on all non-monetary goods the coop produces.

## the formula for a coffee-coop person's balance is calculated as 

## all payments - consumption*cost_when_consumed + all goods - any cash payments to this person.

## load libraries
library(data.table)

## read data
consumption <- read.csv(file="coffee_database/consumption.csv")
payments <- read.csv(file="coffee_database/payments.csv")

# consumption_dt <- data.table(consumption,key=c("data_date","ID"))
# info_dt <- data.table(info,key="Date")
# people_dt <- data.table(people,key="ID")
# 
# info_dt["2010-08-04"]
# consumption_dt[info_dt]
# 
# consumption_dt[info_dt,CostBlack*Coffee+Tea*CostMilk]
# 
# 
# pplmoney <- consumption_dt[info_dt,list(ID,CoffeeCost=CostBlack*Coffee,CostMilk=Tea*CostMilk)]
# pplmoney[,list(sum(CoffeeCost),sum(CostMilk)),by=ID]
# 
# merge(consumption_dt,people_dt,by="ID")
# 
# consumption_dt[J(people$ID)]
# 
# consumption_dt[J("2013-04-22",221)]
# 
# 
# consumption_dt[J("",221)]
# consumption_dt[people]

## this ends with the data all nicely organized into a sheet showing Milk and Coffee consumption.