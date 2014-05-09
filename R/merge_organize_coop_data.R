## The data for the coffee coop is stored in five spreadsheets
## info, which shows how prices of things change over time
## consumption, which tracks who has consumed milk or coffee, and when
## people, with information on every participant of the Coffee Coop 
## payments, which tracks all movements of cash in and out of the Coop
## goods, data on all non-monetary goods the coop produces.



## the formula for a coffee-coop person's balance is calculated as 

## all payments - consumption*cost_when_consumed + all goods - any cash payments to this person.


# load libraries ----------------------------------------------------------
library(plyr)
library(dplyr)
library(xtable)
library(lubridate)


# read data ---------------------------------------------------------------

consumption <- read.csv(file="../coffee_database/consumption.csv",stringsAsFactors=FALSE)
payments <- read.csv(file="../coffee_database/payments.csv",stringsAsFactors=FALSE)
info <- read.csv(file="../coffee_database/info.csv",stringsAsFactors=FALSE)
people  <- read.csv(file="../coffee_database/people.csv",stringsAsFactors=FALSE)
goods <- read.csv(file="../coffee_database/goods.csv",stringsAsFactors=FALSE)
## use Base functions
## convert data columns to same formats

consumption <- mutate(consumption,Date=ymd(data_date))
info <- mutate(info,Date=ymd(Date))

## did you update `info`?
delay <- info$Date %.%
  max() %.%
  difftime(max(consumption$Date),units="days")

if(delay!=0) stop(message("did you update info?"))

# merge for calculating balances ------------------------------------------

## First, We merge on date and calculate cost of coffee and milk
consumption <- tbl_df(consumption)

money_owed <- consumption %.%
  left_join(info) %.%
  mutate(owing=CostBlack*Coffee+Milk*CostMilk) %.%
  group_by(ID)%.%
  summarise(owing_total=sum(owing,na.rm=TRUE))
  

money_paid <- payments %.%
  group_by(ID) %.%
  summarize(paid_total=sum(Payment))

goods_bought <- goods %.%
  group_by(ID) %.%
  summarize(GoodsCredit=sum(Cost))
  
## combine please

### did anyone drink and *never* pay?
anti_join(money_owed,money_paid) %.%
  left_join(people)

### less problematically; did anyone pay but not drink?
pay.not.drink <- anti_join(money_paid,money_owed) %.%
  nrow()

if(pay.not.drink>0) stop(message("somebody paid but did not drink."))

## otherwise let's go ahead
accounts <- left_join(money_owed,money_paid) %.%
  left_join(people) %.%
  filter(!Gone) %.%
  ## some of the remaining (not gone people) have not paid  
  mutate(paid_total_0=ifelse(is.na(paid_total),0,paid_total)) %.%
  left_join(goods_bought) %.%
  mutate(GoodsCredit_0=ifelse(is.na(GoodsCredit),0,GoodsCredit),
         balance=GoodsCredit_0+paid_total_0-owing_total,
         balance=ifelse(ID%in%c(18,214),0,balance)) %.%
  select(ID,Printed.Name,balance) %.%
  rename(c("Printed.Name"="Name")) %.%
  arrange(Name)

## and here we can stop the filtering and merging.  accounts_alphabet contains all the info now.
##ls()[!ls()%in%"accounts_alphabet"]
## we **COULD** remove all the other files but I don't want to.

## When did people last use the coop?

active <- consumption %.%
  select(ID,Date) %.%
  group_by(ID) %.%
  summarise(lastday_drank=max(Date)) %.%
  filter(lastday_drank>(now()-dweeks(8))
         )

accounts_active <- semi_join(accounts,active) %.%
  arrange(Name)

accounts_passive <- anti_join(accounts,active) %.%
  arrange(Name)
## not run

if(FALSE){
# check somebody's payment history ------------------------------------------------------

people %.%
  left_join(payments) %.%
  filter(grepl("Andrew",Printed.Name))


# check somebody's balance ------------------------------------------------

filter(accounts,grepl("Amy",Name))


# cumulative donation -----------------------------------------------------

consumption %.%
  left_join(info) %.%
  filter(CostBlack==0.35) %.%
  summarize(donation=sum(Coffee)*0.1)
}