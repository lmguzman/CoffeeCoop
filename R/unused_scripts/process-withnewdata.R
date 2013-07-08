## 1) LOAD NEW DATA
# All the new data must be in the data/Future.csv file
# Formatted as
# Name,Payment,Payment.Date,Milk,Coffee,Tea
# where Milk is logical (TRUE when people have milk, no tick)
# 
# New members must have been registered in the data/people.csv file
# There must be a line for them, with a coop ID number
#  (highest number at the bottom of the file), which is a unique
# identifier, so make sure not to duplicate it. 

setwd('')

include.newdata()


