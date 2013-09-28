



```r
library(ggplot2)
source("merge_organize_coop_data.R")
```


# Report on the state of the Coffee Co-op

```r
since_last <- difftime(info$Date[length(info$Date)], info$Date[length(info$Date) - 
    1], units = "days")
consumed_coffees <- info$Count[length(info$Count)] - info$Count[length(info$Count) - 
    1]
marked_coffees <- sum(consumption[which(consumption$data_date == max(info$Date)), 
    "Coffee"])
```


It has been 32 days since our sheet was updated. In that time we drank **1598 coffees**, of which **1459** were marked down; making our honesty during this period **91%**

## The distribution of co-op balances:
Among users currently present in the building, **35%** owe the co-op money, representing a total debt of **-370.45**. If we consider all users **EVER** in the co-op, the total debt is **-557.4**.

**Please come and pay Andrew MacDonald (203).  Let's try to improve these numbers!**

## milk consumption
we drank 648 milks, which altogether cost us 97.2.


```r
dd <- with(density(accounts_alphabet$balance), data.frame(x, y))

ggplot(data = dd, mapping = aes(x = x, y = y)) + geom_line(color = "black") + 
    geom_polygon(data = dd, aes(x = ifelse(x < 0, x, 0), y = y)) + scale_y_continuous(limits = c(0, 
    max(dd$y)), name = "Density") + scale_x_continuous(name = "Balance ($)")
```

![The distribution of balances](figure/unnamed-chunk-4.png) 

