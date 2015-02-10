# UBC Biodiversity Coffee Co-op

[![Build Status](https://travis-ci.org/aammd/CoffeeCoop.png?branch=master)](https://travis-ci.org/aammd/CoffeeCoop)

This repository contains all the data on coffee consumption for the UBC biodiversity building coffee co-op.  The co-op is currently managed by Andrew MacDonald.


### Compiling updated lists

This project uses [remake](https://github.com/richfitz/remake), an R-ish version of Make created by @richfitz. 

In order to recreate the summary information about our Coffee co-op, you must have `remake` installed. Then, simply run

```r
remake::make()
```


This will produce three documents:

* **SignupSheet/signuptable.pdf**: The signup table that everybody uses to mark down how much coffee or milk they drink

* **PaymentSheet/payment.pdf**: A sheet which the Coffee Baron (Andrew) uses to write down when you pay for coffee.  This is necessary because Andrew is an ecologist, and if data doesn't start life on paper he doesn't realy know what to do with it.

* **ReportOnTheCoop/Report.pdf**: an automagic document created with knitr and rmarkdown, which gives you up-to-the-minute info on the co-op. 
