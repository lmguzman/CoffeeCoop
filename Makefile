## The first step is moving from flat CSVs to organized data

all: SignupSheet/Signup.pdf PaymentSheet/payment.pdf ReportOnTheCoop/Report.pdf

ReportOnTheCoop/Report.pdf: ReportOnTheCoop/Report.Rmd coffee_database/info.csv coffee_database/consumption.csv SignupSheet/accounts_active.csv
	cd $(<D); Rscript -e "library(rmarkdown); render('Report.Rmd')"

clean: 
	rm SignupSheet/signuptable.tex SignupSheet/Signup.md