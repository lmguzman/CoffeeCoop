## The first step is moving from flat CSVs to organized data

all: SignupSheet/Signup.pdf PaymentSheet/payment.pdf ReportOnTheCoop/Report.pdf

SignupSheet/signuptable.md: SignupSheet/signuptable.Rmd
	cd $(<D); Rscript -e "library(rmarkdown); render('signuptable.Rmd')"  

SignupSheet/Signup.md: SignupSheet/signuptable.tex SignupSheet/signuptable.md
	cd $(<D); cat signuptable.md signuptable.tex > $(@F)

SignupSheet/Signup.pdf: SignupSheet/Signup.md SignupSheet/tabletemplate.tex
	cd $(<D); ~/.cabal/bin/pandoc --template=tabletemplate.tex $(<F) --latex-engine=xelatex --variable fontsize=12pt -o $(@F)

PaymentSheet/payment.pdf: PaymentSheet/payment.Rmd PaymentSheet/longtablepreamble.tex PaymentSheet/payment.R
	cd $(<D); Rscript payment.R

ReportOnTheCoop/Report.pdf: ReportOnTheCoop/Report.Rmd coffee_database/info.csv coffee_database/consumption.csv SignupSheet/accounts_active.csv
	cd $(<D); Rscript -e "library(rmarkdown); render('Report.Rmd')"

clean: 
	rm SignupSheet/signuptable.tex SignupSheet/Signup.md