## The first step is moving from flat CSVs to organized data

all: SignupSheet/Signup.pdf PaymentSheet/payment.pdf

SignupSheet/accounts_active.csv: R/merge_organize_coop_data.R coffee_database/*.csv
	cd R; Rscript $(<F)

SignupSheet/signuptable.tex: R/do_accounts_database.R SignupSheet/accounts_active.csv
	cd R; Rscript $(<F)

SignupSheet/Signup.md: SignupSheet/signuptable.tex SignupSheet/signuptable.md
	cd $(<D); cat signuptable.* > $(@F)

SignupSheet/Signup.pdf: SignupSheet/Signup.md SignupSheet/tabletemplate.tex
	cd $(<D); pandoc --template=tabletemplate.tex $(<F) --latex-engine=xelatex --variable fontsize=12pt -o $(@F)

PaymentSheet/payment.pdf: PaymentSheet/payment.Rmd PaymentSheet/longtablepreamble.tex PaymentSheet/payment.R
	cd $(<D); Rscript payment.R

clean: 
	rm SignupSheet/signuptable.* SignupSheet/Signup.md