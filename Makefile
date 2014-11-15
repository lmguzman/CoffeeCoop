## The first step is moving from flat CSVs to organized data

all: SignupSheet/Signup.md 

SignupSheet/accounts_active.csv: R/merge_organize_coop_data.R coffee_database/*.csv
	cd R; Rscript $(<F)

SignupSheet/signuptable.tex: R/do_accounts_database.R SignupSheet/accounts_active.csv
	cd R; Rscript $(<F)

SignupSheet/Signup.md: SignupSheet/signuptable.tex SignupSheet/signuptable.md
	cd $(<D); cat signuptable.* > Signup.md 