## The first step is moving from flat CSVs to organized data
SignupSheet/accounts_active.csv: R/merge_organize_coop_data.R coffee_database/%.csv
    cd R;R CMD BATCH merge_organize_coop_data.R