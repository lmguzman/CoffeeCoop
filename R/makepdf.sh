
Rscript -e "library(knitr); knit('do_accounts_database.Rmd')"
~/.cabal/bin/pandoc --template=tabletemplate.tex do_accounts_database.md --latex-engine=xelatex -o signup_sheet.pdf
Rscript -e "library(knitr); knit('report_coop_usage.Rmd')"
~/.cabal/bin/pandoc report_coop_usage.md --latex-engine=xelatex -o report_coop_usage.pdf
