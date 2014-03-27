pandoc -f markdown+grid_tables -t latex-simple_tables+grid_tables-simple_tables --latex-engine=xelatex --template=mytemplate.latex -o Payment.pdf Payment_List.md

