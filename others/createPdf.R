library(knitr)

knit('workshopProposal.Rmd', 'workshopProposal.md')
pandoc('workshopProposal.md', 'latex', config="pandoc.cfg")
