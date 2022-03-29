
# Set this working directory to where you want to produce .html file
setwd("/Users/andywilson1/Documents/GitHub/Jenny_Informatics_class/Session 2")

#install.packages("devtools")
#library(devtools)

# devtools::install_github("mattflor/chorddiag")
library(chorddiag)


set.seed(999)
mat = matrix(sample(18, 18), 6, 6) 
rownames(mat) = paste0("Rx", 1:6)
colnames(mat) = paste0("Rx", 1:6)
mat


p <- chorddiag(mat, groupnamePadding = 20)
p


# save the widget

library(htmlwidgets)
saveWidget(p, file="chord_interactive.html")