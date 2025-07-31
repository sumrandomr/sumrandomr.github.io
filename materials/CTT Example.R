### A simple example of using the CTT package for analysis

# Load the library (install it if you haven't yet)
library(CTT)

# Load the scored responses as 'resp'
resp <- read.csv("Example.csv") # Change the path to where it's saved, if not already in the working directory

## Note: The CTT package can score raw responses, if needed. See ?score() for examples

#Run CTT item analysis
IA <- itemAnalysis(resp,
                   easyFlag=.90, # Option that will flag easy items
                   hardFlag=.25, # Option that will flag hard items
                   pBisFlag=.15) # Option that will flag low discrimination

#See some quick summary statistics (# items, # respondents, Alpha)
IA

#See the Item Report
IA$itemReport 

#Save the Item Report as a spreadsheet
write.csv(IA$itemReport,"CTTItemAnalysis.csv", row.names = FALSE)               