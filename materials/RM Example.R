### A simple example of using TAM for a Rasch Analaysis of some example data
###

# Load TAM (install it first, if you haven't yet)
library(TAM)

resp <- read.csv("Example.csv") #Change path to where the file is, if it's not in the working directory

#Run RM analysis
mod <- tam.jml(resp,
               constraint = "items") # Constraint: "items" centres the scale on mean item difficulty, rather than mean student ability.

#Get item fit statistics
fit <- tam.jml.fit(mod)

#Save the Item Fit statistics as a spreadsheet
write.csv(fit$fit.item,"Item Fit Example.csv", row.names = FALSE)               

# Create ICCs (saves them in a folder automatically)
for (i in 1:mod$nitems){
  plot(mod,items=i, 
       type = "expected",
       low = -4,
       high = 4,
       ngroups = 4)
}

# Wright Mapping
library(WrightMap)
WM1 <- wrightMap(mod$WLE, mod$item$xsi.item, item.side=itemClassic)

