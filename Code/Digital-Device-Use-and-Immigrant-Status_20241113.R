## ----setup----------------------------------------------------------------------------------------
#| include: false

#libraries
library(tidyverse)
library(kableExtra)
library(formattable)
library(DescTools)

#Functions
# source("pisa-pv.R")
source("functions/pisa-brr-ppc.R")
# source("wght_lmpv.R")

#Data
df <-  read.csv("DATA/IELS database.csv")

#### Trimming df to a subset to make things a bit more manageable. 
IELS_DeviceUse_INT <- df %>%
  select(IDCNTRY,
         CNTRY,
         CHILDWGT,
         IMMIG,
         STUD_LANG,
         SENWGT,
         SES,
         SES_INT,
         ELPAQ0401,
         ELLITPV1:ELLITPV5,
         ELNUMPV1:ELNUMPV5,
         SRWGT1:SRWGT92) %>%
  mutate(ELPAQ0401 = as.factor(ELPAQ0401)) %>%
  filter(ELPAQ0401 !=7 & ELPAQ0401 !=9) %>%
  filter(IMMIG !=7 & IMMIG !=9) %>%
  droplevels

IELS_DeviceUse_EST <- IELS_DeviceUse_INT %>%
  filter(IDCNTRY == 233)

IELS_DeviceUse_USA <- IELS_DeviceUse_INT %>%
  filter(IDCNTRY == 840)

IELS_DeviceUse_ENG <- IELS_DeviceUse_INT %>%
  filter(IDCNTRY == 926)

filtered_data_ESCS <- read.csv("DATA/PISA_DeviceUse_INT.csv") %>%
  select(ESCS, ST326Q01JA, ST326Q02JA, ST326Q03JA, ST326Q04JA, ST326Q05JA, ST326Q06JA, SENWT) %>%
  filter(ST326Q01JA %in% 1:9,
         ST326Q02JA %in% 1:9,
         ST326Q03JA %in% 1:9,
         ST326Q04JA %in% 1:9,
         ST326Q05JA %in% 1:9,
         ST326Q06JA %in% 1:9) %>%
  na.omit()

gc()

filtered_data_IMMIG <- read.csv("DATA/PISA_DeviceUse_INT.csv") %>%
  select(IMMIG, ST326Q01JA, ST326Q02JA, ST326Q03JA, ST326Q04JA, ST326Q05JA, ST326Q06JA, SENWT) %>%
  filter(IMMIG %in% 1:3,
         ST326Q01JA %in% 1:9,
         ST326Q02JA %in% 1:9,
         ST326Q03JA %in% 1:9,
         ST326Q04JA %in% 1:9,
         ST326Q05JA %in% 1:9,
         ST326Q06JA %in% 1:9) %>%
  na.omit()

gc()

IMMIG_DeviceUse <- read.csv("DATA/PISA_DeviceUse_INT.csv") %>%
  select(ESCS, IMMIG, ST326Q01JA, ST326Q02JA, ST326Q03JA, ST326Q04JA, ST326Q05JA, ST326Q06JA, SENWT, W_FSTURWT1:W_FSTURWT80) %>%
  filter(IMMIG %in% 1:3,
         ST326Q01JA %in% 1:9,
         ST326Q02JA %in% 1:9,
         ST326Q03JA %in% 1:9,
         ST326Q04JA %in% 1:9,
         ST326Q05JA %in% 1:9,
         ST326Q06JA %in% 1:9)  %>%
  na.omit()

gc()



## ----IELSdescriptives-----------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

#IELS Descriptives

IELS_DeviceUse_by_IMMIG <- wght_ppccrossed(sdata = IELS_DeviceUse_INT,
                                      rppc = "IMMIG",
                                      cppc = "ELPAQ0401", 
                                      wght = "SENWGT",
                                      brr = paste0("SRWGT", 1:92))

## Table1 could be excluded/streamlined, but I'm leaving it here because it might be useful to be able to show this table at some point, so it's here in the background.
IELS_PPC_Table1 <- IELS_DeviceUse_by_IMMIG$PPC %>%
  as.data.frame() %>%
  select("ELPAQ0401-4","ELPAQ0401-3","ELPAQ0401-2","ELPAQ0401-1")%>% #reorder cols
  bind_rows(., summarise_all(., sum))%>% #add total
  mutate(Total = rowSums(.[1:4])) %>% # add total column
  lapply(percent, 1) %>% #formatting
  as.data.frame() #return to df

IELS_PPC_Table2 <- IELS_PPC_Table1 %>%
  select(-Total) %>% # Exclude the "Total" column
  mutate(across(everything(), ~ . / IELS_PPC_Table1$Total)) %>% # Divide each column by the "Total" column
  lapply(percent, 1) %>% #formatting
  as.data.frame() %>% # Return to data frame
  slice(1:2) # Exclude the "Total" row
# Set row and column names
rownames(IELS_PPC_Table2) <- c("Native", "Immigrant")
colnames(IELS_PPC_Table2) <- c("Every Day","Weekly", "Monthly", "Rarely")

kbl(IELS_PPC_Table2, caption = "Use of digital devices among five-year-olds by Immigrant Status") %>%
  kable_classic_2()%>%
  footnote(general = "IELS. OECD, 2020.", general_title = "Data:")


## ----PreliminIELS1--------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

######### Effect of IELS SES on Frequency of Device Use
filtered_data_IELS <- IELS_DeviceUse_INT %>%
  select(SES_INT, ELPAQ0401, SENWGT)%>%
  na.omit()

anova_IELS <- aov(SES_INT ~ ELPAQ0401, data = filtered_data_IELS, weights = SENWGT)
# Calculate Partial Eta Squared
eta_squared_IELS <- EtaSq(anova_IELS, type = 2)
Presults_IELS1 <- data.frame(
  Variable = "ELPAQ0401",
  n =  nrow(filtered_data_IELS),
  F_Value = summary(anova_IELS)[[1]][1,4],
  d.f. = summary(anova_IELS)[[1]][1,1],
  P_Value = summary(anova_IELS)[[1]][1,5],
  P.Eta_Squared = eta_squared_IELS[,2])

### Results
kbl(Presults_IELS1, caption = "IELS effect size of SES on frequency of device use", digits = 4) %>%
  kable_classic_2(full_width = F)%>%
  footnote(general = "IELS. OECD, 2020.", general_title = "Data:")


## ----PrelimIELS2----------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false


library(weights)
library(vcd)

########### Effect of IELS IMMIG on Frequency of Device Use
# Filter and select relevant data
filtered_data_IELS <- IELS_DeviceUse_INT %>%
  select(IMMIG, ELPAQ0401, SENWGT) %>%
  na.omit()

# Create a weighted contingency table using xtabs
weighted_table <- xtabs(SENWGT ~ IMMIG + ELPAQ0401, data = filtered_data_IELS)

# Calculate expected frequencies
row_totals <- rowSums(weighted_table)
col_totals <- colSums(weighted_table)
total <- sum(weighted_table)
expected_table <- outer(row_totals, col_totals) / total

# Perform Chi-Square test
chi_test <- chisq.test(weighted_table)

# Calculate weighted Cramér's V
weighted_cramers_v <- confintr::cramersv(weighted_table)

# Create a data frame to store the results
Presults_IELS2 <- data.frame(
  row.names = NULL,
  Variable = c("ELPAQ0401"),
  n = nrow(filtered_data_IELS),
  Chi_Square = chi_test$statistic,
  Degrees_of_Freedom = chi_test$parameter,
  P_Value = chi_test$p.value,
  Cramers_V = weighted_cramers_v
)

# Results
kbl(Presults_IELS2, caption = "IELS effect size of IMMIG and frequency of device use", digits = 4) %>%
  kable_classic_2(full_width = F) %>%
  footnote(general = "IELS. OECD, 2020.", general_title = "Data:")



## ----PISADescriptives-----------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

#using the CNT function, but inputting IMMIG instead of CNT.
DeviceUseVariables <- c("ST326Q01JA", "ST326Q02JA", "ST326Q03JA", "ST326Q04JA", "ST326Q05JA", "ST326Q06JA")
VariableDescriptions <- c("learning activities at school",
                          "learning activities before and after school",
                          "learning activities on weekends",
                          "leisure at school",
                          "leisure before and after school",
                          "leisure on weekends")

# Assign names to DeviceUseVariables
names(DeviceUseVariables) <- VariableDescriptions

# Initialize an empty list to store the tables
tables <- list()

for (i in seq_along(DeviceUseVariables)) {
  var <- DeviceUseVariables[i]
  desc <- names(DeviceUseVariables)[i]
  
  # Calculate DeviceUse_by_IMMIG for the current variable
  DeviceUse_by_IMMIG <- wght_ppc_bycnt(IMMIG_DeviceUse, "IMMIG", var, "SENWT", paste0("W_FSTURWT", 1:80))
  
  # Extract the first table from the list
  PPC_table <- as.data.frame(DeviceUse_by_IMMIG[[1]]) %>%
    lapply(scales::percent, 1) %>% # formatting
    as.data.frame() # return to df for kbl()
  
  # Fix Names
  colnames(PPC_table) <- c("None", "Up to 1 hour", "1 to 2 hours", "2 to 3 hours",
                           "3 to 4 hours", "4 to 5 hours", "5 to 6 hours",
                           "6 to 7 hours", "More than 7 hours")
  rownames(PPC_table) <- c("Native", "Second-Generation", "First-Generation")
  
  # Store the table in the list
  tables[[desc]] <- PPC_table
}

# Render the tables
kbl(tables[["learning activities at school"]],
    caption = "This school year, how many hours/day use digital resources for: learning activities at school") %>%
    kable_classic_2(full_width = F)%>%
  footnote(general = "PISA. OECD, 2022.", general_title = "Data:")

kbl(tables[["learning activities before and after school"]],
     caption = "This school year, how many hours/day use digital resources for: learning activities before and after school") %>%
    kable_classic_2(full_width = F)%>%
  footnote(general = "PISA. OECD, 2022.", general_title = "Data:")

kbl(tables[["learning activities on weekends"]],
     caption = "This school year, how many hours/day use digital resources for: learning activities on weekends") %>%
    kable_classic_2(full_width = F)%>%
  footnote(general = "PISA. OECD, 2022.", general_title = "Data:")

kbl(tables[["leisure at school"]],
     caption = "This school year, how many hours/day use digital resources for: leisure at school") %>%
    kable_classic_2(full_width = F)%>%
  footnote(general = "PISA. OECD, 2022.", general_title = "Data:")

kbl(tables[["leisure before and after school"]],
     caption = "This school year, how many hours/day use digital resources for: leisure before and after school") %>%
    kable_classic_2(full_width = F)%>%
  footnote(general = "PISA. OECD, 2022.", general_title = "Data:")

kbl(tables[["leisure on weekends"]],
     caption = "This school year, how many hours/day use digital resources for: leisure on weekends") %>%
    kable_classic_2(full_width = F)%>%
  footnote(general = "PISA. OECD, 2022.", general_title = "Data:")


## ----PrelimPISA1----------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false


## Let's Have a look to check on 1: we'll look for associations between ESCS
# and : ST326Q02JA, ST326Q03JA, ST326Q05JA, and ST326Q06JA

anova_02 <- aov(ESCS ~ ST326Q02JA, data = filtered_data_ESCS, weights = SENWT)
anova_03 <- aov(ESCS ~ ST326Q03JA, data = filtered_data_ESCS, weights = SENWT)
anova_05 <- aov(ESCS ~ ST326Q05JA, data = filtered_data_ESCS, weights = SENWT)
anova_06 <- aov(ESCS ~ ST326Q06JA, data = filtered_data_ESCS, weights = SENWT)

# Calculate Eta Squared
eta_squared_02 <- EtaSq(anova_02, type = 2)
eta_squared_03 <- EtaSq(anova_03, type = 2)
eta_squared_05 <- EtaSq(anova_05, type = 2)
eta_squared_06 <- EtaSq(anova_06, type = 2)

# Create a data frame to store the results
Presults_PISA1 <- data.frame(
  Variable = c("Learning activities before/after school",
               "Learning activities on weekends",
               "Leisure before/after school",
               "Leisure on weekends"),
  n =  rep_len(nrow(filtered_data_ESCS), 4),
  F_Value = c(summary(anova_02)[[1]][1,4], summary(anova_03)[[1]][1,4], summary(anova_05)[[1]][1,4], summary(anova_06)[[1]][1,4]),
  P_Value = c(summary(anova_02)[[1]][1,5], summary(anova_03)[[1]][1,5], summary(anova_05)[[1]][1,5], summary(anova_06)[[1]][1,5]),
  P.Eta_Squared = c(eta_squared_02[,2], eta_squared_03[,2], eta_squared_05[,2], eta_squared_06[,2])
  )

# Results
kbl(Presults_PISA1, caption = "PISA association of ESCS and use of devices for learning and leisure", digits = 4) %>%
  kable_classic_2(full_width = F)%>%
  footnote(alphabet = "With such a large number of cases, P-values are essentially 0",
           general = "PISA. OECD, 2023.", general_title = "Data:")


## ----PrelimPISA2----------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

## Let's Have a look to check on 2: we'll look for associations between IMMIG
# and : ST326Q02JA, ST326Q03JA, ST326Q05JA, and ST326Q06JA

# Load necessary library
library(survey)
library(Matrix)

# Define a function to calculate Cramér's V
cramers_v <- function(chi_test, contingency_table) {
  sqrt(chi_test$statistic / (sum(contingency_table) * min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)))
}

# Create individual contingency tables
contingency_table_02 <- table(filtered_data_IMMIG$IMMIG, filtered_data_IMMIG$ST326Q02JA)
contingency_table_03 <- table(filtered_data_IMMIG$IMMIG, filtered_data_IMMIG$ST326Q03JA)
contingency_table_05 <- table(filtered_data_IMMIG$IMMIG, filtered_data_IMMIG$ST326Q05JA)
contingency_table_06 <- table(filtered_data_IMMIG$IMMIG, filtered_data_IMMIG$ST326Q06JA)

# Create survey design object
svy_design <- svydesign(ids = ~1, data = filtered_data_IMMIG, weights = ~SENWT)

# Perform weighted Chi-Square tests
chi_test_02 <- svychisq(~IMMIG + ST326Q02JA, svy_design)
chi_test_03 <- svychisq(~IMMIG + ST326Q03JA, svy_design)
chi_test_05 <- svychisq(~IMMIG + ST326Q05JA, svy_design)
chi_test_06 <- svychisq(~IMMIG + ST326Q06JA, svy_design)

# Calculate Cramér's V using the function
cramers_v_02 <- cramers_v(chi_test_02, contingency_table_02)
cramers_v_03 <- cramers_v(chi_test_03, contingency_table_03)
cramers_v_05 <- cramers_v(chi_test_05, contingency_table_05)
cramers_v_06 <- cramers_v(chi_test_06, contingency_table_06)

# Create a data frame to store the results
Presults_PISA2 <- data.frame(
  Variable = c("Learning activities before/after school",
               "Learning activities on weekends",
               "Leisure before/after school",
               "Leisure on weekends"),
  n = c(sum(contingency_table_02), sum(contingency_table_03), sum(contingency_table_05), sum(contingency_table_06)),
  Chi_Square = c(chi_test_02$statistic, chi_test_03$statistic, chi_test_05$statistic, chi_test_06$statistic),
  Degrees_of_Freedom = c(chi_test_02$parameter[1], chi_test_03$parameter[1], chi_test_05$parameter[1], chi_test_06$parameter[1]),
  P_Value = c(chi_test_02$p.value, chi_test_03$p.value,chi_test_05$p.value,chi_test_06$p.value),
  Cramers_V = c(cramers_v_02, cramers_v_03, cramers_v_05, cramers_v_06)
)

# Results
kbl(Presults_PISA2, caption = "PISA association of IMMIG and use of devices for learning and leisure") %>%
  kable_classic_2(full_width = F) %>%
  footnote(alphabet = "With such a large number of cases, P-values are essentially 0",
           general = "PISA. OECD, 2023.", general_title = "Data:")



## ----IELS_PlotPrep--------------------------------------------------------------------------------
#| include: false

library(survey)

########### Effect of IMMIG on Frequency of Device Use, controlling for SES

survey_design <- svydesign(ids = ~1, data = IELS_DeviceUse_INT, weights = ~SENWGT)
ThumbsPressed <- svyolr(ELPAQ0401 ~ IMMIG + SES_INT, design = survey_design)

# Summary statistics
summary(ThumbsPressed)

# Goodness of Fit: Likelihood Ratio Test
LRT_Thumbspressed <- regTermTest(ThumbsPressed, ~IMMIG + SES_INT, method = "LRT")

# Adding P-Values to Coef
coef_table <- summary(ThumbsPressed)$coefficients
p_values <- 2 * (1 - pnorm(abs(coef_table[, "Value"] / coef_table[, "Std. Error"])))
coef_table <- cbind(coef_table, p_values)
print(coef_table)

# Predict probabilities
predicted_probs <- predict(ThumbsPressed, type = "probs")

# Create a data frame for plotting
plot_data <- data.frame(
  SES_INT = IELS_DeviceUse_INT$SES_INT,
  IMMIG = IELS_DeviceUse_INT$IMMIG,
  ELPAQ0401 = IELS_DeviceUse_INT$ELPAQ0401,
  Predicted_Prob_3_4 = predicted_probs[, "3"] + predicted_probs[, "4"],  # Probability of being in categories 3 or 4
  Predicted_Prob_4 = predicted_probs[, "4"]  # Probability of being in category 4
)

# Add Labels to IMMIG in plot_data
plot_data$IMMIG <- factor(plot_data$IMMIG, levels = c(0, 1), labels = c("Native", "Immigrant"))



## ----IELS_PredPlot1_WeeklyOrMore------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

# Plot the predicted probabilities for 'at least Weekly'
ggplot(plot_data, aes(x = SES_INT, y = Predicted_Prob_3_4, color = IMMIG)) +
  geom_line() +
  labs(title = "Difference in predicted probability of using a device at least weekly",
       x = "IELS International SES Index",
       y = "Predicted probability",
       color = "Immigrant Status") +
  theme_minimal() +
  ylim(0, 1)  # Rescale y-axis to show 0 to 1



## ----IELS_PredPlot2_Daily-------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

# Plot the predicted probabilities for Daily Use only
ggplot(plot_data, aes(x = SES_INT, y = Predicted_Prob_4, color = IMMIG)) +
  geom_line() +
  labs(title = "Difference in predicted probability of using a device daily",
       x = "IELS International SES Index",
       y = "Predicted probability",
       color = "Immigrant Status") +
  theme_minimal() +
  ylim(0, 1)  # Rescale y-axis to show 0 to 1


## ----PISA-----------------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false



