#######################################################################################################################

# Unload the packages needed.
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyverse)
library(tidyr)

#######################################################################################################################

# Deliverable One
# Read in the CSV files needed.
MechaCar_MPG_DF <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

# Use the lm() function to determine the linear regression of our data set.
lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD + mpg, data=MechaCar_MPG_DF)

# Use the summary() function to determine all statistical values of our data set.
summary(lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD + mpg, data=MechaCar_MPG_DF))

#######################################################################################################################

# Deliverable Two
# Read in the CSV file needed.
Suspension_Coil_Table <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

#Summary of Totals
Summary_Of_Totals = Suspension_Coil_Table %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups='keep')
Summary_Of_Totals

# Summary of Manufacture Lots
Summary_Of_Lots = Suspension_Coil_Table %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups='keep')
Summary_Of_Lots

#######################################################################################################################

# Deliverable Three
# PSI t test for all Manufacturing Lots.
t.test(Suspension_Coil_Table$PSI, mu = 1500)

# PSI t test for Manufacturing Lot One.
Data_Lot_One = subset(Suspension_Coil_Table, Suspension_Coil_Table$Manufacturing_Lot == 'Lot1')
t.test(Data_Lot_One$PSI, mu = 1500)

# PSI t test for Manufacturing Lot Two.
Data_Lot_Two = subset(Suspension_Coil_Table, Suspension_Coil_Table$Manufacturing_Lot == 'Lot2')
t.test(Data_Lot_Two$PSI, mu = 1500)

# PSI t test for Manufacturing Lot Tree.
Data_Lot_Three = subset(Suspension_Coil_Table, Suspension_Coil_Table$Manufacturing_Lot == 'Lot3')
t.test(Data_Lot_Three$PSI, mu = 1500)

#######################################################################################################################