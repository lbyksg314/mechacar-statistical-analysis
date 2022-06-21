#import library and read file
library(dplyr)
mec_df <- read.csv('MechaCar_mpg.csv', stringsAsFactors = F)

#perform line reg model
lm(mpg ~vehicle_length + vehicle_weight + spoiler_angle + AWD + ground_clearance, data=mec_df)

#determine p-value and r-sqr
summary(lm(mpg ~vehicle_length + vehicle_weight + spoiler_angle + AWD + ground_clearance, data=mec_df))


#Technical Analysis
##import data and read as table
table_tech <- read.csv('Suspension_Coil.csv', stringsAsFactors = F)

#create total_summary df, with mean, median, variance, sd
total_summary <- summarize(table_tech, Mean=mean(PSI),Median= median(PSI),Variance=var(PSI),SD= sd(PSI))

#create lot_summary() df by using group_by() and summarize()

lot_summary <- table_tech%>%group_by(Manufacturing_Lot)%>%summarize(Mean=mean(PSI),Median= median(PSI),Variance=var(PSI),SD= sd(PSI),.groups = 'keep')


#use t.test() to determine if the PSI across all manuf. lots is 
#statistically diff from pop.mean of 1,500 pounds per sq. inch.
t.test(table_tech$PSI, mu=1500)

#t.test() for each individual lots, and subset() argument determine
#if the PSI for each manufacturing lot is statistically different from 
#the population mean of 1,500 pounds per square inch.
Lot1_table <- subset(table_tech, Manufacturing_Lot == 'Lot1')
Lot2_table <- subset(table_tech, Manufacturing_Lot == 'Lot2')
Lot3_table <- subset(table_tech, Manufacturing_Lot == 'Lot3')
t.test(Lot1_table$PSI, mu=1500)
t.test(Lot2_table$PSI, mu=1500)
t.test(Lot3_table$PSI, mu=1500)




