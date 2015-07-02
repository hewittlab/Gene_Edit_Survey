# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

library(plyr) # Various
library(car) # Recode variables
library(doBy) # Group summary stats

#ATTRITION-----------------------------------------------------------------------------------------------

# CHECK ATTRITION as questions progress - Count NA's

# Demographic variables
# "sex","YOB","country","ethnicity","religion","religion_type","edu_level","worked_health","worked_health_type","heard_about","wealth"

# Survey variables
# "genetic_cond","genetic_cond_affected","genetic_cond_type","kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","deter_phys_appear","deter_intell","deter_strength","other_traits_alter","gen_mod_food"

NA_vec <- c("sex","YOB","country","ethnicity","religion","religion_type","edu_level","worked_health","worked_health_type","heard_about","wealth","genetic_cond","genetic_cond_affected","genetic_cond_type","kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","deter_phys_appear","deter_intell","deter_strength","gen_mod_food")

NA_count <- colSums(is.na(all[,NA_vec]))
percent_complete <- round(100 - NA_count/nrow(all)*100,1)
NA_count <- data.frame(NA_count, percent_complete)
NA_count


#DEM_SUMMARY_STATS---------------------------------------------------------------------------------------

# DEMOGRAPHIC SUMMARY STATS
# - Religion 
# - Self-reported inherited disease 

# COUNTRY
country_table <- table(all$country)
country_prop <- round(prop.table(country_table)*100,2)
country_stat <- data.frame(country_table, country_prop)
country_stat <- country_stat[,c(1,2,4)]
country_stat <- rename(country_stat,c("Var1"="country","Freq"="freq","Freq.1"="%"))
# No. observations
sum(country_stat[,2])
# No. NA's
nrow(all)-sum(country_stat[,2])
# Sort
country_stat <- country_stat[order(-country_stat$freq),] 
country_stat
# Write results
write.csv(country_stat, "country_stat.csv")
write.table(country_stat, "country_stat.txt", sep="\t")


# ETHNICITY
ethnicity_table <- table(all$ethnicity)
ethnicity_prop <- round(prop.table(ethnicity_table)*100,2)
ethnicity_stat <- data.frame(ethnicity_table, ethnicity_prop)
ethnicity_stat <- ethnicity_stat[,c(1,2,4)]
ethnicity_stat <- rename(ethnicity_stat,c("Var1"="ethnicity","Freq"="freq","Freq.1"="%"))
# Label the levels
ethnicity_stat$ethnicity <- factor(ethnicity_stat$ethnicity, levels = c(1,2,3,4,5,6,7,8,9,10,11),labels = c("Mixed Race","African/African American","Asian Indian","Caucasian (European)","Caucasian (Middle East)","Hispanic, Latino or Spanish","Indigenous Australian","Native American","North East Asian (Mongol, Tibetan, Korean, Japanese, etc)","Pacific (Polynesian, micronesian, etc)","South East Asian (Chinese, Thai, Malay, Filipino, etc)"))
# No. observations
sum(ethnicity_stat[,2])
# No. NA's
nrow(all)-sum(ethnicity_stat[,2])
# Sort
ethnicity_stat <- ethnicity_stat[order(-ethnicity_stat$freq),] 
ethnicity_stat
# Write results
write.csv(ethnicity_stat, "ethnicity_stat.csv")
write.table(ethnicity_stat, "ethnicity_stat.txt", sep="\t")

# AGE
summary(all$age)


# SEX
sex_table <- table(all$sex)
sex_prop <- round(prop.table(sex_table)*100,2)
sex_stat <- data.frame(sex_table, sex_prop)
sex_stat <- sex_stat[,c(1,2,4)]
sex_stat <- rename(sex_stat,c("Var1"="sex","Freq"="freq","Freq.1"="%"))
# No. observations
sum(sex_stat[,2])
# No. NA's
nrow(all)-sum(sex_stat[,2])
# Sort
sex_stat <- sex_stat[order(-sex_stat$freq),] 
sex_stat
# Write results
write.csv(sex_stat, "sex_stat.csv")
write.table(sex_stat, "sex_stat.txt", sep="\t")


# EDUCATION
edu_level_table <- table(all$edu_level)
edu_level_prop <- round(prop.table(edu_level_table)*100,2)
edu_level_stat <- data.frame(edu_level_table, edu_level_prop)
edu_level_stat <- edu_level_stat[,c(1,2,4)]
edu_level_stat <- rename(edu_level_stat,c("Var1"="education","Freq"="freq","Freq.1"="%"))
# Label the levels
edu_level_stat$education <- factor(edu_level_stat$education, levels = c(1,2,3,4,5,6),labels = c("No formal schooling","Finished primary school","Finished high school","Finished a course following school","Finished undergraduate university degree","Finished postgraduate university degree"))
# No. observations
sum(edu_level_stat[,2])
# No. NA's
nrow(all)-sum(edu_level_stat[,2])
edu_level_stat
# Write results
write.csv(edu_level_stat, "edu_level_stat.csv")
write.table(edu_level_stat, "edu_level_stat.txt", sep="\t")


# WEALTH
wealth_table <- table(all$wealth)
wealth_prop <- round(prop.table(wealth_table)*100,2)
wealth_stat <- data.frame(wealth_table, wealth_prop)
wealth_stat <- wealth_stat[,c(1,2,4)]
wealth_stat <- rename(wealth_stat,c("Var1"="wealth","Freq"="freq","Freq.1"="%"))
# Label the levels
wealth_stat$wealth <- factor(wealth_stat$wealth, levels = c(1,2,3),labels = c("Above average wealth","Average wealth","Below average wealth"))
# No. observations
sum(wealth_stat[,2])
# No. NA's
nrow(all)-sum(wealth_stat[,2])
wealth_stat
# Write results
write.csv(wealth_stat, "wealth_stat.csv")
write.table(wealth_stat, "wealth_stat.txt", sep="\t")




# 4) comparisons 
# a) between ethnicity 
# - Caucasian vs Non-Caucasian 
# - Chinese vs Caucasian 
# b) sex 
# c) age - prob need to also do a subgroup analysis on those over the 
# age of 19 (as this is the estimated readability of the hardest 
#            question). 
# d) Medical Background 
# e) Religion 
# f) Country 
# g) Inherited disease








