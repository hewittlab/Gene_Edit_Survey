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
prop_complete <- round(100 - NA_count/nrow(all)*100,1)
total_count <- rep(nrow(all),length(NA_vec))
obs_count <- total_count - NA_count
attrition <- data.frame(total_count, obs_count, NA_count, prop_complete)
attrition
# Write results
write.csv(attrition, "attrition.csv")
write.table(attrition, "attrition.txt", sep="\t")


#DEM_SUMMARY_STATS---------------------------------------------------------------------------------------

# DEMOGRAPHIC SUMMARY STATS

# COUNTRY
country_table <- table(all$country)
country_prop <- round(prop.table(country_table)*100,2)
country_stat <- data.frame(country_table, country_prop)
country_stat <- country_stat[,c(1,2,4)]
country_stat <- rename(country_stat,c("Var1"="country","Freq"="freq","Freq.1"="%"))
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
# Sort
ethnicity_stat <- ethnicity_stat[order(-ethnicity_stat$freq),] 
ethnicity_stat
# Write results
write.csv(ethnicity_stat, "ethnicity_stat.csv")
write.table(ethnicity_stat, "ethnicity_stat.txt", sep="\t")


# AGE
age_stat <- data.frame(age=all$age, group=all$sex)
age_stat <- summaryBy(age~group, data=age_stat, FUN=c(mean, sd, length))
age.prop <- c(round(length(all$sex[all$sex == "F"])/nrow(all)*100,1), round(length(all$sex[all$sex == "M"])/nrow(all)*100,1))
age_stat <- age_stat[1:2,]
age_stat <- cbind(age_stat, age.prop)
# Calculate and rbind totals
age_total <- c("All", round(mean(all$age),2), round(sd(all$age),2), sum(age_stat[,4]), sum(age_stat[,5]))
age_stat <- rbind(age_stat, age_total)
age_stat$age.mean <-as.numeric(age_stat$age.mean)
age_stat$age.mean <- round(age_stat$age.mean,2)
age_stat$age.sd <-as.numeric(age_stat$age.sd)
age_stat$age.sd <- round(age_stat$age.sd,2)
age_stat
# No. NA's
age_stat$age.length <-as.numeric(age_stat$age.length)
nrow(all)-sum(age_stat[1:2,4])
# Write results
write.csv(age_stat, "age_stat.csv")
write.table(age_stat, "age_stat.txt", sep="\t")


# SEX
sex_table <- table(all$sex)
sex_prop <- round(prop.table(sex_table)*100,2)
sex_stat <- data.frame(sex_table, sex_prop)
sex_stat <- sex_stat[,c(1,2,4)]
sex_stat <- rename(sex_stat,c("Var1"="sex","Freq"="freq","Freq.1"="%"))
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
wealth_stat
# Write results
write.csv(wealth_stat, "wealth_stat.csv")
write.table(wealth_stat, "wealth_stat.txt", sep="\t")


# RELIGION
religion_table <- table(all$religion)
religion_prop <- round(prop.table(religion_table)*100,2)
religion_stat <- data.frame(religion_table, religion_prop)
religion_stat <- religion_stat[,c(1,2,4)]
religion_stat <- rename(religion_stat,c("Var1"="religion","Freq"="freq","Freq.1"="%"))
religion_stat
# Write results
write.csv(religion_stat, "religion_stat.csv")
write.table(religion_stat, "religion_stat.txt", sep="\t")


# - Self-reported inherited disease 

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








