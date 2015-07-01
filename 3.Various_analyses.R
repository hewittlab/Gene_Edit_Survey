# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

# MARK SYSTEM TIME
t_start3<-Sys.time()

library(plyr) # Various
library(car) # Recode variables
library(doBy) # Group summary stats


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





# 2) Demographic summary stats 
# - Number of countries & % 
# - Age 
# - Sex 
# - Education 
# - Self reported wealth 
# - Religion 
# - Self-reported inherited disease 
# 
# 3) Summary of responses to each question 
# - likely collapse Strongly Agree + Agree & Strongly Disagree + Disagree 
# 
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










# MARK SYSTEM TIME AGAIN
# Subtract from t_start and print difference
t_end3<-Sys.time()
t_dur3=t_end3-t_start3
print(t_dur3)