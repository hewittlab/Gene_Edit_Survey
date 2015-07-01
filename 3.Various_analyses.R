# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

# MARK SYSTEM TIME
t_start3<-Sys.time()


# Check attrition as questions progress - Count NA's

# Demographic variables
# "sex","YOB","country","ethnicity","religion","religion_type","edu_level","worked_health","worked_health_type","heard_about","wealth"

# Survey variables
# "genetic_cond","genetic_cond_affected","genetic_cond_type","kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","deter_phys_appear","deter_intell","deter_strength","other_traits_alter","gen_mod_food"

NA_vec <- c("sex","YOB","country","ethnicity","religion","religion_type","edu_level","worked_health","worked_health_type","heard_about","wealth","genetic_cond","genetic_cond_affected","genetic_cond_type","kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","deter_phys_appear","deter_intell","deter_strength","gen_mod_food")

NA_count <- colSums(is.na(all[,NA_vec]))
percent_complete <- round(100 - NA_count/nrow(all)*100,1)
NA_count <- data.frame(NA_count, percent_complete)
NA_count










#POPN----------------------------------------------------------------------------------------

# POPULATION PYRAMID

# Set ages above 90 <- NA
all$age[all$age > 90] <- NA

ggplot(data=all,aes(x=age,fill=sex)) + 
  geom_bar(subset=.(sex=="F")) + 
  geom_bar(subset=.(sex=="M"),aes(y=..count..*(-1))) + 
  scale_fill_grey(start = 0.1, end = 0.4, na.value = "grey50") +
  scale_x_continuous(breaks=seq(0,100,10),labels=abs(seq(0,100,10))) +
  scale_y_continuous(breaks=seq(-260,120,20),labels=abs(seq(-260,120,20))) + 
  coord_flip()
ggsave(file="Pyramid_Plot.eps", width=12, height=10)

#----------------------------------------------------------------------------------------



# MARK SYSTEM TIME AGAIN
# Subtract from t_start and print difference
t_end3<-Sys.time()
t_dur3=t_end3-t_start3
print(t_dur3)