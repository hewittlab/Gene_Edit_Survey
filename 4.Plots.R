# Need to have run the 1.Data_prep.R script and have the resulting data in this workspace

library(plyr)
library(likert)
library(reshape2)
library(car) # Recode variables
library(ggplot2) # Plots

## Need to create a "Figures symlink" folder in the WD to save plots to.

#LIKERT----------------------------------------------------------------------------------------

# LIKERT LINE

likert_df <- all
likert_df$kids_cure_life <- recode(likert_df$kids_cure_life,
'"1" = "3.Agree";
"2" = "3.Agree";
"3" = "2.Neutral";
"4" = "1.Disagree";
"5" = "1.Disagree";
"6" = "2.Neutral"')
likert_df$kids_cure_life <- ordered(likert_df$kids_cure_life)
levels(likert_df$kids_cure_life)
likert_df$kids_cure_debil <- recode(likert_df$kids_cure_debil,
'"1" = "3.Agree";
"2" = "3.Agree";
"3" = "2.Neutral";
"4" = "1.Disagree";
"5" = "1.Disagree";
"6" = "2.Neutral"')
likert_df$kids_cure_debil <- ordered(likert_df$kids_cure_debil)
levels(likert_df$kids_cure_debil)
likert_df$embr_prev_life <- recode(likert_df$embr_prev_life,
'"1" = "3.Agree";
"2" = "3.Agree";
"3" = "2.Neutral";
"4" = "1.Disagree";
"5" = "1.Disagree";
"6" = "2.Neutral"')
likert_df$embr_prev_life <- ordered(likert_df$embr_prev_life)
levels(likert_df$embr_prev_life)
likert_df$embr_prev_debil <- recode(likert_df$embr_prev_debil,
'"1" = "3.Agree";
"2" = "3.Agree";
"3" = "2.Neutral";
"4" = "1.Disagree";
"5" = "1.Disagree";
"6" = "2.Neutral"')
likert_df$embr_prev_debil <- ordered(likert_df$embr_prev_debil)
levels(likert_df$embr_prev_debil)
likert_df$edit_for_nondis <- recode(likert_df$edit_for_nondis,
'"1" = "3.Agree";
"2" = "3.Agree";
"3" = "2.Neutral";
"4" = "1.Disagree";
"5" = "1.Disagree";
"6" = "2.Neutral"')
likert_df$edit_for_nondis <- ordered(likert_df$edit_for_nondis)
levels(likert_df$edit_for_nondis)
likert_df$gen_mod_food <- recode(likert_df$gen_mod_food,
'"1" = "3.Agree";
"2" = "3.Agree";
"3" = "2.Neutral";
"4" = "1.Disagree";
"5" = "1.Disagree";
"6" = "2.Neutral"')
likert_df$gen_mod_food <- ordered(likert_df$gen_mod_food)
levels(likert_df$gen_mod_food)
likert_vec <- c("kids_cure_life","kids_cure_debil","embr_prev_life","embr_prev_debil","edit_for_nondis","gen_mod_food")
likert_df <- likert_df[,likert_vec]


# ALL DATA
likert_tmp <- na.omit(likert_df)
id <- seq(1:length(likert_tmp$gen_mod_food))
likert_tmp <- cbind(likert_tmp,id)
likert_tmp_m <- melt(likert_tmp, id = "id", value.name="Response", variable.name="Question")
head(likert_tmp_m,20)
ggplot(likert_tmp_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.02)) + geom_point() +
  labs(title="Likert Line - All Data")
ggsave(file="Figures symlink/Likert_Line.pdf", width=12, height=10)


# SEX
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$sex)
likert_tmp <- na.omit(likert_tmp)
# Males
likert_male <- subset(likert_tmp, group == "M")
likert_male$group <- NULL
id <- seq(1:length(likert_male$gen_mod_food))
likert_male <- cbind(likert_male,id)
likert_male_m <- melt(likert_male, id = "id", value.name="Response", variable.name="Question")
head(likert_male_m,20)
ggplot(likert_male_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.04)) + geom_point() +
  labs(title="Likert Line - Males")
ggsave(file="Figures symlink/Likert_Males.pdf", width=12, height=10)
# Females
likert_female <- subset(likert_tmp, group == "F")
likert_female$group <- NULL
id <- seq(1:length(likert_female$gen_mod_food))
likert_female <- cbind(likert_female,id)
likert_female_m <- melt(likert_female, id = "id", value.name="Response", variable.name="Question")
head(likert_female_m,20)
ggplot(likert_female_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.04)) + geom_point() +
  labs(title="Likert Line - Females")
ggsave(file="Figures symlink/Likert_Females.pdf", width=12, height=10)


# RELIGION
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$religion_type)
likert_tmp <- na.omit(likert_tmp)
# Christian
likert_christian <- subset(likert_tmp, group == 1 | group == 2 | group == 3)
likert_christian$group <- NULL
id <- seq(1:length(likert_christian$gen_mod_food))
likert_christian <- cbind(likert_christian,id)
likert_christian_m <- melt(likert_christian, id = "id", value.name="Response", variable.name="Question")
head(likert_christian_m,20)
ggplot(likert_christian_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Christians")
ggsave(file="Figures symlink/Likert_Christians.pdf", width=12, height=10)
# Muslim
likert_muslim <- subset(likert_tmp, group == 4)
likert_muslim$group <- NULL
id <- seq(1:length(likert_muslim$gen_mod_food))
likert_muslim <- cbind(likert_muslim,id)
likert_muslim_m <- melt(likert_muslim, id = "id", value.name="Response", variable.name="Question")
head(likert_muslim_m,20)
ggplot(likert_muslim_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Muslims")
ggsave(file="Figures symlink/Likert_Muslims.pdf", width=12, height=10)


# ETHNICITY
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$ethnicity)
likert_tmp <- na.omit(likert_tmp)
# Caucasian
likert_caucasian <- subset(likert_tmp, group == 4 | group == 5)
likert_caucasian$group <- NULL
id <- seq(1:length(likert_caucasian$gen_mod_food))
likert_caucasian <- cbind(likert_caucasian,id)
likert_caucasian_m <- melt(likert_caucasian, id = "id", value.name="Response", variable.name="Question")
head(likert_caucasian_m,20)
ggplot(likert_caucasian_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Caucasians")
ggsave(file="Figures symlink/Likert_Caucasians.pdf", width=12, height=10)
# Asian
likert_asian <- subset(likert_tmp, group == 9 | group == 11)
likert_asian$group <- NULL
id <- seq(1:length(likert_asian$gen_mod_food))
likert_asian <- cbind(likert_asian,id)
likert_asian_m <- melt(likert_asian, id = "id", value.name="Response", variable.name="Question")
head(likert_asian_m,20)
ggplot(likert_asian_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Asians")
ggsave(file="Figures symlink/Likert_Asians.pdf", width=12, height=10)


# WEALTH
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$wealth)
likert_tmp <- na.omit(likert_tmp)
# Above
likert_wealth_above <- subset(likert_tmp, group == 1)
likert_wealth_above$group <- NULL
id <- seq(1:length(likert_wealth_above$gen_mod_food))
likert_wealth_above <- cbind(likert_wealth_above,id)
likert_wealth_above_m <- melt(likert_wealth_above, id = "id", value.name="Response", variable.name="Question")
head(likert_wealth_above_m,20)
ggplot(likert_wealth_above_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.1)) + geom_point() +
  labs(title="Likert Line - Wealth (Above)")
ggsave(file="Figures symlink/Likert_Wealth(above).pdf", width=12, height=10)
# Average
likert_wealth_average <- subset(likert_tmp, group == 2)
likert_wealth_average$group <- NULL
id <- seq(1:length(likert_wealth_average$gen_mod_food))
likert_wealth_average <- cbind(likert_wealth_average,id)
likert_wealth_average_m <- melt(likert_wealth_average, id = "id", value.name="Response", variable.name="Question")
head(likert_wealth_average_m,20)
ggplot(likert_wealth_average_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Wealth (Average)")
ggsave(file="Figures symlink/Likert_Wealth(average).pdf", width=12, height=10)
# Below
likert_wealth_below <- subset(likert_tmp, group == 3)
likert_wealth_below$group <- NULL
id <- seq(1:length(likert_wealth_below$gen_mod_food))
likert_wealth_below <- cbind(likert_wealth_below,id)
likert_wealth_below_m <- melt(likert_wealth_below, id = "id", value.name="Response", variable.name="Question")
head(likert_wealth_below_m,20)
ggplot(likert_wealth_below_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Wealth (Below)")
ggsave(file="Figures symlink/Likert_Wealth(below).pdf", width=12, height=10)


# EDUCATION
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$edu_level)
likert_tmp <- na.omit(likert_tmp)
# School level and below
likert_school <- subset(likert_tmp, group == 1 | group == 2 | group == 3)
likert_school$group <- NULL
id <- seq(1:length(likert_school$gen_mod_food))
likert_school <- cbind(likert_school,id)
likert_school_m <- melt(likert_school, id = "id", value.name="Response", variable.name="Question")
head(likert_school_m,20)
ggplot(likert_school_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Education (School level)")
ggsave(file="Figures symlink/Likert_Education(school).pdf", width=12, height=10)
# Univeristy and above
likert_uni <- subset(likert_tmp, group == 4 | group == 5 | group == 6)
likert_uni$group <- NULL
id <- seq(1:length(likert_uni$gen_mod_food))
likert_uni <- cbind(likert_uni,id)
likert_uni_m <- melt(likert_uni, id = "id", value.name="Response", variable.name="Question")
head(likert_uni_m,20)
ggplot(likert_uni_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Education (Uni level)")
ggsave(file="Figures symlink/Likert_Education(uni).pdf", width=12, height=10)


# HEARD ABOUT
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$heard_about)
likert_tmp <- na.omit(likert_tmp)
# Never
likert_heard_never <- subset(likert_tmp, group == 1)
likert_heard_never$group <- NULL
id <- seq(1:length(likert_heard_never$gen_mod_food))
likert_heard_never <- cbind(likert_heard_never,id)
likert_heard_never_m <- melt(likert_heard_never, id = "id", value.name="Response", variable.name="Question")
head(likert_heard_never_m,20)
ggplot(likert_heard_never_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Heard About (Never)")
ggsave(file="Figures symlink/Likert_Heard(Never).pdf", width=12, height=10)
# Little
likert_heard_little <- subset(likert_tmp, group == 2)
likert_heard_little$group <- NULL
id <- seq(1:length(likert_heard_little$gen_mod_food))
likert_heard_little <- cbind(likert_heard_little,id)
likert_heard_little_m <- melt(likert_heard_little, id = "id", value.name="Response", variable.name="Question")
head(likert_heard_little_m,20)
ggplot(likert_heard_little_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Heard About (Little)")
ggsave(file="Figures symlink/Likert_Heard(Little).pdf", width=12, height=10)
# Lot
likert_heard_lot <- subset(likert_tmp, group == 3)
likert_heard_lot$group <- NULL
id <- seq(1:length(likert_heard_lot$gen_mod_food))
likert_heard_lot <- cbind(likert_heard_lot,id)
likert_heard_lot_m <- melt(likert_heard_lot, id = "id", value.name="Response", variable.name="Question")
head(likert_heard_lot_m,20)
ggplot(likert_heard_lot_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.05)) + geom_point() +
  labs(title="Likert Line - Heard About (Lot)")
ggsave(file="Figures symlink/Likert_Heard(Lot).pdf", width=12, height=10)


# GENETIC CONDITION
# cbind subcategory variable here
likert_tmp <- cbind(likert_df, group = all$genetic_cond)
likert_tmp <- na.omit(likert_tmp)
# Males
likert_gencon_no <- subset(likert_tmp, group == "N")
likert_gencon_no$group <- NULL
id <- seq(1:length(likert_gencon_no$gen_mod_food))
likert_gencon_no <- cbind(likert_gencon_no,id)
likert_gencon_no_m <- melt(likert_gencon_no, id = "id", value.name="Response", variable.name="Question")
head(likert_gencon_no_m,20)
ggplot(likert_gencon_no_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.04)) + geom_point() +
  labs(title="Likert Line - Doesn’t have genetic condition")
ggsave(file="Figures symlink/Likert_gencond(no).pdf", width=12, height=10)
# Females
likert_gencon_yes <- subset(likert_tmp, group == "Y")
likert_gencon_yes$group <- NULL
id <- seq(1:length(likert_gencon_yes$gen_mod_food))
likert_gencon_yes <- cbind(likert_gencon_yes,id)
likert_gencon_yes_m <- melt(likert_gencon_yes, id = "id", value.name="Response", variable.name="Question")
head(likert_gencon_yes_m,20)
ggplot(likert_gencon_yes_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.04)) + geom_point() +
  labs(title="Likert Line - Has genetic condition")
ggsave(file="Figures symlink/Likert_gencond(yes).pdf", width=12, height=10)



# LIKERT BAR

#Drop id variable
likert_df <- likert_df[,-7]

likert_df<-rename(likert_df, c("kids_cure_life"="How much do you agree with the use of genetic editing of cells in children or adults to cure a life threatening disease?","kids_cure_debil"="How much do you agree with the use of genetic editing of cells in children or adults to cure a debilitating disease?","embr_prev_life"="How much do you agree with the use of genetic editing of cells in embryos to prevent a life threatening disease?","embr_prev_debil"="How much do you agree with the use of genetic editing of cells in embryos to prevent a debilitating disease?","edit_for_nondis"="How much do you agree with the use of genetic editing of cells in embryos to alter any non-disease characteristic - such as memory, eye colour or height?","gen_mod_food"="How much do you agree with the use of genetically modified food?"))

likert_df <-likert(likert_df)

summary(likert_df)

# plot(likert_df, type="density")
# ggsave(file="Figures symlink/Likert_Density_Faceted.eps", width=12, height=10)
# 
# plot(likert_df, type="density", facet=F)
# ggsave(file="Figures symlink/Likert_Density.eps", width=12, height=10)

plot(likert_df, type="heat", ordered=T)
ggsave(file="Figures symlink/Likert_Heat.eps", width=12, height=10)

# plot(likert_df, ordered=F)
# ggsave(file="Figures symlink/Likert_Bar.eps", width=12, height=10)



#POPN----------------------------------------------------------------------------------------

# POPULATION PYRAMID

ggplot(data=all,aes(x=age,fill=sex)) + 
  geom_bar(subset=.(sex=="F")) + 
  geom_bar(subset=.(sex=="M"),aes(y=..count..*(-1))) + 
  scale_fill_grey(start = 0.1, end = 0.4, na.value = "grey50") +
  scale_x_continuous(breaks=seq(0,100,10),labels=abs(seq(0,100,10))) +
  scale_y_continuous(breaks=seq(-1200,1000,80),labels=abs(seq(-1200,1000,80))) +
  labs(title="Population Pyramid", x="Age", y="Frequency") +
  coord_flip()
ggsave(file="Figures symlink/Pyramid_Plot.eps", width=12, height=10)


#AGE_BOXPLOTS----------------------------------------------------------------------------------------

# BOXPLOTS OF AGE BY ETHNICITY

ethnic_vec <- all$ethnicity
ethnic_vec <- factor(ethnic_vec, levels = c(1,2,3,4,5,6,7,8,9,10,11),labels = c("Mixed Race","African/African American","Asian Indian","Caucasian (European)","Caucasian (Middle East)","Hispanic, Latino or Spanish","Indigenous Australian","Native American","North East Asian (Mongol, Tibetan, Korean, Japanese, etc)","Pacific (Polynesian, micronesian, etc)","South East Asian (Chinese, Thai, Malay, Filipino, etc)"))
age_stat <- data.frame(age=all$age, group=ethnic_vec)

ggplot(age_stat, aes(x=group, y=age)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(10,100,10),labels=seq(10,100,10)) + 
  labs(title="Boxplots of Age by Ethnicity",x="Ethnicity", y="Age") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file="Figures symlink/Boxplot_Age*Ethnicity.eps", width=12, height=8)


# BOXPLOTS OF AGE (CAUCASIAN VS ASIAN)

age_stat <- data.frame(age=all$age, group=all$ethnicity)
age_stat <- subset(age_stat, group == 4 | group == 5 | group == 9 | group == 11)
# Recode
age_stat$group <- recode(age_stat$group,
'"4" = "Caucasian";
"5" = "Caucasian";
"9" = "Asian";
"11" = "Asian"')
levels(age_stat$group)

ggplot(age_stat, aes(x=group, y=age)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(10,100,10),labels=seq(10,100,10)) + 
  labs(title="Boxplots of Age - Caucasian vs Asian",x="Ethnicity", y="Age")
ggsave(file="Figures symlink/Boxplot_Age*CaucasianVsAsian.eps", width=12, height=8)


# BOXPLOTS OF AGE BY MOST FREQUENT 8 COUNTRIES
country_table <- table(all$country)
country_prop <- round(prop.table(country_table)*100,2)
country_stat <- data.frame(country_table, country_prop)
country_stat <- country_stat[,c(1,2,4)]
country_stat <- rename(country_stat,c("Var1"="country","Freq"="freq","Freq.1"="%"))
country_stat <- country_stat[order(-country_stat$freq),] 
country_stat
top_countries <- as.character(country_stat$country[1:8])
# Create dataframe of most frequent countries
age_country <- data.frame(age=all$age, group=all$country)
(age_country <- subset(age_country, group == top_countries[1] | group == top_countries[2] | group == top_countries[3] | group == top_countries[4] | group == top_countries[5] | group == top_countries[6] | group == top_countries[7] | group == top_countries[8]))
# Change group to character to remove redundant factor levels
age_country$group <- as.character(age_country$group)

ggplot(age_country, aes(x=group, y=age)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(10,100,10),labels=seq(10,100,10)) + 
  labs(title="Boxplots of Age by Country",x="Country", y="Age") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file="Figures symlink/Boxplot_Age*Country.eps", width=12, height=8)


#DENSITY_PLOTS----------------------------------------------------------------------------------------

all$Date <- as.Date(all$createdAt, "%m/%d/%Y")
str(all$Date)

# DENSITY PLOT OF COUNTRY & DATE
qplot(Date, data=all, geom="density", fill=country, alpha=I(.5), 
      main="Responses from each country", xlab="Date", 
      ylab="Density")

# DENSITY PLOT OF TOP X COUNTRIES
all$country_trim <- NA 
all$country_trim[all$country=="United States of America"] <- 1
all$country_trim[all$country=="China"] <- 2
all$country_trim[all$country=="Australia"] <- 3
all$country_trim[all$country=="South Africa"] <- 4
all$country_trim[all$country=="Japan"] <- 5
all$country_trim[all$country=="United Kingdom"] <- 6

all$country_trim <- recode(all$country_trim, '"1" = "United States of America"; "2" = "China"; "3" = "Australia"; "4" = "South Africa"; "5" = "Japan"; "6" = "United Kingdom"')

qplot(Date, data=all, geom="density", fill=country_trim, alpha=I(.5), 
      main="Responses from each country", xlab="Date", 
      ylab="Density")


# DENSITY PLOT OF LANGUAGE & DATE
qplot(Date, data=all, geom="density", fill=language, alpha=I(.5), 
      main="Responses based on language", xlab="Date", 
      ylab="Density")


#----------------------------------------------------------------------------------------










