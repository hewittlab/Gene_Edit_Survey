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
likert_df <- likert_df[complete.cases(all$gen_mod_food),]
id <- seq(1:length(likert_df$gen_mod_food))
likert_df <- cbind(likert_df,id)
likert_df_m <- melt(likert_df, id = "id", value.name="Response", variable.name="Question")
head(likert_df_m,10)

ggplot(likert_df_m, aes(x = Question, y = Response, group = id)) + 
  geom_line(position=position_jitter(w=0.15, h=0.15), color=rgb(0,0,0,alpha=0.02)) + geom_point()


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
# 
# plot(likert_df, type="heat", ordered=T)
# ggsave(file="Figures symlink/Likert_Heat.eps", width=12, height=10)

plot(likert_df, ordered=F)
ggsave(file="Figures symlink/Likert_Bar.eps", width=12, height=10)



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

top_countries <- as.character(country_stat$country[1:8]) # Need to run country_stat in script 3
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










