#libraries ----
library(dplyr)
library(InformationValue)
library(yardstick)
library(DescTools)
library(readr)
library(faraway)
library(MASS)
library(ggplot2)
library(moderndive)
library(ISLR)
library(skimr)
library(plotly)
library(tidyr)
library(jtools)
library(ggcorrplot)
library(gridExtra)
library(cowplot)
library(plotly)
require(caret)
library(kernlab) #svm
library(SparseM)
library(quantreg) #Quantile Regression
library(stargazer)
library(ggplot2)
library(hrbrthemes)
library(olsrr)
library(lmtest)
library(forcats)
library(viridis)
library(tidytext)
library(GGally)
library(ggfortify)
library(tidyverse)
library(ROCR)
library("broom")
library(Metrics)
library(rpart)
library(randomForest)

#Reading data ----
train_obesity <- readRDS("train_obesity.RData")
test_obesity <- readRDS("test_obesity.RData")
train_over_sampled <- readRDS("train_over_sampled.RData")
train_under_sampled <- readRDS("train_under_sampled.RData")
train_both_sampled <- readRDS("train_both_sampled.RData")


#Handling deletions ----
#rm()

#Handling missing values ----
lapply(obesity, function(x) { length(which(is.na(x))) })

#Viewing data ----
#Displaying first few rows of obesity data
head(obesity)

#Displaying first few rows of obesity data
head(obesity)

#Tidying up data ----
#Converting the categorical variables into factors
obesity$Sex <- as.factor(obesity$Sex)
obesity$Education <- as.factor(obesity$Education)
obesity$Veg <- as.factor(obesity$Veg)
obesity$Fruit <- as.factor(obesity$Fruit)
obesity$Obese <- as.factor(obesity$Obese)
obesity$Year <- as.factor(obesity$Year)

#Observing the data
str(obesity)

#Displaying summary of of obesity data by Year
summary(as.factor(obesity$Year))

#Exploratory Analysis ----

#Computing the correlation coefficient between the response variable, BMI and the continuous explanatory variable, Age:
obesity %>% 
  get_correlation(formula = BMI ~ Age)

ggplot(obesity, aes(x = Age, y = BMI, color = Sex)) +
  geom_point(aes(color = Sex, shape = Sex))+
  labs(x = "Age (years)", y = bquote("BMI (kg/"~m^2~")")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(aes(color = Sex, fill = Sex), method = "lm", se = F) +
  scale_color_manual(values = c("#0652DD", "#009432")) + 
  scale_fill_manual(values = c("#0652DD", "#009432")) +
  theme(panel.background = element_blank())

#Density plots of all continuous variables in the obesity dataset
ggplot(obesity, aes(x = BMI)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "#70AD47", fill = "white", lwd = 0.25) +
  geom_density(lwd = 0.5,
               linetype = 2,
               colour = "#70AD47", fill = "#70AD47", alpha = 0.25) + 
  labs(x = bquote("BMI (kg/"~m^2~")"), y = "Frequency")  +
  geom_vline(aes(xintercept = mean(BMI)), color = "darkgreen", linetype = 2, size = 0.75) +
  annotate("text", x = 33, y = 0.08, size = 4.5, label = "italic(Mean == 27.92)", parse = T,  color = "#1B1464") + 
  theme(panel.background = element_blank())

ggplot(obesity, aes(x = Age)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "#70AD47", fill = "white", lwd = 0.25) +
  geom_density(lwd = 0.5,
               linetype = 2,
               colour = "#70AD47", fill = "#70AD47", alpha = 0.25)+ 
  labs(x = "Age  (years)", y = "Frequency")  +
  geom_vline(aes(xintercept = median(Age)), color = "darkgreen", linetype = 2, size = 0.75) +
  annotate("text", x =60, y = 0.021, size = 4.5, label = "italic(Median == 51)", parse = T,  color = "#1B1464")

#Barplots of all categorical variables in the obesity dataset
ggplot(obesity, aes(x = Sex, fill = Sex)) +
  geom_bar(alpha = 0.7) + theme_bw() +
  theme(axis.text.x=element_blank()) + 
  labs(x = "Sex", y = "Count") + 
  theme_ipsum()

ggplot(obesity, aes(Education, fill = Education)) +
  geom_bar(alpha = 0.7) + theme_bw() +
  theme(axis.text.x=element_blank())+ 
  labs(x = "Education", y = "Count") + 
  theme_ipsum()

ggplot(obesity, aes(Veg, fill = Veg)) +
  geom_bar(alpha = 0.7) + theme_bw() +
  theme(axis.text.x=element_blank())+ 
  labs(x = "Veg", y = "Count") + 
  theme_ipsum()

ggplot(obesity, aes(Fruit, fill = Fruit)) +
  geom_bar(alpha = 0.7) + theme_bw() +
  theme(axis.text.x=element_blank())+ 
  labs(x = "Fruit", y = "Count") + 
  theme_ipsum()

ggplot(obesity, aes(Obese, fill = Obese)) +
  geom_bar(alpha = 0.7) + theme_bw() +
  theme(axis.text.x=element_blank())+ 
  labs(x = "Obese", y = "Count") + 
  theme_ipsum()

#Boxplot of presence and absence of obesity based on age
plot(Age ~ Obese, train_obesity)

#Boxplot showing relationship of Education vs BMI
obesity %>% 
  ggplot(aes(x= fct_reorder(Education,BMI), y=BMI, fill=Education)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  labs(x = "Education", y = bquote("BMI (kg/"~m^2~")"), title = "Boxplot of BMI and Education") +
  coord_flip() +
  theme(legend.position="none", panel.background = element_rect(fill = "transparent")) +
  scale_fill_brewer(palette="Dark2") +
  theme(plot.title = element_text(hjust = 0.25))

#Boxplot showing relationship of Fruit vs BMI
obesity %>% 
  ggplot(aes(x= fct_reorder(Fruit,BMI), y=BMI, fill=Fruit)) +
  geom_boxplot(alpha=0.3) + 
  theme_bw() +
  labs(x = "Fruit", y = bquote("BMI (kg/"~m^2~")"), title = "Boxplot of BMI and Fruit") +
  theme(legend.position="none", panel.background = element_rect(fill = "transparent")) +
  scale_fill_brewer(palette="Dark2") +
  theme(plot.title = element_text(hjust = 0.25))

#Boxplot showing relationship of Veg vs BMI
obesity %>% 
  ggplot(aes(x= fct_reorder(Veg,BMI), y=BMI, fill=Veg)) +
  geom_boxplot(alpha=0.3) + 
  theme_bw() +
  labs(x = "Veg", y = bquote("BMI (kg/"~m^2~")"), title = "Boxplot of BMI and Veg") +
  theme(legend.position="none", panel.background = element_rect(fill = "transparent")) +
  scale_fill_brewer(palette="Dark2") +
  theme(plot.title = element_text(hjust = 0.25))

#Yearly variation of BMI ----

#Creating a dataframe to find the yearly proportion of Obese people
obesity$Education <- as.factor()

yearly_proportion <- group_by(obesity, Year, Obese) %>% 
  summarise(count=n()) %>% 
  group_by(Year) %>% 
  mutate(Total = sum(count), Proportion = count/Total) %>% 
  subset(Obese == "Yes")

#Plotting the yearly proportional variation of Obesity 
yearly_proportion %>% 
  ggplot(aes(x=Year, y=Proportion)) + 
  geom_line(color = "#2A638B", lwd=1) + 
  geom_point(size=2.5)

#Performing chi-squared test to identify if there is any yearly variation of BMI
chisq.test(obesity$Obese, obesity$Year)

#Displaying yearly proportional variation of Obesity 
library(gmodels)
prop.table(table(obesity$Obese, obesity$Year))

CrossTable(obesity$Obese, obesity$Year, prop.r = F, prop.c = T, prop.t = F, prop.chisq = F)

#Variation of BMI based on Education ----

#Creating a dataframe to find the proportion of Obese people in each Educational level
edu_proportion <- group_by(obesity, Education, Obese) %>% 
  summarise(count=n()) %>% 
  group_by(Education) %>% 
  mutate(Total = sum(count), Proportion = count/Total) %>% 
  subset(Obese == "Yes")

#Plotting the proportional variation of Obesity with respect to each Educational level
edu_obese_plot <- edu_proportion %>% 
  ggplot(mapping = aes(x=Education, y=Proportion, color = Obese, group = Obese)) + 
  labs(x = "Education", y = "Proportion of Obesity") +
  geom_line(lwd = 2, color = "#70AD47") + 
  geom_point(color="darkred", size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 
edu_obese_plot

#Variation of BMI based on Sex ----

#Creating a dataframe to find the proportion of Obese people in each Sex
sex_proportion <- group_by(obesity, Sex, Obese) %>% 
  summarise(count=n()) %>% 
  group_by(Sex) %>% 
  mutate(Total = sum(count), Proportion = count/Total) %>% 
  subset(Obese == "Yes")

#Plotting the proportional variation of Obesity with respect to each Sex
sex_obese_plot <- sex_proportion %>% 
  ggplot(mapping = aes(x=Sex, y=Proportion, color = Obese, group = Obese)) + 
  geom_line(lwd=1, color = "#70AD47") + 
  geom_point(color="darkred")

#Variation of BMI based on consumption of Fruit ----

#Creating a dataframe to find the proportion of Obese people based on their consumption of Fruit
fruit_proportion <- group_by(obesity, Fruit, Obese) %>% 
  summarise(count=n()) %>% 
  group_by(Fruit) %>% 
  mutate(Total = sum(count), Proportion = count/Total) %>% 
  subset(Obese == "Yes")

#Plotting the proportional variation of Obesity based on their Fruit consumption
fruit_obese_plot <- fruit_proportion %>% 
  ggplot(mapping = aes(x=Fruit, y=Proportion, color = Obese, group = Obese)) + 
  geom_line(lwd=1, color = "#70AD47") + 
  geom_point(color="darkred")

#Variation of BMI based on consumption of Veg ----

#Creating a dataframe to find the proportion of Obese people based on their consumption of Veg
veg_proportion <- group_by(obesity, Veg, Obese) %>% 
  summarise(count=n()) %>% 
  group_by(Veg) %>% 
  mutate(Total = sum(count), Proportion = count/Total) %>% 
  subset(Obese == "Yes")

#Plotting the proportional variation of Obesity based on their Veg consumption
veg_obese_plot <- veg_proportion %>% 
  ggplot(mapping = aes(x=Veg, y=Proportion, color = Obese, group = Obese)) + 
  geom_line(lwd=1, color = "#70AD47") + 
  geom_point(color="darkred")

gridExtra::grid.arrange(sex_obese_plot, edu_obese_plot, fruit_obese_plot, veg_obese_plot, ncol = 2)

#Splitting data into train and test sets ----

#Dividing obesity data into training and test sets
n <- nrow(obesity)
index <- sample(x = c(T,F), size = n, replace = T, prob = c(0.7, 0.3))
index

train_obesity <- obesity[index, ]
test_obesity <- obesity[!index, ]

#Checking dimensions of train and test datasets
dim(train_obesity)
dim(test_obesity)
dim(obesity)

#Checking dimensions of train data set filtered based on Year 
train_obesity %>% filter(Year == 2013) %>% dim()
train_obesity %>% filter(Year == 2014) %>% dim()
train_obesity %>% filter(Year == 2015) %>% dim()
train_obesity %>% filter(Year == 2016) %>% dim()
summary(as.factor(train_obesity$Year))

#Checking dimensions of test data set filtered based on Year 
test_obesity %>% filter(Year == 2013) %>% dim()
test_obesity %>% filter(Year == 2014) %>% dim()
test_obesity %>% filter(Year == 2015) %>% dim()
test_obesity %>% filter(Year == 2016) %>% dim()
summary(as.factor(test_obesity$Year))

#Displaying first few rows of obesity train data set
head(train_obesity)

#Getting numerical summaries of obesity train data set
summary(train_obesity)
summary(obesity)

#Getting summary statistics of obesity train dataset
skim(train_obesity)
my_skim(obesity)
my_skim(train_obesity)
my_skim <- skim_with(base=sfl(n=length,n_missing=n_missing),factor=sfl(ordered=NULL),
                     numeric=sfl(hist = NULL))
levels(train_obesity$Year)
#Exporting train and test datasets to csv files
write_rds(train_obesity,"train_obesity.RData")
write_rds(test_obesity,"test_obesity.RData")

#Checking balance of Yes versus No outcomes
print(table(train_obesity$Obese))
print(prop.table(table(train_obesity$Obese)))

print(table(test_obesity$Obese))
print(prop.table(table(test_obesity$Obese)))

print(table(obesity$Obese))
print(prop.table(table(obesity$Obese)))

#This can be considered as a skewed dataset or rare event wherein the No outcomes are more in contrast to the Yes outcomes.


#Sampling to handle class imbalance ----

#Applying sampling methods to balance the training dataset
#Balancing classes in the training data set

#Carrying out over sampling
train_over_sampled <- ovun.sample(Obese ~ .-BMI, data = train_obesity, method = "over",N = 13632)$data
table(train_over_sampled$Obese)
print(prop.table(table(train_over_sampled$Obese)))

#Carrying out under sampling
train_under_sampled <- ovun.sample(Obese ~ .-BMI, data = train_obesity, method = "under", N = 5990, seed = 1)$data
table(train_under_sampled$Obese)
print(prop.table(table(train_under_sampled$Obese)))

#Carrying out both under and over sampling
train_both_sampled <- ovun.sample(Obese ~ .-BMI, data = train_obesity, method = "both", p=0.5, N=9811, seed = 1)$data
table(train_both_sampled$Obese)
print(prop.table(table(train_both_sampled$Obese)))

#Exporting train and test datasets to csv files
write_rds(train_over_sampled,"train_over_sampled.RData")
write_rds(train_under_sampled,"train_under_sampled.RData")
write_rds(train_both_sampled,"train_both_sampled.RData")

# Logistic Regression ----

## Model Fitting ----

# Creating a dataframe of observed and predicted data of every model
binary <- as.data.frame(x = test_obesity$Obese)
colnames(binary)[1] = "true_value"

### On over sampled training set ----

#### Full Model ----
bin_mod_one <- glm(Obese ~ Age + Sex + Education + Year + Fruit + Veg, family = binomial(link = "logit"), train_over_sampled)
summary(bin_mod_one)

model_selection_os <- step(bin_mod_one)

binary_os_one_prob <- predict(bin_mod_one, test_obesity, type = "response")
binary$os_one_pred <- ifelse(binary_os_one_prob > 0.5, "Yes", "No")

binary_one_cc <- table(binary$true_value, binary$os_one_pred)
binary_one_cc

binary_one_cc_rate <- (binary_one_cc[1,1] + binary_one_cc[2,2]) / nrow(test_obesity)
binary_one_cc_rate

binary_one_cf_mat <- confusionMatrix(as.factor(binary$true_value), as.factor(binary$os_one_pred))
binary_one_cf_mat

#### After removing Year variable - Logit ----
bin_mod_two <- glm(Obese ~ Age + Sex + Education + Fruit + Veg, family = binomial(link = "logit"), train_over_sampled)
summary(bin_mod_two)

binary_os_two_prob <- predict(bin_mod_two, test_obesity, type = "response")
binary$os_two_pred <- ifelse(binary_os_two_prob > 0.5, "Yes", "No")

binary_two_cc <- table(binary$true_value, binary$os_two_pred)
binary_two_cc

binary_two_cc_rate <- (binary_two_cc[1,1] + binary_two_cc[2,2]) / nrow(test_obesity)
binary_two_cc_rate

#### After removing Year and Fruit variable - Better Model - Logit ----
bin_mod_three <- glm(Obese ~ Age + Sex + Education + Veg, family = binomial(link = "logit"), train_over_sampled)
summary(bin_mod_three)

binary_os_three_prob <- predict(bin_mod_three, test_obesity, type = "response")
binary$os_three_pred <- ifelse(binary_os_three_prob > 0.5, "Yes", "No")

binary_three_cc <- table(binary$true_value, binary$os_three_pred)
binary_three_cc

binary_three_cc_rate <- (binary_three_cc[1,1] + binary_three_cc[2,2]) / nrow(test_obesity)
binary_three_cc_rate

#### After removing Year, Veg and Fruit variable - Logit ----
bin_mod_four <- glm(Obese ~ Age + Sex + Education, family = binomial(link = "logit"), train_over_sampled)
summary(bin_mod_four) 
bin_mod_four$coefficients %>% 
  write.csv("/Users/anushaanil/OneDrive/Study/Dissertation/Modelling Obesity in Scotland/Extracted Tables/GLM Summary.csv")

#Computing CIs using profiled log-likelihood
confint(bin_mod_four)

#Computing CIs using profiled standard errors
confint.default(bin_mod_four)

binary_os_four_prob <- predict(bin_mod_four, test_obesity, type = "response")
binary$os_four_pred <- ifelse(binary_os_four_prob > 0.5, "Yes", "No")

binary_four_cc <- table(binary$true_value, binary$os_four_pred)
binary_four_cc

binary_four_cc_rate <- (binary_four_cc[1,1] + binary_four_cc[2,2]) / nrow(test_obesity)
binary_four_cc_rate

#### After removing Year and Veg variable - Better Model - Probit ----
bin_mod_five <- glm(Obese ~ Age + Sex + Education + Fruit, family = binomial(link = "probit"), train_over_sampled)
summary(bin_mod_five)

binary_os_five_prob <- predict(bin_mod_five, test_obesity, type = "response")
binary$os_five_pred <- ifelse(binary_os_five_prob > 0.5, "Yes", "No")

binary_five_cc <- table(binary$true_value, binary$os_five_pred)
binary_five_cc

binary_five_cc_rate <- (binary_five_cc[1,1] + binary_five_cc[2,2]) / nrow(test_obesity)
binary_five_cc_rate

#### After removing Year, Veg and Fruit variable - Probit ----
bin_mod_six <- glm(Obese ~ Age + Sex + Education, family = binomial(link = "probit"), train_over_sampled)
summary(bin_mod_six)

binary_os_six_prob <- predict(bin_mod_six, test_obesity, type = "response")
binary$os_six_pred <- ifelse(binary_os_six_prob > 0.5, "Yes", "No")

binary_six_cc <- table(binary$true_value, binary$os_six_pred)
binary_six_cc

binary_six_cc_rate <- (binary_six_cc[1,1] + binary_six_cc[2,2]) / nrow(test_obesity)
binary_six_cc_rate

### On under sampled training set ----

#### Full Model ----

bin_mod_seven <- glm(Obese ~ Age + Sex + Education + Year + Fruit + Veg, family = binomial(link = "logit"), train_under_sampled)
summary(bin_mod_seven)

model_selection_us <- step(bin_mod_seven)

binary_us_seven_prob <- predict(bin_mod_seven, test_obesity, type = "response")
binary$us_seven_pred <- ifelse(binary_us_seven_prob > 0.5, "Yes", "No")

binary_seven_cc <- table(binary$true_value, binary$us_seven_pred)
binary_seven_cc

binary_seven_cc_rate <- (binary_seven_cc[1,1] + binary_seven_cc[2,2]) / nrow(test_obesity)
binary_seven_cc_rate

#### After removing Year variable - Logit ----
bin_mod_eight <- glm(Obese ~ Age + Sex + Education + Fruit + Veg, family = binomial(link = "logit"), train_under_sampled)
summary(bin_mod_eight)

binary_us_eight_prob <- predict(bin_mod_eight, test_obesity, type = "response")
binary$us_eight_pred <- ifelse(binary_us_eight_prob > 0.5, "Yes", "No")

binary_eight_cc <- table(binary$true_value, binary$us_eight_pred)
binary_eight_cc

binary_eight_cc_rate <- (binary_eight_cc[1,1] + binary_eight_cc[2,2]) / nrow(test_obesity)
binary_eight_cc_rate

#### After removing Year and Veg variable - Better Model - Logit ----
bin_mod_nine <- glm(Obese ~ Age + Sex + Education + Fruit, family = binomial(link = "logit"), train_under_sampled)
summary(bin_mod_nine)

binary_us_nine_prob <- predict(bin_mod_nine, test_obesity, type = "response")
binary$us_nine_pred <- ifelse(binary_us_nine_prob > 0.5, "Yes", "No")

binary_nine_cc <- table(binary$true_value, binary$us_nine_pred)
binary_nine_cc

binary_nine_cc_rate <- (binary_nine_cc[1,1] + binary_nine_cc[2,2]) / nrow(test_obesity)
binary_nine_cc_rate

#### After removing Year, Veg and Fruit variable - Logit ----
bin_mod_ten <- glm(Obese ~ Age + Sex + Education, family = binomial(link = "logit"), train_under_sampled)
summary(bin_mod_ten)

binary_us_ten_prob <- predict(bin_mod_ten, test_obesity, type = "response")
binary$us_ten_pred <- ifelse(binary_us_ten_prob > 0.5, "Yes", "No")

binary_ten_confusion_matrix = confusionMatrix(binary$true_value, as.factor(binary$us_ten_pred))
binary_ten_confusion_matrix

binary_ten_cc <- table(binary$true_value, binary$us_ten_pred)
binary_ten_cc

binary_ten_cc_rate <- (binary_ten_cc[1,1] + binary_ten_cc[2,2]) / nrow(test_obesity)
binary_ten_cc_rate

#### After removing Year and Veg variable - Better Model - Probit ----
bin_mod_eleven <- glm(Obese ~ Age + Sex + Education + Fruit, family = binomial(link = "probit"), train_under_sampled)
summary(bin_mod_eleven)

binary_us_eleven_prob <- predict(bin_mod_eleven, test_obesity, type = "response")
binary$us_eleven_pred <- ifelse(binary_us_eleven_prob > 0.5, "Yes", "No")

binary_eleven_cc <- table(binary$true_value, binary$us_eleven_pred)
binary_eleven_cc

binary_eleven_cc_rate <- (binary_eleven_cc[1,1] + binary_eleven_cc[2,2]) / nrow(test_obesity)
binary_eleven_cc_rate

#### After removing Year, Veg and Fruit variable - Probit ----
bin_mod_twelve <- glm(Obese ~ Age + Sex + Education, family = binomial(link = "probit"), train_under_sampled)
summary(bin_mod_twelve)

binary_us_twelve_prob <- predict(bin_mod_twelve, test_obesity, type = "response")
binary$us_twelve_pred <- ifelse(binary_us_twelve_prob > 0.5, "Yes", "No")

binary_twelve_cc <- table(binary$true_value, binary$us_twelve_pred)
binary_twelve_cc

binary_twelve_cc_rate <- (binary_twelve_cc[1,1] + binary_twelve_cc[2,2]) / nrow(test_obesity)
binary_twelve_cc_rate

### On both under and over sampled training set ----

#### Full Model ----

bin_mod_thirteen <- glm(Obese ~ Age + Sex + Education + Year + Fruit + Veg, family = binomial(link = "logit"), train_both_sampled)
summary(bin_mod_thirteen)

model_selection_both <- step(bin_mod_thirteen)

binary_bs_thirteen_prob <- predict(bin_mod_thirteen, test_obesity, type = "response")
binary$bs_thirteen_pred <- ifelse(binary_bs_thirteen_prob > 0.5, "Yes", "No")

binary_thirteen_cc <- table(binary$true_value, binary$bs_thirteen_pred)
binary_thirteen_cc

binary_thirteen_cc_rate <- (binary_thirteen_cc[1,1] + binary_thirteen_cc[2,2]) / nrow(test_obesity)
binary_thirteen_cc_rate

#### After removing Year variable - Logit ----
bin_mod_fourteen <- glm(Obese ~ Age + Sex + Education + Fruit + Veg, family = binomial(link = "logit"), train_both_sampled)
summary(bin_mod_fourteen)

binary_bs_fourteen_prob <- predict(bin_mod_fourteen, test_obesity, type = "response")
binary$bs_fourteen_pred <- ifelse(binary_bs_fourteen_prob > 0.5, "Yes", "No")

binary_fourteen_cc <- table(binary$true_value, binary$bs_fourteen_pred)
binary_fourteen_cc

binary_fourteen_cc_rate <- (binary_fourteen_cc[1,1] + binary_fourteen_cc[2,2]) / nrow(test_obesity)
binary_fourteen_cc_rate

#### After removing Year and Veg variable - Better Model - Logit ----
bin_mod_fifteen <- glm(Obese ~ Age + Sex + Education + Fruit, family = binomial(link = "logit"), train_both_sampled)
summary(bin_mod_fifteen)

binary_bs_fifteen_prob <- predict(bin_mod_fifteen, test_obesity, type = "response")
binary$bs_fifteen_pred <- ifelse(binary_bs_fifteen_prob > 0.5, "Yes", "No")

binary_fifteen_cc <- table(binary$true_value, binary$bs_fifteen_pred)
binary_fifteen_cc

binary_fifteen_cc_rate <- (binary_fifteen_cc[1,1] + binary_fifteen_cc[2,2]) / nrow(test_obesity)
binary_fifteen_cc_rate

#### After removing Year, Veg and Fruit variable - Logit ----
bin_mod_sixteen <- glm(Obese ~ Age + Sex + Education, family = binomial(link = "logit"), train_both_sampled)
summary(bin_mod_sixteen)

binary_bs_sixteen_prob <- predict(bin_mod_sixteen, test_obesity, type = "response")
binary$bs_sixteen_pred <- ifelse(binary_bs_sixteen_prob > 0.5, "Yes", "No")

binary_sixteen_cc <- table(binary$true_value, binary$bs_sixteen_pred)
binary_sixteen_cc

binary_sixteen_cc_rate <- (binary_sixteen_cc[1,1] + binary_sixteen_cc[2,2]) / nrow(test_obesity)
binary_sixteen_cc_rate

#### After removing Year and Veg variable - Better Model - Probit ----
bin_mod_seventeen <- glm(Obese ~ Age + Sex + Education + Fruit, family = binomial(link = "probit"), train_both_sampled)
summary(bin_mod_seventeen)

binary_bs_seventeen_prob <- predict(bin_mod_seventeen, test_obesity, type = "response")
binary$bs_seventeen_pred <- ifelse(binary_bs_seventeen_prob > 0.5, "Yes", "No")

binary_seventeen_cc <- table(binary$true_value, binary$bs_seventeen_pred)
binary_seventeen_cc

binary_seventeen_cc_rate <- (binary_seventeen_cc[1,1] + binary_seventeen_cc[2,2]) / nrow(test_obesity)
binary_seventeen_cc_rate

#### After removing Year, Veg and Fruit variable - Probit ----
bin_mod_eighteen <- glm(Obese ~ Age + Sex + Education, family = binomial(link = "probit"), train_both_sampled)
summary(bin_mod_eighteen)

binary_bs_eighteen_prob <- predict(bin_mod_eighteen, test_obesity, type = "response")
binary$bs_eighteen_pred <- ifelse(binary_bs_eighteen_prob > 0.5, "Yes", "No")

binary_eighteen_cc <- table(binary$true_value, binary$bs_eighteen_pred)
binary_eighteen_cc

binary_eighteen_cc_rate <- (binary_eighteen_cc[1,1] + binary_eighteen_cc[2,2]) / nrow(test_obesity)
binary_eighteen_cc_rate

## Correct Classification Rate calculation ----

#Creating a dataframe of correct classification rates of all models
binary_correct_classification_rates <- data.frame(binary_one_cc_rate, binary_two_cc_rate, binary_three_cc_rate, 
                                                  binary_four_cc_rate, binary_five_cc_rate, binary_six_cc_rate, 
                                                  binary_seven_cc_rate, binary_eight_cc_rate, binary_nine_cc_rate, 
                                                  binary_ten_cc_rate, binary_eleven_cc_rate, binary_twelve_cc_rate, 
                                                  binary_thirteen_cc_rate, binary_fourteen_cc_rate, binary_fifteen_cc_rate, 
                                                  binary_sixteen_cc_rate, binary_seventeen_cc_rate, binary_eighteen_cc_rate)

#Taking transpose of correct classification rates
binary_correct_classification_rates <- t(binary_correct_classification_rates)

#As model 10 and 4 gives the highest correct classification rate, they can be chosen as the best models as per 
#logistic regression.But since logit models are easier to interpret, we shall choose end up selecting model 4

# Creating a contingency table for model 4
contigency_table <- table(Predicted = binary$os_four_pred, Observed = binary$true_value)
contigency_table

#Confusion matrix for model 12 is given by: 
binary_four_confusion_matrix <- confusionMatrix(as.factor(binary$true_value), as.factor(binary$os_four_pred))
binary_four_confusion_matrix

#Plotting Receiver Operating Characteristic (ROC) Curve  
#Title: Area under the curve: 0.603
score <- prediction(binary_os_four_prob, test_obesity$Obese)
perf <- performance(score, measure = "tpr", x.measure = "fpr")
perf_df <- data.frame(x = perf@x.values[1][[1]], y = perf@y.values[1][[1]])
roc_bin_mod_four <- ggplot(perf_df, aes(x = x, y = y)) + 
  geom_line(color = "#00A421", lwd = 1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  annotate("text", x = 0.875, y = 0.85, size = 4.5, label = "italic(AUC == 0.6035)", parse = T,  color = "#1B1464")
roc_bin_mod_four

#Calculating Area Under the Curve (AUC)
auc <- performance(score, measure = "auc")
auc <- round(auc@y.values[[1]], 4)
auc

#Confidence intervals of model 4
confint.default(bin_mod_four)

#Exponentiationg model 4 coefficients
exp(coef(bin_mod_four))

exp(cbind(odds_ratio = coef(bin_mod_four), confint.default(bin_mod_four)))

#Calculating accuracy of model 4
accuracy_estimate <- accuracy(binary, as.factor(true_value), as.factor(us_four_pred))
accuracy_estimate
#The accuracy() gives rounded estimate i.e. 0.577 for model 4.

# Linear Regression  ----

#Fitting full model
linear_mod_one <- lm(BMI ~ Age + Sex + Education + Veg + Fruit + Year, data = train_obesity)
summary(linear_mod_one)
get_regression_table(linear_mod_one)
AIC(linear_mod_one)
BIC(linear_mod_one)
anova(linear_mod_one)

#Fitting a model on log transforming the response variable
linear_mod_two <- lm(log(BMI) ~ Age + Sex + Education + Veg + Fruit + Year, data = train_obesity)
summary(linear_mod_two)
get_regression_table(linear_mod_two)
AIC(linear_mod_two)
anova(linear_mod_two)

#Plotting the residuals vs fitted plot from the full linear model with BMI 
#log transformed:linear_mod_two
autoplot(linear_mod_two, which = 1 , alpha = 0.7, colour = "#70AD47", ncol = 1,
         smooth.colour = "#ED4C67", smooth.linetype = 1, size = 0.5) + 
  theme(panel.background = element_blank(), title = element_blank())

#Introducing quadratic term for Age

#Scatter plot of log(BMI) with Age
ggplot(obesity, aes(y = log(BMI), x = Age)) + 
  geom_point(color = "#40739e", alpha = 0.5, size = 1) + 
  geom_smooth(lwd = 0.75, color = "#e84118", method = "lm", se = F) + 
  labs(x = "Age (years)", y = bquote('log (BMI) (kg/'~m^2~')')) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())

#Scatter plot of log(BMI) with (Age^2)
ggplot(obesity, aes(y = log(BMI), x = Age)) + 
  geom_point(color = "#40739e", alpha = 0.6, size = 1) + 
  geom_smooth(lwd = 0.5, color = "#e84118", method = "lm", formula = y ~ x + I(x^2), se = F) + 
  labs(x = "Age (years)", y = bquote('log (BMI) (kg/'~m^2~')')) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())

#Arranging scatter plots of Age and (Age^2) with BMI 
grid.arrange(bmi_age_linear_plot, bmi_age_quadratic_plot, ncol = 2)

linear_mod_three <- lm(log(BMI) ~ Age + I(Age^2) + Sex + Education + Veg + Fruit + Year, data = train_obesity)
summary(linear_mod_three)
get_regression_table(linear_mod_three)
AIC(linear_mod_three)
anova(linear_mod_three)

#Plotting the residuals vs fitted plot from the full linear model with BMI 
#log transformed and Age as quadratic term: linear_mod_three
autoplot(linear_mod_three, which = 1 , alpha = 0.7, colour = "#70AD47", ncol = 1,
         smooth.colour = "#ED4C67", smooth.linetype = 1, size = 0.5) + 
  theme(panel.background = element_blank(), title = element_blank())

#Performing step-wise regression for variable selection using AIC
ols_step_both_aic(linear_mod_three)

linear_mod_four <- lm(log(BMI) ~ Age + I(Age^2) + Sex + Education, data = train_obesity)
summary(linear_mod_four)
AIC(linear_mod_four)
BIC(linear_mod_four)
get_regression_table(linear_mod_four)
get_regression_table(linear_mod_four) %>% mutate(estimate = exp(estimate))

linear_mod_five <- lm(log(BMI) ~ Age + I(Age^2) + Education, data = train_obesity)
summary(linear_mod_five)
get_regression_table(linear_mod_five)
AIC(linear_mod_five)
BIC(linear_mod_five)

#Comparing models using ANOVA test
anova(linear_mod_four, linear_mod_five)

#We select linear_mod_four finally based on AIC, BIC, Adj. R^2, and ANOVA test.

#Checking model assumptions using diagnostics plots
autoplot(linear_mod_four, which = c(1, 2, 3, 4), alpha = 0.3, colour = "#70AD47", 
         smooth.colour = "#192a56", smooth.linetype = 1, size = 0.5) + 
  theme(panel.background = element_blank())

#Testing for Heteroscedasticity - Breusch-Pagan Test

#H0: Heteroscedasticity is not present or variance is constant 
#(if p-value > 0.05, we'll accept H0, else reject it and accept alternate H0, 
#ie; accept H1)

#H1: Heteroscedasticity is present or variance is not constant 

bptest(linear_mod_four)


test_obesity_2 <- test_obesity %>% mutate(smooth_y = predict(lm(log(BMI) ~ Age + I(Age^2), data=test_obesity)))
#Finding max. point of deflection for Age
maximum <- test_obesity_2$Age[which.max(test_obesity_2$smooth_y)] ; maximum

#Plotting Age vs BMI showing point of deflection of Age
ggplot(mapping = aes(y = log(BMI), x = Age), data = test_obesity_2) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 1, color = "#6ab04c", aes(fill = "#6ab04c"), show.legend = F) +
  labs(x = "Age (years)", y = bquote("log(BMI) (kg/"~m^2~")")) +
  geom_vline(xintercept = maximum,color="#ED4C67", lwd = 1, lty = 2) + 
  scale_fill_manual(values = c("#6ab04c")) + 
  labs(x = "Age (years)", y = bquote("BMI (kg/"~m^2~")")) +
  annotate("text", x = 69, y = 3.344, size = 4.5, label = "italic(Age == 62)", parse = T,  color = "#1B1464")

#Predicting BMI with linear_mod_four on test data
prediction_linear_mod_four <- exp(predict(linear_mod_four, test_obesity))
prediction_linear_mod_four
summary(prediction_linear_mod_four)

#Calculating MSE, RMSE, MAE for linear_mod_four on test data
mse_linear_mod_four <- mse(prediction_linear_mod_four, test_obesity$BMI)
mse_linear_mod_four
rmse_linear_mod_four <- rmse(prediction_linear_mod_four, test_obesity$BMI)
rmse_linear_mod_four
mae_linear_mod_four <- mae(prediction_linear_mod_four, test_obesity$BMI)
mae_linear_mod_four

#Predicting BMI with linear_mod_five on test data
prediction_linear_mod_five <- exp(predict(linear_mod_five, test_obesity))
prediction_linear_mod_five
summary(prediction_linear_mod_five)

#Calculating MSE, RMSE, MAE for linear_mod_five on test data
mse_linear_mod_five <- mse(prediction_linear_mod_five, test_obesity$BMI)
mse_linear_mod_five
rmse_linear_mod_five <- rmse(prediction_linear_mod_five, test_obesity$BMI)
rmse_linear_mod_five
mae_linear_mod_five <- mae(prediction_linear_mod_five, test_obesity$BMI)
mae_linear_mod_five

# Quantile Regression ----

#OLS Regression
ols_reg <- lm(log(BMI) ~ Age + I(Age^2) + Sex + Education, data = train_obesity)
summary(ols_reg)

#For tau = 0.10
quant_reg_10 <- rq(log(BMI) ~ Age + I(Age^2) + Sex + Education, tau = 0.10, data = train_obesity)
summary(quant_reg_10)

#For tau = 0.25
quant_reg_25 <- rq(log(BMI) ~ Age + I(Age^2) + Sex + Education, tau = 0.25, data = train_obesity)
summary(quant_reg_25)

#For tau = 0.50
quant_reg_50 <- rq(log(BMI) ~ Age + I(Age^2) + Sex + Education, tau = 0.50, data = train_obesity)
summary(quant_reg_50)

#For tau = 0.75
quant_reg_75 <- rq(log(BMI) ~ Age + I(Age^2) + Sex + Education, tau = 0.75, data = train_obesity)
summary(quant_reg_75)

#For tau = 0.90
quant_reg_90 <- rq(log(BMI) ~ Age + I(Age^2) + Sex + Education, tau = 0.90, data = train_obesity)
summary(quant_reg_90)

#quant_reg_all <- rq(BMI ~ Age + Sex + Education, tau = seq(0.25, 0.75, by = 0.25), data = train_obesity)
quant_reg_all <- rq(log(BMI) ~ Age + I(Age^2) + Sex + Education, tau = c(0.10, 0.25, 0.50, 0.75, 0.90), data = train_obesity)
summary_quant_reg_all <- summary(quant_reg_all, se = "boot")
summary_quant_reg_all
quant_reg_all$coefficients

#Plotting estimated coefficients vs quantiles plot
plot(summary_quant_reg_all, lc = "#00A421", lwd = 2,) 

quant_reg_all %>%
  tidy(se.type = "rank", conf.int = TRUE, conf.level = 0.95) %>%
  filter(!grepl("factor", term)) %>%
  ggplot(aes(x=tau,y=estimate))+
  geom_point(color="#009432", size = 2)+ 
  geom_line(color="#009432", size = 1)+ 
  geom_smooth(method=  "lm", colour = "#e84118", aes(fill = "#e84118"), show.legend = F)+  
  scale_fill_manual(values = c("#e84118")) +
  facet_wrap(~term, scales="free", ncol=2) + 
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.25, fill="#009432")

#Making predictions using quantile regression model on test data
prediction_quant_reg_all <- as.tibble(exp(predict(quant_reg_all, test_obesity)))
prediction_quant_reg_all %>% summary()

#Testing significance of quantile regression model
anova(quant_reg_all, joint = F) 

#Comparing estimates of predictors at different quantiles
stargazer(ols_reg, quant_reg_10, quant_reg_25, quant_reg_50, quant_reg_75, quant_reg_90, type = "text")

#Splitting test set based on quantiles
test_obesity_under_q10 <- test_obesity[test_obesity$BMI < quantile(test_obesity$BMI, 0.1),]
test_obesity_around_q10 <- test_obesity[test_obesity$BMI < quantile(test_obesity$BMI, 0.25),]
test_obesity_around_q25 <- test_obesity[test_obesity$BMI < quantile(test_obesity$BMI, 0.50) & 
                                          test_obesity$BMI > quantile(test_obesity$BMI, 0.10),]
test_obesity_around_q50 <- test_obesity[test_obesity$BMI < quantile(test_obesity$BMI, 0.75) & 
                                          test_obesity$BMI > quantile(test_obesity$BMI, 0.25),]
test_obesity_around_q75 <- test_obesity[test_obesity$BMI < quantile(test_obesity$BMI, 0.90) & 
                                          test_obesity$BMI > quantile(test_obesity$BMI, 0.50),]
test_obesity_around_q90 <- test_obesity[test_obesity$BMI > quantile(test_obesity$BMI, 0.75),]
test_obesity_above_q90 <- test_obesity[test_obesity$BMI > quantile(test_obesity$BMI, 0.90),]

#Prediction of BMI values around Q10
prediction_around_q10 <- test_obesity_around_q10 %>% 
  mutate(Prediction_q10 = exp(predict(quant_reg_10, test_obesity_around_q10)))

prediction_around_q10 %>% 
  dplyr::select(BMI, Prediction_q10) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.10) on predicting BMI values around Q10
mse(prediction_around_q10$BMI, prediction_around_q10$Prediction_q10)



#Prediction of BMI values around Q10 on using Quantile Regression (tau = 0.25)
prediction_around_q10 <- prediction_around_q10 %>% 
  mutate(Prediction_q25 = exp(predict(quant_reg_25, test_obesity_around_q10)))

prediction_around_q10 %>% 
  dplyr::select(BMI, Prediction_q10, Prediction_q25) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.25) on predicting BMI values around Q10
mse(prediction_around_q10$BMI, prediction_around_q10$Prediction_q25)



#Prediction of BMI values around Q10 on using Linear Regression
prediction_around_q10 <- prediction_around_q10 %>% 
  mutate(Prediction_lm = exp(predict(linear_mod_four, test_obesity_around_q10)))

prediction_around_q10 %>% 
  dplyr::select(BMI, Prediction_q10, Prediction_q25, Prediction_lm) %>% 
  summary()

#MSE obtained on using Linear Regression on predicting BMI values around Q10
mse(prediction_around_q10$BMI, prediction_around_q10$Prediction_lm)

#Prediction of BMI values around Q25 on using Quantile Regression (tau = 0.10)
prediction_around_q25 <- test_obesity_around_q25 %>% 
  mutate(Prediction_q10 = exp(predict(quant_reg_10, test_obesity_around_q25)))

prediction_around_q25 %>% 
  dplyr::select(BMI, Prediction_q10) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.10) on predicting BMI values around Q25
mse(prediction_around_q25$BMI, prediction_around_q25$Prediction_q10)



#Prediction of BMI values around Q25 on using Quantile Regression (tau = 0.25)
prediction_around_q25 <- prediction_around_q25 %>% 
  mutate(Prediction_q25 = exp(predict(quant_reg_25, test_obesity_around_q25)))

prediction_around_q25 %>% 
  dplyr::select(BMI, Prediction_q10, Prediction_q25) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.25) on predicting BMI values around Q25
mse(prediction_around_q25$BMI, prediction_around_q25$Prediction_q25)



#Prediction of BMI values around Q25 on using Quantile Regression (tau = 0.50)
prediction_around_q25 <- prediction_around_q25 %>% 
  mutate(Prediction_q50 = exp(predict(quant_reg_50, test_obesity_around_q25)))

prediction_around_q25 %>% 
  dplyr::select(BMI, Prediction_q10, Prediction_q25, Prediction_q50) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.25) on predicting BMI values around Q25
mse(prediction_around_q25$BMI, prediction_around_q25$Prediction_q50)



#Prediction of BMI values around Q25 on using Linear Regression
prediction_around_q25 <- prediction_around_q25 %>% 
  mutate(Prediction_lm = exp(predict(linear_mod_four, test_obesity_around_q25)))

prediction_around_q25 %>% 
  dplyr::select(BMI, Prediction_q10, Prediction_q25, Prediction_q50, Prediction_lm) %>% 
  summary()

#MSE obtained on using Linear Regression on predicting BMI values around Q25
mse(prediction_around_q25$BMI, prediction_around_q25$Prediction_lm)



#Prediction of BMI values around Q50 on using Quantile Regression (tau = 0.25)
prediction_around_q50 <- test_obesity_around_q50 %>% 
  mutate(Prediction_q25 = exp(predict(quant_reg_25, test_obesity_around_q50)))

prediction_around_q50 %>% 
  dplyr::select(BMI, Prediction_q25) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.25) on predicting BMI values around Q50
mse(prediction_around_q50$BMI, prediction_around_q50$Prediction_q25)



#Prediction of BMI values around Q50 on using Quantile Regression (tau = 0.50)
prediction_around_q50 <- prediction_around_q50 %>% 
  mutate(Prediction_q50 = exp(predict(quant_reg_50, test_obesity_around_q50)))

prediction_around_q50 %>% 
  dplyr::select(BMI, Prediction_q25, Prediction_q50) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.50) on predicting BMI values around Q50
mse(prediction_around_q50$BMI, prediction_around_q50$Prediction_q50)



#Prediction of BMI values around Q50 on using Quantile Regression (tau = 0.75)
prediction_around_q50 <- prediction_around_q50 %>% 
  mutate(Prediction_q75 = exp(predict(quant_reg_75, test_obesity_around_q50)))

prediction_around_q50 %>% 
  dplyr::select(BMI, Prediction_q25, Prediction_q50, Prediction_q75) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.75) on predicting BMI values around Q50
mse(prediction_around_q50$BMI, prediction_around_q50$Prediction_q75)



#Prediction of BMI values around Q50 on using Linear Regression
prediction_around_q50 <- prediction_around_q50 %>% 
  mutate(Prediction_lm = exp(predict(linear_mod_four, test_obesity_around_q50)))

prediction_around_q50 %>% 
  dplyr::select(BMI, Prediction_q25, Prediction_q50, Prediction_q75, Prediction_lm) %>% 
  summary()

#MSE obtained on using Linear Regression on predicting BMI values around Q50
mse(prediction_around_q50$BMI, prediction_around_q50$Prediction_lm)



#Prediction of BMI values around Q75 on using Quantile Regression (tau = 0.50)
prediction_around_q75 <- test_obesity_around_q75 %>% 
  mutate(Prediction_q50 = exp(predict(quant_reg_50, test_obesity_around_q75)))

prediction_around_q75 %>% 
  dplyr::select(BMI, Prediction_q50) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.50) on predicting BMI values around Q75
mse(prediction_around_q75$BMI, prediction_around_q75$Prediction_q50)



#Prediction of BMI values around Q75 on using Quantile Regression (tau = 0.75)
prediction_around_q75 <- prediction_around_q75 %>% 
  mutate(Prediction_q75 = exp(predict(quant_reg_75, test_obesity_around_q75)))

prediction_around_q75 %>% 
  dplyr::select(BMI, Prediction_q50, Prediction_q75) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.75) on predicting BMI values around Q75
mse(prediction_around_q75$BMI, prediction_around_q75$Prediction_q75)



#Prediction of BMI values around Q75 on using Quantile Regression (tau = 0.90)
prediction_around_q75 <- prediction_around_q75 %>% 
  mutate(Prediction_q90 = exp(predict(quant_reg_90, test_obesity_around_q75)))

prediction_around_q75 %>% 
  dplyr::select(BMI, Prediction_q50, Prediction_q75, Prediction_q90) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.90) on predicting BMI values around Q75
mse(prediction_around_q75$BMI, prediction_around_q75$Prediction_q90)



#Prediction of BMI values around Q75 on using Linear Regression
prediction_around_q75 <- prediction_around_q75 %>% 
  mutate(Prediction_lm = exp(predict(linear_mod_four, test_obesity_around_q75)))

prediction_around_q75 %>% 
  dplyr::select(BMI, Prediction_q50, Prediction_q75, Prediction_q90, Prediction_lm) %>% 
  summary()

#MSE obtained on using Linear Regression on predicting BMI values around Q75
mse(prediction_around_q75$BMI, prediction_around_q75$Prediction_lm)



#Prediction of BMI values around Q90 on using Quantile Regression (tau = 0.75)
prediction_around_q90 <- test_obesity_around_q90 %>% 
  mutate(Prediction_q75 = exp(predict(quant_reg_75, test_obesity_around_q90)))

prediction_around_q90 %>% 
  dplyr::select(BMI, Prediction_q75) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.50) on predicting BMI values around Q75
mse(prediction_around_q90$BMI, prediction_around_q90$Prediction_q75)



#Prediction of BMI values around Q90 on using Quantile Regression (tau = 0.90)
prediction_around_q90 <- prediction_around_q90 %>% 
  mutate(Prediction_q90 = exp(predict(quant_reg_90, test_obesity_around_q90)))

prediction_around_q90 %>% 
  dplyr::select(BMI, Prediction_q75, Prediction_q90) %>% 
  summary()

#MSE obtained on using Quantile Regression (tau = 0.90) on predicting BMI values around Q75
mse(prediction_around_q90$BMI, prediction_around_q90$Prediction_q90)



#Prediction of BMI values around Q75 on using Linear Regression
prediction_around_q90 <- prediction_around_q90 %>% 
  mutate(Prediction_lm = exp(predict(linear_mod_four, test_obesity_around_q90)))

prediction_around_q90 %>% 
  dplyr::select(BMI, Prediction_q75, Prediction_q90, Prediction_lm) %>% 
  summary()

#MSE obtained on using Linear Regression on predicting BMI values around Q90
mse(prediction_around_q90$BMI, prediction_around_q90$Prediction_lm)


#By observing the MSEs, we verify that the quantile regression model was indeed 
#good and necessary for observing the effect of poredictors at different 
#quantiles of the reponse variable.


# Machine Learning Techniques ----

## Classification Tree ----

#Fitting Classification Tree Model
ctree_fit <- rpart(Obese ~ Age + Sex + Education + Veg + Fruit + Year, data=train_over_sampled[,-c(7)])

#Displaying the constructed tree
rpart.plot(ctree_fit, type = 2, extra = 4)

#Summarizing the fit
summary(ctree_fit)

#Making the predictions
ctree_prediction <- predict(ctree_fit, test_obesity, type="class")

#Summarizing the accuracy
table(ctree_prediction, test_obesity$Obese)

#Building the Contingency Table
ctree_contingency_table <- table(test_obesity$Obese, ctree_prediction)
ctree_contingency_table

ctree_confusion_matrix <- confusionMatrix(test_obesity$Obese, ctree_prediction)
ctree_confusion_matrix

#Calculating Mis-classification Error
ctree_misclassification_error <- 1 - sum(diag(ctree_contingency_table)) / 
  sum(ctree_contingency_table)
ctree_misclassification_error

message("Classification Tree - Mis-classification Error: ", ctree_misclassification_error, " %")

#Calculating Precision
ctree_precision <- round(ctree_contingency_table[1,1] / 
                           (ctree_contingency_table[1,1] + 
                              ctree_contingency_table[2,1]) * 100, 1) 
ctree_precision  

message("Classification Tree - Precision: ", ctree_precision, " %")

#Calculating Recall
ctree_recall <- round(ctree_contingency_table[1] / 
                        (ctree_contingency_table[1] + 
                           ctree_contingency_table[1,2]) * 100, 1)
ctree_recall

message("Classification Tree - Recall: ", ctree_recall, " %")

#Calculating Accuracy
ctree_accuracy <- round((ctree_contingency_table[1] + ctree_contingency_table[2,2]) / 
                          (ctree_contingency_table[1] + 
                             ctree_contingency_table[1,2] + 
                             ctree_contingency_table[2,1] + 
                             ctree_contingency_table[2,2]) * 100, 1)
ctree_accuracy

message("Classification Tree - Accuracy: ", ctree_accuracy, " %")

#Calculating F1 Score
ctree_f1Score <- round(2 * (ctree_precision * ctree_recall) / 
                         (ctree_precision + ctree_recall))
ctree_f1Score

message("Classification Tree - F1 Score: ", ctree_f1Score, " %")

## Bagging ----

#Fitting the bagging model
bag_fit_one <- randomForest(Obese ~ Age + Sex + Education + Veg + Fruit + Year, data=train_under_sampled[,-c(7)], mtry = 6, ntree = 200)

summary(bag_fit_one)

# Summarizing the fit
bag_fit_one

# Making the predictions
bagging_prediction <- predict(bag_fit_one, test_obesity, type="class")

#Summarizing the accuracy
table(bag_fit_one, test_obesity$Obese)

#Building the Contingency Table
bagging_contingency_table <- table(test_obesity$Obese, bagging_prediction)
bagging_contingency_table

#Building the Confusion Matrix
bagging_confusion_matrix <- confusionMatrix(test_obesity$Obese, bagging_prediction)
bagging_confusion_matrix

#Calculating Mis-classification Error
bagging_misclassification_error <- 1 - sum(diag(bagging_contingency_table)) / sum(bagging_contingency_table)
bagging_misclassification_error

message("Bagging - Mis-classification Error: ", bagging_misclassification_error, " %")

#Calculating Precision
bagging_precision <- round(bagging_contingency_table[1,1] / 
                             (bagging_contingency_table[1,1] + 
                                bagging_contingency_table[2,1]) * 100, 1)
bagging_precision

message("Bagging - Precision: ", bagging_precision, " %")

#Calculating Recall
bagging_recall <- round(bagging_contingency_table[1] / 
                          (bagging_contingency_table[1] + 
                             bagging_contingency_table[1,2]) * 100, 1)
bagging_recall

message("Bagging - Recall: ", bagging_recall, " %")

#Calculating Accuracy
bagging_accuracy <- round((bagging_contingency_table[1] + bagging_contingency_table[2,2]) / 
                            (bagging_contingency_table[1] + 
                               bagging_contingency_table[1,2] + 
                               bagging_contingency_table[2,1] + 
                               bagging_contingency_table[2,2]) * 100, 1)
bagging_accuracy

message("Bagging - Accuracy: ", bagging_accuracy, " %")

#Calculating F1 Score
bagging_f1Score <- round(2 * (bagging_precision * bagging_recall) / 
                           (bagging_precision + bagging_recall))
bagging_f1Score

message("Bagging - F1 Score: ", bagging_f1Score, " %")

## Random Forest ----

#Random Forest in R

#Fitting the Random Forest model
rf_fit_one <- randomForest(Obese ~ Age + Sex + Education + Veg + Fruit + Year, data=train_under_sampled[,-c(7)], ntree = 200)
rf_fit_one

#Checking the importance of variables based on Gini Index
importance(rf_fit_one)
library(vip)
vip(rf_fit_one, geom = "point", ) 

#Summarizing the fit
rf_fit_one

#Making the predictions
rf_fit_one_prediction <- predict(rf_fit_one, test_obesity, type="class")

#Summarizing the accuracy
table(rf_fit_one_prediction, test_obesity$Obese)

#Building the Contingency Table
rf_fit_one_contingency_table <- table(test_obesity$Obese, rf_fit_one_prediction)
rf_fit_one_contingency_table

#Computing the Confusion Matrix
library(caret)
rf_fit_one_confusion_matrix <- confusionMatrix(test_obesity$Obese,rf_fit_one_prediction)
rf_fit_one_confusion_matrix

#Calculating Mis-classification Error
rf_fit_one_misclassification_error <- 1 - sum(diag(rf_fit_one_contingency_table)) / 
  sum(rf_fit_one_contingency_table)
rf_fit_one_misclassification_error 

message("Random Forest Model 1 - Mis-classification Error: ", rf_fit_one_misclassification_error, " %")

#Calculating Precision
rf_fit_one_precision <- round(rf_fit_one_contingency_table[1,1] / 
                                (rf_fit_one_contingency_table[1,1] + 
                                   rf_fit_one_contingency_table[2,1]) * 100, 1)
rf_fit_one_precision

message("Random Forest Model 1 - Precision: ", rf_fit_one_precision, " %")

#Calculating Recall
rf_fit_one_recall <- round(rf_fit_one_contingency_table[1] / 
                             (rf_fit_one_contingency_table[1] + 
                                rf_fit_one_contingency_table[1,2]) * 100, 1)
rf_fit_one_recall

message("Random Forest Model 1 - Recall: ", rf_fit_one_recall, " %")

#Calculating Accuracy
rf_fit_one_accuracy <- round((rf_fit_one_contingency_table[1] + rf_fit_one_contingency_table[2,2]) / 
                               (rf_fit_one_contingency_table[1] + 
                                  rf_fit_one_contingency_table[1,2] + 
                                  rf_fit_one_contingency_table[2,1] + 
                                  rf_fit_one_contingency_table[2,2]) * 100, 1)
rf_fit_one_accuracy

message("Random Forest Model 1 - Accuracy: ", rf_fit_one_accuracy, " %")

#Calculating F1 Score
rf_fit_one_f1Score <- round(2 * (rf_fit_one_precision * rf_fit_one_recall) / 
                              (rf_fit_one_precision + rf_fit_one_recall))
rf_fit_one_f1Score

message("Random Forest - F1 Score: ", rf_fit_one_f1Score, " %")

## Gradient Boosting ----

#Gradient Boosting in R
#Loading the package
library(gbm)

#Converting the under-sampled dataset to one with Obese values as 0 and 1 for performing boosting technique
boost_train_under_sampled <- train_under_sampled
boost_train_under_sampled$Obese <- ifelse(boost_train_under_sampled$Obese=="Yes",1,0)

#Fitting the Boosting model
boosting_fit <- gbm(Obese~., data=boost_train_under_sampled[,-c(6,7)], distribution="bernoulli")

str(train_under_sampled)

#Summarizing the fit
print(boosting_fit)

#Making the predictions
boosting_prediction <- predict(fit, test_obesity, type="class")

#Summarizing the accuracy
table(boosting_prediction, test_obesity$Obese)

#Building the Contingency Table
boosting_contingency_table <- table(test_obesity$Obese, boosting_prediction)
boosting_contingency_table

#Computing the Confusion Matrix
boosting_confusion_matrix <- confusionMatrix(test_obesity$Obese, boosting_prediction)
boosting_confusion_matrix

#Calculating Mis-classification Error
boosting_misclassification_error <- 1 - sum(diag(boosting_contingency_table))/sum(boosting_contingency_table)
boosting_misclassification_error

message("Boosting - Mis-classification Error: ", boosting_misclassification_error, " %")

#Calculating Precision
boosting_precision <- round(boosting_contingency_table[1,1] / 
                              (boosting_contingency_table[1,1] + 
                                 boosting_contingency_table[2,1]) * 100, 1)
boosting_precision

message("Boosting - Precision: ", boosting_precision, " %")

#Calculating Recall
boosting_recall <- round(boosting_contingency_table[1] / 
                           (boosting_contingency_table[1] + 
                              boosting_contingency_table[1,2]) * 100, 1)
boosting_recall

message("Boosting - Recall: ", boosting_recall, " %")

#Calculating Accuracy
boosting_accuracy <- round((boosting_contingency_table[1] + boosting_contingency_table[2,2]) / 
                             (boosting_contingency_table[1] + 
                                boosting_contingency_table[1,2] + 
                                boosting_contingency_table[2,1] + 
                                boosting_contingency_table[2,2]) * 100, 1)
boosting_accuracy

message("Boosting - Accuracy: ", boosting_accuracy, " %")


#Calculating F1 Score
boosting_f1Score <- round(2 * (boosting_precision * boosting_recall) / 
                            (boosting_precision + boosting_recall))
boosting_f1Score