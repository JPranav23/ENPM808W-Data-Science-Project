# Importing and Reading datasets

cwur <- read.csv("~/Desktop/Data Science 808W/My Project/world-university-rankings/cwurData.csv")
View(cwur)

education_expenditure_supplementary_data <- read.csv("~/Desktop/Data Science 808W/My Project/world-university-rankings/education_expenditure_supplementary_data.csv")
View(education_expenditure_supplementary_data)

school_and_country_table <- read.csv("~/Desktop/Data Science 808W/My Project/world-university-rankings/school_and_country_table.csv")
View(school_and_country_table)

shanghai <- read.csv("~/Desktop/Data Science 808W/My Project/world-university-rankings/shanghaiData.csv")
View(shanghai)

times <- read.csv("~/Desktop/Data Science 808W/My Project/world-university-rankings/timesData.csv")
View(times)

# Analyzing the dataset

summary(school_and_country_table$country)
levels(cwur$country)

# There are 59 countries in cwur dataset.

sum(is.na(cwur))
sum(is.na(cwur$broad_impact))

# Broad impact column has 200 missing values

summary(cwur$country)

# Maximum universities in this dataset are from USA.

range(cwur$world_rank)
mean(cwur$score[cwur$country == "USA"])
mean(cwur$score[cwur$country == "India"])
mean(cwur$score[cwur$country == "United Kingdom"])

# Mean score for USA is greater than mean score for UK which is greater than mean score for India.

range(cwur$patents)
range(cwur$year)

# cwur has data recorded from year 2012 to 2015.

levels(education_expenditure_supplementary_data$direct_expenditure_type)

# There are 3 types of direct expenditure.

sum(education_expenditure_supplementary_data$direct_expenditure_type == "Public")
sum(education_expenditure_supplementary_data$direct_expenditure_type == "Private")
sum(education_expenditure_supplementary_data$direct_expenditure_type == "Total")

sum(is.na(times))
levels(times$world_rank)
levels(times$country)

# There are 72 countries in times dataset.

range(times$year)

# Times dataset has data recorded from 2011 to 2016.

sum(is.na(shanghai))
levels(shanghai$world_rank)
range(shanghai$year)

# Shanghai dataset has data recorded from year 2005 to 2015.

# According to my initial analysis of data, I would be taking cwur data and times dataset for further analysis and model making.

# Visualization

library(tibble)
library(ggplot2)
x <- c(education_expenditure_supplementary_data$direct_expenditure_type)
table(x)
hist(x, col = 1:3, main = "Distribution of Expenditure types", las = 1, xlab = "Type")
ggplot(cwur, aes(score, world_rank, col = 4)) + 
  geom_line() + ggtitle("Score vs World Rank") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Score") + ylab("World Rank")

# We can see that the world rank exponentially increases as the increases
# upto a score of 70 approximately.

# Creating a new object with cwur data for correlation analysis

newpQData <- cwur
newpQData <- newpQData[,-c(2, 3, 4, 11)]

# Creating graphs to find correlation between pairs

pairs(newpQData)

# Creating objects that store information about correlation coefficients using different methods

cr1 <- cor(newpQData, method = "pearson")
cr2 <- cor(newpQData, method = "spearman")
cr3 <- cor(newpQData, method = "kendall")

# Plots

par(mfrow = c(1, 1))
library(corrplot)
corrplot(cr1, type = "lower")
corrplot(cr2, type = "lower")
corrplot(cr3, type = "lower")

# As we see, publications, influence and citations are dominant factors in getting a good world rank.
# As the publications, influence and citations in any university decreases, the university gets a better world rank.
# As the score increases, the rank gets better.

# Splitting the data into training and test data

index_cwur <- 1:nrow(cwur)
testindex_cwur <- sample(index_cwur, trunc(length(index_cwur)/5))
testset_cwur <- cwur[testindex_cwur,]
trainset_cwur <- cwur[-testindex_cwur,]

# Models

model1 <- lm(world_rank~publications+influence+citations+patents, data = trainset_cwur)
summary(model1)
model2 <- lm(world_rank~alumni_employment+quality_of_education, data = trainset_cwur)
summary(model2)

# Predicting using models made

pred_lm1 <- predict(model1, data = testset_cwur)
summary(pred_lm1)
pred_lm1 <- cbind(pred_lm1, testset_cwur$world_rank)
View(pred_lm1)
pred_lm2 <- predict(model2, data = testset_cwur)
summary(pred_lm2)
pred_lm2 <- cbind(pred_lm2, testset_cwur$world_rank)
View(pred_lm2)

# As we see, the prediction of rank using linear regression method is highly inaccurate.
# So we will have to find out other modelling techniques to predict the rank of universities.

# Times_Data Analysis
# Subsetting the data for USA

range(times$year)
times_sub <- subset(times, country = "United States of America")
str(times_sub)
sum(is.na(times_sub))
times_sub$world_rank<-as.numeric(times_sub$world_rank)
times_sub$num_students<-as.numeric(times_sub$num_students)
times_sub$income<-as.numeric(times_sub$income)
times_sub$international <-as.numeric(times_sub$international)
times_sub$total_score<-as.numeric(times_sub$total_score)

# Piping

library(dplyr)
View(times_sub %>% group_by(times_sub$world_rank) %>% 
  tally %>% arrange(desc(n)))

# Correlation and Correlation Plots

cor(times_sub[c(1,4,5,6,7,8,9,10)])
corrplot(cor(times_sub[c(1,4,5,6,7,8,9,10)]), type = "lower")

# As we see world rank does not have any strong positive or negative correlation with any other factors.
# However, research and teaching are highly correlated.
# Teaching and research have a dominant role in getting better total score.

# Plot for dominants factors

par(mfrow = c(1, 2))
boxplot(times_sub$research, main = "Research Boxplot", las = 1)
boxplot(times_sub$teaching, main = "Teaching Boxplot", las = 1)
par(mfrow = c(1, 1))

# Making an subset for universities with above average income and teaching quality

summary(times_sub$income)
best_uni<-subset(times_sub,times_sub$income > 200 & times_sub$teaching > 40)
dim(best_uni)
a <- aggregate(
  best_uni$income,
  by = list(best_uni$teaching),
  FUN = mean,
  na.rm = TRUE
)
View(a)

# Loading required libraries-

library(dplyr)
library(tidyr)
library(stringr)

partc <- select(cwur, institution, country, alumni_employment, citations, world_rank, year)
summarise(partc, num_universities = n_distinct(institution))
summarise(partc, num_countries = n_distinct(country))  
paste0("Number of NA cells = ", sum(is.na(partc))) 
partc <- mutate_each(partc, funs(as.numeric), alumni_employment, citations, world_rank)

med.R <- partc %>%
  group_by(institution, country) %>%
  summarise_each(funs(median), alumni_employment, citations, world_rank)

head(med.R)

high.rank <- filter(med.R, alumni_employment < 100, citations < 100)

high.rank %>%
  group_by(Country = country) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

high.rank$institution <- str_replace(high.rank$institution, 'University', 'Univ.')
high.rank$institution <- str_replace(high.rank$institution, 'Massachusetts Institute of Technology', 'MIT')
high.rank$institution <- str_replace(high.rank$institution, 'California', 'Cal.')
high.rank$institution <- str_replace(high.rank$institution, 'Michigan', 'Mich.')
high.rank$country <- str_replace(high.rank$country, 'United Kingdom', 'UK')
high.rank$country <- str_replace(high.rank$country, 'South Korea', 'S. Korea')

head(arrange(high.rank, alumni_employment), 15)

high.rank$institution <- str_replace(high.rank$institution, 'Federal Institute of Technology', 'FIT')

head(high.rank %>% filter(country != 'USA') %>% arrange(alumni_employment), 15)

### Update after Midterm Deliverable-

## Logistic Regression Models to predict whether the university is a top ranked university or not.

# Finding the years through which data is present

range(cwur$year)

# Subsetting data for each year

j_2012 <- cwur[cwur$year == 2012,]
j_2013 <- cwur[cwur$year == 2013,]
j_2014 <- cwur[cwur$year == 2014,]
j_2015 <- cwur[cwur$year == 2015,]

# Finding the number of observations in each year

dim(j_2012)
dim(j_2013)
dim(j_2014)
dim(j_2015)

# Finding the range of world rank

range(j_2014$world_rank)
range(j_2015$world_rank)

# Creating a new column to tell whether the university is in top 150 world rank or not.

j <- cwur
j$top_rank <- ifelse(cwur$world_rank > 150, 0, 1)
dim(j)
dim(cwur)

j_2014$top_rank <- ifelse(j_2014$world_rank > 150, 0, 1)
j_2015$top_rank <- ifelse(j_2015$world_rank > 150, 0, 1)

# Logistic Regression Model-

# Seperating complete dataset into training dataset and test dataset-

set.seed(7658)
index <- sample(2, nrow(j), replace = T, prob = c(0.7, 0.3))
train.data <- j[index == 1, ]
View(train.data)
test.data <- j[index == 2, ]
View(test.data)

# Models, Predictions, Error Matrix and Accuracy-

lr1 <- glm(top_rank ~ quality_of_education + alumni_employment + quality_of_faculty + score, data = train.data)
pred1 <- predict(lr1, test.data, type = "response")
range(pred1)
pred1 <- ifelse(pred1 > 0.7, 1, 0)
t1 <- table(pred1, test.data$top_rank)
t1
sum(diag(t1))/sum(t1)

# As we see, the accuracy of the above model is around 90%.

lr2 <- glm(top_rank ~ publications + influence + citations + patents, data = train.data)
pred2 <- predict(lr2, test.data, type = "response")
range(pred2)
pred2 <- ifelse(pred2 > 0.45, 1, 0)
t2 <- table(pred2, test.data$top_rank)
t2
sum(diag(t2))/sum(t2)

# As we see, the accuracy of the above model is around 92%.

# Partitioning complete dataset into training and testing dataset-

set.seed(7658)
index_2014 <- sample(2, nrow(j_2014), replace = T, prob = c(0.7, 0.3))
train.data_2014 <- j_2014[index_2014 == 1, ]
test.data_2014 <- j_2014[index_2014 == 2, ]

set.seed(7658)
index_2015 <- sample(2, nrow(j_2015), replace = T, prob = c(0.7, 0.3))
train.data_2015 <- j_2014[index_2015 == 1, ]
test.data_2015 <- j_2014[index_2015 == 2, ]

# Logistic Regression Models, Predictions, Error Matrix and Accuracy-

lr_2014 <- glm(top_rank ~ publications + influence + citations + patents, data = train.data_2014)
pred_2014 <- predict(lr_2014, test.data_2014, type = "response")
range(pred_2014)
pred_2014 <- ifelse(pred_2014 > 0.4, 1, 0)
length(pred_2014)
length(test.data_2014$top_rank)
t_2014 <- table(pred_2014, test.data_2014$top_rank)
t_2014
sum(diag(t_2014))/sum(t_2014)

lr_2015 <- glm(top_rank ~ quality_of_education + alumni_employment + quality_of_faculty + score, data = train.data_2015)
pred_2015 <- predict(lr_2015, test.data_2015, type = "response")
range(pred_2015)
pred_2015 <- ifelse(pred_2015 > 0.38, 1, 0)
length(pred_2015)
length(test.data_2015$top_rank)
t_2015 <- table(pred_2015, test.data_2015$top_rank)
t_2015
sum(diag(t_2015))/sum(t_2015)

# Logistic regression for American Universities-

j_2012 <- cwur[cwur$year == 2012,]
j_2013 <- cwur[cwur$year == 2013,]
j_2014 <- cwur[cwur$year == 2014,]
j_2015 <- cwur[cwur$year == 2015,]

j_2014_USA <- j_2014[j_2014$country == "USA",]
j_2015_USA <- j_2015[j_2015$country == "USA",]

dim(j_2014_USA)
dim(j_2015_USA)

j_2014_USA$top_rank <- ifelse(j_2014_USA$national_rank > 40, 0, 1)
j_2015_USA$top_rank <- ifelse(j_2015_USA$national_rank > 40, 0, 1)

# Partitioning Data

set.seed(7658)
index <- sample(2, nrow(j_2014_USA), replace = T, prob = c(0.7, 0.3))
train.data_2014 <- j_2014_USA[index == 1, ]
test.data_2014 <- j_2014_USA[index == 2, ]

set.seed(7658)
index <- sample(2, nrow(j_2015_USA), replace = T, prob = c(0.7, 0.3))
train.data_2015 <- j_2015_USA[index == 1, ]
test.data_2015 <- j_2015_USA[index == 2, ]

# Logistic Regression Models-

lr5 <- glm(top_rank ~ quality_of_education + alumni_employment + quality_of_faculty + score, data = train.data_2014)
pred5 <- predict(lr5, test.data_2014, type = "response")
range(pred5)
pred5 <- ifelse(pred5 > 0.95, 1, 0)
t5 <- table(pred5, test.data_2014$top_rank)
t5
sum(diag(t5))/sum(t5)

# As we see, the accuracy of the above model is around 90%.

lr6 <- glm(top_rank ~ publications + influence + citations + patents, data = train.data_2014)
pred6 <- predict(lr6, test.data_2014, type = "response")
range(pred6)
pred6 <- ifelse(pred6 > 0.45, 1, 0)
t6 <- table(pred6, test.data_2014$top_rank)
t6
sum(diag(t6))/sum(t6)

# As we see, the accuracy of the above model is around 95%.

lr7 <- glm(top_rank ~ quality_of_education + alumni_employment + quality_of_faculty + score, data = train.data_2015)
pred7 <- predict(lr7, test.data_2015, type = "response")
range(pred7)
pred7 <- ifelse(pred7 > 0.95, 1, 0)
t7 <- table(pred7, test.data_2015$top_rank)
t7
sum(diag(t7))/sum(t7)

# As we see, the accuracy of the above model is around 90%.

lr8 <- glm(top_rank ~ publications + influence + citations + patents, data = train.data_2015)
pred8 <- predict(lr8, test.data_2015, type = "response")
range(pred8)
pred8 <- ifelse(pred8 > 0.45, 1, 0)
t8 <- table(pred8, test.data_2015$top_rank)
t8
sum(diag(t8))/sum(t8)

# As we see, the accuracy of the above model is around 95%.

# From the above models for the year 2014 and 2015, we can say that accuracy of resesarch based as well as employement based models remain approximately sinmilar over the years.

# Logistic regression using times dataset-

range(times$year)
sum(times$year == 2011)
sum(times$year == 2012)
sum(times$year == 2013)
sum(times$year == 2014)
sum(times$year == 2015)
sum(times$year == 2016)

levels(times$world_rank)

times$top_rank <- ifelse(as.numeric(times$world_rank) < 150, 1, 0)

sum(times$top_rank == 1)
sum(times$top_rank == 0)

# Data type changing-

times$teaching <- as.numeric(times$teaching)
times$international <- as.numeric(times$international)
times$research <- as.numeric(times$research)
times$citations <- as.numeric(times$citations)
times$income <- as.numeric(times$income)
times$total_score <- as.numeric(times$total_score)
times$num_students <- as.numeric(times$num_students)
times$student_staff_ratio <- as.numeric(times$student_staff_ratio)

# Splitting-

set.seed(7658)
index <- sample(2, nrow(times), replace = T, prob = c(0.7, 0.3))
train.data_times <- times[index == 1, ]
test.data_times <- times[index == 2, ]

# Models, Prediction, Error Matrix and Accuracy-

lr9 <- glm(top_rank ~ teaching + international + research + citations + income + total_score + num_students + student_staff_ratio, data = train.data_times)
pred9 <- predict(lr9, test.data_times, type = "response")
summary(pred9)
pred9 <- ifelse(pred9 > 0.38, 1, 0)
t9 <- table(pred9, test.data_times$top_rank)
t9
sum(diag(t9))/sum(t9)

# As we see, the accuracy of this model is around 72%.

lr10 <- glm(top_rank ~ teaching + international + income + num_students + student_staff_ratio, data = train.data_times)
pred10 <- predict(lr10, test.data_times, type = "response")
summary(pred10)
pred10 <- ifelse(pred10 > 0.34, 1, 0)
t10 <- table(pred10, test.data_times$top_rank)
t10
sum(diag(t10))/sum(t10)

# Accuracy of the employment based logistic regression model is around 70%.

lr11 <- glm(top_rank ~ research + citations + total_score, data = train.data_times)
pred11 <- predict(lr11, test.data_times, type = "response")
summary(pred11)
pred11 <- ifelse(pred11 > 0.37, 1, 0)
t11 <- table(pred11, test.data_times$top_rank)
t11
sum(diag(t11))/sum(t11)

# As we see, this model has the least accuracy among all the models developed uptill now which is 67%.

# Logistic Regression using Shanghai dataset-

levels(shanghai$world_rank)

# Alotting binary levels-

shanghai$top_rank <- ifelse(as.numeric(shanghai$world_rank) < 100, 1, 0)

# Changing the data type of each column to numeric-

shanghai$world_rank <- as.numeric(shanghai$world_rank)
shanghai$total_score <- as.numeric(shanghai$total_score)
shanghai$alumni <- as.numeric(shanghai$alumni)
shanghai$award <- as.numeric(shanghai$award)
shanghai$hici <- as.numeric(shanghai$hici)
shanghai$ns <- as.numeric(shanghai$ns)
shanghai$pub <- as.numeric(shanghai$pub)
shanghai$pcp <- as.numeric(shanghai$pcp)

# Splitting-

set.seed(7658)
index <- sample(2, nrow(shanghai), replace = T, prob = c(0.7, 0.3))
train.data_shanghai <- shanghai[index == 1, ]
test.data_shanghai <- shanghai[index == 2, ]

# Model-

lr12 <- glm(top_rank ~ total_score + alumni + award + hici + ns + pub + pcp, data = train.data_shanghai)
pred12 <- predict(lr12, test.data_shanghai, type = "response")
summary(pred12)
pred12 <- ifelse(pred12 > 0.91, 1, 0)
t12 <- table(pred12, test.data_shanghai$top_rank)
t12
sum(diag(t12))/sum(t12)

# Model has a very low accuracy of around 45%. 

## Classifying universities based on a seperate feature created-

# Reading Dataset-

cwur <- read.csv("~/Desktop/Data Science 808W/My Project/world-university-rankings/cwurData.csv")

# Subsetting Data according to year-

max(cwur$quality_of_education)
part2 <- subset(cwur, year=="2012", select =c( world_rank:year))
part3 <- subset(cwur, year=="2013", select =c( world_rank:year))

# Feature Definition-

cwur$gen_res<-cwur$quality_of_faculty*0.1+cwur$publications*0.3+cwur$citations*0.3+cwur$patents*0.3

# Loading required libraries-

library(caret)
library(dplyr)

genres<- cwur$gen_res
max(genres)
min(genres)

# Labels of classes of universities-

my_research <- c("Average","Decent","Very Good","Outstanding")
cwur$cat <- cut(cwur$gen_res,breaks = c(0,200,500,700,Inf),labels = my_research)

# Assigning binary levels according to the feature score-

cwur$res<- ifelse(cwur$gen_res > 700,1,0)

# Analyzing data for further process-

a <- table(cwur$res)
no_1 <- as.data.frame(a)

# Splitting the complete dataset into training and testing dataset-

div <- createDataPartition(y=cwur$res,p=0.7,list = FALSE)
training <- cwur[div,]
testing <- cwur[-div,]

# Converting binary datatype to factors-

training[["res"]]= factor(training[["res"]])
testing[["res"]]= factor(testing[["res"]])

# Optimization Protocol-

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Model-

mod_1 <- train(res ~quality_of_faculty+publications, data = training, method = "svmLinear",
               trControl=control,
               preProcess = c("center", "scale"),
               tuneLength = 10)

# Prediction-

pred_1 <- predict(mod_1, newdata = testing)

# Error Matrix-

acc_1 <- confusionMatrix(pred_1, testing$res)
acc_1

# Increment in features-

mod_2 <- train(res ~quality_of_faculty+publications+citations, data = training, method = "svmLinear",
               trControl=control,
               preProcess = c("center", "scale"),
               tuneLength = 10)

pred_2 <- predict(mod_2, newdata = testing)

acc_2 <- confusionMatrix(pred_2, testing$res)
acc_2

mod_3 <- train(res ~influence, data = training, method = "svmLinear",
               trControl=control,
               preProcess = c("center", "scale"),
               tuneLength = 10)

pred_3 <- predict(mod_3,newdata = testing)
acc_3 <- confusionMatrix(pred_3, testing$res)
acc_3

# Testing out whether feature is relevant-

mod_4 <- train(res ~quality_of_education, data = training, method = "svmLinear",
               trControl=control,
               preProcess = c("center", "scale"),
               tuneLength = 10)

pred_4 <- predict(mod_4,newdata = testing)

acc_4 <- confusionMatrix(pred_4, testing$res)
acc_4

# Only quality of education-

mod_5 <-  train(res ~publications+citations+patents, data = training, method = "svmLinear",
                trControl=control,
                preProcess = c("center", "scale"),
                tuneLength = 10)

pred_5 <- predict(mod_5,newdata = testing)

acc_5 <- confusionMatrix(pred_5, testing$res)
acc_5

# Model with maximum accuracy (most relevant features)-

mod_6 <- train(res ~quality_of_faculty+quality_of_education, data = training, method = "svmLinear",
               trControl=control,
               preProcess = c("center", "scale"),
               tuneLength = 10)

pred_6 <- predict(mod_6, newdata = testing)

acc_6 <- confusionMatrix(pred_6, testing$res)
acc_6

# Model with less accuracy as compared to the best, one feature is irrelevant-

# Visualization 

occ <- table(cwur$country)
barplot (occ,main = 'Number of universities considered ',col = 'lightblue')

occ2 <- table(cwur$country[cwur$alumni_employment > '500'])
barplot(occ2,main = 'Country wise universities with high alumni employment' ,col= 'green' )

occ3 <- table(cwur$country[cwur$cat=='Average'])
barplot(occ3, main= 'Average (research) universities by country',col = 'red')

final_acc1 <- acc_1$overall
final_acc1.accuracy <- final_acc1['Accuracy']
predicted.accuracy <- c(88.78,94.39,86.36,88.79,99.1,88.79)
final_models<- c('mod_1','mod_2','mod_3','mod_4','mod_5','mod_6')

featur <- cbind(final_models,predicted.accuracy)
featur<-  data.frame(featur)

# Accuracy comparision Plots-

ggplot(featur, aes(final_models, predicted.accuracy))+geom_bar(stat="identity",color='blue')+xlab("SVM models")+ylab("Accuracy")

# Model5 has maximum accuracy followed by model2, model4, model6 and model1.

