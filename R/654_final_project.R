#654 project

#install.packages("janitor")
#install.packages('gt')

library(tidyverse)
library(janitor)
library(here)

require(readr)
require(caret)
require(recipes)
require(finalfit)
require(glmnet)
require(stringr)
require(ranger)
require(ggplot2)
require(forcats)
require(gt)

data <- read_csv("C:/Users/rlatimer/Documents/personal/EDLD MS/EDLD 654 DS/654project/data/aac_intakes_outcomes.csv")

missing <- ff_glimpse(data)
missing


#only keep dogs
initial <- dplyr::filter(data, grepl('Dog', animal_type))

#need to recode breeds, keeping first breed listed and removing 'mix'
x <- initial$breed
initial$breed1 <- (sapply(strsplit(x,'/'),`[`,1))
initial <- initial %>%
  select(-breed)

#remove "Mix" from breed label
breeds <- c(initial$breed1)
breed2 <- str_remove_all(breeds, "Mix")

initial3 <- cbind(initial,
                   as.data.frame(breed2)
)

#trim whitespace on breed
initial3$breed <- trimws(initial3$breed2, which = c("both"))

initial4 <- initial3 %>%
  select(-breed1, -breed2)


#keeping desired variables
aac <- initial4 %>%
  select(animal_id_outcome,outcome_type,sex_upon_outcome,
         outcome_month, animal_type,breed,intake_condition,intake_type,
         sex_upon_intake,age_upon_intake_years,intake_month,time_in_shelter_days)


#keep only breeds where n>=20
aac<-aac %>%
  group_by(breed) %>%
  filter(n() > 19)

#look at all the breeds and percent
table_breed<-aac %>% group_by(breed) %>% summarize(Freq=n()) %>% arrange(desc(Freq))
View(table_test)



#recipe

outcome <- 'time_in_shelter_days'

id      <- 'animal_id_outcome'

categorical <- c('outcome_type','sex_upon_outcome','breed','intake_condition',
                 'intake_type','sex_upon_intake')

numeric <- c('age_upon_intake_years')

cyclic <- c('outcome_month','intake_month')


blueprint_aac <- recipe(x     = aac,
                           vars  = c(outcome,categorical,numeric,cyclic),
                           roles = c('outcome',rep('predictor',9))) %>%
  step_indicate_na(all_of(categorical),all_of(numeric)) %>%
  step_zv(all_numeric()) %>%
  step_impute_mean(all_of(numeric)) %>%
  step_impute_mode(all_of(categorical)) %>%
  step_harmonic('intake_month',frequency=1,cycle_size=12,role='predictor') %>%
  step_harmonic('outcome_month',frequency=1,cycle_size=12,role='predictor') %>%
  step_ns('age_upon_intake_years',deg_free=3) %>%
  step_normalize(c(paste0(numeric,'_ns_1'),paste0(numeric,'_ns_2'),paste0(numeric,'_ns_3'))) %>%
  step_normalize(c("outcome_month_sin_1","outcome_month_cos_1","intake_month_sin_1","intake_month_cos_1")) %>%
  step_dummy(all_of(categorical),one_hot=TRUE) %>%
  step_rm(c('outcome_month','intake_month'))

#View(blueprint_aac %>% prep() %>% summary)

#splitting data for testing and training

set.seed(12042021)  # for reproducibility

loc      <- sample(1:nrow(aac), round(nrow(aac) * 0.8))
aac_train  <- aac[loc, ]
aac_test  <- aac[-loc, ]

#need all breeds in each sample
#loc2 <- aac %>% group_by(breed1) %>% sample_frac(0.8)
#  %>% else acc_test2


# Randomly shuffle the data

aac_train = aac_train[sample(nrow(aac_train)),]

# Create 10 folds with equal size

folds = cut(seq(1,nrow(aac_train)),breaks=10,labels=FALSE)

# Create the list for each fold 

my.indices <- vector('list',10)
for(i in 1:10){
  my.indices[[i]] <- which(folds!=i)
}

cv <- trainControl(method = "cv",
                   index  = my.indices)


#Ridge
grid <- data.frame(alpha = 0, lambda = seq(0.01,3,.05)) 
#grid

# Train the model

ridge <- caret::train(blueprint_aac, 
                      data      = aac_train, 
                      method    = "glmnet", 
                      trControl = cv,
                      tuneGrid  = grid)

#2 warnings...

ridge$bestTune
#alpha lambda
#35     0   1.71

predict_te_ridge <- predict(ridge, aac_test)

r_rsq_te <- cor(aac_test$time_in_shelter_days,predict_te_ridge)^2
r_rsq_te
# 
r_mae_te <- mean(abs(aac_test$time_in_shelter_days - predict_te_ridge))
r_mae_te
# 
r_rmse_te <- sqrt(mean((aac_test$time_in_shelter_days - predict_te_ridge)^2))
r_rmse_te
# 


#Look at most important predictors

  #using Logistic Regression with Ridge Penalty 

coefs <- coef(ridge$finalModel,
              ridge$bestTune$lambda)

coefs.zero <- coefs[which(coefs[,1]==0),]
length(coefs.zero)

coefs.nonzero <- coefs[which(coefs[,1]!=0),]
length(coefs.nonzero)

ind   <- order(abs(coefs.nonzero),decreasing=T)
head(as.matrix(coefs.nonzero[ind[-1]]),10)

#breed is the most important predictor of time in the shelter.


#Bagged Trees

cv <- trainControl(method = "cv",
                   index  = my.indices)

grid <- expand.grid(mtry = 9,splitrule='variance',min.node.size=2)
#grid

aac_bt <- caret::train(blueprint_aac,
                               data      = aac_train,
                               method    = 'ranger',
                               trControl = cv,
                               tuneGrid  = grid,
                               num.trees = 10,
                               max.depth = 60)

aac_bt$times

# Predictions from a Bagged tree model with xx trees

predicted_te <- predict(aac_bt,aac_test)

# MAE

bt_mae <- mean(abs(aac_test$time_in_shelter_days - predicted_te))
#12.36

# RMSE

bt_rmse <- sqrt(mean((aac_test$time_in_shelter_days - predicted_te)^2))
#34.38

# R-square

bt_rsqd <- cor(aac_test$time_in_shelter_days,predicted_te)^2
#.27

bagmod <- data.frame(Model = c("Bagged Trees Model"),
                     RMSE = c(bt_rmse),
                     MAE = c(bt_mae),
                     Rsq = c(bt_rsqd))

ridgemod <- data.frame(Model = c("Linear Regression with Ridge Penalty"),
                     RMSE = c(r_rmse_te),
                     MAE = c(r_mae_te),
                     Rsq = c(r_rsq_te))

#Final Table
SumTable <- rbind(bagmod, ridgemod)
SumTable

#Visualizations
#pivot wider?
month_count<-data %>%
  count(intake_month,sort=TRUE) 

month_count_wide <- month_count %>% 
  pivot_wider(names_from = "intake_month",
              values_from = "n")

#Rough histogram of total animal intakes by month
ggplot(data = month_count, mapping = aes(intake_month,n)) + 
  geom_col()

#Table of Intake of Animal Type by Month

smry <- data %>% 
  count(animal_type, intake_month) %>% 
  drop_na(animal_type,intake_month) %>%
  pivot_wider(names_from = "animal_type", 
   values_from = "n") 

smry %>% 
  gt() %>% 
tab_spanner(
  label = "Animal Type",
  columns = vars(`Bird`, `Cat`, `Dog`, `Other`)
) %>% 
  data_color(
    vars(`Bird`, `Cat`, `Dog`, `Other`),
    colors = scales::col_numeric(
      palette = c("#FFFFFF", "#FF0000"),
      domain = NULL
    )
  ) %>%
  cols_label(intake_month = "Intake Month")