#654 project

#install.packages("janitor")
library(tidyverse)
library(janitor)
library(here)

require(readr)
require(caret)
require(recipes)
require(finalfit)
require(glmnet)
require(stringr)

initial <- read_csv("C:/Users/rlatimer/Documents/personal/EDLD MS/EDLD 654 DS/654project/data/aac_intakes_outcomes.csv")

missing <- ff_glimpse(initial)
missing


#only keep dogs
initial <- dplyr::filter(initial, grepl('Dog', animal_type))

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

#getting errors in this section; I think because there are different "levels" of breed in
#the test dataset than are in the training dataset
#need to make sure the same breeds are in each (training and testing) datasets
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