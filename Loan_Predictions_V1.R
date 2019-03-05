# -----------------------------------------------------------------------------------
# Loan Predictions III
# -----------------------------------------------------------------------------------

library(tidyverse)
library(ggmosaic)
library(caret)
library(recipes)

set.seed(865)
setwd('C:/Users/Ryan/Desktop/Data Science/Analytics Vidhya Competitions/Loan Prediction Problem III/')



# -----------------------------------------------------------------------------------
# Load in Data and Inspect
# -----------------------------------------------------------------------------------

# Training Data
train <- read_csv("train.csv",
                  col_types = cols(Credit_History = col_character()))

# Testing Data
test <- read_csv("test.csv",
                 col_types = cols(Credit_History = col_character()))

# Look at Data
glimpse(train)
glimpse(test)

# Store Test Set IDs
test_ids <- test$Loan_ID

# Remove ID Variables
train$Loan_ID <- NULL
test$Loan_ID <- NULL

# Combine Training and Testing
train$train_test <- 'train'
test$train_test <- 'test'
dat <- bind_rows(train, test)



# -----------------------------------------------------------------------------------
# Exploratory Data Analysis -- Distributions
# -----------------------------------------------------------------------------------

colSums(is.na(dat))

# Dependent Variable Balance
train %>% 
  ggplot(aes(Loan_Status)) +
    geom_bar() # ~2:1 approved


# Plot Numeric Variables
dat %>% 
  select_if(is.numeric) %>% 
  gather(key = 'var', value = 'value') %>% 
  ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap( ~ var, scales = 'free')

# ApplicantIncome and CoapplicantIncome are both skewed right.
# Loan_Amount_Term may be categorical or binned
# Loan Amount is slightly skewed right

# Plot Categorical Variables
dat %>% 
  select_if(is.character) %>% 
  select(-Loan_Status, -train_test) %>% 
  gather(key = 'var', value = 'value') %>% 
  ggplot(aes(value)) +
    geom_bar() +
    facet_wrap( ~ var, scales = 'free')

plot(dat$Loan_Amount_Term, dat$LoanAmount)



# -----------------------------------------------------------------------------------
# Exploratory Data Analysis -- Missing Values
# -----------------------------------------------------------------------------------

# Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Where are the missing values?
colSums(is.na(dat))

# Loan Amount Term missing values
dat %>% 
  filter(Loan_Amount_Term == 360) %>% 
  summarize(mean(LoanAmount, na.rm = TRUE))

dat %>% 
  filter(Loan_Amount_Term == 180) %>% 
  summarize(mean(LoanAmount, na.rm = TRUE))

# Gender missing values
dat %>% 
  group_by(Gender) %>% 
  summarize(mean_LoanAmount = mean(LoanAmount, na.rm = TRUE)) %>% 
  ggplot(aes(x = Gender, y = mean_LoanAmount)) + 
    geom_bar(stat = 'identity') # Missing Genders are likely Male since their average loan amount is high

# Married missing values
dat %>% 
  group_by(Married) %>% 
  summarize(mean_LoanAmount = mean(LoanAmount, na.rm = TRUE)) %>% 
  ggplot(aes(x = Married, y = mean_LoanAmount)) + 
    geom_bar(stat = 'identity') # Missing Married are likely Yes since their average loan amount is high

# Dependents missing values
dat %>% 
  group_by(Dependents) %>% 
  summarize(mean_LoanAmount = mean(LoanAmount, na.rm = TRUE)) %>% 
  ggplot(aes(x = Dependents, y = mean_LoanAmount)) + 
    geom_bar(stat = 'identity') # Missing Dependent are likely 0 since their average loan amount is low

# Dependents missing values
dat %>% 
  group_by(Credit_History) %>% 
  summarize(mean_LoanAmount = mean(LoanAmount, na.rm = TRUE)) %>% 
  ggplot(aes(x = Credit_History, y = mean_LoanAmount)) + 
  geom_bar(stat = 'identity') # Missing Dependent are likely 0 since their average loan amount is low

# Check Credit History vs Loan Status
train %>% 
  ggplot() +
    geom_mosaic(aes(x = product(Credit_History, Loan_Status), fill = Credit_History), na.rm = TRUE)

dat_no_missings <- dat %>% 
  # Replace missing Loan Amount with median
  mutate(LoanAmount = ifelse(is.na(LoanAmount), median(LoanAmount, na.rm = TRUE), LoanAmount)) %>% 
  # Replace missing Loan Amount Term according to Loan Amount
  mutate(Loan_Amount_Term = ifelse((is.na(Loan_Amount_Term) & LoanAmount >= 144), 360,
                                   ifelse((is.na(Loan_Amount_Term) & LoanAmount < 144), 180,
                                          Loan_Amount_Term)
                                   )
         ) %>% 
  # Replacing missing Gender with Male
  mutate(Gender = ifelse(is.na(Gender), 'Male', Gender)) %>% 
  # Replace missing Married with Yes
  mutate(Married = ifelse(is.na(Married), 'Yes', Married)) %>% 
  # Replace missing Dependents with 0
  mutate(Dependents = ifelse(is.na(Dependents), '0', Dependents)) %>% 
  # Replace missing Self employed with mode
  mutate(Self_Employed = ifelse(is.na(Self_Employed), getmode(Self_Employed), Self_Employed)) %>% 
  # Replace missing Credit History with mode
  mutate(Credit_History = ifelse(is.na(Credit_History), getmode(Credit_History), Credit_History))

# Check for missing values
colSums(is.na(dat))



# -----------------------------------------------------------------------------------
# Feature Engineering
# -----------------------------------------------------------------------------------

dat_trans <- dat_no_missings %>% 
  # Combine Incomes
  mutate(CombinedIncome = ApplicantIncome + CoapplicantIncome) %>% 
  # Ratio of Incomes
  mutate(Income_Ratio = (CoapplicantIncome + 1)/(ApplicantIncome+ 1)) %>% 
  # Has Coapplicant
  mutate(Coapplicant = as.factor(ifelse(CoapplicantIncome == 0, 1, 0))) %>% 
  # Loan Amount Term in Years
  mutate(Loan_Amount_Term_Year = Loan_Amount_Term/12) %>% 
  # Loan Amount per Year
  mutate(Loan_Amount_Year = LoanAmount/Loan_Amount_Term_Year) %>% 
  # Loan Amount per Month
  mutate(Loan_Amount_Month = LoanAmount/Loan_Amount_Term) %>% 
  # Income to Loan Amount per Year
  mutate(Income_Loan_Amount_Year = CombinedIncome/Loan_Amount_Year) %>% 
  # Income to Loan Amount per Month
  mutate(Income_Loan_Amount_Month = CombinedIncome/Loan_Amount_Month) %>% 
  # Married with Kids
  mutate(Married_and_Dependents = as.factor(ifelse(Married == 'Yes' & Dependents != '0', 1, 0))) %>% 
  # Change Dependents to Numeric
  mutate(Dependents = as.numeric(recode(Dependents, '3+' = '3'))) %>%
  # Family Size
  mutate(Family_Size = ifelse(Married == "Y", Dependents + 2, Dependents + 1)) %>% 
  # Income per Family Member
  mutate(Income_per_Family = CombinedIncome/Family_Size) %>% 
  select(-Loan_Amount_Term)

# Check Distributions of new variables
# Plot Numeric Variables
dat_trans %>% 
  select_if(is.numeric) %>% 
  gather(key = 'var', value = 'value') %>% 
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap( ~ var, scales = 'free')

dat_trans <- dat_trans %>% 
  mutate(Loan_Amount_Term_Year = ifelse(Loan_Amount_Term_Year < 20, 'Less than 20',
                                        ifelse(Loan_Amount_Term_Year >= 20 & Loan_Amount_Term_Year < 30, '20 to 30',
                                               '30+')
                                        )
         )
  

# Loan_Amount_Term_Year probably needs to be binned.
# The rest need to be log or sqrt 

# Resplit Data
train_trans <- dat_trans %>% filter(train_test == 'train') %>% select(-train_test)
test_trans <- dat_trans %>% filter(train_test == 'test') %>% select(-train_test)

rec_obj <- recipe(Loan_Status ~ ., data = train_trans) %>%
  # Sqrt Skewed Numeric Variables
  step_sqrt(all_numeric(), -Dependents, -Family_Size) %>% 
  # One Hot Encoding
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  # Normalize
  step_center(all_predictors(), -all_outcomes()) %>% 
  step_scale(all_predictors(), -all_outcomes()) %>% 
  prep(data = train_trans)

X_train <- bake(rec_obj, newdata = train_trans)
X_test <- bake(rec_obj, newdata = test_trans)

Y_train <- X_train$Loan_Status
X_train$Loan_Status <- NULL
X_test$Loan_Status <- NULL



# -----------------------------------------------------------------------------------
# Single Models
# -----------------------------------------------------------------------------------

# Cross Validation Control
cvControl <- trainControl(
  method = 'cv', #cross validation
  number = 5, #5-fold,
  savePredictions = 'final',
  classProbs = TRUE,
#  index = createMultiFolds(Y_test, k = 5, times = 1),
  verbose = TRUE
)


# K-Nearest Neighbors Tune Grid
knn_tune <- caretModelSpec(method = 'knn',
                           tuneGrid = expand.grid(k = 1:20)
)


# C5.0 Tune Grid
C50_tune <- caretModelSpec(method = 'C5.0',
                           tuneGrid = expand.grid(trials = seq(1, 100, length = 10),
                                                  model = c('tree', 'rules'),
                                                  winnow = c(TRUE, FALSE)
                           )
)

# Random Forest Tune Grid
rf_tune <- caretModelSpec(method = 'rf',
                          tuneGrid = expand.grid(mtry = 1:ncol(X_train))
)

# SVM Radial Grid
svm_radial_tune <- caretModelSpec(method = 'svmRadialWeights',
                                  tuneGrid = expand.grid(sigma = seq(11, 12, length = 10),
                                                         C = seq(0.5, 2.5, length = 10),
                                                         Weight = 1:10)
)

# Gradient Boosting Tune Grid
gbm_tune <- caretModelSpec(method = 'gbm',
                           tuneGrid = expand.grid(n.trees = seq(100, 1000, length = 10),
                                                  interaction.depth = c(1, 2),
                                                  shrinkage = seq(0.001, 0.1, length = 10),
                                                  n.minobsinnode = seq(5, 50, length = 10)
                           )
)


# Models to Tune
tuneList <- list(knn_tune, C50_tune, rf_tune, gbm_tune)

# Fit Models
model_list <- caretList(
  x = X_train,
  y = Y_train,
  trControl = cvControl,
  metric = 'Accuracy',
  tuneList = tuneList
)

# Model Accuracies
max(model_list$knn$results$Accuracy)
max(model_list$C5.0$results$Accuracy)
max(model_list$rf$results$Accuracy)
max(model_list$gbm$results$Accuracy)

# Predictions with gbm
pred_gbm <- predict(model_list$gbm, X_test)

submission_gbm <- data_frame(Loan_ID = test_ids, Loan_Status = pred_gbm)
write.csv(submission_gbm, 'Loan_Prediction_gbm_submission.csv', row.names = FALSE)