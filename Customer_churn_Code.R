cat("\014") #Clears Console
rm(list=ls()) #Clears Global environment
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) #Clears Plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #Clears Packages
options(scipen=100) #Disables scientific notation for entire R Session

#Names : Harsha Katreddy & Sruthi Kondra
#Group 11
#Class : ALY6015
#Date : February 14th, 2024

# Load necessary libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(purrr)
library(printr)
library(pROC) 
library(ROCR) 
library(caret)
library(car)
library(rpart)
library(rpart.plot)

# Load the dataset
data <- read.csv("Churn_Modelling.csv")

# Inspect the first few rows of the dataset
head(data)

summary(data)

# Remove rows with NA values

data_clean <- na.omit(data) %>% filter(Geography != "")

data_clean <- as.data.frame(data_clean)

# Drop irrelevant columns that are unlikely to influence the outcome

data_clean <- dplyr::select(data_clean, -RowNumber,-CustomerId,-Surname)


# Check data types
str(data_clean)

data_clean$Geography <- as.factor(data_clean$Geography)
data_clean$Gender <- as.factor(data_clean$Gender)
data_clean$Exited <- as.factor(data_clean$Exited)

# Final check of the cleaned data
summary(data_clean)

################################################################################
#Exploratory Data Analysis
################################################################################
# Pie Chart for Gender Distribution
gender_distribution <- data_clean %>%
  count(Gender) %>%
  mutate(perc = n / sum(n) * 100)

ggplot(gender_distribution, aes(x = "", y = perc, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(perc, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Gender Distribution", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank())

# Stacked Bar Chart showing distribution of Exited within Gender
ggplot(data_clean, aes(x = Gender, fill = Exited)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution of Exited Status within Gender", x = "Gender", y = "Percentage") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Data preparation for Geography Pie Chart
data_clean$Geography <- factor(data_clean$Geography)
geography_data <- data_clean %>%
  group_by(Geography) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)


# Pie Chart for Geography using ggplot2
ggplot(geography_data, aes(x = "", y = Percentage, fill = Geography)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, title = "Geography Distribution", fill = "Geography") +
  theme_void() +
  theme(legend.title = element_blank())

# Stacked Bar Chart showing distribution of Exited or Not within Geography
ggplot(data_clean, aes(x = Geography, fill = factor(Exited))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Geography", y = "Percentage", fill = "Exited", title = "Exited Distribution within Geography") +
  theme_minimal()


# Boxplot for Age based on Exited status
ggplot(data, aes(x = as.factor(Exited), y = Age, fill = as.factor(Exited))) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Exited Status", x = "Exited", y = "Age") +
  scale_fill_discrete(name = "Exited", labels = c("No", "Yes")) +
  theme_minimal()

# Boxplot for CreditScore based on Exited status
ggplot(data, aes(x = as.factor(Exited), y = CreditScore, fill = as.factor(Exited))) +
  geom_boxplot() +
  labs(title = "Boxplot of CreditScore by Exited Status", x = "Exited", y = "CreditScore") +
  scale_fill_discrete(name = "Exited", labels = c("No", "Yes")) +
  theme_minimal()

# Boxplot for EstimatedSalary based on Exited status
ggplot(data, aes(x = as.factor(Exited), y = EstimatedSalary, fill = as.factor(Exited))) +
  geom_boxplot() +
  labs(title = "Boxplot of EstimatedSalary by Exited Status", x = "Exited", y = "EstimatedSalary") +
  scale_fill_discrete(name = "Exited", labels = c("No", "Yes")) +
  theme_minimal()

# Stacked Bar Chart of NumOfProducts based on Exited status
ggplot(data, aes(x = as.factor(NumOfProducts), fill = as.factor(Exited))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution of NumOfProducts by Exited Status",
       x = "Number of Products",
       y = "Percentage",
       fill = "Exited") +
  theme_minimal()

# Grouped Bar Chart for HasCrCard based on Exited status
data_clean %>%
  group_by(HasCrCard, Exited) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = factor(HasCrCard), y = Count, fill = factor(Exited))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Has Credit Card vs. Exited Status",
       x = "Has Credit Card", y = "Count", fill = "Exited Status") +
  theme_minimal()

# Grouped Bar Chart for IsActiveMember based on Exited status
data_clean %>%
  group_by(IsActiveMember, Exited) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = factor(IsActiveMember), y = Count, fill = factor(Exited))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Is Active Member vs. Exited Status",
       x = "Is Active Member", y = "Count", fill = "Exited Status") +
  theme_minimal()

# Grouped Bar Chart for Tenure based on Exited status
data_clean %>%
  group_by(Tenure, Exited) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = factor(Tenure), y = Count, fill = factor(Exited))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Tenure vs. Exited Status",
       x = "Tenure", y = "Count", fill = "Exited Status") +
  theme_minimal()

################################################################################
# Creating Categorical Variables

# Creating dummy variables
model_data <- model.matrix(~ Geography + Gender - 1, data=data_clean)

# Merging the new dummy variables with the original data
final_data <- cbind(data_clean, model_data)


final_data$Exited <- as.numeric(final_data$Exited)
final_data$Age = log(final_data$Age)
final_data$CreditScore = log(final_data$CreditScore)
final_data$Balance = log(final_data$Balance)

final_data$Balance[is.infinite(final_data$Balance)] <- 0


# scaling
scaling <- function(x) {(x - min(x)) / (max(x) - min(x))}

final_data$Age <- scaling(final_data$Age)
final_data$CreditScore <- scaling(final_data$CreditScore)
final_data$Balance <- scaling(final_data$Balance)
final_data$EstimatedSalary <- scaling(final_data$EstimatedSalary)


# Select only numerical variables for correlation analysis
numerical_data <- final_data[, sapply(final_data, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

# View the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         col = colorRampPalette(c("navy", "darkorange"))(200),  # Custom color range
         tl.cex = 0.6, tl.col = "black")

################################################################################

# Chi-Square Test for 'Gender' and 'Exited'
tableGenderExited <- table(final_data$Gender, final_data$Exited)
chisq_test_GenderExited <- chisq.test(tableGenderExited)
print(chisq_test_GenderExited)

# Chi-Square Test for 'HasCrCard' and 'Exited'
tableHasCrCardExited <- table(final_data$HasCrCard, final_data$Exited)
chisq_test_HasCrCardExited <- chisq.test(tableHasCrCardExited)
print(chisq_test_HasCrCardExited)

# Chi-Square Test for 'IsActiveMember' and 'Exited'
tableIsActiveMemberExited <- table(final_data$IsActiveMember, final_data$Exited)
chisq_test_IsActiveMemberExited <- chisq.test(tableIsActiveMemberExited)
print(chisq_test_IsActiveMemberExited)

# Chi-Square Test for 'NumOfProducts' and 'Exited'
tableNumOfProductsExited <- table(final_data$NumOfProducts, final_data$Exited)
chisq_test_NumOfProductsExited <- chisq.test(tableNumOfProductsExited)
print(chisq_test_NumOfProductsExited)

# Chi-Square Test for 'Geography' and 'Exited'
tableGeographyExited <- table(final_data$Geography, final_data$Exited)
chisq_test_GeographyExited <- chisq.test(tableGeographyExited)
print(chisq_test_GeographyExited)


################################################################################

# Creating Training and Test Datasets

set.seed(145) # For reproducibility
splitIndex <- createDataPartition(final_data$Exited, p = 0.7, list = FALSE)

trainSet <- data_clean[splitIndex, ]
testSet <- data_clean[-splitIndex, ]


# Fit a GLM model
glm_model <- glm(Exited ~ ., family = binomial, data = trainSet)

# Summary of the model
summary(glm_model)

#Feature Selection using Step Selection
library(MASS)

model_step <- stepAIC(glm_model, direction = "both")
summary(model_step)

#Checking for Multicollinearity
vif(model_step)


# Get probabilities - Training Dataset
probabilities.train <- predict(model_step, newdata = trainSet, type = "response")

# Convert probabilities to binary outcome
predicted_classes_train <- as.factor(ifelse(probabilities.train >= 0.5, "1", "0"))  # Assuming '1' for 'Exited' and '0' for 'Not Exited'

confusionMatrix(predicted_classes_train, trainSet$Exited, positive = "1")


#Confusion Matrix - Test Dataset
probabilities.test <- predict(model_step, newdata = testSet, type = "response")

# Convert probabilities to binary outcome
predicted_classes_test <- as.factor(ifelse(probabilities.test >= 0.5, "1", "0"))  # Assuming '1' for 'Exited' and '0' for 'Not Exited'

confusionMatrix(predicted_classes_test, testSet$Exited, positive = "1")


# Create ROC object
roc_obj <- roc(response = testSet$Exited, predictor = probabilities.test)

# Plot ROC curve
plot(roc_obj, main = "ROC Curve", col = "#1c61b6")

auc_value <- auc(roc_obj)
print(auc_value)

summary(model_step)

################################################################################
#Decision Tree - Classification Model

# Fit the Decision Tree model
Dtree_model <- rpart(Exited ~ ., data = trainSet, method = "class")

# Optimize the model with cross-validation
set.seed(145)
cv.ct <- rpart(Exited ~ ., data = trainSet, method = "class", cp = 0.00001, minsplit = 5, xval = 5)

# Prune the model based on cross-validation results
prune_dt <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), "CP"])

prp(prune_dt, type = 1, extra = 1, split.font = 1, varlen = -10)
# Predict probabilities on the test set
predict_dt_probs <- predict(prune_dt, testSet, type = "prob")

# Extract probabilities for the positive class ('1')
dt_positive_class_probs <- predict_dt_probs[, "1"]

# Confusion Matrix for actual predictions
# Convert probabilities to binary predictions for confusion matrix
predicted_classes <- ifelse(dt_positive_class_probs > 0.5, 1, 0)
cm_dt <- confusionMatrix(as.factor(predicted_classes), as.factor(testSet$Exited), positive = "1")
print(cm_dt)

# ROC curve and AUC calculation
library(pROC)
roc_obj_dt <- roc(testSet$Exited, dt_positive_class_probs)
plot(roc_obj_dt, main = "ROC Curve for Decision Tree")
auc_value_dt <- auc(roc_obj_dt)
print(auc_value_dt)

summary(prune_dt)
################################################################################

library(e1071)
library(caret)
library(pROC)

# Train the SVM model with probability estimation enabled
learn_svm <- svm(factor(Exited) ~ ., data = trainSet, kernel = "polynomial", probability = TRUE)

# Predict on the test data with probabilities
predict_svm <- predict(learn_svm, newdata = testSet, probability = TRUE)

# Extract predicted probabilities for the positive class
svm_probabilities <- attr(predict_svm, "probabilities")[, "1"]  # Assuming "1" is the label for the positive class

# Create the confusion matrix
cm_svm <- confusionMatrix(as.factor(ifelse(svm_probabilities > 0.25, 1, 0)), as.factor(testSet$Exited), positive = '1')
print(cm_svm)

# Calculate and plot the ROC curve
roc_obj_svm <- roc(response = testSet$Exited, predictor = svm_probabilities)
plot(roc_obj_svm, main = "ROC Curve", col = "#1c61b6")

# Calculate the AUC
auc_value_svm <- auc(roc_obj_svm)
print(auc_value_svm)



