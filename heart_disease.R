library(ggplot2)
library(cowplot)

#load dataset...

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
data

# Rename column name

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain  1 = typical angina,2 = atypical angina,3 = non-anginal pain,4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results, 1 = normal,2 = having ST-T wave abnormality,3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment,1 = upsloping,2 = flat,3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan,3 = normal (no cold spots),6 = fixed defect (cold spots during rest and exercise),7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

#checking for null values

str(data)
sum(is.na(data))
sum(data == "?", na.rm = TRUE)

data[data == "?"] <- NA
print(data)

#adding factors to variables

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd)


#handling missing values remove all missing values that replaced with "NA"

str(data)
data[is.na(data$ca) | is.na(data$thal),]
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)

#checking dependent variable(heart disease) related with each variables with each and every boolean and categorical values, 

xtabs(~hd+sex, data=data)
xtabs(~hd+cp, data=data)
xtabs(~hd+fbs, data=data)
xtabs(~hd+restecg, data=data)
xtabs(~hd+exang, data=data)
xtabs(~hd+slope, data=data)
xtabs(~hd+ca, data=data)
xtabs(~hd+thal, data=data)
xtabs(~hd+sex, data=data)

#split the dataset into traindata and test data

library(caTools)

# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% testing
split <- sample.split(data$hd, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Check the number of observations
nrow(train_data)
nrow(test_data)


# implementing logistic rigression model

##case 1 : try to predict "hd" using gender

### glm() function -> generalized linear models ->logistic regression,linearmodels & other methods


logistic <- glm(hd ~ sex, data = train_data, family = "binomial")
summary(logistic)

# fro glm function we get,
## (Intercept):Represents the log-odds of hd = 1 (positive outcome) when sex is at its reference level (here, likely "F" or female).
##Value: −1.0438
###−1.0438, meaning the log-odds of hd = 1 for the reference level of sex are negative.

##sexM:Represents the change in log-odds of hd = 1 when sex is "M" (male) compared to the reference group ("F").
##Value: 1.2737
###1.2737, indicating that being male increases the log-odds of hd = 1.
###Both coefficients have very small p-values (<0.001), meaning they are statistically significant.


#logistic function; hd = intercept + sexM + .....

# low residual deviance and low AIC tells more effective model

logistic <- glm(hd ~., data = train_data, family = "binomial")
summary(logistic)


# Make prediction with test_data

predicted_probs <- predict(logistic, newdata = test_data, type = "response")

# Convert probabilities to binary classes (Threshold = 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, "Unhealthy", "Healthy")
predicted_classes <- as.factor(predicted_classes)


library(caret)

# Evaluate model accuracy using confusion matrix
conf_matrix <- confusionMatrix(predicted_classes, test_data$hd)
print(conf_matrix)

# Extract and print accuracy
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Model Accuracy:", round(accuracy, 4)))

test_logistic <- glm(hd ~ ., data = test_data, family = "binomial")

# calculating McFadden's Pseudo R^2 

#log-likelihood of null model
ll.nul <- test_logistic$null.deviance/-2
#log-likelihood of logistic model
ll.proposed <- test_logistic$deviance/-2

#Pseudo R^2 
(ll.nul-ll.proposed)/ll.nul
#Pseudo R^2 = 0.55

# Create visualization of predicted probabilities
predicted_test_data <- data.frame(
  probability.of.hd = predicted_probs,
  hd = test_data$hd
)

predicted_test_data <- predicted_test_data[order(predicted_test_data$probability.of.hd, decreasing = FALSE),]
predicted_test_data$rank <- 1:nrow(predicted_test_data)


ggplot(data = predicted_test_data, aes(x = rank, y = probability.of.hd)) +
  geom_point(aes(color = hd), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted Probability of Heart Disease") +
  ggtitle("Predicted Probabilities for Test Data")


ggsave("heart_disease_test_probabilities.pdf")

