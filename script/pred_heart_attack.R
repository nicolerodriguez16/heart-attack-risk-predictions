rm(list = ls()) 
heart_attack=read.csv(file.choose(),header=T, stringsAsFactors=TRUE) 
heart_attack <- heart_attack[, !(names(heart_attack) %in% c("Patient.ID", "Blood.Pressure", "Sedentary.Hours.Per.Day", "Country", "Continent", "Hemisphere"))]
View(heart_attack)

attach(heart_attack) 
heart_attack[,2] =as.factor  (heart_attack[,2]) 
heart_attack$Heart.Attack.Risk = as.factor(heart_attack$Heart.Attack.Risk)
summary(heart_attack[,2]) 

glm.fit1 = glm(Heart.Attack.Risk ~ ., family = "binomial", data = heart_attack)
summary(glm.fit1)
exp(coef(glm.fit1))   

set.seed(1) 
#split data into 10 folds
#we randomly assign each fold to one index
k=10 
folds=sample(1:k,nrow(heart_attack),replace=TRUE) 


accuracy=rep(0,k) 
#run the loop is from one to k because it is what we chose for cross validation within the loop we first build a logistic regression model
#
for(i in 1:k) { 
  glm.fit2=glm(Heart.Attack.Risk ~ .,family="binomial" , data=heart_attack[folds!=i,])#we use 9 folds to train the data and we use one fold for resting later
  heart_attack.test=heart_attack[folds==i, ] #this is our testing dataset
  
  #after we apply the glm.fit2 to our testing data set and we make a predicition
  #we want to have a probability returned and we specify type as response or else it would return the lodgeit
  glm.probs2 = predict(glm.fit2, heart_attack.test, type = "response") 
  #first line of vectors that convert all the probabilities with either yes or no
  #if its above .5 it will be treated as yes
  glm.pred2 = rep("0", nrow(heart_attack.test)) 
  glm.pred2[glm.probs2 > 0.5] <- "1" 
  test.truevalue = heart_attack$Heart.Attack.Risk[folds == i]#true value of default
  
  #down below is the true value of logistic regresiion
  table(glm.pred2, test.truevalue) 
  accuracy[i] = mean(glm.pred2 == test.truevalue)   #calc accuracy by comparing if the predicted value is equal to true value
} 

mean(accuracy)
summary(glm.fit1)


#KNNN


# Load the class package
library(class)

#normalizing the quantative input variables to be in a compareable scale
standardized.Age=scale(Age) 
standardized.Cholesterol=scale(Cholesterol)
standardized.Heart.Rate=scale(Heart.Rate) 
standardized.Exercise.Hours.Per.Week=scale(Exercise.Hours.Per.Week)
standardized.BMI=scale(BMI) 
standardized.Income=scale(Income)
#new input variable #cbind includes normalized stuff
Input.standard=cbind(standardized.Age,standardized.Cholesterol,standardized.Heart.Rate, standardized.Exercise.Hours.Per.Week, standardized.BMI, standardized.Income, Sex, Diabetes, Family.History, Smoking, Obesity, Alcohol.Consumption, Diet, Previous.Heart.Problems, Medication.Use, Stress.Level, Triglycerides, Physical.Activity.Days.Per.Week, Sleep.Hours.Per.Day) 
#using 10-fold cross validation to select the best K from [1,10]
accuracy=matrix(0,10,5) 


set.seed(1) 
folds=sample(1:5,nrow(Input.standard),replace=TRUE) 
for (j in 1:10)   #index of k
{ 
  for(i in 1:5) #index for cross validation fold
  { 
    train.standard=Input.standard[folds!=i,] 
    test.standard=Input.standard[folds==i,] 
    train.truevalue=Heart.Attack.Risk[folds!=i] #y in the training set
    test.truevalue=Heart.Attack.Risk[folds==i]#y in the testing set 
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j) 
    accuracy[j,i]=mean(knn.pred==test.truevalue) }  #comparing the the predict value and true vvalue
}  


cv.accuracy=apply(accuracy,1,mean)


#Carol
library(ggplot2)

# Create subsets of the dataset for individuals with and without heart attack risk
heart_attack_subset1 <- subset(heart_attack, Heart.Attack.Risk == 1)
heart_attack_subset0 <- subset(heart_attack, Heart.Attack.Risk == 0)

# Create a density plot for cholesterol levels in individuals without heart attack risk
plot_0 <- geom_density(data = heart_attack_subset0, aes(x = Cholesterol, color = "No Heart Attack Risk"), alpha = 0.5)

# Create a density plot for cholesterol levels in individuals with heart attack risk
plot_1 <- geom_density(data = heart_attack_subset1, aes(x = Cholesterol, color = "Heart Attack Risk"), alpha = 0.5)

# Combine the density plots and customize the plot
gg <- ggplot() + plot_0 + plot_1 +
  labs(x = "Cholesterol", y = "Density", color = "Heart Attack Risk") +
  ggtitle("Distribution of Cholesterol Levels in Subsets with and without Heart Attack Risk") +
  theme_minimal()

# Display the plot
print(gg)

# Categorize ages into groups
heart_attack$Age_Group <- cut(heart_attack$Age, 
                              breaks = c(0, 30, 80, 100), 
                              labels = c("Under 30", "Between 30 and 80", "80 or Above"))

# Fit logistic regression model
logistic_model <- glm(Heart.Attack.Risk ~ Age_Group, family = binomial(link = "logit"), data = heart_attack)

# Calculate odds ratios
odds_ratios <- exp(coef(logistic_model))

# Create a bar plot for odds ratios
barplot(odds_ratios, 
        names.arg = levels(heart_attack$Age_Group), 
        main = "Odds Ratio of Heart Attack Risk by Age Group", 
        xlab = "Age Group", 
        ylab = "Odds Ratio", 
        col = "steelblue",
        ylim = c(0, max(odds_ratios) * 1.2))

# Load required library for Naive Bayes
library(e1071)

# Prepare the data
data <- heart_attack[, c("BMI", "Cholesterol", "Heart.Rate", "Age", "Heart.Attack.Risk")]

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(nrow(data), 0.7 * nrow(data)) # 70% of the data for training
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit Naive Bayes model
naive_bayes_model <- naiveBayes(Heart.Attack.Risk ~ BMI + Cholesterol + Heart.Rate + Age, data = train_data)

# Make predictions on test data
predictions <- predict(naive_bayes_model, newdata = test_data)

# Evaluate the model
accuracy <- mean(predictions == test_data$Heart.Attack.Risk)
print(paste("Accuracy:", accuracy))


#Nicole S

#What gender has higher chances to die from a heart attack? Male/Fem

heart_attack$Sex <- as.factor(heart_attack$Sex)

summary(heart_attack)

glm_model <- glm(Heart.Attack.Risk ~ Sex, family=binomial(), data=heart_attack)

summary(glm_model)

odds_ratios <- exp(coef(glm_model))
print(odds_ratios)

levels(heart_attack$Sex) <- c("Female", "Male")  

glm_model <- glm(Heart.Attack.Risk ~ Sex - 1, family=binomial(), data=heart_attack)

names(odds_ratios) <- names(coef(glm_model))

barplot(odds_ratios, names.arg = names(odds_ratios), main = "Heart Attack Risks for Males and Females",
        xlab = "Gender", ylab = "Odds Ratio", col = c("red", "blue"), beside = TRUE)

