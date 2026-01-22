#Packages & libraries
install.packages("caTools")
library(dplyr)
library(psych) #for descriptive statistics
library(ggcorrplot) #for correlation plot matrix
library(randomForest)  #To implement the Random Forest model
library(rpart)  #for model Decision tree
library(rpart.plot)  #for Visualize the decision tree
library(caret)  #finding importante variables in spliting the data
library(car) #To display VIF to check multicollinearity
library(pROC) #To plot ROC curve
library(pscl) #To display McFadden’s R² 
library(class)    # For KNN algorithm
library(caTools)  # For data splitting
library(GGally)
library(ggplot2)
library(tidyr)
library(DescTools)
library(kableExtra)
library(RColorBrewer)
library(reshape2)
library(vcd)
library(Rmisc)
library(plotly)
library(gridExtra)
library(ggridges)
library(descr)
library(ggpie)
library(plyr)
library(scales)

#read data
data <- read.csv("C:/yasmeen/Data Science/Project/HCV-EGY-DATA.csv")
data <- read.csv("HCV-EGY-DATA.csv")
str(data)

#Selecting our own sample, train data (70%) which will use it and test (30%)
set.seed(123)
set.seed(42)
nrow(data)
sample_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
train <- data[sample_index, ]
test <- data[-sample_index, ]
# Save the training and testing data to CSV files
write.csv(train, "train_data.csv", row.names = FALSE)
train <- read.csv("train_data.csv")
write.csv(test, "test_data.csv", row.names = FALSE)
test <- read.csv("test_data.csv")

#Check the number of observations and variables
num_observations <- nrow(train)
num_observations
num_variables <- ncol(train)
num_variables

#Create subset of the data set with only the selected variables
train_data <- subset(train, select = c(Age, Gender, BMI, Fever, Nausea.Vomting, Headache, Diarrhea,
                                   Fatigue...generalized.bone.ache, Jaundice, Epigastric.pain, WBC,
                                   RBC, HGB, Plat, AST.1, ALT.1, ALT.48, RNA.Base, RNA.EOT,
                                   Baselinehistological.staging, HCV))
test_data <- subset(test, select = c(Age, Gender, BMI, Fever, Nausea.Vomting, Headache, Diarrhea,
                                    Fatigue...generalized.bone.ache, Jaundice, Epigastric.pain, WBC,
                                    RBC, HGB, Plat, AST.1, ALT.1, ALT.48, RNA.Base, RNA.EOT,
                                    Baselinehistological.staging, HCV))
class(train_data)
str(train_data)
View(train_data)
View(test_data)
#Cleaning & preparing data
#Convert variable types to the correct types
train_data$Gender<- as.factor(train_data$Gender)
train_data$Fever<- as.factor(train_data$Fever)
train_data$Nausea.Vomting<- as.factor(train_data$Nausea.Vomting)
train_data$Headache<- as.factor(train_data$Headache)
train_data$Diarrhea<- as.factor(train_data$Diarrhea)
train_data$Fatigue...generalized.bone.ache<- as.factor(train_data$Fatigue...generalized.bone.ache)
train_data$Jaundice<- as.factor(train_data$Jaundice)
train_data$Epigastric.pain<- as.factor(train_data$Epigastric.pain)
train_data$Baselinehistological.staging<- as.factor(train_data$Baselinehistological.staging)
train_data$HCV<- as.factor(train_data$HCV)
str(train_data)

test_data$Gender<- as.factor(test_data$Gender)
test_data$Fever<- as.factor(test_data$Fever)
test_data$Nausea.Vomting<- as.factor(test_data$Nausea.Vomting)
test_data$Headache<- as.factor(test_data$Headache)
test_data$Diarrhea<- as.factor(test_data$Diarrhea)
test_data$Fatigue...generalized.bone.ache<- as.factor(test_data$Fatigue...generalized.bone.ache)
test_data$Jaundice<- as.factor(test_data$Jaundice)
test_data$Epigastric.pain<- as.factor(test_data$Epigastric.pain)
test_data$Baselinehistological.staging<- as.factor(test_data$Baselinehistological.staging)
test_data$HCV<- as.factor(test_data$HCV)

# Check for missing values
#check if we have any missing in the data
any(is.na(train_data)) #we don't have missing values

#check outliers by boxplot
#First i will make a data frame that contains only numeric variables
#and another data frame contains only categorical variables
numeric_variables<- train_data[sapply(train_data, is.integer)]
summary(numeric_variables)
categorical_variables<- train_data[sapply(train_data, is.factor)]

#we will create a for loop to create boxplots for each variable 
par(mfrow = c(3,4))  # 3 rows and 4 columns
colors <- c("steelblue4", "violetred1", "seagreen", "turquoise1", "mediumorchid4", "chocolate1", "darkred", 
            "gold", "tan4", "purple", "tomato")
i<- 1
for (var_name in names(numeric_variables)) {
  boxplot(numeric_variables[[var_name]], 
          main = paste("Boxplot of", var_name),
          col = colors[i])
  i<- i+1} #we notice that there is no outliers 
layout(1)

#check for duplicates & Remove if we have any.
duplicate<- sum(duplicated(train_data))
duplicate #there is no duplicate

#Descriptive measures for numerical variables
numerical_measures<- describe(numeric_variables)
sink("C:/yasmeen/Data Science/Project/numeric measures.txt", split = TRUE)
numerical_measures
sink()
variance_covariance_matrix<- var(numeric_variables)
sink("C:/yasmeen/Data Science/Project/Variance covariance matrix.txt", split = TRUE)
variance_covariance_matrix
sink()
correlation_matrix<- cor(numeric_variables)
sink("C:/yasmeen/Data Science/Project/correlation matrix.txt", split = TRUE)
correlation_matrix
sink()
#correlation plot matrix
ggcorrplot(correlation_matrix)

#########################(Tables & Plots)#######################################
#Bar plots for categorical train_data
par(mfrow = c(3,3)) #Reset plotting area 
barplot(table(train_data$Gender), main = "Gender", ylab = "Count", col = "chocolate")
barplot(table(train_data$Fever), main = "Fever", ylab = "Count", col = "brown")
barplot(table(train_data$Nausea.Vomting), main = "Nausea.Vomting", ylab = "Count", col = "burlywood4")
barplot(table(train_data$Headache), main = "Headache", ylab = "Count", col = "burlywood")
barplot(table(train_data$Diarrhea), main = "Diarrhea", ylab = "Count", col = "wheat")
barplot(table(train_data$Fatigue...generalized.bone.ache), main = " Fatigue...generalized.bone.ache", ylab = "Count", col = "cyan4")
barplot(table(train_data$Jaundice), main = "Jaundice", ylab = "Count", col = "skyblue4")
barplot(table(train_data$Epigastric.pain), main = "Epigastric.pain", ylab = "Count", col = "pink4")
barplot(table(train_data$Baselinehistological.staging), main = "Baselinehistological.staging", ylab = "Count", col = "red4")

#ggpairs for numerical variable
ggpairs(train_data[, c("Age", "BMI", "WBC", "RBC", "HGB",
                       "Plat", "AST.1", "ALT.1", "ALT.48", "RNA.Base", "RNA.EOT")],
        title = "Pair Plot of Numerical Variables",
        lower = list(continuous = wrap("points", alpha = 0.5)),
        upper = list(continuous = wrap("cor", size = 5, color = "darkgray")),
        diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

#Density Plot
ggplot(train_data, aes(x = RNA.Base, fill = Fever, color = Fever)) +
  geom_density(alpha = 0.3) +
  labs(title = "RNA.Base distribution by Fever",
       x = "Fever",
       y = "RNA.Base") +
  theme_minimal()

ggplot(train_data, aes(x = RNA.Base, fill = Headache, color = Headache)) +
  geom_density(alpha = 0.3) +
  labs(title = "RNA.Base distribution by Headache",
       x = "Headache",
       y = "RNA.Base") +
  theme_minimal()

ggplot(train_data, aes(x = RNA.Base, fill = Diarrhea, color = Diarrhea)) +
  geom_density(alpha = 0.3) +
  labs(title = "RNA.Base distribution by Diarrhea",
       x = "Diarrhea",
       y = "RNA.Base") +
  theme_minimal()

ggplot(train_data, aes(x = RNA.Base, fill = Jaundice, color = Jaundice)) +
  geom_density(alpha = 0.3) +
  labs(title = "RNA.Base distribution by Jaundice",
       x = "Jaundice",
       y = "RNA.Base") +
  theme_minimal()

## pie chart for categorical variables
f1 <- Freq(train_data$Gender)
p1 <- ggplot(f1, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Gender") +
  scale_fill_manual(values =c("1" = "wheat3", "2" = "ivory2")) 
print(p1)
f2 <- Freq(train_data$Fever)
p2 <- ggplot(f2, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Fever") +
  scale_fill_manual(values =  c("1" = "hotpink4", "2" = "hotpink2"))
print(p2)
f3 <- Freq(train_data$Nausea.Vomting)
p3 <- ggplot(f3, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Nausea.Vomting") +
  scale_fill_manual(values = c("1" = "palegreen4", "2" = "palegreen2")) 
print(p3)
f4 <- Freq(train_data$Headache)
p4 <- ggplot(f4, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Headache") +
  scale_fill_manual(values = c("1" = "sienna4",  "2" = "sandybrown")) 
print(p4)
f6 <- Freq(train_data$Diarrhea)
p6 <- ggplot(f6, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Diarrhea") +
  scale_fill_manual(values = c("1" = "sienna",  "2" = "wheat")) 
print(p6)
f7 <- Freq(train_data$Fatigue...generalized.bone.ache)
p7 <- ggplot(f7, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Fatigue...generalized.bone.ache") +
  scale_fill_manual(values = c("1" = "tan4",  "2" = "tan")) 
print(p7)
f8 <- Freq(train_data$Jaundice)
p8 <- ggplot(f8, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Jaundice") +
  scale_fill_manual(values = c("1" = "skyblue",  "2" = "skyblue4")) 
print(p8)
f8 <- Freq(train_data$Epigastric.pain)
p8 <- ggplot(f8, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Epigastric.pain") +
  scale_fill_manual(values = c("1" = "cyan",  "2" = "cyan4")) 
print(p8)
f9 <- Freq(train_data$Baselinehistological.staging)
p9 <- ggplot(f9, aes(x = "", y = freq, fill = level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Pie chart of Baselinehistological.staging") +
  scale_fill_manual(values = c("1" = "pink",  "2" = "pink2", "3" = "pink3", "4" = "pink4" )) 
print(p9)
grid.arrange(p1, p2, p3, p4, p6, p7, p8, p9,  nrow = 3, ncol = 3)

#Scatter plot
ggplot(train_data, aes(x = RNA.Base, y = RNA.EOT)) +
  geom_point(color= "tan4") +
  labs(title = "Scatter Plot of RNA.Base vs RNA.EOT",
       x = "RNA.Base",
       y = "RNA.EOT")

# Histogram for numeric variable
graph1 <- ggplot(train_data, aes(Age)) +
  geom_histogram(bins=12,fill = "pink", color = "deeppink2") +
  labs(title = "Histogram Age",
       x = "Age")
print(graph1)
graph2 <- ggplot(train_data, aes(BMI)) +
  geom_histogram(bins = 12, fill = "darkslategrey", color = "cyan4") +
  labs(title = "Histogram BMI", x = "BMI")
print(graph2)
graph3 <- ggplot(train_data, aes(WBC)) +
  geom_histogram(bins=12,fill = "salmon4", color = "tan") +
  labs(title = "Histogram WBC",
       x = "WBC")
print(graph3)
graph4 <- ggplot(train_data, aes(RBC)) +
  geom_histogram(bins=12,fill = "cyan4", color = "lightyellow2") +
  labs(title = "Histogram RBC",
       x = "RBC")
print(graph4)
graph5 <- ggplot(train_data, aes(HGB)) +
  geom_histogram(bins=12,fill = "deeppink2", color = "lightyellow2") +
  labs(title = "Histogram HGB",
       x = "HGB")
print(graph5)
graph6 <- ggplot(train_data, aes(Plat)) +
  geom_histogram(bins=12,fill = "brown4", color = "wheat") +
  labs(title = "Histogram Plat",
       x = "Plat")
print(graph6)
graph7 <- ggplot(train_data, aes(AST.1)) +
  geom_histogram(bins=12,fill = "wheat4", color = "wheat2") +
  labs(title = "Histogram AST.1",
       x = "AST.1")
print(graph7)
graph8 <- ggplot(train_data, aes(ALT.1)) +
  geom_histogram(bins=12,fill = "green4", color = "lightyellow2") +
  labs(title = "Histogram ALT.1",
       x = "ALT.1")
print(graph8)
graph9 <- ggplot(train_data, aes(ALT.48)) +
  geom_histogram(bins=12,fill = "yellow4", color = "lightyellow2") +
  labs(title = "Histogram ALT.48",
       x = "ALT.48")
print(graph9)
graph10 <- ggplot(train_data, aes(RNA.Base)) +
  geom_histogram(bins=12,fill = "deeppink4", color = "lightyellow2") +
  labs(title = "Histogram RNA.Base",
       x = "RNA.Base")
print(graph10)
graph11 <- ggplot(train_data, aes(RNA.EOT)) +
  geom_histogram(bins=12,fill = "black", color = "lightyellow2") +
  labs(title = "Histogram RNA.EOT",
       x = "RNA.EOT")
print(graph11)
grid.arrange(graph1, graph2, graph3, graph4, graph5, graph6, graph7, graph8,
             graph9,graph10,graph11, nrow = 4, ncol = 3)

#Frequency tables for gender, fever & Baselinehistological.staging.
f1 <- Freq(train_data$Gender)
print(f1)
f2 <- Freq(train_data$Fever)
print(f2)
f9 <- Freq(train_data$Baselinehistological.staging)
print(f9)

#Create two way table for HCV & Gender
# Create a two-way table for HCV and Gender
table <- table(train_data$HCV, train_data$Gender)
print(table)
chi_square <- chisq.test(table)
print(chi_square)

# Create a three-way contingency table for HCV with Gender & Epigastric.pain
HCV <- train_data$HCV
Epigastric.pain <-  train_data$Epigastric.pain
Gender <- train_data$Gender
three_way_table <- table(HCV, Epigastric.pain, Gender)
print(three_way_table)
three_way_table_margin <- addmargins(three_way_table)
three_way_table_margin
total <- sum(three_way_table)
total 
three_way_table_percentage <- (three_way_table_margin/total)*100
three_way_table_percentage <- format(three_way_table_percentage, digits=2, nsmall=2)
three_way_table_percentage
CHM_test <- mantelhaen.test(three_way_table)
CHM_test

#########################(Logistic regression model)############################
train_data$HCV <- ifelse(train_data$HCV == 1, 0, ifelse(train_data$HCV == 2, 1, train_data$HCV))
test_data$HCV <- ifelse(test_data$HCV == 1, 0, ifelse(test_data$HCV == 2, 1, test_data$HCV))

# Logistic Regression Model
logmodel <- glm(HCV ~ Age + Gender + BMI + Fever + Nausea.Vomting + Headache + Diarrhea +
                  Fatigue...generalized.bone.ache + Jaundice + Epigastric.pain + WBC +
                  RBC + HGB + Plat + AST.1 + ALT.1 + ALT.48 + RNA.Base + RNA.EOT +
                  Baselinehistological.staging,
                family = binomial(link = "logit"), data = train_data, control = glm.control(maxit = 100))

#model summary
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/Summary.txt")
summary(logmodel)

stepwise_model <- step(logmodel, direction = "both")
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/stepwise_model.txt")
summary(stepwise_model)

# exponentiation the estimates to get the odds ratios
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/exp.txt")
exp <- exp(stepwise_model$coefficients)
exp

# Check for multicollinearity using VIF
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/vif_stepwise.txt")
vif_stepwise <- vif(stepwise_model)
vif_stepwise

# ANOVA for the model
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/ANOVA.txt")
anova(stepwise_model, test = "Chisq")

# Now predict on the test data
predictedlogit_test <- predict(stepwise_model, test_data)

# Predicted probabilities for the test data
predictedprob_test <- plogis(predictedlogit_test)
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/Predicted probabilities.txt")
summary(predictedprob_test)

# Convert predicted probabilities into binary classes with cutoff = 0.5 for test data
predictclass_test <- ifelse(predictedprob_test > 0.5, 1, 0)

# Confusion matrix for test data
CM_test <- table(test_data$HCV, predictclass_test)
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/CM_test.txt")
print(CM_test)

# Confusion Matrix to test data
confusionMatrix <- confusionMatrix(factor(test_data$HCV), factor(predictclass_test))
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/confusionMatrix.txt")
print(confusionMatrix)

# Classification error metrics for test data
err_metric_test <- function(conf_matrix) {
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[2, 1])
  specificity <- conf_matrix[1, 1] / (conf_matrix[1, 1] + conf_matrix[1, 2])
  list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)}
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/err_metric_test.txt")
err_metric_test(CM_test)

#McFadden’s R2 for our model using the pR2 function
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/McFadden.txt")
pscl::pR2(stepwise_model)["McFadden"] 

#calculate VIF values for each predictor variable in our model 
sink("C:/Users/yi727/OneDrive/Desktop/4th LEVEL/DATA SCIENCE/vif.txt")
car::vif(stepwise_model) 

# ROC curve for test data
roc_score_test <- roc(test_data$HCV, predictedprob_test)
plot(roc_score_test, main = "ROC curve", col = "darkred")
auc_value_test <- auc(roc_score_test)
legend("bottomright", legend = paste("AUC =", round(auc_value_test, 2)), 
       col = "darkred", lwd = 2)


#######################Machine learning Algorithms##############################
#########################1.(Decision tree)######################################
#Display the model
model_tree <- rpart(train_data$HCV ~ ., data = train_data, method = "class")
model_tree

#Decision Tree Visualization
rpart.plot(
  model_tree,
  box.palette = list("darkolivegreen", "darkolivegreen1", "darksalmon", "darkred"),
  shadow.col = "gray",
  col = "white",
  main = "Decision Tree Visualization")

#the most important variable in splitting the data
importance <- varImp(model_tree)
importance %>%
  arrange(desc(Overall))

#predictions using the model
test_data$HCV <- as.factor(test_data$HCV)
prediction <- predict(model_tree, newdata = test_data, type = "class")
prediction

#confusion matrix
confusionMatrix(test_data$HCV, prediction)

#########################2.(KNN)################################################
#use KNN for numerical explanatory and categorical response
numeric_train <- train_data[sapply(train_data, is.integer)]
numeric_test <- test_data[sapply(test_data, is.integer)]
train_scaled <- scale(numeric_train)
test_scaled <- scale(numeric_test)

#choosing K
misClassError<-c()
for(i in 1:15){classifier_knn <- knn(train = train_scaled,
                                     test = test_scaled,
                                     cl = train_data$HCV,
                                     k = i);
misClassError[i] <- mean(classifier_knn != test_data$HCV)}
print(misClassError)
which.min(misClassError)

# K = 7
classifier_knn <- knn(train = train_scaled,
                      test = test_scaled,
                      cl = train_data$HCV,
                      k = 7)
misClassError <- mean(classifier_knn != test_data$HCV)
print(paste('Accuracy =', 1-misClassError))
#Confusion Matrix 
cm <- table(test_data$HCV, classifier_knn)
print(cm)

#########################(Create Function)######################################
#1- function to calculate the difference between RNA.Base & RNA. EOT
My_Function1 <- function (RNA.Base, RNA.EOT){RNA.Base - RNA.EOT}
diff <- My_Function1(train_data$RNA.Base,train_data$RNA.EOT)
print(diff)

#2- function to calculate the average for HCV categories "1
#: Good response to treatment (No virus) & 2: still have the virus"
My_Function2 <- function(data, value_column, category_column) {
  avg_cat1 <- mean(data[[value_column]][data[[category_column]] == 1], na.rm = TRUE)
  avg_cat2 <- mean(data[[value_column]][data[[category_column]] == 2], na.rm = TRUE)
  return(c(avg_cat1, avg_cat2))}
Avg_RNA.EOT <- My_Function2(train_data, "RNA.EOT", "HCV")
print(Avg_RNA.EOT)
