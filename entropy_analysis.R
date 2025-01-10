library(entropy)

# Create a frequency table for the letters in "OXYMORON"
letters <- c('O', 'X', 'Y', 'M', 'O', 'R', 'O', 'N')
letter_freq <- table(letters)

# Convert the frequency table to a probability distribution
letter_prob <- prop.table(letter_freq)

# Calculate the entropy using the entropy() function from the entropy package
entropy_value <- entropy(letter_prob, unit = "log2")

# Print the entropy
entropy_value

    
# Define the letters in the word "OXYMORON"
letters <- c('O', 'X', 'Y', 'M', 'O', 'R', 'O', 'N')

# Create a table of frequencies for the entire set
letter_freq <- table(letters)

# Calculate the total entropy
letter_prob <- prop.table(letter_freq)
total_entropy <- entropy(letter_prob, unit = "log2")

# Split the letters into vowels and consonants
vowels <- c('O', 'O', 'O')  # Only O is a vowel in "OXYMORON"
consonants <- c('X', 'Y', 'M', 'R', 'N')

# Create frequency tables for vowels and consonants
vowel_freq <- table(vowels)
consonant_freq <- table(consonants)

# Calculate probabilities for vowels and consonants
vowel_prob <- prop.table(vowel_freq)
consonant_prob <- prop.table(consonant_freq)

# Calculate entropy for vowels and consonants
vowel_entropy <- entropy(vowel_prob, unit = "log2")
consonant_entropy <- entropy(consonant_prob, unit = "log2")

# Calculate the weighted entropy after the split
total_count <- length(letters)
weighted_entropy <- (length(vowels) / total_count) * vowel_entropy + 
                    (length(consonants) / total_count) * consonant_entropy

# Calculate information gain (reduction in entropy)
information_gain <- total_entropy - weighted_entropy

# Print the information gain
information_gain

    
# Number of possible outcomes (8 distinct Scrabble pieces)
n <- 8

# Maximum entropy formula for equally probable outcomes
max_entropy <- log2(n)

# Print the maximum entropy
max_entropy   
    
    


#| echo: false
c1 <- c(1,2,3,4,5,6)
c2 <- c("false", "false", "false", "true", "true", "true")
c3 <- c("true", "false", "true", "false", "false", "false")
c4 <- c("false", "false", "false", "false", "true", "false")
c5 <- c("true", "false", "true", "false", "true", "false")
df1 <- data.frame(c1, c2, c3, c4, c5)


knitr::kable(df1, caption = 'Prisoners on parole.', col.names = c('ID', 'Good Behavior', 'Age Below 30', 'Drug Dependent', 'Recidivist'), align = "lrrrr")



# Load the rpart library
library(rpart.plot)

# Rename columns for easier reference
colnames(df1) <- c('ID', 'Good_Behavior', 'Age_Below_30', 'Drug_Dependent', 'Recidivist')

# Build the decision tree with adjusted parameters
tree_model <- rpart(Recidivist ~ Good_Behavior + Age_Below_30 + Drug_Dependent, 
                    data = df1, method = "class", 
                    control = rpart.control(cp = 0, minsplit = 1))

# Check the decision tree structure
print(tree_model)

# Plot the decision tree with rpart.plot
rpart.plot(
  tree_model, 
  main = "Decision Tree for Recidivist Prediction", 
  cex = 0.8, 
  type=5, 
)


dev.print(png, filename = "decision_tree_plot.png", width = 800, height = 600)




#| echo: false
c1 <- c(1,2,3,4,5,6,7,8)
c2 <- c(39, 50, 18, 28, 37, 24, 52, 40)
c3 <- c('bachelors', 'bachelors', 'high school', 'bachelors', 'high school', 'high school', 'high school', 'doctorate')
c4 <- c('never married', 'married', 'never married', 'married', 'married', 'never married', 'divorced', 'married')
c5 <- c('transport', 'professional', 'agriculture', 'professional', 'agriculture', 'armed forces', 'transport', 'professional')
c6 <- c('25K-50K', '25K-50K', 'below 25K', '25K-50K', '25K-50K', 'below 25K', '25K-50K', 'over 50K')
df2 <- data.frame(c1,c2,c3,c4,c5,c6)



knitr::kable(df2, caption = 'Census data.', col.names = c('ID', 'Age', 'Education', 'Marital Status', 'Occupation', 'Annual Income'), align = "lrrrrr")



# Install and load the necessary package
library(entropy)

# Extract the target variable (Annual Income) from your dataframe
annual_income <- df2$c6

# Calculate the frequency of each income level
income_freq <- table(annual_income)

# Calculate entropy
income_entropy <- entropy(income_freq, unit = "log2")

# Print the entropy
income_entropy
  


# Extract the target variable (Annual Income) from your dataframe
annual_income <- df2$c6

# Calculate the frequency of each income level
income_freq <- table(annual_income)

# Convert frequencies to proportions
income_prob <- prop.table(income_freq)

# Calculate Gini index: Gini = 1 - sum(p_i^2)
gini_index <- 1 - sum(income_prob^2)

# Print the Gini index
gini_index



# Extract the target variable and the Age feature from the dataframe
annual_income <- df2$c6
age <- df2$c2

# Calculate the total entropy of the target variable (Annual Income)
income_freq <- table(annual_income)
total_entropy <- entropy(income_freq, unit = "log2")

# Function to calculate the weighted entropy after splitting at a given threshold
calculate_weighted_entropy <- function(threshold) {
  # Split the data based on the threshold
  left_split <- annual_income[age <= threshold]
  right_split <- annual_income[age > threshold]
  
  # Calculate the frequency and entropy for the left split
  left_freq <- table(left_split)
  left_entropy <- if(length(left_split) > 0) entropy(left_freq, unit = "log2") else 0
  
  # Calculate the frequency and entropy for the right split
  right_freq <- table(right_split)
  right_entropy <- if(length(right_split) > 0) entropy(right_freq, unit = "log2") else 0
  
  # Calculate the weighted entropy
  total_count <- length(annual_income)
  weighted_entropy <- (length(left_split) / total_count) * left_entropy + 
                      (length(right_split) / total_count) * right_entropy
  
  return(weighted_entropy)
}

# Calculate information gain for all possible splits
thresholds <- sort(unique(age))
information_gains <- sapply(thresholds, function(threshold) total_entropy - calculate_weighted_entropy(threshold))

# Find the threshold that gives the maximum information gain
optimal_threshold <- thresholds[which.max(information_gains)]
max_information_gain <- max(information_gains)

# Print the optimal threshold and the information gain
optimal_threshold
max_information_gain




# Extract the target variable (Annual Income) and the categorical features
annual_income <- df2$c6
education <- df2$c3
marital_status <- df2$c4
occupation <- df2$c5

# Calculate the total entropy of the target variable (Annual Income)
income_freq <- table(annual_income)
total_entropy <- entropy(income_freq, unit = "log2")

# Function to calculate the weighted entropy for a given categorical feature
calculate_weighted_entropy_categorical <- function(feature) {
  # Get the unique categories in the feature
  unique_categories <- unique(feature)
  
  # Calculate the weighted entropy
  total_count <- length(feature)
  weighted_entropy <- 0
  
  for (category in unique_categories) {
    # Subset the target variable based on the current category
    category_split <- annual_income[feature == category]
    
    # Calculate the frequency and entropy for the category split
    category_freq <- table(category_split)
    category_entropy <- if(length(category_split) > 0) entropy(category_freq, unit = "log2") else 0
    
    # Calculate the weighted entropy for the category
    weighted_entropy <- weighted_entropy + (length(category_split) / total_count) * category_entropy
  }
  
  return(weighted_entropy)
}

# Calculate information gain for Education
education_weighted_entropy <- calculate_weighted_entropy_categorical(education)
information_gain_education <- total_entropy - education_weighted_entropy

# Calculate information gain for Marital Status
marital_status_weighted_entropy <- calculate_weighted_entropy_categorical(marital_status)
information_gain_marital_status <- total_entropy - marital_status_weighted_entropy

# Calculate information gain for Occupation
occupation_weighted_entropy <- calculate_weighted_entropy_categorical(occupation)
information_gain_occupation <- total_entropy - occupation_weighted_entropy

# Print the information gain for each feature
information_gain_education
information_gain_marital_status
information_gain_occupation


# Extract the target variable (Annual Income) and the categorical features
annual_income <- df2$c6
education <- df2$c3
marital_status <- df2$c4
occupation <- df2$c5

# Calculate the total entropy of the target variable (Annual Income)
income_freq <- table(annual_income)
total_entropy <- entropy(income_freq, unit = "log2")

# Function to calculate the weighted entropy for a given categorical feature
calculate_weighted_entropy_categorical <- function(feature) {
  # Get the unique categories in the feature
  unique_categories <- unique(feature)
  
  # Calculate the weighted entropy
  total_count <- length(feature)
  weighted_entropy <- 0
  
  for (category in unique_categories) {
    # Subset the target variable based on the current category
    category_split <- annual_income[feature == category]
    
    # Calculate the frequency and entropy for the category split
    category_freq <- table(category_split)
    category_entropy <- if(length(category_split) > 0) entropy(category_freq, unit = "log2") else 0
    
    # Calculate the weighted entropy for the category
    weighted_entropy <- weighted_entropy + (length(category_split) / total_count) * category_entropy
  }
  
  return(weighted_entropy)
}

# Function to calculate the intrinsic value (entropy of the feature itself)
calculate_intrinsic_value <- function(feature) {
  # Calculate the frequency of each category in the feature
  feature_freq <- table(feature)
  
  # Calculate the intrinsic entropy (split information)
  intrinsic_value <- entropy(feature_freq, unit = "log2")
  
  return(intrinsic_value)
}

# Calculate the information gain and intrinsic value for Education
education_weighted_entropy <- calculate_weighted_entropy_categorical(education)
information_gain_education <- total_entropy - education_weighted_entropy
intrinsic_value_education <- calculate_intrinsic_value(education)
gain_ratio_education <- information_gain_education / intrinsic_value_education

# Calculate the information gain and intrinsic value for Marital Status
marital_status_weighted_entropy <- calculate_weighted_entropy_categorical(marital_status)
information_gain_marital_status <- total_entropy - marital_status_weighted_entropy
intrinsic_value_marital_status <- calculate_intrinsic_value(marital_status)
gain_ratio_marital_status <- information_gain_marital_status / intrinsic_value_marital_status

# Calculate the information gain and intrinsic value for Occupation
occupation_weighted_entropy <- calculate_weighted_entropy_categorical(occupation)
information_gain_occupation <- total_entropy - occupation_weighted_entropy
intrinsic_value_occupation <- calculate_intrinsic_value(occupation)
gain_ratio_occupation <- information_gain_occupation / intrinsic_value_occupation

# Print the gain ratio for each feature
gain_ratio_education
gain_ratio_marital_status
gain_ratio_occupation



# Function to calculate the Gini index for a given frequency table
calculate_gini_index <- function(freq_table) {
  proportions <- prop.table(freq_table)
  gini <- 1 - sum(proportions^2)
  return(gini)
}

# Function to calculate the weighted Gini index for a given categorical feature
calculate_weighted_gini_categorical <- function(feature) {
  # Get the unique categories in the feature
  unique_categories <- unique(feature)
  
  # Calculate the weighted Gini index
  total_count <- length(feature)
  weighted_gini <- 0
  
  for (category in unique_categories) {
    # Subset the target variable based on the current category
    category_split <- annual_income[feature == category]
    
    # Calculate the Gini index for the category split
    category_freq <- table(category_split)
    category_gini <- calculate_gini_index(category_freq)
    
    # Calculate the weighted Gini index for the category
    weighted_gini <- weighted_gini + (length(category_split) / total_count) * category_gini
  }
  
  return(weighted_gini)
}

# Extract the target variable and the categorical features
annual_income <- df2$c6
education <- df2$c3
marital_status <- df2$c4
occupation <- df2$c5

# Calculate the total Gini index of the target variable (Annual Income)
income_freq <- table(annual_income)
total_gini <- calculate_gini_index(income_freq)

# Calculate the information gain using Gini for Education
education_weighted_gini <- calculate_weighted_gini_categorical(education)
information_gain_gini_education <- total_gini - education_weighted_gini

# Calculate the information gain using Gini for Marital Status
marital_status_weighted_gini <- calculate_weighted_gini_categorical(marital_status)
information_gain_gini_marital_status <- total_gini - marital_status_weighted_gini

# Calculate the information gain using Gini for Occupation
occupation_weighted_gini <- calculate_weighted_gini_categorical(occupation)
information_gain_gini_occupation <- total_gini - occupation_weighted_gini

# Print the information gain using Gini index for each feature
information_gain_gini_education
information_gain_gini_marital_status
information_gain_gini_occupation


    


c1 <- c(1,2,3,4,5)
c2 <- c('false', 'true', 'false', 'true', 'false')
c3 <- c('high', 'low', 'low', 'high', 'high')
c4 <- c('false', 'true', 'false', 'true', 'false')
df11 <- data.frame(c1,c2,c3,c4)
print(df11)

knitr::kable(df11, caption = 'Pruning set.', col.names = c('ID', 'Chest Pain', 'Blood Pressure', 'Heart Disease'), align = "lrrr")

#Rename columns for clarity
colnames(df11) <- c('ID', 'Chest_Pain', 'Blood_Pressure', 'Heart_Disease')

# Build the decision tree with adjusted parameters
tree_model <- rpart(Heart_Disease ~ Chest_Pain + Blood_Pressure, 
                    data = df11, method = "class", 
                    control = rpart.control(cp = 0, minsplit = 1))

# Visualize the original tree
rpart.plot(
  tree_model, 
  main = "Decision Tree for Heart Disease Prediction",
  type = 5
)



# Check the decision tree structure
print(tree_model)

rpart.plot(
  tree_model, 
  main = "Decision Tree for Heart Disease Prediction",
  type = 5
)

# Plot the decision tree with rpart.plot
dev.print(png, filename = "decision_tree_plot2.png", width = 800, height = 600)

# Use the pruning set to predict heart disease based on the decision tree
predictions <- predict(tree_model, df11, type = "class")

# Compare predictions with actual values
pruning_results <- data.frame(
  ID = df11$ID,
  Actual_Heart_Disease = df11$Heart_Disease,
  Predicted_Heart_Disease = predictions
)

# Print the pruning results to see where errors occur
print(pruning_results)


# Print the tree complexity parameter (cp) table
printcp(tree_model)

# Prune the tree using the optimal cp value
optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree_model, cp = optimal_cp)

# Visualize the pruned tree
rpart.plot(
  pruned_tree, 
  main = "Decision Tree for Heart Disease Prediction",
  type = 5
)

# Predict again using the pruned tree and evaluate performance
pruned_predictions <- predict(pruned_tree, df11, type = "class")

# Compare predictions from the pruned tree with actual values
pruned_results <- data.frame(
  ID = df11$ID,
  Actual_Heart_Disease = df11$Heart_Disease,
  Predicted_Heart_Disease = pruned_predictions
)

# Print the pruned results to check for improvements
print(pruned_results)

ID <- c(1,2,3,4,5)
Exercise <- c('daily', 'weekly', 'daily', 'rarely', 'rarely')
Smoker <- c('false', 'true', 'false', 'true', 'true')
Obese <- c('false', 'false', 'false', 'true', 'true')
Family <- c('yes', 'yes', 'no', 'yes', 'no')
Risk <- c('low', 'high', 'low', 'high', 'high')
df3 <- data.frame(ID, Exercise, Smoker, Obese, Family, Risk)
print(df3)

knitr::kable(df3, caption = 'Heart disease study dataset.', align = "lrrrrr")

#| # Bootstrap Sample A
ID <- c(1,2,2,5,5)
Exercise <- c('daily', 'weekly', 'weekly', 'rarely', 'rarely')
Family <- c('yes', 'yes', 'yes', 'yes', 'no')
Risk <- c('low', 'high', 'high', 'high', 'high')
df4 <- data.frame(ID, Exercise, Family, Risk)
print(df4)

#| # Bootstrap Sample B
ID <- c(1,2,2,4,5)
Smoker <- c('false', 'true', 'true', 'true', 'true')
Obese <- c('false', 'false', 'false', 'true', 'true')
Risk <- c('low', 'high', 'high', 'high', 'high')
df5 <- data.frame(ID, Smoker, Obese, Risk)
print(df5)

#| # Bootstrap Sample C
ID <- c(1,1,2,4,5)
Obese <- c('false', 'false', 'false', 'true', 'true')
Family <- c('yes', 'yes', 'yes', 'yes', 'no')
Risk <- c('low', 'low', 'high', 'high', 'high')
df6 <- data.frame(ID, Obese, Family, Risk)
print(df6)

knitr::kable(df4, caption = 'Heart disease study dataset bootstrap sample A.', align = "lrrrr")

knitr::kable(df5, caption = 'Heart disease study dataset bootstrap sample B.', align = "lrrrr")

knitr::kable(df6, caption = 'Heart disease study dataset bootstrap sample C.', align = "lrrrr")

# Load the rpart library
library(rpart)
library(rpart.plot)

# Build the decision tree for bootstrap sample A
tree_A <- rpart(Risk ~ Exercise + Family, 
                data = df4, method = "class", 
                control = rpart.control(cp=0, minsplit=1))


# Visualize the tree for bootstrap A
rpart.plot(
  tree_A, 
  main = "Decision Tree for Bootstrap Sample A", 
  type = 5
)

# Build the decision tree for bootstrap sample B
tree_B <- rpart(Risk ~ Smoker + Obese, 
                data = df5, method = "class", 
                control = rpart.control(cp=0, minsplit=1))

# Visualize the tree for bootstrap B
rpart.plot(
  tree_B, 
  main = "Decision Tree for Bootstrap Sample B", 
  type =5
)

# Build the decision tree for bootstrap sample C
tree_C <- rpart(Risk ~ Obese + Family, 
                data = df6, method = "class",
                control = rpart.control(cp=0, minsplit=1))

# Visualize the tree for bootstrap C
rpart.plot(
  tree_C, 
  main = "Decision Tree for Bootstrap Sample C", 
  type=5
)

library(randomForest)

# Build the Random Forest model
set.seed(64)  # Set a seed for reproducibility
rf_model <- randomForest(as.factor(Risk) ~ Exercise + Smoker + Obese + Family, data = df3, ntree = 1000)

# Print the model to view the results
print(rf_model)


# Check the importance of features
importance(rf_model)

# Visualize feature importance
varImpPlot(rf_model)

# Predict using the same dataset (for demonstration purposes)
predictions <- predict(rf_model, df3)

# Compare the actual Risk values with the predicted values
comparison <- data.frame(Actual_Risk = df3$Risk, Predicted_Risk = predictions)

# Print the comparison
print(comparison)


# Define the new query as a data frame
new_query <- data.frame(
  Exercise = factor("rarely", levels = c("daily", "weekly", "rarely")),
  Smoker = factor("false", levels = c("false", "true")),
  Obese = factor("true", levels = c("false", "true")),
  Family = factor("yes", levels = c("no", "yes"))
)

# View the new query to confirm
print(new_query)


# Make predictions from each tree
prediction_A <- predict(tree_A, new_query, type = "class")
prediction_B <- predict(tree_B, new_query, type = "class")
prediction_C <- predict(tree_C, new_query, type = "class")

# Print predictions from each tree
print(paste("Tree A Prediction:", prediction_A))
print(paste("Tree B Prediction:", prediction_B))
print(paste("Tree C Prediction:", prediction_C))

# Collect the predictions
predictions <- c(as.character(prediction_A), as.character(prediction_B), as.character(prediction_C))

# Perform majority voting
final_prediction <- names(sort(table(predictions), decreasing = TRUE))[1]

# Print the final prediction after majority voting
print(paste("Final Predicted Risk (Majority Voting):", final_prediction))

