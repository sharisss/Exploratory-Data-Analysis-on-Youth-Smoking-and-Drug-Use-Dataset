
data = read.csv("C:/Users/gabor/OneDrive/Desktop/youth_smoking_drug_data_10000_rows_expanded.csv")
head(data)
# Check data types
str(data)
# Check for duplicates
duplicate_rows <- data[duplicated(data), ]
n_duplicates <- nrow(duplicate_rows)
if (n_duplicates > 0) {
  print(paste("Number of duplicate rows:", n_duplicates))
  print(duplicate_rows)
} else {
  print("No duplicate rows found")
}

# Count missing values in each column
missing_values <- colSums(is.na(data))
print(missing_values)

# Replace missing values with mean for numeric columns
numeric_cols <- sapply(data, is.numeric)
data[numeric_cols] <- lapply(data[numeric_cols], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Verify the result
summary(data)

#DATA VISUALIZATION
# Histogram of Smoking_Prevalence
#This can reveal if smoking prevalence is concentrated around specific levels or spread evenly across different values.
library(ggplot2)

ggplot(data, aes(x = Smoking_Prevalence)) +
  geom_histogram(binwidth = 3, fill = "purple", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Smoking Prevalence", x = "Smoking Prevalence", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14)
  )

# Boxplot of Smoking_Prevalence by Age_Group
#This will help identify if younger age groups have lower smoking prevalence, or if any specific age group has particularly high or low values.
ggplot(data, aes(x = Age_Group, y = Smoking_Prevalence, fill = Age_Group)) +
  geom_boxplot() +
  labs(title = "Smoking Prevalence by Age Group", x = "Age Group", y = "Smoking Prevalence")
# Scatter plot of Peer_Influence vs Smoking_Prevalence
# A positive correlation might indicate that higher peer influence relates to increased smoking prevalence
ggplot(data, aes(x = Peer_Influence, y = Smoking_Prevalence)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "Peer Influence vs Smoking Prevalence", x = "Peer Influence", y = "Smoking Prevalence")


# Smoking Prevalence by Socioeconomic Status

#Objective: Examine if there's a relationship between socioeconomic status and smoking prevalence.
    #Visualization: Violin plot of Smoking_Prevalence by Socioeconomic_Status to show distribution and density.
ggplot(data, aes(x = Socioeconomic_Status, y = Smoking_Prevalence, fill = Socioeconomic_Status)) +
  geom_violin(trim = FALSE) +
  labs(title = "Smoking Prevalence by Socioeconomic Status", x = "Socioeconomic Status", y = "Smoking Prevalence") +
  theme_minimal()
#Drug Experimentation by Age Group

#Objective: Understand drug experimentation trends across different age groups.
#Visualization: Boxplot of Drug_Experimentation by Age_Group
ggplot(data, aes(x = Age_Group, y = Drug_Experimentation, fill = Age_Group)) +
  geom_boxplot() +
  labs(title = "Drug Experimentation by Age Group", x = "Age Group", y = "Drug Experimentation") +
  theme_minimal()
#Peer Influence Across Age Groups

#Objective: See if peer influence varies significantly across different age groups.
#Visualization: Bar plot of average Peer_Influence by Age_Group.
data %>%
  group_by(Age_Group) %>%
  summarise(Average_Peer_Influence = mean(Peer_Influence, na.rm = TRUE)) %>%
  ggplot(aes(x = Age_Group, y = Average_Peer_Influence, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Peer Influence by Age Group", x = "Age Group", y = "Average Peer Influence") +
  theme_minimal()

#Parental Supervision and Smoking Prevalence

#Objective: Analyze the correlation between Parental_Supervision and Smoking_Prevalence.
#Visualization: Scatter plot with trend line for Parental_Supervision vs. Smoking_Prevalence.
ggplot(data, aes(x = Parental_Supervision, y = Smoking_Prevalence)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Parental Supervision vs Smoking Prevalence", x = "Parental Supervision", y = "Smoking Prevalence") +
  theme_minimal()

#Media Influence and Mental Health

#Objective: Understand the relationship between Media_Influence and Mental_Health.
#Visualization: Scatter plot of Media_Influence vs. Mental_Health with jitter for better readability.
ggplot(data, aes(x = Media_Influence, y = Mental_Health)) +
  geom_jitter(color = "purple", alpha = 0.5) +
  labs(title = "Media Influence vs Mental Health", x = "Media Influence", y = "Mental Health") +
  theme_minimal()



#Family Background Impact on Smoking and Drug Experimentation

#Objective: Compare family background scores with Smoking_Prevalence and Drug_Experimentation.
#Visualization: Dual scatter plots to show Family_Background against Smoking_Prevalence and Drug_Experimentation.
ggplot(data, aes(x = Family_Background)) +
  geom_point(aes(y = Smoking_Prevalence), color = "blue", alpha = 0.5) +
  geom_point(aes(y = Drug_Experimentation), color = "green", alpha = 0.5) +
  labs(title = "Family Background Impact on Smoking and Drug Experimentation", x = "Family Background", y = "Prevalence or Experimentation") +
  theme_minimal()



# Summary statistics for Smoking_Prevalence
summary_stats <- data %>%
  summarise(
    Mean = mean(Smoking_Prevalence, na.rm = TRUE),
    Median = median(Smoking_Prevalence, na.rm = TRUE),
    SD = sd(Smoking_Prevalence, na.rm = TRUE)
  )
print(summary_stats)



