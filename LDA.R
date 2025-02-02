library(tidyverse)
library(MASS)
library(klaR)
library(MVN)
library(car)
library(caret)

set.seed(123)

alzheimers_data <- read.csv("C:/Users/Juan/Desktop/YR 4 𝓹𝓮𝓽𝓮𝓻 𝓳𝓸𝓱𝓷 𝓸𝓿𝓮𝓻𝓵𝓸𝓪𝓭/MULTIVAR/archive/alzheimers_disease_data.csv")

attach(alzheimers_data)

View(alzheimers_data)

alzheimers_data <- alzheimers_data %>% dplyr::select(-Gender, -Ethnicity, -EducationLevel,
                                                     -Smoking, -FamilyHistoryAlzheimers,
                                                     -CardiovascularDisease, -Depression,
                                                     -Diabetes, -HeadInjury, -Hypertension,
                                                     -MemoryComplaints, -BehavioralProblems,
                                                     -Confusion, -Disorientation,
                                                     -PersonalityChanges, -DifficultyCompletingTasks,
                                                     -Forgetfulness)

alzheimers_data <- alzheimers_data %>% dplyr::select(-DoctorInCharge, -PatientID)

training_sample <- sample(c(TRUE, FALSE), nrow(alzheimers_data), replace = T, prob = c(0.8,0.2))
train <- alzheimers_data[training_sample, ]
test <- alzheimers_data[!training_sample, ]

model1 <- lda(Diagnosis ~ ., train)
model1

plot(model1, col = as.integer(train$Diagnosis))

png("PartitionLDA.png", width = 4800, height = 4800)
partimat(as.factor(Diagnosis) ~ Age + BMI + AlcoholConsumption + PhysicalActivity + DietQuality + SleepQuality + SystolicBP 
         + DiastolicBP + CholesterolTotal + CholesterolLDL + CholesterolHDL + 
           CholesterolTriglycerides + MMSE + FunctionalAssessment + ADL, data=train, method = "lda")
dev.off()

lda.train <- predict(model1)
train$lda <- lda.train$class
table(train$lda,train$Diagnosis)

lda.test <- predict(model1,test)
test$lda <- lda.test$class
table(test$lda,test$Diagnosis)

#LDA kcross

k_fold_cv_existing_model_lda <- function(alzheimers_data, model1, k = 10)
  
{
  folds <- createFolds(alzheimers_data$Diagnosis, k = k, list = TRUE)
  
  accuracies <- numeric(k)
  
  for (i in 1:k) {
    test_data <- alzheimers_data[folds[[i]], ]
    train_data <- alzheimers_data[-folds[[i]], ]
    
    predictions <- predict(model1, newdata = test_data)$class
    
    accuracies[i] <- sum(predictions == test_data$Diagnosis) / nrow(test_data)
    
    cat("Fold", i, "- Accuracy:", accuracies[i], "\n")
  }
  
  return(mean(accuracies))
}

avg_accuracy_lda <- k_fold_cv_existing_model_lda(alzheimers_data, model1, k = 10)

lda_cv_results <- data.frame(Fold = 1:10, Accuracy = c(0.8047,0.7767,0.7628,0.7757,
                                                       0.8186,0.7814,0.7349,0.7814,0.7395,0.8280))

lda_cv_results

ggplot(lda_cv_results, aes(x=Fold, y =Accuracy)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "blue") +
  labs(x = "Fold Number", y = "Accuracy") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  theme_minimal()

#Average Accuracy from 10-fold CV using existing QDA model
print(paste("Average Accuracy from 10-fold CV using existing LDA model: ", 
            mean(c(0.8047,0.7767,0.7628,0.7757,
                   0.8186,0.7814,0.7349,0.7814,0.7395,0.8280))))
