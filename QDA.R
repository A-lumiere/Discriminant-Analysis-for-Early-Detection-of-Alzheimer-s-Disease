library(tidyverse)
library(MASS)
library(klaR)
library(MVN)
library(car)
library(caret)

set.seed(123)

alzheimers_data <- read.csv("C:/Users/Juan/Desktop/YR 4 𝓹𝓮𝓽𝓮𝓻 𝓳𝓸𝓱𝓷 𝓸𝓿𝓮𝓻𝓵𝓸𝓪𝓭/MULTIVAR/archive/alzheimers_disease_data.csv")

alzheimers_data <- alzheimers_data %>% dplyr::select(-Gender, -Ethnicity, -EducationLevel,
                                                     -Smoking, -FamilyHistoryAlzheimers,
                                                     -CardiovascularDisease, -Depression,
                                                     -Diabetes, -HeadInjury, -Hypertension,
                                                     -MemoryComplaints, -BehavioralProblems,
                                                     -Confusion, -Disorientation,
                                                     -PersonalityChanges, -DifficultyCompletingTasks,
                                                     -Forgetfulness)

alzheimers_data <- alzheimers_data %>% dplyr::select(-DoctorInCharge,-PatientID)

training_sample <- sample(c(TRUE, FALSE), nrow(alzheimers_data), replace = T, prob = c(0.8,0.2))
train <- alzheimers_data[training_sample, ]
test <- alzheimers_data[!training_sample, ]

qda_model <- qda(Diagnosis ~ ., train)
qda_model

png("PartitionQDA.png", width = 4800, height = 4800)
partimat(as.factor(Diagnosis) ~ Age + BMI + AlcoholConsumption + PhysicalActivity + DietQuality + SleepQuality + SystolicBP 
         + DiastolicBP + CholesterolTotal + CholesterolLDL + CholesterolHDL + 
           CholesterolTriglycerides + MMSE + FunctionalAssessment + ADL, data=train, method = "qda")
dev.off()

qda.train <- predict(qda_model)
train$qda <- qda.train$class
table(train$qda,train$Diagnosis)

qda.test <- predict(qda_model, test)
test$qda <- qda.test$class
table(test$qda,test$Diagnosis)

k_fold_cv_existing_model_qda <- function(alzheimers_data, qda_model, k = 10)
  
{
  folds <- createFolds(alzheimers_data$Diagnosis, k = k, list = TRUE)
  
  accuracies <- numeric(k)
  
  for (i in 1:k) {
    test_data <- alzheimers_data[folds[[i]], ]
    train_data <- alzheimers_data[-folds[[i]], ]
    
    predictions <- predict(qda_model, newdata = test_data)$class
    
    accuracies[i] <- sum(predictions == test_data$Diagnosis) / nrow(test_data)
    
    cat("Fold", i, "- Accuracy:", accuracies[i], "\n")
  }
  
  return(mean(accuracies))
}

avg_accuracy_qda <- k_fold_cv_existing_model_qda(alzheimers_data, qda_model, k = 10)
print(paste("Average Accuracy from 10-fold CV using existing QDA model: ", avg_accuracy_qda))

qda_cv_results <- data.frame(Fold = 1:10, Accuracy = c(0.76744,0.8279,0.8047,0.8186,
                                                       0.8186,0.7349,0.8093,0.8131,0.8047,0.8093))

qda_cv_results

ggplot(qda_cv_results, aes(x=Fold, y =Accuracy)) +
  geom_line(color = "red") +
  geom_point(size = 3, color = "red") +
  labs(x = "Fold Number", y = "Accuracy") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  theme_minimal()

#Average Accuracy from 10-fold CV using existing QDA model
print(paste("Average Accuracy from 10-fold CV using existing QDA model: ", 
            mean(c(0.76744,0.8279,0.8047,0.8186,0.8186,0.7349,0.8093,0.8131,0.8047,0.8093))))

