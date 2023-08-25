
#initialising dataset
ct_data <- data.frame(matrix(nrow=20, ncol=9))

#simulating a drug trial
columns <- c("patient_id", "control_test", "age", "gender", "status", "week_1_dosage", "week_2_dosage", "week_3_dosage", "week_4_dosage" )
gender <- c("F","M")
#increasing a dosage value for the week by 0.23 times the previous week
dosage_increase = 0.23
colnames(ct_data) <- columns
set.seed(10)
ct_data$patient_id <- sample(x=1:20, size=nrow(ct_data))
ct_data$age<- sample (x=30:70, size=nrow(ct_data))
ct_data$gender <- sample(x=gender, size=nrow(ct_data), replace=TRUE,prob=c(0.5,0.5) )
ct_data$control_test <- sample(x=0:1, size=nrow(ct_data), replace=TRUE, prob= c(0.5,0.5))
#using a poisson distribution to simulate the status of the indiividual as 0 - completed, 1-death, 2-information not collected, 3- withdrawn
ct_data$status <- rpois(n=nrow(ct_data), lambda = 1.2)
ct_data$week_1_dosage <- rpois(n=nrow(ct_data), lambda = 4)
ct_data$week_2_dosage <- dosage_increase*ct_data$week_1_dosage + ct_data$week_1_dosage
ct_data$week_3_dosage <- dosage_increase*ct_data$week_2_dosage + ct_data$week_2_dosage
ct_data$week_4_dosage <- dosage_increase*ct_data$week_3_dosage + ct_data$week_3_dosage

#performing a logistic regression to understand the effect of drug assignment and age on the final status outcome 
log_reg <- glm(ct_data$status ~ ct_data$control_test + ct_data$age)

print(summary(log_reg))