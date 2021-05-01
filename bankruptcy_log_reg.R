
bankruptcy<-read.csv("C:/Users/valay/OneDrive/Desktop/Ivey/Winter Term 2021/Big Data Analytics/Class 9/Assignment/Bankruptcy Data - Haxo Corporation.csv")

head(bankruptcy)

#educ1 changes categorical variable to binary; if 12th or above then 1 or else 0

bankruptcy_glm <- glm (Bankruptcy~ +Net_Debt_EBITDA +Profit_Margin +Current_Ratio +Debt_to_Cash_Flow_From_Ops +EBITDA, data=bankruptcy, family=binomial(link="logit"))
summary(bankruptcy_glm)
#0.05 p-value

pred <- predict(bankruptcy_glm, bankruptcy, type = "response")
Output <- data.frame(actual = bankruptcy$Bankruptcy, predicted = pred)

classification <- ifelse(pred > 0.2, 1, 0)
#0.2 threshold based on confusion matrix specificity number

actual = bankruptcy$Bankruptcy
Output <- data.frame(actual, predprob=pred, classification)

install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)

confusionMatrix(as.factor(classification), as.factor(actual))
#Confusion matrix gives you your predictions vs. actual (reference)
#accuracy of your predictions (overall what % of your predictions did you get correct?)

#specificity is the % of 1s that you will predict correctly
#Correct Predicted actual 1s divided by all actual 1s

#1-Sensitivity gives you % of predictions for 1s which are wrong 
