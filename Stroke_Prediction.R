library(readr)
stroke_data <- read_csv("C:\\Users\\graci\\Desktop\\stroke_data.zip")
View(stroke_data)

summary(stroke_data)

summary(stroke_data$stroke)
stroke_data$gender<-as.factor(stroke_data$gender)
stroke_data$hypertension<-as.factor(stroke_data$hypertension)
stroke_data$heart_disease<-as.factor(stroke_data$heart_disease)
stroke_data$ever_married<-as.factor(stroke_data$ever_married)
stroke_data$work_type<-as.factor(stroke_data$work_type)
stroke_data$Residence_type<-as.factor(stroke_data$Residence_type)
stroke_data$bmi<-as.numeric(stroke_data$bmi,na.rm=TRUE)
stroke_data$smoking_status<-as.factor(stroke_data$smoking_status)


summary(stroke_data$gender)
summary(stroke_data$smoking_status)
summary(stroke_data$hypertension)
summary(stroke_data$heart_disease)
summary(stroke_data$ever_married)
summary(stroke_data$work_type)
summary(stroke_data$Residence_type)
summary(stroke_data$bmi)
summary(stroke_data)
stroke_data[1,1]

stroke1<-stroke_data[,-1]

boxplot(stroke1$age~stroke1$stroke,names=c("No Stroke","Stroke"),xlab="Stroke Status",ylab="Age",
        main="Age amongst stroke vs non-stroke patients")

boxplot(stroke1$avg_glucose_level~stroke1$stroke,names=c("No Stroke","Stroke"),xlab="Stroke Status",ylab="Average glucose level",
main="Avg Glucose level vs Stroke Status")

hist(stroke1$age,main="Distribution of Age",xlab="Age")

hist(stroke1$avg_glucose_level,main="Distribution of glucose level",
xlab="Average glucose level")



#glm = Generalized Linear Model
#binomial = either have "success or failure"
#stroke~. --> we are using ALL the variables to "predict" stroke prevalence

#we are interested in the final column in the summary(fit.glm) --> we want it to have * beside it :)
# * means it can be useful in prediction -->either *, **, or ***

#Pr(>|z|) --> we want it to be less than 0.05 --> that means potentially significant variable

fit.glm<-glm(stroke~.,data=stroke1,family=binomial)
summary(fit.glm)

#fit.glm<-glm(stroke~age+avg_glucose_level+gender+smoking_status,data=stroke1,family=binomial)


#Step 3: Separate the data into training and testing sets
set.seed(10)                                                                     #need to make the results reproducible
dt=sort(sample(nrow(stroke1),nrow(stroke1)*0.7))                                #goal: sample 70% of the data to be training, and 30% to be tested.
max(dt)
train<-stroke1[dt,]                                                               #we want all rows with row number dt to go into the training set
train
test<-stroke1[-dt,]                                                               #we want all rows without row number dt to go into the test set


#whenever we are choosing a random sample of values, we want to make sure the same sample is chosen every time


summary(train)
summary(test)

install.packages("randomForest")
require(randomForest)


#perform training
randomForest(as.factor(stroke)~.,data=train,ntree=100,mtry=2,importance=TRUE)
#~. <- it means that we are using ALL the variables to predict stroke prevalence
#issue - random forests needs there to be NO missing values
#mtry = the number of variables that are "tested" at each step to be useful
#mtry is used to make sure that one variable does not dominate

#lets remove all of the rows with missing values
summary(stroke_data)
#another option is to remove BMI - the only variable with missing values

#lets try both ways

#1) removing BMI
train_nobmi<-train[,-9] #removing the 9th column --> that has bmi
test_nobmi<-test[,-9]

dim(train_nobmi)
dim(test_nobmi)

#mtry = number of variables tried at each split on the tree = square root of the # of variables = square root 9 predictor variables

#mtry=square root of 10 = 3 or 4
r1<-randomForest(as.factor(stroke)~.,data=train_nobmi,ntree=100,mtry=3,importance=TRUE)


r2<-randomForest(as.factor(stroke)~.,data=train_nobmi,ntree=100,mtry=4,importance=TRUE)
r1
r2


#why does the algorithm not work?
#1) people with a stroke have similar characteristics to people without a stroke
#2) the variables chosen might confuse the algorithm - maybe choose a smaller subset of the variables
#--> try the subset that were significant for the glm
#instead of ~. --> ~age+gender+glucose... --> make sure mtry = sqrt(num of variables)





#0 and 1  on the vertical axis --> those are the TRUE values
# 0 and 1 on top, are the algorithm predicted values


#lets look at the important variables
varImpPlot(r1)  #variables at the top = most useful
#Mean Decrease Accuracy = if we dont use this varaible, how does the accuracy decrease in prediction


#random forest uses two ways to determine variable importance
#1) mean decrease accuracy
#2) mean decrease gini

#commment: age , ever married were useful in 1)
# avg_glucose_level and age were useful in 2)


varImpPlot(r2)
plot(r1)

                                                              #Now, using this random forest model to predict the prevalance of stroke on the test set
prediction_for_table <- predict(r1,test_nobmi[,-10])                   #remove stroke label
summary(prediction_for_table)
test_nobmi$strokefac<-as.factor(test_nobmi$stroke)
summary(test_nobmi$strokefac)
t1<-table(test_nobmi$strokefac,prediction_for_table) 
t1
                                                       #calculating some of the statistics from this confusion matrix 
require(e1071)
require(caret)                                                #<- really popular in anything R related
confusionMatrix(prediction_for_table,test_nobmi$strokefac) 
#1460 - correctly predicted as NOT having a stroke
#2 - correctly predicted as HAVING a stroke
#70 that SHOULD be predicted as stroke...
# 1 that was predicted as having a stroke, but did NOT have a stroke

#Sensitivity, Specificity, Pos Pred Value, Neg Pred Value--> include in slides




#------------------------------------------------------------------------------

#2) removing the missing rows
train_removeNA<-train[complete.cases(train),] #removing rows with missing values
summary(train_removeNA$bmi) #removed all the rows
test_removeNA<-test[complete.cases(test),] #removing rows with missing values
summary(test_removeNA$bmi)

#mtry = number of variabls tried at each split on the tree = square root of the # of variables

#mtry=square root of 10 = 3 or 4
r3<-randomForest(as.factor(stroke)~.,data=train_removeNA,ntree=100,mtry=3,importance=TRUE)
r3
prediction_for_table <- predict(r3,test_removeNA[,-11])
summary(prediction_for_table)
test_removeNA$strokefac<-as.factor(test_removeNA$stroke)
summary(test_removeNA$strokefac)
t1<-table(test_removeNA$strokefac,prediction_for_table) 
confusionMatrix(prediction_for_table,test_removeNA$strokefac) 

#1= having stroke
#0= not having stroke

#reference = actual values
#prediction = algorithm predicted values




#--------------------------------------------------------------------------------
#-- another option is to change the number of trees that are made

#lets do it first for the data without bmi
r4<-randomForest(as.factor(stroke)~.,data=train_nobmi,ntree=10,mtry=3,importance=TRUE)
r5<-randomForest(as.factor(stroke)~.,data=train_nobmi,ntree=1000,mtry=3,importance=TRUE)
r4
r5
#less values on the off-diagonal - better classification

#lets do it now for the rows with NA values
r6<-randomForest(as.factor(stroke)~.,data=train_removeNA,ntree=10,mtry=3,importance=TRUE)
r7<-randomForest(as.factor(stroke)~.,data=train_removeNA,ntree=1000,mtry=3,importance=TRUE)
r6
r7


#note: the larger number of trees does NOT do better at classifying the stroke participants
#but the larger number of trees DOES do better at classifying the non-stroke participants

#Now, applying these models to the test data - how well does the algorithm label the unlabelled data?
prediction_for_table1 <- predict(r4,test_nobmi[,-11])
summary(prediction_for_table)
test_nobmi$strokefac<-as.factor(test_nobmi$stroke)
summary(test_nobmi$strokefac)

prediction_for_table2 <- predict(r6,test_removeNA[,-11])
summary(prediction_for_table)
test_removeNA$strokefac<-as.factor(test_removeNA$stroke)
summary(test_removeNA$strokefac)


#Finally, looking at the confusion matrices
c1<-confusionMatrix(test_nobmi$strokefac,prediction_for_table1) 
c2<-confusionMatrix(test_removeNA$strokefac,prediction_for_table2) 

c1$table
c2$table

