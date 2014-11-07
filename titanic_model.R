library(randomForest)
library(bst)

train = read.csv('/Users/fliptop/Downloads/train.csv', header=T, stringsAsFactor=T)
test = read.csv('/Users/fliptop/Downloads/test.csv', header=T, stringsAsFactor=T)

train$PassengerId = NULL

#Female = 1, Male = 0
train$Sex = gsub('female',1,train$Sex)
train$Sex = gsub('^male',0,train$Sex)

Age_Range <- function(df){
  for (row in 1:nrow(df)){
    if (is.na(df[row,'Age'])){
      #df[row,'Age_Group'] = 0
      df[row,'Age_Class'] = 'Not Available'
    }
    else if (df[row,'Age'] <= 12){
      #df[row,'Age_Group'] = 1
      df[row,'Age_Class'] = 'Child'
    }
    else if (df[row,'Age'] <= 18){
      #df[row,'Age_Group'] = 2
      df[row,'Age_Class'] = 'Young Adult'
    }
    else if (df[row,'Age'] <= 29){
      #df[row,'Age_Group'] = 3
      df[row,'Age_Class'] = 'Adult'
    }
    else if (df[row,'Age'] <= 39){
      #df[row,'Age_Group'] = 4
      df[row,'Age_Class'] = 'Family Man'
    }
    else if (df[row,'Age'] <= 54){
      #df[row,'Age_Group'] = 5
      df[row,'Age_Class'] = 'Mid-Age Madness'
    }
    else{
      #rain[row,'Age_Group'] = 6
      df[row,'Age_Class'] = 'OLD'
    }
  }
  df
}

Title_Assign <- function(df){
  for (row in 1:nrow(df)){
    if (grepl('Mr.', df[row,'Name'], fixed=TRUE))
    {
      df[row,'Title'] = 'Mr.'
    }
    else if (grepl('Mrs.', df[row,'Name'], fixed=TRUE))
    {
      df[row,'Title'] = 'Mrs.'
    }
    else if (grepl('Miss.', df[row,'Name'], fixed=TRUE) | grepl('Ms.', df[row,'Name'], fixed=TRUE))
    {
      df[row,'Title'] = 'Miss.'
    }
    else if (grepl('Master.', df[row,'Name'], fixed=TRUE))
    {
      df[row,'Title'] = 'Master.'
    }
    else if (grepl('Rev.', df[row,'Name'], fixed=TRUE))
    {
      df[row,'Title'] = 'Rev.'
    }
    else if (grepl('Dr.', df[row,'Name'], fixed=TRUE))
    {
      df[row,'Title'] = 'Dr.'
    }
    else
    {
      df[row,'Title'] = 'Other'
    }
  }
  df
}

train2 = Age_Range(train)
train2 = Title_Assign(train2)
train2$Ticket = NULL
train2$Cabin = NULL
#train2$Age_Group = NULL
#train2$Age = NULL
train2$Name = NULL
train2$Title = as.factor(train2$Title)
train2$Age_Class = as.factor(train2$Age_Class)
train2$Embarked = as.factor(train2$Embarked)
train2$Sex = as.factor(train2$Sex)
train2$Pclass = as.factor(train2$Pclass)
train2$Survived = as.factor(train2$Survived)
train_set = train2[1:(round(nrow(train2)*3/4)),]
test_set = train2[(round(nrow(train2)*3/4)):nrow(train2),]

test2 = Age_Range(test)
test2 = Title_Assign(test2)
test2$Sex = gsub('female',1,test2$Sex)
test2$Sex = gsub('^male',0,test2$Sex)
test2$Ticket = NULL
test2$Cabin = NULL
test2$Name = NULL
test2$PassengerId = NULL
test2$Title = as.factor(test2$Title)
test2$Age_Class = as.factor(test2$Age_Class)
test2$Embarked = as.factor(test2$Embarked)
test2$Sex = as.factor(test2$Sex)
test2$Pclass = as.factor(test2$Pclass)
#test2$Survived = as.factor(test2$Survived)

train.rf = randomForest(Survived ~. , data=train2, ntrees=200, na.action=na.omit, type=classification, importance=TRUE)

train.bst = bst(Survived~., train_set)
train2.rf = randomForest(Survived ~. , data=train_set, ntree=200, na.action=na.omit, type=classification, importance=TRUE)

x = predict(train.rf, test_set, type='response')
x1 = gbm.perf(train.gbm, test_set, method='OOB')
y = predict(train2.rf, test2, type='response')