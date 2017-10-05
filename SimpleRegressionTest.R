#Record of production of firtilization of a company in the given year
year <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002)
production<- c(25, 40, 60, 45, 65, 50, 75, 80)
CompanyRecord <- data.frame(year, production)
ggplot(CompanyRecord, aes(x= production, y = year))+geom_point()+geom_smooth()
str(CompanyRecord)
summary(CompanyRecord)
colSums(is.na(CompanyRecord))
cor(CompanyRecord)
reg_Model<- lm(production ~ ., data = CompanyRecord)
summary(reg_Model)
ggplot(CompanyRecord, aes(x= production, y = year))+geom_line()# high residual error 
par(mfrow = c(2,2))
plot(reg_Model)
reg_Model<- update(reg_Model, log(log(production)) ~ .)
summary(reg_Model)
# lets see again how much r square is adjusted
par(mfrow = c(2,2))
plot(reg_Model)
# lets break it into trainig and test set and check 
set.seed(1)
d<- sample(seq_len(nrow(CompanyRecord)), size = nrow(CompanyRecord) * 0.7)
train_set <- CompanyRecord[d, ]
test_set <- CompanyRecord[-d, ]
# doing linear regression on train data
reg_Model <- lm(log(production) ~ .,data = train_set )
#lets predict the production
reg_predict <- predict(reg_Model, test_set)# not accuracy in value
#lets make it accurate
reg_predict <- exp(reg_predict)# not a good prediction
reg_predict
#comparisoin
test_set # output is far from the actual value
##library(Metrics)
##x <- boxplot(train_set$year,varwidth = T,outline = T,border = T,plot = T, col = 8)
rmse(actual = test_set$production,predicted = reg_predict)