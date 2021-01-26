df <- read.csv('C:/Users/ugonn/Videos/DS/ML Course/Data Files/1. ST Academy - Crash course and Regression files/House_Price.csv', header = TRUE)

View(df)

str(df)

summary(df)

# Analyze some columns suspected of having outliers or being skewed
hist(df$crime_rate)
# Plot plenty plots at a time
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df)

# Plot barplots for categorical variables
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))

# Obsevations
# n_hot_rooms and rainfall has outliers
# n_hos_beds has missing values
# bus_ter is a useless variable
# crime_rate has some other functional relationship with price

# Capping and Flooring
quantile(df$n_hot_rooms, 0.99)
uv = 3 * quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv

summary(df$n_hot_rooms)

lv = 0.3 * quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall < lv] <- lv

summary(df$rainfall)

# Missing value imputation
mean(df$n_hos_beds)
mean(df$n_hos_beds, na.rm = TRUE)
which(is.na(df$n_hos_beds))  # Lists rows with missing values
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds, na.rm = TRUE) 

summary(df$n_hos_beds)
which(is.na(df$n_hos_beds)) 


# Variable transformation
pairs(~price+crime_rate, data = df)  # or plot(df$price, df$crime_rate)

df$crime_rate = log(1 + df$crime_rate)

df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4) / 4

df2 <- df[, -7:-10]  # so you dont remove an important column  by mistake
df <- df2
rm(df2)

df <- df[, -14]

# install dummies package to be able to get dummy variables
install.packages('dummies')

df <- dummy.data.frame(df)

df <- df[, -9]
df <- df[, -14]

# Correlation Analysis
cor(df)
round(cor(df), 2)

df <- df[, -16]

# Simple Linear Regression

simple_model <- lm(price~room_num, data=df)
summary(simple_model)

plot(df$room_num, df$price)
abline(simple_model)


# Multiple Linear Regression
multiple_model <- lm(price~., data=df)  # the '.' means all other columns.
summary(multiple_model)


# Train-Test-Split
install.packages('caTools')

set.seed(0)  # Like random state in python
split = sample.split(df, SplitRatio = 0.8) # train_test_split in python
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

lm_a = lm(price~.,data=training_set)

train_a = predict(lm_a, training_set)
test_a = predict(lm_a, test_set)

mean((training_set$price - train_a)^2)
mean((test_set$price - test_a)^2)


# Subset selection techniques
install.packages('leaps')

lm_best = regsubsets(price~., data = df, nvmax = 15)  # Best subset selection technique
# nvmax is the number of variables used in subset selection. default max is 8
summary(lm_best)

# To get the adjusted r2, which will show the best model:
summary(lm_best)$adjr2

which.max(summary(lm_best)$adjr2)

coef(lm_best, 8)


lm_forward = regsubsets(price~., data = df, nvmax = 15, method = 'forward')
# forward selection technique
summary(lm_forward)

summary(lm_forward)$adjr2

which.max(summary(lm_forward)$adjr2)


lm_backward = regsubsets(price~., data = df, nvmax = 15, method = 'backward')
# backward selection technique
summary(lm_backward)

summary(lm_backward)$adjr2

which.max(summary(lm_backward)$adjr2)

# Shrinkage methods: Ridge and Lasso Regression
install.packages('glmnet')

x = model.matrix(price~., data = df)[, -1]  # to remove the price column
y = df$price

grid = 10^seq(10, -2, length=100)  # values of lambda to check the one with lowest error.

lm_ridge = glmnet(x, y, alpha=0, lambda=grid)

cv_fit <- cv.glmnet(x, y, alpha=0, lambda = grid)
plot(cv_fit)

opt_lambda <- cv_fit$lambda.min  # value of lambda with smallest error

# to get the r2 score
tss <- sum((y - mean(y)) ^ 2)

y_a = predict(lm_ridge, s=opt_lambda, newx = x) 

rss = sum((y_a - y) ^ 2)

rsq = 1 - (rss / tss)

# Lasso
lm_lasso = glmnet(x, y, alpha=1, lambda=grid) 

cv_fit_lasso <- cv.glmnet(x, y, alpha=1, lambda = grid)
plot(cv_fit_lasso)

opt_lambda_lasso <- cv_fit_lasso$lambda.min

tss_l <- sum((y - mean(y)) ^ 2)

y_l = predict(lm_lasso, s=opt_lambda_lasso, newx = x) 

rss_l = sum((y_l - y) ^ 2)

rsq_l = 1 - (rss_l / tss_l)
