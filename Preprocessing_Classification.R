df <- read.csv('C://Users/ugonn/Videos/DS/ML Course/Data Files/2. ST Academy - Classification models resource files/House-Price.csv', header=TRUE)
View(df)

str(df)

# EDD 
summary(df)

boxplot(df$n_hot_rooms)

pairs(~df$Sold+df$rainfall)

barplot(table(df$airport))
barplot(table(df$bus_ter))

# OBSERVATIONS
# 1. rainfall, n_hot_rooms has outliers
# 2. n_hos_beds has missing values
# 3. a useless categorical variable. bus_ter


# Capping and Flooring
quantile(df$n_hot_rooms, 0.99)
uv <- 3 * quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv

summary(df$n_hot_rooms)

lv <- 0.3 * quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall < lv] <- lv

summary(df$rainfall)


# Missing Value Imputation
mean(df$n_hos_beds, na.rm = TRUE) 
# na.rm makes sure you can find the mean even though NA values are in the column

which(is.na(df$n_hos_beds))  # which rows have NA values

df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds, na.rm = TRUE) 

summary(df$n_hos_beds)


# Variable Transformation
df$avg_dist = (df$dist1 + df$dist2 + df$dist3 + df$dist4) / 4

# Deleting variables
df2 <- df[,-6:-9]  # delete dist1 - dist4
df <- df2
rm(df2)  # remove the df2 variable

df <- df[,-13]


# Dummy Variable Creation
df <- dummy.data.frame(df)
df <- df[,-8]
df <- df[,-13]


# Logistic Regression with Single Predictor

glm.fit = glm(Sold~price, data=df, family = binomial)
summary(glm.fit)

# Logistic Regression with multiple predictors
glm.fit = glm(Sold~., data=df, family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, type = 'response')
glm.probs[1:10]

glm.pred <- rep('NO', 506)
glm.pred[glm.probs > 0.5] <- 'YES'


# Confusion Matrix
table(glm.pred, df$Sold)


# Linear Discriminant Analysis
lda.fit <- lda(Sold~., data = df )
lda.fit

lda.pred <- predict(lda.fit, df)
lda.pred$posterior

lda.class <- lda.pred$class

table(lda.class, df$Sold)  # Confusion Matrix

# To find number of values after adjusting the boundary
sum(lda.pred$posterior[ ,1] > 0.8)

# To run quadratic discriminant analysis, change lda to qda


set.seed(0)
split = sample.split(df, SplitRatio = 0.8)
train_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# For Logistic Regression
train.fit <- glm(Sold~., data = train_set, family = binomial)
test.probs <- predict(train.fit, test_set, type = 'response')

test.pred = rep('NO', 120)
test.pred[test.probs > 0.5] <- 'YES'

table(test.pred, test_set$Sold)


# K Nearest Neighbor
trainX <- train_set[, -16]
testX <- test_set[, -16]
trainy <- train_set$Sold
testy <- test_set$Sold

k <- 1

# Don't forget to standardize the variables
trainX_s = scale(trainX)
testX_s = scale(testX)

set.seed(0)

knn.pred <- knn(trainX_s, testX_s, trainy, k = k)

table(knn.pred, testy)
