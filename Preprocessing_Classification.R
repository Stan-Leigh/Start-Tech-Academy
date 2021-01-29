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
