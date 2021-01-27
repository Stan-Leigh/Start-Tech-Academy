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
