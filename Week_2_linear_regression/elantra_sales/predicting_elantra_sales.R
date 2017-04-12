
elantra_all = read.csv("elantra.csv")

#Creating training and test set
elantra = subset(elantra_all, Year <= 2012)
elantra_test = subset(elantra_all, Year >= 2013)

# Creating an initial model
sales1 = lm(ElantraSales ~ Unemployment+Queries+CPI_energy+CPI_all, data=elantra)

# Modelling seasonality into our model
sales2 = lm(ElantraSales ~ Month + Unemployment + Queries + CPI_energy + CPI_all, data = elantra)


# Modifying the model by adding Month as a factor
elantra$MonthFactor = as.factor(elantra$Month)
elantra_test$MonthFactor = as.factor(elantra_test$Month)

# Using a new model
sales3 = lm(ElantraSales ~ MonthFactor + Unemployment + Queries + CPI_energy + CPI_all, data = elantra)

# Finding correlation between Queries and other factors
cor(elantra[,c("Queries", "CPI_energy", "CPI_all", "Month", "Unemployment")])

#Creating a new model, removing Queries variable
sales4 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_energy + CPI_all, data=elantra)


# Predicting the test set values using the lates model
sales_pred = predict(sales4, newdata=elantra_test)

sse_test = sum((sales_pred - elantra_test$ElantraSales) ^ 2)


baseline_pred = mean(elantra$ElantraSales)

R2 = 1 - sse_test / sst_test


# Maximum absolute error
sort(abs(sales_pred - elantra_test$ElantraSales))

# We can find the month where we make the maximum error
index = which.max(abs(sales_pred - elantra_test$ElantraSales))[[1]]
elantra_test[index,]
