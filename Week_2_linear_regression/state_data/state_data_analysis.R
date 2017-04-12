
#loading the data
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

# plotting the locations of the capitals of each state
plot(statedata$x, statedata$y)

# Determining region with highest mean High school graduation
tapply(statedata$HS.Grad, statedata$state.region, mean)

# making a boxplot of the murder rate by region
boxplot(Murder ~ state.region, data=statedata)

# outlier in the Northeast region of the boxplot 
subset(statedata, state.region=="Northeast", select=Murder)


# Initial model Predicting life expectancy
lifeex = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)

# plotting a graph of life expectancy vs income, as the model is not as expected
plot(statedata$Income, statedata$Life.Exp, xlab="State Income", ylab="Life Expectancy", col="red")


# After experimentation, finding the best model
lifeex_best = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)

# Comparing minimum life expectancy predicted and actual value
sort(predict(lifeex_best))
statedata$state.name[which.min(statedata$Life.Exp)]	# Actual min value
statedata$state.name[which.max(statedata$Life.Exp)]	# Actual max value

# Finding the states where we make the largest absolute error
sort(abs(lifeex_best$residuals))
