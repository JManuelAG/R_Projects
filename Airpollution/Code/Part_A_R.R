## Question 1
# a)
airpollution = read.csv("airpollution.csv", header = FALSE, skip = 2)
df = apply(airpollution, 2, str_split, boundary("word"))
df = df$V1

table(sapply(df, length))

for (a in 1:length(df)) {
  if (length(df[[a]]) == 14) {
  df[[a]] = c(paste(df[[a]][1],df[[a]][2]), df[[a]][3:14])
  }
}

df = df[-82]

df = data.frame(matrix(unlist(df), nrow = length(df), byrow=TRUE))
names(df) = df[1,]
df = df[-1,]
row.names(df) = 1:80
df = data.frame("CITY" = df[,1],apply(df[,-1], 2, as.double))

df


# b) Find the covariance matrix 

df.cov = cov(df[,-1])

# c) Use R to find the eigenvalues and eigenvectors of the covariance matrix. 

df.eigen = eigen(df.cov)

# d) Provide the diagonalized form of the covariance matrix in R.

df.diagonal = diag(df.eigen$values,12,12)



## Question 2

df.prcomp = prcomp(df[,-1])
summary(df.prcomp)

# i) Split your airpollution dataset into a training 
# and testing datasets as in W4S3 topic notes where 80% of dataset is in training set.

ind = sample(2, nrow(df), replace = TRUE, prob = c(.8, .2))
training = df[ind==1,]
testing = df[ind==2,]


# ii) Use R to compute the correlation coefficient between the variables and a 
# scatterplot to describe the relationship between any two of the variables which 
# are strongly correlated. 

df.cor = cor(df[,-1])
df.cor>.8
model = lm(df$TMR ~ df$GE65)
summary(model)

plot(TMR ~ GE65, data = df)
abline(model, col = "red")
legend("topleft", legend = "Regression line", 
       col = "red", lty = 1, cex = .6)



# iii) Conduct PCA analysis on the training dataset, 
# present your findings and discuss the principal components and explained variance. 

training.pca = prcomp(training[,-1])
plot(training.pca, type = "l", main = "Top 10 PCS")
summary(training.pca)

## Plot - Option2 
screeplot( training.pca, type= "l", npcs =10, main = "Plot the 10 first PCS" )


# iv) Provide a visualization with ggbiplot of the first two PCs. 

Group = if_else(training$TMR > mean(training$TMR), "High TMR", "Low TMR")
ggbiplot(training.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE, 
         groups = Group)

# v)  Form a data set with your training set in terms of the 
# PC components and objective variable. 

Pred.training = predict(training.pca, training)
Pred.training = data.frame(Pred.training, training[2])

head(Pred.training)
# vi) Use glm or lm to determine a general linear model to predict the response 
# variable of TMR. Present your R code, output and summary of model. Are the coefficients 
# for the slopes significant? Is the intercept meaningful, if it is not meaningful 
# then explain why and produce a zero-intercept model?

TMR.Lmodel = lm(TMR ~ (PC1 + PC2), data = Pred.training)
summary(TMR.Lmodel)
TMR.Lmodel$coefficients
abline(TMR.Lmodel)

training %>% summarise(mean(TMR))

# vii)

## Test on testing
Pred.testing = predict(training.pca, testing)
Pred.testing = data.frame(Pred.testing, testing[2])

test2 = predict(TMR.Lmodel, Pred.testing )
test2.df = data.frame(df[names(test2),c(1,2)],"Prediction" = test2)
test2.df

## Test on training 
test = predict(TMR.Lmodel, Pred.training)
test.df = data.frame(df[names(test),c(1,2)],test)

test.df[test.df$CITY == "SAN FRAN",]


### GE65 example
GE65.Lmodel = lm(training$GE65 ~ (PC1 + PC2), data = Pred.training)
test2 = predict(GE65.Lmodel, Pred.training)
data.frame(df[names(test2),c(1,12)],test2)



## Question 3

Cost = function(x,y){ 920 - (x-120)* y * exp(-((x-120)^2+(y/100)^2 ))}
Der.CostX = function(x,y){(2*(x-120)^2) * y * exp(-((x-120)^2+(y/100)^2 ))}
Der.CostY = function(x,y){(x - 120)*((2/10000)*y^2 -1)*exp(-((x-120)^2+(y/100)^2 ))}


GradientD = function(startingX, startingY, stepfactor, tests, functionX, derivativeFX, derivativeFY){
  x = startingX
  xtrace = x
  y = startingY
  ytrace = y
  stepFactor = stepfactor
  ftrace = functionX(x,y)
  for (step in 1:tests){
    x = x - stepFactor*derivativeFX(x,y)
    y = y - stepFactor*derivativeFY(x,y)
    xtrace = c(xtrace, x)
    ytrace = c(ytrace, y)
    ftrace = c(ftrace,functionX(x,y))
  }
  return(data.frame("x" = xtrace,"y" = ytrace,  "f(x,y)" = ftrace ))
}

GradientD(121,70,.001,100,Cost,Der.CostX,Der.CostY)

Cost(120.7,70)


# g) Prediction

df[df$PMAX<125 & df$SMAX<75,]

df.avarages = summarise_all(df[-1], mean)
df.avarages[c(4,7)] = c(70,120.7)
avarages.pred = predict(training.pca, df.avarages)
TMR.Grad.Pred = c("TMR Prediction" = predict(TMR.Lmodel, as.data.frame(avarages.pred)))

df[df$TMR>=940 & df$TMR<=942,]
