library(mice)


# Fragen:
# Bei impute, then transform nutzt man die gemittelten Daten (complete) oder transformed man da für jede einzelne Imputation?


set.seed(123)
n <- 100
iterations <- 1000
percent_missing <- 0.3
n_imp <- 5

X1 <- rnorm(n, 10, 3)
X2 <- rnorm(n, 200, 50)
X3 <- rnorm(n, 100, 30)

#TODO checken, ob die autocorreliert sind

Y <- 2.4 * X1 + 1.2 * X2 + 0.7 * X3 + 0.5 * X1^2 + rnorm(n, 0, 5)

true_model <- lm(Y ~ X1 + X2 + X3 + I(X1^2))

hist(Y)
plot(X1, Y)


X1[sample(n, percent_missing * n, replace = FALSE)] <- NA
X2[sample(n, percent_missing * n, replace = FALSE)] <- NA
Y[sample(n, percent_missing * n, replace = FALSE)] <- NA

data <- data.frame(X1 = X1, X2 = X2, X3 = X3, Y = Y)


#Impute, then transform

ImpData <- mice(data)

imputed_data <- complete(ImpData, action = "all")

#for(i in 1:n_imp){
 # imputed_set <- imputed_data[[i]]
  #imputed_set <- cbind(imputed_set, X1_squared = imputed_set$X1^2)

  #fit <- lm(Y ~ X1 + X2 + X3 + X1_squared, imputed_set)

#}

fit <- with(ImpData, lm(Y ~ X1 + X2 + X3 + I(X1^2))) #Ist das die einfachere Lösung? ALso muss ich überhaupt erst die Variable in den Dataframe hinzufügen

pooledRes <- pool(fit)
summary(pooledRes)
