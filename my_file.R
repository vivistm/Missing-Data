library(mice)


# Fragen:
# Bei impute, then transform nutzt man die gemittelten Daten (complete) oder transformed man da für jede einzelne Imputation?
# soll im dgp ein intercept sein?


# TODO:
# Z transformation
# andere imputation method
# wie speichern wie die Ergebnisse? -> Mehrere Durchläufe
# Methoden anschauen

# methods_names <- c("pmm_imp_first", "pmm_trans_first", "pmm_3rd_method",
#                   "reg_imp_first", "reg_trans_first", "reg_3rd_method")

# Speichern für Ergebnisse der Iterationen
#results_of_all_methods <- setNames(vector("list", length = 6), methods_names)

results_of_all_methods <- list (
  "pmm_imp_first" = list(),
  "pmm_trans_first" = list()#,
  #"pmm_3rd_method" = list(),
  #"reg_imp_first" = list(),
  #"reg_trans_first" = list(),
  #"reg_3rd_method" = list()
)

results_imp_first <- list()
results_trans_first <- list()

set.seed(123)
n <- 200
iterations <- 10
percent_missing <- 0.3
n_imp <- 5

for (i in 1:iterations) {
  X1 <- rnorm(n, 10, 3)
  X2 <- rnorm(n, 200, 50)
  X3 <- rnorm(n, 100, 30)

  #TODO checken, ob die autokorreliert sind

  Y <-
    20 + 1.2 * X1 + 2.4 * X2 + 0.7 * X3 + 0.5 * X1 ^ 2 + rnorm(n, 0, 5)

  true_model <- lm(Y ~ X1 + X2 + X3 + I(X1 ^ 2))
  summary(true_model)



  X1[sample(n, percent_missing * n, replace = FALSE)] <- NA
  X2[sample(n, percent_missing * n, replace = FALSE)] <- NA
  Y[sample(n, percent_missing * n, replace = FALSE)] <- NA

  data <- data.frame(X1 = X1,
                     X2 = X2,
                     X3 = X3,
                     Y = Y)


  #Impute, then transform

  ImpData <- mice(data)

  model_impute_first <- with(ImpData, lm(Y ~ X1 + X2 + X3 + I(X1 ^ 2)))
  pooled_imp_first <- pool(model_impute_first)
  coeff_imp_first <- summary(pooled_imp_first)

  results_of_all_methods[["pmm_imp_first"]][[i]] <-
    calculate_metrics(coeff_imp_first, true_model) #Frage: Reicht es diese Werte abzuspeichern oder brauchen wir noch mehr?

  #results <- list(coeffcients = coeff_imp_first, bias = bias_impute_first, coverage = cov_imp_first)


  #Transform then impute

  data_with_transformed_variable <-
    cbind(data, X1_squared <- data$X1 ^ 2)

  ImpDataTransFirst <- mice(data_with_transformed_variable)

  model_trans_first <-
    with(ImpData, lm(Y ~ X1 + X2 + X3 + X1_squared))
  pooled_trans_first <- pool(model_trans_first)
  coeff_trans_first <- summary(pooled_trans_first)


  results_of_all_methods[["pmm_trans_first"]][[i]] <-
    calculate_metrics(coeff_trans_first, true_model)

}


# Calculate averages over all iterations of one method

all_average_results <- list()


for (method_name in names(results_of_all_methods)) {
  method <- results_of_all_methods[[method_name]]
  avg_results <- data.frame(estimate = numeric(0), bias = numeric(0), coverage = numeric(0))

  for (covariate in row.names(method[[1]])) {
    coefficient <- mean(sapply(results_imp_first, function(df)
        df[covariate, "estimate"]))
    bias <- mean(sapply(results_imp_first, function(df)
        df[covariate, "bias"]))
    coverage <- mean(sapply(results_imp_first, function(df)
        df[covariate, "cov"]))

    # Create a new data frame for the row to bind
    new_row <- data.frame(estimate = coefficient, bias = bias, coverage = coverage)

    # Bind the row while preserving column names
    avg_results <- rbind(avg_results, new_row)

    # Assign the row name
    row.names(avg_results)[nrow(avg_results)] <- covariate
  }
  all_average_results[[method_name]] <- avg_results
}

#TODO: vlt umbenennen, da nicht nur Metriken berechnet, sondern auch Coefficients abgespeichert werden. vlt create_results oder so?
calculate_metrics <-
  function(coefficients, true_model, ci_value = 0.95) {
    bias <- true_model$coefficients - coefficients$estimate

    ci_lower <- coefficients$estimate - ci_value / 2 * coeff_imp_first$std.error
    ci_upper <- coefficients$estimate + ci_value / 2 * coeff_imp_first$std.error
    cov <- ci_lower <= true_model$coefficients & true_model$coefficients <= ci_upper

    metrics <-
      cbind(coefficients, bias, cov) # man könnte überlegen, auch noch die CI grenzen zu speichern
  }


#Ablage
#Impute then transform

#imputed_data <- complete(ImpData, action = "all")

#for(i in 1:n_imp){
# imputed_set <- imputed_data[[i]]
#imputed_set <- cbind(imputed_set, X1_squared = imputed_set$X1^2)

#fit <- lm(Y ~ X1 + X2 + X3 + X1_squared, imputed_set)

#}
