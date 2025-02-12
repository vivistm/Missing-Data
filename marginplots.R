# Sinus

# PMM

comp_pmm_trans <- complete(imp_data_pmm_trans_first)
comp_pmm_trans_indices <- cbind(comp_pmm_trans[,c("x_1", "sinus_x1")], sinus_x1_imp = imp_data_pmm_trans_first$where[, "x_1"])
marginplot(comp_pmm_trans_indices, delimiter = "_imp")

comp_pmm_imp <- complete(imp_data_pmm_imp_first)
comp_pmm_imp_indices <- cbind(comp_pmm_imp[,"x_1"], sinus_x1 = sin(comp_pmm_imp[,"x_1"]), sinus_x1_imp = imp_data_pmm_imp_first$where[, "x_1"])
marginplot(comp_pmm_imp_indices, delimiter = "_imp")

# RF

comp_rf_trans <- complete(imp_data_rf_trans_first)
comp_rf_trans_indices <- cbind(comp_rf_trans[,c("x_1", "sinus_x1")], sinus_x1_imp = imp_data_rf_trans_first$where[, "x_1"])
marginplot(comp_rf_trans_indices, delimiter = "_imp")

comp_rf_imp <- complete(imp_data_rf_imp_first)
comp_rf_imp_indices <- cbind(comp_rf_imp[,"x_1"], sinus_x1 = sin(comp_rf_imp[,"x_1"]), sinus_x1_imp = imp_data_rf_imp_first$where[, "x_1"])
marginplot(comp_rf_imp_indices, delimiter = "_imp")

# Interaction

# PMM
comp_pmm_trans <- complete(imp_data_pmm_trans_first)
comp_pmm_trans_indices <- cbind(comp_pmm_trans[,c("x_1", "interaktion")], interaktion_imp = imp_data_pmm_trans_first$where[, "x_1"])
marginplot(comp_pmm_trans_indices, delimiter = "_imp")

comp_pmm_imp <- complete(imp_data_pmm_imp_first)
comp_pmm_imp_indices <- cbind(comp_pmm_imp[,"x_1"], interaktion = comp_pmm_imp[,"x_1"]*comp_pmm_imp[,"x_3"], interaktion_imp = imp_data_pmm_imp_first$where[, "x_1"])
marginplot(comp_pmm_imp_indices, delimiter = "_imp")

# RF

comp_rf_trans <- complete(imp_data_rf_trans_first)
comp_rf_trans_indices <- cbind(comp_rf_trans[,c("x_1", "interaktion")], interaktion_imp = imp_data_rf_trans_first$where[, "x_1"])
marginplot(comp_rf_trans_indices, delimiter = "_imp")

comp_rf_imp <- complete(imp_data_rf_imp_first)
comp_rf_imp_indices <- cbind(comp_rf_imp[,"x_1"], interaktion = comp_rf_imp[,"x_1"]*comp_rf_imp[,"x_3"], interaktion_imp = imp_data_rf_imp_first$where[, "x_1"])
marginplot(comp_rf_imp_indices, delimiter = "_imp")

# Quadratic

# PMM
comp_pmm_trans <- complete(imp_data_pmm_trans_first)
comp_pmm_trans_indices <- cbind(comp_pmm_trans[,c("x_1", "x1_squared")], x1_squared_imp = imp_data_pmm_trans_first$where[, "x_1"])
marginplot(comp_pmm_trans_indices, delimiter = "_imp")

comp_pmm_imp <- complete(imp_data_pmm_imp_first)
comp_pmm_imp_indices <- cbind(comp_pmm_imp[,"x_1"], x1_squared = comp_pmm_imp[,"x_1"]^2, x1_squared_imp = imp_data_pmm_imp_first$where[, "x_1"])
marginplot(comp_pmm_imp_indices, delimiter = "_imp")

# RF

comp_rf_trans <- complete(imp_data_rf_trans_first)
comp_rf_trans_indices <- cbind(comp_rf_trans[,c("x_1", "x1_squared")], x1_squared_imp = imp_data_rf_trans_first$where[, "x_1"])
marginplot(comp_rf_trans_indices, delimiter = "_imp")

comp_rf_imp <- complete(imp_data_rf_imp_first)
comp_rf_imp_indices <- cbind(comp_rf_imp[,"x_1"], x_1_squared = comp_rf_imp[,"x_1"]^2, x_1_squared_imp = imp_data_rf_imp_first$where[, "x_1"])
marginplot(comp_rf_imp_indices, delimiter = "_imp")

# Polynomial method


