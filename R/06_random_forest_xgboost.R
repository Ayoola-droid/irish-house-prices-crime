# Random Forest and XGBoost
# =========================
# Tuned with tidymodels, variable importance, partial dependence plots for each crime type, test-set metrics.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: output of 03

# Machine learning Models RANDFR & XGBOOST --------------------------------

#prep & bake recipe
rec_prep   <- prep(rec)
train_rf  <- bake(rec_prep, 
                     new_data = crime_500_train_data)
test_rf   <- bake(rec_prep, 
                     new_data = crime_500_test_data)

#turn train_clean into validation split
val_split  <- validation_split(train_rf, #changed validation_split to initial_validation_split
                               prop = 0.8)

# make random-forest spec with tuning placeholder
rf_spec <- rand_forest(
    mode   = "regression",
    trees  = tune()) %>%
    set_engine(
        "ranger",
        importance  = "impurity",
        num.threads = parallel::detectCores()
    )

# workflow that uses H1 formula
rf_wf_H1 <- workflow() %>%
    add_model(rf_spec) %>%
    add_formula(model_formula_H1)

# workflow that uses H2 formula
rf_wf_H2 <- workflow() %>%
    add_model(rf_spec) %>%
    add_formula(model_formula_H2)

# workflow that uses H3 formula
rf_wf_H3 <- workflow() %>%
    add_model(rf_spec) %>%
    add_formula(model_formula_H3)

# workflow that uses H4 formula
rf_wf_H4 <- workflow() %>%
    add_model(rf_spec) %>%
    add_formula(model_formula_H4)

# workflow that uses H5 formula
rf_wf_H5 <- workflow() %>%
    add_model(rf_spec) %>%
    add_formula(model_formula_H5)

# small grid to speed up rf
rf_grid <- grid_regular(trees(range = c(100, 2000)), 
                        levels = 3)

#tune on the preprocessed training data
rf_tune_H1 <- tune_grid(
    rf_wf_H1,
    resamples = val_split,
    grid      = rf_grid,
    metrics   = metric_set(rmse)
)
# tune H2
rf_tune_H2 <- tune_grid(
    rf_wf_H2,
    resamples = val_split,
    grid      = rf_grid,
    metrics   = metric_set(rmse)
)
# H3
rf_tune_H3 <- tune_grid(
    rf_wf_H3,
    resamples = val_split,
    grid      = rf_grid,
    metrics   = metric_set(rmse)
)
# H4
rf_tune_H4 <- tune_grid(
    rf_wf_H4,
    resamples = val_split,
    grid      = rf_grid,
    metrics   = metric_set(rmse)
)
# H5
rf_tune_H5 <- tune_grid(
    rf_wf_H5,
    resamples = val_split,
    grid      = rf_grid,
    metrics   = metric_set(rmse)
)


#grab best tree count
best_trees_H1 <- select_best(rf_tune_H1, 
                          metric = "rmse")
# H2
best_trees_H2 <- select_best(rf_tune_H2, 
                             metric = "rmse")
# H3
best_trees_H3 <- select_best(rf_tune_H3, 
                             metric = "rmse")
# H4
best_trees_H4 <- select_best(rf_tune_H4, 
                             metric = "rmse")
# H5
best_trees_H5 <- select_best(rf_tune_H5, 
                             metric = "rmse")

# finalize and refit on all of train_clean
final_rf_H1 <- finalize_workflow(rf_wf_H1, best_trees_H1)
rf_fit_H1    <- fit(final_rf_H1, 
                 data = train_rf)
# H2
final_rf_H2 <- finalize_workflow(rf_wf_H2, best_trees_H2)
rf_fit_H2    <- fit(final_rf_H2, 
                    data = train_rf)
# H3
final_rf_H3 <- finalize_workflow(rf_wf_H3, best_trees_H3)
rf_fit_H3    <- fit(final_rf_H3, 
                    data = train_rf)
# H4
final_rf_H4 <- finalize_workflow(rf_wf_H4, best_trees_H4)
rf_fit_H4    <- fit(final_rf_H4, 
                    data = train_rf)
# H5
final_rf_H5 <- finalize_workflow(rf_wf_H5, best_trees_H5)
rf_fit_H5    <- fit(final_rf_H5, 
                    data = train_rf)


#extract ranger object
ranger_obj_H1 <- rf_fit_H1 %>%
    extract_fit_parsnip() %>%
    .$fit

#plot variable importance
vip(ranger_obj_H1)
# H2
ranger_obj_H2 <- rf_fit_H2 %>%
    extract_fit_parsnip() %>%
    .$fit

#plot variable importance
vip(ranger_obj_H2)
# H3
ranger_obj_H3 <- rf_fit_H3 %>%
    extract_fit_parsnip() %>%
    .$fit

#plot variable importance
vip(ranger_obj_H3)
# H4
ranger_obj_H4 <- rf_fit_H4 %>%
    extract_fit_parsnip() %>%
    .$fit

#plot variable importance
vip(ranger_obj_H4)
# H5
ranger_obj_H5 <- rf_fit_H5 %>%
    extract_fit_parsnip() %>%
    .$fit

#plot variable importance
vip(ranger_obj_H5)




# hypothesis main variable
imp_H1 <- ranger_obj_H1$variable.importance
imp_crime_H1 <- imp_H1[names(imp_H1) %in% c("TotalCrime","ViolentCrime",
                                   "NonViolentCrime","PersonalSafetyCrime",
                                   "PropertyCrime","TotalCrime_lag1",
                                   "TotalCrime_lag2")]
print(imp_crime_H1)
# H2
imp_H2 <- ranger_obj_H2$variable.importance
imp_crime_H2 <- imp_H2[names(imp_H2) %in% c("TotalCrime","ViolentCrime",
                                            "NonViolentCrime","PersonalSafetyCrime",
                                            "PropertyCrime","TotalCrime_lag1",
                                            "TotalCrime_lag2")]
print(imp_crime_H2)
# H3
imp_H3 <- ranger_obj_H3$variable.importance
imp_crime_H3 <- imp_H3[names(imp_H3) %in% c("TotalCrime","ViolentCrime",
                                            "NonViolentCrime","PersonalSafetyCrime",
                                            "PropertyCrime","TotalCrime_lag1",
                                            "TotalCrime_lag2")]
print(imp_crime_H3)
# H4
imp_H4 <- ranger_obj_H4$variable.importance
imp_crime_H4 <- imp_H4[names(imp_H4) %in% c("TotalCrime","ViolentCrime",
                                            "NonViolentCrime","PersonalSafetyCrime",
                                            "PropertyCrime","TotalCrime_lag1",
                                            "TotalCrime_lag2")]
print(imp_crime_H4)
# H5
imp_H5 <- ranger_obj_H5$variable.importance
imp_crime_H5 <- imp_H5[names(imp_H5) %in% c("TotalCrime","ViolentCrime",
                                            "NonViolentCrime","PersonalSafetyCrime",
                                            "PropertyCrime","TotalCrime_lag1",
                                            "TotalCrime_lag2")]
print(imp_crime_H5)

# Stratified sampling ensures representation across key ranges
create_smart_sample <- function(data, target_var, n_sample = 2000) {

    # Create quantile-based strata
    data_with_strata <- data %>%
        mutate(
            strata = case_when(
                !!sym(target_var) <= quantile(!!sym(target_var), 0.25) ~ "Q1_Low",
                !!sym(target_var) <= quantile(!!sym(target_var), 0.50) ~ "Q2_Medium",
                !!sym(target_var) <= quantile(!!sym(target_var), 0.75) ~ "Q3_High",
                TRUE ~ "Q4_Very_High"
            )
        )

    # Sample proportionally from each stratum
    smart_sample <- data_with_strata %>%
        group_by(strata) %>%
        slice_sample(n = n_sample / 4) %>%
        ungroup() %>%
        select(-strata)

    return(smart_sample)
}

train_smart <- create_smart_sample(train_rf,
                                   "TotalCrime",
                                   n_sample = 2000)

predict_ranger <- function(object, newdata) {
    predict(object, data = newdata)$predictions
}
# set up parallel
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)


# for up or down direction
pd_rf_H1 <- pdp::partial(
    object   = ranger_obj_H1,    # trained model
    pred.var = "TotalCrime",  # variable of interest
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)

autoplot(pd_rf_H1) +
    labs(
        x = "TotalCrime",
        y = "Predicted Price",
        title = "Partial Dependence: TotalCrime → Price (Random Forest)"
    )

# rename
pd_rf_H1 <- pd_rf_H1 %>%
    rename(
        # TotalCrime = value,  # or rename(value = TotalCrime)
        Predicted  = yhat
    )
# plot with ggplot
ggplot(pd_rf_H1,
       aes(x = TotalCrime,
           y = Predicted)) +
    geom_line() +
    labs(
        x     = "TotalCrime",
        y     = "Predicted Price",
        title = "Partial Dependence: TotalCrime  vs Price (1000m) (Random Forest)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

# H2
pd_ViolentCrime_rf_H2 <- pdp::partial(
    object   = ranger_obj_H2,
    pred.var = "ViolentCrime",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
pd_NonViolentCrime_rf_H2 <- pdp::partial(
    object   = ranger_obj_H2,
    pred.var = "NonViolentCrime",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
# plot
ggplot(pd_ViolentCrime_rf_H2,
       aes(x = ViolentCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Violent Crime vs Price (1000m) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))

ggplot(pd_NonViolentCrime_rf_H2,
       aes(x = NonViolentCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: NonViolent Crime vs Price (1000m) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))

# H3
pd_PersonalSafetyCrime_rf_H3 <- pdp::partial(
    object   = ranger_obj_H3,
    pred.var = "PersonalSafetyCrime",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
pd_PropertyCrime_rf_H3 <- pdp::partial(
    object   = ranger_obj_H3,
    pred.var = "PropertyCrime",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
# plot
ggplot(pd_PersonalSafetyCrime_rf_H3,
       aes(x = PersonalSafetyCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Personal Safety Crime vs Price (1000m) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_PropertyCrime_rf_H3,
       aes(x = PropertyCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Property Crime vs Price (1000m) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
# Add interaction term for partial dependencies
train_smart$`TotalCrime:HighIncome_Regular` <- train_smart$TotalCrime * train_smart$HighIncome_Regular
# H4
pd_rf_H4  <- pdp::partial(
    object   = ranger_obj_H4,
    pred.var = c("TotalCrime","HighIncome_Regular"),
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none",
    # chull    = TRUE        # restricts to observed combos
)
# plot
ggplot(pd_rf_H4,
       aes(x = TotalCrime,
           y = yhat,
           color = as.factor(HighIncome_Regular))) +
    geom_line(size = 1) +
    labs(
        title = "Partial Dependence: TotalCrime × HighIncome_Regular (1000)",
        x = "TotalCrime",
        y = "Predicted Price",
        color = "High Income Propeties"
    ) +
    theme_minimal()


# H5
pd_TotalCrime_rf_H5 <- pdp::partial(
    object   = ranger_obj_H5,
    pred.var = "TotalCrime",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
pd_TotalCrime_lag1_rf_H5 <- pdp::partial(
    object   = ranger_obj_H5,
    pred.var = "TotalCrime_lag1",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
pd_TotalCrime_lag2_rf_H5 <- pdp::partial(
    object   = ranger_obj_H5,
    pred.var = "TotalCrime_lag2",
    train    = train_smart
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
# plot
ggplot(pd_TotalCrime_rf_H5,
       aes(x = TotalCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Total Crime when accounting for lagged crime (1000) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_TotalCrime_lag1_rf_H5,
       aes(x = TotalCrime_lag1, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: TotalCrime_lag1 when accounting for lagged crime  (1000) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_TotalCrime_lag2_rf_H5,
       aes(x = TotalCrime_lag2, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: TotalCrime_lag2 when accounting for lagged crime (1000) (Random Forest)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))


# test on your held-out test DATA
# H1
rf_preds_H1  <- predict(rf_fit_H1, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(rf_preds_H1, truth = Price, estimate = .pred)

# H2
rf_preds_H2  <- predict(rf_fit_H2, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(rf_preds_H2, truth = Price, estimate = .pred)

# H3
rf_preds_H3  <- predict(rf_fit_H3, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(rf_preds_H3, truth = Price, estimate = .pred)

# H4
rf_preds_H4  <- predict(rf_fit_H4, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(rf_preds_H4, truth = Price, estimate = .pred)

# H5
rf_preds_H5  <- predict(rf_fit_H5, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(rf_preds_H5, truth = Price, estimate = .pred)

# results table for Rand forest
model_results <- tibble(
    Hypothesis = paste0("H", 1:5),
    RMSE = c(
        metrics(rf_preds_H1, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(rf_preds_H2, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(rf_preds_H3, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(rf_preds_H4, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(rf_preds_H5, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate)
    ),
    RSQ = c(
        metrics(rf_preds_H1, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(rf_preds_H2, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(rf_preds_H3, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(rf_preds_H4, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(rf_preds_H5, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate)
    ),
    MAE = c(
        metrics(rf_preds_H1, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(rf_preds_H2, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(rf_preds_H3, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(rf_preds_H4, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(rf_preds_H5, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate)
    )
)

print(model_results)
kable(model_results, 
      caption = "Random forest model metrics for Properties joined with crime counts within 3000 meters from Reporting Garda station") %>% 
    kable_styling(full_width = FALSE, position = "center")

# for XGBOOSTED trees
xgb_spec <- boost_tree(
    mode       = "regression",
    trees      = tune(),
    learn_rate = 0.1,
    mtry       = tune()) %>%
    set_engine("xgboost")

# set the boosted tree work flow for each model
xgb_wf_H1 <- workflow() %>%
    add_model(xgb_spec) %>%
    add_formula(model_formula_H1)
# H2
xgb_wf_H2 <- workflow() %>%
    add_model(xgb_spec) %>%
    add_formula(model_formula_H2)
# H3
xgb_wf_H3 <- workflow() %>%
    add_model(xgb_spec) %>%
    add_formula(model_formula_H3)
# H4
xgb_wf_H4 <- workflow() %>%
    add_model(xgb_spec) %>%
    add_formula(model_formula_H4)
# H5
xgb_wf_H5 <- workflow() %>%
    add_model(xgb_spec) %>%
    add_formula(model_formula_H5)

xgb_grid <- grid_regular(trees(range = c(100,1000)),
                         mtry(range = c(5, 20)),
                         levels = 3)

# tuning the trees for each model
xgb_tune_H1 <- tune_grid(
    xgb_wf_H1,
    resamples = val_split,
    grid      = xgb_grid,
    metrics   = metric_set(rmse)
)
# H2
xgb_tune_H2 <- tune_grid(
    xgb_wf_H2,
    resamples = val_split,
    grid      = xgb_grid,
    metrics   = metric_set(rmse)
)
# H3
xgb_tune_H3 <- tune_grid(
    xgb_wf_H3,
    resamples = val_split,
    grid      = xgb_grid,
    metrics   = metric_set(rmse)
)
# H4
xgb_tune_H4 <- tune_grid(
    xgb_wf_H4,
    resamples = val_split,
    grid      = xgb_grid,
    metrics   = metric_set(rmse)
)
# H5
xgb_tune_H5 <- tune_grid(
    xgb_wf_H5,
    resamples = val_split,
    grid      = xgb_grid,
    metrics   = metric_set(rmse)
)

# Choosing the best tree for each XGB model
best_xgb_H1 <- select_best(xgb_tune_H1,
                        metric =  "rmse")
# H2
best_xgb_H2 <- select_best(xgb_tune_H2,
                           metric =  "rmse")
# H3
best_xgb_H3 <- select_best(xgb_tune_H3,
                           metric =  "rmse")
# H4
best_xgb_H4 <- select_best(xgb_tune_H4,
                           metric =  "rmse")
# H5
best_xgb_H5 <- select_best(xgb_tune_H5,
                           metric =  "rmse")

# finalise and fit the models to each hypothesis
final_xgb_H1 <- finalize_workflow(xgb_wf_H1, 
                               best_xgb_H1)
xgb_fit_H1   <- fit(final_xgb_H1, 
                 data = train_rf)
# H2
final_xgb_H2 <- finalize_workflow(xgb_wf_H2, 
                                  best_xgb_H2)
xgb_fit_H2   <- fit(final_xgb_H2, 
                    data = train_rf)
# H3
final_xgb_H3 <- finalize_workflow(xgb_wf_H3, 
                                  best_xgb_H3)
xgb_fit_H3   <- fit(final_xgb_H3, 
                    data = train_rf)
# H4
final_xgb_H4 <- finalize_workflow(xgb_wf_H4, 
                                  best_xgb_H4)
xgb_fit_H4   <- fit(final_xgb_H4, 
                    data = train_rf)
# H5
final_xgb_H5 <- finalize_workflow(xgb_wf_H5, 
                                  best_xgb_H5)
xgb_fit_H5   <- fit(final_xgb_H5, 
                    data = train_rf)



#extract the raw xgboost model
xgb_obj_H1 <- xgb_fit_H1 %>%
    extract_fit_parsnip() %>%
    .$fit
# H2
xgb_obj_H2 <- xgb_fit_H2 %>%
    extract_fit_parsnip() %>%
    .$fit
# H3
xgb_obj_H3 <- xgb_fit_H3 %>%
    extract_fit_parsnip() %>%
    .$fit
# H4
xgb_obj_H4 <- xgb_fit_H4 %>%
    extract_fit_parsnip() %>%
    .$fit
# H5
xgb_obj_H5 <- xgb_fit_H5 %>%
    extract_fit_parsnip() %>%
    .$fit


imp_full_H1 <- xgb.importance(model = xgb_obj_H1)
imp_crime_H1 <- imp_full_H1 %>%
    filter(Feature %in% c("TotalCrime","ViolentCrime",
                          "NonViolentCrime","PersonalSafetyCrime",
                          "PropertyCrime","TotalCrime_lag1",
                          "TotalCrime_lag2"))
print(imp_crime_H1)
# H2
imp_full_H2 <- xgb.importance(model = xgb_obj_H2)
imp_crime_H2 <- imp_full_H2 %>%
    filter(Feature %in% c("TotalCrime","ViolentCrime",
                          "NonViolentCrime","PersonalSafetyCrime",
                          "PropertyCrime","TotalCrime_lag1",
                          "TotalCrime_lag2"))
print(imp_crime_H2)
# H3
imp_full_H3 <- xgb.importance(model = xgb_obj_H3)
imp_crime_H3 <- imp_full_H3 %>%
    filter(Feature %in% c("TotalCrime","ViolentCrime",
                          "NonViolentCrime","PersonalSafetyCrime",
                          "PropertyCrime","TotalCrime_lag1",
                          "TotalCrime_lag2"))
print(imp_crime_H3)
# H4
imp_full_H4 <- xgb.importance(model = xgb_obj_H4)
imp_crime_H4 <- imp_full_H4 %>%
    filter(Feature %in% c("TotalCrime","ViolentCrime",
                          "NonViolentCrime","PersonalSafetyCrime",
                          "PropertyCrime","TotalCrime_lag1",
                          "TotalCrime_lag2"))
print(imp_crime_H4)
# H5
imp_full_H5 <- xgb.importance(model = xgb_obj_H5)
imp_crime_H5 <- imp_full_H5 %>%
    filter(Feature %in% c("TotalCrime","ViolentCrime",
                          "NonViolentCrime","PersonalSafetyCrime",
                          "PropertyCrime","TotalCrime_lag1",
                          "TotalCrime_lag2"))
print(imp_crime_H5)

pd_total_H1 <- pdp::partial(
    object   = xgb_obj_H1,       # model
    pred.var = "TotalCrime",  # variable to plot
    train    = train_smart[, xgb_obj_H1$feature_names]
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
    )       #  preprocessed training data
# H2
pd_violent_xgb_H2 <- pdp::partial(
    object   = xgb_obj_H2,
    pred.var = "ViolentCrime",
    train    = train_smart[, xgb_obj_H2$feature_names],
    grid.resolution = 30,
    progress = "none"
)

pd_nonviolent_xgb_H2 <- pdp::partial(
    object   = xgb_obj_H2,
    pred.var = "NonViolentCrime",
    train    = train_smart[, xgb_obj_H2$feature_names]
    # grid.resolution = 30,
    # progress = "none"
)
# H3
pd_PersonalSafetyCrime_xgb_H3 <- pdp::partial(
    object   = xgb_obj_H3,       # model
    pred.var = "PersonalSafetyCrime", # variable to plot
    train    = train_smart[, xgb_obj_H3$feature_names]
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
    )
pd_PropertyCrime_xgb_H3 <- pdp::partial(
    object   = xgb_obj_H3,       # model
    pred.var = "PropertyCrime",  # variable to plot
    train    = train_smart[, xgb_obj_H3$feature_names]
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
# Add interaction term for partial dependencies
train_smart$`TotalCrime:HighIncome_Regular` <- train_smart$TotalCrime * train_smart$HighIncome_Regular
# H4
pd_total_H4 <- pdp::partial(
    object   = xgb_obj_H4,       # model
    pred.var = c("TotalCrime","HighIncome_Regular"),  # variable to plot
    train    = train_smart[, xgb_obj_H4$feature_names],
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none",
    chull    = TRUE)
# H5
pd_TotalCrime_xgb_H5 <- pdp::partial(
    object   = xgb_obj_H5,       # model
    pred.var = "TotalCrime", # variable to plot
    train    = train_smart[, xgb_obj_H5$feature_names]
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
    )
pd_TotalCrime_lag1_xgb_H5 <- pdp::partial(
    object   = xgb_obj_H5,       # model
    pred.var = "TotalCrime_lag1",# variable to plot
    train    = train_smart[, xgb_obj_H5$feature_names]
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)
pd_TotalCrime_lag2_xgb_H5 <- pdp::partial(
    object   = xgb_obj_H5,       # model
    pred.var = "TotalCrime_lag2",  # variable to plot
    train    = train_smart[, xgb_obj_H5$feature_names]
    # pred.fun = predict_ranger,
    # parallel = FALSE,
    # grid.resolution = 30,
    # progress       = "none"
)


#plot
# plot with ggplot
ggplot(pd_total_H1,
       aes(x = TotalCrime,
           y = yhat)) +
    geom_line() +
    labs(
        x     = "TotalCrime",
        y     = "Predicted Price",
        title = "Partial Dependence: TotalCrime → Price (XGBOOSTED Trees)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
# # H2
ggplot(pd_violent_xgb_H2,
       aes(x = ViolentCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Violent Crime (XGBoost)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_nonviolent_xgb_H2,
       aes(x = NonViolentCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: NonViolent Crime (xgboosted trees)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
# # H3
ggplot(pd_PersonalSafetyCrime_xgb_H3,
       aes(x = PersonalSafetyCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Personal Safety Crime (xgboosted trees)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_PropertyCrime_xgb_H3,
       aes(x = PropertyCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Property Crime (xgboosted trees)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
# # H4
ggplot(pd_total_H4,
       aes(x = TotalCrime,
           y = yhat,
           color = as.factor(HighIncome_Regular))) +
    geom_line(size = 1) +
    labs(
        title = "Partial Dependence: TotalCrime × HighIncome_Regular",
        x = "TotalCrime",
        y = "Predicted Price",
        color = "High Income Area"
    ) +
    theme_minimal()
# # H5
ggplot(pd_TotalCrime_xgb_H5,
       aes(x = TotalCrime, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: Total Crime when accounting for lagged crime (1000) (xgboosted trees)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_TotalCrime_lag1_xgb_H5,
       aes(x = TotalCrime_lag1, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: TotalCrime_lag1 when accounting for lagged crime (1000) (xgboosted trees)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
ggplot(pd_TotalCrime_lag2_xgb_H5,
       aes(x = TotalCrime_lag2, 
           y = yhat)) +
    geom_line() +
    labs(
        x     = "Crime Count",
        y     = "Predicted Price",
        title = "Partial Dependence: TotalCrime_lag2 when accounting for lagged crime (1000) (xgboosted trees)"
    ) +
    theme(plot.title = element_text(hjust = 0.5))

stopCluster(cl)

# test on your held-out test_clean
# H1
xgb_preds_H1  <- predict(xgb_fit_H1, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(xgb_preds_H1, truth = Price, estimate = .pred)

# H2
xgb_preds_H2  <- predict(xgb_fit_H2, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(xgb_preds_H2, truth = Price, estimate = .pred)

# H3
xgb_preds_H3  <- predict(xgb_fit_H3, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(xgb_preds_H3, truth = Price, estimate = .pred)

# H4
xgb_preds_H4  <- predict(xgb_fit_H4, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(xgb_preds_H4, truth = Price, estimate = .pred)

# H5
xgb_preds_H5  <- predict(xgb_fit_H5, new_data = test_rf) %>%
    bind_cols(test_rf)
metrics(xgb_preds_H5, truth = Price, estimate = .pred)

# results table for XGBOOSTED TREES
model_results_xgb <- tibble(
    Hypothesis = paste0("H", 1:5),
    RMSE = c(
        metrics(xgb_preds_H1, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(xgb_preds_H2, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(xgb_preds_H3, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(xgb_preds_H4, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate),
        metrics(xgb_preds_H5, truth = Price, estimate = .pred) %>%
            filter(.metric == "rmse") %>% pull(.estimate)
    ),
    RSQ = c(
        metrics(xgb_preds_H1, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(xgb_preds_H2, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(xgb_preds_H3, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(xgb_preds_H4, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate),
        metrics(xgb_preds_H5, truth = Price, estimate = .pred) %>%
            filter(.metric == "rsq") %>% pull(.estimate)
    ),
    MAE = c(
        metrics(xgb_preds_H1, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(xgb_preds_H2, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(xgb_preds_H3, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(xgb_preds_H4, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate),
        metrics(xgb_preds_H5, truth = Price, estimate = .pred) %>%
            filter(.metric == "mae") %>% pull(.estimate)
    )
)

print(model_results_xgb)
kable(model_results_xgb, 
      caption = "XGBoosted Trees model metrics for Properties joined with crime counts within 3000 meters from Reporting Garda station") %>% 
    kable_styling(full_width = FALSE, position = "center")



