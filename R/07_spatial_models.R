# Spatial econometric models
# ==========================
# Moran's I on OLS residuals, then spatial lag, spatial error and spatial Durbin models; GWR/MGWR experiments.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: output of 03

# spatial model accounting for spatial auto_correlation -----------------------------------------------------------
# post recipes
model_formula <- Price ~
    TotalCrime + constr_index +
    int_rates +
    # latitude +
    # longitude +
    # dist_to_bua_m +
    dist_to_com_air_m +
    dist_to_hos_m +
    dist_to_trainstaion +
    Pop_est +
    dist_to_Garda_Station_m +
    # House_stk +
    # staion_count +
    # Pop_est_x_House_stk +
    property_type_Second.Hand +
    County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin +
    County_Galway + County_Kerry + County_Kildare + County_Kilkenny + County_Laois +
    County_Leitrim + County_Limerick + County_Longford + County_Louth + County_Mayo +
    County_Meath + County_Monaghan + County_Offaly + County_Roscommon + County_Sligo +
    County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
    unit_type_House +
    Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 +
    Year_X2016 + Year_X2017 + Year_X2018 + Year_X2019 + Year_X2020 +
    Year_X2021 + Year_X2022

# Define which variables you want PCA applied to
control_vars <- c("constr_index", "int_rates", 
                  "Pop_est", "House_stk", 
                  "Pop_density", "housing_density", 
                  "median_income", 
                  # "staion_count",
                  "dist_to_bua_m", "dist_to_trainstaion",
                  "dist_to_airport", "dist_to_hos_m",
                  "dist_to_com_air_m","dist_to_Garda_Station_m")

rec_pca <- recipe(Price ~.,
              data = crime_500_train_data) %>% 
    
    # step_log(Price, base = 10) %>%
    step_log(dist_to_bua_m, dist_to_com_air_m, dist_to_hos_m, dist_to_airport,
             dist_to_trainstaion, dist_to_Garda_Station_m, offset = 1, base = 10) %>%
    step_mutate(Year = factor(Year)) %>%
    step_rm(Date, prop_id,`Size m²`) %>%  
    step_zv(all_predictors()) %>%  # removes columns with 0 variance
    step_nzv(all_predictors()) %>% 
    step_normalize(all_numeric_predictors(), 
                   -all_of(c("latitude", "longitude")))%>%
    # step_interact(terms = ~ Pop_est:House_stk) %>%
    step_dummy(all_nominal_predictors()) %>%  
    step_pca(all_of(control_vars), threshold = 0.95)

sp_model_data <- prep(rec) %>%
    bake(new_data = crime_500_train_data)

# set up spatial weights
coords <- cbind(
    jitter(sp_model_data$longitude, amount = 1e-6),
    jitter(sp_model_data$latitude, amount = 1e-6)
)
#
knn_nb <- knn2nb(knearneigh(coords, k = 4))
sym_nb <- make.sym.nb(knn_nb)

# Create inverse distance weights
distances <- nbdists(sym_nb, coords)
inv_dist <- lapply(distances, function(x) 1 / x)

# Create listw object with weights
listw <- nb2listw(sym_nb, glist = inv_dist, style = "W", zero.policy = TRUE)
# listw <- nb2listw(dthreshold, style = "W", zero.policy = TRUE)

# check for auto correlation in OLS
lm_model_H1_residuals <- residuals(lm_model_H1)

moran.test(lm_model_H1_residuals, listw)
lm.LMtests(lm_model_H1, listw, test = "all")
# 2
lm_model_H2_residuals <- residuals(lm_model_H2)

moran.test(lm_model_H2_residuals, listw)
lm.LMtests(lm_model_H2, listw, test = "all")
# 3
lm_model_H3_residuals <- residuals(lm_model_H3)

moran.test(lm_model_H3_residuals, listw)
lm.LMtests(lm_model_H3, listw, test = "all")
# 4
lm_model_H4_residuals <- residuals(lm_model_H4)

moran.test(lm_model_H4_residuals, listw)
lm.LMtests(lm_model_H4, listw, test = "all")
# 5
lm_model_H5_residuals <- residuals(lm_model_H5)

moran.test(lm_model_H5_residuals, listw)
lm.LMtests(lm_model_H5, listw, test = "all")

# Helper function to extract test results
get_spatial_tests <- function(model, listw) {
    resids <- residuals(model)
    
    # Moran's I
    moran <- moran.test(resids, listw, zero.policy = TRUE)
    
    # LM tests
    lm_tests <- lm.LMtests(model, listw, test = "all", zero.policy = TRUE)
    
    tibble(
        Moran_I = moran$estimate[["Moran I statistic"]],
        Moran_p = moran$p.value,
        
        LM_Lag = lm_tests$LMlag$statistic,
        LM_Lag_p = lm_tests$LMlag$p.value,
        
        LM_Error = lm_tests$LMerr$statistic,
        LM_Error_p = lm_tests$LMerr$p.value,
        
        Robust_Lag = lm_tests$RLMlag$statistic,
        Robust_Lag_p = lm_tests$RLMlag$p.value,
        
        Robust_Error = lm_tests$RLMerr$statistic,
        Robust_Error_p = lm_tests$RLMerr$p.value
    )
}

# Apply to all models
spatial_test_results <- bind_rows(
    get_spatial_tests(lm_model_H1, listw),
    get_spatial_tests(lm_model_H2, listw),
    get_spatial_tests(lm_model_H3, listw),
    get_spatial_tests(lm_model_H4, listw),
    get_spatial_tests(lm_model_H5, listw)
) %>%
    mutate(Hypothesis = paste0("H", 1:5), .before = 1)

kable(spatial_test_results, 
      caption = "Spatial Autocorrelation in OLS") %>% 
    kable_styling(full_width = FALSE, position = "center")


# Fit spatial lag model with Matrix method (faster, less memory)
slm_H1 <- lagsarlm(model_formula_H1,
                data = sp_model_data,
                listw = listw,
                method = "Matrix",
                zero.policy = TRUE)

summary(slm_H1)
# H2
slm_H2 <- lagsarlm(model_formula_H2,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   zero.policy = TRUE)

summary(slm_H2)
# H3
slm_H3 <- lagsarlm(model_formula_H3,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   zero.policy = TRUE)

summary(slm_H3)
# H4
slm_H4 <- lagsarlm(model_formula_H4,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   zero.policy = TRUE)

summary(slm_H4)
# H5
slm_H5 <- lagsarlm(model_formula_H5,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   zero.policy = TRUE)

summary(slm_H5)
# Summary SLM
modelsummary(
    list("H1: TotalCrime"                 = slm_H1,
         "H2: Violent vs NonViolent"      = slm_H2,
         "H3: PersonalSafety vs Property" = slm_H3,
         "H4: HighIncome*TotalCrime"      = slm_H4,
         "H5: Crime Lags"                 = slm_H5),
    statistic = "({std.error})",  # shows std errors in parentheses
    stars = TRUE,
    output = "html"
)
# Function to test residuals
check_slm_residuals <- function(model, listw) {
    resid <- residuals(model)
    moran.test(resid, listw, zero.policy = TRUE)
}

# Run for all models
check_slm_residuals(slm_H1, listw)
check_slm_residuals(slm_H2, listw)
check_slm_residuals(slm_H3, listw)
check_slm_residuals(slm_H4, listw)
check_slm_residuals(slm_H5, listw)

# Spatial Error Model (SEM)
sem_H1 <- errorsarlm(model_formula_H1,
                  data = sp_model_data,
                  listw = listw,
                  method = "Matrix",
                  zero.policy = TRUE)

summary(sem_H1)
# H2
sem_H2 <- errorsarlm(model_formula_H2,
                     data = sp_model_data,
                     listw = listw,
                     method = "Matrix",
                     zero.policy = TRUE)

summary(sem_H2)
# H3
sem_H3 <- errorsarlm(model_formula_H3,
                     data = sp_model_data,
                     listw = listw,
                     method = "Matrix",
                     zero.policy = TRUE)

summary(sem_H3)
# H4
sem_H4 <- errorsarlm(model_formula_H4,
                     data = sp_model_data,
                     listw = listw,
                     method = "Matrix",
                     zero.policy = TRUE)

summary(sem_H4)
# H5
sem_H5 <- errorsarlm(model_formula_H5,
                     data = sp_model_data,
                     listw = listw,
                     method = "Matrix",
                     zero.policy = TRUE)

summary(sem_H5)
# model summary SEM
modelsummary(
    list("H1: TotalCrime"                 = sem_H1,
         "H2: Violent vs NonViolent"      = sem_H2,
         "H3: PersonalSafety vs Property" = sem_H3,
         "H4: HighIncome*TotalCrime"      = sem_H4,
         "H5: Crime Lags"                 = sem_H5),
    statistic = "({std.error})",  # shows std errors in parentheses
    stars = TRUE,
    output = "html"
)



# sp_model_data_pca <- prep(rec_pca) %>%
#     bake(new_data = crime_500_train_data)
# colnames(sp_model_data_pca)
# 
# coords <- cbind(
#     jitter(sp_model_data_pca$longitude, amount = 1e-6),
#     jitter(sp_model_data_pca$latitude, amount = 1e-6)
# )
# #
# knn_nb <- knn2nb(knearneigh(coords, k = 4))
# sym_nb <- make.sym.nb(knn_nb)
# 
# # Create inverse distance weights
# distances <- nbdists(sym_nb, coords)
# inv_dist <- lapply(distances, function(x) 1 / x)
# 
# # Create listw object with weights
# listw <- nb2listw(sym_nb, glist = inv_dist, style = "W", zero.policy = TRUE)
# # listw <- nb2listw(dthreshold, style = "W", zero.policy = TRUE)
# 
# car::vif(lm(model_formula_H1_gwr, data = sp_model_data_pca))

# spatial durbin model
sdm_H1 <- lagsarlm(formula = Price ~ TotalCrime +
                       # Date + 
                       int_rates + 
                       # dist_to_bua_m + 
                       # dist_to_trainstaion + dist_to_airport +
                       dist_to_hos_m + dist_to_com_air_m +
                       # Pop_est +
                       # # House_stk +
                       # dist_to_Garda_Station_m + 
                       latitude + longitude +
                       station_count +
                       # prop_id + 
                       # `Attempts/threats to murder, assaults, harassments and related offences` +
                       # `Dangerous or negligent acts` + `Kidnapping and related offences` +
                       # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
                       # `Theft and related offences` + `Fraud, deception and related offences` +
                       # `Controlled drug offences` + `Weapons and Explosives Offences` +
                       # `Damage to property and to the environment` + `Public order and other social code offences` +
                       # `Offences against government, justice procedures and organisation of crime` + 
                       # County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
                       # County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
                       # County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
                       # County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
                       # County_Wicklow +
                       Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
                       Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 + 
                       property_type_Second.Hand + unit_type_House + HighIncome_Regular + 
                       Pop_density +
                       # `Size m²`+
                       # housing_density +
                       median_income ,
                data = sp_model_data,
                listw = listw,
                method = "Matrix",
                Durbin = ~ TotalCrime)

summary(sdm_H1)
summary(sdm_H1)$aliased
# H2
sdm_H2 <- lagsarlm(formula = Price ~ ViolentCrime + NonViolentCrime +
                       # Date +
                       int_rates + 
                       # dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
                       dist_to_hos_m + dist_to_com_air_m + 
                       # Pop_est + 
                       # House_stk + 
                       # dist_to_Garda_Station_m + 
                       latitude + longitude + 
                       station_count +
                       # prop_id + 
                       # `Attempts/threats to murder, assaults, harassments and related offences` +
                       # `Dangerous or negligent acts` + `Kidnapping and related offences` +
                       # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
                       # `Theft and related offences` + `Fraud, deception and related offences` +
                       # `Controlled drug offences` + `Weapons and Explosives Offences` +
                       # `Damage to property and to the environment` + `Public order and other social code offences` +
                       # `Offences against government, justice procedures and organisation of crime` + 
                       # County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
                       # County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
                       # County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
                       # County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
                       # County_Wicklow +
                       Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
                       Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 + 
                       property_type_Second.Hand + unit_type_House + HighIncome_Regular   + Pop_density   +
                       # `Size m²` + 
                       # housing_density + 
                       median_income ,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   Durbin = ~ ViolentCrime)# + NonViolentCrime)

summary(sdm_H2)
summary(sdm_H2)$aliased
# H3
sdm_H3 <- lagsarlm(formula = Price ~ PersonalSafetyCrime + PropertyCrime +
                       # Date + 
                       int_rates + 
                       # dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
                       dist_to_hos_m + 
                       # dist_to_com_air_m +
                       # Pop_est + 
                       # House_stk + 
                       # dist_to_Garda_Station_m + 
                       latitude + longitude + 
                       station_count +
                       # prop_id + 
                       # `Attempts/threats to murder, assaults, harassments and related offences` +
                       # `Dangerous or negligent acts` + `Kidnapping and related offences` +
                       # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
                       # `Theft and related offences` + `Fraud, deception and related offences` +
                       # `Controlled drug offences` + `Weapons and Explosives Offences` +
                       # `Damage to property and to the environment` + `Public order and other social code offences` +
                       # `Offences against government, justice procedures and organisation of crime` + 
                       # County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
                       # County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
                       # County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
                       # County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
                       # County_Wicklow +
                       Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
                       Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 + 
                       property_type_Second.Hand + unit_type_House + 
                       # HighIncome_Regular   + 
                       Pop_density   +
                       # `Size m²` + 
                       # housing_density + 
                       median_income ,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   Durbin = ~ PropertyCrime)# + )

summary(sdm_H3)
summary(sdm_H3)$aliased
# H4
sdm_H4 <- lagsarlm(formula = Price ~ TotalCrime*HighIncome_Regular + 
                       # Date + 
                       int_rates + 
                       # dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
                       # dist_to_hos_m + dist_to_com_air_m + #Pop_est + 
                       # House_stk + 
                       # dist_to_Garda_Station_m + 
                       latitude + longitude + 
                       station_count +
                       # prop_id + 
                       # `Attempts/threats to murder, assaults, harassments and related offences` +
                       # `Dangerous or negligent acts` + `Kidnapping and related offences` +
                       # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
                       # `Theft and related offences` + `Fraud, deception and related offences` +
                       # `Controlled drug offences` + `Weapons and Explosives Offences` +
                       # `Damage to property and to the environment` + `Public order and other social code offences` +
                       # `Offences against government, justice procedures and organisation of crime` + 
                       # County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
                       # County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
                       # County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
                       # County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
                       # County_Wicklow +
                       Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
                       Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
                       property_type_Second.Hand + unit_type_House    + Pop_density   + 
                       # `Size m²` +
                       # housing_density + 
                       median_income ,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   Durbin = ~ TotalCrime*HighIncome_Regular)

summary(sdm_H4)
summary(sdm_H4)$aliased
# H5
sdm_H5 <- lagsarlm(formula = Price ~ TotalCrime + TotalCrime_lag1 + 
                       TotalCrime_lag2 +
                       # Date + 
                       int_rates + 
                       # dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
                       # dist_to_hos_m + dist_to_com_air_m + 
                       # Pop_est + 
                       # House_stk + 
                       # dist_to_Garda_Station_m + 
                       latitude + longitude + 
                       station_count +
                       # prop_id + 
                       # `Attempts/threats to murder, assaults, harassments and related offences` +
                       # `Dangerous or negligent acts` + `Kidnapping and related offences` +
                       # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
                       # `Theft and related offences` + `Fraud, deception and related offences` +
                       # `Controlled drug offences` + `Weapons and Explosives Offences` +
                       # `Damage to property and to the environment` + `Public order and other social code offences` +
                       # `Offences against government, justice procedures and organisation of crime` + 
                       # County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
                       # County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
                       # County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
                       # County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
                       # County_Wicklow +
                       Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
                       Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
                       property_type_Second.Hand + unit_type_House + HighIncome_Regular + 
                       Pop_density   + 
                       # `Size m²` + 
                       # housing_density + 
                       median_income ,
                   data = sp_model_data,
                   listw = listw,
                   method = "Matrix",
                   Durbin =  ~ #TotalCrime + 
                       TotalCrime_lag1 + 
                       TotalCrime_lag2)

summary(sdm_H5)
summary(sdm_H5)$aliased
modelsummary(
    list("H1: TotalCrime"                 = sdm_H1,
         "H2: Violent vs NonViolent"      = sdm_H2,
         "H3: PersonalSafety vs Property" = sdm_H3,
         "H4: HighIncome*TotalCrime"      = sdm_H4,
         "H5: Crime Lags"                 = sdm_H5),
    statistic = "({std.error})",  # shows std errors in parentheses
    stars = TRUE,
    output = "html"
)


# spatial model test ------------------------------------------------------

sp_model_data_test <- prep(rec) %>%
    bake(new_data = crime_500_test_data)

# set up spatial weights
coords_test <- cbind(
    jitter(sp_model_data_test$longitude, amount = 1e-6),
    jitter(sp_model_data_test$latitude, amount = 1e-6)
)
#
knn_nb_test <- knn2nb(knearneigh(coords_test, k = 4))
sym_nb_test <- make.sym.nb(knn_nb_test)

rownames(sp_model_data_test) <- paste0("test_", seq_len(nrow(sp_model_data_test)))

# Create inverse distance weights
distances_test <- nbdists(sym_nb_test, coords_test)
inv_dist_test <- lapply(distances_test, function(x) 1 / x)

attr(sym_nb_test, "region.id") <- rownames(sp_model_data_test)

# Create listw object with weights
listw_test <- nb2listw(sym_nb_test, 
                       glist = inv_dist_test, 
                       style = "W", zero.policy = TRUE)


# Predict using the Spatial Lag Model (SLM)
pred_slm_H1 <- predict(slm_H1,
                    newdata = sp_model_data_test,
                    listw = listw_test,
                    zero.policy = TRUE)

# Compare to actual
results_H1 <- sp_model_data_test %>%
    mutate(
        pred_log_price = pred_slm_H1,
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_H1, truth = actual_log_price, estimate = pred_log_price)
# H2
# Predict using the Spatial Lag Model (SLM)
pred_slm_H2 <- predict(slm_H2,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_H2 <- sp_model_data_test %>%
    mutate(
        pred_log_price = pred_slm_H2,
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_H2, truth = actual_log_price, estimate = pred_log_price)
# H3
# Predict using the Spatial Lag Model (SLM)
pred_slm_H3 <- predict(slm_H3,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_H3 <- sp_model_data_test %>%
    mutate(
        pred_log_price = pred_slm_H3,
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_H3, truth = actual_log_price, estimate = pred_log_price)
# H4
# Predict using the Spatial Lag Model (SLM)
pred_slm_H4 <- predict(slm_H4,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_H4 <- sp_model_data_test %>%
    mutate(
        pred_log_price = pred_slm_H4,
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_H4, truth = actual_log_price, estimate = pred_log_price)
# H5
# Predict using the Spatial Lag Model (SLM)
pred_slm_H5 <- predict(slm_H5,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_H5 <- sp_model_data_test %>%
    mutate(
        pred_log_price = pred_slm_H5,
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_H5, truth = actual_log_price, estimate = pred_log_price)

# Collect metrics for each SDM model
metrics_slm_H1 <- metrics(results_H1, truth = actual_log_price, estimate = pred_log_price)
metrics_slm_H2 <- metrics(results_H2, truth = actual_log_price, estimate = pred_log_price)
metrics_slm_H3 <- metrics(results_H3, truth = actual_log_price, estimate = pred_log_price)
metrics_slm_H4 <- metrics(results_H4, truth = actual_log_price, estimate = pred_log_price)
metrics_slm_H5 <- metrics(results_H5, truth = actual_log_price, estimate = pred_log_price)

# Combine them into one table
model_results_slm <- tibble(
    Hypothesis = paste0("H", 1:5),
    RMSE = c(
        metrics_slm_H1 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_slm_H2 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_slm_H3 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_slm_H4 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_slm_H5 %>% filter(.metric == "rmse") %>% pull(.estimate)
    ),
    RSQ = c(
        metrics_slm_H1 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_slm_H2 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_slm_H3 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_slm_H4 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_slm_H5 %>% filter(.metric == "rsq") %>% pull(.estimate)
    ),
    MAE = c(
        metrics_slm_H1 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_slm_H2 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_slm_H3 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_slm_H4 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_slm_H5 %>% filter(.metric == "mae") %>% pull(.estimate)
    )
)

# View table
print(model_results_slm)
kable(model_results_slm, 
      caption = "Spatial lag model metrics for Properties joined with crime counts within 3000 meters from Reporting Garda station") %>% 
    kable_styling(full_width = FALSE, position = "center")

# predict using spatial ,error model
pred_sem_H1 <- predict(sem_H1,
                    newdata = sp_model_data_test,
                    listw = listw_test,
                    zero.policy = TRUE)

# Compare to actual
results_sem_H1 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sem_H1),
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_sem_H1, truth = actual_log_price, estimate = pred_log_price)
# H2
pred_sem_H2 <- predict(sem_H2,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_sem_H2 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sem_H2),
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_sem_H2, truth = actual_log_price, estimate = pred_log_price)
# H3
pred_sem_H3 <- predict(sem_H3,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_sem_H3 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sem_H3),
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_sem_H3, truth = actual_log_price, estimate = pred_log_price)
# H4
pred_sem_H4 <- predict(sem_H4,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_sem_H4 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sem_H4),
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_sem_H4, truth = actual_log_price, estimate = pred_log_price)
# H5
pred_sem_H5 <- predict(sem_H5,
                       newdata = sp_model_data_test,
                       listw = listw_test,
                       zero.policy = TRUE)

# Compare to actual
results_sem_H5 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sem_H5),
        actual_log_price = Price
    )
# evaluate accuracy
metrics(results_sem_H5, truth = actual_log_price, estimate = pred_log_price)

# Collect metrics for each SEM model
metrics_sem_H1 <- metrics(results_sem_H1, truth = actual_log_price, estimate = pred_log_price)
metrics_sem_H2 <- metrics(results_sem_H2, truth = actual_log_price, estimate = pred_log_price)
metrics_sem_H3 <- metrics(results_sem_H3, truth = actual_log_price, estimate = pred_log_price)
metrics_sem_H4 <- metrics(results_sem_H4, truth = actual_log_price, estimate = pred_log_price)
metrics_sem_H5 <- metrics(results_sem_H5, truth = actual_log_price, estimate = pred_log_price)

# Combine them into one table
model_results_sem <- tibble(
    Hypothesis = paste0("H", 1:5),
    RMSE = c(
        metrics_sem_H1 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sem_H2 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sem_H3 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sem_H4 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sem_H5 %>% filter(.metric == "rmse") %>% pull(.estimate)
    ),
    RSQ = c(
        metrics_sem_H1 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sem_H2 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sem_H3 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sem_H4 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sem_H5 %>% filter(.metric == "rsq") %>% pull(.estimate)
    ),
    MAE = c(
        metrics_sem_H1 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sem_H2 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sem_H3 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sem_H4 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sem_H5 %>% filter(.metric == "mae") %>% pull(.estimate)
    )
)

# View table
print(model_results_sem)
kable(model_results_sem, 
      caption = "Spatial Error model metrics for Properties joined with crime counts within 3000 meters from Reporting Garda station") %>% 
    kable_styling(full_width = FALSE, position = "center")


# spatial durbin model
create_robust_sdm_prediction <- function(model, test_data, listw_test) {
    
    # Get all model information
    coefs <- coefficients(model)
    coef_names <- names(coefs)
    
    # Separate coefficient types
    direct_mask <- !grepl("^lag\\.|^lambda|^rho", coef_names)
    spatial_mask <- grepl("^lag\\.", coef_names)
    
    direct_coefs <- coefs[direct_mask]
    spatial_coefs <- coefs[spatial_mask]
    
    # Handle intercept separately if present
    has_intercept <- "(Intercept)" %in% names(direct_coefs)
    intercept_value <- ifelse(has_intercept, direct_coefs["(Intercept)"], 0)
    
    # Remove intercept from direct coefficients for matrix multiplication
    if(has_intercept) {
        direct_coefs <- direct_coefs[names(direct_coefs) != "(Intercept)"]
    }
    
    # Initialize prediction with intercept
    n_obs <- nrow(test_data)
    predictions <- rep(intercept_value, n_obs)
    
    # Add direct effects
    for(var_name in names(direct_coefs)) {
        if(var_name %in% names(test_data)) {
            predictions <- predictions + test_data[[var_name]] * direct_coefs[var_name]
        } else {
            warning(paste("Variable", var_name, "not found in test data"))
        }
    }
    
    # Add spatial lag effects
    spatial_var_names <- gsub("^lag\\.", "", names(spatial_coefs))
    for(i in 1:length(spatial_var_names)) {
        var_name <- spatial_var_names[i]
        coef_name <- names(spatial_coefs)[i]
        
        if(var_name %in% names(test_data)) {
            lag_var <- lag.listw(listw_test, test_data[[var_name]], zero.policy = TRUE)
            predictions <- predictions + lag_var * spatial_coefs[coef_name]
        } else {
            warning(paste("Spatial variable", var_name, "not found in test data"))
        }
    }
    
    return(predictions)
}



# Predict log prices using the SDM model
pred_sdm_H1 <- create_robust_sdm_prediction(sdm_H1, sp_model_data_test, listw_test)

# Combine with actual values
results_sdm_H1 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sdm_H1),
        actual_log_price = Price
    )

# Evaluate prediction accuracy
metrics(results_sdm_H1, truth = actual_log_price, estimate = pred_log_price)
# H2
# Predict log prices using the SDM model
pred_sdm_H2 <- create_robust_sdm_prediction(sdm_H2, sp_model_data_test, listw_test)

# Combine with actual values
results_sdm_H2 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sdm_H2),
        actual_log_price = Price
    )

# Evaluate prediction accuracy
metrics(results_sdm_H2, truth = actual_log_price, estimate = pred_log_price)
# H3
# Predict log prices using the SDM model
pred_sdm_H3 <-  create_robust_sdm_prediction(sdm_H3, sp_model_data_test, listw_test)

# Combine with actual values
results_sdm_H3 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sdm_H3),
        actual_log_price = Price
    )

# Evaluate prediction accuracy
metrics(results_sdm_H3, truth = actual_log_price, estimate = pred_log_price)
# H4
# Predict log prices using the SDM model
pred_sdm_H4 <- create_robust_sdm_prediction(sdm_H4, sp_model_data_test, listw_test)

# Combine with actual values
results_sdm_H4 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sdm_H4),
        actual_log_price = Price
    )

# Evaluate prediction accuracy
metrics(results_sdm_H4, truth = actual_log_price, estimate = pred_log_price)
# H5
# Predict log prices using the SDM model
pred_sdm_H5 <- create_robust_sdm_prediction(sdm_H5, sp_model_data_test, listw_test)

# Combine with actual values
results_sdm_H5 <- sp_model_data_test %>%
    mutate(
        pred_log_price = as.numeric(pred_sdm_H5),
        actual_log_price = Price
    )

# Evaluate prediction accuracy
metrics(results_sdm_H5, truth = actual_log_price, estimate = pred_log_price)

# Collect metrics for each SDM model
metrics_sdm_H1 <- metrics(results_sdm_H1, truth = actual_log_price, estimate = pred_log_price)
metrics_sdm_H2 <- metrics(results_sdm_H2, truth = actual_log_price, estimate = pred_log_price)
metrics_sdm_H3 <- metrics(results_sdm_H3, truth = actual_log_price, estimate = pred_log_price)
metrics_sdm_H4 <- metrics(results_sdm_H4, truth = actual_log_price, estimate = pred_log_price)
metrics_sdm_H5 <- metrics(results_sdm_H5, truth = actual_log_price, estimate = pred_log_price)

# Combine them into one table
model_results_sdm <- tibble(
    Hypothesis = paste0("H", 1:5),
    RMSE = c(
        metrics_sdm_H1 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sdm_H2 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sdm_H3 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sdm_H4 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_sdm_H5 %>% filter(.metric == "rmse") %>% pull(.estimate)
    ),
    RSQ = c(
        metrics_sdm_H1 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sdm_H2 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sdm_H3 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sdm_H4 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_sdm_H5 %>% filter(.metric == "rsq") %>% pull(.estimate)
    ),
    MAE = c(
        metrics_sdm_H1 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sdm_H2 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sdm_H3 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sdm_H4 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_sdm_H5 %>% filter(.metric == "mae") %>% pull(.estimate)
    )
)

# View table
print(model_results_sdm)
kable(model_results_sdm, 
      caption = "Spatial Durbin model metrics for Properties joined with crime counts within 3000 meters from Reporting Garda station") %>% 
    kable_styling(full_width = FALSE, position = "center")

# # Geographically Weighted Regression (GWR) and MGWR -----------------------
# 
# gwr_model_data <- prep(rec_pca) %>%
#     bake(new_data = crime_500_train_data)
# # str(gwr_model_data)
# # CONVERT to spatial points
# coordinates(gwr_model_data) <- ~longitude+latitude
# # Bandwidth selection for GWR (by AICc)
# 
# # model formular after recipes
# model_formula_gwr <- Price ~ TotalCrime + constr_index + int_rates + dist_to_bua_m +
#     dist_to_com_air_m + dist_to_hos_m + dist_to_trainstaion +
#     Pop_est + dist_to_Garda_Station_m + House_stk +
#     property_type_Second.Hand + unit_type_House +
#     # County_Cavan + 
#     County_Clare + County_Cork + County_Donegal + County_Dublin +
#     County_Galway + County_Kerry + County_Kildare + County_Kilkenny + County_Laois +
#     County_Leitrim + County_Limerick + County_Longford + County_Louth +
#     County_Mayo + County_Meath + County_Monaghan + County_Offaly +
#     County_Roscommon + County_Sligo + County_Tipperary + County_Waterford +
#     County_Westmeath + County_Wexford + County_Wicklow +
#     # Year_X2011 + 
#     # Year_X2012 + 
#     Year_X2013 + Year_X2014 + Year_X2015 +
#     Year_X2016 + Year_X2017 + Year_X2018 + Year_X2019 +
#     Year_X2020 + Year_X2021 + Year_X2022 # + Year_X2023
# 
# model_formula_gwr_simple <- Price ~ TotalCrime + constr_index + int_rates +
#     dist_to_bua_m +
#     dist_to_com_air_m + 
#     dist_to_hos_m + dist_to_trainstaion +
#     Pop_est + 
#     dist_to_Garda_Station_m + 
#     House_stk+
#     property_type_Second.Hand + unit_type_House
# 
# # tune bandwith
# model_formula_H1_gwr <- Price ~ TotalCrime +
#     # County_Cavan + County_Clare + County_Cork + County_Donegal + 
#     # County_Dublin +
#     # County_Galway +
#     # County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
#     # County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
#     # County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + 
#     # County_Wexford +
#     # County_Wicklow +
#     Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
#     Year_X2018 + Year_X2019 + Year_X2020 + 
#     Year_X2021 +
#     Year_X2022 +
#     # PC1 + PC2 +
#     PC3 + PC4 + PC5 + PC6 + PC7 + PC8 +
#     property_type_Second.Hand +
#     unit_type_House +
#     HighIncome_Regular
# 
# 
# # Sample 5000 spatial points from gwr_model_data
# set.seed(5553) # for reproducibility
# sample_idx <- sample(1:nrow(gwr_model_data), 10000)
# sample_data <- gwr_model_data[sample_idx, ]
# 
# car::vif(lm(model_formula_H1_gwr, data = gwr_model_data))
# 
# # bw_gwr <- bw.gwr(model_formula_H1_gwr, 
# #                  data = gwr_model_data, 
# #                  approach = "AICc", 
# #                  kernel = "bisquare", 
# #                  adaptive = TRUE,)
#             
# 
# gwr_res_H1 <- gwr.basic(model_formula_H1_gwr, 
#                      data = gwr_model_data, 
#                      regression.points = gwr_model_data,
#                      # manually set band with
#                      bw = 3100,
#                      # bw = bw_gwr,
#                      adaptive = TRUE, 
#                      kernel = "bisquare")
# summary(gwr_res_H1)
# # car::vif(lm(model_formula_gwr_simple, data = gwr_model_data))
# 
# # multi geographically weighted regression
# mgwr_res_H1 <- gwr.multiscale(model_formula_H1_gwr, 
#                            data = sample_data, 
#                            kernel = "bisquare", 
#                            adaptive = TRUE)
# summary(mgwr_res_H1)
# 
# 
# # spatial model, no temporal aspect. model is potentially useless ----------
# 
# 
# # Fitting multiscale GWR with spatial autocorrelation 
# coords_mat <- as.matrix(gwr_model_data[,c("longitude","latitude")])
# 
# # Create a weight matrix (4-nearest neighbors) 
# W4 <- kernel_matW(H = 4, kernels = 'rectangle', 
#                   coords = coords_mat, 
#                   NN = 4, adaptive = TRUE, 
#                   diagnull = TRUE)
# 
# mgwrsar_res <- MGWRSAR(formula = model_formula_gwr_simple, 
#                        data = gwr_model_data, 
#                        coords = coords_mat, 
#                        fixed_vars = c("int_rates", "constr_index"), 
#                        kernels = 'gauss', 
#                        H = 20, 
#                        Model = 'MGWRSAR_0_kc_kv', 
#                        control = list(adaptive = TRUE, W = W4))
# summary(mgwrsar_res)
# 
# 
# # Extract coefficients for a variable (e.g. TotalCrime)
# total_crime_coef <- mgwrsar_res@Betav[, "TotalCrime"]
# 
# # Add coords and turn into sf object
# gwr_model_data$TotalCrime_coef <- total_crime_coef
# gwr_model_data_sf <- st_as_sf(gwr_model_data, coords = c("longitude", "latitude"), crs = 4326)
# 
# # Plot with ggplot
# ggplot(gwr_model_data_sf) +
#     geom_sf(aes(color = TotalCrime_coef), size = 0.5) +
#     scale_color_viridis_c() +
#     theme_minimal() +
#     labs(title = "Spatial Coefficient for TotalCrime")
# 
# 
