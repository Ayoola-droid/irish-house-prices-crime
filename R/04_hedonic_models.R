# Hedonic pricing models (OLS)
# ============================
# Five hypothesis models (H1-H5), multicollinearity checks, test-set RMSE and R-squared.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: output of 03

# HPM linear regression model ---------------------------------------------
# set parameters and control variables
# Define the recipe
rec <- recipe(Price ~.,
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
    step_dummy(all_nominal_predictors()) 
    # step_pca(all_predictors(), threshold = 0.80)

# hypothesis one
model_formula_H1 <- Price ~ TotalCrime +
 # Date + 
    int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
    dist_to_hos_m + dist_to_com_air_m + Pop_est + 
    # House_stk +
    dist_to_Garda_Station_m + 
    latitude + longitude + 
    # prop_id + 
    # `Attempts/threats to murder, assaults, harassments and related offences` +
    # `Dangerous or negligent acts` + `Kidnapping and related offences` +
    # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
    # `Theft and related offences` + `Fraud, deception and related offences` +
    # `Controlled drug offences` + `Weapons and Explosives Offences` +
    # `Damage to property and to the environment` + `Public order and other social code offences` +
    # `Offences against government, justice procedures and organisation of crime` + 
    County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
    County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
    County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
    County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
    County_Wicklow +
    Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
    Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 + 
    property_type_Second.Hand + unit_type_House + HighIncome_Regular + 
    Pop_density + station_count +
    # `Size m²`+
    # housing_density +
    median_income 

# hypothesis two
model_formula_H2 <- Price ~ ViolentCrime + NonViolentCrime +
    # Date +
    int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
    dist_to_hos_m + dist_to_com_air_m + Pop_est + 
    # House_stk + 
    dist_to_Garda_Station_m + 
    latitude + longitude + 
    # prop_id + 
    # `Attempts/threats to murder, assaults, harassments and related offences` +
    # `Dangerous or negligent acts` + `Kidnapping and related offences` +
    # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
    # `Theft and related offences` + `Fraud, deception and related offences` +
    # `Controlled drug offences` + `Weapons and Explosives Offences` +
    # `Damage to property and to the environment` + `Public order and other social code offences` +
    # `Offences against government, justice procedures and organisation of crime` + 
    County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
    County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
    County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
    County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
    County_Wicklow +
    Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
    Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 + 
    property_type_Second.Hand + unit_type_House + HighIncome_Regular   + Pop_density   +
    # `Size m²` + 
    # housing_density + 
    median_income 

# hypothesis three
model_formula_H3 <- Price ~ PersonalSafetyCrime + PropertyCrime + 
    # Date + 
    int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
    dist_to_hos_m + dist_to_com_air_m + Pop_est + 
    # House_stk + 
    dist_to_Garda_Station_m + 
    latitude + longitude + 
    # prop_id + 
    # `Attempts/threats to murder, assaults, harassments and related offences` +
    # `Dangerous or negligent acts` + `Kidnapping and related offences` +
    # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
    # `Theft and related offences` + `Fraud, deception and related offences` +
    # `Controlled drug offences` + `Weapons and Explosives Offences` +
    # `Damage to property and to the environment` + `Public order and other social code offences` +
    # `Offences against government, justice procedures and organisation of crime` + 
    County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
    County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
    County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
    County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
    County_Wicklow +
    Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
    Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 + 
    property_type_Second.Hand + unit_type_House + HighIncome_Regular   + Pop_density   + station_count +
    # `Size m²` + 
    # housing_density + 
    median_income 

# hypothesis four
model_formula_H4 <- Price ~ TotalCrime*HighIncome_Regular + 
    # Date + 
    int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
    dist_to_hos_m + dist_to_com_air_m + Pop_est + 
    # House_stk + 
    dist_to_Garda_Station_m + 
    latitude + longitude + 
    # prop_id + 
    # `Attempts/threats to murder, assaults, harassments and related offences` +
    # `Dangerous or negligent acts` + `Kidnapping and related offences` +
    # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
    # `Theft and related offences` + `Fraud, deception and related offences` +
    # `Controlled drug offences` + `Weapons and Explosives Offences` +
    # `Damage to property and to the environment` + `Public order and other social code offences` +
    # `Offences against government, justice procedures and organisation of crime` + 
    County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
    County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
    County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
    County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
    County_Wicklow +
    Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
    Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
    property_type_Second.Hand + unit_type_House    + Pop_density   + station_count +
    # `Size m²` +
    # housing_density + 
    median_income 

# hypothesis five
model_formula_H5 <- Price ~ TotalCrime + TotalCrime_lag1 + 
    TotalCrime_lag2 +
    # Date + 
    int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport +
    dist_to_hos_m + dist_to_com_air_m + Pop_est + 
    # House_stk + 
    dist_to_Garda_Station_m + 
    latitude + longitude + 
    # prop_id + 
    # `Attempts/threats to murder, assaults, harassments and related offences` +
    # `Dangerous or negligent acts` + `Kidnapping and related offences` +
    # `Robbery, extortion and hijacking offences` + `Burglary and related offences` +
    # `Theft and related offences` + `Fraud, deception and related offences` +
    # `Controlled drug offences` + `Weapons and Explosives Offences` +
    # `Damage to property and to the environment` + `Public order and other social code offences` +
    # `Offences against government, justice procedures and organisation of crime` + 
    County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
    County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
    County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
    County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
    County_Wicklow +
    Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
    Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
    property_type_Second.Hand + unit_type_House + HighIncome_Regular + station_count +
    Pop_density   + 
    # `Size m²` + 
    # housing_density + 
    median_income 



# Define the model spec
lm_spec <- linear_reg() %>% 
    set_engine("lm")

# Create the workflow
wf <- workflow() %>%
    add_model(lm_spec) %>%
    add_recipe(rec) 

# Fit the model
model_enhanced <- wf %>% 
    fit(data = crime_500_train_data)

# View summary
summary(model_enhanced %>% 
            extract_fit_parsnip() %>%
            .$fit)

# Predict on test data
predictions <- predict(model_enhanced, 
                       new_data = crime_500_test_data) %>%
    bind_cols(crime_500_test_data)
# compute metrics
predictions <- predictions %>%
    metrics(truth = Price, estimate = .pred)
print(predictions)
colnames(crime_500_test_data)

# without using workflows
lm_model_data <- prep(rec) %>%
    bake(new_data = crime_500_train_data)

cor_matrix <- cor(lm_model_data, use = "complete.obs")
round(cor_matrix, 2)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)
high_corr <- findCorrelation(cor_matrix, cutoff = 0.8)
colnames(cor_matrix)[high_corr]
# pca_result <- prcomp(lm_model_data[, high_corr], scale. = TRUE)
# lm_model_data$PC1 <- pca_result$x[, 1]

lm_model_H1 <- lm(model_formula_H1, 
               data = lm_model_data)
vif(lm_model_H1)
summary(lm_model_H1)

#    Replace 'instrument' with your actual instrument, e.g. station_count
# iv_model <- ivreg(
#     # formula: left of ~ is outcome; 
#     # right of ~ before | are endogenous + controls;
#     # right of | are instruments + the same controls
#     Price ~ 
#         TotalCrime
#     + int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport
#     + dist_to_hos_m + dist_to_com_air_m + Pop_est + House_stk
#     + dist_to_Garda_Station_m + latitude + longitude
#     +  property_type_Second.Hand + unit_type_House + HighIncome_Regular
#     + Pop_density + housing_density + median_income +
#     County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
#         County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
#         County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
#         County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
#         County_Wicklow +
#         Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
#         Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022
#     |
#         station_count + int_rates + dist_to_bua_m + dist_to_trainstaion + dist_to_airport  + 
#         dist_to_hos_m + dist_to_com_air_m + Pop_est + House_stk + 
#         dist_to_Garda_Station_m + latitude + longitude +  
#         property_type_Second.Hand + unit_type_House + HighIncome_Regular +
#         Pop_density + housing_density + median_income  + County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
#         County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
#         County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
#         County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford +
#         County_Wicklow +
#         Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
#         Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022,
#     data = lm_model_data
# )

# H2
lm_model_H2 <- lm(model_formula_H2, 
                  data = lm_model_data)
summary(lm_model_H2)

# H3
lm_model_H3 <- lm(model_formula_H3, 
                  data = lm_model_data)
summary(lm_model_H3)

# H4
lm_model_H4 <- lm(model_formula_H4, 
                  data = lm_model_data)
summary(lm_model_H4)

# H5
lm_model_H5 <- lm(model_formula_H5, 
                  data = lm_model_data)
summary(lm_model_H5)

# Output as HTML
 modelsummary(
    list("H1: TotalCrime"                 = lm_model_H1,
         "H2: Violent vs NonViolent"      = lm_model_H2,
         "H3: PersonalSafety vs Property" = lm_model_H3,
         "H4: HighIncome*TotalCrime"      = lm_model_H4,
         "H5: Crime Lags"                 = lm_model_H5),
    statistic = "({std.error})",  # shows std errors in parentheses
    stars = TRUE,
    output = "html"
)
# kable(model_coef_hpm, 
#       caption = "Linear model metrics for Properties joined with crime counts 500 meters from Reporting Garda station") %>% 
#     kable_styling(full_width = FALSE, position = "center")
# without using workflows
lm_model_data_test <- prep(rec) %>%
    bake(new_data = crime_500_test_data)


# Predict for each hypothesis model
preds_H1 <- predict(lm_model_H1, newdata = lm_model_data_test)
preds_H2 <- predict(lm_model_H2, newdata = lm_model_data_test)
preds_H3 <- predict(lm_model_H3, newdata = lm_model_data_test)
preds_H4 <- predict(lm_model_H4, newdata = lm_model_data_test)
preds_H5 <- predict(lm_model_H5, newdata = lm_model_data_test)

# Combine predictions with actual Price
lm_preds_H1 <- lm_model_data_test %>% 
    mutate(.pred = preds_H1) 
lm_preds_H2 <- lm_model_data_test %>% 
    mutate(.pred = preds_H2) 
lm_preds_H3 <- lm_model_data_test %>% 
    mutate(.pred = preds_H3) 
lm_preds_H4 <- lm_model_data_test %>% 
    mutate(.pred = preds_H4) 
lm_preds_H5 <- lm_model_data_test %>% 
    mutate(.pred = preds_H5) 

# Compute metrics for each model
metrics_H1 <- metrics(lm_preds_H1, truth = Price, estimate = .pred)
metrics_H2 <- metrics(lm_preds_H2, truth = Price, estimate = .pred)
metrics_H3 <- metrics(lm_preds_H3, truth = Price, estimate = .pred)
metrics_H4 <- metrics(lm_preds_H4, truth = Price, estimate = .pred)
metrics_H5 <- metrics(lm_preds_H5, truth = Price, estimate = .pred)

# Combine metrics into a single table
model_results_lm <- tibble(
    Hypothesis = paste0("H", 1:5),
    RMSE = c(
        metrics_H1 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_H2 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_H3 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_H4 %>% filter(.metric == "rmse") %>% pull(.estimate),
        metrics_H5 %>% filter(.metric == "rmse") %>% pull(.estimate)
    ),
    RSQ = c(
        metrics_H1 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_H2 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_H3 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_H4 %>% filter(.metric == "rsq") %>% pull(.estimate),
        metrics_H5 %>% filter(.metric == "rsq") %>% pull(.estimate)
    ),
    MAE = c(
        metrics_H1 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_H2 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_H3 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_H4 %>% filter(.metric == "mae") %>% pull(.estimate),
        metrics_H5 %>% filter(.metric == "mae") %>% pull(.estimate)
    )
)

print(model_results_lm)
kable(model_results_lm, 
      caption = "Linear model metrics for Properties joined with crime counts 3000 meters from Reporting Garda station") %>% 
    kable_styling(full_width = FALSE, position = "center")




