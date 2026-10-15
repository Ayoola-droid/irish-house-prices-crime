# Panel data models
# =================
# Fixed-effects panel specification.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: output of 03

# Panel Data Models -------------------------------------------------------
# Define the recipe
rec_pnl <- recipe(Price ~.,
              data = crime_500_train_data) %>%

    # step_log(Price, base = 10) %>%
    step_log(dist_to_bua_m, dist_to_com_air_m, dist_to_hos_m,
             dist_to_trainstaion, dist_to_Garda_Station_m, offset = 1, base = 10) %>%
    step_mutate(Year = factor(Year)) %>%

    step_zv(all_predictors()) %>%  # removes columns with 0 variance
    step_nzv(all_predictors()) %>%
    step_normalize(all_numeric_predictors(),
                   -all_of(c("latitude", "longitude","prop_id")))%>%
    # step_interact(terms = ~ Pop_est:House_stk) %>%
    step_dummy(all_nominal_predictors(),
               -all_of(c("Year", "prop_id","County")))

PD_model_data_train <- prep(rec_pnl) %>%
    bake(new_data = crime_500_train_data)

model_formula_pnl <- Price ~
    TotalCrime +
    constr_index +
    int_rates +
    dist_to_bua_m +
    dist_to_com_air_m +
    dist_to_hos_m +
    dist_to_trainstaion +
    Pop_est +
    dist_to_Garda_Station_m +
    # House_stk +
    property_type_Second.Hand +
    # County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin +
    # County_Galway + County_Kerry + County_Kildare + County_Kilkenny + County_Laois +
    # County_Leitrim + County_Limerick + County_Longford + County_Louth + County_Mayo +
    # County_Meath + County_Monaghan + County_Offaly + County_Roscommon + County_Sligo +
    # County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
    unit_type_House + County

# colnames(PD_model_data_test)
# increases R² but reduces ststistical significance
# PD_model_data_train <- PD_model_data_train %>%
#     group_by(County, Year) %>%
#     summarise(across(where(is.numeric), mean, na.rm = TRUE),
#               property_type_Second.Hand = first(property_type_Second.Hand),
#               unit_type_House = first(unit_type_House),
#               .groups = "drop")

PD_model_data_train <- pdata.frame(PD_model_data_train, index = c("County", "Year"))
# check for multicollinearity
car::vif(lm(model_formula_pnl, data = PD_model_data_train))

fe_model <- plm(model_formula_pnl,
                data = PD_model_data_train,
                model = "within")   # fixed effects (within)
summary(fe_model)

re_model <- plm(model_formula_pnl,
                data = PD_model_data_train,
                model = "random",
                random.method = "amemiya")   # random effects
summary(re_model)
# Hausman test to decide between FE and RE
phtest(fe_model, re_model)


