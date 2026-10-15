# Generalised additive models
# ===========================
# Smooth, non-linear versions of H1-H5.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: output of 03

# Generalized Additive Model ----------------------------------------------
# without using workflows
gam_model_data <- prep(rec) %>%
    bake(new_data = crime_500_train_data)

colnames(gam_model_data)
# hypothesis 1

gam_model_H1 <- gam(
    formula = Price ~ 
        s(TotalCrime) + 
        s(int_rates) + 
        s(dist_to_bua_m) + 
        s(dist_to_trainstaion) + 
        s(dist_to_airport) + 
        s(dist_to_hos_m) + 
        s(dist_to_com_air_m) + 
        s(Pop_est) + 
        s(dist_to_Garda_Station_m) +
        station_count +
        s(latitude, longitude) +  # spatial smooth
        County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
        County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
        County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
        County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
        Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
        Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
        property_type_Second.Hand + unit_type_House + HighIncome_Regular + 
        s(Pop_density) + s(median_income),
    data = gam_model_data,
    method = "REML"
)
summary(gam_model_H1)
plot(gam_model_H1, pages = 1, se = TRUE)
gratia::draw(gam_model_H1, select = "s(TotalCrime)")

# hypothesis 2

gam_model_H2 <- gam(
    formula = Price ~  
        s(ViolentCrime) + 
        s(NonViolentCrime) +
        s(int_rates) + 
        s(dist_to_bua_m) + 
        s(dist_to_trainstaion) + 
        s(dist_to_airport) + 
        s(dist_to_hos_m) + 
        s(dist_to_com_air_m) + 
        station_count +
        s(Pop_est) + 
        s(dist_to_Garda_Station_m) + 
        s(latitude, longitude) +  # spatial smooth
        County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
        County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
        County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
        County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
        Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
        Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
        property_type_Second.Hand + unit_type_House + HighIncome_Regular +
        s(Pop_density) + s(median_income),
    data = gam_model_data,
    method = "REML"
)
summary(gam_model_H2)
plot(gam_model_H2, pages = 1, se = TRUE)
gratia::draw(gam_model_H2, select = "s(NonViolentCrime)")

# hypothesis 3

gam_model_H3 <- gam(
    formula = Price ~ 
        s(PersonalSafetyCrime) + 
        s(PropertyCrime) +
        s(int_rates) + 
        s(dist_to_bua_m) + 
        s(dist_to_trainstaion) + 
        s(dist_to_airport) + 
        station_count +
        s(dist_to_hos_m) + 
        s(dist_to_com_air_m) + 
        s(Pop_est) + 
        s(dist_to_Garda_Station_m) + 
        s(latitude, longitude) +  # spatial smooth
        County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
        County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
        County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
        County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
        Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
        Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
        property_type_Second.Hand + unit_type_House + HighIncome_Regular +
        s(Pop_density) + s(median_income),
    data = gam_model_data,
    method = "REML"
)
summary(gam_model_H3)

# hypothesis 4

gam_model_H4 <- gam(
    formula = Price ~  
        s(TotalCrime, by = HighIncome_Regular) + 
        s(int_rates) + 
        s(dist_to_bua_m) + 
        s(dist_to_trainstaion) + 
        s(dist_to_airport) + 
        station_count +
        s(dist_to_hos_m) + 
        s(dist_to_com_air_m) + 
        s(Pop_est) + 
        s(dist_to_Garda_Station_m) + 
        s(latitude, longitude) +  # spatial smooth
        County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
        County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
        County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
        County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
        Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
        Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
        property_type_Second.Hand + unit_type_House + #HighIncome_Regular +
        s(Pop_density) + s(median_income),
    data = gam_model_data,
    method = "REML"
)
summary(gam_model_H4)

# hypothesis 5

gam_model_H5 <- gam(
    formula = Price ~  
        s(TotalCrime) +
        s(TotalCrime_lag1) + 
        s(TotalCrime_lag2) +
        s(int_rates) + 
        s(dist_to_bua_m) + 
        s(dist_to_trainstaion) +
        station_count +
        s(dist_to_airport) + 
        s(dist_to_hos_m) + 
        s(dist_to_com_air_m) + 
        s(Pop_est) + 
        s(dist_to_Garda_Station_m) + 
        s(latitude, longitude) +  # spatial smooth
        County_Cavan + County_Clare + County_Cork + County_Donegal + County_Dublin + County_Galway +
        County_Kerry + County_Kildare + County_Kilkenny + County_Laois + County_Leitrim + County_Limerick +
        County_Longford + County_Louth + County_Mayo + County_Meath + County_Offaly + County_Roscommon +
        County_Sligo + County_Tipperary + County_Waterford + County_Westmeath + County_Wexford + County_Wicklow +
        Year_X2012 + Year_X2013 + Year_X2014 + Year_X2015 + Year_X2016 + Year_X2017 +
        Year_X2018 + Year_X2019 + Year_X2020 + Year_X2021 + Year_X2022 +
        property_type_Second.Hand + unit_type_House + HighIncome_Regular +
        s(Pop_density) + s(median_income),
    data = gam_model_data,
    method = "REML"
)
summary(gam_model_H5)

