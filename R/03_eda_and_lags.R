# Exploratory analysis and lagged datasets
# ========================================
# Profiles each buffer dataset and joins lagged crime counts; train/test split.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: outputs of 02

# Analysis ----------------------------------------------------------------
# inspect dataset
# skim(crime_in_500m)
# str(crime_in_500m)
# rename crime cols
crime_in_1000m <- crime_in_1000m %>% 
    rename("Attempts/threats to murder, assaults, harassments and related offences"    = total_attempts_assaults,
           "Dangerous or negligent acts"                                               = total_dangerous_negligent,
           "Kidnapping and related offences"                                           = total_kidnapping,
           "Robbery, extortion and hijacking offences"                                 = total_robbery_extortion,
           "Burglary and related offences"                                             = total_burglary,
           "Theft and related offences"                                                = total_theft,
           "Fraud, deception and related offences"                                     = total_fraud_deception,
           "Controlled drug offences"                                                  = total_drugs,
           "Weapons and Explosives Offences"                                           = total_weapons,
           "Damage to property and to the environment"                                 = total_damage,
           "Public order and other social code offences"                               = total_public_order,
           "Offences against government, justice procedures and organisation of crime" = total_offences_against_govt)

# convert price column to numeric and remove Currency sign
crime_in_1000m <- crime_in_1000m %>% 
    rename(Price = `Price ()`) %>% 
    mutate(
        Price = str_remove_all(Price, "[€$£,]"),  # Remove currency symbols and commas
        Price = str_trim(Price),                   # Remove whitespace
        Price = na_if(Price, ""),                  # Convert empty strings to NA
        Price = as.numeric(Price),
        
    )
 # Agreegating crime types
crime_in_1000m <- crime_in_1000m %>% 
    mutate(
        # total crime
        TotalCrime      =`Attempts/threats to murder, assaults, harassments and related offences`+`Dangerous or negligent acts`+`Kidnapping and related offences`+`Robbery, extortion and hijacking offences`+`Burglary and related offences`+`Theft and related offences`+`Fraud, deception and related offences`+`Controlled drug offences`+`Weapons and Explosives Offences`+`Damage to property and to the environment`+`Public order and other social code offences`+`Offences against government, justice procedures and organisation of crime`,
        # Property-targeting crimes (burglary, theft, vandalism)
        PropertyCrime   = `Burglary and related offences`+`Damage to property and to the environment`+`Theft and related offences`,
        # Violent crimes (assault, robbery, murder, etc.)
        ViolentCrime    = `Attempts/threats to murder, assaults, harassments and related offences`+`Robbery, extortion and hijacking offences`+`Weapons and Explosives Offences`,
        # Optionally, non-violent = total crimes minus violent crimes
        NonViolentCrime = `Dangerous or negligent acts`+`Burglary and related offences`+`Theft and related offences`+`Fraud, deception and related offences`+`Controlled drug offences`+`Damage to property and to the environment`+`Public order and other social code offences`+`Offences against government, justice procedures and organisation of crime`,
        # Personal-safety crimes (e.g., armed robbery, murder)
        PersonalSafetyCrime = `Attempts/threats to murder, assaults, harassments and related offences`+`Dangerous or negligent acts`+`Kidnapping and related offences`+`Robbery, extortion and hijacking offences`+`Weapons and Explosives Offences`  # as example
    )
# Address outliers in the price column
# crime_in_500m %>% 
#     mutate(Year = as_factor(Year)) %>% 
#     ggplot(aes(x = Year,
#                y = Price,
#                fill = Year))+
#     geom_boxplot()+
#     facet_wrap(~ County, scales = "free_y") +  # Creates separate plot for each county
#     labs(title = "Boxplot of Sales Price by Year Sold",
#          subtitle = "Outliers have been Handled")+
#     scale_y_continuous(labels = dollar)

# outlier tables
outliers_by_county <- crime_in_1000m %>%
    group_by(Year, County) %>%
    filter(Price %in% boxplot.stats(Price)$out) %>%
    ungroup()

# kable(outliers_by_county, caption = "Outliers in Sale Price by Brand")

# filter out outliers
crime_in_1000m <- crime_in_1000m %>% 
    anti_join(outliers_by_county,
              by = "prop_id")

# calculate percentiles by year
price_cutoffs_by_year <- crime_in_1000m %>%
    group_by(Year, County) %>% 
    summarise(
        price_cutoff_75th = quantile(Price, 0.80, na.rm = TRUE),
        .groups = "drop"
    )
# find expensive properties by year and county
crime_in_1000m <- crime_in_1000m %>%
    left_join(price_cutoffs_by_year, by = c("Year", "County")) %>%
    mutate(
        HighIncome = ifelse(Price >= price_cutoff_75th, "High", "Regular")
    ) %>% 
    select(-price_cutoff_75th)

# 
# # Summary of crime by income group
# crime_in_500m %>%
#     filter(station_count != 0) %>% 
#     group_by(HighIncome) %>%
#     summarize(
#         MeanPrice      = mean(Price, 
#                          na.rm = TRUE),
#         MeanTotalCrime = mean(TotalCrime,
#                               na.rm = TRUE)
#     )
# 
# # Scatter plot of Price vs TotalCrime
# crime_in_500m %>% 
#     filter(station_count != 0) %>% 
# ggplot(aes(x = TotalCrime, 
#            y = Price)) +
#     geom_point(alpha = 0.3) +
#     labs(title = "Price vs Total Crime", 
#          x = "Total Crime Count",
#          y = "Price (€)") +
#     geom_smooth(method = "lm", se = FALSE, color = "blue")
# 
# # Correlation test
# crime_in_500m %>% 
#     group_by(Year, County) %>% 
#     filter(station_count != 0) %>% 
#     with(cor.test(Price, TotalCrime))
# 
# # More detailed correlation analysis by group
# correlation_detailed <- crime_in_500m %>% 
#     filter(station_count != 0) %>%
#     group_by(Year, County) %>% 
#     do(tidy(cor.test(.$Price, .$TotalCrime))) %>%
#     ungroup()
# 
# View(correlation_detailed)

# factorise data
crime_in_1000m <- crime_in_1000m %>% 
    mutate(Year          = as.factor(Year),
           County        = as.factor(County),
           HighIncome    = as.factor(HighIncome),
           unit_type     = as.factor(unit_type),
           property_type = as.factor(property_type)) %>% 
    select(-Address,-Eircode,-`Description of Property`,-Full_Address) %>%
    filter(station_count != 0,
           !Year          %in% c(2010, 2023, 2024, 2025))

# set.seed(123)
# crime_500_split      <- initial_split(crime_in_500m,
#                             prop = 0.8)
# crime_500_train_data <- training(crime_500_split)
# crime_500_test_data  <- testing(crime_500_split)
# Transform the target in both sets



# lag analysis ------------------------------------------------------------

# rename crime cols
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>% 
    rename("Attempts/threats to murder, assaults, harassments and related offences"    = total_attempts_assaults,
           "Dangerous or negligent acts"                                               = total_dangerous_negligent,
           "Kidnapping and related offences"                                           = total_kidnapping,
           "Robbery, extortion and hijacking offences"                                 = total_robbery_extortion,
           "Burglary and related offences"                                             = total_burglary,
           "Theft and related offences"                                                = total_theft,
           "Fraud, deception and related offences"                                     = total_fraud_deception,
           "Controlled drug offences"                                                  = total_drugs,
           "Weapons and Explosives Offences"                                           = total_weapons,
           "Damage to property and to the environment"                                 = total_damage,
           "Public order and other social code offences"                               = total_public_order,
           "Offences against government, justice procedures and organisation of crime" = total_offences_against_govt)

# convert price column to numeric and remove Currency sign
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>% 
    rename(Price = `Price ()`) %>% 
    mutate(
        Price = str_remove_all(Price, "[€$£,]"),  # Remove currency symbols and commas
        Price = str_trim(Price),                   # Remove whitespace
        Price = na_if(Price, ""),                  # Convert empty strings to NA
        Price = as.numeric(Price),
        
    )
# Agreegating crime types
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>% 
    mutate(
        # total crime
        TotalCrime      =`Attempts/threats to murder, assaults, harassments and related offences`+`Dangerous or negligent acts`+`Kidnapping and related offences`+`Robbery, extortion and hijacking offences`+`Burglary and related offences`+`Theft and related offences`+`Fraud, deception and related offences`+`Controlled drug offences`+`Weapons and Explosives Offences`+`Damage to property and to the environment`+`Public order and other social code offences`+`Offences against government, justice procedures and organisation of crime`,
        # Property-targeting crimes (burglary, theft, vandalism)
        PropertyCrime   = `Burglary and related offences`+`Damage to property and to the environment`+`Theft and related offences`,
        # Violent crimes (assault, robbery, murder, etc.)
        ViolentCrime    = `Attempts/threats to murder, assaults, harassments and related offences`+`Robbery, extortion and hijacking offences`+`Weapons and Explosives Offences`,
        # Optionally, non-violent = total crimes minus violent crimes
        NonViolentCrime = `Dangerous or negligent acts`+`Burglary and related offences`+`Theft and related offences`+`Fraud, deception and related offences`+`Controlled drug offences`+`Damage to property and to the environment`+`Public order and other social code offences`+`Offences against government, justice procedures and organisation of crime`,
        # Personal-safety crimes (e.g., armed robbery, murder)
        PersonalSafetyCrime = `Attempts/threats to murder, assaults, harassments and related offences`+`Dangerous or negligent acts`+`Kidnapping and related offences`+`Robbery, extortion and hijacking offences`+`Weapons and Explosives Offences`  # as example
    )
# outlier tables
outliers_by_county <- crime_in_1000m_lag1 %>%
    group_by(Year, County) %>%
    filter(Price %in% boxplot.stats(Price)$out) %>%
    ungroup()

# kable(outliers_by_county, caption = "Outliers in Sale Price by Brand")

# filter out outliers
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>% 
    anti_join(outliers_by_county,
              by = "prop_id")

# calculate percentiles by year
price_cutoffs_by_year <- crime_in_1000m_lag1 %>%
    group_by(Year, County) %>% 
    summarise(
        price_cutoff_75th = quantile(Price, 0.80, na.rm = TRUE),
        .groups = "drop"
    )
# find expensive properties by year and county
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>%
    left_join(price_cutoffs_by_year, by = c("Year", "County")) %>%
    mutate(
        HighIncome = ifelse(Price >= price_cutoff_75th, "High", "Regular")
    ) %>% 
    select(-price_cutoff_75th)
# factorise data
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>% 
    mutate(Year          = as.factor(Year),
           County        = as.factor(County),
           HighIncome    = as.factor(HighIncome),
           unit_type     = as.factor(unit_type),
           property_type = as.factor(property_type)) %>% 
    select(-Address,-Eircode,-`Description of Property`,-Full_Address) %>%
    filter(station_count != 0,
           !Year          %in% c(2010, 2023, 2024, 2025))


# set.seed(1234)
# crime_500_split_lag1      <- initial_split(crime_in_500m_lag1,
#                                       prop = 0.8)
# crime_500_train_data_lag1 <- training(crime_500_split_lag1)
# crime_500_test_data_lag1  <- testing(crime_500_split_lag1)
# Transform the target in both sets
# crime_500_train_data_lag1 <- crime_500_train_data_lag1 %>%
#     mutate(Price = log10(Price))
# 
# crime_500_test_data_lag1 <- crime_500_test_data_lag1 %>%
#     mutate(Price = log10(Price))

# rename crime cols
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>% 
    rename("Attempts/threats to murder, assaults, harassments and related offences"    = total_attempts_assaults,
           "Dangerous or negligent acts"                                               = total_dangerous_negligent,
           "Kidnapping and related offences"                                           = total_kidnapping,
           "Robbery, extortion and hijacking offences"                                 = total_robbery_extortion,
           "Burglary and related offences"                                             = total_burglary,
           "Theft and related offences"                                                = total_theft,
           "Fraud, deception and related offences"                                     = total_fraud_deception,
           "Controlled drug offences"                                                  = total_drugs,
           "Weapons and Explosives Offences"                                           = total_weapons,
           "Damage to property and to the environment"                                 = total_damage,
           "Public order and other social code offences"                               = total_public_order,
           "Offences against government, justice procedures and organisation of crime" = total_offences_against_govt)

# convert price column to numeric and remove Currency sign
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>% 
    rename(Price = `Price ()`) %>% 
    mutate(
        Price = str_remove_all(Price, "[€$£,]"),  # Remove currency symbols and commas
        Price = str_trim(Price),                   # Remove whitespace
        Price = na_if(Price, ""),                  # Convert empty strings to NA
        Price = as.numeric(Price),
        
    )
# Agreegating crime types
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>% 
    mutate(
        # total crime
        TotalCrime      =`Attempts/threats to murder, assaults, harassments and related offences`+`Dangerous or negligent acts`+`Kidnapping and related offences`+`Robbery, extortion and hijacking offences`+`Burglary and related offences`+`Theft and related offences`+`Fraud, deception and related offences`+`Controlled drug offences`+`Weapons and Explosives Offences`+`Damage to property and to the environment`+`Public order and other social code offences`+`Offences against government, justice procedures and organisation of crime`,
        # Property-targeting crimes (burglary, theft, vandalism)
        PropertyCrime   = `Burglary and related offences`+`Damage to property and to the environment`+`Theft and related offences`,
        # Violent crimes (assault, robbery, murder, etc.)
        ViolentCrime    = `Attempts/threats to murder, assaults, harassments and related offences`+`Robbery, extortion and hijacking offences`+`Weapons and Explosives Offences`,
        # Optionally, non-violent = total crimes minus violent crimes
        NonViolentCrime = `Dangerous or negligent acts`+`Burglary and related offences`+`Theft and related offences`+`Fraud, deception and related offences`+`Controlled drug offences`+`Damage to property and to the environment`+`Public order and other social code offences`+`Offences against government, justice procedures and organisation of crime`,
        # Personal-safety crimes (e.g., armed robbery, murder)
        PersonalSafetyCrime = `Attempts/threats to murder, assaults, harassments and related offences`+`Dangerous or negligent acts`+`Kidnapping and related offences`+`Robbery, extortion and hijacking offences`+`Weapons and Explosives Offences`  # as example
    )
# outlier tables
outliers_by_county <- crime_in_1000m_lag2 %>%
    group_by(Year, County) %>%
    filter(Price %in% boxplot.stats(Price)$out) %>%
    ungroup()

# kable(outliers_by_county, caption = "Outliers in Sale Price by Brand")

# filter out outliers
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>% 
    anti_join(outliers_by_county,
              by = "prop_id")

# calculate percentiles by year
price_cutoffs_by_year <- crime_in_1000m_lag2 %>%
    group_by(Year, County) %>% 
    summarise(
        price_cutoff_75th = quantile(Price, 0.80, na.rm = TRUE),
        .groups = "drop"
    )
# find expensive properties by year and county
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>%
    left_join(price_cutoffs_by_year, by = c("Year", "County")) %>%
    mutate(
        HighIncome = ifelse(Price >= price_cutoff_75th, "High", "Regular")
    ) %>% 
    select(-price_cutoff_75th)

skim(crime_in_1000m_lag2)

# factorise data
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>% 
    mutate(Year          = as.factor(Year),
           County        = as.factor(County),
           HighIncome    = as.factor(HighIncome),
           unit_type     = as.factor(unit_type),
           property_type = as.factor(property_type)) %>% 
    select(-Address,-Eircode,-`Description of Property`,-Full_Address) %>%
    filter(station_count != 0,
           !Year          %in% c(2010, 2023, 2024, 2025))


# 
# set.seed(235)
# crime_500_split_lag2      <- initial_split(crime_in_500m_lag2,
#                                       prop = 0.8)
# crime_500_train_data_lag2 <- training(crime_500_split_lag2)
# crime_500_test_data_lag2  <- testing(crime_500_split_lag2)
# Transform the target in both sets
# crime_500_train_data_lag2 <- crime_500_train_data_lag2 %>%
#     mutate(Price = log10(Price))
# 
# crime_500_test_data_lag2 <- crime_500_test_data_lag2 %>%
#     mutate(Price = log10(Price))





# join lagged values ------------------------------------------------------
crime_in_1000m <- crime_in_1000m %>% 
    select(-`Attempts/threats to murder, assaults, harassments and related offences`, -
               `Dangerous or negligent acts`, - `Kidnapping and related offences`, -
                `Robbery, extortion and hijacking offences`, - `Burglary and related offences`, -
                `Theft and related offences`, - `Fraud, deception and related offences`, -
                `Controlled drug offences`, - `Weapons and Explosives Offences`, -
                `Damage to property and to the environment`, - `Public order and other social code offences`, -
                `Offences against government, justice procedures and organisation of crime`)

crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>% 
    select(prop_id, TotalCrime, PropertyCrime, ViolentCrime, NonViolentCrime, PersonalSafetyCrime) %>% 
    rename(TotalCrime_lag1          = TotalCrime,
           PropertyCrime_lag1       = PropertyCrime,
           ViolentCrime_lag1        = ViolentCrime,
           NonViolentCrime_lag1     = NonViolentCrime,
           PersonalSafetyCrime_lag1 = PersonalSafetyCrime)

crime_in_1000m <- crime_in_1000m %>% 
    left_join(crime_in_1000m_lag1,
              by = "prop_id")

# for two year lag
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>% 
    select(prop_id, TotalCrime, PropertyCrime, ViolentCrime, NonViolentCrime, PersonalSafetyCrime) %>% 
    rename(TotalCrime_lag2          = TotalCrime,
           PropertyCrime_lag2       = PropertyCrime,
           ViolentCrime_lag2        = ViolentCrime,
           NonViolentCrime_lag2     = NonViolentCrime,
           PersonalSafetyCrime_lag2 = PersonalSafetyCrime)

crime_in_1000m <- crime_in_1000m %>% 
    left_join(crime_in_1000m_lag2,
              by = "prop_id")

# # for the cummulative lag values
# crime_in_500m_lag2_cumm <- crime_in_500m_lag2_cumm %>% 
#     select(prop_id, TotalCrime, PropertyCrime, ViolentCrime, NonViolentCrime, PersonalSafetyCrime) %>% 
#     rename(TotalCrime_lag2_cumm          = TotalCrime,
#            PropertyCrime_lag2_cumm       = PropertyCrime,
#            ViolentCrime_lag2_cumm        = ViolentCrime,
#            NonViolentCrime_lag2_cumm     = NonViolentCrime,
#            PersonalSafetyCrime_lag2_cumm = PersonalSafetyCrime)
# 
# crime_in_500m <- crime_in_500m %>% 
#     left_join(crime_in_500m_lag2_cumm,
#               by = "prop_id")

# split data
set.seed(123)
crime_500_split      <- initial_split(crime_in_1000m,
                                      prop = 0.8)
crime_500_train_data <- training(crime_500_split)
crime_500_test_data  <- testing(crime_500_split)

crime_500_train_data <- crime_500_train_data %>%
    mutate(Price = log10(Price))

crime_500_test_data <- crime_500_test_data %>%
    mutate(Price = log10(Price))


