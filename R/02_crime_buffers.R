# Crime within 500 m to 6 km of each sale
# =======================================
# Spatial buffers around each property, counting crimes by type from Garda stations inside the buffer, for the sale year and one and two years before.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: output of 01, geocoded Garda crime data

# Convert data to Spatial Objects to join crime data -----------------------------------------
# convert property data to spatial features with lat and lon
skim(crime_ireland_wide)
# give unique ID to property data
detail_prop$prop_id <- 1:nrow(detail_prop)
# convert to spatial points
prop_city_sf <- st_as_sf(
    detail_prop,
    coords = c("longitude", "latitude"),
    crs = 4326
)

crime_sf <- st_as_sf(
    crime_ireland_wide,
    coords = c("lon", "lat"),
    crs = 4326
)

#transformation into Metric crs
prop_city_sf <- st_transform(prop_city_sf, 2157)
crime_sf <- st_transform(crime_sf, 2157)

# create a 500m , 1000m, 1500m, 2000m, 3000m and 6000m buffer around each property
property_buffer_500  <- st_buffer(prop_city_sf, dist = 500)
property_buffer_1000 <- st_buffer(prop_city_sf, dist = 1000)
property_buffer_1500 <- st_buffer(prop_city_sf, dist = 1500)
property_buffer_2000 <- st_buffer(prop_city_sf, dist = 2000)
property_buffer_3000 <- st_buffer(prop_city_sf, dist = 3000)
property_buffer_6000 <- st_buffer(prop_city_sf, dist = 6000)

# join the crimes with properties that are within the buffer
prop_crime_500_ <- st_join(
    property_buffer_500,
    crime_sf,
    join = st_intersects
)
# radius of 1000m
prop_crime_1000_ <- st_join(
    property_buffer_1000,
    crime_sf,
    join = st_intersects
)
# radius of 1500m
prop_crime_1500_ <- st_join(
    property_buffer_1500,
    crime_sf,
    join = st_intersects
)
# radius of 2000m
prop_crime_2000_ <- st_join(
    property_buffer_2000,
    crime_sf,
    join = st_intersects
)
# radius of 3000m
prop_crime_3000_ <- st_join(
    property_buffer_3000,
    crime_sf,
    join = st_intersects
)
# radius of 6000m
prop_crime_6000_ <- st_join(
    property_buffer_6000,
    crime_sf,
    join = st_intersects
)
# keep only the rows where property matches with the crime YEAR
# prop_crime_500 <- prop_crime_500_ %>%
#     filter(Year.x == Year.y)
# # radius 1000m
# prop_crime_1000 <- prop_crime_1000_ %>%
#     filter(Year.x == Year.y)
# # radius 1500m
# prop_crime_1500 <- prop_crime_1500_ %>%
#     filter(Year.x == Year.y)
# # radius 2000m
# prop_crime_2000 <- prop_crime_2000_ %>%
#     filter(Year.x == Year.y)
# # radius 3000m
# prop_crime_3000 <- prop_crime_3000_ %>%
#     filter(Year.x == Year.y)
# # radius 6000m
# prop_crime_6000 <- prop_crime_6000_ %>%
#     filter(Year.x == Year.y)

# CRIME JOINS ON LAST YEAR
prop_crime_500_lag1 <- prop_crime_500_ %>%
    filter(Year.x == Year.y + 1)
# radius 1000m
prop_crime_1000_lag1 <- prop_crime_1000_ %>%
    filter(Year.x == Year.y + 1)
# radius 1500m
prop_crime_1500_lag1 <- prop_crime_1500_ %>%
    filter(Year.x == Year.y + 1)
# radius 2000m
prop_crime_2000_lag1 <- prop_crime_2000_ %>%
    filter(Year.x == Year.y + 1)
# radius 3000m
prop_crime_3000_lag1 <- prop_crime_3000_ %>%
    filter(Year.x == Year.y + 1)
# radius 6000m
prop_crime_6000_lag1 <- prop_crime_6000_ %>%
    filter(Year.x == Year.y + 1)

# create Two year cummulative lag
prop_crime_500_lag2_cumm <- prop_crime_500_ %>%
    filter(Year.y %in% (Year.x - 2:3))
# radius 1000m
prop_crime_1000_lag2_cumm <- prop_crime_1000_ %>%
    filter(Year.y %in% (Year.x - 2:3))
# radius 1500m
prop_crime_1500_lag2_cumm <- prop_crime_1500_ %>%
    filter(Year.y %in% (Year.x - 2:3))
# radius 2000m
prop_crime_2000_lag2_cumm <- prop_crime_2000_ %>%
    filter(Year.y %in% (Year.x - 2:3))
# radius 3000m
prop_crime_3000_lag2_cumm <- prop_crime_3000_ %>%
    filter(Year.y %in% (Year.x - 2:3))
# radius 6000m
prop_crime_6000_lag2_cumm <- prop_crime_6000_ %>%
    filter(Year.y %in% (Year.x - 2:3))

# create ONE year lag
prop_crime_500_lag2 <- prop_crime_500_ %>%
    filter(Year.x == Year.y + 2)
# radius 1000m
prop_crime_1000_lag2 <- prop_crime_1000_ %>%
    filter(Year.x == Year.y + 2)
# radius 1500m
prop_crime_1500_lag2 <- prop_crime_1500_ %>%
    filter(Year.x == Year.y + 2)
# radius 2000m
prop_crime_2000_lag2 <- prop_crime_2000_ %>%
    filter(Year.x == Year.y + 2)
# radius 3000m
prop_crime_3000_lag2 <- prop_crime_3000_ %>%
    filter(Year.x == Year.y + 2)
# radius 6000m
prop_crime_6000_lag2 <- prop_crime_6000_ %>%
    filter(Year.x == Year.y + 2)

# create Two year lag
prop_crime_500_lag3 <- prop_crime_500_ %>%
    filter(Year.x == Year.y + 3)
# radius 1000m
prop_crime_1000_lag3 <- prop_crime_1000_ %>%
    filter(Year.x == Year.y + 3)
# radius 1500m
prop_crime_1500_lag3 <- prop_crime_1500_ %>%
    filter(Year.x == Year.y + 3)
# radius 2000m
prop_crime_2000_lag3 <- prop_crime_2000_ %>%
    filter(Year.x == Year.y + 3)
# radius 3000m
prop_crime_3000_lag3 <- prop_crime_3000_ %>%
    filter(Year.x == Year.y + 3)
# radius 6000m
prop_crime_6000_lag3 <- prop_crime_6000_ %>%
    filter(Year.x == Year.y + 3)

# prop_crime_500 %>% 
#     # count(prop_id) %>% 
#     # slice(1:50) %>% 
#     select(`Garda Location`,prop_id) %>% 
#     view()



# group by prop_id, summarize and count garda staions in buffer zone
prop_crime_500 <- prop_crime_500 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 1000m
prop_crime_1000 <- prop_crime_1000 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 1500m
prop_crime_1500 <- prop_crime_1500 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 2000m
prop_crime_2000 <- prop_crime_2000 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 3000m
prop_crime_3000 <- prop_crime_3000 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 6000m
prop_crime_6000 <- prop_crime_6000 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )


# join by radius
crime_in_500m <- detail_prop %>%
    left_join(prop_crime_500, by = "prop_id")
# 1000m
crime_in_1000m <- detail_prop %>%
    left_join(prop_crime_1000, by = "prop_id")
# 1500m
crime_in_1500m <- detail_prop %>%
    left_join(prop_crime_1500, by = "prop_id")
# 2000m
crime_in_2000m <- detail_prop %>%
    left_join(prop_crime_2000, by = "prop_id")
# 3000m
crime_in_3000m <- detail_prop %>%
    left_join(prop_crime_3000, by = "prop_id")
# 6000m
crime_in_6000m <- detail_prop %>%
    left_join(prop_crime_6000, by = "prop_id")


# replace NA with Zero for properties not in radius of garda station
crime_in_500m <- crime_in_500m %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 1000m radius
crime_in_1000m <- crime_in_1000m %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 1500m radius
crime_in_1500m <- crime_in_1500m %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 2000m radius
crime_in_2000m <- crime_in_2000m %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 3000m radius
crime_in_3000m <- crime_in_3000m %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 6000m radius
crime_in_6000m <- crime_in_6000m %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )

# Data sets with lagged crime effects 1 year and 2 years prior ------------
# keep only the rows where property matches with the crime YEAR

# group by prop_id, summarize and count garda staions in buffer zone
prop_crime_500_lag1 <- prop_crime_500_lag1 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 1000m
prop_crime_1000_lag1 <- prop_crime_1000_lag1 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 1500m
prop_crime_1500_lag1 <- prop_crime_1500_lag1 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 2000m
prop_crime_2000_lag1 <- prop_crime_2000_lag1 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 3000m
prop_crime_3000_lag1 <- prop_crime_3000_lag1 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 6000m
prop_crime_6000_lag1 <- prop_crime_6000_lag1 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )


# join by radius
crime_in_500m_lag1 <- detail_prop %>%
    left_join(prop_crime_500_lag1, by = "prop_id")
# 1000m
crime_in_1000m_lag1 <- detail_prop %>%
    left_join(prop_crime_1000_lag1, by = "prop_id")
# 1500m
crime_in_1500m_lag1 <- detail_prop %>%
    left_join(prop_crime_1500_lag1, by = "prop_id")
# 2000m
crime_in_2000m_lag1 <- detail_prop %>%
    left_join(prop_crime_2000_lag1, by = "prop_id")
# 3000m
crime_in_3000m_lag1 <- detail_prop %>%
    left_join(prop_crime_3000_lag1, by = "prop_id")
# 6000m
crime_in_6000m_lag1 <- detail_prop %>%
    left_join(prop_crime_6000_lag1, by = "prop_id")


# replace NA with Zero for properties not in radius of garda station
crime_in_500m_lag1 <- crime_in_500m_lag1 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 1000m radius
crime_in_1000m_lag1 <- crime_in_1000m_lag1 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 1500m radius
crime_in_1500m_lag1 <- crime_in_1500m_lag1 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 2000m radius
crime_in_2000m_lag1 <- crime_in_2000m_lag1 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 3000m radius
crime_in_3000m_lag1 <- crime_in_3000m_lag1 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 6000m radius
crime_in_6000m_lag1 <- crime_in_6000m_lag1 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )





# TWO year lag ------------------------------------------------------------

# group by prop_id, summarize and count garda staions in buffer zone
prop_crime_500_lag2 <- prop_crime_500_lag2 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 1000m
prop_crime_1000_lag2 <- prop_crime_1000_lag2 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 1500m
prop_crime_1500_lag2 <- prop_crime_1500_lag2 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 2000m
prop_crime_2000_lag2 <- prop_crime_2000_lag2 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 3000m
prop_crime_3000_lag2 <- prop_crime_3000_lag2 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )
# Radius 6000m
prop_crime_6000_lag2 <- prop_crime_6000_lag2 %>%
    st_drop_geometry() %>%   # drop geometry before summarizing
    group_by(prop_id) %>%
    summarize(
        station_count = n_distinct(`Garda Station`),
        total_attempts_assaults = sum(
            `Attempts/threats to murder, assaults, harassments and related offences`,
            na.rm = TRUE
        ),
        total_dangerous_negligent = sum(
            `Dangerous or negligent acts`,
            na.rm = TRUE
        ),
        total_kidnapping = sum(
            `Kidnapping and related offences`,
            na.rm = TRUE
        ),
        total_robbery_extortion = sum(
            `Robbery, extortion and hijacking offences`,
            na.rm = TRUE
        ),
        total_burglary = sum(
            `Burglary and related offences`,
            na.rm = TRUE
        ),
        total_theft = sum(
            `Theft and related offences`,
            na.rm = TRUE
        ),
        total_fraud_deception = sum(
            `Fraud, deception and related offences`,
            na.rm = TRUE
        ),
        total_drugs = sum(
            `Controlled drug offences`,
            na.rm = TRUE
        ),
        total_weapons = sum(
            `Weapons and Explosives Offences`,
            na.rm = TRUE
        ),
        total_damage = sum(
            `Damage to property and to the environment`,
            na.rm = TRUE
        ),
        total_public_order = sum(
            `Public order and other social code offences`,
            na.rm = TRUE
        ),
        total_offences_against_govt = sum(
            `Offences against government, justice procedures and organisation of crime`,
            na.rm = TRUE
        )
    )


# join by radius
crime_in_500m_lag2 <- detail_prop %>%
    left_join(prop_crime_500_lag2, by = "prop_id")
# 1000m
crime_in_1000m_lag2 <- detail_prop %>%
    left_join(prop_crime_1000_lag2, by = "prop_id")
# 1500m
crime_in_1500m_lag2 <- detail_prop %>%
    left_join(prop_crime_1500_lag2, by = "prop_id")
# 2000m
crime_in_2000m_lag2 <- detail_prop %>%
    left_join(prop_crime_2000_lag2, by = "prop_id")
# 3000m
crime_in_3000m_lag2 <- detail_prop %>%
    left_join(prop_crime_3000_lag2, by = "prop_id")
# 6000m
crime_in_6000m_lag2 <- detail_prop %>%
    left_join(prop_crime_6000_lag2, by = "prop_id")


# replace NA with Zero for properties not in radius of garda station
crime_in_500m_lag2 <- crime_in_500m_lag2 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 1000m radius
crime_in_1000m_lag2 <- crime_in_1000m_lag2 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 1500m radius
crime_in_1500m_lag2 <- crime_in_1500m_lag2 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 2000m radius
crime_in_2000m_lag2 <- crime_in_2000m_lag2 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 3000m radius
crime_in_3000m_lag2 <- crime_in_3000m_lag2 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )
# 6000m radius
crime_in_6000m_lag2 <- crime_in_6000m_lag2 %>%
    mutate(
        station_count = replace_na(station_count, 0),
        total_attempts_assaults = replace_na(total_attempts_assaults, 0),
        total_dangerous_negligent = replace_na(total_dangerous_negligent, 0),
        total_kidnapping = replace_na(total_kidnapping, 0),
        total_robbery_extortion = replace_na(total_robbery_extortion, 0),
        total_burglary = replace_na(total_burglary, 0),
        total_theft = replace_na(total_theft, 0),
        total_fraud_deception = replace_na(total_fraud_deception, 0),
        total_drugs = replace_na(total_drugs, 0),
        total_weapons = replace_na(total_weapons, 0),
        total_damage = replace_na(total_damage, 0),
        total_public_order = replace_na(total_public_order, 0),
        total_offences_against_govt = replace_na(total_offences_against_govt, 0)
    )

