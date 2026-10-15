# Location features
# =================
# Distance from each sale to the nearest town centre, train station, airport, commercial airport, hospital (only hospitals open at the sale date) and Garda station; population density; mortgage rate and construction cost index matched to sale date.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: geocoded property data (see GEO_coding- repo)

# Built up areas or city/town settlement centers ------------------------------------------------------------
prop_city_sf <- detail_prop %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# Reproject both to Irish Transverse Mercator (EPSG:2157)
prop_city_sf <- st_transform(prop_city_sf, 2157)
city <- st_transform(city, 2157)
# Compute distances (meters) to all BUAs, then pick the min for each property
dist_mat <- st_distance(prop_city_sf,
                        city)
prop_city_sf$dist_to_bua_m <- apply(dist_mat, 1, min)

# Drop geometry to get plain data frame
detail_prop <- prop_city_sf %>%
    st_drop_geometry()

detail_prop <- detail_prop %>% 
    distinct()

detail_prop <- detail_prop %>% 
    select(-distance_to_citycenter)




# distance to train station -----------------------------------------------
# Reproject both to Irish Transverse Mercator (EPSG:2157)
prop_city_sf <- st_transform(prop_city_sf, 2157)
trainstation <- st_transform(trainstation, 2157)
# Compute distances (meters) to all BUAs, then pick the min for each property
dist_mat <- st_distance(prop_city_sf,
                        trainstation)
prop_city_sf$dist_to_trainstaion <- apply(dist_mat, 1, min)

# Drop geometry to get plain data frame
detail_prop <- prop_city_sf %>%
    st_drop_geometry()

detail_prop <- detail_prop %>% 
    distinct()

# detail_prop <- detail_prop %>% 
#     select(-dist_to_trainstaion)


# Distance to airport -----------------------------------------------------
# Reproject both to Irish Transverse Mercator (EPSG:2157)
prop_city_sf <- st_transform(prop_city_sf, 2157)
airports <- st_transform(airports, 2157)
# Compute distances (meters) to all BUAs, then pick the min for each property
dist_mat <- st_distance(prop_city_sf,
                        airports)
prop_city_sf$dist_to_airport <- apply(dist_mat, 1, min)

# Drop geometry to get plain data frame
detail_prop <- prop_city_sf %>%
    st_drop_geometry()

detail_prop <- detail_prop %>% 
    distinct()



# distance to COMMERCIAL airports -----------------------------------------
com_airports <- com_airports %>% 
    mutate(airport_addr = str_c(Airport,
                               "Ireland",
                               sep = " "))

# geo code city center
com_airports <- com_airports %>%
    tidygeocoder::geocode(address = airport_addr,
                          method  = "geoapify",
                          lat     = latitude,
                          long    = longitude)
# find distance to closest commercial airports from each property
prop_city_sf <- detail_prop %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
com_airports <- com_airports %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Reproject both to Irish Transverse Mercator (EPSG:2157)
prop_city_sf <- st_transform(prop_city_sf, 2157)
com_airports <- st_transform(com_airports, 2157)

# Compute distances (meters) to all Airports, then pick the min for each property
dist_mat <- st_distance(prop_city_sf,
                        com_airports)
# Ignore NAs
prop_city_sf$dist_to_com_air_m <- apply(dist_mat, 1, min)

# Drop geometry to get plain data frame
detail_prop <- prop_city_sf %>%
    st_drop_geometry()

detail_prop <- detail_prop %>% 
    distinct()





# distance to HOSPITALS ---------------------------------------------------
all_hos <- all_hos %>% 
    mutate(
        h_name = str_remove_all(
            h_name,
            "[\\p{C}]"           # remove all control/formatting chars
        ),
        h_name = str_squish(h_name)  
        # collapse any runs of spaces to one, trim ends
    )

all_hos <- all_hos %>% 
    mutate(hos_addr = str_c(h_name, 
                            "Ireland",
                            sep = " "))
# find hospital coordinate location
all_hos <- all_hos %>%
    tidygeocoder::geocode(address = hos_addr,
                          method  = "geoapify",
                          lat     = latitude,
                          long    = longitude)

# find distance to closest hospital from each property
prop_city_sf <- detail_prop %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
all_hos <- all_hos %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Reproject both to Irish Transverse Mercator (EPSG:2157)
prop_city_sf <- st_transform(prop_city_sf, 2157)
all_hos <- st_transform(all_hos, 2157)

all_hos <- all_hos %>%
    mutate(open_date = as.Date(if_else(
        h_name == "Children's Health Ireland at Connolly Hospital",
        "2019-07-01",
        # else, all other hospitals where open prior to 2007
        "2007-01-01"
    )))

# Compute distances (meters) to all BUAs, then pick the min for each property
dist_mat <- st_distance(prop_city_sf,
                        all_hos)
# ACCOUNTING FOR TEMPORAL DEPENDENCIES
# vectors of dates
prop_dates <- prop_city_sf$`Date of Sale (dd/mm/yyyy)`
hos_dates  <- all_hos$open_date

# matrix of TRUE where hospital opens after property date
too_late <- outer(prop_dates, hos_dates, FUN = `<`)  
# rows = properties, cols = hospitals: TRUE if prop_date < hos_open

# set those to NA
dist_mat[too_late] <- NA

# Ignore NAs
prop_city_sf$dist_to_hos_m <- apply(dist_mat, 1, min,  na.rm = TRUE)

# Drop geometry to get plain data frame
detail_prop <- prop_city_sf %>%
    st_drop_geometry()

detail_prop <- detail_prop %>% 
    distinct()








# Distance to nearest station ---------------------------------------------
# find distance to closest Garda station from each property
prop_city_sf <- detail_prop %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
crime_sf <- crime_ireland_wide %>%
    distinct(`Garda Location`,lon,lat) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Reproject both to Irish Transverse Mercator (EPSG:2157)
prop_city_sf <- st_transform(prop_city_sf, 2157)
crime_sf <- st_transform(crime_sf, 2157)

# Compute distances (meters) to all Garda Stations, then pick the min for each property
dist_mat <- st_distance(prop_city_sf,
                        crime_sf)
# Ignore NAs
prop_city_sf$dist_to_Garda_Station_m <- apply(dist_mat, 1, min)

# Drop geometry to get plain data frame
detail_prop <- prop_city_sf %>%
    st_drop_geometry()

detail_prop <- detail_prop %>% 
    distinct()



# calculate population density by county and year -------------------------
county_size <- county_size %>% 
    select(County,`Size (km²)`) %>% 
    # CONVERT TO METER SQUARE
    mutate(`Size (km²)` =  `Size (km²)`* 1000000) %>% 
    rename( "Size m²" = `Size (km²)`)

detail_prop <- detail_prop %>% 
    left_join(county_size,
              by = "County")

skim(detail_prop)

detail_prop <- detail_prop %>% 
    mutate(Pop_density     = Pop_est/`Size m²`,
           housing_density = House_stk/`Size m²`)

# Mortgage rates ----------------------------------------------------------
skim(mortgage)
# factor as date
mortgage <- mortgage %>% 
    mutate(`Reporting date`= as.Date(`Reporting date`,
                                     format = "%d/%m/%Y"))
mortgage <- mortgage %>% 
    select(`Reporting date`,`Loans to households, loans for house purchases with original maturity, over 5 years, interest rate (%)`)
# Convert  data.frames to data.tables
setDT(detail_prop)
setDT(mortgage)
# sort
setkey(detail_prop,`Date of Sale (dd/mm/yyyy)`)
setkey(mortgage, `Reporting date`)
# Rolling join on nearest date
#    roll = "nearest" finds the closest rate_date for each sale_date
detail_prop <- mortgage[detail_prop, roll = "nearest"]

detail_prop <- detail_prop %>% 
    rename(int_rates = `Loans to households, loans for house purchases with original maturity, over 5 years, interest rate (%)`)



# SCSI construction index -------------------------------------------------
skim(scsi)
# factor as date
scsi <- scsi %>% 
    mutate(Date = as.Date(Date,
                                     format = "%d/%m/%Y"))
scsi <- scsi %>% 
    select(Date,Index)
# Convert  data.frames to data.tables
setDT(detail_prop)
setDT(scsi)
# sort
setkey(detail_prop,`Date of Sale (dd/mm/yyyy)`)
setkey(scsi, Date)
# Rolling join on nearest date
#    roll = "nearest" finds the closest rate_date for each sale_date
detail_prop <- scsi[detail_prop, roll = "nearest"]

detail_prop <- detail_prop %>% 
    rename(constr_index = Index)

