# Visualisations
# ==============
# Charts of population, housing stock, income and prices.
# Part of: How Crime Shapes Property Values (MSc Business Analytics thesis, Ayoola Fadare, 2025)
# Inputs: outputs of earlier scripts

# VIZ ---------------------------------------------------------------------

pop_estimate %>% 
    filter(County != "State") %>% 
    group_by(Year, County) %>% 
    mutate(County = fct_infreq(County)) %>%
    summarise(TotalSalePrice = sum(estimate, na.rm = TRUE), .groups = "drop") %>%  # Summarize total sales
    ggplot(aes(x = Year, 
               y = TotalSalePrice, 
               color = County))+
    geom_line(linewidth = 1.4)+
    geom_point(size = 2) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Population for Counties in the Republic of Ireland", 
         x = "Year",
         y = "Population")


population_ireland %>% 
    filter(County != "State") %>% 
    group_by(CensusYear, County) %>% 
    mutate(County = fct_infreq(County)) %>%
    summarise(TotalSalePrice = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%  # Summarize total sales
    ggplot(aes(x = CensusYear, 
               y = TotalSalePrice, 
               color = County))+
    geom_line(linewidth = 1.4)+
    geom_point(size = 2) +
    scale_y_continuous(labels = dollar)


stock %>% 
    filter(County != "State") %>% 
    group_by(Year, County) %>% 
    mutate(County = fct_infreq(County)) %>%
    summarise(TotalSalePrice = sum(estimate, na.rm = TRUE), .groups = "drop") %>%  # Summarize total sales
    ggplot(aes(x = Year, 
               y = TotalSalePrice, 
               color = County))+
    geom_line(linewidth = 1.4)+
    geom_point(size = 2) +
    scale_y_continuous(labels = dollar)

house_stk %>% 
    filter(County != "State") %>% 
    group_by(Year, County) %>% 
    mutate(County = fct_infreq(County)) %>%
    summarise(TotalSalePrice = sum(estimate, na.rm = TRUE), .groups = "drop") %>%  # Summarize total sales
    ggplot(aes(x = Year, 
               y = TotalSalePrice, 
               color = County))+
    geom_line(linewidth = 1.4)+
    geom_point(size = 2) +
    scale_y_continuous(labels = label_number()) + 
    labs(title = "Housing Stock for Counties in the Republic of Ireland", 
         x = "Year",
         y = "Number of houses")

income %>% 
    filter(County != "State",
           Year   != 2010) %>% 
    group_by(Year, County) %>% 
    mutate(County = fct_infreq(County)) %>%
    summarise(TotalSalePrice = sum(estimate, na.rm = TRUE), .groups = "drop") %>%  # Summarize total sales
    ggplot(aes(x = Year, 
               y = TotalSalePrice, 
               color = County))+
    geom_line(linewidth = 1.4)+
    geom_point(size = 2) +
    scale_y_continuous(labels = dollar) +
    labs(title = "Median Income for Counties in the Republic of Ireland", 
         x = "Year",
         y = "Income")

# Address outliers in the price column
crime_in_500m %>% 
    mutate(Year = as_factor(Year)) %>% 
    ggplot(aes(x = Year,
               y = Price,
               fill = Year))+
    geom_boxplot()+
    facet_wrap(~ County, scales = "free_y") +  # Creates separate plot for each county
    labs(title = "Boxplot of Sales Price by Year Sold",
         subtitle = "Outliers have been Identified")+
    scale_y_continuous(labels = dollar)
 

# Scatter plot of Price vs TotalCrime
crime_in_500m %>% 
    filter(station_count != 0) %>% 
    ggplot(aes(x = TotalCrime, 
               y = Price)) +
    geom_point(alpha = 0.3) +
    labs(title = "Price vs Total Crime", 
         x = "Total Crime Count",
         y = "Price (€)") +
    geom_smooth(method = "lm", se = FALSE, color = "blue")
