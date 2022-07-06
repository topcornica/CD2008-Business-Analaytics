library(dplyr)
glimpse(storms) #storms is a data that come with dplyr library

storms_names <- select(storms, "name", "year")
storms_names_1980s <- subset(storms_names ,year > 1979 & year < 1990) #Year will display only 1980s-1989s
storms_names_1980s #display a table that contains name and year. 

storms_year <- as_tibble(select(storms, "year"))
storms_per_year <- count(storms_year, year)
storms_per_year #display year and number of storms recorded in each year.

storms_records <- as_tibble(select(storms, "year", "name"))
storms_records_per_year <- count(storms_records, year, name)
storms_records_per_year #display name, year and number of name's storms recorded.

distinct(storms, status, .keep_all = FALSE) #display all status in data.

distinct(storms, category, .keep_all = FALSE) #display all category in data.

storms_5 <- as_tibble(distinct(storms, category), .keep_all = FALSE)
storms_categ <- as_tibble(select(storms, "year", "name", "category"))
storms_categ5_1 <- merge(x=storms_categ, y=storms_5, by=c("category"),all.y = TRUE)
storms_categ5 <- distinct(storms_categ5_1, year, name)
storms_categ5 #display name and year.

storms_info <- as_tibble(distinct(storms, category, status, pressure, wind))
storms_statistics <- storms_info %>% group_by(category, status) %>% summarise_at(vars(c(pressure, wind)), list(avg = mean))
colnames(storms_statistics) <- c("category", "status", "avg_pressure", "avg_wind")
storms_statistics #display category, status, average pressure and average wind in each category and status.

storms_wind <- as_tibble(distinct(storms, year, name, wind))
max_wind_per_storm <- storms_wind %>% group_by(year, name) %>% summarise_if(is.numeric, max)
colnames(max_wind_per_storm) <- c("year", "name", "max(wind)")
max_wind_per_storm #display year, name and maximum value of wind in each name.

storms_wind2 <- as_tibble(distinct(storms, year, name, wind))
max_wind1 <- storms_wind2 %>% group_by(year, name) %>% summarise_if(is.numeric, max)
max_wind_per_year <- max_wind1[order(-max_wind1$wind),]
max_wind_per_year #display year, name that order high value to low value.
