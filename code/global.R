

pacman::p_load("tidyverse", "lubridate","shiny")

malaria_data <- rio::import(here::here("/Users/mahimakaur/Downloads", "facility_count_data.rds")) %>% 
  as_tibble()

print(malaria_data)

malaria_data <- malaria_data %>%
  select(-newid) %>%
  pivot_longer(cols = 
                 starts_with("malaria_"), names_to = "age_group", 
               values_to = "cases_reported")

print(malaria_data)


plot_epicurve <- function(data, district = "All", agegroup = "malaria_tot", facility = "All") {
  
  if (!("All" %in% district)) {
    data <- data %>%
      filter(District %in% district)
    
    plot_title_district <- stringr::str_glue("{paste0(district, collapse = ', ')} districts")
    
  } else {
    
    plot_title_district <- "all districts"
    
  }
  
  # if no remaining data, return NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  data <- data %>%
    filter(age_group == agegroup)
  
  
  # if no remaining data, return NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  if (agegroup == "malaria_tot") {
    agegroup_title <- "All ages"
  } else {
    agegroup_title <- stringr::str_glue("{str_remove(agegroup, 'malaria_rdt')} years")
  }
  
  if (!("All" %in% facility)) {
    data <- data %>%
      filter(location_name == facility)
    
    plot_title_facility <- facility
    
  } else {
    
    plot_title_facility <- "all facilities"
    
  }
  
  # if no remaining data, return NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  
  
  ggplot(data, aes(x = data_date, y = cases_reported)) +
    geom_col(width = 1, fill = "darkred") +
    theme_minimal() +
    labs(
      x = "date",
      y = "number of cases",
      title = stringr::str_glue("Malaria cases - {plot_title_district}; {plot_title_facility}"),
      subtitle = agegroup_title
    )

  
  
}

all_districts <- c("All", unique(malaria_data$District))

# data frame of location names by district
facility_list <- malaria_data %>%
  group_by(location_name, District) %>%
  summarise() %>% 
  ungroup()

