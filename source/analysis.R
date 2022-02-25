setwd("~/_Code/a3-noah-rarick/source")
incarceration <- read.csv("incarceration-trends/incarceration_trends.csv")
View(incarceration)

library("dplyr")
incarceration_by_race <- incarceration %>%
                         select(total_pop_15to64 , latinx_pop_15to64, aapi_pop_15to64, black_pop_15to64, native_pop_15to64, white_pop_15to64)
incarceration_by_race <- incarceration_by_race[complete.cases(incarceration_by_race),]
incarceration_by_race <- incarceration_by_race %>%
                         mutate(total_asian_pop_15to64 = sum(aapi_pop_15to64)) %>%
                         mutate(total_white_pop_15to64 = sum(white_pop_15to64)) %>%
                         mutate(total_black_pop_15to64 = sum(black_pop_15to64)) %>%
                         mutate(total_native_pop_15to64 = sum(native_pop_15to64))%>%
                         mutate(total_latinx_pop_15to64 = sum(latinx_pop_15to64)) %>%
                         mutate(sum_total_pop_15to64 = sum(total_pop_15to64)) %>%
                         mutate(prop_black_15to64 = total_black_pop_15to64 / sum_total_pop_15to64) %>%
                         mutate(prop_white_15to64 = total_white_pop_15to64 / sum_total_pop_15to64) %>% 
                         mutate(prop_asian_15to64 = total_asian_pop_15to64 / sum_total_pop_15to64) %>%
                         mutate(prop_native_15to64 = total_native_pop_15to64 / sum_total_pop_15to64) %>%
                         mutate(prop_latinx_15to64 = total_latinx_pop_15to64 / sum_total_pop_15to64)
View(incarceration_by_race)

summary_info <- list()
summary_info$proportion_black_15to64 <- incarceration_by_race$prop_black_15to64[1]
summary_info$proportion_white_15to64 <- incarceration_by_race$prop_white_15to64[1]
summary_info$proportion_asian_15to64 <- incarceration_by_race$prop_asian_15to64[1]
summary_info$proportion_native_15to64 <-incarceration_by_race$prop_native_15to64[1]
summary_info$proportion_latinx_15to64 <- incarceration_by_race$prop_latinx_15to64[1]
                                        
View(summary_info)
incarceration_inca_over_time <- incarceration %>%
                                filter(state == "CA") %>%
                                filter(total_pop > 1000000)
library("ggplot2")
View(incarceration_inca_over_time)
p <- ggplot(data = incarceration_inca_over_time, aes(x = year, y = total_pop, color = county_name)) + geom_point() + ylab("Total Incarcerated Population") + xlab("Year") + labs(color = "County Name") + ggtitle("Highest 9 California Counties in Incarcerated Population Over Time")

#Displaying 
black_pop_v_total_pop <- incarceration %>%
                         mutate(prop_black = black_pop_15to64 / total_pop, na.rm = TRUE) %>%
                         mutate(prop_white = white_pop_15to64 / total_pop, na.rm = TRUE) %>%
                         filter(year == "2018")
View(black_pop_v_total_pop)
black_pop_over_time <- ggplot(black_pop_v_total_pop, aes(x = prop_black, y = prop_white, color = region)) + geom_point() + 
  labs (
    title = "Proportion of White Incarceration Compared to Proportion of Black Incarceration by Region",
    x = "Proportion of Black Incarcerated Individuals",
    y = "Proportion of White Incarcerated Individuals",
    color = "Region"
  )
prop_in_states <- black_pop_v_total_pop %>%
              mutate(state = abbr2state(state)) %>%
              mutate(state = tolower(state)) 
View(prop_in_states)
state_shape <- map_data("state") %>%
  dplyr::rename(state = region) %>%
  left_join(prop_in_states, by = "state")

prop_map <- ggplot(state_shape) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prop_black, size = .1)) + 
  labs(
  y = "Longitude",
  x = "Latitude",
  title = "Proportion of Black Incarceration Across All US States",
  fill = "Proportion of Black Incarceration")

View(prop_in_wa)
install.packages(usdata)
library(usdata)
MainStates <- map_data("state")
ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )
View(MainStates)
MainStates <- rename(MainStates, state = region)
state.abb[match(MainStates$region,state.name)]
View(black_pop_v_total_pop)
MergedStates <- inner_join(MainStates, black_pop_v_total_pop, by = "state")
View(MergedStates)
ggplot() +
  geom_polygon( data = black_pop_v_total_pop,
                aes(x=long, y=lat, group = group, fill = prop_black),
                color = "white", size = .2) 
