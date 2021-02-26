library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(stringr)
library(shiny)

co2data <- read.csv("/home/tommy/projects/rproject4/owid-co2-data.csv")
codebook <- read.csv("/home/tommy/projects/rproject4/owid-co2-codebook.csv")

# step 0 : analyze for severity of missing data
column_missing_data <- co2data %>%
  select(everything()) %>%  
  summarise_all(funs(sum(is.na(.)))) 
column_missing_data_summary <- as.data.frame(t(column_missing_data))

# Variables for analysis
# co2
# co2_growth_abs
# co2_per_capita
# share_global_co2
# share_global_cumulative_co2
# co2_per_gdp
# coal_co2
# oil_co2
# coal_co2_per_capita
# oil_co2_per_capita
# share_global_coal_co2
# share_global_oil_co2
# cumulative_coal_co2
# cumulative_oil_co2
# population

# step 1 : find top 10 countries that have the most co2 through all the years
top_country_yearly_total <- co2data %>%
  filter(country != "World" & country != "Europe" & country != "Asia" &
           country != "North America" & country != "EU-28" & country != "EU-27" &
           country != "Europe (excl. EU-27)" & country != "Europe (excl. EU-28)" &
           country != "Asia (excl. China & India)" & 
           country != "North America (excl. USA)" & country != "International transport") %>% 
  group_by(country) %>%
  summarize(total_co2 = sum(co2, na.rm=TRUE)) %>%
  select(country, total_co2) %>%
  arrange(-total_co2) %>%
  slice(1:10)

topCountries_total <- inner_join(co2data, top_country_yearly_total, by="country") %>%
  filter(year >= 1900) %>%
  select(country, year, co2, co2_growth_abs, co2_per_capita, share_global_co2, 
         share_global_cumulative_co2, co2_per_gdp, coal_co2, oil_co2,
         coal_co2_per_capita, oil_co2_per_capita, share_global_coal_co2,
         share_global_oil_co2, cumulative_coal_co2, cumulative_oil_co2, population)

topCountries_total_co2_plot <- ggplot(data = topCountries_total, aes(x=year, y=co2, group=country)) +
  ggtitle("Annual production-based emissions of carbon dioxide (CO2) ") +
  xlab("Year") + ylab("million tonnes") +
  geom_line(aes(linetype="solid", color=country, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# step 2 : top countries co2_oil
ggplot(data = topCountries_total, aes(x=year, y=oil_co2, group=country)) +
  ggtitle("CO2 emissions from oil production") +
  xlab("Year") + ylab("million tonnes") +
  geom_line(aes(linetype="solid", color=country, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# step 3 : top countries coal_oil
ggplot(data = topCountries_total, aes(x=year, y=coal_co2, group=country)) +
  ggtitle("CO2 emissions from coal production") +
  xlab("Year") + ylab("million tonnes") +
  geom_line(aes(linetype="solid", color=country, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")




ggplot(data = state_latinx, aes(x=year, y=total_latinx_jail_pop, group=state)) +
  geom_line(aes(linetype="solid", color=state, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")





#------------------------------------------------------------------------------------
# Step 1
white_and_poc_all_years <- incarceration_trends %>%
  group_by(year) %>%
  summarize(total_white_jail_pop = sum(white_jail_pop, na.rm=TRUE),
            total_aapi_jail_pop = sum(aapi_jail_pop, na.rm=TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm=TRUE),
            total_other_race_jail_pop = sum(other_race_jail_pop, na.rm=TRUE)
  ) %>%
  select(year, white=total_white_jail_pop, aapi=total_aapi_jail_pop, black=total_black_jail_pop, 
         latinx=total_latinx_jail_pop, native=total_native_jail_pop, 
         other_race=total_other_race_jail_pop)  

white_and_poc_all_years_summary <- white_and_poc_all_years %>%
  gather(key=race, value=jail_pop, white, black, latinx, aapi, native, other_race) %>%
  select(year, race, jail_pop)

ggplot(data = white_and_poc_all_years_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 2 

white_and_poc <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(year) %>%
  summarize(total_white_jail_pop = sum(white_jail_pop, na.rm=TRUE),
            total_aapi_jail_pop = sum(aapi_jail_pop, na.rm=TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm=TRUE),
            total_other_race_jail_pop = sum(other_race_jail_pop, na.rm=TRUE)
  ) %>%
  mutate(total_non_white_jail_pop = total_aapi_jail_pop + total_black_jail_pop +
           total_latinx_jail_pop + total_native_jail_pop + total_other_race_jail_pop) %>%
  select(year, white=total_white_jail_pop, non_white=total_non_white_jail_pop, aapi=total_aapi_jail_pop, black=total_black_jail_pop, 
         latinx=total_latinx_jail_pop, native=total_native_jail_pop, 
         other_race=total_other_race_jail_pop)
#  gather(key = race, value = jail_pop, white, non_white, aapi, black, latinx, native, other_race) %>%
#  select(year, race, jail_pop)
View(white_and_poc)

# Step 3 
white_vs_non_white_summary <- white_and_poc %>%
  gather(key=race, value=jail_pop, white, non_white) %>%
  select(year, race, jail_pop)

ggplot(data = white_vs_non_white_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 4
white_vs_each_race_summary <- white_and_poc %>%
  gather(key=race, value=jail_pop, white, black, latinx, aapi, native, other_race) %>%
  select(year, race, jail_pop)
ggplot(data = white_vs_each_race_summary, aes(x=year, y=jail_pop, group=race)) +
  geom_line(aes(linetype="solid", color=race, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 5

# This shows the top 5 states that has the highest latinx incarceration rate since 1985, 
# it consistently shows FL, NY, AZ, NJ, MA
state_latinx <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(year, state) %>%
  summarize(total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE)) %>%
  select(year, state, total_latinx_jail_pop) %>%
  arrange(-total_latinx_jail_pop) %>%
  slice(1:5)
ggplot(data = state_latinx, aes(x=year, y=total_latinx_jail_pop, group=state)) +
  geom_line(aes(linetype="solid", color=state, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

# Step 6
# This shows the top 5 states that has the highest black incarceration rate since 1985, 
state_black <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(year, state) %>%
  summarize(total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE)) %>%
  select(year, state, total_black_jail_pop) %>%
  arrange(-total_black_jail_pop) %>%
  slice(1:5)
View(state_black)
ggplot(data = state_black, aes(x=year, y=total_black_jail_pop, group=state)) +
  geom_line(aes(linetype="solid", color=state, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")


# Step 7

# full join to find common top 5 states and which are not
state_latinx_black <- full_join(state_latinx, state_black, by=c("year", "state")) %>%
  select(year, state, latinx = total_latinx_jail_pop, black = total_black_jail_pop) %>%
  gather(key=race, value=total_jail, latinx, black) %>%
  mutate(race_in_state = paste(race, state, sep="::")) %>%
  filter(!is.na(total_jail)) %>%
  arrange(year, race_in_state) %>%
  select(year, race_in_state, total_jail)
View(state_latinx_black)
ggplot(data = state_latinx_black, aes(x=year, y=total_jail, group=race_in_state)) +
  geom_line(aes(linetype="solid", color=race_in_state, size=1)) +
  geom_point() +
  theme(legend.position = "right") +
  guides(color=guide_legend(override.aes = list(size=3)))
#  scale_color_brewer(palette = "Greens")

# STEP 7A: scatter plot to compare latinx::CA and black::CA
CA_latinx_black <- state_latinx_black %>%
  filter(race_in_state == "latinx::CA" | race_in_state == "black::CA")
View(CA_latinx_black)
ggplot(CA_latinx_black, aes(x=year, y=total_jail, shape=race_in_state, color=race_in_state )) +
  geom_point()


# TODO : following is work in progress


# Step 8 : obtain state geo long/lat definition and state abbreviation, 
#          and define centralized coordinate for each state
us_states <- map_data("state")
us_states$state <- state.abb[match(str_to_title(us_states$region), state.name)]
View(us_states)

centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
centroids$state<-state.abb[match(centroids$region,tolower(state.name))]
View(centroids)

View(us_states)

# step 9 : summarize black total jail by state since 1985
black_by_states_summary <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(state) %>%
  summarize(total_black_jail_pop = sum(black_jail_pop, na.rm=TRUE)
  ) %>%
  select(state, total_black_jail_pop)
black_by_states_summary$black_jail_pop_range <- cut(black_by_states_summary$total_black_jail_pop, 12)
View(black_by_states_summary)



# Step 10 : merge black_by_states_summary, and us_states by state
black_by_states_summary_merged <- merge(us_states, black_by_states_summary, by="state")
View(black_by_states_summary_merged)

black_by_states_summary_merged_plot <- ggplot(black_by_states_summary_merged, 
                                              aes(x=long, y=lat, group=state, fill=black_jail_pop_range, color=black_jail_pop_range)) +
  geom_polygon(color="grey", size=0.05) + coord_equal() +
  with(centroids,
       annotate(geom="text", x=long, y=lat, label=state, size=4, color="white")
  ) +
  scale_fill_brewer(palette = "Spectral")
plot(black_by_states_summary_merged_plot)

# step 11 : summarize latinx total jail by state since 1985
latinx_by_states_summary <- incarceration_trends %>%
  filter(year >= 1985) %>%
  group_by(state) %>%
  summarize(total_latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE)
  ) %>%
  select(state, total_latinx_jail_pop)
latinx_by_states_summary$latinx_jail_pop_range <- cut(latinx_by_states_summary$total_latinx_jail_pop, 12)
View(latinx_by_states_summary)

# Step 12 : merge latinx_by_states_summary, and us_states by state
latinx_by_states_summary_merged <- merge(us_states, latinx_by_states_summary, by="state")

ggplot(latinx_by_states_summary_merged, 
       aes(x=long, y=lat, group=state, fill=latinx_jail_pop_range, color=latinx_jail_pop_range)) +
  geom_polygon(color="grey", size=0.05) + coord_equal() +
  with(centroids,
       annotate(geom="text", x=long, y=lat, label=state, size=4, color="white")
  ) +
  scale_fill_brewer(palette = "Spectral")



