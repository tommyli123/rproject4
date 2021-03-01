library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(stringr)
library(tidyverse)
library(shiny)

co2data <- read.csv("owid-co2-data.csv")
codebook <- read.csv("owid-co2-codebook.csv")
# google centroid data source : https://raw.githubusercontent.com/google/dspl/master/samples/google/canonical/countries.csv
countryCentroids <- read.csv("google-country-centroids.csv")
countryCentroids_enhanced <- countryCentroids %>%
  mutate(country = name)

world <- map_data("world") %>%
  mutate(country = region)
world$country[world$country == "UK"] <- "United Kingdom"

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

topCountries_total_oil_co2_plot <- ggplot(data = topCountries_total, aes(x=year, y=oil_co2, group=country)) +
  ggtitle("CO2 emissions from oil production") +
  xlab("Year") + ylab("million tonnes") +
  geom_line(aes(linetype="solid", color=country, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

topCountries_total_coal_co2_plot <- ggplot(data = topCountries_total, aes(x=year, y=coal_co2, group=country)) +
  ggtitle("CO2 emissions from coal production") +
  xlab("Year") + ylab("million tonnes") +
  geom_line(aes(linetype="solid", color=country, size=1)) +
  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "right")

topCountries_co2_per_person <- topCountries_total %>%
  filter(year >= 1900) %>%
  mutate(co2_per_person = co2 / population * 1000000) %>%
  select(country, year, co2_per_person)

topCountries_co2_per_person_plot <- ggplot(data = topCountries_co2_per_person, aes(x=year, y=co2_per_person, group=country)) +
  ggtitle("Top 10 countries CO2 emission per person") +
  xlab("Year") + ylab("CO2 emission per person-year (tons)") +
  geom_line(aes(linetype="solid", color=country)) +
  geom_label(aes(label = country), data = topCountries_co2_per_person %>% filter(year == max(year)),nudge_x = 0.35, size = 4) +
  #  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) 

topCountries_coal_co2_per_person <- topCountries_total %>%
  filter(year >= 1900) %>%
  mutate(coal_co2_per_person = coal_co2 / population * 1000000) %>%
  select(country, year, coal_co2_per_person)
topCountries_coal_co2_per_person_plot <- ggplot(data = topCountries_coal_co2_per_person, aes(x=year, y=coal_co2_per_person, group=country)) +
  ggtitle("Top 10 countries coal CO2 emission per person") +
  xlab("Year") + ylab("CO2 coal emission per person-year (tons)") +
  geom_line(aes(linetype="solid", color=country)) +
  geom_label(aes(label = country), data = topCountries_coal_co2_per_person %>% filter(year == max(year)),nudge_x = 0.35, size = 4) +
  guides(color=guide_legend(override.aes = list(size=3))) 

topCountries_oil_co2_per_person <- topCountries_total %>%
  filter(year >= 1900) %>%
  mutate(oil_co2_per_person = oil_co2 / population * 1000000) %>%
  select(country, year, oil_co2_per_person)
topCountries_oil_co2_per_person_plot <- ggplot(data = topCountries_oil_co2_per_person, aes(x=year, y=oil_co2_per_person, group=country)) +
  ggtitle("Top 10 countries oil CO2 emission per person") +
  xlab("Year") + ylab("CO2 oil emission per person-year (tons)") +
  geom_line(aes(linetype="solid", color=country)) +
  geom_label(aes(label = country), data = topCountries_oil_co2_per_person %>% filter(year == max(year)),nudge_x = 0.35, size = 4) +
  guides(color=guide_legend(override.aes = list(size=3))) 

topCountries_coal_co2_contribution_to_gdp <- topCountries_total %>%
  filter(year >= 1900) %>%
  mutate(coal_co2_contribution_to_gdp = coal_co2 / co2 * co2_per_gdp) %>%
  select(country, year, coal_co2_contribution_to_gdp)
topCountries_coal_co2_contribution_to_gdp_plot <- ggplot(data = topCountries_coal_co2_contribution_to_gdp, aes(x=year, y=coal_co2_contribution_to_gdp, group=country)) +
  ggtitle("Top 10 countries coal CO2 contribution to per GDP") +
  xlab("Year") + ylab("Coal CO2 contribution to per GDP (million tons)") +
  geom_line(aes(linetype="solid", color=country)) +
  geom_label(aes(label = country), data = topCountries_coal_co2_contribution_to_gdp %>% filter(year == max(year)),nudge_x = 0.35, size = 4) +
  guides(color=guide_legend(override.aes = list(size=3))) 

topCountries_oil_co2_contribution_to_gdp <- topCountries_total %>%
  filter(year >= 1900) %>%
  mutate(oil_co2_contribution_to_gdp = oil_co2 / co2 * co2_per_gdp) %>%
  select(country, year, oil_co2_contribution_to_gdp)
topCountries_oil_co2_contribution_to_gdp_plot <- ggplot(data = topCountries_oil_co2_contribution_to_gdp, aes(x=year, y=oil_co2_contribution_to_gdp, group=country)) +
  ggtitle("Top 10 countries oil CO2 contribution to per GDP") +
  xlab("Year") + ylab("Oil CO2 contribution to per GDP (million tons)") +
  geom_line(aes(linetype="solid", color=country)) +
  geom_label(aes(label = country), data = topCountries_oil_co2_contribution_to_gdp %>% filter(year == max(year)),nudge_x = 0.35, size = 4) +
  #  geom_point() +
  guides(color=guide_legend(override.aes = list(size=3))) 




# UI definitions
ui <- fluidPage(
  
  # App title ----
  titlePanel("Global CO2 emission analysis"),
  
  tabsetPanel(
    tabPanel("Summary", fluid=TRUE,
             mainPanel(
               fluidRow(
                 column(12, htmlOutput("introduction"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesOverallCo2Section"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesOverallCo2Plot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesOilCo2Section"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesOilCo2Plot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesCoalCo2Section"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesCoalCo2Plot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesCo2PerPersonSection"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesCo2PerPersonPlot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesOilCo2PerPersonSection"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesOilCo2PerPersonPlot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesCoalCo2PerPersonSection"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesCoalCo2PerPersonPlot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesOilCo2ContributionToGdpSection"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesOilCo2ContributionToGdpPlot"))  
               ),
               fluidRow(
                 column(12, htmlOutput("topCountriesCoalCo2ContributionToGdpSection"))  
               ),
               fluidRow(
                 column(12, plotOutput("topCountriesCoalCo2ContributionToGdpPlot"))  
               )
             )
    ),
    tabPanel("global CO2 distribution", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 # textInput("year", "Year [1910-2020] :", value=2020, width=NULL, placeholder=NULL)           
                 sliderInput("year", "Since 1900 to ", min=1900, max=2020, value=2020, step=10, sep=''),
                 selectInput("type", "data type", 
                             c("overall CO2" = "total_co2",
                               "coal CO2" = "coal_co2",
                               "oil CO2" = "oil_co2"
                             )
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(12, plotOutput("worldMap"))  
                 ),
                 fluidRow(
                   column(12, htmlOutput("worldMapInsight"))  
                 )
               )           
             )
    )
  )  
)
# Server logic
server <- function(input, output) {
  
  output$introduction <- renderUI(
    tags$div(
      h2("Introduction"),
      p("Biltong ham flank, venison brisket capicola hamburger porchetta jowl turkey bacon picanha cow landjaeger prosciutto. 
        Pork flank cow spare ribs strip steak frankfurter, pork belly tongue pig alcatra ball tip. "),
      p("Shankle turkey pastrami, jerky andouille hamburger pork bacon bresaola venison. Meatloaf ground round filet mignon leberkas sirloin. 
        Pig frankfurter turducken sausage pork chop rump pork biltong short ribs ham hock swine burgdoggen flank. 
        Fatback pork brisket cupim jerky pork chop. Salami rump jowl strip steak porchetta spare ribs pork belly beef ribs ham hock tongue fatback cow. 
        Alcatra tenderloin t-bone turkey.")
    )
  )
  
  output$topCountriesOverallCo2Section <- renderUI(
    tags$div(
      h3("Top countries with most overall CO2 emission"),
      p("Spicy jalapeno turducken buffalo bacon prosciutto. Alcatra t-bone pancetta meatball kielbasa, brisket bacon ball tip kevin tongue burgdoggen strip steak. ")
    )
  )
  output$topCountriesOverallCo2Plot <- renderPlot(topCountries_total_co2_plot)
  
  output$topCountriesOilCo2Section <- renderUI(
    tags$div(
      h3("Top countries with most oil CO2 emission"),
      p("Tri-tip picanha andouille, tongue ball tip chuck tenderloin capicola short ribs filet mignon porchetta landjaeger.")
    )
  )
  output$topCountriesOilCo2Plot <- renderPlot(topCountries_total_oil_co2_plot)
  
  output$topCountriesCoalCo2Section <- renderUI(
    tags$div(
      h3("Top countries with most coal CO2 emission"),
      p("Hamburger venison tenderloin andouille, shank ribeye ball tip tail turducken tri-tip meatloaf ham hock bresaola landjaeger buffalo.")
    )
  )
  output$topCountriesCoalCo2Plot <- renderPlot(topCountries_total_coal_co2_plot)
  
  output$topCountriesCo2PerPersonSection <- renderUI(
    tags$div(
      h3("Top countries CO2 emission per persion"),
      p("Drumstick jowl ground round, pig alcatra ham hock corned beef sirloin tail rump pork chop ham strip steak. Chicken tenderloin bresaola biltong tongue.")
    )
  )
  output$topCountriesCo2PerPersonPlot <- renderPlot(topCountries_co2_per_person_plot)
  
  output$topCountriesOilCo2PerPersonSection <- renderUI(
    tags$div(
      h3("Top countries oil CO2 emission per persion"),
      p("Sirloin jerky pork venison kevin filet mignon. Chicken corned beef jowl tail beef ribs pancetta drumstick tri-tip turkey kevin meatloaf.")
    )
  )
  output$topCountriesOilCo2PerPersonPlot <- renderPlot(topCountries_oil_co2_per_person_plot)
  
  output$topCountriesCoalCo2PerPersonSection <- renderUI(
    tags$div(
      h3("Top countries coal CO2 emission per persion"),
      p("Jerky ham chicken sausage, kevin short ribs beef salami. Venison doner turkey meatball short ribs, flank chislic alcatra ground round. Jerky strip steak bacon, beef ribs kevin pig picanha ribeye tenderloin ham brisket venison chislic chuck.")
    )
  )
  output$topCountriesCoalCo2PerPersonPlot <- renderPlot(topCountries_coal_co2_per_person_plot)
  
  output$topCountriesOilCo2ContributionToGdpSection <- renderUI(
    tags$div(
      h3("Top countries oil CO2 emission contribution to GDP"),
      p("Shoulder tail short ribs hamburger sirloin pork leberkas brisket tri-tip. Doner ground round chislic chuck meatloaf swine. Bacon fatback kevin landjaeger boudin chicken andouille spare ribs flank ham meatloaf ground round. ")
    )
  )
  output$topCountriesOilCo2ContributionToGdpPlot <- renderPlot(topCountries_oil_co2_contribution_to_gdp_plot)
  
  output$topCountriesCoalCo2ContributionToGdpSection <- renderUI(
    tags$div(
      h3("Top countries coal CO2 emission contribution to GDP"),
      p("Meatball ham hock pork chop bacon beef ribs chislic chuck tail spare ribs shankle shoulder.")
    )
  )
  output$topCountriesCoalCo2ContributionToGdpPlot <- renderPlot(topCountries_coal_co2_contribution_to_gdp_plot)
  
  
  
  output$worldMap <- renderPlot({
    
    all_countries_yearly_total <- co2data %>%
      filter(country != "World" & country != "Europe" & country != "Asia" &
               country != "North America" & country != "EU-28" & country != "EU-27" &
               country != "Europe (excl. EU-27)" & country != "Europe (excl. EU-28)" &
               country != "Asia (excl. China & India)" & 
               country != "North America (excl. USA)" & country != "International transport") %>%
      filter(year <= input$year) %>% 
      group_by(country) %>%
      summarize(total_co2 = sum(co2, na.rm=TRUE), 
                total_oil_co2 = sum(oil_co2, na.rm=TRUE),
                total_coal_co2 = sum(coal_co2, na.rm=TRUE)) %>%
      arrange(country) %>%
      select(country, total_co2, total_oil_co2, total_coal_co2)    
    
    world_all_countries_yearly_total_merged <- merge(countryCentroids_enhanced, all_countries_yearly_total, by="country")
    
    millionTonnes <- reactive({
      if ("total_co2" == input$type) return(world_all_countries_yearly_total_merged$total_co2)
      if ("coal_co2" == input$type) return(world_all_countries_yearly_total_merged$total_coal_co2)
      if ("oil_co2" == input$type) return(world_all_countries_yearly_total_merged$total_oil_co2)
    })
    title_data_type_to_use <- reactive({
      if ("total_co2" == input$type) return("Global overall CO2")
      if ("coal_co2" == input$type) return("Global coal CO2")
      if ("oil_co2" == input$type) return("Global oil CO2")
    })
    
    ggplot() +
      geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region), 
        color = "white", fill = "lightblue", size = 0.1
      ) +
      geom_point(
        data = world_all_countries_yearly_total_merged,
        aes(longitude, latitude, size=millionTonnes()),
        alpha = 5
      ) +
      scale_size(range=c(1, 15)) +
      theme_void() +
      labs(title=paste(title_data_type_to_use(), "emission cumulation from year 1900 to", input$year))   
  })

  output$worldMapInsight <- renderUI(
    tags$div(
      h3("Insights from the world map data"),
      p("Apple pie apple pie dragée candy canes toffee. Apple pie sweet roll powder. 
        Sesame snaps cake cake icing gummies tootsie roll. Jelly bonbon ice cream apple pie. 
        Dessert cupcake oat cake. Candy canes sweet cheesecake sesame snaps macaroon tootsie 
        roll danish candy canes. Liquorice ice cream tart jelly beans wafer gummies cake 
        bear claw. Halvah biscuit cake pudding dragée dessert lemon drops cake. 
        Apple pie halvah croissant brownie chocolate bar brownie donut pie. Bonbon bear claw 
        biscuit apple pie tiramisu. Sweet roll pastry pie cheesecake halvah candy cake pudding. 
        Soufflé tart toffee jelly-o muffin brownie danish cupcake. Candy canes bear claw 
        bonbon liquorice tiramisu halvah halvah jelly beans tootsie roll.")
    )
  )
  
}
shinyApp(ui = ui, server = server)