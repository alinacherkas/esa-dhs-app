# this script provides plotting functions for the app

plot_map <- function(df.lambda){
  pal <- colorNumeric(palette = "YlOrRd", domain = NULL)
  
  values <- df.lambda %>% slot(name = "data") %>% pull(IndicatorValue)
  values <- case_when(is.na(values) ~ "Not Reported", T ~ paste(as.character(values), "%", sep = ""))
  
  labels <- sprintf("<strong>%s</strong><br>Region: %s<br>Value: %s<br><a href=''>Click to expand...</a>", 
                    df.lambda$CountryName, df.lambda$RegionID, values) %>% 
    lapply(htmltools::HTML)
  
  popups <- sprintf("<strong>%s</strong><br>Region: %s<br>Value: %s<br>Year: %d<br>Survey: %s",
                    df.lambda$CountryName, df.lambda$RegionID, values,
                    df.lambda$SurveyYear, df.lambda$SurveyType) %>% 
    lapply(htmltools::HTML)
  
  popups[values == "Not Reported"] <- values[values == "Not Reported"]
  
  m <- leaflet(df.lambda) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~pal(IndicatorValue),
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = T),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                popup = popups,
                popupOptions = popupOptions(closeOnClick = T)) %>% 
    setView(lng = 27, lat = -10, zoom = 4)
  return(m)
}

plot_hist <- function(df.lambda){
  average <- mean(df.lambda$IndicatorValue, na.rm = T)
  
  p <- ggplot(df.lambda, aes(x = IndicatorValue, text = paste("Value: ", IndicatorValue))) + 
    geom_histogram(binwidth = 1, color = "#FFFFFF", fill = "#d94b41") +
    scale_x_continuous(n.breaks = 10, limits = c(0, 100)) +
    geom_vline(xintercept = average, linetype = "dotted", color = "#000000", size = .5) +
    labs(title = "Figure 1. Distribution of Indicator Values", x = "Indicator Value [in %]", y = "Region Count")
  
  return(ggplotly(p, tooltip = c("text")))
}

plot_scatterplot <- function(df.lambda){
  df.lambda <- coordinates(df.lambda) %>% as_tibble() %>% 
    set_names(c("Lon", "Lat")) %>% 
    rowid_to_column() %>% 
    merge(., df.lambda %>% 
            slot(name = "data") %>% 
            select(IndicatorValue) %>%
            rowid_to_column()
    ) %>%
    drop_na()
  
  m <- lm(IndicatorValue ~ Lat, data = df.lambda)
  df.lambda %<>% mutate(Y_hat = fitted(m))
  p <- ggplot(df.lambda, aes(x = Lat, y = IndicatorValue)) + 
    geom_point(size = 2, color = "#d94b41", shape = 1, fill = "#d94b41") +
    geom_line(aes(x = Lat, y = Y_hat, group = 1,
                  text = sprintf("Latitude: %.2f\nPredicted Value: %.2f%%\nIntercept: %.2f\nSlope: %.2f\np-value: %.2f",
                                 Lat, Y_hat, coef(m)[1], coef(m)[2], summary(m)$coefficients[,4][2])),
              color = "#000000") +
    scale_x_continuous(n.breaks = 5, limits = c(-35, 15)) +
    scale_y_continuous(n.breaks = 5, limits = c(0, 100)) +
    labs(title = "Figure 2. Relationship between Latitude and Indicator Value", x = "Latitude", y = "Indicator Value [in %]")
  
  return(ggplotly(p, tooltip = c("text")))
}