#download and plot seismic data over time
#required libraries ----
library(dplyr)
library(raster)
library(marmap)
library(plotly)
library(htmlwidgets)


### define start and end date of visualisation ---- 
# Define the start and end dates
start_date <- as.Date("2025-01-24") #add custom date here
end_date <- as.Date(format(Sys.time(), "%Y-%m-%d"))  # Get today's date


### identify and load existing data ----
file_name<-'earthquake_data.RDS'
if (file.exists(file_name)) {
#load existing data
df<-readRDS(file_name)
start_date<-as.Date(max(df$originTime.UTC.))+1
}

#### load data if the dataset is outdated ----
if (!(end_date-start_date==0)) {
  # Initialize an empty dataframe
  earthquake_data <- data.frame()
  duration<-seq(start_date, end_date, by = "day")
  # Loop through each day and fetch data
  for (i in 1:length(duration)-1) {
    
    # Format the start and end times for the API request
    formatted_start <- paste0(format(duration[i], "%Y-%m-%d"), "T00%3A00")
    formatted_end <- paste0(format(duration[i], "%Y-%m-%d"), "T23%3A59")
    
    # Construct the API request URL
    url <- paste0("https://eida.gein.noa.gr/fdsnws/event/1/query?",
                  "starttime=", formatted_start, 
                  "&endtime=", formatted_end, 
                  "&minlatitude=36&maxlatitude=37.5",
                  "&minlongitude=25&maxlongitude=26.5",
                  "&format=csv&formatted=true&nodata=404")
    
    # Try to read the data and append it to the dataframe
    daily_data <- tryCatch({
      read.csv(url)
    }, error = function(e) NULL)  # Handle errors (e.g., no data)
    
    # If data exists, append it to the main dataframe
    if (!is.null(daily_data) && nrow(daily_data) > 0) {
      earthquake_data <- bind_rows(earthquake_data, daily_data)
      print(paste("Added data for", duration[i]))
    } else {
      print(paste("No data for", duration[i]))
    }
  }
  
  # Print dimensions of collected data
  print(dim(earthquake_data))  
  
  #add data to existing
  df<-bind_rows(df,earthquake_data)
  #update the old file
  saveRDS(df,file_name)
}

### data handling ----
df$day<-as.Date(df$originTime.UTC.)
df$day<-as.character(df$day)
### create daily boxplots ----
avdday<-ggplot(df)+
  geom_point(aes(x=day, y=magnitude), na.rm=TRUE)+
  geom_boxplot(aes(x=((as.character(day))), y=as.numeric(magnitude)))+
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.1))+
  labs(x='date', y='magnitude')+
  annotate("text",
           x = (as.character(unique(((df$day))))),
           y = aggregate(as.numeric(magnitude) ~ as.character(day), df, median)[ , 2],
           label = table((df$day)),
           col = "red",
           vjust = - 1)

#save figure
ggsave("ggplot_image.png", plot = avdday, width = 8, height = 6)

### download elevation data and handling ----
baltic_bathy <- getNOAA.bathy(25, 26.5, 36, 37, resolution = 0.1)
longitudes <- as.numeric(rownames(baltic_bathy))  # Extract  longitudes from row names
latitudes<- as.numeric(colnames(baltic_bathy))  # Extract latitudes from column names

depth_matrix <- as.matrix(baltic_bathy) #create a depth matrix for visualization

##Separate positive and negative depth values for different visualization
depth_matrix_positive <- depth_matrix
depth_matrix_positive[depth_matrix < 0] <- NA  # Keep land

depth_matrix_negative <- depth_matrix
depth_matrix_negative[depth_matrix >= 0] <- NA  # Keep water

###create the plot-ly plot for elevation data ----
# create the plotly interactive plot
fig <- plot_ly() %>%
  add_surface(
    x = latitudes,
    y = longitudes,
    z = depth_matrix_positive / 1000,  # Convert to km
    colorscale = 'Earth',
    showscale = TRUE,
    opacity = 1,  #  earth is colorful
    colorbar = list(title = "Elevation (km)" ,x=-0.1,name = "Elevation Surface")
  ) %>%
  
  # add water
  add_surface(
    x = latitudes,
    y = longitudes,
    z = depth_matrix_negative / 1000,  # convert to km
    colorscale = 'Blues',  # water is blue
    showscale = FALSE,  # no scale there
    opacity = 0.5  # water opacity
  )
# Step 4: Add earthquake points (animated over time)
fig <- fig %>%
  add_trace(
    data = df,
    x = ~latitude,  #i messed up one axis
    y = ~longitude,
    z = ~-depth,  
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 5, 
      color = ~magnitude, 
      colorscale = 'Viridis', 
      cmin = 0,  
      cmax = 6,
      showscale = TRUE,
      colorbar = list(title = "Magnitude",name = "Magnitude")
    ),
    showlegend= FALSE,
    frame = ~as.character(day),  # Animate over time
    # Add hover text
    text = ~paste('Magnitude: ', round(magnitude,2), '<br>Latitude: ', round(latitude,2), '<br>Longitude: ', round(longitude,2), '<br>Depth: ', round(depth,2)),
    hoverinfo = 'text'  # Display the custom hover text
  )

#Layout configuration
fig <- fig %>% layout(
  title = "3D Earthquake Visualization",
  scene = list(
    xaxis = list(title = 'Latitude', autorange = 'reversed'), 
    yaxis = list(title = 'Longitude'),
    zaxis = list(title = 'Depth', range = c(-20, 5))  # set depth limit
  )
)

### save the plot 
saveWidget(fig, "3dplot.html")

