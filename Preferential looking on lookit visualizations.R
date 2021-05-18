#### ICIS Online Testing Webinar Script Overview #### 
  # Overview:     R script that creates interactive timeline visualization and geographic representations of study participation by State
  # 
  # Written by:   Aaron G. Beckner
  #               PhD Student
  #               UC Davis Infant Cognition Laboratory
  #               agbeckner@ucdavis.edu
  # 
  # Data files needed:
  #               your-study-demographic-snapshots.csv: file that contains the demographics in the format they are exported in from lookit
  #               States Key.csv: file that contains "dummy codes" for states so that we can use what is in lookit to create our geographic plots
  # 
  # Note that if you have the States Key and demographic snapshot from lookit you should be able to generate all the figures in this file, 
  # but if you want to modify the timeline you will have to do that manually
  # 
#### Setting up Shop ####
  # specify packages
  packages <- c("caret","ggmap","RColorBrewer",
                "matrixStats","usmap","vistime",
                "plotly","tidyverse","stringr",
                "maps","sf","sp","ggiraph","rspatial",
                "magrittr","highcharter") 
  
  # load packages
  lapply(packages, library, character.only = TRUE) 

#### Time-line Figure #### 
  # specify timeline variables and dates
  timeline_data <- data.frame(event = c(
    "Discovered Lookit",
    "Started Lookit Tutorial",
    "IRB Approval for Online Data Collection",
    "Created Baby Tetris",
    "Submitted Baby Tetris for First Review",
    "Submitted Baby Tetris for Second Review",
    "Baby Tetris Approved by Kim Scott",
    "Data Collection",
    "Manuscript Preparation"), 
    start = c(
      "2020-03-07",
      "2020-05-12",
      "2020-06-19",
      "2020-07-19",
      "2020-08-18",
      "2020-08-28",
      "2020-08-31",
      "2020-09-01",
      "2021-01-20"), 
    end = c(
      "2020-03-14",
      "2020-06-19",
      "2020-07-19",
      "2020-08-18",
      "2020-08-28",
      "2020-08-31",
      "2020-09-01",
      "2021-01-20",
      "2021-05-03"), 
    group = "")
  
  # create visualization
  p <- vistime(timeline_data, optimize_y = FALSE, linewidth = 45) 
  
  # build interactive visualization in plotly
  lookit_timeline <- plotly_build(p)
  
  # loop through the x-axes and change the font size for each element
  for(i in grep(pattern = "yaxis*", names(lookit_timeline$x$layout))){
    yax <- lookit_timeline$x$layout[[i]]
    yax$tickfont <- list(size = 28)
    lookit_timeline$x$layout[[i]] <- yax
  }
  # specify size for x-axses
  lookit_timeline$x$layout$xaxis$tickfont <- list(size = 28)
  
  # loop through the labels and change the font size for each element
  lookit_timeline$x$data <- lapply(lookit_timeline$x$data, function(x){
    if(x$mode == "text"){
      x$textfont$size <- 28
      return(x)
    }else{
      return(x)
    }})
  
  # print figure
  lookit_timeline
  
#### Geographic Figure #### 
  # specify working directory where your lookit demographic snapshot csv is located
  # for details, see documentation at https://lookit.readthedocs.io/en/develop/researchers-experiment-data.html)
  demoData <- "~/Downloads/"
  setwd(demoData)
  
  # grab key codes for states
  state_key <- read.csv("State Key.csv",header = T) %>%
    mutate(postal = tolower(postal),state = tolower(state))
  
  # grab demographics for full sample
  demos <- read.csv("your-study_all-demographic-snapshots.csv",header= T,sep = ",") %>%
    filter(demographic__country == "US") %>%
    rename(postal = demographic__state) %>%
    mutate(postal = tolower(postal)) %>%
    group_by(postal) %>%
    summarize(n = n_distinct(response__uuid))
  
  # merge demographics with state key
  data <- merge(demos,state_key, by = c("postal")) %>%
    rename(region = state)
  
  # full country
  us <- us_map(regions = c("states")) %>%
    rename(long = x, lat = y, region = full) %>%
    mutate(region = tolower(region))
  
  # combine all data sources
  final_data <- inner_join(us,data, by = "region",all = T) 
  
  # ggplot state
  us_base <- ggplot(data = us, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1) + 
    geom_polygon(color = "white", fill = "gray")

  # ditch the axes
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
  
  # add counts
  us_counts <- us_base + 
    geom_polygon(data = final_data, aes(x = long,y = lat,fill = n,group = group),color = "white") +
    theme_bw() +
    scale_fill_distiller(palette = "RdYlBu")+
    ditch_the_axes 
  us_counts

#### Interactive Geographic Figure #### 
  # reformat state labels for interactive figure
  interactive_map_data <- data %>%
    mutate(region = str_to_title(region)) 
    
  # interactive figure using plotly (can render in HTML)
  hcmap(
    map = "countries/us/us-all", data = interactive_map_data,
    joinBy = c("name","region"), value = "n", name = "Sample Size",
    borderColor = "#FAFAFA", borderWidth = 0.1,
    tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "")) %>%
    hc_title(text = "Participants in Baby Tetris Across the United States" )

  
  
  
  
  
  
