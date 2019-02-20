
################################################################################
## Initialize Environment ######################################################
################################################################################

library(tidyverse)
library(sf)
library(plotly)
library(raster)
library(fasterize)
library(gstat)
library(rasterVis)


df2sf <- function(df,width,height){
  
  x_vec = c(0,1,1,0,0)*width
  y_vec = c(0,0,1,1,0)*height
  
  
  df %>%
    mutate(
      geometry =  map2(x,y,function(x_val,y_val){
        cbind(
          map2_dbl(x_vec,y_vec,~(.x+x_val)),
          map2_dbl(x_vec,y_vec,~(.y+y_val))
        )
      }
      )
    ) %>% 
    mutate(
      geometry = map(geometry,~st_polygon(list(.x))),
      geometry = st_sfc(geometry)
    ) %>%
    st_sf()
}


################################################################################
## Setup Study Design (create shapes) ##########################################
################################################################################


width = 2
height = 1.80
edge_width = 0.55
edge_height = 0.45

square_inner = height - edge_height*2

tische_df <- tibble(
  y = height*0:3,
  x = rep(0,4),
  tischnr = 1:4
)

tische_sf <- df2sf(tische_df,width,height)


logger_df <- tische_df %>%
  mutate(
    x = x + edge_width,
    y = y + edge_height
  )

logger_spacing = 0.45

logger_df %>%
  mutate(
    data = map2(x,y,function(x,y){
      x_org <- x
      y_org <- y
      x <- x_org+c(0,2)*logger_spacing
      y <- y_org+c(0,2)*logger_spacing
      crossing(x,y) %>%
        rbind(
          data_frame(
            x = x_org+1*logger_spacing,
            y = y_org+1*logger_spacing)
        ) %>%
        mutate(
          sensor = letters[1:nrow(.)]
        )
    })
  ) %>%
  dplyr::select(-c(x,y)) %>%
  unnest() %>%
  mutate(
    geometry = map2(x,y,~st_point(c(.x,.y))),
    geometry = st_sfc(geometry)
  ) -> logger_sf



# ggplot(tische_sf) + 
#   geom_sf(aes(fill = "Tisch"), alpha = 0.3,lwd = 2) +
#   geom_sf(fill = NA, alpha = 1) + 
#   geom_sf(data = logger_sf) +
#   scale_y_continuous(breaks = logger_df$y) +
#   scale_x_continuous(breaks = logger_df$x) +
#   theme(
#     panel.grid.minor = element_line(linetype = 3,colour = "grey"),
#     panel.grid.major = element_line(linetype = 2,colour = "black"),
#     panel.background = element_rect(fill = "white"),
#     legend.position = "bottom"
#     ) +
#   labs(fill = NULL) 


################################################################################
## Create some dummy data ######################################################
################################################################################




werte <- crossing(
  hour = 1:24,
  tischnr = 1:4,
  sensor = letters[1:5]
) %>%
  mutate(
    factor_hour = hour*0.2/23+0.95,
    factor_tisch = tischnr*0.2/3+0.95,
    temperatur = rnorm(nrow(.),15,5),
    temperatur = temperatur+factor_hour+factor_tisch,
    luftfeuchtigkeit = rnorm(nrow(.),90,2)
  )

loggerwerte <- left_join(logger_sf,werte,by = c("tischnr","sensor")) %>%
  st_sf()

loggerwerte <- loggerwerte %>%
  mutate(
    factor_x = x*0.3/(max(x)-min(x))+0.8,
    factor_y = y*0.3/(max(y)-min(y))+0.8,
    temperatur = temperatur+factor_x+factor_y
  )

loggerwerte %>%
  ggplot(aes(y,temperatur)) + geom_point()



coordinates_add <- function(input){
  cbind(input,sp::coordinates(input))
}


idw2raster <- function(loggerwerte,tische_sf, resolution = 0.1){
  points2interpolate_sp <- st_make_grid(tische_sf,square = T,cellsize = resolution/2,what = "centers") %>%
    as_Spatial(T)
  
  
  loggerwerte_sp <- sf::as_Spatial(loggerwerte,T)
  raster_template <- raster(extent(coordinates(points2interpolate_sp)), resolution = resolution)
  
  
  interpolated <- loggerwerte_sp %>%
    split(.$hour) %>%
    map(function(x){
      interpol <- idw(formula=temperatur ~ 1, locations = x, newdata = points2interpolate_sp, idp = 2.0) #%>% coordinates_add()
      rasterize(interpol,raster_template,field = "var1.pred")
    })
}

loggerwerte_hour <- loggerwerte %>%
  mutate(temp_short = format(round(temperatur), nsmall = 2)) %>%
  split(.$hour)


run_full = F
if(run_full){
  interpolated <- idw2raster(loggerwerte,tische_sf,0.05)
  
  minval <- map_dbl(interpolated, ~minValue(.x)) %>%
    min()
  maxval <- map_dbl(interpolated, ~maxValue(.x)) %>%
    max()
  
  
  gg_p <- pmap(list(interpolated,loggerwerte_hour,names(interpolated)),function(x,y,z){
    # x <- interpolated[[1]]
    p <- gplot(x) +
      geom_raster(aes(fill = value)) +
      geom_sf(data = tische_sf, fill = NA) +
      scale_fill_viridis_c(name = "Temperatur", limits = c(minval,maxval)) +
      geom_sf(data = y, fill = NA,shape = 1) +
      geom_sf_text(data = y, aes(label = temp_short),nudge_x = 0.15,nudge_y = 0.15,size = 2) +
      coord_sf() +
      theme(
        axis.title = element_blank(),
        panel.grid.minor = element_line(linetype = 3,colour = "grey"),
        panel.grid.major = element_line(linetype = 2,colour = "black"),
        panel.background = element_rect(fill = "white"),
      )
    # if(z == "1"){
    #   leg <- ggpubr::get_legend(p)
    #   leg <- ggpubr::as_ggplot(leg)
    #   ggsave("legend.png",leg)
    # }
    # p <- p + 
    #   theme(legend.position = "none")
    filename <- paste0("Plots/",z,".png")
    ggsave(filename,p,width = 7,height = 12,units = "cm")
    filename
  })
  
  save(gg_p,file = "gg_p.Rda")
} else{
  load("gg_p.Rda")
}




library(shiny)


# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Stunde:",
#                   min = 1,
#                   max = 24,
#                   value = 1,
#                   animate = animationOptions(loop = T),
#                   step = 1)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot",width = "100%")
#     )
#   )
# )

ui <- fluidPage(
  
  
  fluidPage(
    sliderInput("bins",
                "Stunde:",
                min = 1,
                max = 24,
                value = 1,
                animate = animationOptions(loop = T,interval = 500),
                step = 1),
    plotOutput("distPlot",width = "50%")
    
  )
  
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderImage({
    filename <- normalizePath(gg_p[[input$bins]])
    
    list(src = filename,
         alt = "This is alternate text")
    
  },deleteFile = F)
}

# Run the application 
shinyApp(ui = ui, server = server)

