## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = F, warning = F,message = F)

## ------------------------------------------------------------------------


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





## ------------------------------------------------------------------------
################################################################################
## Setup Study Design (create shapes) ##########################################
################################################################################
library(tidyverse)
library(plotly)
run_full = F

if(run_full){
  
  library(sf)

  library(janitor)
  
  library(raster)
  library(spatstat)
  library(isoband)
  library(ggisoband)
  library(gganimate)
  
  height = 2
  width = 1.80
  edge_height = 0.55
  edge_width = 0.45
  
  square_inner = height - edge_width*2
  
  tische_df <- tibble(
    x = width*0:3,
    y = rep(0,4),
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
            tibble(
              x = x_org+1*logger_spacing,
              y = y_org+1*logger_spacing)
          ) %>%
          mutate(
            id0 = c("A","E","B","D","C")
          )
      })
    ) %>%
    dplyr::select(-c(x,y)) %>%
    unnest() %>%
    mutate(
      geometry = map2(x,y,~st_point(c(.x,.y))),
      geometry = st_sfc(geometry),
      id1 = paste0(tischnr,id0)
    ) %>% st_sf() -> logger_sf
  
  
  
  
  logger_sf %>%
    filter(tischnr == 3) %>%
    st_set_geometry(NULL) %>%
    mutate(
      z1 = 0.40,
      z2 = 1.00,
      z3 = 1.60
    ) %>%
    gather(zval,z,z1:z3) %>%
    rowwise() %>%
    mutate(
      geometry = pmap(list(x,y,z),~st_point(c(..1,..2,..3))),
      geometry = st_sfc(geometry),
      id2 = paste0(substr(id1,2,2),substr(zval,2,2))
    ) %>%
    st_sf()-> logger_sf2
  
  
  
  files <- list.files("Data/",pattern = ".csv",recursive = T,full.names = T) %>%
    set_names(str_remove(basename(.),".csv"))
  
  
  files_df <- imap_dfr(files,function(x,y){
    read_csv(x,
             col_names = F,
             col_types = cols(col_skip(),col_datetime(),col_double(),col_double(),col_double(),col_skip(),col_skip()),
             skip = 1) %>%
      magrittr::set_colnames(c("Datum","Temperatur","Luftfeuchtigkeit","Taupunkt")) %>%
      mutate(
        sensor = y
      )
  }
  )
  
  
  
  allocation <- tribble(
    ~id2,~sensor,
    "A1",1289,
    "B1",1310,
    "C1",1290,
    "D1",1292,
    "E1",1300,
    "A2",1280,
    "B2",1313,
    "C2",1304,
    "D2",1295,
    "E2",1322,
    "A3",1307,
    "B3",1298,
    "C3",1301,
    "D3",1281,
    "E3",1318,
  )
  
  
  logger_sf2_time <- logger_sf2 %>%
    left_join(allocation,by = "id2") %>%
    mutate(sensor = as.character(sensor)) %>%
    left_join(files_df,by = "sensor")
  
  save(logger_sf2_time, file = "logger_sf2_time.Rmd")
  
  
  
  tische_sf2 <- tische_sf %>% filter(tischnr == 3)
  
  resolution = 0.1
  points2interpolate_sp <- tische_sf %>%
    filter(tischnr == 3) %>%
    st_make_grid(square = T,cellsize = resolution/2,what = "centers") %>%
    as_Spatial(T)
  
  
  interpolated <-logger_sf2_time %>%
    filter(!is.na(Datum)) %>%
    ungroup() %>%
    group_by(Datum,zval) %>%
    nest() %>%
    mutate(
      raster = map(data,function(x){
        loggerwerte_sp <- sf::as_Spatial(x,T)
        interpol <- gstat::idw(formula=Temperatur ~ 1, locations = loggerwerte_sp, newdata = points2interpolate_sp, idp = 2.0) %>%
          as.data.frame() 
      })
    )
  
  interpolated_raster <- interpolated %>%
    ungroup() %>%
    select(-data) %>%
    unnest() %>% 
    group_by(Datum) %>%
    nest(.key = raster) 
  
  interpolated <- interpolated %>%
    ungroup() %>%
    select(-raster) %>%
    unnest() %>%
    st_as_sf() %>%
    group_by(Datum) %>%
    nest() %>%
    ungroup() %>%
    select(-Datum) %>%
    bind_cols(interpolated_raster)
  
  
  
  
  zval_translate = c(
    "z1" = "40 cm",
    "z2" = "100 cm",
    "z3" = "160 cm"
  )
  
  
  # 
  # 
  nmax = 24
  minval <- min(map_dbl(interpolated$raster[1:nmax],~min(.x$var1.pred)))
  maxval <- max(map_dbl(interpolated$raster[1:nmax],~max(.x$var1.pred)))
  
  
  
  interpolated %>%
    head(nmax) %>%
    pmap(function(data,Datum,raster){
      ggplot() +
        geom_raster(data = raster,aes(coords.x1,coords.x2,fill = var1.pred),inherit.aes = F)+
        geom_isobands(data = raster,
                      binwidth = 1, 
                      aes(coords.x1,coords.x2,fill = var1.pred,z = var1.pred), 
                      fill = NA,  
                      polygon_outline = T,
                      inherit.aes = F)  +
        geom_point(data = data,aes(x,y),inherit.aes = F)  +
        ggrepel::geom_text_repel(data = data,aes(x,y,label = Temperatur),nudge_x = 0.1,nudge_y = 0.1,inherit.aes = F) +
        coord_equal(1) +
        facet_grid(~zval,labeller = as_labeller(zval_translate))+
        labs(title = Datum,fill = "Temp.")+
        scale_fill_viridis_c(name = "Temperatur",limits = c(minval,maxval))  +
        theme(
          axis.title = element_blank(),
          legend.position = "bottom"
        )
      
      filename_i <- str_replace(Datum,"-","_") %>%
        str_replace(" ","_") %>%
        str_replace(":","_") %>%
        str_replace(":","_") %>%
        paste0(.,".png")
      ggsave(filename_i,height = 15,width = 20,units = "cm")
      
      return(filename_i)
    }) -> names
  
  save(names,file = "names.Rmd")
} else{
  load("names.Rmd")
  load("logger_sf2_time.Rmd")
}


p <- plotly::ggplotly(
  logger_sf2_time %>%
    gather(key,val,c(Temperatur,Luftfeuchtigkeit)) %>%
    ggplot(aes(Datum,val,colour = sensor)) +
    geom_line() +
    facet_grid(key~.,scales = "free_y") +
    theme_classic()+
    theme(axis.title.y = element_blank(), legend.position = "none") 
)
fils <- map_chr(names,~.x)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("raeumliche interpolation",
             plotOutput("plot3",width = "100%"),
             shinyWidgets::sliderTextInput("n", "sdf", fils,animate = animationOptions(T,interval = 500))
             ),
    
    tabPanel("timeseries",plotlyOutput("plot4"))
  )
  

    
  )
  
  server <- function(input, output, session) {
    


    # Send a pre-rendered image, and don't delete the image after sending it
    # NOTE: For this example to work, it would require files in a subdirectory
    # named images/
    output$plot3 <- renderImage({
      filename <- normalizePath(file.path(input$n))
      list(src = filename)
    }, deleteFile = FALSE)
    
    output$plot4 <- renderPlotly({p})
  }
  
  shinyApp(ui, server)
