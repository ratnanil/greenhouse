
library(tidyverse)

library(janitor)
library(sf)
library(raster)
library(spatstat)
library(isoband)
library(ggisoband)
library(gganimate)
library(plotly)


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



height = 2
width = 1.80
edge_height = 0.55
edge_width = 0.45
levels = c(0.40,1.00,1.60)

square_inner = height - edge_width*2

quadrante1 = LETTERS[1:4]
quadrante2 = LETTERS[5:8]
arten1 = c("Raime","Faserhanf","Jute","Baumwolle")
arten2 = c("Raime","Faserhanf","Jute","Baumwolle")

tische <- pmap(list(list(quadrante1,quadrante2),list(arten1,arten2),list("I","II")),function(quadrante,arten,tischnr){
  pmap_dfr(list(quadrante,arten,0:3),function(quadrant,art,width_fac){
    tibble(
      x = width*width_fac,
      y = 0,
      art = art,
      quadrant = quadrant,
      level1 = 0.4,
      level2 = 1.0,
      level3 = 1.6
    ) %>%
      gather(level,z,starts_with("level"))
  }) %>%
    mutate(tisch = tischnr)
})



tische_sf <- tische %>%
  map(function(tisch){
    df2sf(tisch,width,height)
  }) %>%
  do.call(rbind,.)

logger_spacing = 0.45

logger_sf <- tische %>%
  map(function(tisch){
    tisch %>%
      mutate(
        x = x + edge_width,
        y = y + edge_height
      ) %>%
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
              loggerid = c("1","5","2","4","3")
            )
        })
      ) %>%
      dplyr::select(-c(x,y)) %>%
      unnest() %>%
      rowwise() %>%
      mutate(
        geometry = pmap(list(x,y,z),~st_point(c(..1,..2))), # add ..3 to make it a 3D Point
        geometry = st_sfc(geometry),
        combid = paste0(quadrant,loggerid)
      ) %>% 
      ungroup() %>%
      st_sf()
  }) %>%
  do.call(rbind,.)
  




loggerdata <- list.files("Data/2019_05_22/",pattern = ".csv",recursive = T,full.names = T) %>%
  tibble(fullpath = .) %>%
  rowwise() %>%
  mutate(
    sep = str_split(fullpath,"/"),
    loggerid = str_remove(sep[[length(sep)]],".csv"),
    quadrant = sep[[length(sep)-2]]
  ) %>%
  dplyr::select(-sep) %>%
  pmap_dfr(function(fullpath,loggerid,quadrant){
    read_delim(fullpath,
               delim = ",",
               col_names = c("row","Datum","Temperatur","Luftfeuchtigkeit","Taupunkt","Seriennummer"),
               col_types = cols_only(Datum = col_datetime(),Temperatur = col_double(),Luftfeuchtigkeit = col_double(),Taupunkt = col_double()),
               skip = 1
    ) %>%
      mutate(
        loggerid = as.integer(loggerid),
        quadrant = quadrant
      )
  })


allocation <- tribble(
  ~tisch,~level,~quadrant,~sensorpos,~sensor,
  "I",1,"A",1,2783,
  "I",1,"A",2,1293,
  "I",1,"A",3,1309,
  "I",1,"A",4,2801,
  "I",1,"A",5,1284,
  "I",1,"B",1,1291,
  "I",1,"B",2,2824,
  "I",1,"B",3,1320,
  "I",1,"B",4,1306,
  "I",1,"B",5,1339,
  "I",1,"C",1,1283,
  "I",1,"C",2,1311,
  "I",1,"C",3,1305,
  "I",1,"C",4,1319,
  "I",1,"C",5,1302,
  "I",1,"D",1,2802,
  "I",1,"D",2,2784,
  "I",1,"D",3,1296,
  "I",1,"D",4,1279,
  "I",1,"D",5,1308,
  "I",2,"B",1,1301,
  "I",2,"B",2,2819,
  "I",2,"B",3,2803,
  "I",2,"B",4,1297,
  "I",2,"B",5,2794,
  "I",3,"B",1,1282,
  "I",3,"B",2,2786,
  "I",3,"B",3,1303,
  "I",3,"B",4,1299,
  "I",3,"B",5,2810,
  "II",1,"E",1,1285,
  "II",1,"E",2,1295,
  "II",1,"E",3,2799,
  "II",1,"E",4,1321,
  "II",1,"E",5,2814,
  "II",1,"F",1,1280,
  "II",1,"F",2,2790,
  "II",1,"F",3,1318,
  "II",1,"F",4,2789,
  "II",1,"F",5,1304,
  "II",1,"G",1,1300,
  "II",1,"G",2,1313,
  "II",1,"G",3,1292,
  "II",1,"G",4,2785,
  "II",1,"G",5,2800,
  "II",1,"H",1,2805,
  "II",1,"H",2,2828,
  "II",1,"H",3,2795,
  "II",1,"H",4,1289,
  "II",1,"H",5,1310,
  "II",2,"F",1,2797,
  "II",2,"F",2,1340,
  "II",2,"F",3,1294,
  "II",2,"F",4,1281,
  "II",2,"F",5,1298,
  "II",3,"F",1,1323,
  "II",3,"F",2,1288,
  "II",3,"F",3,1322,
  "II",3,"F",4,2796,
  "II",3,"F",5,1290,
) %>%
  mutate(
    combid = paste0(quadrant,sensorpos),
    level = paste0("level",level)
    )

logger_sensor <- logger_sf %>%
  inner_join(allocation, by = c("tisch","level","combid","quadrant"))
  

cols <- c("firebrick","dodgerblue4","gold3","springgreen4")
names(cols) <- arten1

tische_sf %>%
  mutate(
    level = fct_rev(level)
  ) %>%
  ggplot(aes(fill = art)) + 
  geom_sf() +
  scale_fill_manual(values = cols) +
  geom_sf(data = logger_sensor) +
  geom_sf_text(data = logger_sensor,aes(label = sensor),nudge_x = 0.1,nudge_y = 0.1) +
  facet_grid(level~tisch)


logger_sensor_data <- logger_sensor %>%
  left_join(dplyr::select(loggerdata,-quadrant), by = c("sensor" = "loggerid"))


resolution = 0.1
points2interpolate_sp <- tische_sf %>%
  st_make_grid(square = T,cellsize = resolution/2,what = "centers") %>%
  as_Spatial(T)



tische_sf <- tische_sf %>%
  filter(!(level %in% c("level2","level3") & quadrant %in% LETTERS[c(1,3:5,7:8)]))


points2interpolate <- tische_sf %>%
  group_nest(tisch,level) %>%
  mutate(
    newloc = map(data,function(x){
      x %>%
        st_as_sf() %>%
        st_make_grid(square = T,cellsize = resolution/2,what = "centers") %>%
        as_Spatial(T)
    })
  ) %>%
  select(-data)

points2interpolate %>%
  mutate(newloc = map(newloc,~as.data.frame(.x))) %>%
  unnest() %>%
  ggplot(aes(coords.x1,coords.x2)) + 
  geom_point() +
  facet_grid(level~tisch) +
  coord_equal()
  


interpolated <- logger_sensor_data %>%
  st_set_geometry(NULL) %>%
  group_nest(Datum,tisch,level) %>%
  left_join(points2interpolate,by = c("tisch","level")) %>%
  mutate(
    loggerwerte_sp = map(data,function(x){sp::SpatialPointsDataFrame(x[,c("x","y")],x)}),
    raster = pmap(list(data,loggerwerte_sp,newloc),function(data,loggerwerte_sp,newloc){
      # gstat::idw(formula=Temperatur ~ 1, locations = loggerwerte_sp, newdata = newloc, idp = 1.0) %>%
      #   as.data.frame()
      gstat::krige(formula=Temperatur ~ 1, locations = loggerwerte_sp, newdata = newloc) %>%
        as.data.frame()
    })
  ) %>%
  select(-newloc)






dat[[1]]$Datum
    

library(zeallot)

library(lubridate)
library(av)

theme_set(theme_light())
interpolated %>%
  filter(Datum >= parse_date("2019-05-07"), Datum <= parse_date("2019-05-13")) %>%
  group_nest(Datum2 = strftime(Datum,format = "%d.%m.%Y",tz = "UTC"),.key = "dat") %>%
  mutate(
    maxval = max(map_dbl(dat,function(x){max(map_dbl(x$raster,function(y){max(y$var1.pred)}))})),
    minval = min(map_dbl(dat,function(x){min(map_dbl(x$raster,function(y){min(y$var1.pred)}))}))
  ) %>% 
  # head(1) %>%
# %->% c(Datum2,dat,maxval,minval)
  pmap(function(Datum2,dat,maxval,minval){
    path <- "Plots/"
    filename_i <- Datum2 %>%
      str_replace_all("-| |:","_") %>%
      paste0(.,".mp4")
    
    folder <- file.path("Plots",substr(filename_i,0,10))
    dir.create(folder, showWarnings = FALSE)
    filepath = file.path(folder,filename_i)
    
    data_i <- dat %>% 
      dplyr::select(Datum, tisch,level,data) %>%
      unnest(data) %>%
      mutate(
        Temperatur = format(Temperatur, nsmall = 2)
      )  %>%
      mutate(hour = hour(Datum)+minute(Datum)/60) %>%
      filter(hour %% 1 == 0) %>%
      mutate(hour = as.integer(hour))
    
    raster_i <- dat %>%
      dplyr::select(Datum,tisch,level,raster) %>%
      unnest(raster) %>%
      mutate(hour = hour(Datum)+minute(Datum)/60) %>%
      filter(hour %% 1 == 0) %>%
      mutate(hour = as.integer(hour))
    

      p <-     ggplot() +
      geom_raster(data = raster_i,aes(coords.x1,coords.x2,fill = var1.pred)) +
      scale_fill_viridis_c(name = "Temperatur",limits = c(minval,maxval)) +
      coord_equal(1) +
      geom_point(data = data_i, aes(x,y),inherit.aes = F) +
      geom_label(data = data_i, aes(x,y,label = Temperatur),alpha  = 0.4,inherit.aes = F,nudge_x = 0.15,nudge_y = 0.15) +
      facet_grid(level~tisch) +
      theme(axis.title = element_blank(),
              axis.text = element_blank(), axis.ticks = element_blank())  +
      labs(title = Datum2, subtitle = "Temperatur um {frame_time}h") +
      transition_time(hour)
      
      p2 <- animate(p,renderer = av_renderer(),height = 750, width =1500)
      anim_save(filename = filename_i,animation = p2,path= folder)
          
    
  })





