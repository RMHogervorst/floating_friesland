### making a dutch country without friesland.
library(tidyverse)
library(sf)
library(ggrepel)
library(paletti) # thank you Edwin Thoen! @edwin_thoen
library(magick)

# location of file 
# I still have the files from a previous plotting exercise.
NLD <- read_sf("data/NLD_adm1.shp")



NLD %>% 
    filter(NAME_1 != "Friesland") %>% 
    filter(TYPE_1 != "Water body") %>%  # keep the waters out
    ggplot()+
    geom_sf(fill = "darkgreen")
# only the first island is part of North Holland and 
# Rottermerplaat and Rottermeroog are part of Groningen.

NLD %>% 
    filter(NAME_1 == "Friesland") %>% 
    ggplot()+
    geom_sf()

# morph this? move over place? in gif?

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

NLD %>% 
    filter(NAME_1 == "Friesland") %>% 
    mutate(geometry = geometry +10) %>% 
    ggplot()+
    geom_sf()

#ncg2 = (ncg - cntrd) * rot(pi/2) * .75 + cntrd
#
NLD %>% 
    mutate_if(table$NAME_1 == "Friesland", geometry = geometry +10) %>% 
    ggplot()+
    geom_sf()
# doesnt work


rest <- NLD %>% 
    filter(NAME_1 != "Friesland") %>% 
    filter(TYPE_1 != "Water body") 

friesl <- NLD %>% 
    filter(NAME_1 == "Friesland") %>% 
    mutate(geometry = geometry + c(-1,1)) %>% 
    st_set_crs("+proj=longlat +datum=WGS84 +no_defs")

# so we can move Friesland out of the way with +c(x,y)

rbind(friesl, rest) %>% 
    ggplot() + geom_sf()

rbind(friesl, rest) %>% st_centroid()  %>% st_coordinates()

#prob should make this a function

move_province <- function(provincename, movement){
    #province_name <- quo(province_name)
    mov <- quo(movement)
    rest <- NLD %>% 
        filter(NAME_1 != !!provincename) %>% 
        filter(TYPE_1 != "Water body") 
    #rest %>% st_centroid() %>% st_as_text() 
    
        
    province <- NLD %>% 
        filter(NAME_1 == !!provincename) %>% 
        mutate(geometry = geometry + !!mov) %>% 
        st_set_crs("+proj=longlat +datum=WGS84 +no_defs")
    
    data1 <- rbind(province, rest)
    centroids <- data1 %>% st_centroid()  %>% st_coordinates()
    cbind(data1, centroids)
    #data1
}
# test
dutchmasters_fill <- get_scale_fill(get_pal(dutchmasters))



move_province("Friesland", c(-1.7,1.7)) %>% 
    ggplot()+
    geom_sf(aes(fill = NAME_1),color = "grey80", alpha = 3/4)+
    geom_text_repel(aes(X,Y, label = NAME_1), size = 3)+
    lims(x = c(3.2,7.1), y = c(50.8,55))+
    labs(x="", y = "", color = "")+
    dutchmasters_fill("little_street")+
    #theme_grey()+
    theme( legend.position = "empty", # we already labeled the provinces
           panel.grid.major = element_line(colour = "grey80"))

# ideal path 0,0    -.4,.3     -.4, .4 , equal numbers  tot 1.6
## maybe a function where we make plots ?  and only apply a df with x and y coordinates?
# make list of ggplots
frames <- lapply(as.list(front), function(x) image_composite(image_in, x))

## 
## alternative would be to move the islands seperately?


#perhaps try with germany and belgium against ?