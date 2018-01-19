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
    geom_sf(aes(fill = NAME_1),color = "grey50", alpha = 3/4)+
    geom_text_repel(aes(X,Y, label = NAME_1), size = 8)+
    lims(x = c(3.2,7.1), y = c(50.8,55))+
    labs(x="", y = "", caption = "shapefiles from www.gadm.org", title = "Floating Friesland")+
    dutchmasters_fill("little_street")+
    #theme_grey()+
    theme( legend.position = "empty", # we already labeled the provinces
           panel.grid.major = element_line(colour = "grey80"))

# ideal path 0,0    -.4,.3     -.4, .4 , equal numbers  tot 1.6
## maybe a function where we make plots ?  and only apply a df with x and y coordinates?
# make list of ggplots
#frames <- lapply(as.list(front), function(x) image_composite(image_in, x))

# make function to create plot
plot_netherlands <- function(province, movement, filename){
    {move_province(provincename = province, movement = movement) %>% 
        ggplot()+
        geom_sf(aes(fill = NAME_1),color = "grey50", alpha = 3/4)+
        geom_text(aes(X,Y, label = NAME_1), size = 6)+
        lims(x = c(3.2,7.1), y = c(50.8,55))+
        labs(x="", y = "", caption = "shapefiles from www.gadm.org", title = "Floating Friesland")+
        dutchmasters_fill("little_street")+
        theme( legend.position = "empty", # we already labeled the provinces
               panel.grid.major = element_line(colour = "grey80"))} %>% 
        ggsave(filename = filename, width = 150, height = 250, units = "mm")
    #300 x 506
}

plot_netherlands("Friesland", c(0,0), "test1.png")
plot_netherlands("Friesland", c(-1,1), "test2.png")

## create several plots over a range
## 
create_dir_if_necessary <- function(basename, debug = FALSE){
    directory <- dirname(basename)
    if(directory != "."){
        if(!dir.exists(directory)){
            if(debug){message("creating directory: ",directory)}
            dir.create(directory)    # would be silly to create the directory twice, now wouldn't it?
        }
    }
}



plot_province_over_range <- function(basename = "plot", offset_matrix, province = "Friesland", debug = FALSE){
    # create directory if it doesn't exist
    create_dir_if_necessary(basename, debug = debug)
    if(any(is.na(offset_matrix))){stop("I cannot handle empty movements, there are NA's in movement_matrix")} 
    if(NCOL(offset_matrix) != 2) stop("movement_matrix needs to have exactly 2 columns")
    actionsframe <- data_frame(x = offset_matrix[,1], y = offset_matrix[,2]) %>% 
        mutate(rownumber = row_number())
               #rownumber = formatC(rownumber, flag = 0,width = 4),
    actionsframe$name <- paste0(basename, formatC(actionsframe$rownumber, flag = 0,width = 4))
    for (i in actionsframe$rownumber) {
        suppressWarnings(plot_netherlands(province, c(actionsframe$x[[i]], actionsframe$y[[i]]), paste0(actionsframe$name[[i]],".png")))
        if(debug){ message("plotting ",actionsframe$name[[i]], " with " ,c(actionsframe$x[[i]], actionsframe$y[i]))}
    }
}

movement <- matrix(c(seq(0, -1.0, -0.1), seq(0, 1.0, .1)),ncol = 2)
# now this doesn't look so nice. So I need the first few sequences to be different
movement <- matrix(c(c(0,-.1,-.2,-.2,-.3), c(0,.03,.05,.1,.15)) ,ncol = 2)
movement2 <- matrix(c(seq(from = -.3, by = -.1, length.out = 14),seq(from = .2, by = .1, length.out = 14)), ncol = 2)

plot_province_over_range(basename = "friesland/plot", rbind(movement, movement2),province = "Friesland", debug = TRUE)
lnnh <- 10
plot_province_over_range(basename = "NH/Noord-Holland",matrix(c(seq(from = 0,by = -.1, length.out = lnnh),rep(0,lnnh)),ncol = 2),province = "Noord-Holland",debug = TRUE)

### function to take all images in a folder in sequence and produce a gif 
# with example from https://rud.is/b/2016/07/27/u-s-drought-animations-with-the-witchs-brew-purrr-broom-magick/
make_gif_of_folder <- function(foldername, gifname, fps = 1, loop =0){
    list.files(path = foldername, full.names = TRUE,recursive = FALSE) %>% 
    map(image_read) %>%
        image_join() %>%
        image_animate(fps=fps, loop=loop) %>%
        image_write(gifname)
}

make_gif_of_folder("friesland", "friesland/friesland.gif")
make_gif_of_folder("NH", "noord-holland.gif")



## alternative would be to move the islands seperately?




# perhaps try with germany and belgium plotted filled in at side and bottom ?
# if we keep the limits the same everything else would just fall away