# using imagemagick as a source



# # Create place to write too
# fig <- image_graph(width = 400, height = 400, res = 96)
# # ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)  # the plotting
# dev.off()



# OR 
# https://gist.github.com/jeroen/1bf5dffed5f41d5f10cbfeb82c8148c8#file-mapping-with-magick-r-L57
# # now we start the graphics device

suppressMessages(library(tidyverse)) 
library(magick)
library(sf)
library(paletti)

# functions I will use (we could put all of the functions somewhere else, of course, but this works too)
# colorscheme
dutchmasters_fill <- get_scale_fill(get_pal(dutchmasters))
# the data
NLD <- read_sf("data/NLD_adm1.shp")


# basic function that moves an a province
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

# make function to create plot
# using the previous function to move the province
plot_netherlands <- function(province, movement){
    plotunit <- move_province(provincename = province, movement = movement) %>% 
            ggplot()+
            geom_sf(aes(fill = NAME_1),color = "grey50", alpha = 3/4)+
            geom_text(aes(X,Y, label = NAME_1), size = 6)+
            lims(x = c(3.2,7.1), y = c(50.8,55))+
            labs(x="", y = "", caption = "shapefiles from www.gadm.org", title = "Floating Friesland")+
            dutchmasters_fill("little_street")+
            theme( legend.position = "empty", # we already labeled the provinces
                   panel.grid.major = element_line(colour = "grey80"))
    print(plotunit) # you have to explicitly tell it to print so the image is captured
}



# This example from jeroen ooms and Bob rudis uses ticks (which is an awesome feature I did not know was in dplyr)
# it uses purr:: walk to go over values and make a side effect: printing to the image magick thing

# set up print location 
frames <- image_graph(width = 1500, height = 900, res = 300)

plot_province_over_range <- function(offset_matrix, province = "Friesland", debug = FALSE){
    
    if(any(is.na(offset_matrix))){stop("I cannot handle empty movements, there are NA's in movement_matrix")} 
    if(NCOL(offset_matrix) != 2) stop("movement_matrix needs to have exactly 2 columns")
    actionsframe <- data_frame(x = offset_matrix[,1], y = offset_matrix[,2]) %>% 
        mutate(rownumber = row_number())
    actionsframe$name <- paste0(formatC(actionsframe$rownumber, flag = 0,width = 4))
    
    pb <- progress_estimated(NROW(actionsframe))

    walk(actionsframe$name, ~{
        
        pb$tick()$print()
        vars <- filter(actionsframe, name == .x)
        if(debug){
            message("using values from: ",vars)
        }
        
        plot_netherlands(province = province,movement = c(vars$x[[1]], vars$y[[1]]))
    
        }) # ends the walk action
}
## then the creation starts with the movement
Friesland_moves <- rbind(
    matrix(c(c(0,-.1,-.2,-.2,-.3), c(0,.03,.05,.1,.15)) ,ncol = 2),
    matrix(c(seq(from = -.3, by = -.1, length.out = 14),seq(from = .2, by = .1, length.out = 14)), ncol = 2)
)


plot_province_over_range(offset_matrix = Friesland_moves, province = "Friesland")

# animate the foliage
image_animate(frames, 1)
