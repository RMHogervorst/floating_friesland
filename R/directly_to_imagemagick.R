# using imagemagick as a source



# Create place to write too
fig <- image_graph(width = 400, height = 400, res = 96)
# ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)  # the plotting
dev.off()



# OR 
# https://gist.github.com/jeroen/1bf5dffed5f41d5f10cbfeb82c8148c8#file-mapping-with-magick-r-L57
# # now we start the graphics device

suppressMessages(library(tidyverse)) 
library(magick)
frames <- image_graph(width = 1500, height = 900, res = 300)

# Gebruikt ticks zodat je progress ziet, (zit in dplyr klaarblijkelijk)
# en walk over alle niveaus
# make a ggplot object for each week and print the graphic
pb <- progress_estimated(nlevels(foliage_sf$week))
walk(1:nlevels(foliage_sf$week), ~{
    
    pb$tick()$print()
    
    xdf <- filter(foliage_sf, week == levels(week)[.x])
    
    ggplot() +
        geom_sf(data=xdf, aes(fill=value), size=0.05, color="#2b2b2b") +
        geom_sf(data=states_sf, color="white", size=0.125, fill=NA) +
        viridis::scale_fill_viridis(
            name=NULL,
            discrete = TRUE,
            labels=c("No Change", "Minimal", "Patchy", "Partial", "Near Peak", "Peak", "Past Peak"),
            drop=FALSE
        ) +
        labs(title=sprintf("Foliage: %s ", unique(xdf$week))) +
        ggthemes::theme_map() +
        theme(panel.grid=element_line(color="#00000000")) +
        theme(panel.grid.major=element_line(color="#00000000")) +
        theme(legend.position="right") -> gg
    
    print(gg)
    
})

# animate the foliage
image_animate(frames, 1)