#--------------------------------------------------------------------------
# Libraries of interest
library(magick)
library(tidyverse)
library(here)
library(scales)
#--------------------------------------------------------------------------

# read in data
hex <- read_csv("hex.csv")

# change long header set by google
hex <- rename(hex, response = "Which option would you like to see as our R4DS online learning community logo?")

# generate the count for each response
results <- count_(hex, "response") %>% 
    separate(response, c("number", "background"), " ") %>% 
    mutate(total = 82,
           percent = n/total)

# what percent chose blue?
results %>% 
    group_by(background) %>% 
    summarize(percent_color = sum(percent))

# what percent chose the bird?
results %>% 
    group_by(number) %>% 
    summarize(percent_response = sum(percent))

# build the ggplot
hex <- ggplot(results, aes(x = fct_reorder(number, percent, .desc = T), y = percent, fill = background)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    labs(x = "\n\n\n\nResponse",
         y = "Percent of Total\n",
         title = "Hex-sticker voting in the R4DS community poll",
         subtitle = "Blue was the favorite background and the bird was the word with 35.4% of the vote!") +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = c("blue", "red"))

# view the plot
hex

# save the ggplot
ggsave("hex.png", width = 7, height = 5, dpi = 300)

# read the ggplot in via magick
plot <- image_read(paste0(here("/"), "hex.png")) 

# look at dimensions of image
image_info(plot)

#--------------------------------------------------------------------------
# Read in the various images and scale them to appropriate size 
#--------------------------------------------------------------------------

# generic function for reading in images and scaling
logo_read_resize <- function(x) {
    image_read(paste0(here("/"), x, ".png")) %>% 
        image_scale("250")
}

# sanity check for testing function, it works
logo_read_resize(1)

# map the function to a vector/list of logo names, stored as list
# I saved the 6 logos as 1.png, 2.png, 3.png, etc
num_list <- c(1, 2, 3, 4, 5, 6)

logo_list <- num_list %>% 
    map(., logo_read_resize) 

# start building the layers on the original plot
# there might be some purrr possibilities here, but this really didn't take that long

plot_1 <- image_composite(plot,   logo_list[[1]], offset = "+520+1100")
plot_2 <- image_composite(plot_1, logo_list[[2]], offset = "+760+1100")
plot_3 <- image_composite(plot_2, logo_list[[3]], offset = "+1000+1100")
plot_4 <- image_composite(plot_3, logo_list[[4]], offset = "+1480+1100")
plot_5 <- image_composite(plot_4, logo_list[[5]], offset = "+1240+1100")
plot_6 <- image_composite(plot_5, logo_list[[6]], offset = "+280+1100")

# view the "final" layered plot + logos
plot_6

# save the final plot as a png
image_write(plot_6, paste0(here("/"), "r4ds_vote.png"))  
