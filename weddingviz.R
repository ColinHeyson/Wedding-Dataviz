library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
require(mapproj)
library(googlesheets)
suppressPackageStartupMessages(library("dplyr"))

# Grab sheets existing to the user
# Prompts you to log into google if you're not already signed in
sheets.available <- gs_ls()

sheet.object <- gs_title('Addresses')
wedding.list <- as.data.frame(gs_read(ss = sheet.object))
wedding.list <- wedding.list[!is.na(wedding.list$State), ]

# reorder state variable by sum of invitees
wedding.list %>%
    group_by(State) %>%
    summarise(Sum = sum(Invitees)) %>%
    select(State, Sum) %>%
    arrange(desc(Sum)) -> invitees.summary

wedding.list <- within(wedding.list,
                       State <- factor(State,
                                       levels = invitees.summary$State))

wedding.list$city.state <- paste0(wedding.list$City, ', ', wedding.list$State)

geocodes <- geocode(wedding.list$Zip)
geocodes.usa <- geocodes[!geocodes$lon > 0, ]


# Using ggplot, plot the Base USA Map
mp <- NULL
map <- borders("state", colour = "gray50") # create a layer of borders
mp <- ggplot() + map

# Now Layer the cities on top
mp <- mp + geom_point(aes(x = geocodes.usa$lon, y = geocodes.usa$lat) ,color="#0099FF", size = 4) +
    theme_nothing()
mp

# bar plot of state
state.plot <- ggplot(wedding.list, aes(x = State, y = Invitees))
state.plot + geom_bar(stat = 'identity', fill = '#0099FF') + theme_minimal()


# bar plot of city, state
wedding.list %>%
    group_by(city.state) %>%
    summarise(Sum = sum(Invitees)) %>%
    select(city.state, Sum) %>%
    arrange(desc(Sum)) -> invitees.summary


city.state.plot <- ggplot(invitees.summary[1:4, ], aes(x = reorder(city.state, Sum, function(x) - sum(x)),
                                                        y = Sum))
city.state.plot + geom_bar(stat = 'identity', fill = '#0099FF') + 
    labs(x = 'City', y = 'Invitees', title = 'Top Cities for Guests') + theme_minimal()



