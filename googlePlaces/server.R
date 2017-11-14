#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(httr)
library(glue)
library(jsonlite)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(igraph)
library(forcats)

# Define server logic 
shinyServer(function(input, output) {
  
  temps_trajet <- read.csv2('../data/temps_trajet.csv')
  base_trajet_total <- read.csv2('../data/base_trajet_total.csv')
  
  # table de correspondance nom de stations/trip_id
  nodes_trajet <- base_trajet_total[, 1:2]
  nodes_trajet <- unique(nodes_trajet)
  
  temps_trajet_igraph <- graph_from_data_frame(d=temps_trajet, vertices = nodes_trajet, directed = T)
  
  #Function which finds the optimal solution (where to meet)
  
  best_path <- function(station1, station2){
      
    #We will put the solution in a list
    sol <- list()
    
    trajet_plus_court <- shortest_paths(temps_trajet_igraph, V(temps_trajet_igraph)[stop_name == station1], to = V(temps_trajet_igraph)[stop_name == station2], output = "vpath")
    
    
    #Ici vous avez le temps total de trajet : il faut encore l'input de l'utilisateur
    
    df_distances <- as.data.frame(distances(temps_trajet_igraph, V(temps_trajet_igraph)[stop_name == station1], to = V(temps_trajet_igraph)[stop_name == station2]))
    
    
    inds = which(df_distances == min(df_distances), arr.ind=TRUE)
    n_colonne <- inds[1, 2] # IMPORTANT pour choisir les stations par la suite
    
    sol$path$stop_ids <- V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name
    
    ###ON A OBTENU LE CHEMIN IDEAL, MAINTENANT IL FAUT TROUVER LE MIDPOINT
    
    # pathlist_exclu comprend tous les éléments de l'itinéraire sauf le dernier (1ère colonne de chemin)
    
    # création de la table chemin pour lire le chemin le plus court
    
    pathlist_from <-c()
    for (i in 1:(length(V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name)-1)){
      pathlist_from[i] <- trajet_plus_court$vpath[[n_colonne]]$name[i]
    }
    
    pathlist_to <- c()
    for (i in 2:(length(V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name))){
      pathlist_to[i-1] <- trajet_plus_court$vpath[[n_colonne]]$name[i]
    }
    
    traj <-c()
    for (i in 1:(length(V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name)-1)){
      traj[i] <- (temps_trajet %>% filter(from_stop_id == trajet_plus_court$vpath[[n_colonne]]$name[i], to_stop_id == trajet_plus_court$vpath[[n_colonne]]$name[i+1]))$weight[1]
    }
    
    #traj <- is.numeric(as.character(traj))
    
    chemin <- cbind(pathlist_from,pathlist_to,traj)
    chemin <- as.data.frame(chemin)
    chemin$traj <- as.numeric(chemin$traj)
    
    # cumsum des temps de trajet
    
    chemin <- chemin %>% transform(Total = ave(traj, FUN = cumsum))
    
    #mi-chemin
    
    mid <- max(chemin$Total)/2
    
    sol$mid$stop_id <- (chemin %>% filter(Total > mid ))$pathlist_to[1]
    sol$mid$stop_name <- (nodes_trajet %>% filter(stop_id == sol$mid$stop_id))[1,2]
    
    sol$mid$lat_lng <- paste((base_trajet_total %>% filter(stop_id == sol$mid$stop_id))[1,4], ',',
                             (base_trajet_total %>% filter(stop_id == sol$mid$stop_id))[1,5], sep = '')
    sol$mid$text <- paste("Your optimal meeting point is the", sol$mid$stop_name, "station")
    
    ########
    #Now we prepare our graph
    base_trajet_court <- base_trajet_total %>%
      filter (stop_id %in% V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name) %>% 
      slice(match(V(temps_trajet_igraph)[trajet_plus_court$vpath[[n_colonne]]]$name, stop_id))
    
    base_metro_map <- base_trajet_total %>%
      distinct(stop_name, route_short_name, .keep_all = TRUE)
    
    base_metro_map$route_short_name = factor(base_metro_map$route_short_name)
    
    base_trajet_total$route_short_name = factor(base_trajet_total$route_short_name)
    
    base_metro_map$route_short_name <- fct_relevel(base_metro_map$route_short_name, c(1, 2, 3, "3B", 4, 5, 6, 7, "7B", 8, 9, 10, 11, 12, 13, 14, "A", "B"))
    
    base_trajet_total$route_short_name <- fct_relevel(base_trajet_total$route_short_name, c(1, 2, 3, "3B", 4, 5, 6, 7, "7B", 8, 9, 10, 11, 12, 13, 14, "A", "B"))
    
    #########################
    #Trajet pour utilisateur 1
    chemin_1 <- (chemin %>% filter(Total <= mid))$pathlist_from
    
    base_trajet1 <- base_trajet_total %>% filter (stop_id %in% chemin_1) %>% slice(match(chemin_1, stop_id))
    
    #Trajet pour utilisateur 2
    chemin_2 <- rev((chemin %>% filter( Total > mid))$pathlist_to) # Attention, pas même que 1
    
    base_trajet2 <- base_trajet_total %>% filter (stop_id %in% chemin_2) %>% slice(match(chemin_2, stop_id))
    
    #############################
    #Visualisation generale
    
    factpal=colorFactor(palette = c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"), 
                        domain = base_metro_map$route_short_name, levels = levels(base_metro_map$route_short_name), ordered = TRUE, na.color = "#808080", alpha = FALSE)
    
    
    map_metro_ligne = leaflet() %>%
      # Add CartoDB background map
      addProviderTiles("CartoDB.DarkMatter") %>%  
      # Add a marker for each stop
      addMarkers(lng = base_trajet_court$stop_lon[1], lat = base_trajet_court$stop_lat[1], data= base_trajet_court, popup = paste("User 1 departure:", station1))%>%
      addMarkers(lng = base_trajet1$stop_lon[dim(base_trajet1)[1]], lat = base_trajet1$stop_lat[dim(base_trajet1)[1]], data= base_trajet_court, popup = paste("Meeting point:", sol$mid$stop_name))%>%
      addMarkers(lng = base_trajet_court$stop_lon[dim(base_trajet_court)[1]], lat = base_trajet_court$stop_lat[dim(base_trajet_court)[1]], data= base_trajet_court, popup = paste("User 2 departure:", station2))%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
      
      addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_court, stroke = FALSE, 
                       fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
      addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
                labels = levels(base_trajet_total$route_short_name),title = "Metro line Paris")
    
    sol$map <- map_metro_ligne
    
    #Visualisation 2
    map_metro_ligne1 = leaflet() %>%
      # Add CartoDB background map
      addProviderTiles("CartoDB.DarkMatter") %>%  
      # Add a marker for each stop
      addMarkers(lng = base_trajet1$stop_lon[1], lat = base_trajet1$stop_lat[1], data= base_trajet1, popup = paste("User 1 departure:", station1) )%>%
      addMarkers(lng = base_trajet1$stop_lon[dim(base_trajet1)[1]], lat = base_trajet1$stop_lat[dim(base_trajet1)[1]], data= base_trajet1, popup = paste("Meeting point:", sol$mid$stop_name))%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1 %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
      
      addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet1, stroke = FALSE, 
                       fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
      addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
                labels = levels(base_trajet_total$route_short_name),title = "Metro line Paris")
    
    sol$map1 <- map_metro_ligne1
    
    #Visualisation 3
    
    map_metro_ligne2 = leaflet() %>%
      # Add CartoDB background map
      addProviderTiles("CartoDB.DarkMatter") %>%  
      # Add a marker for each stop
      addMarkers(lng = base_trajet2$stop_lon[1], lat = base_trajet2$stop_lat[1], data= base_trajet2, popup = paste("Meeting point:", sol$mid$stop_name))%>%
      addMarkers(lng = base_trajet2$stop_lon[dim(base_trajet2)[1]], lat = base_trajet2$stop_lat[dim(base_trajet2)[1]], data= base_trajet2, popup = paste("User 2 departure:",station2 ))%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
      addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2 %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
      
      addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet2, stroke = FALSE, 
                       fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
      addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
                labels = levels(base_trajet_total$route_short_name),title = "Metro line Paris")
    
    sol$map2 <- map_metro_ligne2
    
    return(sol)
  }
  
  
  ########################
  #Google maps part
  
  places <- function(latlng, radius, type) {
    
    key <- 'AIzaSyDgPocAf-cxrzFDaMHctZYoVpsBxpjezZI'
    url <- glue('https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={key}&location={latlng}&radius={input$radius}&type={input$type}')
    
    #Use the url to with a get request -> google api
    list <- GET(url = url)
    #Check if there is an error
    if(list$status_code != 200){
      table_results <- data.frame("API ERROR")
    } else {
      results <- content(list)$results
    }
    #Some places dont have a rating: add a NaN rating
    #In fact you should add default values like this for all the fields you are interested in
    for (i in 1:length(results)){
      if(is.null(results[[i]]$rating)){
        results[[i]]$rating = NaN
      }
    }
    
    #Here are the fields we will put in our table
    names <- sapply(results, FUN = get, x="name")
    ratings <- sapply(results, FUN = get, x="rating")
    address <- sapply(results, FUN = get, x="vicinity")
    
    table_results <- data.frame(name = names, rating = ratings, address = address)
    return(table_results)
  }
  
  ############################Output part
  
  no_path <- reactive({input$station1 == input$station2})
  
    solution <- reactive({best_path(input$station1, input$station2)})
    output$solution <- renderText(
      if (!no_path()){
        paste(unlist(solution()$mid$text), collapse = '')
      }
    else{
      print("Please choose two different metro stations")
    })
    output$leaflet <- renderLeaflet(
      if(!no_path()){
        solution()$map
      })
    output$leaflet1 <- renderLeaflet(
      if(!no_path()){
        solution()$map1
      })
    output$leaflet2 <- renderLeaflet(
      if(!no_path()){
        solution()$map2
      })
    output$result_table <- renderTable(
      if(!no_path()){
        places(solution()$mid$lat_lng, input$radius, input$type)
      })
})


#######################################
##Use this if you want the full paris metro map

# #Prepare the color palette
# factpal=colorFactor(palette = c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"), 
#                     domain = base_trajet_total$route_short_name, levels = levels(base_trajet_total$route_short_name), ordered = TRUE, na.color = "#808080", alpha = FALSE)
# #Code the plot
# myplot = leaflet() %>%
#   # Add CartoDB background map
#   addProviderTiles("CartoDB.DarkMatter") %>%  
#   # Add a marker for each stop
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 1, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FFCD00")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 2, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#003CA6")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 3, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#837902")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "3B", direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#00AE41")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 4, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#CF009E")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 5, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FF7E2E")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 6, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 7, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 7, trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#FA9ABA")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "7B", trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "7B", trip_short_name ==201), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6ECA97")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 8, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E19BDF")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 9, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#B6BD00")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 10), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#C9910D")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 11, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#704B1C")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 12, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#007852")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 13, trip_short_name ==101), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 13, trip_short_name ==102), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#6EC4E8")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== 14, direction_id ==0), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#62259D")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "A", trip_headsign == "NELY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "A", trip_headsign == "QIKY"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#E2231A")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "B", trip_headsign == "SOIR"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
#   addPolylines(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total %>% filter(route_short_name== "B", trip_headsign == "KOCQ"), group = ~ route_id, weight = 2,  fillOpacity = 0.5, color = "#7BA3DC")%>%
#   
#   addCircleMarkers(lng= ~ stop_lon, lat= ~stop_lat, data = base_trajet_total, stroke = FALSE, 
#                    fillOpacity = 0.5, radius =4, color = ~ factpal(route_short_name)) %>% 
#   addLegend(colors =c("#FFCD00", "#003CA6", "#837902", "#00AE41", "#CF009E", "#FF7E2E", "#6ECA97", "#FA9ABA", "#6ECA97", "#E19BDF", "#B6BD00", "#C9910D", "#704B1C", "#007852", "#6EC4E8", "#62259D", "#E2231A", "#7BA3DC"),
#             labels = levels(base_trajet_total$route_short_name)[1:18],title = "Metro line Paris")
# 
