calculate_real_distances <- function (run_data){
  # Retrieves the list of timestamps of the ribbon retrievals.
  ribbons_name <- paste("lintjes_tijden_", names(run_data), sep="")
  ribbons <- run_data[[1]][[ribbons_name]]
  
  # Retrieves the lists of both tracks.
  track_a_name <- paste("track_GPSA_", names(run_data), sep="")
  track_a <- run_data[[1]][[track_a_name]]
  track_b_name <- paste("track_GPSB_", names(run_data), sep="")
  track_b <- run_data[[1]][[track_b_name]]

  start_time <- track_a$time[1]

  # Filters the tracks with the timestamps of the ribbons.
  track_a <- track_a %>% filter(
    track_a$time %in% ribbons$GPSA_Timestamp & !is.na(type))
  track_b <- track_b %>% filter(
    track_b$time %in% ribbons$GPSB_Timestamp & !is.na(type))

  # Makes new data frame with the distances between ribbons.
  real_distances <- data.frame(starting_point=character(), 
                               ribbon_collected=character(),
                               start_time=character(),
                               end_time=character(),
                               distance_a=double(),
                               distance_b=double())
  
  # Fills the rows with distances from ribbon to ribbon.
  previous_ribbon <- "start"
  start_distance_a = 0
  start_distance_b = 0
  for (i in 1:length(ribbons$locations)){
    # Calculates the distance to the ribbon from current position.
    distance_a <- (track_a$distance..km.[i] - start_distance_a) * 1000
    distance_b <- (track_b$distance..km.[i] - start_distance_b) * 1000
    
    real_distances[i,] <- list(previous_ribbon, ribbons$locations[i],
                               start_time, track_a$time[i],
                               distance_a, distance_b)
    
    # Sets the start values for next ribbon to current ribbon.
    previous_ribbon <- ribbons$locations[i]
    start_time <- track_a$time[i]
    start_distance_a <- track_a$distance..km.[i]
    start_distance_b <- track_b$distance..km.[i]
  }

  real_distances
}


calculate_optimal_distances <- function (run_data){
  # Retrieves the optimal run from the list of the run.
  optimal_name <- paste("optimaal_", names(run_data), sep="")
  optimal_run <- run_data[[1]][[optimal_name]]
  
  # Creates an empty data frame but with the column names.
  optimal_distances <- data.frame(starting_point=character(),
                                  ribbon_collected=character(),
                                  distance=double())

  # Fills the data frame with the distance between each ribbon.
  row_numbers <- which(optimal_run$ribbon.collected != "")
  previous_ribbon <- "start"
  previous_distance <- 0
  for (i in 1:length(row_numbers)){
    # Converts the model distance to real-life meters.
    distance <- (row_numbers[i] - previous_distance) * 0.9996371971067133
    ribbon <- optimal_run$ribbon.collected[row_numbers[i]]

    optimal_distances[i,] <- list(previous_ribbon, ribbon, distance)
    
    # Sets current values as the starting point for the next ribbon.
    previous_ribbon <- optimal_run$ribbon.collected[row_numbers[i]]
    previous_distance <- row_numbers[i] * 0.9996371971067133
  }

  optimal_distances
}


calculate_quotients <- function (real_distances, optimal_distances){
  a <- real_distances$distance_a / optimal_distances$distance
  b <- real_distances$distance_b / optimal_distances$distance

  # Thursday's GPSB track is missing 11 data points.
  na_values_indices <- which(is.na(b))
  if (length(na_values_indices) > 0){
    b[na_values_indices] <- a[na_values_indices]
  }

  # Calculates the time between the ribbons.
  start_time <- as.POSIXct(real_distances$start_time, format="%H:%M:%S")
  end_time <- as.POSIXct(real_distances$end_time, format="%H:%M:%S")
  time <- end_time - start_time
  
  # Retrieves the 'strategy' of the tree location.
  filtered_trees <- trees %>% filter(
    trees$name %in% real_distances$ribbon_collected)
  
  # Orders the trees based on the order in which the ribbons were collected.
  filtered_trees <- filtered_trees[match(
    real_distances$ribbon_collected, filtered_trees$name),]

  quotients <- data.frame(starting_point=real_distances$starting_point,
                          ribbon_collected=real_distances$ribbon_collected,
                          strategy=filtered_trees$strategy,
                          a,
                          b,
                          time)
}
