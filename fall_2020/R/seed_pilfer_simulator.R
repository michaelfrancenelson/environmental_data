#' Simulate seed predation experiment from Bolker 2008, Ch 1.
#' 
#' 
#' @param n_simulations How many simulations to perform?
#' @param n_stations Numeric (integer) vector.  How many observations for each species?
#' @param pilfer_rates Numeric vector.  Pilfer rates for each species.  
#' Each rate must be between 1 and 0 inclusive.
#' @param species_names Optional character vector of species names.
#' 
#' @return A data.frame object with the results of the simlations.
#' @export

seed_pilfer_simulator = function(
  n_simulations,
  n_stations,
  pilfer_rates,
  species_names = NULL)
{
  
  # Assign species names if they weren't provided
  if (is.null(species_names))
    species_names = sprintf(
      "species_%0.2d", 
      1:length_n_stations)
  
  LETTERS[1:5]
  
  if (FALSE)
  {
    n_simulations = 5
    n_stations = c(700, 300)
    pilfer_rates = c(0.2, 0.2)
    species_names = c("pol", "psd")  
    i = 1
  }
  
  # Empty data frame to hold results
  dat_out = data.frame()  
  
  for (i in 1:length(n_stations))
  {
    # Randomly pilfer seeds for species i  
    pilfered_i = rbinom(
      n = n_simulations,
      size = n_stations[i],
      prob = pilfer_rates[i])
    
    dat_i =
      data.frame(
        simulation = 1:n_simulations, 
        species    = species_names[i],
        n_pilfered = pilfered_i,
        n_intact   = n_stations[i] - pilfered_i,
        n_total    = n_stations[i])
    
    dat_i$pilfer_rate = dat_i$n_pilfered / dat_i$n_total
    dat_i$pilfer_odds = dat_i$n_pilfered / dat_i$n_intact
    
    dat_out = rbind(dat_out, dat_i)
  }
  
  # Sort the data in ascending order of simulation number and species:
  return(dat_out[order(dat_out$simulation, dat_out$species), ])
}



