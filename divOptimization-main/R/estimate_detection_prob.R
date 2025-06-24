estimate_detection_prob <- function(simulated_fits, threshold = 0.05) {
  if (length(simulated_fits) == 0) return(NULL)
  
  library(dplyr)
  library(tibble)
  
  extract_detection <- function(sim_results) {
    purrr::map_dfr(sim_results, function(effects, label) {
      if (is.null(effects) || all(is.na(effects))) {
        return(tibble(species = label, direction = NA, detection_prob = NA))
      }
      
      effects <- unlist(effects)
      direction <- ifelse(grepl("increase", label), "increase", "decrease")
      species <- gsub("_increase|_decrease", "", label)
      
      prob_detected <- mean(abs(effects) > threshold, na.rm = TRUE)
      
      tibble(species = species,
             direction = direction,
             detection_prob = prob_detected)
    }, .id = "label")
  }
  
  site_results <- purrr::imap_dfr(simulated_fits, function(sim_res, site_id) {
    if (is.null(sim_res)) return(NULL)
    
    extract_detection(sim_res) %>%
      mutate(siteID = site_id)
  })
  
  return(site_results)
}
