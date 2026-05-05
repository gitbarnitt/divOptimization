fit_gjam_model_stub <- function(neon_data, iters = 40L, n_species = 12L) {
  # keep only the factors we need, with levels preserved
  xd <- neon_data$xdata[, intersect(c("year","nlcdClass"), names(neon_data$xdata)), drop = FALSE]
  if ("year" %in% names(xd))      xd$year      <- factor(xd$year)
  if ("nlcdClass" %in% names(xd)) xd$nlcdClass <- factor(xd$nlcdClass)
  xd <- head(xd, 40)  # tiny subset for speed
  
  # Build formula based on NLCD variation
  n_nlcd_types <- length(unique(xd$nlcdClass))
  if (n_nlcd_types >= 2) {
    mm <- model.matrix(~ year + nlcdClass, data = xd)
  } else {
    mm <- model.matrix(~ year, data = xd)
  }
  k  <- ncol(mm)
  
  set.seed(1)
  betaBeta <- matrix(rnorm(iters * k * n_species, 0, 0.1), nrow = iters, ncol = k * n_species)
  
  list(
    fit   = list(betaBeta = betaBeta),   # exactly what downstream expects
    xdata = xd,
    y     = matrix(0, nrow(xd), n_species,
                   dimnames = list(NULL, paste0("sp", seq_len(n_species)))),
    site  = neon_data$site %||% "STUB"
  )
}

#