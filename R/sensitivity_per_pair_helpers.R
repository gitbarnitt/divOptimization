plots_for_pair <- function(xdata, y1, y2) {
  p1 <- unique(xdata$plotID[xdata$year == y1])
  p2 <- unique(xdata$plotID[xdata$year == y2])
  sort(intersect(p1, p2))
}

year_pairs_consecutive <- function(xdata) {
  ys <- sort(unique(xdata$year))
  if (length(ys) < 2) return(character())
  paste(ys[-length(ys)], ys[-1], sep = "_")
}