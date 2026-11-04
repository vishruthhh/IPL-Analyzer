# Shared chart colours keep the app's appearance consistent.
pal <- list(navy = "#0f2a5c", orange = "#f97316", teal = "#14b8a6",
            grey = "#64748b", light = "#e2e8f0", red = "#dc2626")

theme_ipl <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold", colour = pal$navy),
          plot.subtitle = element_text(colour = pal$grey),
          axis.title = element_text(colour = pal$grey),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom")
}
