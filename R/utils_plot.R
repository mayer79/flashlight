rotate_x <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
}

override_alpha <- function() {
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1)))
}
