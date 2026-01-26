

#' @export
getDensityColors <- function(x, y, transform = FALSE) {

    if (transform) {
        dataframe <- data.frame(log2(x), log2(y))
    } else {
        dataframe <- data.frame(x, y)
    }

    z <- grDevices::densCols(dataframe, colramp = grDevices::colorRampPalette(c("black", "white")))

    density <- grDevices::col2rgb(z)[1, ] + 1L

    return(density)

}