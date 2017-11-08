#' View a guess as pictures of items added together.
#' @import dplyr
#' @export
view_guess <- function(guess_str, item_labels, view = FALSE) {
  char_vector <- stringr::str_split_fixed(guess_str, "_", 4)
  num_vector <- rev(as.numeric(char_vector))
  num_vector <- num_vector[num_vector > 0]
  list_of_items <- as.list(num_vector)

  env <- new.env()
  if (missing(item_labels)) {
    data("ItemLabels", package = "totems", envir = env)
  } else {
    env$ItemLabels <- item_labels
  }

  images <- purrr::map(list_of_items, get_item_image, item_labels = env$ItemLabels)
  grobs <- purrr::map(images, function(path_to_jpeg) {
    grid::rasterGrob(jpeg::readJPEG(path_to_jpeg))
  })

  guess_grobs <- gridExtra::arrangeGrob(grobs = grobs, nrow = 1)

  if (view == TRUE) {
    grid::grid.newpage()
    grid::grid.draw(guess_grobs)
  } else {
    return(guess_grobs)
  }
}


#' Get the path to an item image based on its number.
#' @export
get_item_image <- function(item_number, item_labels) {
  item_filename <- get_item_filename(item_number, item_labels)
  image_filepath <- system.file("extdata/items", item_filename, package = "totems")
  if (!file.exists(image_filepath))
    stop(sprintf("Image for item #%s not found", item_number))

  image_filepath
}


#' Get the image filename for an item from it's image number.
#' @export
get_item_filename <- function(item_number, item_labels) {
  env <- new.env()
  if (missing(item_labels)) {
    data("ItemLabels", package = "totems", envir = env)
  } else {
    env$ItemLabels <- item_labels
  }
  dplyr::filter(env$ItemLabels, Number == item_number)[["Name"]]
}
