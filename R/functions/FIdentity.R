#' Example activation function
FIdentity.activate <- function(input) {
  if (!is.numeric(input))
    stop("Activation Function Input must be numeric!");

  return (input);
}

#' Exmaple derivation function
FIdentity.derive <- function(input) {
  if (!is.numeric(input))
    stop("Derivation Function Input must be numeric!");

  return (1);
}
