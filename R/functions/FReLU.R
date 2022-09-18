#' Example activation function
FReLU.activate <- function(input) {
  if (!is.numeric(input))
    stop("Activation Function Input must be numeric!");

  if(input >= 0) {
    return (input);
  } else {
    return (0);
  }
}

#' Exmaple derivation function
FReLU.derive <- function(input) {
  if (!is.numeric(input))
    stop("Derivation Function Input must be numeric!");

  if(input >= 0) {
    return(1);
  } else {
    return (0);
  }
}
