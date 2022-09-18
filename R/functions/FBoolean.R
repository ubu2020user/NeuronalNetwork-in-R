#' Example activation function
FBoolean.activate <- function(input) {
  if (!is.numeric(input))
    stop("Activation Function Input must be numeric!");

  if(input < 0) {
    return (0);
  } else {
    return (1);
  }
}

#' Exmaple derivation function
FBoolean.derive <- function(input) {
  if (!is.numeric(input))
    stop("Derivation Function Input must be numeric!");

  print("WARNING: Not the write derivative used for FBoolean!!!")

  return (1); # not the right derivative => not needed right now!
}
