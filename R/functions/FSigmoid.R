#' Example activation function
FSigmoid.activate <- function(input) {
  if (!is.numeric(input))
    stop("Activation Function Input must be numeric!");

  # exp(1) <- euler's number
  return (1 / (1 + (exp(1) ^ -input)));
}

#' Exmaple derivation function
FSigmoid.derive <- function(input) {
  if (!is.numeric(input))
    stop("Derivation Function Input must be numeric!");

  sigmoid <- FSigmoid.activate(input);
  return(sigmoid * (1 - sigmoid));
}

# checked with https://keisan.casio.com/exec/system/15157249643425
# print(FSigmoid.derive(-15))

