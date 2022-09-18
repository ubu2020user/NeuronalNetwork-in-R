#' Example activation function
FHyperbolicTangent.activate <- function(input) {
  if (!is.numeric(input))
    stop("Activation Function Input must be numeric!");

  epx <- exp(input)
  enx <- exp(-input)

  return (
    (epx - enx)/(epx + enx)
    );
}

#' Exmaple derivation function
FHyperbolicTangent.derive <- function(input) {
  if (!is.numeric(input))
    stop("Derivation Function Input must be numeric!");

  tanh <- FHyperbolicTangent.activate(input)
  return (1 - tanh * tanh);
}

# Checked with https://keisan.casio.com/exec/system/15411343272927
# and https://math.tools/calculator/trignometry/tanh
# print(FHyperbolicTangent.derive(0.1))

# rounds after .7 or so
# output in radians not degrees!

