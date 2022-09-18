requiredComponents <- list(value = "numeric", bias = "numeric")

# Declare class
setClass("Neuron",
   slots=requiredComponents,
   #package=neuronsPackage
)

#' A \code{Neuron} class constructor. For developers only.
#'
#' @details
#' Sets a lot of default values and helps intializing "Neurons"
#'
#' @param value
#' Contains the neurons value
#'
#' @param bias
#' Bias is a numeric value that will be added to the calculated value of the Neuron: Value = NeuronsValue + bias
#'
#' @return
#' A \code{Neuron} class object.
#'
#' @export
Neuron = function(value = 1, bias = 0)
{
  if (!is.numeric(value) || !is.numeric(bias))
    stop("data must have numeric type")

  new(Class="Neuron", value=value, bias = bias)
}

#' A \code{Neuron} Custom print function.
#'
#' @details
#' Sets a lot of default values and helps printing and viewing "Neurons" values
#'
#' @param neuron
#' Needs a neuron to print!
#'
#' @export
print.Neuron <- function(neuron,  ...) {
  if(is.null(neuron)) {
    # If [Neuron] is null #
    print("Neuron is null")
    return;
  }

  variables <- c("Weight", "Bias")
  values <- c(neuron@value, neuron@bias)
  table <- data.frame(variables, values)
  matrix <- matrix(1:2)
  matrix[1,] <- values[1]
  matrix[2,] <- values[2]

  rownames(matrix) <- variables
  colnames(matrix) <- "Neuron"

  print(matrix)
}
