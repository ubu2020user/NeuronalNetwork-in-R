source("R/neurons/Neuron.R")
source("R/neurons/OutputNeuron.R")

# The components needed by the class' constructor
requiredComponents <- list(weight="numeric",
                           neuron= "Neuron",
                           momentum="numeric", # Momentum can enhance the learning rate
                           weightTemp="numeric")

# Declare constructor
setClass("Connection",
         slots=requiredComponents
)

#' Constructor of \code{Connection}
#'
#' @details
#' Helps to intialize and default the Connection class object.
#'
#' @param neuron
#' Neuron or OutputNeuron to work with.
#' We use inheritance here. Really Cool!
#'
#' @param weight
#' Weight of the connection.
#'
#' @param momentum
#' Momentum for batchApply
#'
#' @return
#' A \code{Connection} class object that holds the new state.
#'
#' @export
Connection = function(neuron,
                      weight,
                      momentum = 0.9)
{
  if (!is.numeric(weight))
    stop("Weight must have numeric type")

  new(Class="Connection",
      neuron = neuron,
      weight = weight,
      momentum = momentum,
      weightTemp = 0)
}

#' Calculates the \code{Connection} value
#'
#' @details
#' Calculates the value of this connection recursively.
#' TODO Same as ONeuron, check if state is being saved!
#'
#' @param connection
#' Connection to work with.
#'
#' @return
#' A \code{Connection} class object that holds the new state.
#'
#' @export
GetConnectionValue <- function(connection) {
  if(is.null(connection)) return(0);
  if(is.null(connection@neuron)) return (0);

  if(class(connection@neuron) == "Neuron"){
    return (connection@neuron@value * connection@weight + connection@neuron@bias)
  } else {
    connection@neuron@value <- GetOutputNeuronValue(connection@neuron)
    connection@neuron@bCalculated <- TRUE
    return (connection@neuron@value * connection@weight + connection@neuron@bias)
  }
  return (0);
}

#' Apply value changes to \code{Connection}
#'
#' @details
#' If deltas were calculated the new weight value can be applied.
#'
#' @param connection
#' Connection to work with.
#'
#' @param momentumFactor
#' Momentums factor to enhance learn effect and
#' circumvent 'stuckholes'
#'
#' @return
#' A \code{Connection} class object that holds the new state.
#'
#' @export
ApplyBatch.Connection <- function(connection, momentumFactor = -1) {
  connection@momentum <- connection@momentum + connection@weightTemp;
  if(momentumFactor == -1) {
    # print("Using default momentum value of 0.9")
    connection@momentum <- connection@momentum * 0.9; # default value
  } else {
    connection@momentum <- connection@momentum * momentumFactor;
  }

  connection@weight <- connection@weightTemp + connection@momentum;
  connection@weightTemp = 0

  return (connection)
}

#' Add weightDelta to \code{Connection}
#'
#' @details
#' Adding weightDelta to the connection.
#'
#' @param connection
#' Connection to work with.
#'
#' @param weightDelta
#' New numeric value
#'
#' @return
#' A \code{Connection} class object that holds the new state.
#'
#' @export
AddMomentumWeight <- function(connection, weightDelta = 1) {
  connection@weightTemp <- connection@weightTemp + weightDelta;
  return (connection);
}

#' Custom print function a \code{Connection}
#'
#' @details
#' Print the connections values in a coole visualization.
#'
#' @param connection
#' Took a lot of work to figure it out.
#'
#' @return
#' A \code{Connection} class object that holds the new state.
#'
#' @export
print.Connection <- function(connection,  ...) {
  if(is.null(connection@neuron)) {
    # If [Connection Neuron] is null #
    print("Neuron is null")
    return;
  }

  if(is.null(connection@weight)) {
    # If [Connection Weight] is null #
    print("Conection Weight is null")
    return;
  }

  variables <- c("Weight")
  values <- c(connection@weight)
  matrix <- matrix(1:1)
  matrix[1,] <- values[1]

  rownames(matrix) <- variables
  colnames(matrix) <- "Connection"

  print("[Connection]")
  print(matrix)
  print(connection@neuron)
}
