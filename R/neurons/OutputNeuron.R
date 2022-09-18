source("R/functions/FIdentity.R") # standard

# The components needed by the class' constructor
requiredComponents <- list(connectionList="list",
                           bCalculated="logical",
                           smallDelta="numeric",
                           value="numeric",
                           bias="numeric",
                           activationFunction="function",
                           derivateFunction="function")

# Declare constructor
setClass("OutputNeuron",
         slots=requiredComponents,
         contains = "Neuron"
)

#' A \code{OutputNeuron} class constructor. For developers only.
#'
#' @details
#' Sets a lot of default values and helps intializing "Neurons"
#'
#' @param connectionList
#' List with Connection objects
#'
#' @param activationFunction
#' Holds a function(input) that returns an activated value. Use predefined methods here. (FSigmoid)
#'
#' @param derivateFunction
#' Holds a function(input) that returns an activated value. Use predefined methods here. (FSigmoid)
#'
#' @param bias
#' Bias is a numeric value that will be added to the calculated value of the Neuron: Value = NeuronsValue + bias
#'
#' @return
#' A \code{OutputNeuron} class object.
#'
#' @export
OutputNeuron = function(connectionList,
                        activationFunction=function(input) {
                          return(FIdentity.activate(input))
                          },
                        derivateFunction=function(input) {
                          return(FIdentity.derive(input))
                          },
                        bias=0)
{
  new(Class="OutputNeuron",
      value=1,
      bias = bias,
      connectionList = connectionList,
      bCalculated = FALSE,
      activationFunction = activationFunction,
      derivateFunction = derivateFunction,
      smallDelta = 0.01)
}

#' Calculate the \code{OutputNeuron} value.
#'
#' @details
#' This function is recursive. It sums up all previous neurons values with their connections.
#' bCalculated helps saving calculating power by returning a calculated value if nothing changed.
#' TODO: Check if this is working. Value gets returned. But will outputNeuron hold the calculated states?
#'
#' @param outputNeuron
#' Needs an outputNeuron to start calculating.
#'
#' @return
#' Returns the \code{OutputNeuron} value as numeric
#'
#' @export
GetOutputNeuronValue <- function(outputNeuron) {
  if(!outputNeuron@bCalculated) {
    sum <- 0
    newList <- list()
    for(c in outputNeuron@connectionList) {
      if(class(c) == "Connection"){
        sum <- sum + GetConnectionValue(c);
      } else {
        print("You inputted")
        print(class(c@neuron))
        stop("What did you input into the connectionList. Somewhere is not a Connection.")
      }
      newList <- append(newList, c)
    }
    outputNeuron@connectionList <- newList
    outputNeuron@value <- outputNeuron@activationFunction(sum);
  }

  return (outputNeuron@value);
}

#' A \code{OutputNeuron} function
#'
#' @details
#' Calculates the difference of the expected value and the true value of the outputNeuron.
#' TODO: Check if GetOutputNeuronValue saves state or just value gets returned...
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @param expectedValue
#' Value that was expected.
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
CalculateOutputDelta <- function(outputNeuron, expectedValue) {
  outputNeuron@value <- GetOutputNeuronValue(outputNeuron);
  outputNeuron@bCalculated <- TRUE;
  outputNeuron@smallDelta <- (expectedValue - outputNeuron@value);
  return(outputNeuron);
}

#' A \code{OutputNeuron} backpropagation attempt
#'
#' @details
#' Backpropagates if outputNeuron holds smallDelta.
#' Then the difference between should and is value will be splitted on connections and distributed proportionally.
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
BackpropagateSmallDelta <- function(outputNeuron) {
  newList <- list()
  for(c in outputNeuron@connectionList) {
    if(class(c@neuron) == "OutputNeuron"){
      c@neuron@smallDelta <- outputNeuron@smallDelta  * c@weight;
    }
    newList <- append(newList, c)
  }
  outputNeuron@connectionList <- newList
  return(outputNeuron)
}

#' Best practice to add connections. Do not forget to save the new state!
#' Therefore the outputNeuron is returned!
#'
#' @details
#' Should add a connection to the outputNeuron.
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @param connectionList
#' List containing all connections.
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
AddConnection <- function(outputNeuron, connectionList) {
  outputNeuron@connectionList <- append(outputNeuron@connectionList + list(connectionList))
  return (outputNeuron)
}

#' Set \code{OutputNeuron} activationFunction.
#' Best practice: Set in constructor
#'
#' @details
#' Sets the activation function
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @param activationFunction
#' The function to activate an input with.
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
SetActivationFunction <- function(outputNeuron, activationFunction) {
  outputNeuron@activationFunction <- activationFunction
  return (outputNeuron)
}

#' A \code{OutputNeuron} apply batch for the backpropagation
#'
#' @details
#' If you backpropagated you can apply the "batch". You can apply all calculated changes.
#' E.g. you bp. for 100 times then you can use those and change the weights all in one run.
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @param momentumFactor
#' If the momentumFactor = -1 it will be set to 0.9
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
ApplyBatch.OutputNeuron <- function(outputNeuron, momentumFactor = -1) {
  newList <- list()
  for(c in outputNeuron@connectionList) {
    c <- ApplyBatch.Connection(c, momentumFactor)
    newList <- append(newList, c)
  }
  outputNeuron@connectionList <- newList
  return (outputNeuron);
}

#' Calculates the BigDelta of \code{OutputNeuron}
#'
#' @details
#' Calculates the BigDelta.
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @param epsilon
#' This is the learning rate.
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
DeltaLearn <- function(outputNeuron, epsilon = 0.01) {
  if(!is.numeric(epsilon)) stop("Epsilon has to be numeric!");

  outputNeuron@value <- GetOutputNeuronValue(outputNeuron)
  outputNeuron@bCalculated <- TRUE

  bigDeltaFactor <- outputNeuron@derivateFunction(outputNeuron@value) * epsilon * outputNeuron@smallDelta;

  newConnections <- list()
  for(connection in outputNeuron@connectionList) {
    bigDelta <- 0

    if(class(connection@neuron) == "Neuron"){
      bigDelta <- bigDeltaFactor * connection@neuron@value;
    } else {
      bigDelta <- bigDeltaFactor * GetOutputNeuronValue(connection@neuron);
    }
    connection@weightTemp <- connection@weightTemp + bigDelta;
    newConnections <- append(newConnections, connection)
  }

  outputNeuron@connectionList <- newConnections

  return (outputNeuron);
}

#' Reset the \code{OutputNeuron}.
#'
#' @details
#' Sets bCalculated to false and smallDelta to 0.
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @return
#' A \code{OutputNeuron} class object that holds the new state.
#'
#' @export
ResetNeuron <- function(outputNeuron) {
  outputNeuron@bCalculated = FALSE;
  outputNeuron@smallDelta = 0;

  newList <- list()
  for(n in outputNeuron@connectionList) {
    if(class(n@neuron) == "OutputNeuron") {
      n@neuron <- ResetNeuron(n@neuron)
    }
    newList <- append(newList, n)
  }
  outputNeuron@connectionList <- newList
  return (outputNeuron);
}

#' Custom cool print function of \code{OutputNeuron}
#'
#' @details
#' Took a lot of time to figure this all out :D
#'
#' @param outputNeuron
#' OutputNeuron to work with.
#'
#' @export
print.OutputNeuron <- function(outputNeuron,  ...) {
  if(is.null(outputNeuron@connectionList)) {
    # If [Connection] is null #
    print("Connection is null")
    return;
  }

  print("[OutputNeuron]")
  print(Neuron(outputNeuron@value, outputNeuron@bias))
  print(outputNeuron@connectionList)
}

