source("R/neurons/Connection.R")
source("R/functions/FSigmoid.R") # advanced

# The components needed by the class' constructor
requiredComponents <- list(outputList="list")

# Declare constructor
setClass("NeuronalNet",
         slots=requiredComponents
)

#' Constructor of \code{NeuronalNet}
#'
#' @details
#' Helps initializing and defaulting the new NeuronalNet
#'
#' @param inputNeuronsList
#' List with inputNeurons
#'
#' @param hiddenNeuronsCount
#' Number of hiddenNeurons
#'
#' @param outputNeuronsCount
#' Number of outputNeurons
#'
#' @return
#' A \code{NeuronalNet} class object that holds the new state.
#'
#' @export
NeuronalNet = function(inputNeuronsList, hiddenNeuronsCount, outputNeuronsCount)
{
  stopifnot(is.numeric(hiddenNeuronsCount)
            || is.numeric(outputNeuronsCount))

  hiddenList <- list()
  for(i in 1:hiddenNeuronsCount) {
    connections <- list()
    for(n in inputNeuronsList) {
      connections <- append(connections, Connection(n, 0.5, 0))
    }
    hiddenList <- append(hiddenList, OutputNeuron(connections))
  }

  outputList <- list()
  for(i in 1:outputNeuronsCount) {
    connections <- list()
    for(n in hiddenList) {
      connections <- append(connections, Connection(n, 0.5, 0))
    }
    outputList <- append(outputList, OutputNeuron(connections,
      activationFunction = function(input) {
        return(FSigmoid.activate(input))
      },
      derivateFunction = function(input) {
        return(FSigmoid.derive(input))
      },
    ))
  }

  new(Class="NeuronalNet",
      outputList=outputList)
}

#' Learn function of \code{NeuronalNet}
#'
#' @details
#' This function should train a neuronalNet.
#'
#' @param neuronalNet
#' NeuronalNet to work with.
#'
#' @param valueList
#' Contains values for outputNeurons.
#' If you have 3 outputNeurons the param should be c(1, 2, 3)
#' for example with your expected values.
#'
#' @return
#' A \code{NeuronalNet} class object that holds the new state.
#'
#' @export
Learn <- function(neuronalNet, valueList) {
  newList <- list()
  for (n in neuronalNet@outputList) {
    list2 <- list()
    for(c in n@connectionList) {
      c@neuron <- ResetNeuron(c@neuron)
      list2 <- append(list2, c)
    }
    n@connectionList <- list2

    newList <- append(newList, ResetNeuron(n))
  }
  neuronalNet@outputList <- newList
  # print(neuronalNet) > Reset working

  newList <- list()
  for (i in 1:length(neuronalNet@outputList)) {
    o <- neuronalNet@outputList[[i]]
    o <- ResetNeuron(o)
    o <- CalculateOutputDelta(o, expectedValue = valueList[[i]])
    o <- BackpropagateSmallDelta(o)

    # backpropagate hidden layer to input layer
    list2 <- list()
    for(c in o@connectionList) {
      c@neuron <- BackpropagateSmallDelta(c@neuron)
      list2 <- append(list2, c)
    }
    o@connectionList <- list2
    o <- DeltaLearn(o, 0.1) # learn outputNeuron

    # learn hidden layer
    list2 <- list()
    for(c in o@connectionList) {
      c@neuron <- DeltaLearn(c@neuron, 0.1)
      c@neuron <- ApplyBatch.OutputNeuron(c@neuron) # Applying instantly
      list2 <- append(list2, c)
    }
    o@connectionList <- list2

    o <- ApplyBatch.OutputNeuron(o) # Applying instantly
    newList <- append(newList, o)
  }

  neuronalNet@outputList <- newList
  return (neuronalNet)
}

#' Print the \code{NeuronalNet}
#'
#' @details
#' Very cool and detailed function to print three layers of this net.
#' The bCalculated Column was used to test the net, if reset works.
#'
#' @param neuronalNet
#' NeuronalNet to work with.
#'
#' @return
#' A \code{NeuronalNet} class object that holds the new state.
#'
#' @export
print.NeuronalNet <- function(neuronalNet) {
  hiddenList <- list()
  # since it is fully meshed
  for(c in neuronalNet@outputList[[1]]@connectionList) {
    hiddenList <- append(hiddenList, c@neuron)
  }

  inputList <- list()
  for(c in hiddenList[[1]]@connectionList) {
    inputList <- append(inputList, c@neuron)
  }

  # first matrix > Count Neurons
  variables <- c("Input", "Hidden", "Output")
  matrix <- matrix(nrow = 1, ncol = 3)
  matrix[1,] <- c(length(inputList), length(hiddenList), length(neuronalNet@outputList))
  rownames(matrix) <- c("Neurons")
  colnames(matrix) <- variables
  print(matrix)

  # outputNeurons matrix
  print("Output Neurons")
  omList <- list() # outputMatrix list
  for(o in neuronalNet@outputList) {
    sum <- 0
    for(c in o@connectionList) {
      sum <- sum + c@weight
    }
    omList <- append(omList, c(c(GetOutputNeuronValue(o), sum), o@bCalculated)) # will still be like 1, 2, 3 in the list ;)
  }

  matrix <- matrix(unlist(omList), ncol=3, byrow = TRUE)
  colnames(matrix) <- c("Weights Sum", "Value", "bCalculated")
  print(matrix)

  # hiddenNeurons matrix
  print("Hidden Neurons")
  hmList <- list() # outputMatrix list
  for(o in hiddenList) {
    sum <- 0
    for(c in o@connectionList) {
      sum <- sum + c@weight
    }
    hmList <- append(hmList, c(c(GetOutputNeuronValue(o), sum), o@bCalculated)) # will still be like 1, 2, 3 in the list ;)
  }

  matrix <- matrix(unlist(hmList), ncol=3, byrow = TRUE)
  colnames(matrix) <- c("Weights Sum", "Value", "bCalculated")
  print(matrix)

  # hiddenNeurons matrix
  print("Input Neurons")
  imList <- list() # outputMatrix list
  for(o in inputList) {
    imList <- append(imList, o@value + o@bias) # will still be like 1, 2, 3 in the list ;)
  }

  matrix <- matrix(unlist(imList), ncol=1, byrow = TRUE)
  colnames(matrix) <- c("Value")
  print(matrix)
}
