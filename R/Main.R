print("Set your working directory to Praxisarbeit/")
print("This is your current directory: ")
getwd()
# error if working directory is not right!
stopifnot(endsWith(getwd(), "NeuronalNetwork-in-R"))
print("Everything is fine. Initializing..")

# use Classes
source("R/NeuronalNet.R")

# init neuronalNet
n <- NeuronalNet(inputNeuronsList =
                   list(Neuron(4), Neuron(2), Neuron(3)),
                 hiddenNeuronsCount = 1,
                 outputNeuronsCount = 3)
print(n)
n <- Learn(n, list(0, 1, 1)) # learn
print(n)
#' learn a lot of times
#' This poc does apply the batches every time.
#' Consider programming your own NeuronalNet class.
for(i in 1:100) {
  n <- Learn(n, list(0, 2, 1))
}
print(n)

