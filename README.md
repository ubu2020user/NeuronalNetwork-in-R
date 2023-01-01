# NeuronalNetwork-in-R
This project was an attempt of a **proof of concept** to create a NeuronalNetwork from scratch.
The language was learned and used in a months time.

# General Notice #
This achievement was completed as part of the course for an examination module.
The question was if it is possible to create an object oriented neuronal network in R.
Most of the parts of this neuronal network work. 

The neuronal network example does not work right. 
It has some flaws, that could not be identified and fixed in time.
Nevertheless, it works somehow and is interesting to be viewed. Give it a try ;)

# Introduction #
In **Main.R** file you find an example how to use the objects and classes.
It depends on **NeuronalNet.R**. This class contains an example neuronal network out of
the implemented work. The batch is applied after every learn!

For your needs you can create your own "Network" class with different layers, 
momentum changes, batch changes, etc.

The NN example was for testing purposes and as proof of concept.

If you want to use neuronal networks for real tasks, I encourage you to *use libraries* that
are tested, developed and maintained regularly. Those are performant and overall bettter.

_________

If you want to improve, enhance or fix this work. 
Feel free :)

The print function(s) of the NN was the coolest part of the whole work! 
I love it! <3

_________

Personally, I learned a lot about R. Since it is not object oriented in the 
"old fashioned" way, I had difficulties creating classes, functions and to connect them.

A major topic is the state of variables. If a function alters a variable, it does not get a new value 
(I called it "state" in the docs).
The function **does not** override the original variable but creates a new one.

You have to do it the following way:
```x <- function(x)```

Such things were new to me and consumed a lot of time.
But I learned a great deal about R, I wouldn't have the usual way.

If you'd like a real presentation and 1to1 experience sharing, 
feel free to contact me, :)

Dan

