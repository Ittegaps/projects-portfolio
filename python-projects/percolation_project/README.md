This project implements a simulation of the site percolation problem on a square lattice using Monte Carlo methods. The model analyzes the probability of forming a connected path between the first and last row of the lattice as a function of occupation probability p.

The implementation is based on the _Burning Method_, which is used to determine whether a percolating path exists in each simulation run. The program reads input parameters (such as lattice size, number of trials, and probability range) from an external configuration file (`perc_ini.txt`).

To execute the project, the `percolation_burning_method.py` module must first be run for different input configurations. The generated results are then used as input for the `percolation_threshold module.py`, which produces the final plots illustrating the relationship between percolation probability and p.
