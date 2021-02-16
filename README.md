
<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li><a href="#about-the-project">About The Project</a> </li>
    <li><a href="#usedPackages">Packages</a></li>
    <li><a href="#structre-of-code">Repo Structure</a></li>
    <li><a href="#results">Results Overview</a></li>
  </ol>
</details>


<!-- ABOUT THE PROJECT -->
## About The Project

The aim of this project was to study ABC Sampling Techniques for repulsive mixture models in order to solve clustering problems. The code was developed in R considering incrementally more general problems. The most general model formulation is the following:

![alt text](images/forMD1.png)

### Packages

The entire code was written in R. The important packages we used are:
* coda, gtools, transport, MCMCpack

* MASS, extraDistr, spatstat
 
## Repo Structure

In the code directory you can find all the code: to run the sampler you just need to open the main file, which is ABCForNormalMixture. In the images directory there are the pictures needed for the readme file, while in the data directory we uploaded some of the most meaniningful simulations.


## Results Overview

Below you can see the results obtained when running our sampler on a Gaussian Mixture using three different repulsive priors:

 <img src="images/forMD2.png" width="100%" height="50%">
 <img src="images/forMD3.png" width="100%" height="50%">
 <img src="images/forMD4.png" width="100%" height="50%">
