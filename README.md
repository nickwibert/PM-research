# Surface PM2.5 Pollution
##### Investigating particulate matter in air pollution across the United States, and formulating possible interventions to reduce its impact on public health 

The data used in this research comes from the [Atmospheric Composition Analysis Group](https://sites.wustl.edu/acag/). Ground-level fine particulate matter (PM2.5) total and compositional mass concentrations for North America, China, Europe, and other regions are publicly available at [this link](https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V4.NA.03). We used the monthly data from the [V4.NA.02 dataset](https://wustl.app.box.com/s/wk3144jc6xfy6ujfvyv5m2yfk33nz2nn), restricting the boundaries to the continental US.

As of October 2021, my assistance included performing preliminary data exploration by visualizing the spatial data in R and running clustering algorithms to determine the spatial and temporal relationships of the different elements composing the particulate matter (black carbon, dust, sulfate, etc.).

As of February 2022, I have started looking into causal inference using methods like synthetic control and Bayesian structural time series (BSTS), the latter of which is not a causal effects method but can be extended to causal applications. These methods will allow us to evaluate the impact of various pollution policies or other time-relevant events on the pollution mixture / concentration in an area.
