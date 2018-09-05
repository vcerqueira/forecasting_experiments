## Arbitrage of Forecasting Experts

This is a repo containing a set of experiments for forecast combination. 
Below is a brief summary on how to run the scripts.

### Content

#### data

Contains the time series datasets and a script to transform these into a tabular format using time delay embedding.

#### sources

Source functions used throughout the experiments, including methods, workflows, utility functions, 
plotting functions, and pipelines for analysing the experiments.

#### experiments

Scripts used for running the experiments

#### analysis

Scripts used for analysing the experiments

### How to run the experiments

1. Running data/data.r transforms a set of time series into a tabular format using time delay embedding 

2. experiments/main-analysis.r is the main script for running the experiments.

    a. State of the art forecast combination methods in 62 time series;
    
    b. Scalability analysis (in runtime);
    
    c. A study on the value of adding new experts to the ensemble;
    
    d. A comparison of the sequential reweighting procedure versus removing correlation in feature space;
    
    e. A study on the impact of the sequential reweighting process on state of the art  methods for forecast combination.
    
    
3. experiments/sensitivity-analysis.r

    a. A study on the sensitivity analysis of the arbitrage of forecasting experts to the lambda and omega parameters
    
4. experiments/retrain-analysis.r 

    a. A study on different deployment solution of the arbitrage of forecasting experts approach.
    
    
## Contact

Feel free to contact me at vitor.cerqueira@fe.up.pt


