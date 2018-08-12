## Arbitrage of Forecasting Experts

This is a repo containing a set of experiments for forecast combination. 
Below is a brief summary on how to run the scripts and what they contain.

### Folders

#### data

Contains the time series datasets and a script to transform these to a tabular format

#### sources

Source functions used throughout the experiments, including methods, workflows, utility functions, 
plotting functions, and pipelines for analysing the experiments.

#### experiments

Scripts used for running the experiments

#### analysis

Scripts used for analysing the experiments


### How to run the experiments

1. Running data/data.r transforms a set of time series into a tabular format 

2. experiments/1-main-analysis.r is the main script for running the experiments. It takes a while to compute all the analysis. To speed up computations you can comment some of the pipelines, reduce training/test data, no. of monte carlo simulations, or use a lighter ensemble. The following experiments are run:

    a. State of the art forecast combination methods in 62 time series;
    
    b. Scalability analysis (in runtime) of several methods, including the arbitrage of forecasting experts;
    
    c. Study on the value of adding new experts to the ensemble;
    
    d. Comparison of the sequential reweighting procedure versus removing correlation in feature space;
    
    e. Study on the impact of the sequential reweighting process on state of the art  methods for forecast combination.
    
    
3. experiments/2.1-sensitivity-analysis.r

    a. Study on the sensitivity analysis of the arbitrage of forecasting experts to the lambda and omega parameters
    
4. experiments/2.2-retrain-analysis.r 

    a. Study on different deployment solution of the arbitrage of forecasting experts approach.
    
    
## Contact

If you have any doubt, please contact me at vitor.cerqueira@fe.up.pt


