## Arbitrage of Forecasting Experts

This is a repo containing a set of experiments for forecast combination. 
Below is a brief summary on how to run the scripts.

### Content

#### Data

Contains the time series datasets and a script to transform these into a tabular format using time delay embedding.

#### Sources

Source functions used throughout the experiments, including methods, workflows, utility functions, 
plotting functions, and pipelines for analysing the experiments.

#### Experiments

Scripts used for running the experiments

#### Analysis

Scripts used for analysing the experiments

### How to run the experiments

1. Running data/data.r transforms a set of time series into a tabular format using time delay embedding 

2. experiments/all-pipelines.r is the main script for running the experiments.

    a. State of the art forecast combination methods in 62 time series;
    
    b. Scalability analysis (in runtime);
    
    c. A study on the value of adding new experts to the ensemble;
    
    d. A comparison of the sequential reweighting procedure versus removing correlation in feature space;
    
    e. A study on the impact of the sequential reweighting process on state of the art  methods for forecast combination.

2.1. experiments/all-pipelines-quickcheck.r runs a sample of the experiments to check if everything can be run smoothly

2.2. The pipelines can be ran separately, by running each of the numbered scripts (1-5)
    
3. experiments/sensitivity-analysis.r

    a. A study on the sensitivity analysis of the arbitrage of forecasting experts to the lambda and omega parameters
    
4. experiments/retrain-analysis.r 

    a. A study on different deployment solution of the arbitrage of forecasting experts approach.
    
    
## Contact

Feel free to contact me at vitor.cerqueira@fe.up.pt


