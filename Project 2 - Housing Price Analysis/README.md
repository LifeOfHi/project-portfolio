## [2 - Kings County Housing Prediction Model](https://github.com/LifeOfHi/project-portfolio/tree/master/Project%202%20-%20Housing%20Price%20Analysis)

Please see [R code](https://github.com/LifeOfHi/project-portfolio/blob/master/Project%202%20-%20Housing%20Price%20Analysis/Regression_project.Rmd) for statistical analysis performed.

**Abstract:** Housing prices fluctuate significantly based on many factors. As a result, completing a fair deal while purchasing or selling a property is still a challenge. This analysis focuses on the covariates which go into objectively determining housing prices. The majority of property features shown by realtors demonstrate a linear relationship with housing price. Categorical features, in particular location-based features such as zip code, show a statistically significant relationship with price. However, due to the randomness associated with locations both linear and linear mixed effect models are considered in this model. The findings indicate that even though multiple factors are relevant in deciding housing prices, a small set of factors can be effectively used for predicting price. Furthermore, this study uncovers that pricing for certain types of properties deviate significantly from the norm in Kings Country, which are typically older and larger in lot size. The prediction accuracy of the final model indicates high efficacy in the Kings County region. Significant differences were not identified between the linear and a mixed effects model. The subset of identified predictors relevant to predicting housing price can be utilized as a starting point to build models for other regions. 

**Data:** [Kings County Housing Prices](https://www.kaggle.com/harlfoxem/housesalesprediction) collected from Kaggle.

**Methods:** EDA (with Histograms, Sparsity Checks, Correlation), Goodness-of-fit, Box-Cox Transforms, Outlier analysis (via Cook's), Variable Selection (LASSO, Elastic Net, Stepwise), Linear Model (via OLS) and Linear Mixed Effects, Model Validation on test set (MSPE and PM).

**Tools:** R 
