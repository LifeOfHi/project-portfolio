# Project Portfolio

Welcome, my name is Hari Ilangovan, and in this repository I maintain a living list of interesting projects I worked on in Data Science and Visualization. Additional relevant details for each project are included in the project folders.

## [1 - Anomaly Detection - Opex Analytics sponsored Case Competition](https://github.com/LifeOfHi/project-portfolio/tree/master/Project%201%20-%20Opex%20Analytics%20Case)

**1st Place Finish in Presentation & Storytelling** - [winning video submission](https://spark.adobe.com/video/NSes6Wi9eP6oY)

The consulting firm [Opex Analytics](https://opexanalytics.com/) hosted an analytics case competition at Georgia Tech. If you'd like to jump straight to my team's [winning video submission](https://spark.adobe.com/video/NSes6Wi9eP6oY) go right ahead. My intention with the video is to provide an overview of the problem, reiterate the objective, and walk through my proposed analytics solution in an engaging and meaningful way to business stakeholders. Per request of the sponsor, the data is not allowed to be publicly available, but I have included an overall summary of our analysis below:

**The Problem:** A very large eCommerce company is notorious for ordering items erratically from week to week. This was problematic to the client, a _Consumer Goods Company_, because unexpectedly high orders wiped out their inventory in a distribution center or the network at large.

**Goal:** Respond to the volatility of eCommerce demand by detecting anomalous orders and predicting items most likely to cut (i.e. quantity not fulfilled in full in an order).

**Business Value:** Increased service levels at modest and deliberate inventory cost.

**Data Issues Encountered:** No strong correlations between covariates, lack of strong predictive power, high volatility between product types, low total volume of data (weekly data only for one year)

**Our Approach:**

- Perform exploratory data analysis, cleansing, and merging of datasets
- Split the data into training and test
- Use domain knowledge to feature engineer additional predictors to test in models
- Apply regression techniques and time-series models (exponential smoothing & ARIMA)
- Compare results on test set and weigh pros and cons in terms of interpretability and performance
- Provide final model recommendation while addressing limitations and future considerations for improvement

If you would like more details, please check out the [video submission](https://spark.adobe.com/video/NSes6Wi9eP6oY)!

## [2 - Kings County Housing Prediction Model](https://github.com/LifeOfHi/project-portfolio/tree/master/Project%202%20-%20Housing%20Price%20Analysis)

**Advanced Regression Analysis and Modeling**

Please see [R code](https://github.com/LifeOfHi/project-portfolio/blob/master/Project%202%20-%20Housing%20Price%20Analysis/Regression_project.Rmd) for statistical analysis performed.

**Abstract:** Housing prices fluctuate significantly based on many factors. As a result, completing a fair deal while purchasing or selling a property is still a challenge. This analysis focuses on the covariates which go into objectively determining housing prices. The majority of property features shown by realtors demonstrate a linear relationship with housing price. Categorical features, in particular location-based features such as zip code, show a statistically significant relationship with price. However, due to the randomness associated with locations both linear and linear mixed effect models are considered in this model. The findings indicate that even though multiple factors are relevant in deciding housing prices, a small set of factors can be effectively used for predicting price. Furthermore, this study uncovers that pricing for certain types of properties deviate significantly from the norm in Kings Country, which are typically older and larger in lot size. The prediction accuracy of the final model indicates high efficacy in the Kings County region. Significant differences were not identified between the linear and a mixed effects model. The subset of identified predictors relevant to predicting housing price can be utilized as a starting point to build models for other regions.

**Data:** [Kings County Housing Prices](https://www.kaggle.com/harlfoxem/housesalesprediction) collected from Kaggle.

**Methods:** EDA (with Histograms, Sparsity Checks, Correlation), Goodness-of-fit, Box-Cox Transforms, Outlier analysis (via Cook's), Variable Selection (LASSO, Elastic Net, Stepwise), Linear Model (via OLS) and Linear Mixed Effects, Model Validation on test set (MSPE and PM).

**Tools:** R

## [3 - Optimizing Spending to Improve Athletic Department Performance](https://github.com/LifeOfHi/project-portfolio/tree/master/Project%203%20-%20Optimizing%20Spending%20to%20Improve%20Athletic%20Department%20Performance)

**Statistical Learning**

**Abstract:** Collegiate athletics has a considerable impact both on institutions and student life due to large revenue and expenditure figures. In the present study, an investigation into the relationship between athletic budget management and institutional athletic performance is conducted. Using Capital One Cup ranking score as a metric of performance, several data mining techniques are applied in order to identify significant financial indicators of collegiate athletic success. The results demonstrate the importance of investing in medical expenses and increasing revenues in order to improve overall athletic department performance. Moreover, the analysis highlights the necessity of data collection beyond expenses and revenues, which will enable future researchers to enhance the predictive power of the formulated models.

**Tools:** R
