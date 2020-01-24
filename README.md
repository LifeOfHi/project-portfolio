# Project Portfolio

Welcome, my name is Hari Ilangovan, and in this repository I maintain a living list of interesting projects I worked on in Data Science and Visualization. Additional relevant details for each project are included in the project folders.

## Project 1: Opex Analytics sponsored Case Competition

**Result - 1st Place Finish in Presentation & Storytelling**

The consulting firm [Opex Analytics](https://opexanalytics.com/) hosted an analytics case competition at Georgia Tech. If you'd like to jump straight to my team's [winning video submission](https://spark.adobe.com/video/NSes6Wi9eP6oY) go right ahead. My intention with the video is to provide an overview of the problem, reiterate the objective, and walk through my proposed analytics solution in an engaging and meaningful way to business stakeholders. Per request of the sponsor, the data is not allowed to be publicly available, but I have included an overall summary of our analysis below:

**The Problem:** A very large eCommerce company is notorious for ordering items erratically from week to week. This was problematic to the client, a *Consumer Goods Company*, because unexpectedly high orders wiped out their inventory in a distribution center or the network at large.

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
