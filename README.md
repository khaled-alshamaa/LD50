# Lethal Dose/Concentration Calculator
In toxicology, the median lethal dose, LD50 (abbreviation for "lethal dose, 50%"), LC50 (lethal concentration, 50%) is a measure of the lethal dose of a toxin, radiation, or pathogen. The value of LD50 for a substance is the dose required to kill half the members of a tested population after a specified test duration.

## About This Web Application
This web application has been developed using R language and Shiny framework to calculate the Lethal Dose/Concentration by fit a Generalized Linear Model (GLM) assuming error distribution is binomial; it can apply three different link functions for probability transformation: Probit, Logit, and Complementary log-log.

## Analysis Input
User can upload an Excel spreadsheet file includes data of ([Sample Data File](https://icarda.shinyapps.io/ld50/_w_77a55d07/data.xlsx)):
- Number of subjects (e.g. total number of insects).
- Responded number (e.g. insects got killed).
- Explanatory variate (e.g. dose).

Analysis parameters includes:
- Effective (or lethal) dose/concentration (default is 50% and accepted value is in the range [0 – 100]).
- Level of confidence interval (default is 80% and accepted value is in the range [0 – 100]).
- Link function (default is “Probit” and available options includes also “Logit” and “Complementary log-log”).
- Take logs of explanatory (default is “None” and available transformations includes log base 10 and log base e).

## Analysis Output
Estimate of the Lethal Dose/Concentration, standard errors, fitted model parameters, and summary statistics. Further output can be obtained from the “Graphics Output” tab, which draws a graph of the fitted model showing the relationship of the response with the explanatory variable, including the confidence interval and LD cutting line for selected effective dose/concentration.

> If there is a grouping factor (e.g. different types of pesticides), the analysis will be performed for each grouping level separately.

## Online Demo
You can demonstrate an online demo of this application available here: [https://icarda.shinyapps.io/ld50/](https://icarda.shinyapps.io/ld50/)

## Related Book Chapter
More information can be found in the following book chapter:
> Singh, M., Al-Khatri, S., El-Shamaa, K., and Niane, A.A. 2018. **Statistical Design and Analysis of Date Palm Insect Pest Management Experiments**. *In M. El Bouhssini and J.R. Faleiro (Editors), Date Palm Pests and Diseases Integrated Management Guide (pp. 20-39). ISBN 978-9291275052. Abu Dhabi*. [[link](http://repo.mel.cgiar.org/handle/20.500.11766/8914)]
