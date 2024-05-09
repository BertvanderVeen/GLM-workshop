# Physalia Generalised Linear Model workshop by Bert van der Veen
This repository includes material for the Physalia workshop on Generalized linear models, 6-10 May 2024. Feel free to share, alter, or re-use this material with appropriate referencing of this repository.

Workshop webpage: https://www.physalia-courses.org/courses-workshops/glm-in-r

## Generalized Linear Models
Generalized Linear Models (GLMs) are a class of statistical models that were unified by Nelder and Wedderburn (1972). The models existed before then, but were fitted differently, and were applied independently when appropriate. The unification of these models into a class made them easier to teach, and the fitting algorithm that was developed allowed for faster and more robust parameter estimation.

The models could be unified because they have many things in common, despite being applicable to different types of data. Many more complex statistical models can be seen as an extension, or as having a relationship, with GLMs. As such, GLMs provide a strong basis for more complex statistical modeling.

This workshop teaches GLMs by first considering linear models as a special type of GLM where things function a little bit smoother, and the maths works out a little nicer. I will assume all workshop participants to be familiar with the R statistical programming language.

## Updating R
Please make sure to update your R installation prior to the workshop. Most of the code used in the workshop should function on older versions of R as well, but not all R packages used might be available or function fully.

[You can find an R installation based on your operating system here](https://cran.r-project.org/bin/windows/base/)

## PROGRAM
Sessions from 14:00 to 20:00 (Monday to Thursday), 14:00 to 18:00 on Friday (Berlin time). Sessions will consist of a mix of lectures, in-class discussion, and practical exercises / case studies over Slack and Zoom.

### Monday
On the first day we will go through R basics and background theory of frequentist statistics to build a foundation for the rest of the workshop.
* Brief reminder of R programming
* Reminder of foundational statistical concepts
* Simple linear regression

[First presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/1Monday/Introduction.pdf)

[Second presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/1Monday/SamplingMLE.pdf)

[Third presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/1Monday/introLM.pdf)

## Tuesday
* Multiple linear regression
* Model validation
* Introduction to GLMs
* Visualizing outputs

[First presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/2Tuesday/MultipleRegression.pdf)

[Second presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/2Tuesday/ModelValidation.pdf)

[Third presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/2Tuesday/GLMs.pdf)

## Wednesday
* Models for binary data: binomial regression
* Model selection
* P-values recap
* $R^2$ measures of variation

[First presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/3Wednesday/BinomialGLMs.pdf)

[Second presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/3Wednesday/ModelComparison.pdf)

[Third presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/3Wednesday/Pvalues.pdf)

[Fourth presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/3Wednesday/R2.pdf)

## Thursday
* Models for count data: Poisson, NB
* Residual diagnostics in GLMs
* Other useful models
  * Models for positive continuous data: Gamma, log-normal, and inverse gaussian regression
  * Multinomial and ordinal regression
  * Beta regression
  * Tweedie regression
  * Zero-inflated regression

[First presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/4Thursday/CountGLMs.pdf)

[Second presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/4Thursday/ModelValidationGLMs.pdf)

[Third presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/4Thursday/OtherModels.pdf)

## Friday
* What lies ahead (GAMs, GLMMs, etc.)
* Participants' case studies (bring your own data)

# Detailed schedule
|   Day   |Time         |Subject                                                |
|---------|-------------|-------------------------------------------------------|
|Monday   |14:00 - 15:00| Introduction, getting started                         |
|         |15:00 - 15:45| Recap R (coding)                                      |
|         |15:45 - 16:00| Break                                                 |
|         |16:00 - 16:45| Sampling theory and Maximum likelihood estimation     |
|         |16:45 - 17:45| Practical 1: Simulation and variance                  |
|         |17:45 - 18:30| Break                                                 |
|         |18:30 - 19:15| Introduction to Linear models                         |
|         |19:15 - 20:00| Practical 2: Simple linear regression                 |
|---------|-------------|-------------------------------------------------------|
|Tuesday  |14:00 - 14:45| Multiple linear regression                            |
|         |14:45 - 16:00| Practical 3: multiple linear regression               |
|         |16:00 - 16:15| Break                                                 |
|         |16:15 - 17:00| Model validation                                      |
|         |17:00 - 17:45| Practical 4: Checking fitted models assumptions       |
|         |17:45 - 18:30| Break                                                 |
|         |18:30 - 19:15| Introduction to GLMs                                  |
|         |19:15 - 20:00| Practical 5: visualizing model results                |
|---------|-------------|-------------------------------------------------------|
|Wednesday|14:00 - 14:45| Binomial regression                                   |
|         |14:45 - 15:30| Practical 6: Binomial GLM                             |
|         |15:30 - 16:15| Model comparison                                      |
|         |16:15 - 16:30| Break                                                 |
|         |16:30 - 17:15| Practical 7: Model comparison: exploratory            |  
|         |17:15 -      | P-values                                              |
|         |      - 17:45| $R^2$                                                 |
|         |17:45 - 18:30| Break                                                 |
|         |18:30 - 19:15| Practical 8: Model comparison: confirmatory           |
|         |19:15 - 20:00| Q & A / Practical 9: Repeat practical 5 with GLM      |
|---------|-------------|-------------------------------------------------------|
|Thursday |14:00 - 14:45| Models for count data                                 |
|         |15:00 - 16:00| Practical 10: Poisson and NB regression               |
|         |16:00 - 16:15| Break                                                 |
|         |16:15 - 17:00| Residuals diagnostics in GLMs                         |
|         |17:00 - 17:45| Practical 11: Residuals diagnostics in GLMs           |
|         |17:45 - 18:30| Break                                                 |
|         |18:30 - 19:15| Other useful models                                   |
|         |19:15 - 20:00| Practical 12: other useful models                     |
|---------|-------------|-------------------------------------------------------|
|Friday   |14:00 - 15:00| What lies ahead (GLMMs, GAMs, GLLVMs, Bayesian stats) |
|         |15:00 - 18:00| Bring/present your own data (in group)                |

