# Physalia Generalised Linear Model workshop by Bert van der Veen
This repository includes material for the Physalia workshop on Generalized linear models, 24-28 February 2025. Feel free to share, alter, or re-use this material with appropriate referencing of this repository.

Workshop webpage: https://www.physalia-courses.org/courses-workshops/glm-in-r-1

## Generalized Linear Models
Generalized Linear Models (GLMs) are a class of statistical models that were unified by Nelder and Wedderburn (1972). The models existed before then, but were fitted differently, and were applied independently when appropriate. The unification of these models into a class made them easier to teach, and the fitting algorithm that was developed allowed for faster and more robust parameter estimation.

The models could be unified because they have many things in common, despite being applicable to different types of data. Many more complex statistical models can be seen as an extension, or as having a relationship, with GLMs. As such, GLMs provide a strong basis for more complex statistical modeling.

This workshop teaches GLMs by first considering linear models as a special type of GLM where things function a little bit smoother, and the maths works out a little nicer. I will assume all workshop participants to be familiar with the R statistical programming language.

## Updating R
Please make sure to update your R installation prior to the workshop. Most of the code used in the workshop should function on older versions of R as well, but not all R packages used might be available or function fully.

[You can find an R installation based on your operating system here](https://cran.r-project.org/bin)

## PROGRAM
Sessions from 14:00 to 20:00 (Monday to Thursday), 14:00 to 18:00 on Friday (Berlin time). Sessions will consist of a mix of lectures, in-class discussion, and practical exercises / case studies over Slack and Zoom.

### Monday
On the first day we will go through R basics and background theory of frequentist statistics to build a foundation for the rest of the workshop.
* Brief reminder of R programming
* Reminder of foundational statistical concepts
* Simple linear regression

[First presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/1Monday/Introduction.pdf)

[Second presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/1Monday/SamplingMLE.pdf)

[Third presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/1Monday/IntroLM.pdf)

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
* Beyond GLMs: Generalised linear mixed models
* Participants' case studies (bring your own data)
* Discussion/help/..

[Presentation here](https://github.com/BertvanderVeen/GLM-workshop/blob/main/5Friday/BeyondGLMs.pdf)

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
|         |14:45 - 16:00| Practical 3: multiple linear regression               | <!-- do a continuous-categorical interaction to add to the slides, maybe with the body temperature data. Maybe also centering. -->
|         |16:00 - 16:15| Break                                                 |
|         |16:15 - 17:00| Model validation                                      |
|         |17:00 - 17:45| Practical 4: Checking fitted models assumptions       |
|         |17:45 - 18:30| Break                                                 |
|         |18:30 - 19:15| Introduction to Generalised Linear Models             |
|         |19:15 - 20:00| Practical 5: visualizing model prediction             | <!-- marginal effects? -->
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
|Thursday |14:00 - 14:45| Models for unbounded count data                       |
|         |15:00 - 16:00| Practical 10: log-linear regression                   | <!--get people to interpret/discuss the results with CIs, try a genomics  dataset for the exercise -->
|         |16:00 - 16:15| Break                                                 |
|         |16:15 - 17:00| Residuals diagnostics in GLMs                         |
|         |17:00 - 17:45| Practical 11: Residuals diagnostics in GLMs           |
|         |17:45 - 18:30| Break                                                 |
|         |18:30 - 19:15| Other useful models                                   |
|         |19:15 - 20:00| Practical 12: other useful models                     |
|---------|-------------|-------------------------------------------------------|
|Friday   |14:00 - 16:00| Generalised Linear Mixed-effects Models               |
|         |16:00 - 18:00| Practical 13: Real data exercise                      | <!-- first a demonstration, then discussion and correction after -->

<!-- from survey
more real data less simulations
examples beyond ecology
hypothesis testing (examples?)
fewer equations
one-or-two sessions with real data after learned how to model
own practice after seeing a demonstration and discussion/correction after that
-->

<!--
separate r packages in "we will use' and "might be useful"?
add a table of example data for each presentation, after the "data" slide
then find examples beyond ecology
think about the hypothesis testing
Generally, I should swap to short exercises first + discussion, and a longer exercise after that?
use marginal effects package throughout exercises? or maybe for "predictionn" part
make slides with many equations incremental, such a s the re-arranging slides to glm in binomial.
beyond glms should repeat poisson bits, and maybe a poisson example
consider adding equation and equation in words for the examples?!
-->
