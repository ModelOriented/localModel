---
title: 'localModel: an R package for local explanations of machine learning models'
tags:
  - R
  - machine learning
  - interpretability
authors:
  - name: Mateusz Staniak
    affiliation: 1
  - name: Przemysław Biecek
    orcid: 0000-0000-0000-0000
    affiliation: 1
affiliations:
 - name: Faculty of Mathematics and Information Science, Warsaw University of Technology
   index: 1
date: 14.12.2018
bibliography: paper.bib
---

# Summary

In [@ribeiro_why_2016], authors proposed a method called _LIME_ of explaining predictions of any classifier. 
This paper became an inspiration for many research in the field of Interpretable Machine Learning, which aims to make complex _black box_ models comprehensible to humans in order to increase their reliability and trustworthiness.

TODO: diagram podsumowujacy LIME

In [@laugel_defining_2018], it was noted that _LIME_ explanations are often dominated by global behavior of the model, despite their locality.
Author proposed a modified method of sampling for local exploration based on _localSurrogate_ algorithm.
In this paper, we introduce an R package that provides explanations based on the idea of LIME, but using features extracted from Ceteris Paribus profiles ([@ceteris]), that describe the relationship between a single predictor and model response on an individual (observation) level.


# Overview of the methodolody

The main contribution of the `localModel` package is a novel approach to creating simplified features, in terms of which the explanation is given.

For numerical predictors, Ceteris Paribus profiles are calculated for each predictor and a decision tree with maximum depth equal to 2 is fitted to captured the marginal relationship between model response and a single predictor.
Based on this decision tree, the predictor is dichotomized.

For categorical predictors, model predictions for the original dataset are regressed on each predictor (marginally) using a decision tree with a single split. This way, predictors are dichotomized.

The interval into which the actual value of the feature falls (or the group of factor labels), is treated as an interpretable feature, and the 
This mimics the original _LIME_ approach to image data, where superpixels (interpretable features) were grayed out (_turned off_) to create new observation.
Here, switching the value of a predictor to _baseline_ is th analogue of _graying out_.
This way, the effect of feature having a value in a specific interval is compared to the situation, when this feature is outside that interval.
For example, in the `apartments` data modelling, the effect of an apartment being on a middle floor can be compared to high and low floors or high floor to lower floors.

After these simplified inputs are created, a new dataset is simulated.
Each copy of the original observation is perturbed in the following way:

  1. A number of features _k_ to be perturbed is sampled uniformly.
  2. Then, _k_ values are changed to _baseline_ with probability equal to 1 (parameter `sampling` set to _uniform_) or with probability equal to the proportion of _baseline_ labels in the original data (parameter `sampling` set to _non-uniform_).
  
LASSO regression model is fitted to model prediction calculated for the new dataset. Since all new inputs are binary, a transformation into the original feature space must be applied before computing predictions.
This is done by choosing random value from the set corresponding either to _baseline_ or the interpretable input for each feature.
LASSO penalty is chosen via 5-fold cross-validation.

All these calculation are performed by `individual_surrogate_model` function, which takes `DALEX` explainer, a single observation (as a data frame) and the size of simulated as a parameters.
Additionally, `seed` parameter can be set to ensure reproducibility and method of sampling new observation can be altered via `sampling` parameter (as described above).


The drawback of this method is that comparing explanations across different observations is problematic, because each observation has a different set of interpretable features. 
To deal with this problem, another type of plot was proposed.

*TODO*: zaimplementowac i opisać wykres do porownan.


# Example

TODO: jak w winietce.


# Acknowledgements


This work was financially supported by NCN grant ...

# References


