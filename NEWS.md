# localModel 0.3.11

* Changes dependencies: `DALEX2` to `DALEX`, `ceterisParibus2` to `ingredients`.

# localModel 0.3.10

* Vignettes updated. In particular, seed is now set before using randomForest.
* Plot function now shows a message and returns and empty plot if all effects
  are equal to 0.
* dev.ratio from glmnet is now returned as one of the columns of surrogate_model_explainer.

# localModel 0.3.9

* Explanation models is now fitted to raw predictions, not difference between predictions and model mean.

# localModel 0.3.8

* Fixed bug related to one-colum model matrix in glmnet.

# localModel 0.3.7

* Bug fixed in example.

# localModel 0.3.6

* Improved examples.

# localModel 0.3.5

* Fixed issues with examples.
* Improved documentation.

# localModel 0.3.4

* Fixed a bug related to interpretable numerical inputs (missing as.numeric)
* Added tests for 100% coverage.

# localModel 0.3.3

* Set.seed before fitting cv.glmnet.
* New vignette describing methodology.

# localModel 0.3.2

* Weighting option added.
* Improved documentation.

# localModel 0.3.1

* Improvements to plots 

# localModel 0.3.0

* Multi-model plot option added.
* `stringr` dependency removed.

# localModel 0.2.9

* Same interpretable features are used for every response level.
* `rpart` dependency replaced with `partykit`.

# localModel 0.2.8

* Print method added.

# localModel 0.2.7

* Updates link in README.

# localModel 0.2.6

* Adds help for the package.

# localModel 0.2.5

* Fixes bug related to number of levels when sampling new observation.

# localModel 0.2.4

* Sets seed in tests.

# localModel 0.2.3

* Minor updates to documentation (including paper/paper.md) and unit tests.

# localModel 0.2.2

* Fixed bugs related to column names in case when response in is new_observation.
* Unit tests added.

# localModel 0.2.1

* Vignettes added.

# localModel 0.2.0

* Major changes: interpretable features are created using decision trees-based discretization. 
* Now-unnecessary dependencies dropped.
* Plots are now drawn around intercept.

# localModel 0.1.2

* Fixes bug in observation enconding.
* Improves and simplifies encoding.
* Improves plot.

# localModel 0.1.1

* Improved plot labels.

# localModel 0.1

* Added possibility of coding variables as (baseline / lower or higher) or ( baseline / lower / higher).
* Added option to skip loess smoothing.
* Removed option of setting `family` argument in glmnet.

# localModel 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
