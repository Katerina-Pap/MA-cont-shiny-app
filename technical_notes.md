
### About the tool

`MA-cont: pre/post effect size` is a complement of [Meta‐analysis of continuous outcomes: Using pseudo IPD created from aggregate data to adjust for baseline imbalance and assess treatment‐by‐baseline modification, Res Synth Methods July 2020](https://doi.org/10.1002/jrsm.1434) and 
[MA-cont:pre/post effect size: An interactive tool for themeta-analysis of continuous outcomes using R Shiny](https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1592)

It enables the user to fit routinely used AD approaches and the novel pseudo IPD ANCOVA approach to meta-analyse continuous outcomes measured at baseline and follow-up.
More technical details on the model formulations are provided in the references above. 
<br>

#### How to best use the tool?

We encourage the user to watch the video of instructions under the `Home` tab, demonstrating the functionalities of this app. 
Different methods of increasing complexity are applied and the output needs to be interpreted assuming a basic understanding of the models running in the backend. All code is freely available in [Github](https://github.com/Katerina-Pap/MA-cont-shiny-app).

<br>
#### Glossary of Terms

| Approach/Assumption  | Description |
| :--------------------------: | :----------- |
| `ANCOVA Recovered effect estimates` | A regression model called, where the final score is the dependent variable and the variables for the treatment groups and baseline scores enter as covariates. The coefficient for the treatment group estimates the **treatment effect**, that is, the difference between the randomised groups and it can be recovered by published aggregate data           |
|`Change scores analysis`|The treatment effect is estimated as the difference between groups in *mean* changes from baseline scores |
| | |
| `Final (follow-up) scores analysis`| The treatment effect is estimated as difference in *mean* final scores between the treatment group and control group. This approach ignores any baseline information|
| | |
| `Hartung-Knapp Adjustment` | Derives the SE and CI of the summary treatment effect while justifying percentiles from a t-distribution when sample sizes and/or number of included studies are small |
| | |
|Arm- and study-specific $\sigma^2_{ik}$| The within-trial variance varies by arm and study; the most flexible approach  |
|Study-specific $\sigma^2_{i}$| The within-trial variance varies by study but is assumed equal between treatment and control arms; to be used when variation of outcomes is expected to be same between the two arms|
|Arm-specific $\sigma^{2}_{k}$|The within-trial variance varies by arm but is assumed equal across all studies; to be used when outcomes are expected to vary between treatment and control arms, particularly when greater variability is expected in the active treatment group|
|One variance $\sigma^2$|The within-trial variance does not vary across arms or studies|
<br>
#### List of Packages Used
```{r}
library(dplyr)
library(DT)
library(markdown)
library(metafor)
library(nlme)
library(readxl)
library(reshape2)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(skimr)
```

 










