
## About the tool

`MA-cont: pre/post effect size` v0.1 is a complement of [Meta‐analysis of continuous outcomes: Using pseudo IPD created from aggregate data to adjust for baseline imbalance and assess treatment‐by‐baseline modification, Res Synth Methods July 2020](https://doi.org/10.1002/jrsm.1434)

It enables the user to fit routinely used AD approaches and the novel pseudo IPD ANCOVA approach to meta-analyse continuous outcomes measured at baseline and follow-up.
More technical details on the model formulation 

#### Standard Aggegate Data (AD) approaches 
- Final scores analysis 

The simplest approach is to perform a meta-analysis using the follow-up (final) scores. Per trial, an estimate of the treatment effect, is obtained by calculating the mean difference of the follow-up scores:

$$ \\hat{\\theta}^{(F)}_i = \\bar{Y}_{FTi} - \\bar{Y}_{FCi} $$

- Change scores analysis 

The treatment effect in trial, can be estimated as the difference in change scores between the two groups: 

$$ \hat{\theta}^{(CS)}_i = {CS}_{Ti}-{CS}_{Ci}= (\bar{Y}_{FTi} - \bar{Y}_{FCi}) - (\bar{Y}_{BTi} - \bar{Y}_{BCi}) $$








