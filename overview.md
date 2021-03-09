A worked out data example, including missing summary statistics, is provided as a default data set. A data template is given under the [`Data upload and pre-processing`] tab and should be used to prepare your data accordingly, for example long format is required. 
- Use the upload button to upload your dataset and follow the steps to fill-in any missing data
- If your data set is full (no missing data), proceed to the [`View final data`] tab

The tool is designed to perform meta-analysis of continuous aggregate data (AD) measured at baseline (pre-treatment) and follow-up (post-treatment) [`Meta-Analysis`].

The available statistical approaches are:

- Standard AD: Follow-up scores, Changes scores, Recovered Ancova
- One-stage pseudo IPD ANCOVA
- Two-stage pseudo IPD ANCOVA

For details regarding on model formulations and statistical calculations look at `Technical details`.


### Page Structure & Typical Workflow

Each section is separated by tabs, for example, this one is a sub-tab of the `Home` tab.

A typical workflow starts by uploading a CSV file of the data in long format. Some values may be partially available from published sources, we provide simple back-calculations and sensible imputations by actiobuttons under `Data upload & pre-processing`. Once the final dataset is created, standard AD and pseudo IPD methods to synthesize the data are performed.

For more information on how to use the tool please follow the instructive video or get in contact with the developer of the tool. The app is powered by [Shiny](shiny.rstudio.com) using the statistical software [R](http://cran.r-project.org/).


