```yaml
---
title: "AID-PSQI README"
author: Drew Cooper
date: 06 May 2026
output:
  github_document:
    toc: true
    toc_level: 2
---
```
# **Temp-Todo's**
**sub-analysis_v2.R**:

- Add Holm-Bonferroni p-values to age and diabetes-duration analyses [w/ PSQI, HFS-II, A1c].
- Fully flesh out correlation table(s) for above comparisons
- Perform subgroup analyses on questionnaire-questionnaire correlations, again w/ age and diabetes-duration using model()
- ...and do this within the AID user subgroup
- explore results between age + diabetes-durations vars, and HFS !!!

# **AID-PSQI README**

## Introduction

This is a README document for the AID-PSQI data pipeline, developed by Drew Cooper and Tebbe Ubben for publication alongside the manuscript titled:

“Sleep-AID: a cross-sectional analysis of subjective sleep quality, psychosocial measures and real-world glycemic outcomes in people with diabetes using automated insulin delivery systems”.

## Main

There are a lot of files included here in this repository. The basic workflow is as follows:

- **main.R**: Initial processing of the OPEN dataset; uses many of the additional R files contained herein (not listed for brevity). See the script for details on usage and rationale.
- **Enrollment_Extraction.R** / **psqi_5j_analysis.R**: semantic coding for PSQI 5J (open-ended response question).
- **analysis_v2.R**: Pre-processing of the dataset; variable name and type cleaning, writing `study\_data.xlsx` for later analysis.
- **sub-analysis_v2.R**: Further pre-processing, statistical analyses, and generation of data frames, tables, and figures.

## Remaining Incongruences

There are some remaining build specifics that need to be cleaned up, although they do not impact the functionality of the script.

- *Enrollment_Extraction.R* and *psqi_5j_analysis.R* serve identical functions with different approaches; **these should be unified**.
- Both of these above files have a critical infrastructure element **to be integrated into main.R**.
- *revision.R* has been added recently for BMI calculations, and **should be further cleaned** and potentially integrated into other existing files when possible.
- *sub-analysis_v2.R* still has a big commented block [and some other blocks] related to sub-sample analyses. **This still has to be updated and/or removed** if it isn't necessary. *revision.R* could also theoretically be added here.

Also it should be noted [and should be clear] that this script does **not** ship with the data files required to use it. Please see below. 

## Wrap-up

If you would like to work with these data as outlined, please contact <drew.cooper@charite.de> regarding data access.

Thank you for your time in reading this document, and please feel free to reach out with any questions or feedback.
