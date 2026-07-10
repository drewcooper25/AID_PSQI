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

# **AID-PSQI README**

## Introduction

This is a README document for the AID-PSQI data pipeline, developed by Drew Cooper and Tebbe Ubben for publication alongside the manuscript titled:

“Sleep-AID: a cross-sectional analysis of subjective sleep quality, psychosocial measures and real-world glycemic outcomes in people with diabetes using automated insulin delivery systems”.

**Note:** The core processing files require access to the OPEN-Project-dataset to generate the manuscript results; thess scripts do **not** ship with the data files required to use it. Please see below regarding database access.

## Main

There are a lot of files included here in this repository. The basic workflow is as follows:

- **config.R**: This handles all path processing—update the main path to your directory where AID_PSQI is stored `before running any analyses`. It is also very important [when you are working with the dataset itself] to ensure your OPEN-Project-dataset `is in the same parent folder` as AID_PSQI. Nothing will work correctly without this initial setup.
- **main.R**: Initial processing of the OPEN Project dataset; uses many linked source() .R files contained herein (not listed for brevity). This script generates `BgReadings.xlsx` as a final output.
- **psqi_5j_analysis.R**: semantic coding for PSQI 5J (open-ended response question), producing chi-squared analyses of these encodings.
- **analysis_v2.R**: Pre-processing of the dataset; variable name and type cleaning, writing `study\_data.xlsx` for later analysis.
- **sub-analysis_v2.R**: Further pre-processing, statistical analyses, and generation of data frames, tables, and figures.

In the most recent re-tooling of this repo, absolute file paths have been removed in favour of relative paths for both scripting .R files, and for calling OPEN dataset files.

R scripts—where applicable—push created .csv files to output/, and .tif files to figures/.

## Remaining Incongruences

There are some remaining build specifics that need to be cleaned up, although they do not impact the functionality of the script.

- *revision.R* has been added recently for BMI calculations, and **should be further cleaned** and potentially integrated into other existing files when possible. It could also theoretically be added to *sub-analysis_v2.R*.

## Data Access

If you would like to work with the OPEN Project dataset using these scripts, please contact <drew.cooper@charite.de> regarding data access. This repository ships without data [for obvious privacy reasons] but we are happy to work with researchers and citizen scientists to establish the proper data protection agreement(s).

## Wrap-up

Thank you for your time in reading this document, and please feel free to reach out with any questions or feedback.