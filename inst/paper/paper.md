---
title: 'idmodelr: an R package containing a library of infectious disease models and utilities'
tags:
  - R
  - rstats
  - Visualisation
  - Infectious Disease
  - Modelling
authors:
  - name: Sam Abbott
orcid: 0000-0001-8057-8037
affiliation: "1"
affiliations:
  - name:  'Bristol Medical School: Population Health Sciences, University of Bristol, Bristol, UK'
index: 1
date: 23 July 2019
bibliography: paper.bib
---


# Summary

Infectious disease models are being increasingly used by the public health research community but require a substantially different knowledge base to other commonly used tools. Whilst many resources exist for teaching theory the ``R`` infectious disease modelling landscape is fragmented. Providing a consistent framework, along with key utilities, reduces the barriers to entry. This allows non-specialists to explore infectious disease models more easily and allows specialists to focus on the dynamic that they are explaining rather than the tooling required to get there.
 
``idmodelr`` is an R package [@RCoreTeam2019] that facilitates the exploration of a range of infectious disease models in a consistent framework. The package code is archived on Zenodo [@Abbott:2019] and [Github](https://www.samabbott.co.uk/idmodelr/). The primary aim of ``idmodelr`` is to provide a library of infectious disease models for researchers, students, and other interested individuals. These models can be used to understand the underlying dynamics and as a reference point when developing models for research. The secondary aim of ``idmodelr`` is to make experimenting with infectious disease models in ``R`` easier. It does this by providing models that have been developed in a consistent framework along with tools to facilitate a range of modelling tasks.

 Unlike other modelling packages such as ``pomp`` [@pomp], ``libbi`` [@libbi], and ``EpiModel`` [@epimodel], `idmodelr` serves primarily as an exploratory tool. It is most comparable to ``epirecipes`` [@epirecipes] but provides a more consistent framework, an ``R`` based workflow, and additional utility tooling. After users have explored model dynamics with `idmodelr` they may then implement their model using one of these packages in order to utilise the model fitting tools they provide or develop their own. 
 
  The ``idmodelr`` package allows a large range of infectious disease models to be simulated, summarised and plotted without exhaustive domain, or ``R``, knowledge. It also contains functions that can help more advanced users with munging complex mutli-dimensional models, streamlining scenario analysis, looking up parameter definitions, and developing new models. It is hoped that future releases will build on the current library of models, introduce new model families, and expand on the available tooling. A dashboard has been developed that makes use of some of the package functionality to allow users to explore a range of infectious disease models in an interactive interface.[@exploreidmodels] See https://www.samabbott.co.uk/idmodelr/ for documentation.

 
# Acknowledgements

SA is funded by the National Institute for Health Research Health Protection Research Unit (NIHR HPRU) in Evaluation of Interventions at University of Bristol in partnership with Public Health England (PHE). The views expressed are those of the author and not necessarily those of the NHS, the NIHR, the Department of Health or Public Health England.


# References
