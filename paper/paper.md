---
title: 'sdstudio: A Companion Package for Designing and Managing surveydown Surveys'
tags:
  - R
  - shiny
  - surveydown
  - survey research
  - graphical user interface
  - data collection
authors:
  - name: Pingfan Hu
    orcid: 0009-0001-4877-4844
    affiliation: 1
    corresponding: true
  - name: Bogdan Bunea
    orcid: 0009-0006-2942-0588
    affiliation: 1
  - name: John Paul Helveston
    orcid: 0000-0002-2657-9191
    affiliation: 1
affiliations:
 - name: Department of Engineering Management and Systems Engineering, George Washington University, Washington, District of Columbia, United States of America
   index: 1
date: "28 September 2025"
bibliography: paper.bib
---

# Summary

Survey research is fundamental to social sciences, market research, and data collection across numerous disciplines. While `surveydown` as a code-based survey platform [@surveydown2025] offers powerful capabilities, researchers often benefit from complementary tools that enhance workflow efficiency. `sdstudio` is a companion R package for the `surveydown` platform, offering multiple interaction modes to suit different preferences and use cases. Built using the Shiny framework [@shiny2021], `sdstudio` features three main components: a visual survey builder with real-time code synchronization (the "Build" tab), an interactive preview system supporting both desktop and mobile views (the "Preview" tab), and a data management interface for response collection and analysis (the "Responses" tab). A comprehensive tutorial and demonstration is available [@sdstudio_tutorial2025].

# Statement of need

The `surveydown` platform has established itself as a powerful framework for creating reproducible, code-based surveys [@surveydown2025]. However, not all researchers are comfortable with coding. Some researchers prefer visual drag-and-drop interfaces for rapid prototyping and rapid feedback during survey development. Three key areas present opportunities for improved efficiency: (1) visual survey construction for users who prefer graphical interfaces, (2) immediate live preview capabilities during development, and (3) convenient database access for response management.

`sdstudio` addresses these diverse needs by providing a comprehensive companion interface for the `surveydown` survey platform [@surveydown2025]. The package offers three specialized capabilities through dedicated tabs: Build, Preview, and Responses.

The Build tab presents a graphical interface for survey construction, which supports toggle controls, and drag-and-drop behaviors. There are clear buttons for managing pages and contents. The Preview tab enables immediate survey testing and has both desktop and mobile view modes. This combination ensures a GUI experience for both survey creation and survey testing, improving the user experience of `surveydown` for survey researchers in general. Lastly, the Responses tab streamlines database connection and data management. These capabilities serve researchers regardless of their preference for GUI or code-based development, providing unified interfaces for survey design, testing, and data analysis [@tourangeau2017adaptive].

# Implementation

`sdstudio` is built using the R Shiny framework and can be installed directly from GitHub:

```r
pak::pak("surveydown-dev/sdstudio", ask = FALSE)
```

To launch the application, simply call:

```r
sdstudio::launch()
```

When launched, a local Shiny application will open in a new browser window. The workflow starts with a template system, which uses existing templates available from the `surveydown-dev` GitHub organization [@surveydowndev]. The Build tab interface features a dual-pane design (\autoref{fig:interface}): the left "Structure" panel provides hierarchical page and content management with drag-and-drop functionality, while the right "Code" panel displays the automatically generated `surveydown` markup using the ACE code editor. During the survey design process, the survey is rendered on the backend in real time. The Preview tab uses an iframe to display the rendered survey, with a loading spinner providing visual feedback while rendering is in progress. The Responses tab provides database integration that supports both local CSV files and online databases.

This three-tab interface works seamlessly while generating the desired survey files, enabling smooth workflow transitions among design, preview, and data management. Security features include `.env` for database credentials and automatic `.gitignore` generation to prevent exposure of sensitive files.

![The dual-pane interface of the Build tab showing the Structure panel (left) for graphical user experience and the Code panel (right) displaying the automatically generated `survey.qmd` script.\label{fig:interface}](sdstudio.png)

# Comparison with Existing Tools

Unlike proprietary platforms such as Qualtrics or SurveyMonkey, `sdstudio` maintains full data ownership thanks to the capability of `surveydown`. Compared to other open-source solutions like LimeSurvey [@limesurvey2012] or formr [@arslan2019formr], `sdstudio` uniquely combines visual design capabilities with programmatic reproducibility through its tight integration with the `surveydown` ecosystem. The real-time code synchronization feature distinguishes `sdstudio` from traditional form builders, ensuring that visual changes are immediately reflected in version-controllable markdown files while preserving the full analytical capabilities of the R ecosystem.

# Usage and Target Applications

`sdstudio` serves diverse research contexts from academic social science studies requiring complex experimental designs to market research projects needing rapid prototype iteration. The dual-pane interface accommodates both experienced R users who prefer code visibility and newcomers who benefit from visual feedback during survey development. Educational institutions can use the package for teaching survey methodology, allowing students to learn both visual design principles and underlying code structure simultaneously. Research teams can leverage the collaborative features for multi-investigator projects, where team members with different technical backgrounds can contribute effectively. The database management capabilities particularly benefit longitudinal studies and large-scale data collection efforts where response monitoring and real-time data access are essential for research operations.

# Acknowledgements

We acknowledge the survey research community for valuable feedback and testing during development. Special thanks to early adopters who provided insights into workflow requirements and user interface design. This work builds upon the foundation provided by the R and Shiny ecosystems, particularly the contributions of the RStudio team to reactive web application development. We also recognize the open-source survey research community, whose prior work on platforms like LimeSurvey and formr informed our understanding of researcher needs and technical requirements.

# References