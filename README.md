# Scottish Bowel Screening Programme Statistics
This repository contains the [Reproducible Analytical Pipeline (RAP)](https://www.isdscotland.org/About-ISD/Methodologies/_docs/Reproducible_Analytical_Pipelines_paper_v1.4.pdf) of the bi-annual [Scottish Bowel Screening Programme Statistics publication](https://www.isdscotland.org/Health-Topics/Cancer/Bowel-Screening/).

## Resources
- [A Quick Guide to Git & GitHub](https://nhs-nss-transforming-publications.github.io/git-guide/index.html)

## Folder Structure
A new folder should be created in CancerGroup1 for each iteration of this publication. Inside that folder should be a sub-folder named _TPP_ or similar. This folder should contain:

- A _Master_ folder
- A folder named after each analyst who has worked on the publication e.g. a folder called _Tom_

### The _Master_ folder
The _Master_ folder is the _master copy_ of the publication repository. This is the production-ready version that is used for the publication process each quarter. Once cloned, two folders should be created inside the _Master_ folder; one named _Output_ and one named _Temp_. Aside from creating those folders, the master copy should **never be edited** and should only be updated from approved changes pulled from GitHub.

### The individual analyst folders
These folders also contain up-to-date copies of the repository and these are the versions to which edits are made. Analysts should only work in their own folders on their own development branches. Once cloned, the _Output_ and _Temp_ sub-folders should be created as they were in the _Master_ folder. Once an analyst is content that their changes are ready for the master branch, they must create a pull request on GitHub and have the other analyst(s) from the team review their changes and, if satisfied, merge them back into the master branch. **It is then that the master folder is updated**.

## The repository

### Files and Folders
A number of files and folders will be created or need to be created every time this repository is cloned. They are explained below.

#### Folders

- **.git**: This is the folder containing the version control history of the repository. It can be safely ignored.
- **.Rproj.user**: Where project-specific temporary files are saved. This can be safely ignored.
- **code**: Where the R scripts to do the analysis for the publication and create the Excel files are saved. 
- **RMarkdown**: Where the assorted RMarkdown-related files are saved.
- **Temp**: Where intermediate datasets are saved. This folder is not tracked by git and so it is safe to have data stored here, although it will need to be created manually after cloning the repository.
- **Output**: Where output files are saved. This folder is not tracked by git and so it is safe to have output files stored here, although it will need to be created manually after cloning the repository.

#### Files

- **.gitignore**: Any files and folders that should not be tracked by git should be added to this file.
- **SBSS.Rproj**: The project file for this repository. Wherever this file is saved is where RStudio automatically sets the working directory.
- **README.md**: This document.

## Running the publication

### Updating the code
To update the publication each quarter, the analyst responsible for updating the scripts/running the publication should complete the following steps:

- Pull the most recent version of the master branch into their own folder
- Create a fresh branch to make necessary changes
- Update the dates in the `0_housekeeping.R` file
- Ensure the filepaths in the `0_housekeeping.R` and `1_analysis_dataset.R` are correct.
- Push new branch to GitHub
- Create pull request for other analyst(s) to review changes
- Once changes have been approved, merge the branch into the master branch and delete the branch
- If no more changes are required, pull the updated master branch into the copy of the repository in the _Master_ folder

### Running the code

- In the _Master_ folder, run scripts 1:7
- Check for any errors in the code and investigate if necessary
- Check the files created in the _Temp_ sub-folder look as they should along the way
- Check the contents of the _Output_ sub-folder look as they should

Once this is done, the raw data files and Excel tables for the publication have been produced. The final step is knitting the RMarkdown documents:

- In the _RMarkdown_ sub-folder inside the _Master_ folder, open both `.Rmd` scripts and click _Knit_
- Check output
- A couple of manual steps are required to finish off the markdown documents (adding cover page, table of contents and formatting tables correctly). They are outlined in the README of the [National Statistics Publication Templates repository](https://github.com/NHS-NSS-transforming-publications/National-Stats-Template).
