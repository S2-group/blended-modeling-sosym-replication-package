# Academic literature review with snowballing
In this directory one can find all data collected during the academic literature review and the snowballing process. 

### Remark

Due to the use of long filenames, it could be the case, that Windows complains about too long path names. We recommend to save the replication package in a directory just below root to avoid these problems. 

### Content description

This directory contains the following data:
* `01-Google Scholar search results (stage 0).xlsx` - detailed list of all papers found in the initial search with the decisions made by the two reviewers and (where applicable) the arbiter. The ID in the first column is used for traceability to the file `/tools/identified_tools.xlsx`.
* `0N-Results snowballing - stage M.xlsx` (with N = 2..5, M = N-1) - detailed list of all papers that might be included for the next snowballing stage depending on the final decision. 
* `06-AllPapers.csv` - list of all reviewed papers without duplicates and the decisions of the two reviewers and (where applicable) the arbiter
* `07-agreement.R` - R script for calculation of percentage agreement and Cohen's Kappa based on the data in `AllPapers.csv`. The columns F - I contain the following information: 
  * "SuggestInclusion1" and "SuggestInclusion2" represent the decision by the two reviewers.
  * "derived decision" represents the combination of the two former columns according to the following table:

    |       | YES   | MAYBE | NO    |
    |-------|-------|-------|-------|
    | YES   | YES   | YES   | MAYBE |
    | MAYBE | YES   | MAYBE | NO    |
    | NO    | MAYBE | NO    | NO    |
  
  * "FinalDecision" shows the final decision made by the arbiter for those columns where the combination results in "MAYBE".
  * All columns include a specific number for each entry (1. NO, 2. MAYBE, 3. YES) to define a specific order of the values. This is necessary for a correct handling of the entries of the R-script (see `agreement.R`). 
  
* `08-Summary.xlsx`
  *  Contains all numbers of the search process
      * the amount of found papers via a search string (stage 0), citations or references (stages 1-4) 
      * the amount of included papers per stage
      * a interrater table of all (distinct) papers (see `agreement.R`)
      * the percentage agreement of 97%
      * Cohen's Kappa with equal weights and including MAYBEs of 0.453
      * Cohen's Kappa without MAYBEs of 0.864
  * In the sheets "stage 1" - "stage 4", one can find detailed information about the references and cites for each paper

* `./snowballing/stage N` (with N = 1..4)
   * these directories contain for each reviewed paper all considered references and citations with the applied inclusion and exclusion criteria each. 
   * KNOWN REFS or KNOWN CITES are hits that were found in the database and thus they were not reviewed again 