# ABCD-policy-project
This repository is home to the analyses in Hughes et al (under review). Pre-print posted here: https://www.medrxiv.org/content/10.64898/2025.12.19.25342709v1

# Note 02-17-2026
TBD on accessing the MAP data. Since it is not publicly available, I need to check to see what we're allowed to post here. Stay tuned..

# Description
Everything you need to reproduce the results (and figures and tables) should be included in the package/ directory. You'll first need access to the ABCD data (https://www.nbdc-datahub.org/). Once downloaded, take note of where the data live and then pass that directory name into the scripts/masterCreation.rmd file. I'm realizing right now that since we use release 5.1 for the analyses in this project, it might take some finessing to make the script compatible with the default file structure output from releases >= 6.0. 

If you have access to the release 5.1 in its default file structure, that's your best bet. Everything should (?) work fresh out of the box. It would probably be available here: https://nda.nih.gov/. I'll paste the file paths for the 5.1 directories/file paths below, so you can try to organize it into the format if you can't get access to the 5.1 data. Sorry.. if you run into any trouble, you can email me at hughesdy@ucla.edu. 

### Demographics
"abcd-general/abcd_p_demo.csv"
"abcd-general/abcd_y_lt.csv"

### Puberty
"physical-health/ph_p_pds.csv"
"physical-health/ph_y_pds.csv"

### Bullying (PEQ)
"mental-health/mh_y_peq.csv"

### Psychotic-like experiences (PQ-BC)
"mental-health/mh_y_pps.csv"

### Broad mental health problems (BPM)
"mental-health/mh_y_bpm.csv"




