######################
REPLICATION  MATERIALS: READ ME FILE
Article: “How the Party Commands the Gun: The Foreign-Domestic Threat Dilemma in China”
Author: Daniel Mattingly 
This Version: July 19, 2022
######################

####################
#The R script files were created with R version 4.0.3 running on Mac OS Big Sur (11.6)
#It was tested on four systems: on Mac OS Big Sur, on GNU Bash 3.2.57(1), on MATE Desktop Environment 1.1.16, and on Windows 10 Enterprise for Virtual Desktops
####################


To replicate the Tables and Figures in the article, run the file run_me.R. You can do so either in R, an IDE like RStudio, or using the Unix command line (Terminal on a Mac). Before running the program, please read the preamble to run_me.R. You will need to either set your working directory manually or, if you are running R from the Unix command line, you should delete the line of code that sets the working directory.

Please note the following:

-Since the data files contain text in Chinese using UTF-8 encoding. It is possible to encounter errors if R Studio or your computer environment is not configured to read files encoded in Chinese.

- If you have downloaded files from the Harvard Dataverse they should be in the original .csv format for the code to work properly. 

- LaTeX is required to view the tables output by the stargazer package.



##########
CONTENTS
##########

HELP FILES

1. README.txt: You are here!
2. Codebook.txt: Description of the variables.

R SCRIPTS

1. run_me.R: Loads and installs packages, loads data, and calls the individual analysis files.
2. variable_creation.R: Creates the variables used in the analysis from the raw data.
3. main_tables.R: Creates the tables and figures used in the main text.
4. appendix_tables_figures.R: Creates the appendix tables and figures.

DATA FILES

5. bio_data.csv: Data file of the individual-level characteristics for each PLA officer.
6. career_data.csv: Data on positions held by each PLA officer. Each observation is an officer-position.
7. key_positions_data.csv: Data on key positions held by each PLA officer. This version contains only the key positions in a regularized format, whereas the career_data.csv file has all prior positions and some lower-level positions are not regularized.