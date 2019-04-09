Included are files for the "Intro to Shiny" Workshop
2017 UNC Biology Symposium
Presented by: Chris Payne 
Date: May 3, 2017

"Think that Shiny will take too long to learn? Let me show you otherwise! 
Learn to use RStudio's interactive app creation package in under one hour! 
Interact with and share your data like never before!
Bring your laptop with R and R Studio installed."

###################

Files include:

  Readme.txt
   > Describes the files in this zip folder.
  
  IntroToShinyWorkshop-2017BioSymposium-payne.pptx
   > Introductory powerpoint for workshop.
  
  IntroToShinyWorkshop-payne2017.pdf
   > PDF of Shiny Exmaple Code Walkthrough
  
  Example Code (folder)
   > Folder containing code used throughout "IntroToShinyWorkshop-payne2017.pdf"
   > Can more easily use this code using `source()` in rStudio.
   > Includes all iterations of CO2 data examples:
  
        IntroToShinyWorkshop - CO2 Example 1.R
        IntroToShinyWorkshop - CO2 Example 2.R
        IntroToShinyWorkshop - CO2 Example 3.R
        IntroToShinyWorkshop - CO2 Example 4.R
        IntroToShinyWorkshop - CO2 Example 5.R
  
  Presentation files ("Under the Hood" files): 
    [You don't need these to learn Shiny, but feel free to look at my messy Rmd file code to help learn rmarkdown]
  
    GettingStarted.R
     > Downloads workshop files from GitHub (user theforestecologist) 
     > Runs runShinyWorkshop.R
     > Note: you don't need this file at all after already downloaded all files.
       > It's here for "housekeeping" sake. 
  
    runShinyWorkshop.R
     > Installs all necessary packages to run/view the Shiny Exmaple Code Walkthrough HTML page from the workshop
     > Runs/renders this workshop walkthrough HTML page (called IntroToShinyWorkshop.Rmd)
    
    IntroToShinyWorkshop.Rmd
     > R Markdown code
       > this is the code used to create the interactive r markdown HTML document (page). 
       > Essentially, this is the on-screen HTML version of IntroToShinyWorkshop-payne2017.pdf
  
    ShinyGeneralWorkflow.PNG
     > Image file needed by IntroToShinyWorkshop.Rmd to render image in walkthrough HTML page. 