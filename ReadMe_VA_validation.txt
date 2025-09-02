Path to this file in VINCI: P:/ORD_Curtius_202210036D/shiny_app_rootDir/ReadMe.txt

To start colonoscopy and pathology report (UC-CaRE) validation:
Simply launch Rstudio and run (in R console):
shiny::runApp("P:/ORD_Curtius_202210036D/shiny_app_rootDir/shiny_app/reviewScripts_UseThese/colo_path_report_app_<Name>.R")

Brian:
shiny::runApp("P:/ORD_Curtius_202210036D/shiny_app_rootDir/shiny_app/reviewScripts_UseThese/colo_path_report_app_Brian.R")
Anna:
shiny::runApp("P:/ORD_Curtius_202210036D/shiny_app_rootDir/shiny_app/reviewScripts_UseThese/colo_path_report_app_Anna.R")
Hyrum:
shiny::runApp("P:/ORD_Curtius_202210036D/shiny_app_rootDir/shiny_app/reviewScripts_UseThese/colo_path_report_app_Hyrum.R")
Kit:
shiny::runApp("P:/ORD_Curtius_202210036D/shiny_app_rootDir/shiny_app/reviewScripts_UseThese/colo_path_report_app_Kit.R")

If we have others who will be reviewing, just make a copy of any of the scripts
above and change the names in the filename and at the top of the script itself.

Upon launching the shiny app, you will see a pathology report and colonoscopy 
report side-by-side. Simply fill in the annotations on the right for each set 
of reports and click next. Multiple notes may be included for the colonoscopy reports (right 
side). Distinct notes will have a clear break between them given by "<<<<<<<< >>>>>>>>>"

Your validations (and progress) will be automatically saved in csvs in:
"P:/ORD_Curtius_202210036D/shiny_app_rootDir/shiny_app_output/colo_path/"
under a folder with your name. There are 1,000 IBD path reports with matching
colonoscopy reports ready for validation. We probably won't need to validate 
all of these. Once you validate 5 reports, the next time you launch it will say
there are 995 reports to be validated. When you log off or get disconnected 
it shouldn't affect your progress, just launch as above and you'll pick up
where you left off.

The app allows you to characterize up to 5 distinct lesions. You may also
characterize a few lesions together if the colo/path reports group them. For 
example, "transverse colon polyps x3: tubular adenomas" could be characterized 
once and you can select that 3 lesions have those characteristics. Use the 
largest size given for this group.
