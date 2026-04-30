wri_project_root <- Sys.getenv("WRI_PROJECT_ROOT", unset = "/home/shares/wwri-wildfire")

library(readxl)

# went to cat's link then back one then advanced search

test <- read_excel(file.path(wri_project_root, "data", "water-domain-data", "raw", "epa", "2024", "q4", "Water System Summary_20250311-9.xlsx"), skip = 4) # tried to repeat the Cat link
test2 <- read_excel(file.path(wri_project_root, "data", "water-domain-data", "raw", "epa", "2024", "q4", "Water System Summary_20250311-8.xlsx"), skip = 4) # used advanced search like all other time
test3 <- read_excel(file.path(wri_project_root, "data", "water-domain-data", "raw", "epa", "2024", "q4", "Water System Summary_20250227.xlsx"), skip = 4) # downloaded at link Cat sent
                                                               
colnames(test)
 # [1] "PWS ID"                     "PWS Name"                  
 # [3] "PWS Type"                   "Primary Source"            
 # [5] "Counties Served"            "Cities Served"             
 # [7] "Population<br>Served Count" "Number of Facilities"      
 # [9] "Number of Violations"       "Number of Site Visits"     
colnames(test2)
#  [1] "PWS ID"                      "PWS Name"                   
#  [3] "EPA Region"                  "Primacy Agency"             
#  [5] "PWS Type"                    "Population<br> Served Count"
#  [7] "Cities Served"               "Counties Served"            
#  [9] "# of Facilities"             "# of Violations"            
# [11] "# of Site Visits"           
colnames(test3)
 # [1] "PWS ID"                     "PWS Name"                  
 # [3] "PWS Type"                   "Primary Source"            
 # [5] "Counties Served"            "Cities Served"             
 # [7] "Population<br>Served Count" "Number of Facilities"      
 # [9] "Number of Violations"       "Number of Site Visits"