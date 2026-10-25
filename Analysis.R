library(tidyverse) # Managing Data
library(dplyr) # Managing Data
library(readxl) # For Excel
library(haven) # For SPSS Output
library(ltm) # For Reliability 




###########################
#        Study #1         #
###########################



###########################
#        Study #2         #
###########################

  ##############################
  # Data Cleaning/Organization #
  ##############################

# Import Data
  rawQualtrics <- read.csv("data/study2/qualtrics.csv")
  rawPhys <- read_xlsx("data/study2/physiological.xlsx")
  rawUnreal <- read.csv("data/study2/unrealoutput.csv")
  rawDebrief <- read_xlsx("data/study2/debrief.xlsx")

# Qualtrics
############
  # Remove all non-participants
    dataQualtrics <- rawQualtrics
    dataQualtrics$Participant <- as.numeric(dataQualtrics$Participant)
    dataQualtrics <- dataQualtrics |>
                      filter(!is.na(Participant))
  
  # Remove label rows
    dataQualtrics <- dataQualtrics[c(-1,-2),]
  
  # Compute composite scores
    # Empathy
      scaleEmpathy <- dataQualtrics |> 
                        dplyr::select(contains("Empathy"))
      scaleEmpathy <- scaleEmpathy |>
                        mutate(across(everything(), as.numeric))
      
      compositeEmpathy <- rowSums(scaleEmpathy)
      compositeEmpathy <- as.data.frame(compositeEmpathy)
      
    # Altruism
      scaleAltruism <- dataQualtrics |> 
        dplyr::select(contains("Altruism"))
      scaleAltruism <- scaleAltruism |>
        mutate(across(everything(), as.numeric))
      
      compositeAltrusim <- rowSums(scaleAltruism)
      compositeAltrusim <- as.data.frame(compositeAltrusim)
      
    # Courage
      scaleCourage <- dataQualtrics |> 
        dplyr::select(contains("Courage"))
      scaleCourage <- scaleCourage |>
        mutate(across(everything(), as.numeric))
      
      compositeCourage <- rowSums(scaleCourage)
      compositeCourage <- as.data.frame(compositeCourage)
  
  
  # Grab Important Columns
    dataQualtricsClean <- dataQualtrics |> 
                            dplyr::select(c("Participant", "VGUse", "VRExperience", "MotionSickness",
                                            "Gender", "Age", "Ethnicity"))
    dataQualtricsClean <- dataQualtricsClean |>
                            mutate(across(everything(), as.numeric))
    dataQualtricsClean <-



  
  
  
  
  
# Unreal Output
  dataUnreal <- rawUnreal |>
    rename_at('ParticipantID', ~'Participant') 
  dataGaming <- dataGaming[-1, ]

  
  
  
data <- merge(dataGaming, dataQualtrics, by = "Participant", all = TRUE)


  #######################################
  #        Descriptive Statistics       #
  #######################################

# Reliability Check
  cronbach.alpha(ScaleEmpathy, na.rm = TRUE)
  cronbach.alpha(scaleAltruism, na.rm = TRUE)
  cronbach.alpha(scaleCourage, na.rm = TRUE)
  
#


