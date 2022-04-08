#-------------------------------------------------------------------------------
# tblpart1
# Created 4/7/2022
#-------------------------------------------------------------------------------
tblpart1 <- tblpart1 %>%
  
  # Delete unused columns
  select(-c(starts_with("_"), Completed)) %>%
  
  # Create/convert columns
  mutate(
    
    # Convert datetime to date
    across(c(matches("Date"), DOB, matches("ColDT")), as.Date),
    
    # Recode 0 in multiple choices (1,2,3...) to missing
    across(c(Age15, Refer, Refer48hrs, IsEligible, IsConsent, ChildConsent, 
             SirsTemp:qS_Glasgow),
           function(f) {ifelse(f == 0, NA, f)}),
    
    # Recode 2 in Y/N (1/2) to 0
    across(c(Age15, Refer, Refer48hrs, IsEligible, IsConsent, ChildConsent, 
             SirsTemp:qS_Glasgow),
           function(f) {ifelse(f == 2, 0, f)}),
    
    # Rename and factor HospitalID
    Hospital = factor(HospitalID,
                      levels = c(1, 2),
                      labels = c("Nakorn Phanom","Mae Sot"))) %>% 
  
  # Delete unused columns
  select(-HospitalID)
