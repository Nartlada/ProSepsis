#-------------------------------------------------------------------------------
# tblLabVItalS
# Created 4/7/2022
#-------------------------------------------------------------------------------
tblLabVItalS <- tblLabVItalS %>%
  
  # Delete unused columns
  select(-c(starts_with("_"))) %>%
  
  # Create/convert columns
  mutate(
    
    # MAP calculation
    vsmap = (VSBPS+2*VSBPD)/3
  )

tblLabVItalSL <- pivot_wider(
  tblLabVItalS %>% select(-c(ID, LabID, VSCVPND, VSIVCND, VSOthVaso:VSPtType)) %>% 
    arrange(SepsisID, VSDT) %>% 
    group_by(SepsisID) %>% 
    mutate(n=row_number(VSDT)),
  names_from = n, 
  values_from = VSDT:vsmap)
