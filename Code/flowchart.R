df <- tblpart1 %>% 
 # filter(Hospital=='Nakorn Phanom') %>% 
#  filter(Hospital=='Mae Sot') %>% 
  summarise(scr = n(),
            inf = sum(IFTropSep == TRUE | 
                        IFRespiratory == TRUE |
                        IFBlood == TRUE |
                        IFGas == TRUE |
                        IFUrine == TRUE |
                        IFSkin == TRUE |
                        IFNerve == TRUE |
                        IFOth == TRUE, na.rm = TRUE),
            exc = sum(Refer48hrs == 1, na.rm = TRUE),
            sirs = sum(SirsTemp + SirsHR + SirsRR + SirsWBC >= 2, na.rm = TRUE),
            sofa = sum(qS_RR + qS_BP + qS_Glasgow >= 2, na.rm = TRUE),
            sussep = sum(SirsTemp + SirsHR + SirsRR + SirsWBC >= 2 |
                        qS_RR + qS_BP + qS_Glasgow >= 2, na.rm = TRUE),
            enr = sum(!is.na(SepsisID), na.rm = TRUE))

df2 <- psmast %>% 
# filter(HospitalID ==1) %>% 
# filter(HospitalID ==2) %>% 
  summarise(Sepsis = sum(Sofa_Sepsis == 1 |
                         SepReview1 %in% c(1,2) |
                         SepReview2 %in% c(1,2), na.rm = TRUE),
            Shock = sum(Septic_Shock ==1, na.rm = TRUE))
  
l1 <- paste0(scales::comma(df$scr),' persons age >= 15 years visit at ER/OPD', '\n', ' were screened')
l2 <- paste0(df$inf, ' (', scales::percent(df$inf/df$scr,0.1), ') with suspected infection')
l3 <- paste0(df$exc, ' (', scales::percent(df$exc/df$scr,0.1), ') Admitted > 48 hours','\n', 'from referral hospital')
l4 <- paste0(df$sussep, ' (', scales::percent(df$sussep/df$inf,0.1), ') suspected sepsis (SIRS or qSOFA >= 2)', '\n',
             df$sirs, ' (', scales::percent(df$sirs/df$inf,0.1), ') SIRS >= 2', '\n',
             df$sofa, ' (', scales::percent(df$sofa/df$inf,0.1), ') SOFA >= 2')
l5 <- paste0(df$enr, ' (', scales::percent(df$enr/df$sussep,0.1), ') enrolled suspected sepsis')
l6 <- paste0(df2$Sepsis, ' (', scales::percent(df2$Sepsis/df$inf,0.1), ') sepsis (SOFA >= 2 or doctor determine)', '\n',
             df2$Shock, ' (', scales::percent(df2$Shock/df$inf,0.1), ') septic shock (MAP <65 mmHg)')


DiagrammeR::grViz("
  digraph graph2 {
  
  graph [layout = dot, rankdir = TB]
  
  # node definitions with substituted label text
  node [shape = rectangle, color = gray,
        style = filled, fillcolor = WhiteSmoke, 
        fontname = Helvetica,
        fixedsize = true, width = 5, height = 0.8]
  a [label = '@@1']
  b [label = '@@2']
  c [label = '@@3']
  d [label = '@@4']
  e [label = '@@5']
  f [label = '@@6']
  
  a -> b -> d -> e -> f
  a -> c
  }
  
  [1]: l1
  [2]: l2
  [3]: l3
  [4]: l4
  [5]: l5
  [6]: l6
  ",
)

