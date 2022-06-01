library(rsvg)
library(DiagrammeRsvg)
library(DiagrammeR)

df <- tblpart1 %>% 
# filter(Hospital=='Nakorn Phanom') %>%
# filter(Hospital=='Mae Sot') %>% 
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
            sussep = sum(SirsTemp + SirsHR + SirsRR + SirsWBC >= 2, na.rm = TRUE),
            enr = sum(!is.na(SepsisID), na.rm = TRUE))

df2 <- psmast %>%
# filter(HospitalID ==1) %>%
 #filter(HospitalID ==2) %>%
  
  summarise(nonsepsis = sum(Sofa_Sepsis == 2, na.rm = TRUE),
            sepsis    = sum(Sofa_Sepsis == 1, na.rm = TRUE),
            complete   = sum(!is.na(DischargeStatus) &!is.na(Sofa_Sepsis), na.rm = TRUE),
            CompleteSepsis    = sum(!is.na(DischargeStatus) & Sofa_Sepsis == 1, na.rm = TRUE),
            CompleteNonsepsis = sum(!is.na(DischargeStatus) & Sofa_Sepsis == 2, na.rm = TRUE))
  
l1 <- paste0(scales::comma(df$scr),' persons age >= 15 years visit at ER/OPD', '\n', ' were screened')
l2 <- paste0(df$inf, ' (', scales::percent(df$inf/df$scr,0.1), ') with suspected infection')
l3 <- paste0(df$exc, ' (', scales::percent(df$exc/df$scr,0.1), ') Admitted > 48 hours','\n', 'from referral hospital')
l4 <- paste0(df$sussep, ' (', scales::percent(df$sussep/df$inf,0.1), ') suspected sepsis (SIRS >= 2)', '\n',
             df$sirs, ' (', scales::percent(df$sirs/df$inf,0.1), ') SIRS >= 2')
l5 <- paste0(df$enr, ' (', scales::percent(df$enr/df$sussep,0.1), ') enrolled suspected sepsis')
l6 <- paste0(df2$complete, ' (', scales::percent(df2$complete/df$enr,0.1), ') with complete data','\n','(having discharge status)')
l8 <- paste0(df2$CompleteNonsepsis, ' (', scales::percent(df2$CompleteNonsepsis/df2$complete,0.1), ') non-sepsis')
l7 <- paste0(df2$CompleteSepsis, ' (', scales::percent(df2$CompleteSepsis/df2$complete,0.1), ') sepsis','\n','(SOFA >= 2)')
#l8 <- paste0(df2$shock, ' (', scales::percent(df2$shock/df$enr,0.1), ') septic shock','\n','(MAP <=65 mmHg and lactate > 2.0)')
#l8 <- paste0(df2$pending, ' (', scales::percent(df2$pending/df$enr,0.1), ') pending')

DiagrammeR::grViz("digraph graphtest {
  
  graph [layout = dot,
         label = 'All',
         labelloc = t,
         fontname = Helvetica,
         fontsize  = 36]
       
  node [shape = box, color = gray,
        style = filled, fillcolor = WhiteSmoke, 
        fontname = Helvetica, fontsize = 20,
        fixedsize = t, width = 6, height = 0.8]
        
  screened [label = '@@1']
  infection [label = '@@2']
  s_sepsis [label = '@@4']
  enrolled [label = '@@5']
  complete [label = '@@6', width = 4]
  CompleteSepsis [label = '@@7', width = 4]
  CompleteNonsepsis [label = '@@8', width = 4]
 
  blank [label = '', width = 0.01, height = 0.01]
  excluded [label = '@@3']
  
  { rank = same; blank excluded }
  
  screened -> blank[ dir = none ];
  blank -> excluded[ minlen = 5 ];
  blank -> infection;
  infection -> s_sepsis -> enrolled;
  enrolled -> complete;
  complete -> CompleteSepsis;
  complete -> CompleteNonsepsis;
 
}
  [1]: l1
  [2]: l2
  [3]: l3
  [4]: l4
  [5]: l5
  [6]: l6
  [7]: l7
  [8]: l8
  
") 

