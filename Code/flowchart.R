df <- tblpart1 %>% 
  # filter(Hospital=='Mae Sot') %>% 
  summarise(scr = n(),
            inf = sum(tblpart1$IFTropSep == TRUE | 
                        tblpart1$IFRespiratory == TRUE |
                        tblpart1$IFBlood == TRUE |
                        tblpart1$IFGas == TRUE |
                        tblpart1$IFUrine == TRUE |
                        tblpart1$IFSkin == TRUE |
                        tblpart1$IFNerve == TRUE |
                        tblpart1$IFOth == TRUE, na.rm = TRUE),
            exc = sum(Refer48hrs == 1, na.rm = TRUE),
            sirs = sum(SirsTemp + SirsHR + SirsRR + SirsWBC >= 2, na.rm = TRUE),
            sofa = sum(qS_RR + qS_BP + qS_Glasgow >= 2, na.rm = TRUE),
            sussep = sum(SirsTemp + SirsHR + SirsRR + SirsWBC >= 2 |
                        qS_RR + qS_BP + qS_Glasgow >= 2, na.rm = TRUE),
            enr = sum(!is.na(SepsisID), na.rm = TRUE))

l1 <- paste0(scales::comma(df$scr),' persons age >= 15 years visit at ER/OPD', '\n', ' were screened')
l2 <- paste0(df$inf, ' (', scales::percent(df$inf/df$scr,0.1), ') with suspected infection')
l3 <- paste0(df$exc, ' (', scales::percent(df$exc/df$scr,0.1), ') Admitted > 48 hours','\n', 'from referral hospital')
l4 <- paste0(df$sussep, ' (', scales::percent(df$sussep/df$inf,0.1), ') suspected sepsis (SIRS or qSOFA >= 2)', '\n',
             df$sirs, ' (', scales::percent(df$sirs/df$inf,0.1), ') SIRS >= 2', '\n',
             df$sofa, ' (', scales::percent(df$sofa/df$inf,0.1), ') SOFA >= 2')
l5 <- paste0(df$enr, ' (', scales::percent(df$enr/df$sussep,0.1), ') enrolled suspected sepsis')

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
  
  a -> b -> d -> e
  a -> c
  }
  
  [1]: l1
  [2]: l2
  [3]: l3
  [4]: l4
  [5]: l5
  ",
)

