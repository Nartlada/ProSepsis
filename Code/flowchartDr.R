library(rsvg)
library(DiagrammeRsvg)
library(DiagrammeR)
library(tidyverse)
library(haven)

#-- haven is used to read data from other prg i.e _sas, _stata --#

tblpart1 <- read_sas("C:/MyD/Sepsis_Pro/MF/tblpart1.sas7bdat", NULL) 
psmast <- read_sas("C:/MyD/Sepsis_Pro/MF/psmast.sas7bdat", NULL)

df <- tblpart1 %>% 
mutate(
  infect = ifelse(IFTropSep == TRUE |
                    IFRespiratory == TRUE |
                    IFBlood == TRUE |
                    IFGas == TRUE |
                    IFUrine == TRUE |
                    IFSkin == TRUE |
                    IFNerve == TRUE |
                    IFOth == TRUE,1,0),
  sirstemp= ifelse (SirsTemp ==2,0,SirsTemp),
  sirshr = ifelse(SirsHR ==2,0,SirsHR),
  sirsrr = ifelse(SirsRR ==2,0,SirsRR),
  sirswbc = ifelse(SirsWBC ==2,0,SirsWBC),
  qs_rr = ifelse(qS_RR ==2,0,qS_RR),
  qs_bp = ifelse(qS_BP ==2,0,qS_BP),
  qs_glasgow = ifelse(qS_Glasgow ==2,0,qS_Glasgow),
  sirs_count = sirstemp + sirshr + sirsrr +sirswbc,
  qs_count = qs_rr + qs_bp + qs_glasgow) %>% 


# filter(Hospital=='Nakorn Phanom') %>%
# filter(Hospital=='Mae Sot') %>% 
  summarise(scr = n(),
            scrNP = sum(HospitalID == 1, na.rm=TRUE),
            scrMS = sum(HospitalID == 2, na.rm=TRUE),
            exc48 = sum(Refer48hrs == 1, na.rm = TRUE),
            excinf = sum(IFNo == 1, na.rm = TRUE),

            inf = sum(IFTropSep == TRUE |
                       IFRespiratory == TRUE |
                       IFBlood == TRUE |
                       IFGas == TRUE |
                       IFUrine == TRUE |
                       IFSkin == TRUE |
                       IFNerve == TRUE |
                       IFOth == TRUE, na.rm = TRUE),
           
            exc_sirs = sum(infect == 1 & sirs_count < 2 & qs_count < 2,na.rm = TRUE),

           # sirs = sum(SirsTemp + SirsHR + SirsRR + SirsWBC >= 2, na.rm = TRUE),
           
            sirs_or_qs = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2), na.rm = TRUE),
            sirsp_qsp = sum(infect == 1 & (sirs_count >= 2 & qs_count >= 2), na.rm = TRUE),
            sirsp_qsm = sum(infect == 1 & (sirs_count >= 2 & qs_count < 2), na.rm = TRUE),
            sirsm_qsp = sum(infect == 1 & (sirs_count < 2 & qs_count >= 2), na.rm = TRUE),
            sofa = sum(qS_RR + qS_BP + qS_Glasgow >= 2, na.rm = TRUE),
            exc_consent = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2) & IsConsent == 2, na.rm = TRUE),
           Reason1 = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2) & IsConsent == 2 & ReasonNo == 1, na.rm = TRUE),
           Reason2 = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2) & IsConsent == 2 & ReasonNo == 2, na.rm = TRUE),
           Reason3 = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2) & IsConsent == 2 & ReasonNo == 3, na.rm = TRUE),
           Reason4 = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2) & IsConsent == 2 & ReasonNo == 4, na.rm = TRUE),
           Reason5 = sum(infect == 1 & (sirs_count >= 2 | qs_count >= 2) & IsConsent == 2 & ReasonNo == 5, na.rm = TRUE),
  )
           

df2 <- psmast %>%
#  filter(ScreenDate <= '2022-02-25') %>% 
# filter(HospitalID ==1) %>%
 #filter(HospitalID ==2) %>%
  
  summarise(
            enr = n(),       
            exc_disc= sum(is.na(DischargeStatus), na.rm = TRUE),
           # exc_mortal = sum(dead4 == 1, na.rm = TRUE),
            exc_sep = sum(is.na(Review_Scaler), na.rm = TRUE),
            nonsepsis_np = sum(Review_Scaler == 1 & HospitalID == 1, na.rm = TRUE),
            nonsepsis_ms = sum(Review_Scaler == 1 & HospitalID ==2, na.rm = TRUE),
            sepsis_np    = sum(Review_Scaler == 2 & HospitalID == 1, na.rm = TRUE),
            sepsis_ms    = sum(Review_Scaler == 2 & HospitalID == 2, na.rm = TRUE),
            shock_np    = sum(Review_Scaler == 3 & HospitalID == 1, na.rm = TRUE),
            shock_ms    = sum(Review_Scaler == 3 & HospitalID == 2, na.rm = TRUE),
            d_nonsepsis_np = sum(dead4 == 1 & Review_Scaler == 1 & HospitalID == 1, na.rm = TRUE),
            d_nonsepsis_ms = sum(dead4 == 1 & Review_Scaler == 1 & HospitalID ==2, na.rm = TRUE),
            d_sepsis_np    = sum(dead4 == 1 & Review_Scaler == 2 & HospitalID == 1, na.rm = TRUE),
            d_sepsis_ms    = sum(dead4 == 1 & Review_Scaler == 2 & HospitalID == 2, na.rm = TRUE),
            d_shock_np    = sum(dead4 == 1 & Review_Scaler == 3 & HospitalID == 1, na.rm = TRUE),
            d_shock_ms    = sum(dead4 == 1 & Review_Scaler == 3 & HospitalID == 2, na.rm = TRUE))
 
l1 <- paste0(scales::comma(df$scrNP),' patients at NP', '\n', ' were screened')
l2 <- paste0(scales::comma(df$scrMS),' patients at MS', '\n', ' were screened') 
l3 <- paste0(scales::comma(df$scr),' total patients age >= 15 years',  '\n', 'visit at ER/OPD were screened')
l4 <- paste0(df$exc48, ' (', scales::percent(df$exc48/df$scr,0.1), ') Admitted > 48 hours from referral hospital \n',
             df$excinf, ' (', scales::percent(df$excinf/df$scr,0.1), ') no source of infection'
)
l5 <- paste0(df$inf, ' (', scales::percent(df$inf/df$scr,0.1), ') with suspected infection')
l6 <- paste0(df$exc_sirs, ' (', scales::percent(df$exc_sirs/df$scr,0.1), ')  SIRS or qSOFA < 2')
l7 <- paste0(df$sirs_or_qs, ' (', scales::percent(df$sirs_or_qs/df$inf,0.1), ') suspected sepsis (SIRS or qSOFA >= 2)')
l8 <- paste0(df$sirsp_qsp, ' (', scales::percent(df$sirsp_qsp/df$sirs_or_qs,0.1), ') SIRS + qSOFA +')
l9 <- paste0(df$sirsp_qsm, ' (', scales::percent(df$sirsp_qsm/df$sirs_or_qs,0.1), ') SIRS + qSOFA -')
l10 <- paste0(df$sirsm_qsp, ' (', scales::percent(df$sirsm_qsp/df$sirs_or_qs,0.1), ') SIRS - qSOFA +')
l11 <- paste0(df$exc_consent, ' (', scales::percent(df$exc_consent/df$sirs_or_qs,0.1), ') Not agree to participate \n',
              df$Reason1, ' (', scales::percent(df$Reason1/df$sirs_or_qs,0.1), ') No interest \n',
              df$Reason2, ' (', scales::percent(df$Reason2/df$sirs_or_qs,0.1), ') No benefit\n',
              df$Reason3, ' (', scales::percent(df$Reason3/df$sirs_or_qs,0.1), ') Guardian N/A \n',
              df$Reason4, ' (', scales::percent(df$Reason4/df$sirs_or_qs,0.1), ') Condition N/A \n',
              df$Reason5, ' (', scales::percent(df$Reason5/df$sirs_or_qs,0.1), ') Other ')
l12 <- paste0(df2$enr, ' (', scales::percent(df2$enr/df$sirs_or_qs,0.1), ') enrolled suspected sepsis')
#l14 <- paste0(df2$exc_mortal, ' (', scales::percent(df2$exc_mortal/df$sirs_or_qs,0.1), ')  28 days mortality')
l13 <- paste0(df2$exc_sep, ' (', scales::percent(df2$exc_sep/df$sirs_or_qs,0.1), ')  excluded')
l14 <- paste0((df2$nonsepsis_np + df2$nonsepsis_ms), ' (', scales::percent((df2$nonsepsis_np + df2$nonsepsis_ms)/df$sirs_or_qs,0.1), ') Non sepsis \n\n',
              df2$nonsepsis_np , ' (', scales::percent(df2$nonsepsis_np/df$sirs_or_qs,0.1), ') at NP \n',
              df2$nonsepsis_ms, ' (', scales::percent( df2$nonsepsis_ms/df$sirs_or_qs,0.1), ') at MS')
l15 <- paste0((df2$sepsis_np + df2$sepsis_ms), ' (', scales::percent((df2$sepsis_np + df2$sepsis_ms)/df$sirs_or_qs,0.1), ') sepsis \n\n',
              df2$sepsis_np , ' (', scales::percent(df2$sepsis_np/df$sirs_or_qs,0.1), ') at NP \n',
              df2$sepsis_ms, ' (', scales::percent( df2$sepsis_ms/df$sirs_or_qs,0.1), ') at MS')
l16 <- paste0((df2$shock_np + df2$shock_ms), ' (', scales::percent((df2$shock_np + df2$shock_ms)/df$sirs_or_qs,0.1), ') shock \n\n',
              df2$shock_np , ' (', scales::percent(df2$shock_np/df$sirs_or_qs,0.1), ') at NP \n',
              df2$shock_ms, ' (', scales::percent( df2$shock_ms/df$sirs_or_qs,0.1), ') at MS')

l17 <- paste0((df2$d_nonsepsis_np + df2$d_nonsepsis_ms), ' (', scales::percent((df2$d_nonsepsis_np + df2$d_nonsepsis_ms)/df$sirs_or_qs,0.1), ') 28 days mortality \n\n',
              df2$d_nonsepsis_np , ' (', scales::percent(df2$d_nonsepsis_np/df$sirs_or_qs,0.1), ') at NP \n',
              df2$d_nonsepsis_ms, ' (', scales::percent( df2$d_nonsepsis_ms/df$sirs_or_qs,0.1), ') at MS')

l18 <- paste0((df2$d_sepsis_np + df2$d_sepsis_ms), ' (', scales::percent((df2$d_sepsis_np + df2$d_sepsis_ms)/df$sirs_or_qs,0.1), ') 28 days mortality \n\n',
              df2$d_sepsis_np , ' (', scales::percent(df2$d_sepsis_np/df$sirs_or_qs,0.1), ') at NP \n',
              df2$d_sepsis_ms, ' (', scales::percent( df2$d_sepsis_ms/df$sirs_or_qs,0.1), ') at MS')

l19 <- paste0((df2$d_shock_np + df2$d_shock_ms), ' (', scales::percent((df2$d_shock_np + df2$d_shock_ms)/df$sirs_or_qs,0.1), ') 28 days mortality\n\n',
              df2$d_shock_np , ' (', scales::percent(df2$d_shock_np/df$sirs_or_qs,0.1), ') at NP \n',
              df2$d_shock_ms, ' (', scales::percent( df2$d_shock_ms/df$sirs_or_qs,0.1), ') at MS')


DiagrammeR::grViz("digraph graphtest {
  
  graph [layout = dot,
         label = <<b>Figure 1.</b>  Flow chart presenting the number of screening and enroll patients with sepsis who participating in this sepsis epidemiology activities<br/>Sepsis outcomes determined by Physician Review<br/><br/>>,
         labelloc = t,
         fontname = Helvetica,
         fontsize  = 30,
         compound = true]
       
  node [shape = box, color = gray,
        style = filled, fillcolor = WhiteSmoke, 
        fontname = Helvetica, fontsize = 20,
        fixedsize = t, width = 6, height = 0.8]
        
  subgraph cluster1 {
  label = ''
  node [shape = box, color = gray,
        style = filled, fillcolor = WhiteSmoke, 
        fontname = Helvetica, fontsize = 20,
        fixedsize = t, width = 4, height = 0.8]
        
    sirs_qs_3 [label = '@@10']
    sirs_qs_2 [label = '@@9']
    sirs_qs_1 [label = '@@8']
    
    sirs_qs_3;
    sirs_qs_2;
    sirs_qs_1
   {rank = same; sirs_qs_3 sirs_qs_2 sirs_qs_1};
  }
      
  subgraph cluster2 {
  label = ''
  node [shape = box, color = gray,
        style = filled, fillcolor = WhiteSmoke, 
        fontname = Helvetica, fontsize = 20,
        fixedsize = t, width = 4, height = 0.8]
        
    out3 [label = '@@16']
    out2 [label = '@@15']
    out1 [label = '@@14']
    dead1 [label = '@@17']
    dead2 [label = '@@18']
    dead3 [label = '@@19']
    
    out1 -> dead1;
    out2 -> dead2;
    out3 -> dead3
    {rank = same; out1 out2 out3};
    
  }
    
  screenedNP [label = '@@1']
  screenedMS [label = '@@2']
  screened [label = '@@3']
  exc48 [label = '@@4']
  infection [label = '@@5']
  exc2 [label = '@@6']
  s_sepsis [label = '@@7']
  exc_consent [label = '@@11']
  s_enr [label = '@@12']
  excs [label = '@@13']
  
  blank1 [label = '', width = 0.01, height = 0.01]
  blank2 [label = '', width = 0.01, height = 0.01]
  blank3 [label = '', width = 0.01, height = 0.01]
  blank4 [label = '', width = 0.01, height = 0.01]
  blank5 [label = '', width = 0.01, height = 0.01]
  
  screenedNP -> screened [ minlen = 3 ];
  screened -> screenedMS[dir = back, minlen = 3];
                  {rank = same; screenedNP screened screenedMS};
  screened -> blank1[ dir = none ];
  blank1 -> exc48[minlen = 5 ];
                  {rank = same; blank1 exc48};
  blank1 -> infection;
  infection -> blank2[ dir = none ];
  blank2 -> exc2[minlen = 5 ];
                  {rank = same; blank2 exc2};
  blank2 -> s_sepsis;
  s_sepsis ->  sirs_qs_2[lhead = cluster1];
  sirs_qs_2 -> blank3[ dir = none , ltail = cluster1];
  blank3 -> exc_consent[minlen = 5 ];
                  {rank = same; blank3 exc_consent};
  blank3 -> s_enr;
  s_enr -> blank4[ dir = none ];
  blank4 -> excs[minlen = 5 ];
                  {rank = same; blank4 excs};

  
  blank4 -> out2[lhead = cluster2]
  

  
}
  [1]: l1
  [2]: l2
  [3]: l3
  [4]: l4
  [5]: l5
  [6]: l6
  [7]: l7
  [8]: l8
  [9]: l9
  [10]: l10
  [11]: l11
  [12]: l12
  [13]: l13
  [14]: l14
  [15]: l15
  [16]: l16
  [17]: l17
  [18]: l18
  [19]: l19
") 

