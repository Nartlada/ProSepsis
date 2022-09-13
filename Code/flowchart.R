# Figures in first 11 boxes (up to enrollment) are from screening (df_scr)
df_sumscr <- df_scr %>%
  mutate(
    infect = ifelse(
      IFTropSep == 1 |
        IFRespiratory == 1 |
        IFBlood == 1 |
        IFGas == 1 |
        IFUrine == 1 |
        IFSkin == 1 |
        IFNerve == 1 |
        IFOth == 1,
      1,
      0
    ),
    across(SirsTemp:qS_Glasgow, ~ ifelse(. == 2, 0, .)),
    N_sirs = SirsTemp + SirsHR + SirsRR + SirsWBC,
    N_QSofa = qS_RR + qS_BP + qS_Glasgow
  ) %>%
  summarise(
    scr         = n(),
    scr_NP      = sum(HospitalID == 1, na.rm = TRUE),
    scr_MS      = sum(HospitalID == 2, na.rm = TRUE),
    exc_48      = sum(Refer48hrs == 1, na.rm = TRUE),
    exc_inf     = sum(IFNo == 1, na.rm = TRUE),
    inf         = sum(infect, na.rm = TRUE),
    exc_sirs_qs = sum(infect == 1 &
                        (N_sirs <  2 & N_QSofa <  2), na.rm = TRUE),
    sep         = sum(infect == 1 &
                        (N_sirs >= 2 | N_QSofa >= 2), na.rm = TRUE),
    pos_pos     = sum(infect == 1 &
                        (N_sirs >= 2 & N_QSofa >= 2), na.rm = TRUE),
    pos_neg     = sum(infect == 1 &
                        (N_sirs >= 2 & N_QSofa <  2), na.rm = TRUE),
    neg_pos     = sum(infect == 1 &
                        (N_sirs <  2 & N_QSofa >= 2), na.rm = TRUE),
    exc_con     = sum(infect == 1 & (N_sirs >= 2 | N_QSofa >= 2) &
                        IsConsent == 2, na.rm = TRUE),
    reason1     = sum(
      infect == 1 & (N_sirs >= 2 | N_QSofa >= 2) &
        IsConsent == 2 &
        ReasonNo == 1,
      na.rm = TRUE
    ),
    reason2     = sum(
      infect == 1 & (N_sirs >= 2 | N_QSofa >= 2) &
        IsConsent == 2 &
        ReasonNo == 2,
      na.rm = TRUE
    ),
    reason3     = sum(
      infect == 1 & (N_sirs >= 2 | N_QSofa >= 2) &
        IsConsent == 2 &
        ReasonNo == 3,
      na.rm = TRUE
    ),
    reason4     = sum(
      infect == 1 & (N_sirs >= 2 | N_QSofa >= 2) &
        IsConsent == 2 &
        ReasonNo == 4,
      na.rm = TRUE
    ),
    reason5     = sum(
      infect == 1 & (N_sirs >= 2 | N_QSofa >= 2) &
        IsConsent == 2 &
        ReasonNo == 5,
      na.rm = TRUE
    )
  )

# Figures in boxes after enrollment are from enrollment (df_enr)
df_sumenr <- df_enr %>%
  mutate(across(c(dead28,Sepsis_Scale), ~ as.numeric(.))) %>% 
  summarise(
    enr         = n(),
    exc_pending = sum(is.na(dead28), na.rm = TRUE),
    d           = sum(dead28 == 1, na.rm = TRUE),
    d_non       = sum(dead28 == 1 & Sepsis_Scale == 1, na.rm = TRUE),
    d_sepsis    = sum(dead28 == 1 & Sepsis_Scale == 2, na.rm = TRUE),
    d_shock     = sum(dead28 == 1 & Sepsis_Scale == 3, na.rm = TRUE),
    a           = sum(dead28 == 2, na.rm = TRUE),
    a_non       = sum(dead28 == 2 & Sepsis_Scale == 1, na.rm = TRUE),
    a_sepsis    = sum(dead28 == 2 & Sepsis_Scale == 2, na.rm = TRUE),
    a_shock     = sum(dead28 == 2 & Sepsis_Scale == 3, na.rm = TRUE)
  )

# Texts for figures
l1 <- paste0(comma(df_sumscr$scr_NP),
             ' patients at NP',
             '\n',
             ' were screened')
l2 <- paste0(comma(df_sumscr$scr_MS),
             ' patients at MS',
             '\n',
             ' were screened')
l3 <- paste0(comma(df_sumscr$scr),
             ' total patients age >= 15 years',
             '\n',
             'visit at ER/OPD were screened')
l4 <- paste0(df_sumscr$exc_48,
             ' (',
             percent(df_sumscr$exc_48 / df_sumscr$scr, 0.1),
             ') admitted more than 48 hours from referral hospital')
l5 <- paste0(df_sumscr$exc_inf,
             ' (',
             percent(df_sumscr$exc_inf / df_sumscr$scr, 0.1),
             ') no source of infection')
l6 <- paste0(comma(df_sumscr$inf),
             ' (',
             percent(df_sumscr$inf / df_sumscr$scr, 0.1),
             ') with suspected infection')
l7 <- paste0(df_sumscr$exc_sirs_qs,
             ' (',
             percent(df_sumscr$exc_sirs_qs / df_sumscr$inf, 0.1),
             ')  SIRS & qSOFA < 2')
l8 <- paste0(df_sumscr$sep,
             ' (',
             percent(df_sumscr$sep / df_sumscr$inf, 0.1),
             ') suspected sepsis (SIRS >= 2 or qSOFA >= 2)')
l9 <- paste0(df_sumscr$pos_pos,
             ' (',
             percent(df_sumscr$pos_pos / df_sumscr$sep, 0.1),
             ') SIRS + qSOFA +')
l10 <- paste0(df_sumscr$pos_neg,
              ' (',
              percent(df_sumscr$pos_neg / df_sumscr$sep, 0.1),
              ') SIRS + qSOFA -')
l11 <- paste0(df_sumscr$neg_pos,
              ' (',
              percent(df_sumscr$neg_pos / df_sumscr$sep, 0.1),
              ') SIRS - qSOFA +')
l12 <- paste0(df_sumscr$exc_con,
              ' (',
              percent(df_sumscr$exc_con / df_sumscr$sep, 0.1),
              ') not agree to participate')
l13 <- paste0(df_sumscr$reason1,
              ' (',
              percent(df_sumscr$reason1 / df_sumscr$sep, 0.1),
              ') not interest')
l14 <- paste0(df_sumscr$reason2,
              ' (',
              percent(df_sumscr$reason2 / df_sumscr$sep, 0.1),
              ') no benefit')
l15 <- paste0(df_sumscr$reason3,
              ' (',
              percent(df_sumscr$reason3 / df_sumscr$sep, 0.1),
              ') guardian N/A')
l16 <- paste0(df_sumscr$reason4,
              ' (',
              percent(df_sumscr$reason4 / df_sumscr$sep, 0.1),
              ') not in condition')
l17 <- paste0(df_sumscr$reason5,
              ' (',
              percent(df_sumscr$reason5 / df_sumscr$sep, 0.1),
              ') other')
l18 <- paste0(df_sumenr$enr,
              ' (',
              percent(df_sumenr$enr / df_sumscr$sep, 0.1),
              ') enrolled suspected sepsis')
l19 <- paste0(df_sumenr$exc_pending,
              ' (',
              percent(df_sumenr$exc_pending / df_sumenr$enr, 0.1),
              ')  in pending')
l20 <- paste0(df_sumenr$d,
              ' (',
              percent(df_sumenr$d / (df_sumenr$d + df_sumenr$a), 0.1),
              ') 28 days mortality')
l21 <- paste0(df_sumenr$d_non,
              ' (',
              percent(df_sumenr$d_non / df_sumenr$d, 0.1),
              ') non-sepsis')
l22 <- paste0(df_sumenr$d_sepsis,
              ' (',
              percent(df_sumenr$d_sepsis / df_sumenr$d, 0.1),
              ') sepsis')
l23 <- paste0(df_sumenr$d_shock,
              ' (',
              percent(df_sumenr$d_shock / df_sumenr$d, 0.1),
              ') septic shock')
l24 <- paste0(df_sumenr$a,
              ' (',
              percent(df_sumenr$a / (df_sumenr$d + df_sumenr$a), 0.1),
              ') survivors')
l25 <- paste0(df_sumenr$a_non,
              ' (',
              percent(df_sumenr$a_non / df_sumenr$a, 0.1),
              ') non-sepsis')
l26 <- paste0(df_sumenr$a_sepsis,
              ' (',
              percent(df_sumenr$a_sepsis / df_sumenr$a, 0.1),
              ') sepsis')
l27 <- paste0(df_sumenr$a_shock,
              ' (',
              percent(df_sumenr$a_shock / df_sumenr$a, 0.1),
              ') septic shock')

d <- DiagrammeR::grViz(
  "digraph flowchart {

    graph [layout = dot,
           label = '',
           labelloc = t,
           fontname = Helvetica,
           fontsize  = 28,
           compound = true]
  
    node [shape = box, 
          color = gray,
          style = filled, 
          fillcolor = WhiteSmoke,
          fontname = Helvetica, 
          fontsize = 20,
          fixedsize = t, 
          width = 6, 
          height = 0.8]
  
    subgraph cluster1 {
    
      label = ''
      node [shape = box, 
            color = gray,
            style = filled, 
            fillcolor = WhiteSmoke,
            fontname = Helvetica, 
            fontsize = 20,
            fixedsize = t, 
            width = 4, 
            height = 0.8]
  
      neg_pos [label = '@@11']
      pos_neg [label = '@@10']
      pos_pos [label = '@@9']
  
      neg_pos;
      pos_neg;
      pos_pos;
      {rank = same; neg_pos pos_neg pos_pos}
      
    }
  
    scr_NP [label = '@@1']
    scr_MS [label = '@@2']
    scr [label = '@@3']
    exc_48 [label =
<
&#8226; @@4<br ALIGN = 'LEFT'/>
&#8226; @@5<br ALIGN = 'LEFT'/>
>
        ]
    inf [label = '@@6']
    exc_inf [label = '@@7']
    sep [label = '@@8']
    exc_con [label =
<    
@@12<br/><br/>
&#8226; @@13<br ALIGN = 'LEFT'/>
&#8226; @@14<br ALIGN = 'LEFT'/>
&#8226; @@15<br ALIGN = 'LEFT'/>
&#8226; @@16<br ALIGN = 'LEFT'/>
&#8226; @@17<br ALIGN = 'LEFT'/>
>
        ]
    enr [label = '@@18']
    exc_pending [label = '@@19']
    d [label = 
<
@@20<br/><br/>
&#8226; @@21<br ALIGN = 'LEFT'/>
&#8226; @@22<br ALIGN = 'LEFT'/>
&#8226; @@23<br ALIGN = 'LEFT'/>
>
      ]
    a [label = 
<
@@24<br/><br/>
&#8226; @@25<br ALIGN = 'LEFT'/>
&#8226; @@26<br ALIGN = 'LEFT'/>
&#8226; @@27<br ALIGN = 'LEFT'/>
>
      ]
  
    blank1 [label = '', width = 0.01, height = 0.01]
    blank2 [label = '', width = 0.01, height = 0.01]
    blank3 [label = '', width = 0.01, height = 0.01]
    blank4 [label = '', width = 0.01, height = 0.01]
    blank5 [label = '', width = 0.01, height = 0.01]
  
    scr_NP  -> scr [minlen = 3];
    scr     -> scr_MS [dir = back, minlen = 3];
    {rank = same; scr_NP scr scr_MS};
    scr     -> blank1 [dir = none];
    blank1  -> exc_48 [minlen = 5];
    {rank = same; blank1 exc_48};
    blank1  -> inf;
    inf     -> blank2 [dir = none];
    blank2  -> exc_inf [minlen = 5];
    {rank = same; blank2 exc_inf};
    blank2  -> sep;
    sep     -> pos_neg [lhead = cluster1];
    pos_neg -> blank3 [dir = none , ltail = cluster1];
    blank3  -> exc_con [minlen = 5];
    {rank = same; blank3 exc_con};
    blank3  -> enr;
    enr     -> blank4 [dir = none];
    blank4  -> exc_pending [minlen = 5];
    {rank = same; blank4 exc_pending};
    blank4  -> blank5 [dir = none];
    blank5  -> d;
    blank5  -> a
    
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
  [20]: l20
  [21]: l21
  [22]: l22
  [23]: l23
  [24]: l24
  [25]: l25
  [26]: l26
  [27]: l27
      
")
