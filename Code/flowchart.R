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
    ),
    exc_sirsneg = sum(infect == 1 & IsConsent == 1 &
                        N_sirs < 2, na.rm = TRUE),
    exc_msopd   = sum(infect == 1 & IsConsent == 1 &
                        N_sirs >= 2 &
                        (OPD_IPD == 2 & HospitalID == 2), na.rm = TRUE)
  )

# Figures in boxes after enrollment are from enrollment (df_enr)
df_sumenr <- df_enr %>%
  # filter(SIRS == 1 & !(OPD_IPD == '2 -OPD' & HospitalID == 2)) %>% 
  mutate(across(c(dead28c,Sepsis_Scale), ~ as.numeric(.))) %>%
  summarise(
    enr          = n(),
    ipd          = sum(OPD_IPD == '1 -IPD', na.rm = TRUE),
    di           = sum(OPD_IPD == '1 -IPD' & dead28c == 1, na.rm = TRUE),
    di_non       = sum(OPD_IPD == '1 -IPD' & dead28c == 1 & Sepsis_Scale == 1, na.rm = TRUE),
    di_sepsis    = sum(OPD_IPD == '1 -IPD' & dead28c == 1 & Sepsis_Scale == 2, na.rm = TRUE),
    di_shock     = sum(OPD_IPD == '1 -IPD' & dead28c == 1 & Sepsis_Scale == 3, na.rm = TRUE),
    ai           = sum(OPD_IPD == '1 -IPD' & dead28c == 2, na.rm = TRUE),
    ai_non       = sum(OPD_IPD == '1 -IPD' & dead28c == 2 & Sepsis_Scale == 1, na.rm = TRUE),
    ai_sepsis    = sum(OPD_IPD == '1 -IPD' & dead28c == 2 & Sepsis_Scale == 2, na.rm = TRUE),
    ai_shock     = sum(OPD_IPD == '1 -IPD' & dead28c == 2 & Sepsis_Scale == 3, na.rm = TRUE),
    exci_pending = sum(OPD_IPD == '1 -IPD' & is.na(dead28c), na.rm = TRUE),
    opd          = sum(OPD_IPD == '2 -OPD', na.rm = TRUE),
    do           = sum(OPD_IPD == '2 -OPD' & dead28c == 1, na.rm = TRUE),
    do_non       = sum(OPD_IPD == '2 -OPD' & dead28c == 1 & Sepsis_Scale == 1, na.rm = TRUE),
    do_sepsis    = sum(OPD_IPD == '2 -OPD' & dead28c == 1 & Sepsis_Scale == 2, na.rm = TRUE),
    do_shock     = sum(OPD_IPD == '2 -OPD' & dead28c == 1 & Sepsis_Scale == 3, na.rm = TRUE),
    ao           = sum(OPD_IPD == '2 -OPD' & dead28c == 2, na.rm = TRUE),
    ao_non       = sum(OPD_IPD == '2 -OPD' & dead28c == 2 & Sepsis_Scale == 1, na.rm = TRUE),
    ao_sepsis    = sum(OPD_IPD == '2 -OPD' & dead28c == 2 & Sepsis_Scale == 2, na.rm = TRUE),
    ao_shock     = sum(OPD_IPD == '2 -OPD' & dead28c == 2 & Sepsis_Scale == 3, na.rm = TRUE),
    exco_pending = sum(OPD_IPD == '2 -OPD' & is.na(dead28c), na.rm = TRUE)
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
             ')\nSIRS + qSOFA +')
l10 <- paste0(df_sumscr$pos_neg,
              ' (',
              percent(df_sumscr$pos_neg / df_sumscr$sep, 0.1),
              ')\nSIRS + qSOFA -')
l11 <- paste0(df_sumscr$neg_pos,
              ' (',
              percent(df_sumscr$neg_pos / df_sumscr$sep, 0.1),
              ')\nSIRS - qSOFA +')
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
l18 <- paste0(df_sumscr$exc_sirsneg,
              ' (',
              percent(df_sumscr$exc_sirsneg / df_sumscr$sep, 0.1),
              ') SIRS -')
l19 <- paste0(df_sumscr$exc_msopd,
              ' (',
              percent(df_sumscr$exc_msopd / df_sumscr$sep, 0.1),
              ') OPD in Mae Sot')
l20 <- paste0(df_sumenr$enr,
              ' (',
              percent(df_sumenr$enr / df_sumscr$sep, 0.1),
              ') enrolled suspected sepsis')
l21 <- paste0(df_sumenr$ipd,
              ' (',
              percent(df_sumenr$ipd / df_sumenr$enr, 0.1),
              ') IPD')
l22 <- paste0(df_sumenr$di,
              ' (',
              percent(df_sumenr$di / df_sumenr$ipd, 0.1),
              ') 28 days mortality')
l23 <- paste0(df_sumenr$di_non,
              ' (',
              percent(df_sumenr$di_non / df_sumenr$di, 0.1),
              ') non-sepsis')
l24 <- paste0(df_sumenr$di_sepsis,
              ' (',
              percent(df_sumenr$di_sepsis / df_sumenr$di, 0.1),
              ') sepsis')
l25 <- paste0(df_sumenr$di_shock,
              ' (',
              percent(df_sumenr$di_shock / df_sumenr$di, 0.1),
              ') septic shock')
l26 <- paste0(df_sumenr$ai,
              ' (',
              percent(df_sumenr$ai / df_sumenr$ipd, 0.1),
              ') survivors')
l27 <- paste0(df_sumenr$ai_non,
              ' (',
              percent(df_sumenr$ai_non / df_sumenr$ai, 0.1),
              ') non-sepsis')
l28 <- paste0(df_sumenr$ai_sepsis,
              ' (',
              percent(df_sumenr$ai_sepsis / df_sumenr$ai, 0.1),
              ') sepsis')
l29 <- paste0(df_sumenr$ai_shock,
              ' (',
              percent(df_sumenr$ai_shock / df_sumenr$ai, 0.1),
              ') septic shock')
# l30 <- paste0(df_sumenr$exci_pending,
#               ' (',
#               percent(df_sumenr$exci_pending / df_sumenr$ipd, 0.1),
#               ') pending')
l30 <- paste0(df_sumenr$opd,
              ' (',
              percent(df_sumenr$opd / df_sumenr$enr, 0.1),
              ') OPD')
l31 <- paste0(df_sumenr$do,
              ' (',
              percent(df_sumenr$do / df_sumenr$opd, 0.1),
              ') 28 days mortality')
l32 <- paste0(df_sumenr$do_non,
              ' (',
              percent(df_sumenr$do_non / df_sumenr$do, 0.1),
              ') non-sepsis')
l33 <- paste0(df_sumenr$do_sepsis,
              ' (',
              percent(df_sumenr$do_sepsis / df_sumenr$do, 0.1),
              ') sepsis')
l34 <- paste0(df_sumenr$do_shock,
              ' (',
              percent(df_sumenr$do_shock / df_sumenr$do, 0.1),
              ') septic shock')
l35 <- paste0(df_sumenr$ao,
              ' (',
              percent(df_sumenr$ao / df_sumenr$opd, 0.1),
              ') survivors')
l36 <- paste0(df_sumenr$ao_non,
              ' (',
              percent(df_sumenr$ao_non / df_sumenr$ao, 0.1),
              ') non-sepsis')
l37 <- paste0(df_sumenr$ao_sepsis,
              ' (',
              percent(df_sumenr$ao_sepsis / df_sumenr$ao, 0.1),
              ') sepsis')
l38 <- paste0(df_sumenr$ao_shock,
              ' (',
              percent(df_sumenr$ao_shock / df_sumenr$ao, 0.1),
              ') septic shock')


d <- DiagrammeR::grViz(
  "digraph flowchart {

    graph [layout = dot,
           nodesep = 1;
           compound = true]

    node [shape = box,
          fixedsize = t,
          width = 4,
          height = 1,
          style = filled,
          color = gray,
          fillcolor = WhiteSmoke,
          fontname = Helvetica,
          fontsize = 24]

    subgraph cluster_s {

      label = ''
      node [shape = box,
            fixedsize = t,
            width = 4,
            height = 1,
            style = filled,
            color = gray,
            fillcolor = WhiteSmoke,
            fontname = Helvetica,
            fontsize = 24]

      neg_pos [label = '@@11']
      pos_neg [label = '@@10']
      pos_pos [label = '@@9']

      pos_pos;
      pos_neg;
      neg_pos;
      {rank = same; pos_pos pos_neg neg_pos}

    }

    subgraph cluster_i {

      label = ''
      node [shape = box,
            fixedsize = t,
            width = 4,
            height = 1,
            style = filled,
            color = gray,
            fillcolor = WhiteSmoke,
            fontname = Helvetica,
            fontsize = 24]

      
      ai [label =
<
@@26<br/><br/>
&#8226; @@27<br ALIGN = 'LEFT'/>
&#8226; @@28<br ALIGN = 'LEFT'/>
&#8226; @@29<br ALIGN = 'LEFT'/>
>
      ]
      di [label =
<
@@22<br/><br/>
&#8226; @@23<br ALIGN = 'LEFT'/>
&#8226; @@24<br ALIGN = 'LEFT'/>
&#8226; @@25<br ALIGN = 'LEFT'/>
>
      ]

      di -> ai [style = invis];

    }

    subgraph cluster_o {

      label = ''
      node [shape = box,
            fixedsize = t,
            width = 4,
            height = 1,
            style = filled,
            color = gray,
            fillcolor = WhiteSmoke,
            fontname = Helvetica,
            fontsize = 24]

      ao [label =
<
@@35<br/><br/>
&#8226; @@36<br ALIGN = 'LEFT'/>
&#8226; @@37<br ALIGN = 'LEFT'/>
&#8226; @@38<br ALIGN = 'LEFT'/>
>
      ]
      do [label =
<
@@31<br/><br/>
&#8226; @@32<br ALIGN = 'LEFT'/>
&#8226; @@33<br ALIGN = 'LEFT'/>
&#8226; @@34<br ALIGN = 'LEFT'/>
>
      ]

       do -> ao [style = invis];

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
    exc_sirs_qs [label = '@@7']
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
    exc_enr [label =
<
&#8226; @@18<br ALIGN = 'LEFT'/>
&#8226; @@19<br ALIGN = 'LEFT'/>
>
        ,fillcolor = yellow]
    enr [label = '@@20']
    ipd [label = '@@21']
    opd [label = '@@30']

    blank1 [label = '', width = 0.01, height = 0.01]
    blank2 [label = '', width = 0.01, height = 0.01]
    blank3 [label = '', width = 0.01, height = 0.01]
    blank4 [label = '', width = 0.01, height = 0.01]
    blank5 [label = '', width = 0.01, height = 0.01]

    scr_NP  -> scr;
    scr     -> scr_MS [dir = back];
    {rank = same; scr_NP scr scr_MS};
    scr     -> blank1 [dir = none];
    blank1  -> exc_48;
    {rank = same; blank1 exc_48};
    blank1  -> inf;
    inf     -> blank2 [dir = none];
    blank2  -> exc_sirs_qs;
    {rank = same; blank2 exc_sirs_qs};
    blank2  -> sep;
    sep     -> pos_neg [lhead = cluster_s];
    pos_neg -> blank3 [dir = none, ltail = cluster_s];
    blank3  -> exc_con;
    {rank = same; blank3 exc_con};
    blank3  -> blank4 [dir = none];
    blank4  -> exc_enr;
    {rank = same; blank4 exc_enr};
    blank4  -> enr;
    enr     -> blank5 [dir = none];
    ipd     -> blank5 [dir = back];
    blank5  -> opd;
    {rank = same; ipd blank5 opd};
    ipd     -> di [lhead = cluster_i];
    opd     -> do [lhead = cluster_o];

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
  [28]: l28
  [29]: l29
  [30]: l30
  [31]: l31
  [32]: l32
  [33]: l33
  [34]: l34
  [35]: l35
  [36]: l36
  [37]: l37
  [38]: l38

  

")

