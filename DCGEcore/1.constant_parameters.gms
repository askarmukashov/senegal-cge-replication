$gdxin "tmp/maindata.gdx"
$load AC A ARG C F FLAB FCAP FLND RG INS INSD INSDNG EN H CTD CTE CTM MARG LESELAS1 FRISCH TRADELAS PRODELAS SAM ELASCES

SET
 HrDT(H)  households DT
 HrNO(H)  households NO
 HrCE(H)  households CE
 HrSO(H)  households SO
;


PARAMETER
 FIXED(*) fixed  parameters
   labforce(FLAB,*)
    fprdgr(F, A, RG, MM_T)    growth of fprd
    PARAMETERS
     pdwt(C)                         weight of commodity c in PD index
     pwewt(C)                        weight of commodity c in pwe index
     pwmwt(C)                        weight of commodity c in pwm index
     pwwt(*)                         weight of aggregate exports-imports in pw (tradables) index
     PDIND(MM_T) domestic producers price indexed
     PWEIND(MM_T) export price index
     PWMIND(MM_T) import price index
     TOFT(MM_T) terms of trade index
     PWIND(MM_T) world market prices index
     REXR(MM_T) real exchange rate
     QDSTX(C, MM_T) stocks

;


AAGR('acrop') =  YES;
AAGR('aoagr') =  YES;

AAGRPR('acprc')=  YES;
AAGRPR('aoprc')=  YES;


AOIND('aolig') =  YES;
AOIND('aoman') =  YES;
AOIND('autil') =  YES;
AOIND('acons') =  YES;



APSERV('atrad') =  YES;
APSERV('atran') =  YES;
APSERV('aacfo') =  YES;
APSERV('acomm') =  YES;
APSERV('afsrv') =  YES;
APSERV('areal') =  YES;
APSERV('absrv') =  YES;
APSERV('aeduc') =  YES;
APSERV('aosrv') =  YES;

CAGR('ccrop') =  YES;
CAGR('coagr') =  YES;



ROW(INS)=  YES;
ROW(INSD)=  NO;

GOV(INSD)=  YES;
GOV(INSDNG)=  NO;
CPUB(C)$SUM(GOV, SAM(C, GOV)) = yes;


 MAC(A,C)$SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,C)) = yes;

*Regional activity CES aggregation function?
 ACES2(A)               =  YES;

*CET disaggregation function in the make matrix?
 ACET2(A)               = YES;

 ACNT(AC)               = YES;
 ACNT('TOTAL')          = NO;

 AFLEO(A)               = NO;

  DOMX(C) = SUM(ARG, SAM(ARG,C));
  IMPX(C) = SAM('ROW',C);
  m(C)$(IMPX(C) + DOMX(C))  = SAM('TRC',C)/(IMPX(C) + DOMX(C));

* net of trc
  EXPX(C)$m(c) = SAM(C,'ROW')/(1 + m(c));
  DOMSALX(C) = DOMX(C) - EXPX(C);

  SAM(CTE,C)$EXPX(C) = EXPX(C) * m(C);
  SAM(CTM,C)$IMPX(C) = IMPX(C) * m(C);
  SAM(CTD,C)$DOMSALX(C) = DOMSALX(C) * m(C);

  TRSHR(C)$SUM(CP, SAM(CP,'TRC')) = SAM(C,'TRC')/SUM(CP, SAM(CP,'TRC'));

  SAM(C,CTE) = TRSHR(C) * SUM(CP, SAM(CTE,CP));
  SAM(C,CTM) = TRSHR(C) * SUM(CP, SAM(CTM,CP));
  SAM(C,CTD) = TRSHR(C) * SUM(CP, SAM(CTD,CP));

  SAM('TRC',AC) = 0;
  SAM(AC,'TRC') = 0;

*Account totals are recomputed. Check for SAM balance.
  SAM(AC,ACP) =  SAM(AC,ACP) * SCALE;

  SAM('TOTAL',AC) = 0;
  SAM(AC,'TOTAL') = 0;
  SAM('TOTAL',AC) = SUM(ACNT, SAM(ACNT,AC));
  SAM(AC,'TOTAL') = SUM(ACNT, SAM(AC,ACNT));
  SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');
  SAMBALCHK(AC)$(abs( SAMBALCHK(AC)) lt 1e-6) = 0;

 ACHK(A)$(SUM((ARG,RG)$MARG(ARG,A,RG), SAM(ARG,'TOTAL')) EQ 0) = YES;
 FCHK(F)$(SAM(F,'TOTAL') EQ 0) = YES;
 HCHK(H)$(SAM(H,'TOTAL') EQ 0) = YES;
 CCHK(C)$(SAM(C,'TOTAL') EQ 0) = YES;

*doesnt matter
  ELASAC(C)         = 10.0;
*doesnt matter
  ELASCA(A)     = 10;

  LESELAS2(A,H)   =  1;


*Default: SAM value
 QFBASE(F,ARG) = SAM(F,ARG);
 QFSBASE(F) = SUM(ARG, QFBASE(F,ARG));
* change for labor
$load labforce
 QFSBASE(FLAB) = labforce(FLAB, 'thousands');
 QFBASE(FLAB,ARG) = SAM(FLAB,ARG)/SUM(ARGP, SAM(FLAB,ARGP))*labforce(FLAB, 'thousands');


$load POPTAB
 hpop0(H) = POPTAB(H,'survey');
 hpop(H) = hpop0(H);


*If needed, manually define shrhome.
 shrhome(A,H) = 0;


*direct taxes on domestic institutions
 TAXPAR('INSTAX',INSD)  = SAM('DTAX',INSD);
*direct factor taxes
 TAXPAR('FACTAX',F)     = SAM('FTAX',F);
*import taxes
 TAXPAR('IMPTAX',C)     = SAM('MTAX',C);
*export taxes
 TAXPAR('EXPTAX',C)     = 0;
*value-added taxes
 TAXPAR('VATAX',ARG)    = 0;
*taxes on activity revenue
 TAXPAR('ACTTAX',ARG)   = SAM('stax',ARG);
*taxes on commodity sales in domestic market
 TAXPAR('COMTAX',C)     = SAM('STAX',C);

   


*SAM adjustments ----------------------------------------------

*Adjustment for sectors with only exports and no domestic sales.
*If there is a very small value for domestic sales, add the discrepancy
*to exports.

 SAM(C,'ROW')$(ABS(SUM(ARG, SAM(ARG,C)) - (SAM(C,'ROW') - TAXPAR('EXPTAX',C)
                                    - SUM(CTE, SAM(CTE,C))) ) LT 1.E-6)
                 = SUM(ARG, SAM(ARG,C)) +  TAXPAR('EXPTAX',C)
                                    + SUM(CTE, SAM(CTE,C)) ;

*Netting transfers between domestic institutions and RoW.
 SAM(INSD,'ROW')   = SAM(INSD,'ROW') - SAM('ROW',INSD);
 SAM('ROW',INSD)   = 0;

*Netting transfers between factors and RoW.
 SAM('ROW',F)  = SAM('ROW',F) - SAM(F,'ROW');
 SAM(F,'ROW')  = 0;

*Netting transfers between government and domestic non-
*government institutions.
 SAM(INSDNG,'GOV') = SAM(INSDNG,'GOV') - SAM('GOV',INSDNG);
 SAM('GOV',INSDNG) = 0;

*Eliminating payments of any account to itself.
 SAM(ACNT,ACNT) = 0;

*Checking SAM balance -----------------------------------------

*Account totals are recomputed. Check for SAM balance.
 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');
 SAMBALCHK(AC)$(abs( SAMBALCHK(AC)) lt 1e-6) = 0;

*Additional set definitions based on country SAM======================

 CD(C)  = YES$(SUM(ARG, SAM(ARG,C)) GT (SAM(C,'ROW') - TAXPAR('EXPTAX',C)
                                                 - SUM(CTE, SAM(CTE,C))) );

 CDN(C) = NOT CD(C);

 CE(C)  = YES$(SAM(C,'ROW'));
 CEN(C) = NOT CE(C);

 CM(C)  = YES$(SAM('ROW',C));
 CMN(C) = NOT CM(C);

 CX(C) = YES$SUM(ARG, SAM(ARG,C));

 CT(C)$(SUM(CTD, SAM(C,CTD)) + SUM(CTE, SAM(C,CTE)) + SUM(CTM, SAM(C,CTM))) = YES;

*Physical factor quantities -----------------------------------

*If there is a SAM payment from A to F and supply (but not
*demand) quantities have been defined in the country data file,
*then the supply values are used to compute demand quantities.
 QF2BASE(F,ARG)$(SAM(F,ARG)$((NOT QFBASE(F,ARG)) AND QFSBASE(F)))
   = QFSBASE(F)*SAM(F,ARG)/SUM(ARGP, SAM(F,ARGP));

*If there is a SAM payment from A to F and neither supply nor
*demand quantities have been defined in the country data file,
*then SAM values are used as quantities
 QF2BASE(F,ARG)$(SAM(F,ARG)$((QFBASE(F,ARG) EQ 0) AND (QFSBASE(F) EQ 0)))
                                                    = SAM(F,ARG);

*If there is a SAM payment from A to F and demand quantities have
*been defined in the country data file, then this information is used.
 QF2BASE(F,ARG)$QFBASE(F,ARG) = QFBASE(F,ARG);

*3. PARAMETER DECLARATIONS ##########################################
$ontext

This section is divided into the following subsections:
a. Parameters appearing in model equations
b. Parameters used for model calibration (to initialize variables and
   to define model parameters)

In each group, the parameters are declared in alphabetical order.

$offtext
*Price block --------------------------------------------------
 PSUP(C)             = 1;
 PE0(C)$CE(C)        = PSUP(C);
 PX0(C)$CX(C)        = PSUP(C);
 PDS0(C)$CD(C)       = PSUP(C);
 PXAC0(A,C)$SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,C)) = PSUP(C);
 PA0(A)              = 1;
 PAR0(A,RG)          = 1;
 EXR0                = 1;


*Activity quantity = payment to activity divided by activity price
*QA covers both on-farm consumption and marketed output output GROSS of tax
 QAR0(A,RG)        =  SUM(ARG$MARG(ARG,A,RG), SAM('TOTAL',ARG))/PAR0(A,RG) ;
 QA0(A)            =  SUM(RG, QAR0(A,RG));

*Unit value-added price = total value-added / activity quantity define pva gross of tax
 QVA0(A,RG)              = SUM(ARG$MARG(ARG,A,RG), SUM(F, SAM(F,ARG))+ TAXPAR('VATAX',ARG)) ;
 PVA0(A,RG)$QVA0(A,RG)   = SUM(ARG$MARG(ARG,A,RG), SUM(F, SAM(F,ARG))+ TAXPAR('VATAX',ARG))/QVA0(A,RG);
 iva(A,RG)$QAR0(A,RG)    = QVA0(A,RG)/QAR0(A,RG) ;
 QXAC0(A,C)$PXAC0(A,C)   =  SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,C)) / PXAC0(A,C);
 QHA0(A,H)         = SUM((ARG,RG)$MARG(ARG,A,RG),SAM(ARG,H))/PA0(A);
 QANET0(A)         = QA0(A) - SUM(H, QHA0(A,H)) ;

*Output quantity = value received by producers divided by producer price
*QX covers only marketed output
 QX0(C)$SUM(ARG, SAM(ARG,C)) = SUM(ARG, SAM(ARG,C)) / PX0(C);

*Export quantity = export revenue received by producers
*(ie. minus tax and transactions cost) divided by export price.
 QE0(C)$SAM(C,'ROW') = (SAM(C,'ROW') - TAXPAR('EXPTAX',C)
                                     - SUM(CTE, SAM(CTE,C)))/PE0(C);

*RoW export price = RoW export payment (in for curr) / export qnty
 pwe0(C)$QE0(C) = (SAM(C,'ROW')/EXR0) / QE0(C);
 pwe(C) = pwe0(C);

 te0(C)$SAM(C,'ROW') = TAXPAR('EXPTAX',C)/SAM(C,'ROW');
 te(C) =  te0(C);

*Quantity of output sold domestically = output quantity less quantity
*exported = value of domestic sales divided by domestic supply price
*QD0 covers only marketed output
 QD0(C)$CD(C) =  QX0(C) - QE0(C);

*Domestic demander price = demander payment divided by quantity bought
 PDD0(C)$QD0(C)= (PDS0(C)*QD0(C) + SUM(CTD, SAM(CTD,C)))/QD0(C);

*Define import price to equal domestic price so that import and domestic
*units are the same to the purchaser. If no domestic good, set PM to 1.
* PM0(C) = 1 ;
 PM0(C) = PDD0(C) ;
 PM0(C)$(QD0(C) EQ 0) = 1 ;

*Import quantity = demander payment for imports (including tariffs
*and marketing cost) divided by demander price.
 QM0(C)$CM(C) = (SAM('ROW',C) + TAXPAR('IMPTAX',C) +
                 SUM(CTM, SAM(CTM,C)))/PM0(C);

*World price = import value (in foreign currency / import quantity
 pwm0(C)$QM0(C)= (SAM('ROW',C)/EXR0) / QM0(C);
 pwm(C) = pwm0(C);
 tm0(C)$SAM('ROW',C) = TAXPAR('IMPTAX',C) / SAM('ROW',C);
 tm(C) = tm0(C);

*Composite supply is the sum of domestic market sales and imports
*(since they are initialized at the same price).
 QQ0(C)$(CD(C) OR CM(C)) = QD0(C) + QM0(C) ;
 PQ0(C)$QQ0(C) = (SAM(C,'TOTAL') - SAM(C,'ROW'))/QQ0(C);
 tq0(C)$QQ0(C) = TAXPAR('COMTAX',C)/(PQ0(C)*QQ0(C)) ;

 SHCTD(CT)$SUM(CTD, SAM('TOTAL',CTD)) = SUM(CTD, SAM(CT,CTD)/SAM('TOTAL',CTD)) ;
 SHCTM(CT)$SUM(CTM, SAM('TOTAL',CTM)) = SUM(CTM, SAM(CT,CTM)/SAM('TOTAL',CTM)) ;
 SHCTE(CT)$SUM(CTE, SAM('TOTAL',CTE)) = SUM(CTE, SAM(CT,CTE)/SAM('TOTAL',CTE)) ;

*Transactions input coefficients
 icd(CT,C)$QD0(c)
   = (shctd(ct)*SUM(CTD, SAM(CTD,C))/PQ0(ct)) / QD0(C);
 icm(CT,C)$QM0(C)
  = (shctm(ct)*SUM(CTM, SAM(CTM,C))/PQ0(ct)) / QM0(C);
 ice(CT,C)$QE0(C)
  = (shcte(ct)*SUM(CTE, SAM(CTE,C))/PQ0(ct)) / QE0(C);

*Indirect activity tax rate = tax payment / output value
*Tax is here applied to total output value (incl. on-farm cons.)
 tva0(A,RG)$QVA0(A,RG) = SUM(ARG$MARG(ARG,A,RG),TAXPAR('VATAX',ARG)) /
                         (PVA0(A,RG)*QVA0(A,RG));
 tva(A,RG) = tva0(A,RG);

*QA is GROSS of tax, so base for ta is as well
 ta0(A,RG)$QAR0(A,RG) = SUM(ARG$MARG(ARG,A,RG), TAXPAR('ACTTAX',ARG) /
                         (SAM(ARG,'TOTAL')));

*Yield coefficient = quantity produced and delivered to market.
*Home consumption is assumed to come from activities
 theta(A,C)$PXAC0(A,C)
  =  QXAC0(A,C)/(QA0(A)- SUM(H,QHA0(A,H))) ;

*Intermediate input coefficient = input use / output quantity
 QINTA0(A,RG) = SUM(C$PQ0(C), SUM(ARG$MARG(ARG,A,RG), SAM(C,ARG))  / PQ0(C)) ;

 ica(C,A,RG)$(QINTA0(A,RG) AND PQ0(C))
                = SUM(ARG$MARG(ARG,A,RG), SAM(C,ARG))/PQ0(C) / QINTA0(A,RG) ;

 inta(A,RG)$QAR0(A,RG) = QINTA0(A,RG) / QAR0(A,RG) ;
 pinta0(A,RG)      = SUM(C, ica(C,A,RG)*PQ0(C)) ;

*CPI weight by comm'y = hhd cons value for comm'y / total hhd cons value
*CPI does not consider on-farm consumption.
 cwts(C) = SUM(H, SAM(C,H)) / SUM((CP,H), SAM(CP,H));

*CPI weights by household
 cwtsh(c,h)$sam(c,h)    = sam(c,h)/(SUM(cp, sam(cp,h)) + SUM(a, QHA0(A,H)*PA0(A)));
 cwtsh(a,h)$QHA0(A,H)    = QHA0(A,H)*PA0(A)/(SUM(cp, sam(cp,h)) + SUM(ap, QHA0(ap,H)*PA0(ap)));
 cwtshchk(h)   = SUM(acnt, cwtsh(acnt,h)) - 1 ;

*Domestic sales price index weight = dom sales value for comm'y
*/ total domestic salues value
*Domestic sales price index does not consider on-farm consumption.
 dwts(C)       = (SUM(ARG, SAM(ARG,C)) - (SAM(C,'ROW') -
                  SUM(cte, SAM(cte,C))))/
                  SUM(CP, SUM(ARG, SAM(ARG,CP)) - (SAM(CP,'ROW') -
                  SUM(cte, SAM(cte,CP))));

 CWTSCHK       = SUM(C, cwts(C)) - 1;
 DWTSCHK       = SUM(C, dwts(C)) - 1;

 CPI0          = SUM(C, cwts(C)*PQ0(C)) ;
 DPI0          = SUM(CD$(NOT CERES(CD)), dwts(CD)*PDS0(CD)) ;

*Demand computations ----

*Defining factor employment and supply.
 QF0(F,A,RG)  = SUM(ARG$MARG(ARG,A,RG),QF2BASE(F,ARG));
*Defining employment for aggregate factors in factor nesting
 QFS0(F)      = SUM((A,RG), QF0(F,A,RG));

*Activity-specific wage is activity labor payment over employment
 WFA(F,A,RG)$QF0(F,A,RG) = SUM(ARG$MARG(ARG,A,RG),SAM(F,ARG))/QF0(F,A,RG);

*Economy-wide wage average is total factor income over employment
 WF0(F)$SUM((A,RG), QF0(F,A,RG))   = SUM(ARG, SAM(F,ARG))/SUM((A,RG), QF0(F,A,RG));

DISPLAY
"If the value of WF0 for any factor is very different from one (< 0.1"
"or >10) the user may consider rescaling the initial values for QFBASE"
"or QFSBASE for this factor to get a value of WF0 such that"
"0.1 < WF0 < 10"
 WF0
 ;

*Wage distortion factor
 wfdist0(F,A,RG)$WF0(F) = WFA(F,A,RG)/WF0(F);

 ifa(F,A,RG)$QVA0(A,RG) = QF0(F,A,RG) / QVA0(A,RG);

*Intermediate demand
 QINT0(C,A)$PQ0(C) = SUM((ARG,RG)$MARG(ARG,A,RG),SAM(C,ARG)) / PQ0(C);

*Transactions demand
 QT0(CT) = ( SUM(CTD, SAM(CT,CTD)) + SUM(CTE, SAM(CT,CTE))
           + SUM(CTM, SAM(CT,CTM)) ) / PQ0(CT) ;

*Institution block --------------------------------------------

*Institutional income
 YI0(INSDNG) = SAM('TOTAL',INSDNG);

*Factor income by factor category
 YF0(F)      = SUM(ARG, SAM(F,ARG));

*Institution income from factors
 YIF0(INSD,F) = SAM(INSD,F);

*Transfers to RoW from factors
 trnsfr('ROW',F) = SAM('ROW',F)/EXR0;
 rf(f) = 0;

*Transfers from RoW to institutions
 trnsfr(INSD,'ROW') = SAM(INSD,'ROW')/EXR0;

*Government transfers
 trnsfr(INSD,'GOV') = SAM(INSD,'GOV')/CPI0;

*Factor taxes
 tf0(F)$SAM('TOTAL',F) = TAXPAR('FACTAX',F)/SAM('TOTAL',F);
 tf(F)                 = tf0(F);

*Shares of domestic institutions in factor income (net of factor taxes and transfers to RoW).
 shif(INSD,F)$SAM(F,'TOTAL')  = SAM(INSD,F)/(SAM(F,'TOTAL') - TAXPAR('FACTAX',F) - SAM('ROW',F));

 SHIFCHK(F)    = SUM(INSD, shif(INSD,F)) - 1 ;

*Inter-institution transfers
 TRII0(INSDNG,INSDNGP) = SAM(INSDNG,INSDNGP);

*Share of dom non-gov institution in income of other dom non-gov institutions (net of direct taxes and savings).
 shii(INSDNG,INSDNGP)$SAM(INSDNG,INSDNGP) = SAM(INSDNG,INSDNGP)
   /(SAM('TOTAL',INSDNGP) - TAXPAR('INSTAX',INSDNGP) - SAM('S-I',INSDNGP));

*Scaling factors for savings and direct tax shares
 MPSADJ0      = 0;
 TINSADJ0     = 0;

*Savings rates
 MPS0(INSDNG)$SAM('S-I',INSDNG)   = SAM('S-I',INSDNG)/(SAM('TOTAL',INSDNG) - TAXPAR('INSTAX',INSDNG));
 mpsbar(INSDNG) = MPS0(INSDNG);

*Direct tax rates
 TINS0(INSDNG)$TAXPAR('INSTAX',INSDNG)   = TAXPAR('INSTAX',INSDNG) / SAM('TOTAL',INSDNG);
 tinsbar(INSDNG) = TINS0(INSDNG);

*"Point" change in savings and direct tax shares
 DMPS0  = 0;
 DTINS0 = 0;

*Selecting institutions for potential "point" change in savings and tax rates

*If DMPS or MPSADJ is flexible, institutions with a value of 1 for mps01
*change their savings rates.
 mps01(EN)  = 1;

*If DTIMS is flexible, institutions with a value of 1 for tins01 change
*their savings rates.
 tins01(EN) = 1;

*Household consumption spending and consumption quantities.
 EH0(H)         = SUM(C, SAM(C,H)) + SUM(A, QHA0(A,H)*PA0(A));
 QH0(C,H)$PQ0(C) = SAM(C,H)/PQ0(C);

*Government indicators
 YG0           = SAM('TOTAL','GOV');
 EG0           = SAM('TOTAL','GOV') - SAM('S-I','GOV');
 QG0(C)$PQ0(C) = SAM(C,'GOV')/PQ0(C);

 qbarg0(C)     = QG0(C);
 qbarg(C)      = qbarg0(C);
 GADJ0         = 1;
 GSAV0         = SAM('S-I','GOV');

*Fixed investment
 qbarinv(C)$PQ0(C) = SAM(C,'S-I')/PQ0(C);
 QINV0(C)          = qbarinv(C);
 IADJ0             = 1;

*investment demand
 iwts(C)   = qbarinv(C) / SUM(CP, qbarinv(CP));

*Stock changes
 qdst0(C)$PQ0(C) = SAM(C,'DSTK')/PQ0(C);
 qdst(C)         = qdst0(C);

 FSAV0         = SAM('S-I','ROW')/EXR0;

 TABS0         = SUM((C,H), SAM(C,H)) + SUM((A,H), QHA0(A,H)*PA0(A))
                 + SUM(C, SAM(C,'GOV')) + SUM(C, SAM(C,'S-I'))
                 + SUM(C, SAM(C,'DSTK'));

 INVSHR0       = SAM('TOTAL','S-I')/TABS0;
 GOVSHR0       = SUM(C, SAM(C,'GOV'))/TABS0;

 WALRAS0       = 0;

 trnsfr0(INS,AC) = trnsfr(INS,AC);



*--------------- price index
*Price indices ------------------------------

 pdwt(C)  = PDD0(C)*QD0(C)/SUM(CP, PDD0(CP)*QD0(CP));

 pwewt(C) = pwe0(C)*QE0(C)/SUM(CP, pwe0(CP)*QE0(CP));

 pwmwt(C) = pwm0(C)*QM0(C)/SUM(CP, pwm0(CP)*QM0(CP));

 pwwt('EXP') = SUM(CP, pwe0(CP)*QE0(CP))  /
 (SUM(CP, pwe0(CP)*QE0(CP)) + SUM(CP, pwm0(CP)*QM0(CP)));

 pwwt('IMP')
  = SUM(CP, pwm0(CP)*QM0(CP))/
  (SUM(CP, pwe0(CP)*QE0(CP)) + SUM(CP, pwm0(CP)*QM0(CP)));



** capital
SETS
 AFX(A,RG)               sectors with calibrated capital stock growth
 ANFX(A,RG)              sectors without calibrated capital stock growth
;
PARAMETERS
 AFXGR(A,RG)         growth rate for sectors with fixed resources
;
* current inputs
 AFX(A,RG) =  NO;
 ANFX(A,RG) = NO;



******************** Askar regional mobility module

***** to distiribute
SET
  facnational                       national factor types
  flabnational(facnational)         national labor types
  fcapnational(facnational)         national capital types
  flab_map(facnational, FLAB, RG)   mapping - labor
  fcap_map(facnational, FCAP, RG)   mapping - capital
  ins_reg(INSDNG, RG)               mapping between ins and regions
  fac_reg(F, RG)                    mapping between factors and regions
  type_nat(facnational)             current type - national (loop)

  native_cap(INSDNG)                    native cap owners (loop)
  foreign_cap(INSDNG)                   foreign cap owners (loop)


  native(H)                    native flab owners (loop)
  foreign(H)                   foreign flab owners (loop)

  emigr_immigr(INSDNG, INSDNGP,RG)
  immigr_orig_FLAB(H, FLAB)
  host_orig_FLAB(H, FLAB)
  immigr_dest(H, HP)
  immigr_H(H)  immigrants to active region
  act(RG)
natH(H)
  migrh(H)
  emigr_assest(F,FP)
;

$LOAD  natH migrh ins_reg facnational fcapnational  flabnational flab_map fcap_map  emigr_immigr  immigr_orig_FLAB  immigr_dest host_orig_FLAB emigr_assest
ALIAS(foreign,foreignP);
ALIAS(foreign_cap,foreign_capP);

PARAMETER
    share_emigr_of_mob_flab(INSDNG) share of origin
    share_emigr_of_mob_flab2(FLABP) share of origin
    share_emigr_of_tot(INSDNG)
        share_emigr_of_tot2(INSDNG)
        prd_emigr(F) Productivity of emigrated relative to remained

    QFSwithout(F)          without mobility - current
    QFSwithoutX(F,MM_T)    without mobility - years
    QFSmigrpercX(F,MM_T)       net migr stock perc

    hpop_without(H)          without mobility

    net_balance(F)   net_balance btw  QFSwithout and QFS_with
    net_balanceX(F, MM_T)
    ownership(INSDNG)                       current quant (loop)
    foreigners_assets(INSDNG)               current foreigners (loop)

    dist(INSDNG)                            distirubtion of positive net_balance among foregners (loop)
    shifX(INSDNG,F,MM_T)                    share of dom. inst on i in income of factor f
    shif0(INSDNG,F)
        shif_pre(INSDNG,F)
    BUD(H)                                  available budget for new households
;
$LOAD fac_reg
shif0(INSDNG,F) = shif(INSDNG,F);

$load FIXED

SET
  h_flab0(H,FLAB)
;

h_flab0(H,FLAB)$SAM(H,FLAB) = YES;

hpopgr(H, MM_T)$ins_reg(H,'DT')    =                FIXED('hpopgr_DT')/100;
hpopgr(H, MM_T)$ins_reg(H,'NO')    =                FIXED('hpopgr_NO')/100;
hpopgr(H, MM_T)$ins_reg(H,'CE')    =                FIXED('hpopgr_CE')/100;
hpopgr(H, MM_T)$ins_reg(H,'SO')    =                FIXED('hpopgr_SO')/100;


 QFSgr(FLAB, MM_T)$SUM(flabnational, flab_map(flabnational,FLAB,'DT')) =   FIXED('hpopgr_DT')/100;
 QFSgr(FLAB, MM_T)$SUM(flabnational, flab_map(flabnational,FLAB,'NO'))   =  FIXED('hpopgr_NO')/100;
 QFSgr(FLAB, MM_T)$SUM(flabnational, flab_map(flabnational,FLAB,'CE'))   =  FIXED('hpopgr_CE')/100;
 QFSgr(FLAB, MM_T)$SUM(flabnational, flab_map(flabnational,FLAB,'SO'))   =  FIXED('hpopgr_SO')/100;

QFSgr(FLND, MM_T)  =                 FIXED('flnd')/100;
accrate            =                 FIXED('fcap_nat')/100;
natdrate           =                 0;
FSAVgr(MM_T)                 =       FIXED('fcap_nat')/100;
alphavagr(A, RG, MM_T)       =      FIXED('tfp_all')/100;


PARAMETER
 qfs_nat0(flabnational)                total  supply
* up nest CET-based defining quant in REGION
 w_nat(flabnational)           ACET elast of transformation of national to regions
 v_nat(flabnational)           ACET exponent of transformation of national to regions
 alpha_nat(flabnational)  shift par
 share_nat(flabnational, RG) share par
 qfs_rg0(flabnational, RG)  quantity to REGION - for initial calibration
 qfs_rg_cal(flabnational, RG)
 p_rg0(flabnational, RG)
 P_ind0(flabnational)
*** for reporting
 QFS_rgX(flabnational, RG, MM_T)
 P_rgX(flabnational, RG, MM_T)
 P_indX(flabnational, MM_T)
  QFSX_flab(FLAB,MM_T)                     qnty of factor supply
   Pc_cons(H)
   Pc_consX(H, MM_T)
   Tot_cons(H)
      Tot_consX(H,MM_T)
   Pc_inc(H)

   Pc_incX(H,MM_T)
;

VARIABLES
qfs_nat(flabnational)                total  supply
QFS_rg(flabnational, RG)
p_rg(flabnational, RG)
P_ind(flabnational)
;

EQUATIONS
  constraint_def
  QFS_rg_def
  QFS_def2
  p_rg_def
;

 constraint_def(flabnational)$w_nat(flabnational)..
  qfs_nat(flabnational) =E= SUM(RG, QFS_rg(flabnational, RG)) ;

 QFS_rg_def(flabnational, RG)$(w_nat(flabnational) and QFS_rg0(flabnational, RG))..
 QFS_rg(flabnational, RG) =E= qfs_nat(flabnational) *
 (p_rg(flabnational, RG)/(P_ind(flabnational)*share_nat(flabnational,RG)*alpha_nat(flabnational)**v_nat(flabnational)))**(1/(v_nat(flabnational)-1)) ;

 QFS_def2(FLAB)$SUM((flabnational,RG), qfs_rg0(flabnational,RG)$flab_map(flabnational, FLAB, RG))..
  QFS(FLAB) =E=
   SUM((flabnational,RG), QFS_rg(flabnational,RG)$flab_map(flabnational, FLAB, RG))
  ;

 p_rg_def(flabnational, RG)$p_rg0(flabnational, RG)..
 p_rg(flabnational, RG)   =E=
  SUM(FLAB, wf(FLAB)$flab_map(flabnational, FLAB, RG));

MODEL CET_macro   /
STANDCGE,
QFS_rg_def,
constraint_def,
QFS_def2,
p_rg_def

/;
 CET_macro.HOLDFIXED   = 1 ;
 CET_macro.TOLINFREP   = .001 ;
 CET_macro.MODELSTAT = 0;
 CET_macro.SOLVESTAT = 0;
 CET_macro.NUMREDEF  = 0;
option iterlim = 100;
 hpop_without(H)=hpop(H);

PARAMETERS
 facVAsh(F,A,RG)
 facVAshX(F,A,RG,MM_T)
  ;
        fprd_adj(F) =1;
