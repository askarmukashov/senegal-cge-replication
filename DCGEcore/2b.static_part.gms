$ontext
$ONSYMLIST ONSYMXREF OFFUPPER
$ONEMPTY
$offlisting
$offsymxref offsymlist
option
    limrow = 0,
    limcol = 0,
    solprint = off,
    sysout = off;
OPTION SOLVELINK=5;
$offtext
*Production and trade block==========================
*Compute exponents from elasticites
 rhoq(C)$(CM(C) AND CD(C)) = (1/TRADELAS(C,'SIGMAQ')) - 1;
 rhot(C)$(CE(C) AND CD(C)) = (1/TRADELAS(C,'SIGMAT')) + 1;
 rhova(A,RG)$SUM(ARG$MARG(ARG,A,RG),PRODELAS(ARG))
               = SUM(ARG$MARG(ARG,A,RG),(1/PRODELAS(ARG)) - 1);
*Add elasticity of substitution in the CES for the cost function
 elasva(A,RG)  = 1/(1 + rhova(A,RG)) ;

*Aggregation of domestic output from different activities

 RHOAC(C)$ELASAC(C) = 1/ELASAC(C) - 1;

 deltaac(A,C)$ (QXAC0(A,C)$ELASAC(C))
               = (PXAC0(A,C)*QXAC0(A,C)**(1/ELASAC(C)))/
                 SUM(AP, PXAC0(AP,C)*QXAC0(AP,C)**(1/ELASAC(C)));

 alphaac(C)$SUM(A,deltaac(A,C))
               = QX0(C)/
                 (SUM(A$deltaac(A,C), deltaac(A,C) * QXAC0(A,C)
                 **(-RHOAC(C))) )**(-1/RHOAC(C));

*CET disaggregation function (multiple outputs from same activity)
 rhoca(A)$ELASCA(A)  = (1/ELASCA(A)) + 1 ;
 deltaca(A,C)$QXAC0(A,C) =
    (PXAC0(A,C)*(QXAC0(A,C))**(1-rhoca(A)))/
    SUM(CP$QXAC0(A,CP), PXAC0(A,CP)*(QXAC0(A,CP))**(1-rhoca(A))) ;
 alphaca(A)$(NOT ACHK(A)) = (QA0(A) - SUM(H, QHA0(A,H)))
         /(SUM(C$deltaca(A,C),deltaca(A,C)*QXAC0(A,C)**rhoca(A)))**(1/rhoca(A));

*CES activity production function
 deltava(F,A,RG)$QF0(F,A,RG) = (wfdist0(F,A,RG) * WF0(F) * (QF0(F,A,RG))**(1+rhova(A,RG)) )
              / SUM(FP$QF0(FP,A,RG), wfdist0(FP,A,RG) * WF0(FP)*(QF0(FP,A,RG))**(1+rhova(A,RG)));

 alphava0(A,RG)$QVA0(A,RG) = QVA0(A,RG)/( SUM(F$(QF0(F,A,RG)), deltava(F,A,RG)*QF0(F,A,RG)
               **(-rhova(A,RG))) )**(-1/rhova(A,RG));
 alphava(A,RG) = alphava0(A,RG);

 fprd(F,A,RG) = 1;

 rhoa2(A)$ELASCES(A) = (1/ELASCES(A))-1;

 deltaa2(A,RG)$QAR0(A,RG)
    = (PAR0(A,RG)*(QAR0(A,RG))**(1+rhoa2(A)))/SUM(RGP, PAR0(A,RGP)*(QAR0(A,RGP))**(1+rhoa2(A)));

 alphaa2(A)$(NOT ACHK(A))
     = QA0(A)/(SUM(RG$QAR0(A,RG), deltaa2(A,RG)*QAR0(A,RG)**(-rhoa2(A))))**(-1/rhoa2(A));

 rhoca(A)  = (1/elasca(A)) + 1 ;

 deltaca(A,C)$QXAC0(A,C) =
    (PXAC0(A,C)*(QXAC0(A,C))**(1-rhoca(A)))/
    SUM(CP$QXAC0(A,CP), PXAC0(A,CP)*(QXAC0(A,CP))**(1-rhoca(A))) ;

*CET transformation
 deltat(C)$(CE(C) AND CD(C))
   = 1 / (1 + PDS0(C)/PE0(C)*(QE0(C)/QD0(C))**(rhot(C)-1));

 alphat(C)$(CE(C) AND CD(C))
   = QX0(C) / (deltat(C)*QE0(C)**rhot(C) + (1-deltat(C))
                 *QD0(C)**rhot(C))**(1/rhot(C));

*Armington aggregation

 predelta(C)$(CM(C) AND CD(C))
   = (PM0(C)/(PDD0(C)))*(QM0(C)/QD0(C))**(1+rhoq(C)) ;

 deltaq(C)$(CM(C) AND CD(C))
   = predelta(C)/(1 + predelta(C)) ;

 alphaq(C)$(CM(C) AND CD(C))
               = QQ0(C)/(deltaq(C)*QM0(C)**(-rhoq(C))
                 +(1-deltaq(C))*QD0(C)**(-rhoq(C)))**(-1/rhoq(C)) ;

*LES calibration ----------------------------------------------

 BUDSHR(C,H)$SAM(C,H)    = SAM(C,H)/(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
 BUDSHR2(A,H)$QHA0(A,H)   = QHA0(A,H)*PA0(A)
                  /(SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP)));
 BUDSHRCHK(H)   = SUM(C, BUDSHR(C,H)) + SUM(A, BUDSHR2(A,H)) - 1 ;
 ELASCHK(H)     = SUM(C, BUDSHR(C,H)*LESELAS1(C,H))
                  + SUM(A, BUDSHR2(A,H)*LESELAS2(A,H)) - 1 ;

*Correct elasticities to make them satisfy Engle aggregation exactly
 LESELAS1(C,H)$SAM(C,H)   = LESELAS1(C,H)/(ELASCHK(H) + 1);
 LESELAS2(A,H)$QHA0(A,H)   = LESELAS2(A,H)/(ELASCHK(H) + 1);

*Check Engle aggregation again
 ELASCHK(H)      = SUM(C, BUDSHR(C,H)*LESELAS1(C,H)) +
                   SUM(A, BUDSHR2(A,H)*LESELAS2(A,H)) - 1;

 betam(C,H)   = BUDSHR(C,H)*LESELAS1(C,H);
 betah(A,H)   = BUDSHR2(A,H)*LESELAS2(A,H);

 gammam0(C,H)$BUDSHR(C,H)
     =  ( (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PQ0(C) )
                      * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));

 gammah0(A,H)$BUDSHR2(A,H)
     =  ( (SUM(CP, SAM(CP,H)) + SUM(AP, QHA0(AP,H)*PA0(AP))) / PA0(A) )
                      * ( BUDSHR2(A,H) + betah(A,H)/FRISCH(H));

 gammam(C,H)   =  gammam0(C,H);

 gammah(A,H)   =  gammah0(A,H);

*Checking LES parameters===================================

 SUPERNUM(H)  = SUM(A, gammah(A,H)*PA0(A))
                + SUM(C, gammam(C,H)*PQ0(C)) ;
 FRISCH2(H)$EH0(H)   = -EH0(H)/(EH0(H) - SUPERNUM(H));
 LESCHK(H)$(ABS(FRISCH(H) - FRISCH2(H)) GT 0.00000001) = 1/0;

*Set proper units of capital in the XXX.DAT file
*Capital stock calibration for recursive investment-based updating
 QINVK = (natdrate + accrate) * SUM(FCAP, QFSBASE(FCAP));
 alphainv  = ((natdrate + accrate) * SUM(FCAP, QFSBASE(FCAP))) / PROD(C, QINV0(C)**iwts(C));

*Diagnostics on CES and CET function share parameters
*   Test for small values of CES and CET function parameters in production
*   and trade aggregation.  DELTAVA(A), DELTAA(A), DELTAM(C,R), DELTAE(C,R)
*   DELTAT(C), DELTAQ(C)

 DELTATEST1(F,A,RG)$(deltava(F,A,RG) LT 1e-4) = deltava(F,A,RG);
 DELTATESTME(C,"deltat")$(DELTAT(C) LT 1.e-4) = deltat(C) ;
 DELTATESTME(C,"deltaQ")$(DELTAQ(C) LT 1.e-4) = deltaq(C) ;

 DISPLAY$SUM((F,A,RG), deltatest1(F,A,RG))
  "*#*#*# Warning. Small deltas in CES production functions *#*#*#" ;
 DISPLAY$(SUM(C, deltatestme(C,"deltat") + deltatestme(C,"deltaq")) gt 1.e-8)
  "*#*#*# Warning. Small deltas in top trade aggregation functions *#*#*#" ;
 DISPLAY deltatest1, deltatestme ;

 GDPS0(A, RG) = PVA0(A,RG)*(1-tva0(A,RG))*QVA0(A,RG);
 GDPS0(A, RG) = GDPS0(A, RG) / SUM((AP,RGP), GDPS0(AP, RGP) );
 GDP = SUM((A,RG), PVA0(A,RG)*(1-tva(A,RG))*QVA0(A,RG));
 GDPS(A, RG) = GDPS0(A, RG);
* VARIABLE DECLARATIONS ###########################################
 CPI.L                  = CPI0;
 DMPS.L                 = DMPS0;
 DPI.L                  = DPI0;
 DTINS.L                = DTINS0;
 EG.L                   = EG0;
 EH.L(H)                = EH0(H);
 EXR.L                  = EXR0;
 FSAV.L                 = FSAV0;
 GSAV.L                 = GSAV0;
 QINV.L(C)              = QINV0(C);
 IADJ.L                 = IADJ0;
 GADJ.L                 = GADJ0;
 GOVSHR.L               = GOVSHR0;
 INVSHR.L               = INVSHR0;
 MPS.L(INSDNG)          = MPS0(INSDNG);
 MPSADJ.L               = MPSADJ0;
 PA.L(A)                = PA0(A);
 PAR.L(A,RG)            = PAR0(A,RG);
 TA.L(A,RG)             = TA0(A,RG);
 TQ.L(C)                = TQ0(C);
 PDD.L(C)               = PDD0(C);
 PDS.L(C)               = PDS0(C);
 PINTA.L(A,RG)          = PINTA0(A,RG) ;
 PE.L(C)                = PE0(C);
 PM.L(C)                = PM0(C);
 PQ.L(C)                = PQ0(C);
 PVA.L(A,RG)            = PVA0(A,RG);
 PX.L(C)                = PX0(C);
 PXAC.L(A,C)            = PXAC0(A,C);
 QA.L(A)                = QA0(A);
 QAR.L(A,RG)            = QAR0(A,RG);
 QD.L(C)                = QD0(C);
 QE.L(C)                = QE0(C);
 QF.L(F,A,RG)           = QF0(F,A,RG);
 QFS.L(F)               = QFS0(F);
 QG.L(C)                = QG0(C);
 QH.L(C,H)              = QH0(C,H);
 QHA.L(A,H)             = QHA0(A,H);
 QINT.L(C,A)            = QINT0(C,A);
 QINTA.L(A,RG)          = QINTA0(A,RG);
 QM.L(C)                = QM0(C);
 QQ.L(C)                = QQ0(C);
 QT.L(C)                = QT0(C);
 QVA.L(A,RG)            = QVA0(A,RG);
 QX.L(C)                = QX0(C);
 QXAC.L(A,C)            = QXAC0(A,C);
 TABS.L                 = TABS0;
 TRII.L(INSDNG,INSDNGP) = TRII0(INSDNG,INSDNGP);
 TINS.L(INSDNG)         = TINS0(INSDNG);
 TINSADJ.L              = TINSADJ0;
 WALRAS.L               = WALRAS0;
 WF.L(F)                = WF0(F);
 WFDIST.L(F,A,RG)       = WFDIST0(F,A,RG);
 YF.L(F)                = YF0(f);
 YG.L                   = YG0;
 YI.L(INS)              = YI0(INS);
 YIF.L(INS,F)           = YIF0(INS,F);

*--------------------------------------------------------------------------------------------
* FIXING VARIABLES NOT IN MODEL AT ZERO
*--------------------------------------------------------------------------------------------

 QA.FX(A)$(NOT QA0(A))           = 0;
 PA.FX(A)$(NOT QA0(A))           = 0;
 QAR.FX(A,RG)$(NOT QAR0(A,RG))   = 0;
 PAR.FX(A,RG)$(NOT QAR0(A,RG))   = 0;
 TA.FX(A,RG)$(NOT TA0(A,RG))   = 0;
 TQ.FX(C)$(NOT TQ0(C))   = 0;
 QVA.FX(A,RG)$(NOT QVA0(A,RG))   = 0;
 PVA.FX(A,RG)$(NOT QVA0(A,RG))   = 0;
 PDD.FX(C)$(NOT CD(C))           = 0;
 PDS.FX(C)$(NOT CD(C))           = 0;
 PE.FX(C)$(NOT CE(C))            = 0;
 PM.FX(C)$(NOT CM(C))            = 0;
 PX.FX(C)$(NOT CX(C))            = 0;
 PXAC.FX(A,C)$(NOT PXAC0(A,C))   = 0;
 QD.FX(C)$(NOT CD(C))            = 0;
 QE.FX(C)$(NOT CE(C))            = 0;
 QF.FX(F,A,RG)$(NOT QF0(F,A,RG)) = 0;
 QG.FX(C)$(NOT SAM(C,'GOV'))     = 0;
 QH.FX(C,H)$(NOT QH0(C,H))       = 0;
 QHA.FX(A,H)$(NOT BETAH(A,H))    = 0;
 QINT.FX(C,A)$(NOT QINT0(C,A))   = 0;
 QINV.FX(C)$(NOT QINV0(C))       = 0;
 QM.FX(C)$(NOT CM(C))            = 0;
 QQ.FX(C)$(NOT (CD(C) OR CM(C))) = 0;
 QT.FX(C)$(NOT CT(C))            = 0;
 QX.FX(C)$(NOT CX(C))            = 0;
 QXAC.FX(A,C)$(NOT QXAC0(A,C))   = 0;
 TRII.FX(INSDNG,INSDNGP)$(NOT SAM(INSDNG,INSDNGP))       = 0;
 YI.FX(INS)$(NOT INSD(INS))      = 0;
 YIF.FX(INS,F)$((NOT INSD(INS)) OR (NOT SAM(INS,F)))     = 0;

*--------------------------------------------------------------------------------------------
* MODEL Default CLOSURE
*--------------------------------------------------------------------------------------------

*Factor markets
*Disaggregate factors:
   WFDIST.FX(FLAB,A,RG) = WFDIST0(FLAB,A,RG);
   QFS.FX(FLAB) = QFS.L(FLAB);
   WF.LO(FLAB) = -INF;
   WF.UP(FLAB) = +INF;
   QF.LO(FLAB,A,RG)$QF0(FLAB,A,RG) = -INF;
   QF.UP(FLAB,A,RG)$QF0(FLAB,A,RG) = +INF;


   WFDIST.FX(FLND,A,RG) = WFDIST0(FLND,A,RG);
   QFS.FX(FLND) = QFS.L(FLND);
   WF.LO(FLND) = -INF;
   WF.UP(FLND) = +INF;
   QF.LO(FLND,A,RG)$QF0(FLND,A,RG) = -INF;
   QF.UP(FLND,A,RG)$QF0(FLND,A,RG) = +INF;

   WF.FX(FCAP)   = WF0(FCAP);
   QF.FX(FCAP,A,RG) = QF.L(FCAP,A,RG);
   WFDIST.LO(FCAP,A,RG)$QF0(FCAP,A,RG) = -INF;
   WFDIST.UP(FCAP,A,RG)$QF0(FCAP,A,RG) = +INF;
   QFS.LO(FCAP) = -INF;
   QFS.UP(FCAP) = +INF;

*Current account of RoW
   FSAV.FX   = FSAV.L;
   EXR.LO    = -INF;
   EXR.UP    = +INF;

*Government
   TINSADJ.FX = TINSADJ0;
   DTINS.FX   = DTINS0;
   GSAV.LO    = -INF;
   GSAV.UP    = +INF;

*SAVINGS AND INVESTMENT
   MPSADJ.FX = MPSADJ.l;
   DMPS.LO   = -INF;
   DMPS.UP   = +INF;
   IADJ.LO   = -INF;
   IADJ.UP   = +INF;
   INVSHR.FX = INVSHR.l;
   GADJ.FX   = GADJ.L;
   GOVSHR.LO   = -INF;
   GOVSHR.UP   = +INF;

*Numeraire price index
   CPI.FX     = CPI0;
   DPI.LO     = -INF;
   DPI.UP     = +INF;

 TA.FX(A,RG) = TA0(A,RG);
 TQ.FX(C) = TQ0(C);

*--------------------------------------------------------------------------------------------
* SOLUTION STATEMENT
*--------------------------------------------------------------------------------------------

 STANDCGE.HOLDFIXED   = 1 ;
 STANDCGE.TOLINFREP   = .001 ;
 STANDCGE.MODELSTAT = 0;
 STANDCGE.SOLVESTAT = 0;
 STANDCGE.NUMREDEF  = 0;
option iterlim = 100;
 SOLVE STANDCGE USING MCP;

scalar stop /0/;
