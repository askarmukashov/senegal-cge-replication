 ALPHAVAX(A,RG,%1)  = alphava(A,RG);
 CPIX(%1)           = CPI.L;
 DPIX(%1)           = DPI.L;
 EGX(%1)            = EG.L;
 EHX(H,%1)          = EH.L(H);
 EXRX(%1)           = EXR.L;
 GADJX(%1)          = GADJ.L;
 GOVSHRX(%1)        = GOVSHR.L;
 GSAVX(%1)              = GSAV.L;
 FSAVX(%1)              = FSAV.L;
 IADJX(%1)       = IADJ.L;
 GDPSX(A,RG,%1)     = GDPS(A, RG);
 GDPX(%1)     = GDP;
 INVSHRX(%1)        = INVSHR.L;
 MPSX(INS,%1)       = MPS.L(INS);
 MPSADJX(%1)        = MPSADJ.L;
 PAX(A,%1)          = PA.L(A);
 PARX(A,RG,%1)      = PAR.L(A,RG);
 PDDX(C,%1)         = PDD.L(C);
 PDSX(C,%1)         = PDS.L(C);
 PEX(C,%1)          = PE.L(C);
 PINTAX(A,RG,%1)    = PINTA.L(A,RG);
 PMX(C,%1)          = PM.L(C);
 PQX(C,%1)          = PQ.L(C);
 PVAX(A,RG,%1)      = PVA.L(A,RG);
 PWEX(C,%1)         = pwe(C);
 PWMX(C,%1)         = pwm(C);
 PXX(C,%1)          = PX.L(C);
 PXACX(A,C,%1)      = PXAC.L(A,C);
 QARX(A,RG,%1)      = QAR.L(A,RG);
 QAX(A,%1)          = QA.L(A);
 QDX(C,%1)          = QD.L(C);
 QEX(C,%1)          = QE.L(C);
 QFX(F,A,RG,%1)     = QF.L(F,A,RG);
 QFSX(F,%1)         = QFS.L(F);
 QGX(C,%1)          = QG.L(C);
 QHX(C,H,%1)        = QH.L(C,H);
 QHAX(A,H,%1)       = QHA.L(A,H);
 QINTX(C,A,%1)      = QINT.L(C,A);
 QINTAX(A,RG,%1)    = QINTA.L(A,RG);
 QINVX(C,%1)     = QINV.L(C);
 QMX(C,%1)          = QM.L(C);
 QQX(C,%1)          = QQ.L(C);
 QTX(C,%1)          = QT.L(C);
 QVAX(A,RG,%1)      = QVA.L(A,RG);
 QXX(C,%1)          = QX.L(C);
 QXACX(A,C,%1)      = QXAC.L(A,C);
 TABSX(%1)          = TABS.L;
 TINSX(INS,%1)      = TINS.L(INS);
 TINSADJX(%1)       = TINSADJ.L;
 TAX(A,RG,%1)       = ta.l(A,RG);
 TMX(C,%1)          = tm(C);
 TQX(C,%1)          = tq.l(C);
 TRIIX(INS,INSP,%1) = TRII.L(INS,INSP);
 WALRASX(%1)        = WALRAS.L;
 WFX(F,%1)          = WF.L(F);
 WFDISTX(F,A,RG,%1) = WFDIST.L(F,A,RG);
 YFX(F,%1)          = YF.L(F);
 YGX(%1)            = YG.L;
 YIFX(INS,F,%1)     = YIF.L(INS,F);
 YIX(INS,%1)        = YI.L(INS);
 hpopx(H,%1)        = hpop(H);
  hpop_withoutx(H,%1)        = hpop_without(H);

 trnsfrX(INS,AC, %1) = trnsfr(INS,AC);


******
 PDIND(%1)  = 100*SUM(C, pdwt(C)*PDD.L(C));
 PWEIND(%1) = 100*SUM(C, pwe(C)*pwewt(C))
   /SUM(C, pwe0(C)*pwewt(C));
 PWMIND(%1) = 100*SUM(C, pwm(C)*pwmwt(C))
   /SUM(C, pwm0(C)*pwmwt(C));
 TOFT(%1)  = 100*PWEIND(%1)/PWMIND(%1);
 PWIND(%1) = pwwt('EXP')*PWEIND(%1) +  pwwt('IMP')*PWMIND(%1);
 REXR(%1) = EXR.L*PWIND(%1) / PDIND(%1) ;
 QFS_rgX(flabnational, RG, %1) = qfs_rg.L(flabnational, RG);
 p_rgX(flabnational, RG, %1)  = p_rg.l(flabnational, RG) ;
 QDSTX(C, %1) =  QDST(C);
 P_indX(flabnational, %1) =  P_ind.l(flabnational);
 shifX(INSDNG,F,MM_T) = shif(INSDNG,F);
 QFSwithoutX(F,%1)= QFSwithout(F);


 net_balanceX(FLAB, %1) = net_balance(FLAB);
 QFSmigrpercX(FLAB,%1)$QFSwithout(FLAB) = net_balance(FLAB) / QFSwithout(FLAB);
Pc_consX(H, %1) = Pc_cons(H);
Tot_consX(H, %1) = Tot_cons(H);
facVAshX(F,A,RG,MM_T) = facVAsh(F,A,RG);
