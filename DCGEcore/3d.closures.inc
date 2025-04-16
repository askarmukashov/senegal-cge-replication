*Macro closures ---------------------------------

*Numeraire
  IF(NUMERAIRE EQ 1,
   CPI.FX     = CPI.L;
   DPI.LO     = -INF;
   DPI.UP     = +INF;
   );
  IF(NUMERAIRE EQ 2,
   DPI.FX     = DPI.L;
   CPI.LO     = -INF;
   CPI.UP     = +INF;
   );
  IF(NUMERAIRE EQ 3,
   EXR.FX     = EXR.L;
   DPI.LO     = -INF;
   DPI.UP     = +INF;
   CPI.LO     = -INF;
   CPI.UP     = +INF;
   );

*Government
   TINSADJ.FX = TINSADJ.l;
   DTINS.FX   = DTINS.l;
   GSAV.LO    = -INF;
   GSAV.UP    = +INF;

*SAVINGS AND INVESTMENT
  IF(SICLOS EQ 1,
   MPSADJ.FX = MPSADJ.L;
   DMPS.LO   = -INF;
   DMPS.UP   = +INF;
   IADJ.FX   = IADJ.L;
   INVSHR.LO = -INF;
   INVSHR.UP = +INF;
   GADJ.FX   = GADJ.L;
   GOVSHR.LO = -INF;
   GOVSHR.UP = +INF;
   );
  IF(SICLOS EQ 2,
   MPSADJ.LO = -INF;
   MPSADJ.UP = +INF;
   DMPS.FX   = DMPS.L;
   IADJ.FX   = IADJ.L;
   INVSHR.LO = -INF;
   INVSHR.UP = +INF;
   GADJ.FX   = GADJ.L;
   GOVSHR.LO = -INF;
   GOVSHR.UP = +INF;
   );
  IF(SICLOS EQ 3,
   MPSADJ.FX = MPSADJ.L;
   DMPS.FX   = DMPS.L;
   IADJ.LO   = -INF;
   IADJ.UP   = +INF;
   INVSHR.LO = -INF;
   INVSHR.UP = +INF;
   GADJ.FX   = GADJ.L;
   GOVSHR.LO = -INF;
   GOVSHR.UP = +INF;
   );
  IF(SICLOS EQ 4,
   MPSADJ.FX = MPSADJ.L;
   DMPS.LO   = -INF;
   DMPS.UP   = +INF;
   IADJ.LO   = -INF;
   IADJ.UP   = +INF;
   INVSHR.FX = INVSHR.L;
   GADJ.LO   = -INF;
   GADJ.UP   = +INF;
   GOVSHR.FX = GOVSHR.L;
   );
  IF(SICLOS EQ 5,
   MPSADJ.LO = -INF;
   MPSADJ.UP = +INF;
   DMPS.FX   = DMPS.L;
   IADJ.LO   = -INF;
   IADJ.UP   = +INF;
   INVSHR.FX = INVSHR.L;
   GADJ.LO   = -INF;
   GADJ.UP   = +INF;
   GOVSHR.FX = GOVSHR.L;
   );


*Current account
  IF(ROWCLOS EQ 1,
   FSAV.FX   = FSAV.L;
   EXR.LO    = -INF;
   EXR.UP    = +INF;
   );
  IF(ROWCLOS EQ 2,
   EXR.FX     = EXR.L;
   FSAV.LO   = -INF;
   FSAV.UP   = +INF;
   );
  IF(ROWCLOS EQ 3,
   FSAV.FX   = FSAV.L;
   );

*Factor closures ------------------

  LOOP(F$FMOBFE(F),
   WFDIST.FX(F,A,RG) = WFDIST.l(F,A,RG);
   QFS.FX(F) = QFS.L(F);
   WF.LO(F) = -INF;
   WF.UP(F) = +INF;
   QF.LO(F,A,RG)$QF0(F,A,RG) = -INF;
   QF.UP(F,A,RG)$QF0(F,A,RG) = +INF;
  );

  LOOP(F$FACTFE(F),
   WF.FX(F)   = WF.l(F);
   QF.FX(F,A,RG) = QF.L(F,A,RG);
   WFDIST.LO(F,A,RG)$QF0(F,A,RG) = -INF;
   WFDIST.UP(F,A,RG)$QF0(F,A,RG) = +INF;
   QFS.LO(F) = -INF;
   QFS.UP(F) = +INF;
  );

  LOOP(F$FMOBUE(F),
   WFDIST.FX(F,A,RG) = WFDIST.l(F,A,RG);
   WF.FX(F) = WF.l(F);
   QF.LO(F,A,RG)$QF0(F,A,RG) = -INF;
   QF.UP(F,A,RG)$QF0(F,A,RG) = +INF;
   QFS.LO(F) = -INF;
   QFS.UP(F) = +INF;
  );
