SETS
*a. model sets
 AC                      global set for model accounts - aggregated microsam accounts
 ACNT(AC)                all elements in AC except TOTAL
 ARG(AC)                 input in the form A*_RG* (activity*region)
 A(AC)                   activities

 AAGR(A)                 agricultural sector activities (used for parZ calculations)
 AAGRPR(A)               agroprocessing activities (used for parZ calculations)
 AAGRB(A)                broad agricultural sector activities (used for parZ calculations)
 AOIND(A)                other industrial sector activities (used for parZ calculations)
 APSERV(A)               private sector activities (used for parZ calculations)


 RG(AC)                  internal regions
 ACES2(A)                activities with CES activity output aggregation function
 ACET2(A)                activities with CET activity output disaggregation function
 AFLEO(A)                activities with Leontief factor demands
 C(AC)                   commodities
 CAGR(C)                 agricultural commodities (used for parZ calculations)
 CAGRPR(C)               agroprocessing commodities (used for parZ calculations)
 CAGRB(C)                broad agricultural commodities (used for parZ calculations)

 CD(C)                   commodities with domestic sales of output
 CDN(C)                  commodities without domestic sales of output
 CE(C)                   exported commodities
 CEN(C)                  non-export commodities
 CM(C)                   imported commodities
 CMN(C)                  non-imported commodities
 CX(C)                   commodities with output
 F(AC)                   factors
 FLAB(F)                 labor
 FLND(F)                 land
 FCAP(F)                 capital
 INS(AC)                 institutions
 ROW(INS)                ROW institutions
 INSD(INS)               domestic institutions
 GOV(INSD)               gov institutions
 CPUB(C)                 public goods
 INSDNG(INSD)            domestic non-government institutions
 H(INSDNG)               households
 EN(INSDNG)              enterprises
*b. calibration sets
 CT(C)                   transaction service commodities
 CTD(AC)                 domestic transactions cost account
 CTE(AC)                 export transactions cost account
 CTM(AC)                 import transactions cost account
*c. mappings
 MAC(A,C)                mapping between activities and commodities
 MARG(ARG,A,RG)          mapping between SAM regions
  CERES(C)        commodities with residual export treatment     / /
;

PARAMETER
 SAM(AC,AC)              standard SAM
 SAMBALCHK(AC)           column minus row total for SAM
 FACNEST(F,F)            nested structure of factors in the model
 FACTREE(F,F)            direct and indirect factor mapping in nested factor structure
 QF2BASE(F,AC)           qnty of fac f employed by act a (extracted data)
 TNUM                    number of active years (used for reporting)
 SCALE           scaling parameter for SAM (multiplier)          / 1 /
 natdrate        national annual capital depreciation rate
 accrate         annual capital accumulation rate
;

ALIAS
 (AC,ACP), (ACNT,ACNTP), (A,AP,APP,APPP), (C,CP,CPP), (CE,CEP), (CM,CMP)
 (F,FP,FPP,FPPP), (FLAB,FLABP), (FCAP,FCAPP), (FLND,FLNDP), (INS,INSP)
 (INSD,INSDP), (INSDNG,INSDNGP), (H,HP), (ARG,ARGP), (RG,RGP), (CTE,CTEP), (CTM,CTMP)
;

PARAMETER
 m(c) transaction margin (implied to be the same for domestic, import CIF and export FOB)
 DOMX(C) domestic production (domestic sales + export)
 DOMSALX(C) domestic sales of domestically produced C
 IMPX(C) import
 EXPX(C) export
 TRSHR(C)
;

*Check for activities with no output
SET
 ACHK(A)         Activities with no output
 FCHK(F)         Factors with no values
 HCHK(H)         Households with no values
 CCHK(C)         Commodities with no values
;


*Trade elasticities --------------------------------

SET
 TRDELAS  trade elasticities
 /
 SIGMAQ  Armington elasticity
 SIGMAT  CET elasticity
 /

 PRDELAS  production elasticities
 /
 PRODELAS
 /

 AGGELAS  elasticities for output (dis) agg regation
 /
 ELASCA
 ELASCES
 /
;

PARAMETER
 TRADELAS(AC,TRDELAS)    Armington and CET elasticities by commodity
 PRODELASTAB(AC,PRDELAS)
 PRODELAS(AC)             Elas of substit bt. factors - bottom of technology nest
 ELASAC(C)               Output aggregation elasticity for commodity C

 AGGTAB(A,AGGELAS)
 ELASCA(A)               CET elasticity for output disaggregation
 ELASCES(A)              CES activity output aggregation function
;

*Household consumption elasticities ----------------
*Note: The Frisch parameter is included in this section.

PARAMETERS
 LESELAS1(C,H)    LES expenditure elasticities for commodities
 FRISCH(H)        Frisch parameter for household LES demand
 LESELAS2(A,H)    Expe elasticity of home dem by act - hhd
;


*----------------------------------------------------------------------------
*4. PHYSICAL FACTOR QUANTITIES AND FACTOR MARKET STRUCTURES
*----------------------------------------------------------------------------

PARAMETER
 QFSBASE(AC)             total employment data
 QFBASE(F,AC)            sectoral employment
;

*-----------------------------------------------------------------------------
*5. COMMODITY VALUE SHARES FOR HOME CONSUMPTION
*-----------------------------------------------------------------------------

PARAMETER
 shrhome(A,H) value share for activity a in home cons of hhd h from act a
 POPTAB(H,*)
 hpop0(H)
 hpop(H)
 ;



*-----------------------------------------------------------------------------
*6. INITIALIZATION OF TAX DATA
*-----------------------------------------------------------------------------

SET
 TX  taxes in the model
 /
 INSTAX         direct taxes on domestic institutions
 FACTAX         direct factor taxes
 IMPTAX         import taxes
 EXPTAX         export taxes
 VATAX          value-added taxes
 ACTTAX         taxes on activity revenue
 COMTAX         taxes on commodity sales in domestic market
 /
;

PARAMETER
 TAXPAR(TX,AC)   payment by account ac to tax account tx
 ;



PARAMETERS
*a. Parameters appearing in model equations
*Parameters other than tax rates
 alphaac(C)              shift parameter for domestic commodity aggregation fn
 alphaq(C)               shift parameter for Armington function
 alphat(C)               shift parameter for CET function
 alphava(A,RG)           shift parameter for CES activity production function
 alphaa2(A)              shift parameter on ces activity output aggregation function
 alphaca(A)              shift parameter on CET activity output disaggregation function
 beta1                   capital mobility parameter by type                      / 2.00 /
 beta2                   capital mobility by sector                              / 2.00 /
 betah(A,H)              marg shr of hhd cons on home com c from act a
 betam(C,H)              marg share of hhd cons on marketed commodity c
 cwts(C)                 consumer price index weights
 cwtsh(AC,H)             consumer price index weight for com'y c or act a for hhd h
 deltaac(A,C)            share parameter for domestic commodity aggregation fn
 deltaq(C)               share parameter for Armington function
 deltat(C)               share parameter for CET function
 deltava(F,A,RG)         share parameter for CES activity production function
 deltaa2(A,RG)           share parameters on the activity output aggregation function
 deltaca(A,C)            share parameter for CET output disaggregation
 dwts(C)                 domestic sales price weights
 fprd(F,A,RG)            factor-specific productivity
 gammah(A,H)             per-cap subsist cons for hhd h on home com c fr act a
 gammam(C,H)             per-cap subsist cons of marketed com c for hhd h
 ica(C,A,RG)             intermediate input c per unit of aggregate intermediate
 inta(A,RG)              aggregate intermediate input coefficient
 iva(A,RG)               aggregate value added coefficient
 icd(C,CP)               trade input of c per unit of comm'y cp produced & sold dom'ly
 ice(C,CP)               trade input of c per unit of comm'y cp exported
 icm(C,CP)               trade input of c per unit of comm'y cp imported
 iwts(C)                 investment commodity demand weight
 ifa(F,A,RG)             fixed factor shares for leontief factor demand
 mps01(INS)              0-1 par for potential flexing of savings rates
 mpsbar(INS)             marg prop to save for dom non-gov inst ins (exog part)
 pwe(C)                  world price of exports
 pwm(C)                  world price of imports
 qdst(C)                 inventory investment by sector of origin
 qbarg(C)                exogenous (unscaled) government demand
 qbarinv(C)              exogenous (unscaled) investment demand
 rhoac(C)                domestic commodity aggregation function exponent
 rhoq(C)                 Armington function exponent
 rhot(C)                 CET function exponent
 rhova(A,RG)             CES activity production function exponent
 rhoa2(A)                CES activity output aggregation function exponent
 rhoca(A)                CET activity disaggregation function exponent
 rf(F)                   factor foreign remittances
 elasva(A,RG)            CES elasticity of substitution in production
 shif(INS,F)             share of dom. inst'on i in income of factor f
 shii(INS,INSP)          share of inst'on i in post-tax post-sav income of inst ip
 supernum(H)             LES supernumerary income

 SUBSIST(H)      subsistence spending
 FRISCH2(H)      alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)       check on LES parameter definitions (error mssg if error)
  DELTATEST1(F,A,RG)  Small delta parameters in CES production function
  DELTATESTME(C,*) Small delta parameters in top trade aggregation functions

 theta(A,C)              yield of commodity C per unit of activity A
 tins01(INS)             0-1 par for potential flexing of dir tax rates
 trnsfr(INS,AC)          transfers fr. inst. or factor ac to institution ins

*Tax rates
 te(C)                   rate of tax on exports
 tf(F)                   rate of direct tax on factors (soc sec tax)
 tinsbar(INS)            rate of (exog part of) direct tax on dom inst ins
 tm(C)                   rate of import tariff
 tva(A,RG)               rate of value-added tax

*b. Parameters used for model calibration
*Parameters for definition of model parameters
 alphainv                investment shift parameter
 alphava0(A,RG)          shift parameter for CES activity production function

 qdst0(C)                stock change
 qbarg0(C)               exogenous (unscaled) government demand
 gammah0(A,H)            per-cap subsist cons for hhd h on home com c fr act a
 gammam0(C,H)            per-cap subsist cons of marketed com c for hhd h
 pwe0(C)                 world price of exports
 pwm0(C)                 world price of imports
 ta0(A,RG)               rate of tax on producer gross output value
 te0(C)                  rate of tax on exports
 tf0(F)                  rate of direct tax on factors -- soc sec tax
 tins0(INS)              rate of direct tax on domestic institutions ins
 tm0(C)                  rate of import tariff
 trnsfr0(INS,AC)         transfers fr. inst. or factor ac to institution ins
 tva0(A,RG)              rate of value-added tax

*Check parameters
 cwtschk                 check that CPI weights sum to unity
 cwtshchk(h)             check that CPI weights sum to unity for hhd h
 dwtschk                 check that PDIND weights sum to unity
 shifchk(F)              check that factor payment shares sum to unity
*Parameters for variable initialization
 CPI0                    consumer price index -- PQ-based
 DPI0                    index for domestic producer prices --PDS-based
 DMPS0                   change in marginal propensity to save for selected inst
 DTINS0                  change in domestic institution tax share
 EG0                     total current government expenditure
 EH0(H)                  household consumption expenditure
 EXR0                    exchange rate
 FSAV0                   foreign savings
 GADJ0                   government demand scaling factor
 GOVSHR0                 govt consumption share of absorption
 GSAV0                   government savings
 IADJ0                   investment scaling factor for fixed capital formation
 INVSHR0                 investment share of absorption
 MPS0(INS)               marginal propensity to save for dom non-gov inst ins
 MPSADJ0                 savings rate scaling factor
 PA0(A)                  output price of aggregate national a
 PAR0(A,RG)              output price of region specific activity
 PDD0(C)                 demand price for com'y c produced & sold domestically
 PDS0(C)                 supply price for com'y c produced & sold domestically
 PE0(C)                  price of exports
 PINTA0(A,RG)            price of intermediate aggregate
 PM0(C)                  price of imports
 PQ0(C)                  price of composite good c
 PVA0(A,RG)              value added price
 PX0(C)                  average output price
 PXAC0(A,C)              price of commodity c from activity a
 QA0(A)                  level of domestic activity nationally
 QAR0(A,RG)              level of domestic activity regionally
 QANET0(A)               QA net of home consumption
 QD0(C)                  quantity of domestic sales
 QE0(C)                  quantity of exports
 QF0(F,A,RG)             quantity demanded of factor f from activity a
 QFS0(F)                 quantity of factor supply
 QG0(C)                  quantity of government consumption
 QH0(C,H)                quantity consumed of marketed commodity c by hhd h
 QHA0(A,H)               quantity consumed of home commodity c fr act a by hhd h
 QINT0(C,A)              quantity of intermediate demand for c from activity a
 QINTA0(A,RG)            quantity of aggregate intermediate input
 QINV0(C)                quantity of fixed investment demand
 QM0(C)                  quantity of imports
 QQ0(C)                  quantity of composite goods supply
 QT0(C)                  quantity of trade and transport demand for commodity c
 QVA0(A,RG)              quantity of aggregate value added
 QX0(C)                  quantity of aggregate marketed commodity output
 QXAC0(A,C)              quantity of ouput of commodity c from activity a
 TABS0                   total absorption
 TINS0(INS)              rate of direct tax on domestic institutions ins
 TINSADJ0                direct tax scaling factor
 TQ0(C)                  rate of sales tax
 TRII0(INS,INSP)         transfers to dom. inst. insdng from insdngp
 WALRAS0                 savings-investment imbalance (should be zero)
 WF0(F)                  economy-wide wage (rent) for factor f
 WFDIST0(F,A,RG)         factor wage distortion variable
 YF0(F)                  factor income
 YG0                     total current government income
 YIF0(INS,F)             income of institution ins from factor f
 YI0(INS)                income of (domestic non-governmental) institution ins
*Capital stock updating parameters (only used in the simulation file)
 CAPSHR1(F)              shares of aggregate capital by type (sums to one)
 CAPSHR2(F,A,RG)         sectoral shares of capital by type (rows sum to one)
 CAPSHR1TOT              used to speed up capital accumulation calculations
 CAPSHR2TOT(F)           used to speed up capital accumulation calculations
 DKAP(F,A,RG)            change in sectoral real capital stock
 DKAPS(F)                change in aggregate real capital stock
 INVSHR1(F)              investment shares by type of capital
 INVSHR2(F,A,RG)         investment shares by sector for each capital type
 NGFCF                   GFCF net of exogenous capital adjustments in fixed sectors
 QINVK                   quantity of new capital stock
 RKAP(F,A,RG)            annual rate of growth of sectoral capital stock by type
 RKAPS(F)                annual rate of growth of aggregate capital stock by type
 WFADJ(F)                WF adjusted to exclude fixed sectors
 WFK1AV                  average rental on all capital (economywide)
 WFK2AV(F)               average rental on capital by type (across all activities)
 WFDIST2(F,A,RG)         ratio of sectoral to average rental by capital type
 WFDISTADJ(F,A,RG)       WFDIST adjusted to exclude fixed sectors
*Calibration parameters
 PSUP(C)                 initial supply-side market price for commodity c
 SHCTD(C)                share of comm'y ct in trans services for domestic sales
 SHCTM(C)                share of comm'y ct in trans services for imports
 SHCTE(C)                share of comm'y ct in trans services for exports
 WFA(F,A,RG)             wage for factor f in activity a (used for calibration)
 predeltaa(A)            dummy used to define deltaa
 predelta(C)             dummy used to define deltaq
 BUDSHR(C,H)             budget share for marketed commodity c and household h
 BUDSHR2(A,H)            budget share for home commodity c - act a - hhd h
 BUDSHRCHK(H)            check that budget shares some to unity
 ELASCHK(H)              check that expenditure elasticities satisfy Engel aggr
 SUBSIST(H)              subsistence spending
 FRISCH2(H)              alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)               check on LES parameter definitions (error mssg if error)


;

*5. VARIABLE DECLARATIONS ###########################################
*This section only includes variables that appear in the model.
*The variables are declared in alphabetical order.

VARIABLES
 CPI                     consumer price index (PQ-based)
 DPI                     index for domestic producer prices (PDS-based)
 DMPS                    change in marginal propensity to save for selected inst
 DTINS                   change in domestic institution tax share
 EG                      total current government expenditure
 EH(H)                   household consumption expenditure
 EXR                     exchange rate
 FSAV                    foreign savings
 GADJ                    government demand scaling factor
 GOVSHR                  govt consumption share of absorption
 GSAV                    government savings
 IADJ                    investment scaling factor (for fixed capital formation)
 INVSHR                  investment share of absorption
 MPS(INS)                marginal propensity to save for dom non-gov inst ins
 MPSADJ                  savings rate scaling factor
 PA(A)                   output price of national aggregate activity a
 PAR(A,RG)               output price of regional activity a
 PDD(C)                  demand price for com'y c produced & sold domestically
 PDS(C)                  supply price for com'y c produced & sold domestically
 PE(C)                   price of exports
 PINTA(A,RG)             price of intermediate aggregate
 PM(C)                   price of imports
 PQ(C)                   price of composite good c
 PVA(A,RG)               value added price
 PX(C)                   average output price
 PXAC(A,C)               price of commodity c from activity a
 QA(A)                   level of domestic aggregate activity
 QAR(A,RG)               level of domestic regional activity
 QD(C)                   quantity of domestic sales
 QE(C)                   quantity of exports
 QF(F,A,RG)              quantity demanded of factor f from activity a
 QFS(F)                  quantity of factor supply
 QG(C)                   quantity of government consumption
 QH(C,H)                 quantity consumed of marketed commodity c by household h
 QHA(A,H)                quantity consumed of home act a by hhd h
 QINT(C,A)               quantity of intermediate demand for c from activity a
 QINTA(A,RG)             quantity of aggregate intermediate input
 QINV(C)                 quantity of fixed investment demand
 QM(C)                   quantity of imports
 QQ(C)                   quantity of composite goods supply
 QT(C)                   quantity of trade and transport demand for commodity c
 QVA(A,RG)               quantity of aggregate value added
 QX(C)                   quantity of aggregate marketed commodity output
 QXAC(A,C)               quantity of ouput of commodity c from activity a
 TABS                    total absorption
 TINS(INS)               rate of direct tax on domestic institutions ins
 TINSADJ                 direct tax scaling factor
 TA(A,RG)                rate of tax on producer gross output value
 TQ(C)                   rate of sales tax
 TRII(INS,INSP)          transfers to dom. inst. insdng from insdngp
 WALRAS                  savings-investment imbalance (should be zero)
 WALRASSQR               Walras squared
 WF(F)                   economy-wide wage (rent) for factor f
 WFDIST(F,A,RG)          factor wage distortion variable
 YF(F)                   factor income
 YG                      total current government income
 YIF(INS,F)              income of institution ins from factor f
 YI(INS)                 income of (domestic non-governmental) institution ins

;

*--------------------------------------------------------------------------------------------
*5. EQUATIONS
*--------------------------------------------------------------------------------------------

EQUATIONS
*Price block
 PMDEF(C)                domestic import price
 PEDEF(C)                domestic export price
 PDDDEF(C)               dem price for com-y c produced and sold domestically
 PQDEF(C)                value of sales in domestic market
 PXDEF(C)                value of marketed domestic output
 PXDEF2(C)               commodities with residual export treatment
 PADEF(A)                output price for national aggregate activity a
 PADEF2(A,RG)            output price for regional activity a
 PINTADEF(A,RG)          price of aggregate intermediate input
 PVADEF(A,RG)            value-added price
 CPIDEF                  consumer price index
 DPIDEF                  domestic producer price index
*Production and trade block
 LEONFAC1(A,RG)
 LEONFAC2(F,A,RG)
 QADEF(A)                define national aggregate as sum of regional production
 LEOAGGINT(A,RG)         Leontief aggreg intermed dem (if Leontief top nest)
 LEOAGGVA(A,RG)          Leontief aggreg value-added dem (if Leontief top nest)
 CESVAPRD(A,RG)          CES value-added production function
 CESVAFOC(F,A,RG)        CES value-added first-order condition
 QACES(A)                CES activity output aggregation function
 QACESFOC(A,RG)          CES activity output aggregation function first-order condition
 INTDEM(C,A)             intermediate demand for commodity c from activity a
 COMPRDFN1(A,C)          production function for commodity c and activity a (LEO)
 COMPRDFN2(A,C)          production function for commodity c and activity a (CET)
 OUTAGGFN(C)             output aggregation function
 OUTAGGFOC(A,C)          first-order condition for output aggregation function
 CET(C)                  CET function
 CET2(C)                 domestic sales and exports for outputs without both
 ESUPPLY(C)              export supply
 EXPRESID1(C)            export supply quantity
 EXPRESID2(C)            domestic world price equality
 ARMINGTON(C)            composite commodity aggregation function
 COSTMIN(C)              first-order condition for composite commodity cost min
 ARMINGTON2(C)           comp supply for com-s without both dom. sales and imports
 QTDEM(C)                demand for transactions (trade and transport) services
*Institution block
 YFDEF(F)                factor incomes
 YIFDEF(INS,F)           factor incomes to domestic institutions
 YIDEF(INS)              total incomes of domest non-govt institutions
 EHDEF(H)                household consumption expenditures
 TRIIDEF(INS,INSP)       transfers to inst-on ins from inst-on insp
 HMDEM(C,H)              LES cons demand by hhd h for marketed commodity c
 HADEM(A,H)              LES cons demand by hhd h for home commodity c fr act a
 INVDEM(C)               fixed investment demand
 GOVDEM(C)               government consumption demand
 EGDEF                   total government expenditures
 YGDEF                   total government income
*System constraint block
 COMEQUIL(C)             composite commodity market equilibrium
 FACEQUIL(F)             factor market equilibrium
 CURACCBAL               current account balance (of RoW)
 GOVBAL                  government balance
 TINSDEF(INS)            direct tax rate for inst ins
 MPSDEF(INS)             marg prop to save for inst ins
 SAVINVBAL               savings-investment balance
 TABSEQ                  total absorption
 INVABEQ                 investment share in absorption
 GDABEQ                  government consumption share in absorption
;

*Price block

 PMDEF(C)$CM(C)..        PM(C) =E= pwm(C)*(1 + tm(C))*EXR + SUM(CT, PQ(CT)*icm(CT,C));

 PEDEF(C)$CE(C)..        PE(C) =E= pwe(C)*(1 - te(C))*EXR - SUM(CT, PQ(CT)*ice(CT,C));

 PDDDEF(C)$CD(C)..       PDD(C) =E= PDS(C) + SUM(CT, PQ(CT)*icd(CT,C));

 PQDEF(C)$((CD(C) OR CM(C)))..  PQ(C)*(1 - TQ(C))*QQ(C) =E= PDD(C)*QD(C) + PM(C)*QM(C);

 PXDEF(C)$(CX(C) AND NOT CERES(C))..  PX(C)*QX(C) =E= PDS(C)*QD(C) + PE(C)*QE(C);

 PXDEF2(C)$(CX(C) AND CERES(C))..   PX(C) =E= PDS(C);

 PADEF(A)$QA0(A)..      PA(A) =E= SUM(C, PXAC(A,C)*theta(A,C));

 PINTADEF(A,RG)$QVA0(A,RG)..   PINTA(A,RG) =E= SUM(C, PQ(C)*ica(C,A,RG)) ;

 PVADEF(A,RG)$QVA0(A,RG)..     PAR(A,RG)*(1-TA(A,RG))*QAR(A,RG) =E=
                               PVA(A,RG)*QVA(A,RG) + PINTA(A,RG)*QINTA(A,RG) ;

 CPIDEF..                CPI =E= SUM(C, cwts(C)*PQ(C)) ;

 DPIDEF..                DPI =E= SUM(CD$(NOT CERES(CD)), dwts(CD)*PDS(CD)) ;

*Production and trade block

 LEOAGGINT(A,RG)$QVA0(A,RG)..  QINTA(A,RG) =E= inta(A,RG)*QAR(A,RG);

 LEOAGGVA(A,RG)$QVA0(A,RG)..   QVA(A,RG) =E= iva(A,RG)*QAR(A,RG);

 LEONFAC1(A,RG)$(AFLEO(A) AND QVA0(A,RG))..
   PVA(A,RG)*QVA(A,RG) =E= SUM(F, QF(F,A,RG)*WF(F)*WFDIST(F,A,RG));

 LEONFAC2(F,A,RG)$(AFLEO(A) AND QF0(F,A,RG))..
   QVA(A,RG)*ifa(F,A,RG) =E= QF(F,A,RG);

   PARAMETER
           fprd_adj(F);


 CESVAPRD(A,RG)$(QVA0(A,RG) AND NOT AFLEO(A))..   QVA(A,RG) =E= alphava(A,RG)*
         (SUM(F$QF0(F,A,RG), deltava(F,A,RG)*((fprd_adj(F)*fprd(F,A,RG))*QF(F,A,RG))**(-rhova(A,RG))) )**(-1/rhova(A,RG)) ;

 CESVAFOC(F,A,RG)$(QF0(F,A,RG) AND QVA0(A,RG)  AND NOT AFLEO(A))..
   WF(F)*WFDIST(F,A,RG) =E= PVA(A,RG)*(1-tva(A,RG)) * QVA(A,RG) *
         SUM(FP, deltava(FP,A,RG)*((fprd_adj(FP)*fprd(FP,A,RG))*QF(FP,A,RG))**(-rhova(A,RG)))**(-1)
         *deltava(F,A,RG)*(fprd_adj(F)*fprd(F,A,RG))**(-rhova(A,RG))*QF(F,A,RG)**(-rhova(A,RG)-1);

*PERF SUBS (or single region) ACTIVITY AGGREGATION FUNCTION
 QADEF(A)$(NOT ACES2(A))..  QA(A) =E=  SUM(RG,QAR(A,RG));
 PADEF2(A,RG)$(NOT ACES2(A) AND QAR0(A,RG)).. PA(A)=E= PAR(A,RG);

*CES ACTIVITY AGGREGATION FUNCTION
 QACES(A)$(ACES2(A) AND NOT ACHK(A))..
   QA(A) =E= alphaa2(A)*(SUM(RG, deltaa2(A,RG)*QAR(A,RG)**(-rhoa2(A))))**(-1/rhoa2(A));
 QACESFOC(A,RG)$(ACES2(A) AND NOT ACHK(A) AND deltaa2(A,RG))..
   PAR(A,RG) =E= PA(A)*deltaa2(A,RG)*(SUM(RGP, deltaa2(A,RGP)*QAR(A,RGP)**(-rhoa2(A))))**(-1)*QA(A)*QAR(A,RG)**(-rhoa2(A)-1);

 INTDEM(C,A)$SUM(RG,ica(C,A,RG))..  QINT(C,A) =E= SUM(RG,ica(C,A,RG)*QINTA(A,RG));

*CET disaggregation function (multiple outputs from same activity)
 COMPRDFN2(A,C)$(ACET2(A) AND deltaca(A,C))..
  QXAC(A,C) =E= (QA(A) - SUM(H, QHA(A,H)))*(PXAC(A,C)/(PA(A)*deltaca(A,C)*alphaca(A)**rhoca(A)))**(1/(rhoca(A)-1)) ;

 COMPRDFN1(A,C)$(NOT ACET2(A) AND theta(A,C)).. QXAC(A,C) =E= theta(A,C)*(QA(A) - SUM(H, QHA(A,H))) ;

 OUTAGGFN(C)$CX(C)..     QX(C) =E= alphaac(C)*SUM(A, deltaac(A,C)*QXAC(A,C)**(-rhoac(C)))**(-1/rhoac(C));

 OUTAGGFOC(A,C)$deltaac(A,C)..
   PXAC(A,C) =E= PX(C)*QX(C) * SUM(AP, deltaac(AP,C)*QXAC(AP,C)**(-rhoac(C)) )**(-1)*deltaac(A,C)*QXAC(A,C)**(-rhoac(C)-1);

 CET(C)$(CE(C) AND CD(C) AND (NOT CERES(C)))..
   QX(C) =E= alphat(C)*(deltat(C)*QE(C)**rhot(C) + (1 - deltat(C))*QD(C)**rhot(C))**(1/rhot(C)) ;

 ESUPPLY(C)$(CE(C) AND CD(C) AND (NOT CERES(C)))..
   QE(C) =E= QD(C)*((PE(C)/PDS(C))*((1 - deltat(C))/deltat(C)))**(1/(rhot(C)-1)) ;

 EXPRESID1(C)$(CX(C) AND CERES(C)).. QE(C)  =E= QX(C) - QD(C);

 EXPRESID2(C)$(CX(C) AND CERES(C)).. PDS(C) =E= PE(C);

 CET2(C)$((CD(C) AND CEN(C)) OR (CE(C) AND CDN(C)))..
   QX(C) =E= QD(C) + QE(C);

 ARMINGTON(C)$(CM(C) AND CD(C))..
   QQ(C) =E= alphaq(C)*(deltaq(C)*QM(C)**(-rhoq(C)) + (1 -deltaq(C))*QD(C)**(-rhoq(C)))**(-1/rhoq(C)) ;

 COSTMIN(C)$(CM(C) AND CD(C))..
   QM(C) =E= QD(C)*((PDD(C)/PM(C))*(deltaq(C)/(1 - deltaq(C))))**(1/(1 + rhoq(C)));

 ARMINGTON2(C)$( ((CD(C) AND CMN(C)) OR (CM(C) AND CDN(C))))..
   QQ(C) =E= QD(C) + QM(C);

 QTDEM(C)$CT(C)..
   QT(C) =E= SUM(CP, icm(C,CP)*QM(CP)+ ice(C,CP)*QE(CP)+ icd(C,CP)*QD(CP));

*Institution block

 YFDEF(F)..      YF(F) =E= WF(F)*SUM((A,RG), WFDIST(F,A,RG)*QF(F,A,RG));

 YIFDEF(INSD,F)$shif(INSD,F)..
   YIF(INSD,F) =E= shif(INSD,F)*((1-tf(f))*(1-rf(F))*YF(F) - SUM(ROW, trnsfr(ROW,F))*EXR);

 YIDEF(INSDNG)..
   YI(INSDNG) =E= SUM(F, YIF(INSDNG,F))  + SUM(INSDNGP, TRII(INSDNG,INSDNGP))
             + SUM(GOV, trnsfr(INSDNG, GOV))*CPI + SUM(ROW, trnsfr(INSDNG, ROW))*EXR;

 TRIIDEF(INSDNG,INSDNGP)$( shii(INSDNG,INSDNGP))..
   TRII(INSDNG,INSDNGP) =E= shii(INSDNG,INSDNGP) * (1 - MPS(INSDNGP)) * (1 - TINS(INSDNGP))* YI(INSDNGP);

 EHDEF(H).. EH(H) =E= (1 - SUM(INSDNG, shii(INSDNG,H))) * (1 - MPS(H)) * (1 - TINS(H)) * YI(H);

 HMDEM(C,H)$( betam(C,H))..
   PQ(C)*QH(C,H) =E= PQ(C)*gammam(C,H)
       + betam(C,H)*( EH(H) - SUM(CP, PQ(CP)*gammam(CP,H))
                            - SUM(A, PA(A)*gammah(A,H))) ;

 HADEM(A,H)$( betah(A,H))..
  PA(A)*QHA(A,H) =E= PA(A)*gammah(A,H)
                + betah(A,H)*(EH(H) - SUM(CP, PQ(CP)*gammam(CP,H)) - SUM(AP, PA(AP)*gammah(AP,H))) ;

 INVDEM(C)$( qbarinv(C))..  QINV(C) =E= IADJ*qbarinv(C);

 GOVDEM(C)$( qbarg(C))..  QG(C) =E= GADJ*qbarg(C);

 YGDEF.. YG =E= SUM(INSDNG, TINS(INSDNG)*YI(INSDNG))
           + SUM(F, tf(F)*YF(F)) + SUM((A,RG), tva(A,RG)*PVA(A,RG)*QVA(A,RG))
           + SUM((A,RG), TA(A,RG)*PAR(A,RG)*QAR(A,RG)) + SUM(C, tm(C)*pwm(C)*QM(C))*EXR
           + SUM(C, te(C)*pwe(C)*QE(C))*EXR + SUM(C, TQ(C)*PQ(C)*QQ(C))
           + SUM((F,GOV), YIF(GOV,F)) + SUM((GOV, ROW), trnsfr(GOV,ROW))*EXR;

 EGDEF..
   EG =E= SUM(C, PQ(C)*QG(C)) + SUM((INSDNG, GOV), trnsfr(INSDNG,GOV))*CPI;

*System constraint block

 FACEQUIL(F)$QFS0(F)..
   SUM((A,RG), QF(F,A,RG)) =E= QFS(F);

 COMEQUIL(C)..
   QQ(C) =E= SUM(A, QINT(C,A)) + SUM(H, QH(C,H)) + QG(C)
                 + QINV(C) + qdst(C) + QT(C);

 CURACCBAL..
   SUM(C, pwm(C)*QM(C)) + SUM((F,ROW), trnsfr(ROW, F)) + SUM(F, (1-tf(f))*rf(F)*YF(F))/EXR =E=
   SUM(C, pwe(C)*QE(C)) + SUM((INSD, ROW), trnsfr(INSD, ROW)) + FSAV;

 GOVBAL.. YG =E= EG + GSAV;

 TINSDEF(INSDNG)..
   TINS(INSDNG) =E= tinsbar(INSDNG)*(1 + TINSADJ*tins01(INSDNG)) + DTINS*tins01(INSDNG);

 MPSDEF(INSDNG)..
   MPS(INSDNG)  =E= mpsbar(INSDNG)*(1 + MPSADJ*mps01(INSDNG)) + DMPS*mps01(INSDNG);

 SAVINVBAL..
   SUM(INSDNG, MPS(INSDNG) * (1 - TINS(INSDNG)) * YI(INSDNG)) + GSAV + FSAV*EXR =E=
   SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C)) + WALRAS;

 TABSEQ..
  TABS =E=
   SUM((C,H), PQ(C)*QH(C,H)) + SUM((A,H), PA(A)*QHA(A,H)) +
   SUM(C, PQ(C)*QG(C)) + SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C));

 INVABEQ.. INVSHR*TABS =E= SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C));

 GDABEQ..  GOVSHR*TABS =E= SUM(C, PQ(C)*QG(C));


*--------------------------------------------------------------------------------------------
*6. MODEL
*--------------------------------------------------------------------------------------------

MODEL STANDCGE  standard CGE model /
*Price block
 PMDEF.PM
 PEDEF.PE
 PQDEF
 PXDEF
 PXDEF2
 PDDDEF.PDD
 PADEF.PA
 PINTADEF.PINTA
 PVADEF.PVA
 CPIDEF
 DPIDEF
*Production and trade block
 QADEF.QA
 PADEF2.PAR
 QACES
 QACESFOC
 LEOAGGINT
 LEOAGGVA
 CESVAPRD.QVA
 CESVAFOC
 INTDEM.QINT
 COMPRDFN1
 COMPRDFN2
 OUTAGGFN.QX
 OUTAGGFOC.QXAC
 CET
 CET2
 ESUPPLY
 EXPRESID1
 EXPRESID2
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM.QT
*Institution block
 YFDEF.YF
 YIFDEF.YIF
 YIDEF.YI
 EHDEF.EH
 TRIIDEF.TRII
 HMDEM.QH
 HADEM.QHA
 EGDEF.EG
 YGDEF.YG
 GOVDEM.QG
 GOVBAL
 INVDEM.QINV
*System-constraint block
 FACEQUIL
 COMEQUIL
 CURACCBAL
 SAVINVBAL.WALRAS
 TINSDEF.TINS
 MPSDEF.MPS
 TABSEQ.TABS
 INVABEQ
 GDABEQ
 LEONFAC1
 LEONFAC2
/;

***************** for 3dynamic_part
PARAMETERS
 alphavagr(A, RG, MM_T)    growth of alphava (sector-specific)
 pwe_gr(C, MM_T)           growth of pwe
 pwm_gr(C, MM_T)           growth of pwm
 QFSgr(F, MM_T)            growth of QFS
 hpopgr(H, MM_T)           growth of hpop
 FSAVgr(MM_T)              growth of foreign savings
*Macroeconomic closures
 NUMERAIRE            numeraire
 SICLOS               value for savings-investment closure
 ROWCLOS              value for rest-of-world closure
 GOVCLOS              value for government closure
*Factor market closures
 FMOBFE(F)             factor is fully employed and mobile
 FACTFE(F)             factor is fully employed and activity-specific
 FMOBUE(F)             factor is unemployed and mobile

* other par
 GDP                   current GDP value (billions CFA)
 GDPS(A,RG)            GDP share of A - current
 GDPS0(A,RG)           GDP share of A - initial, constant
 alphavaECONgr         TFP growth (economywide)
 trnsfr_def(INS,AC)
 hpopprev
 CPIX(MM_T)                       consumer price index (PQ-based)
 DPIX(MM_T)                       index for domestic producer prices (PDS-based)
 EGX(MM_T)                        total current government expenditure
 EHX(H,MM_T)                      household consumption expenditure
 EXRX(MM_T)                       exchange rate
 GSAVX(MM_T)                      government savings
 FSAVX(MM_T)                      foreign savings
 GADJX(MM_T)                      government demand scaling factor
 GOVSHRX(MM_T)                    govt consumption share of absorption
 IADJX(MM_T)                      investment scaling factor (for fixed capital formation)
 INVSHRX(MM_T)                    total investment share of absorption
 MPSX(INS,MM_T)                   marginal propensity to save for dom non-gov inst ins
 MPSADJX(MM_T)                    savings rate scaling factor
 PAX(A,MM_T)                      output price of activity a
 PARX(A,RG,MM_T)                  output price of activity a
 PDDX(C,MM_T)                     demand price for com'y c produced & sold domestically
 PDSX(C,MM_T)                     supply price for com'y c produced & sold domestically
 PEX(C,MM_T)                      price of exports
 PINTAX(A,RG,MM_T)                price of intermediate aggregate
 PMX(C,MM_T)                      price of imports
 PQX(C,MM_T)                      price of composite good c
 PVAX(A,RG,MM_T)                  value added price
 PWEX(C,MM_T)                     world price of exports
 PWMX(C,MM_T)                     world price of imports
 PXX(C,MM_T)                      average output price
 PXACX(A,C,MM_T)                  price of commodity c from activity a
 QAX(A,MM_T)                      level of domestic activity
 QARX(A,RG,MM_T)                  level of domestic activity
 QDX(C,MM_T)                      qnty of domestic sales
 QEX(C,MM_T)                      qnty of exports
 QFX(F,A,RG,MM_T)                 qnty demanded of factor f from activity a
 QFSX(F,MM_T)                     qnty of factor supply
 QGX(C,MM_T)                      qnty of government consumption
 QHX(C,H,MM_T)                    qnty consumed of market commodity c by household h
 QHAX(A,H,MM_T)                   qnty consumed of home commodity c fr act a by hhd h
 QINTX(C,A,MM_T)                  qnty of intermediate demand for c from activity a
 QINTAX(A,RG,MM_T)                qnty of aggregate intermediate input
 QINVX(C,MM_T)                     qnty of fixed investment demand
 QMX(C,MM_T)                      qnty of imports
 QQX(C,MM_T)                      qnty of composite goods supply
 QTX(C,MM_T)                      qnty of trade and transport demand for commodity c
 QVAX(A,RG,MM_T)                  qnty of aggregate value added
 QXX(C,MM_T)                      qnty of aggregate marketed commodity output
 QXACX(A,C,MM_T)                  qnty of ouput of commodity c from activity a
 TABSX(MM_T)                      total absorption
 TINSX(INS,MM_T)                  rate of direct tax on domestic institutions ins
 TINSADJX(MM_T)                   direct tax scaling factor
 TAX(A,RG,MM_T)                   activity tax rate
 TMX(C,MM_T)                      import tariff rate
 TQX(C,MM_T)                      indirect sales tax rate
 TRIIX(INS,INSP,MM_T)             transfers to dom. inst. insdng from insdngp
 WALRASX(MM_T)                    savings-capital investment imbalance (should be zero)
 WFX(F,MM_T)                      economy-wide wage (rent) for factor f
 WFDISTX(F,A,RG,MM_T)             factor wage distortion variable
 YFX(F,MM_T)                      factor income
 YGX(MM_T)                        total current government income
 YIFX(INS,F,MM_T)                 income of institution ins from factor f
 YIX(INS,MM_T)                    income of (domestic non-governmental) institution ins
 alphavax(A,RG,MM_T)              TFP level (activity-specific)
 GDPSX(A,RG,MM_T)                 GDP share of A
 GDPX(MM_T)                       current GDP value (billions CFA)
 debtX(MM_T)                      external debt stock (billions CFA)
 alphavaECONgrX(MM_T)             TFP growth (economywide)

 CO2percapitaX(MM_T)
 hpopx(H,MM_T)        population of H in the year MM_T
   hpop_withoutx(H,MM_T)
 trnsfrX(INS,AC, MM_T);

*---------------for poverty module
SET
 HURB(H) urban households
 HRUR(H) rural households
 MM_T1(MM_T)
 MM_TN(MM_T)
 HID                      household ID in household survey
 MAPHIDH(HID,H)           final mapping between survey and model households
 PAC(AC)                  SAM accounts included in the poverty measure or line
;

PARAMETER
 POPGR(H,MM_T)
 P0             report poverty incidence (1 for yes)              / 1 /
 P1             report poverty depth (1 for yes)                  / 0 /
 P2             report poverty severity (1 for yes)               / 0 /
 MM_TNUM
*Core survey expenditure parameter
 HDATA(HID,AC)            survey household expenditure data
*Model and survey households mapping
 HIDINFO(HID,*)           household information
*Poverty measure calculations
 PCE(HID,AC,MM_T)       final per capita expenditure poverty measure
 HSIZE(HID,MM_T)        updated household sizes for poverty measure (may be adult eq)
 PSIZE(HID,MM_T)        updated household population sizes
 DHSIZE(HID)          dummy variable for household population loop
 DPSIZE(HID)          dummy variable for household population loop
 MODEXP(H,AC,MM_T)      expenditure changes for model households
 POVLINE(HID)             poverty line by survey household
*Poverty tables
 NATPOP(MM_T)           national population
 POOR(HID,MM_T)         household below poverty line
 POVTAB(*,*,MM_T)       final poverty table
 POORTAB(*,MM_T)        number of poor people
;
