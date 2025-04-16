$call "gdxxrw i=0.data.xlsx o=tmp/maindata.gdx index=index!a6"
$ONEMPTY

SETS
  vars                  all vars of interest
  static(vars)          static inputs
  dynamic(vars)         dynamic inputs
  hZ(vars)              policy goals (output vars)
  MM_X  all simulations
  MM_XC(MM_X) active simulations
  MM_XNC(MM_X) not active simulations
  MM_T time periods
;
ALIAS (MM_T, MM_TP);

Singleton Set MM_XCURR(MM_X);

PARAMETERS
  mm_inputs(MM_X, vars) all inputs
  mm_I_static(static) current static inputs (for loop)
  mm_I_dynamic(dynamic) current dynamic inputs (for loop)
  mm_suc(MM_T, vars) current vars for which sim was successful
  data(MM_X, MM_T, vars) output file
;

$gdxin "tmp/maindata.gdx"

$load vars static dynamic hZ MM_T MM_X mm_inputs

  MM_XC(MM_X)= YES;
MM_XNC(MM_X) = YES$(NOT MM_XC(MM_X));
mm_inputs(MM_XNC, vars) = NO;

$include DCGEcore/0.declarations.gms
