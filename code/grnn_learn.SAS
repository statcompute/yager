%macro grnn_learn(data = , x = , y = , sigma = , nn_out = );
options mprint mlogic nocenter;
********************************************************;
* THIS MACRO IS TO TRAIN A GENERAL REGRESSION NEURAL   *;
* NETWORK (SPECHT, 1991) AND STORE THE SPECIFICATION   *;
*------------------------------------------------------*;
* INPUT PARAMETERS:                                    *;
*  DATA  : INPUT SAS DATASET                           *;
*  X     : A LIST OF PREDICTORS IN THE NUMERIC FORMAT  *;
*  Y     : A RESPONSE VARIABLE IN THE NUMERIC FORMAT   *;
*  SIGMA : THE SMOOTH PARAMETER FOR GRNN               *;
*  NN_OUT: OUTPUT SAS DATASET CONTAINING THE GRNN      *;
*          SPECIFICATION                               *;
*------------------------------------------------------*;
* AUTHOR:                                              *;
*  WENSUI.LIU@53.COM                                   *;
********************************************************;
 
data _tmp1;
  set &data (keep = &x &y);
  where &y ~= .;
  array _x_ &x;
  _miss_ = 0;
  do _i_ = 1 to dim(_x_);
    if _x_[_i_] = . then _miss_ = 1; 
  end;
  if _miss_ = 0 then output;
run;
 
proc summary data = _tmp1;
  output out = _avg_ (drop = _type_ _freq_)
  mean(&x) = ;
run;
 
proc summary data = _tmp1;
  output out = _std_ (drop = _type_ _freq_)
  std(&x) = ;
run;
 
proc standard data = _tmp1 mean = 0 std = 1 out = _data_;
  var &x;
run;
 
data &nn_out (keep = _neuron_ _key_ _value_);
  set _last_ end = eof;
  _neuron_ + 1;
  length _key_ $32;
  array _a_ &y &x;
  do _i_ = 1 to dim(_a_);
    if _i_ = 1 then _key_ = '_Y_';
    else _key_ = upcase(vname(_a_[_i_]));
    _value_ = _a_[_i_];
    output;
  end; 
  if eof then do;
    _neuron_ = 0;
    _key_  = "_SIGMA_";
    _value_  = &sigma;
    output;
    set _avg_;
    array _b_ &x;
    do _i_ = 1 to dim(_b_);
      _neuron_ = -1;
      _key_ = upcase(vname(_b_[_i_]));
      _value_ = _b_[_i_];
      output;
    end;
    set _std_;
    array _c_ &x;
    do _i_ = 1 to dim(_c_);
      _neuron_ = -2;
      _key_ = upcase(vname(_c_[_i_]));
      _value_ = _c_[_i_];
      output;
    end;
  end;
run;
 
proc datasets library = work;
  delete _: / memtype = data;
run;
quit;
 
********************************************************;
*              END OF THE MACRO                        *;
********************************************************;
%mend grnn_learn;
