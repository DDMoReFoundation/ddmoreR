$PROBLEM Warfarin_PK_ODE in 0.3.1

$DATA warfarin_conc.csv IGNORE=@ 
; The IGNORE= statement will use the first character in the header row 
; of the dataset. If this character is alphabetical the @ symbol can be set. 
; If there is no header row there should be no IGNORE=<character> option on 
; the $DATA statement.

$INPUT ID TIME WT AMT DVID DV MDV LOGTWT

$SUBS ADVAN13 TOL=9 ; Hardcoded for DE problems

$MODEL
COMP (GUT)
COMP (CENTR)

$PK

; Population parameter definitions

  POP_TLAG=THETA(1)
  POP_KA=THETA(2)
  POP_V=THETA(3)
  POP_CL=THETA(4)
  BETA_V_WT=THETA(5)
  BETA_CL_WT=THETA(6)

; Individual parameter definitions
  
  TLAG=POP_TLAG*EXP(ETA(1))
  KA=POP_KA*EXP(ETA(2))
  V=POP_V*EXP(BETA_V_WT*LOGTWT)*EXP(ETA(3))
  CL=POP_CL*EXP(BETA_CL_WT*LOGTWT)*EXP(ETA(4))
  
  ; I am hoping to include the below addition that will enable the 
  ; mu referencing feature for EM methods in Nonmem. Unfortunately
  ; I'm not yet sure how to handle cases where the PharmML breaks 
  ; Nonmem rules for mu referencing.
  ;
  ; MU_1=LOG(POP_TLAG)
  ; MU_2=LOG(POP_KA)
  ; MU_3=LOG(POP_V)
  ; MU_4=LOG(POP_CL)
  
$DES
  
  IF(TIME.GT.TLAG) THEN
    RATEIN=A(1)*KA
  ELSE
    RATEIN=0
  ENDIF
  
  DADT(1)=-RATEIN
  DADT(2)=RATEIN-(CL*(A(2)/V))
  
$ERROR

  CC=A(2)/V
  
; Hardcoded CombinedError1 with the mapping of f to CC, additive to RUV_ADD and
; proportional to RUV_PROP

   IPRED = CC+0.00001
   IF(CC.GT.0) IPRED = CC
   W = THETA(7)+THETA(8)*IPRED
   Y = CC+W*EPS(1)
   IPRED = CC
   IRES = DV - IPRED
   IWRES = IRES / W
  
$THETA
  1             ; POP_TLAG
  0.362         ; POP_KA
  8             ; POP_V
  0.1           ; POP_CL
  1 FIX         ; BETA_V_WT
  0.75 FIX      ; BETA_CL_WT
  0.1           ; RUV_ADD
  0.1   	    ; RUV_PROP
  
$OMEGA
  0.1 SD        ; PPV_TLAG 
  0.1 SD        ; PPV_KA

; Blocks are now written out line by line to satisfy output parser requirements.
  
$OMEGA BLOCK(2) SD CORRELATION
  0.1           ; PPV_V
  0.01          ; CORR_CL_V
  0.1           ; PPV_CL
  
$SIGMA
  1 FIX 

; Hardcoded SAEM method
  
;$ESTIMATION METHOD=SAEM INTER NBURN=2000 NITER=100 ISAMPLE=2 IACCEPT=0.4 PRINT=10 CTYPE=3

$EST METHOD=COND INTER MAXEVALS=9999 PRINT=10

 
; Table output logic not yet fully defined. It is being reworked for xpose compatibility. 

$TABLE ID TIME DV IPRED Y IRES IWRES  FILE=sdtab

$TABLE ID TIME TLAG KA V CL FILE=patab NOAPPEND

$TABLE ID TIME WT LOGTWT FILE=cotab NOAPPEND

