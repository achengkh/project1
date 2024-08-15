create or replace package PKG_FAST_TRACK AS

FUNCTION GET_INMNO (
      vCIN		IN VARCHAR2
  ) RETURN NUMBER;

FUNCTION GET_ADMNO (
      vCIN		IN VARCHAR2
  ) RETURN NUMBER;  

FUNCTION GET_INSCODE (
      vCIN		IN VARCHAR2
  ) RETURN VARCHAR2;   

PROCEDURE CONVERT_CONVICT_REMAND (
      vCIN		IN VARCHAR2
  );

PROCEDURE CONVERT_CONVICT_SUPERVISEE (
      vCIN		IN VARCHAR2
  );

PROCEDURE CONVERT_BAILED (
      vCIN		IN VARCHAR2
  );  

PROCEDURE CONVERT_DISCHARGED (
      vCIN		IN VARCHAR2
  );  

PROCEDURE CONVERT_ESCAPE (
      vCIN		IN VARCHAR2
  );    

PROCEDURE CONVERT_NOT_RETURNING (
      vCIN		IN VARCHAR2
  );   

PROCEDURE CONVERT_OUTSIDE (
      vCIN		IN VARCHAR2
  );   

PROCEDURE CONVERT_TRANSFERRED (
      vCIN		IN VARCHAR2,
      vTO_INS		IN VARCHAR2
  ); 

PROCEDURE ACK_TRANSFERRED (
      vCIN		IN VARCHAR2
  );  

PROCEDURE TRANSFER (
      vCIN		IN VARCHAR2,
      vTO_INS		IN VARCHAR2
  );  

PROCEDURE CONVERT_REMAND_CONVICT (
      vCIN		IN VARCHAR2
  );   

PROCEDURE REQUEST_RECEIVE (
      vCIN		IN VARCHAR2,
      vFROM_CIN		IN VARCHAR2
  ); 

PROCEDURE REQUEST_SEND (
      vCIN		IN VARCHAR2,
      vTO_CIN		IN VARCHAR2
  );   

PROCEDURE INMATE_REFUSE_INMATE (
      vCIN		IN VARCHAR2,
      vREFUSED_CIN		IN VARCHAR2
  );   

PROCEDURE INMATE_REFUSE_OUTSIDER (
      vCIN		IN VARCHAR2,
      vOUTSIDER		IN VARCHAR2
  );   

PROCEDURE OUTSIDER_REFUSE_INMATE (
      vOUTSIDER		IN VARCHAR2,
      vOUTSIDER_ADDRESS IN VARCHAR2,
      vREFUSED_CIN		IN VARCHAR2
  );  

PROCEDURE REQUEST_SEM (
      vCIN		IN VARCHAR2
  );  

PROCEDURE REQUEST_QAH (
    vINSCODE IN VARCHAR2,
      vCORR		IN VARCHAR2
  );  

PROCEDURE SPECIAL_CARE_INM (
      vCIN		IN VARCHAR2
  );   

PROCEDURE SPECIAL_CARE_OUM (
      vCIN		IN VARCHAR2
  );    

PROCEDURE MAIL_RECEIVE (
      vCIN		IN VARCHAR2
  );   
PROCEDURE MAIL_SEND (
      vCIN		IN VARCHAR2
  );   
  
PROCEDURE MAIL_PATCH_COLLECT_DT (
      vCIN		IN VARCHAR2,
      vFM_DT    IN VARCHAR2,
      vTO_DT    IN VARCHAR2
  );    
  
FUNCTION PIC_STATUS_DATE (
      vCIN		IN VARCHAR2,
      vDATE IN VARCHAR2
  )RETURN T_INM_INMATE.INM_STATUS%TYPE;   

PROCEDURE OLD_DATE_OUT_T_WOR (
      P_RUN_DATE		IN Date,
      REFERENCE_DATE IN VARCHAR2,
      FM_DAY		IN Number,
      TO_DAY		IN Number
  ); 


FUNCTION GET_EWS_WORKSHOP_CODE(
	JOB_CODE IN T_WOR_WORK.WOP_PA_JOB_CODE%type
	) RETURN T_EWS_EV_WORKSHOP.EVW_WORKSHOP_CODE%TYPE;  

PROCEDURE DATE_OUT_T_WOR_DEL_T_EWS (
      FM_DT		IN Date,
      TO_DT		IN Date
  ); 

PROCEDURE DATE_OUT_T_WOR (
      P_RUN_DATE		IN Date,
      REFERENCE_DATE IN VARCHAR2,
      FM_DAY		IN Number,
      TO_DAY		IN Number
  );   

PROCEDURE DATE_OUT_T_WOR_FALLBACK (
      vRUN_DT		IN VARCHAR2
  );   

PROCEDURE MOBILE_MOVE(
      vMOBILE_ID		IN VARCHAR2,
      vTO_INS_CODE IN  VARCHAR2
  );  

PROCEDURE ASSIGN_MOBILE_ROLE (
      vMACADDRESS		IN VARCHAR2,
      vMOBILE_ROLE IN  VARCHAR2
  );   
PROCEDURE ASSIGN_USER_ROLE (
      vUSRID		IN VARCHAR2,
      vINSCODE IN VARCHAR2,
      vUSER_ROLE IN  VARCHAR2,
      vUSI_ADMIN_LEVEL IN T_Z_USI_USER_INST.USI_ADMIN_LEVEL%TYPE
  );   

PROCEDURE COPY_USER_ROLE (
      vUSRID		IN VARCHAR2,
      vFROM_USRID IN VARCHAR2
  );   

PROCEDURE CREATE_USER (
      vUSRID		IN VARCHAR2,
      vURRID    IN VARCHAR2,
      vINSCODE  IN VARCHAR2
  );    

PROCEDURE URINE_REQUEST (
      vCIN		IN VARCHAR2,
      vGOVT   IN VARCHAR2
  );   
PROCEDURE URINE_PRINT_ESCORT (
      vTRE_REQ_SER_NO		IN VARCHAR2
  );    
PROCEDURE URINE_PRINT_LABEL (
      vTRE_REQ_SER_NO		IN VARCHAR2
  );    
PROCEDURE URINE_SUBMIT (
      vTES_TEST_NO		IN VARCHAR2
  );  

PROCEDURE URINE_RECEIVE (
      vURB_LOG_NO		IN VARCHAR2
  );    
PROCEDURE URINE_PREP_TEST (
      vTES_TEST_NO		IN VARCHAR2
  );   
PROCEDURE URINE_CONF_TEST (
      vTES_TEST_NO		IN VARCHAR2
  );    

PROCEDURE URINE_TEST_RESULT (
      vTES_TEST_NO		IN VARCHAR2,
      vRESULT IN VARCHAR2
  );   

function timediff(
   ts1   in timestamp,
   ts2   in timestamp,
   units in varchar2)
/* units - ms=millisecond ss=second, mi=minute, hr=hour, dy=day */
return number;  

PROCEDURE MISSED_T_IMF (
      vIML_ID		IN T_IML_IN_MAIL.IML_ID%TYPE
  ); 

PROCEDURE MISSED_T_IMF_LAST (
      vCollectDate		IN VARCHAR2,
      vInsCode IN VARCHAR2
  );  

PROCEDURE T_OML_BLANK_ADM_NO (
      vOML_ID		IN T_OML_OUT_MAIL.OML_ID%TYPE
  ); 

PROCEDURE T_OML_BLANK_ADM_NO_DATE (
      vCollectDate		IN VARCHAR2,
      vInsCode IN VARCHAR2
  );  

PROCEDURE PATCH_IML_PRN_FOR_SORTING ;  
PROCEDURE PATCH_OML_PRN_FOR_SORTING ;  

PROCEDURE DAILY_DISCHARGED_TRANSFERRED_BAILED_NOT_RETURNING ; 

PROCEDURE EXPLODE (
--123,456,789
      vIMPLODED_STRING		IN VARCHAR2,
      vDELIMITER IN VARCHAR2
  );   

PROCEDURE EXPLODE_MAX (
--123,456,789
      vIMPLODED_STRING		IN VARCHAR2,
      vDELIMITER IN VARCHAR2,
      vMaximum IN NUMBER
  );     

  FUNCTION GET_EOG_ORG_CODE (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type
  )RETURN T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%TYPE;

    FUNCTION GET_EOS_SUBJECT_CODE (
  vEOG_ORG_CODE IN T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%TYPE,
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type
  )RETURN T_EPE_EV_ORG_PERIOD.EOS_SUBJECT_CODE%TYPE;

PROCEDURE EDU_014_NEW_COURSE_EXAM (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );

FUNCTION T_EPE_INSERT (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vEV_FLAG IN T_EPE_EV_ORG_PERIOD.EV_FLAG%TYPE,
      vFM_DT IN VARCHAR2
  ) RETURN T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE;  

PROCEDURE EDU_041_NEW_COURSE_EXAM_ENROLL (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE T_EVE_INSERT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vEV_FLAG IN T_EPE_EV_ORG_PERIOD.EV_FLAG%TYPE
  );   

PROCEDURE EDU_042_NEW_COURSE_EXAM_PERFORMANCE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE T_EVE_UPDATE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vEV_FLAG IN VARCHAR2
  );  

PROCEDURE EDU_030_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vFUND_TYPE IN  T_EFF_EV_FUND_EXAM.eft_fund_type%type
  ); 

PROCEDURE EDU_030_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE EDU_033_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE EDU_033_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE EDU_061_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE EDU_061_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
	  vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vFUND_TYPE IN T_EFF_EV_FUND_EXAM.eft_fund_type%type
  );  

PROCEDURE EDU_062_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  ); 

PROCEDURE EDU_062_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE EDU_063_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE EDU_063_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
	  vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vFUND_TYPE IN T_EFF_EV_FUND_EXAM.eft_fund_type%type
  );     

PROCEDURE EDU_064_COURSE_EXAM_FUND_ENDORSE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE EDU_034_COURSE_EXAM_FUND_COMPLETE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE EDU_COURSE_EXAM_FUND_COMPLETE_4_PIC (
      vCIN_1 IN t_inm_inmate.inm_cin%type,
      vCIN_2 IN t_inm_inmate.inm_cin%type,
	  vCIN_3 IN t_inm_inmate.inm_cin%type,
      vCIN_4 IN t_inm_inmate.inm_cin%type,
	  vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type
  );     

PROCEDURE EDU_005_WAIT_FOR_CLASS_ALLOC (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EVS_EV_CASE.edu_ins_code%type
);

PROCEDURE EDU_002_ATTAINMENT_TEST_PERFORMANCE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EVD_EV_CASE_DETAILS.evd_ins_code%type,
      vCLASS IN VARCHAR2,
      vTEST_DT IN VARCHAR2
);

PROCEDURE EDU_006_CLASS_ALLOC (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EVD_EV_CASE_DETAILS.evd_ins_code%type,
      vCLASS IN VARCHAR2
);

FUNCTION EDU_059_SAVE (
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )RETURN T_Z_EVP_EV_PERIOD.EVP_ID%TYPE;

PROCEDURE VTR_075_NEW_COURSE_EXAM (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );

PROCEDURE VTR_004_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE VTR_035_EVALUATE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  ); 

PROCEDURE VTR_050_WITHDRAW (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE VTR_051_WITHDRAW_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );    

PROCEDURE VTR_004_REJECT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );     

PROCEDURE VTR_006_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE VTR_006_RECOMMEND (
      --vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vUSR_RECOMMEND IN VARCHAR2
  );



PROCEDURE VTR_008_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE VTR_025_APPTITUDE_TEST (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vTEST_DT IN VARCHAR2
  );   

PROCEDURE VTR_012_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  ); 

PROCEDURE VTR_013_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE VTR_013_RECOMMEND (
      --vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,  
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vUSR_RECOMMEND IN VARCHAR2
  );  

PROCEDURE VTR_014_RECOMMEND_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );    

PROCEDURE VTR_015_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  



PROCEDURE VTR_016_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE VTR_019_APPEAL (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE VTR_020_APPEAL_RECOMMEND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE VTR_021_APPEAL_GRANTED (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );  

PROCEDURE VTR_022_ENROLL (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );    

PROCEDURE VTR_023_PERFORMANCE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  );   

PROCEDURE VTR_086_EMPLOYMENT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vTO_DT IN VARCHAR2
  ); 


FUNCTION T_SUC_INSERT (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_SUC_SUP_CASE.SUC_CASE_NO%TYPE; 


PROCEDURE T_SEM_INSERT (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  ); 
  
PROCEDURE VTR_001_4_ADD (
      vJOB_CODE IN T_WOR_WORK.WOP_PA_JOB_CODE%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  );  
  
PROCEDURE IND_003_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );  
  
PROCEDURE IND_004_SAVE (
      vUSR_ID IN t_wvt_wvtab.usr_id%type,
      vWVT_MEMBER_TYPE IN varchar2,
      vINS_CODE IN t_wvt_wvtab.ins_code%type
  );   
  
PROCEDURE IND_005_RECOMMEND (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );    
  
PROCEDURE IND_006_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );   
  
PROCEDURE IND_018_SAVE (
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vWOL_LOC_DESC IN T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  );   
  
FUNCTION GET_WOL_LOC_DESC (
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;   
  
FUNCTION GET_WOC_CODE (
      vWOL_LOC_DESC IN T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_Z_WOC_WORKSHOP_CODE.WOC_CODE%type;     
  
FUNCTION GET_WOCCODE (
      vWOL_LOCAT IN T_Z_WOC_WORKSHOP_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_Z_WOC_WORKSHOP_CODE.WOC_CODE%type;    
  
FUNCTION GET_WOP_PA_JOB_CODE (
      vWOL_LOCAT IN T_Z_WOC_WORKSHOP_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return t_z_tjm_trade_job_mapping.WOP_PA_JOB_CODE%type;      
  
PROCEDURE IND_021_REQUEST (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );     
  
PROCEDURE IND_022_RECOMMEND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );   
  
PROCEDURE IND_022_2_VETTING (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );    
  
PROCEDURE IND_023_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );    
  
PROCEDURE IND_030_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );   
  
PROCEDURE IND_031_CONFIRM (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );  
  
PROCEDURE IND_031_2_RECOMMEND (
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vUSR_RECOMMEND IN VARCHAR2
  ); 
  
PROCEDURE IND_031_3_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );  
  
PROCEDURE IND_033_TOOL_SAVE (
      vTOL_SERIAL_NO IN t_tol_workshop_tool.TOL_SERIAL_NO%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  );   
  
PROCEDURE IND_035_TOOL_CHECKOUT (
      vUSR_ID IN t_tio_workshop_tool_chk_in_out.tio_chk_out_by_usr_id%type,
      vTOL_SERIAL_NO IN t_tol_workshop_tool.TOL_SERIAL_NO%type,
      --vTOL_EID IN t_tio_workshop_tool_chk_in_out.TOL_EID%type,
      vFM_DT IN varchar2
  );   
  
PROCEDURE IND_036_TOOL_CHECKIN (
      vUSR_ID IN t_tio_workshop_tool_chk_in_out.tio_chk_out_by_usr_id%type,
      vTOL_SERIAL_NO IN t_tol_workshop_tool.TOL_SERIAL_NO%type,
      --vTOL_EID IN t_tio_workshop_tool_chk_in_out.TOL_EID%type,
      vFM_DT IN varchar2
  );   
    
PROCEDURE IND_040_TOOL_VERIFY (
      vWOC_CODE IN t_wvh_ws_tool_verify_header.WOC_CODE%type,
	  vTOOL_BOX_CODE IN t_tol_workshop_tool.TOOL_BOX_CODE%type,
      vINS_CODE IN t_wvh_ws_tool_verify_header.INS_CODE%type,      
      vTASK IN t_wvh_ws_tool_verify_header.wvh_type%type,
      vFM_DT IN varchar2,
      vUSR_ID IN t_wvh_ws_tool_verify_header.wvh_req_usr_id%type      
  );       

PROCEDURE IND_040_2_TOOL_VERIFY_CONFIRM (
      vWOC_CODE IN t_wvh_ws_tool_verify_header.WOC_CODE%type,
	  vTOOL_BOX_CODE IN t_tol_workshop_tool.TOOL_BOX_CODE%type,
      vINS_CODE IN t_wvh_ws_tool_verify_header.INS_CODE%type,      
      vTASK IN t_wvh_ws_tool_verify_header.wvh_type%type,
      vFM_DT IN varchar2,
      vUSR_ID IN t_wvh_ws_tool_verify_header.wvh_req_usr_id%type,      
      vCONFIRM_USR_ID IN t_wvh_ws_tool_verify_header.wvh_confirm_usr_id%type  
  ); 

END PKG_FAST_TRACK;