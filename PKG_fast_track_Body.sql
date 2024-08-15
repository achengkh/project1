create or replace package body PKG_FAST_TRACK AS

FUNCTION GET_INMNO (
      vCIN		IN VARCHAR2
  ) RETURN NUMBER
IS
vINM NUMBER;
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN

select inm_no into vINM from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';

Return vINM;


EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_INMNO - No record','vERM:'||vERM,vERR);    
    commit;
    WHEN OTHERS THEN
                vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_INMNO - Other record','vERM:'||vERM,vERR);    
    commit;
END GET_INMNO;

FUNCTION GET_ADMNO (
      vCIN		IN VARCHAR2
  ) RETURN NUMBER
IS
vADM NUMBER;
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN

select adm_no into vADM from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';

Return vADM;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_ADMNO - No record','vERM:'||vERM,vERR);    
    commit;
    WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_ADMNO - Other record','vERM:'||vERM,vERR);    

    commit;
END GET_ADMNO;

FUNCTION GET_INSCODE (
      vCIN		IN VARCHAR2
  ) RETURN VARCHAR2
IS
vINSCODE VARCHAR2(8);
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN

select ins_code into vINSCODE from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';

Return vINSCODE;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_INSCODE - No record','vERM:'||vERM,vERR);    
    commit;
    WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_INSCODE - Other record','vERM:'||vERM,vERR);  

    commit;
END GET_INSCODE;

PROCEDURE  CONVERT_CONVICT_REMAND (
		vCIN		IN VARCHAR2
  )
  IS
vINM NUMBER;
vADM NUMBER;
vNUM NUMBER;
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN
--Not Remand
IF INSTR(vCIN, '-')=0 THEN
BEGIN
vINM := GET_INMNO( vCIN);
vADM := GET_ADMNO( vCIN);
vNUM:=PKG_ADMISSION.GEN_REMAND_NO(to_number(to_char(sysdate,'yy')));
update t_inm_inmate set inm_status='D', inm_curr_ind='N', inm_con_dis_da_dt=trunc(sysdate), inm_con_edd_dt=trunc(sysdate), inm_con_ldd_dt=trunc(sysdate) where inm_no=vINM and inm_curr_ind='Y';

insert into t_inm_inmate (inm_no, adm_no, inm_curr_ind, inm_rem_ser_no, inm_rem_yr, bas_con_adm_no, inm_cin, inm_drug_dep, inm_status, clc_code, cad_code, ins_code, inm_par_on, inm_cum_earn, inm_cum_co_sa, inm_cum_pun, inm_cum_ca_pu, inm_sent_year, inm_sent_mon, inm_sent_day,  inm_loc_ind, inm_work_ind, inm_prev_status, inm_prop_sta, inm_act_status, inm_loss_remission, inm_edd_confm_user, inm_rem_adm_da_dt, inm_prop_inst, inm_id_csd_kept, inm_dup_rem_prd, inm_id_perm_checked, inm_soc_vis_quo, inm_sent_fine, created_ts,  created_by) select inm_no, adm_no+1, 'Y', vNUM, to_char(sysdate,'yy'), 0, 0, inm_drug_dep, 'P', clc_code, cad_code, ins_code, inm_par_on, 0, 0, 0, 0, 0, 0, 0, 'N', 'N', null, 'O', null, inm_loss_remission, null, trunc(sysdate), inm_prop_inst, 'Y', 'N', 'Y', 1, 0, systimestamp, created_by from t_inm_inmate where inm_no=vINM and inm_curr_ind='N' and adm_no=vADM;
EXCEPTION
	WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'CONVERT_CONVICT_REMAND - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'CONVERT_CONVICT_REMAND - Other record','vERM:'||vERM,vERR);    
    commit;

END;
END IF;
END CONVERT_CONVICT_REMAND;

PROCEDURE  CONVERT_CONVICT_SUPERVISEE (
		vCIN		IN VARCHAR2
  )
  IS
vINM NUMBER;
vINSCODE VARCHAR2(5);
vCAS_NO_SER NUMBER;
vIST_TEAM_CODE VARCHAR2(5);
vPROG_CODE VARCHAR2(5);
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN
--Not Remand
IF INSTR(vCIN, '-')=0 THEN
BEGIN
vINM := GET_INMNO( vCIN);
vINSCODE := GET_INSCODE( vCIN);
select nvl(max(cas_no_ser),0)+1 into vCAS_NO_SER from t_cas_case where cas_no_court='4796' and cas_year=to_char(sysdate,'yyyy');

select t.ist_team_code into vIST_TEAM_CODE from t_tmu_team_user t, t_inm_inmate i where i.inm_no=vINM and t.ins_code=i.ins_code and t.usr_id=i.created_by and t.tmu_status='A' and i.inm_curr_ind='Y';

select p.prog_code into vPROG_CODE from T_BAS_INMATE_BASIC b,t_inm_inmate i left join T_SPP_SUP_PRIS_PARM p on i.ins_code=p.ins_code and i.clc_code=p.clc_code1 where i.inm_no=vINM and i.inm_curr_ind='Y' and i.inm_no=b.inm_no;
if vPROG_CODE is null then 

select prog_code into vPROG_CODE from 
(SELECT prog_code 
FROM T_SPP_SUP_PRIS_PARM where ins_code=vINSCODE
GROUP BY ins_code, prog_code
ORDER BY COUNT(*) DESC) c where rownum<2;
End if;
update t_inm_inmate set inm_status='D', inm_con_adm_da_dt=trunc(sysdate), inm_con_dis_da_dt=trunc(sysdate), inm_con_edd_dt=trunc(sysdate),  inm_con_ldd_dt=add_months(trunc(sysdate),12), inm_con_sdd_dt=trunc(sysdate), inm_edd_confm_date_dt=trunc(sysdate), inm_super_date_dt=add_months(trunc(sysdate),12), inm_super_order=vPROG_CODE where inm_no=vINM and inm_curr_ind='Y';

insert into t_cas_case (inm_no, adm_no, cas_ser_no, cas_no_court, cas_no_ser, cas_year, cas_cng_ser_no, cas_type, cas_pri_ind, cot_code, cas_sent_type, cas_sent_year, cas_sent_mon, cas_sent_day, cas_curr_con_adm, cas_merge_ind, cas_loss_appeal, cas_days_paid, cas_date_of_sent_dt, cas_fine_amt, created_ts, created_by) select inm_no, adm_no, 1, '4796', vCAS_NO_SER, to_char(sysdate,'yyyy'), 0, 'S', 'Y', 'TWDC', 'DA', 0, 0, 0, adm_no, 'N', 0, 0, trunc(sysdate), 0, systimestamp, created_by from t_inm_inmate where inm_no=vINM and inm_curr_ind='Y';

insert into t_suc_sup_case(suc_case_no, inm_no, adm_no, bas_con_adm_no, inm_cin, team_inst_code, ist_team_code, pgm_code, age_code, clc_code, suc_name, in_out, birth_code, reli_code, suc_year_arr_hk, nati_code, mari_code, educ_code, cas_no_court, cas_no_ser, cas_year, cas_date_of_sent_dt, inm_con_dis_da_dt, suc_status, assi_flag, appr_flag, ackn_flag, appr_dt, appr_user_id, created_ts, created_by) select seq_sup_case_no.nextval, inm_no, adm_no, 1, inm_cin, ins_code, vIST_TEAM_CODE,inm_super_order, 'AD',  clc_code, 'Supervisee', 'O', 1, 2, to_char(sysdate,'yyyy'), 12, 4, 1, '4796', vCAS_NO_SER, to_char(sysdate,'yyyy'), trunc(sysdate), trunc(sysdate), 'A', 'Y', 'Y', 'Y', trunc(sysdate), created_by, systimestamp, created_by from t_inm_inmate where inm_no=vINM and inm_curr_ind='Y';
EXCEPTION
	WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'CONVERT_CONVICT_SUPERVISEE - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'CONVERT_CONVICT_SUPERVISEE - Other record','vERM:'||vERM,vERR);  
    commit;
END;
END IF;
END CONVERT_CONVICT_SUPERVISEE;

PROCEDURE CONVERT_BAILED (
      vCIN		IN VARCHAR2
  ) 
  IS
vINM NUMBER;

BEGIN
vINM := GET_INMNO( vCIN);
update t_inm_inmate set inm_status='B' where inm_no=vINM and inm_curr_ind='Y';
insert into t_ino_in_out (inm_no, adm_no, ino_out_ser_no, ino_out_code, ino_out_ins_code, ino_date_out_dt, created_ts, created_by) select inm_no, adm_no, 1, 'B', ins_code, trunc(sysdate), systimestamp, created_by from t_inm_inmate where inm_no=vINM and inm_curr_ind='Y';

END CONVERT_BAILED;


PROCEDURE CONVERT_DISCHARGED (
      vCIN		IN VARCHAR2
  )  
  IS
vINM NUMBER;

BEGIN
vINM := GET_INMNO( vCIN);
--Convict
IF INSTR(vCIN, '-')=0 THEN

update t_inm_inmate set inm_status='D', inm_con_dis_da_dt=sysdate, inm_con_edd_dt=sysdate, inm_con_ldd_dt=sysdate where inm_no=vINM and inm_curr_ind='Y';
ELSE
update t_inm_inmate set inm_status='D', inm_rem_dis_da_dt=sysdate where inm_no=vINM and inm_curr_ind='Y';

END IF;


END CONVERT_DISCHARGED;

PROCEDURE CONVERT_ESCAPE (
      vCIN		IN VARCHAR2
  ) 
  IS
vINM NUMBER;

BEGIN
vINM := GET_INMNO( vCIN);
update t_inm_inmate set inm_status='E' where inm_no=vINM and inm_curr_ind='Y';

END CONVERT_ESCAPE;
PROCEDURE CONVERT_NOT_RETURNING(
      vCIN		IN VARCHAR2
  ) 
  IS
vINM NUMBER;

BEGIN
vINM := GET_INMNO( vCIN);
update t_inm_inmate set inm_status='N' where inm_no=vINM and inm_curr_ind='Y';
insert into t_ino_in_out (inm_no, adm_no, ino_out_ser_no, ino_out_code, ino_out_ins_code, ino_date_out_dt, created_ts, created_by) select inm_no, adm_no, 1, 'N', ins_code, trunc(sysdate), systimestamp, created_by from t_inm_inmate where inm_no=vINM and inm_curr_ind='Y';

END CONVERT_NOT_RETURNING;

PROCEDURE CONVERT_OUTSIDE(
      vCIN		IN VARCHAR2
  ) 
  IS
vINM NUMBER;

BEGIN
vINM := GET_INMNO( vCIN);
update t_inm_inmate set inm_status='Z' where inm_no=vINM and inm_curr_ind='Y';
insert into t_ino_in_out (inm_no, adm_no, ino_out_ser_no, ino_out_code, ino_out_ins_code, ino_date_out_dt, created_ts, created_by) select i.inm_no, i.adm_no, nvl(max(o.ino_out_ser_no),0)+1, 'E', i.ins_code, trunc(sysdate), systimestamp, 'ADMIN_MM' from t_inm_inmate i left join t_ino_in_out o on o.inm_no=i.inm_no and o.adm_no=i.adm_no where i.inm_no=vINM and i.inm_curr_ind='Y' group by i.ins_code, i.inm_no, i.adm_no;

END CONVERT_OUTSIDE;

PROCEDURE CONVERT_TRANSFERRED(
      vCIN		IN VARCHAR2,
      vTO_INS		IN VARCHAR2
  )
  IS
vINM NUMBER;
BEGIN
vINM := GET_INMNO( vCIN);
insert into t_tra_transfer (inm_no, adm_no, tra_ser_no, ins_code, tra_status, tra_from_inst, tra_date_dt, created_ts, created_by, updated_ts, updated_by) select i.inm_no, i.adm_no, nvl(max(t.tra_ser_no),0)+1, vTO_INS, 'T', i.ins_code, trunc(sysdate), systimestamp, 'ADMIN_MM', systimestamp, 'ADMIN_MM' from t_inm_inmate i left join t_tra_transfer t on t.inm_no=i.inm_no and t.adm_no=i.adm_no where i.inm_no=vINM and i.inm_curr_ind='Y' group by i.ins_code, i.inm_no, i.adm_no;
update t_inm_inmate set inm_status='T', updated_ts=systimestamp, updated_by='ADMIN_MM' where inm_no=vINM and inm_curr_ind='Y';
END CONVERT_TRANSFERRED;


PROCEDURE ACK_TRANSFERRED (
      vCIN		IN VARCHAR2
  )IS
vINM NUMBER;
vTO_INS VARCHAR2(6);
BEGIN
vINM := GET_INMNO( vCIN);
select ins_code into vTO_INS from t_tra_transfer where inm_no=vINM and updated_ts = (select max(updated_ts) from t_tra_transfer where inm_no=vINM );
update t_inm_inmate set inm_status='P', ins_code=vTO_INS, updated_ts=systimestamp, updated_by='ADMIN_MM' where inm_no=vINM and inm_curr_ind='Y';
END ACK_TRANSFERRED;

PROCEDURE TRANSFER (
      vCIN		IN VARCHAR2,
      vTO_INS		IN VARCHAR2
  )IS
BEGIN
CONVERT_TRANSFERRED(vCIN,vTO_INS);
ACK_TRANSFERRED(vCIN);
END TRANSFER;

PROCEDURE  CONVERT_REMAND_CONVICT (
		vCIN		IN VARCHAR2
  )
  IS
vINM NUMBER;
vBAS_CIN NUMBER;
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN
--Not Remand
IF INSTR(vCIN, '-')>0 THEN
BEGIN
vINM := GET_INMNO( vCIN);
vBAS_CIN:=FN_GEN_CIN;
update t_inm_inmate set bas_con_adm_no=nvl(bas_con_adm_no,0) + 1, inm_cin=vBAS_CIN, inm_con_adm_da_dt=trunc(sysdate), updated_ts=systimestamp, updated_by=created_by where inm_no=vINM and inm_curr_ind='Y';
update t_bas_inmate_basic set bas_cin=vBAS_CIN where inm_no=vINM;
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'CONVERT_REMAND_CONVICT - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'CONVERT_REMAND_CONVICT - Other record','vERM:'||vERM,vERR);  
    commit;    

END;
END IF;
END CONVERT_REMAND_CONVICT;


PROCEDURE REQUEST_RECEIVE (
      vCIN		IN VARCHAR2,
      vFROM_CIN		IN VARCHAR2
  )
  IS
  vINM NUMBER;
  vADM NUMBER;
  vINSCODE VARCHAR2(8);
  vMRQID NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100);
BEGIN  
  vADM := GET_ADMNO( vFROM_CIN);
  vINM := GET_INMNO( vFROM_CIN);
  vINSCODE := GET_INSCODE( vFROM_CIN);
  select nvl(max(mrq_id),0)+1 into vMRQID from t_mrq_request;
insert into T_MRQ_REQUEST(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, ins_code, mrq_start_date_dt, mrq_end_date_dt, mrq_id, mrq_prn, mrq_receivefrom_adm_no, mrq_receivefrom_inm_no, mrq_receivefrom_ins_code, mrq_reg_date_dt, mrq_type) select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp,  inm_no, ins_code, trunc(sysdate), trunc(sysdate), vMRQID, vCIN, vADM, vINM, vINSCODE, trunc(sysdate), 'FOI' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';

EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_RECEIVE - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_RECEIVE - Other record','vERM:'||vERM,vERR);  
    commit;  

END REQUEST_RECEIVE;
PROCEDURE REQUEST_SEND (
      vCIN		IN VARCHAR2,
      vTO_CIN		IN VARCHAR2
  )
  IS
  vINM NUMBER;
  vADM NUMBER;
  vINSCODE VARCHAR2(8);
  vMRQID NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100);  
BEGIN  
  vADM := GET_ADMNO( vTO_CIN);
  vINM := GET_INMNO( vTO_CIN);
  vINSCODE := GET_INSCODE( vTO_CIN);
  select nvl(max(mrq_id),0)+1 into vMRQID from t_mrq_request;
insert into T_MRQ_REQUEST(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, ins_code, mrq_start_date_dt, mrq_end_date_dt, mrq_id, mrq_prn, mrq_sendto_adm_no, mrq_sendto_inm_no, mrq_sendto_ins_code, mrq_reg_date_dt, mrq_type) select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, ins_code, trunc(sysdate), trunc(sysdate), vMRQID, vCIN, vADM, vINM, vINSCODE, trunc(sysdate), 'SOI' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_SEND - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_SEND - Other record','vERM:'||vERM,vERR);  
    commit;  

END REQUEST_SEND;


PROCEDURE INMATE_REFUSE_INMATE (
      vCIN		IN VARCHAR2,
      vREFUSED_CIN		IN VARCHAR2
  )
  IS
  vINM NUMBER;
  vADM NUMBER;
  vINSCODE VARCHAR2(8);
  vERR NUMBER;
  vERM VARCHAR2(100);  
BEGIN  
  vADM := GET_ADMNO( vREFUSED_CIN);
  vINM := GET_INMNO( vREFUSED_CIN);
  vINSCODE := GET_INSCODE( vREFUSED_CIN);
  --select nvl(max(mrq_id),0)+1 into vMRQID from t_mrq_request;
insert into T_RLI_REFUSAL_LIST_INMATE(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, rli_adm_no, rli_id, rli_inm_no, rli_ins_code, RLI_REQ_DATE_DT, rli_start_date_dt, rli_end_date_dt, rli_type) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, vADM, seq_rli_id.nextval, vINM, vINSCODE, trunc(sysdate), trunc(sysdate), trunc(sysdate), '3' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'INMATE_REFUSE_INMATE - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'INMATE_REFUSE_INMATE - Other record','vERM:'||vERM,vERR);  
    commit; 

END INMATE_REFUSE_INMATE;

PROCEDURE INMATE_REFUSE_OUTSIDER (
      vCIN		IN VARCHAR2,
      vOUTSIDER		IN VARCHAR2
  )
  IS
  vERR NUMBER;
  vERM VARCHAR2(100); 
BEGIN  
  --vADM := GET_ADMNO( vREFUSED_CIN);
  --vINM := GET_INMNO( vREFUSED_CIN);
  --vINSCODE := GET_INSCODE( vREFUSED_CIN);
  --select nvl(max(mrq_id),0)+1 into vMRQID from t_mrq_request;
insert into T_RLI_REFUSAL_LIST_INMATE(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, rli_adm_no, rli_id, rli_inm_no, rli_ins_code, RLI_REQ_DATE_DT, rli_start_date_dt, rli_end_date_dt, rli_type, rli_name) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, null, seq_rli_id.nextval, null, null, trunc(sysdate), trunc(sysdate), trunc(sysdate), '4', vOUTSIDER from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'INMATE_REFUSE_OUTSIDER - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'INMATE_REFUSE_OUTSIDER - Other record','vERM:'||vERM,vERR);  
    commit; 

END INMATE_REFUSE_OUTSIDER;

PROCEDURE OUTSIDER_REFUSE_INMATE (
      vOUTSIDER		IN VARCHAR2,
      vOUTSIDER_ADDRESS IN VARCHAR2,
      vREFUSED_CIN		IN VARCHAR2
  )
  IS
  vERR NUMBER;
  vERM VARCHAR2(100);   
BEGIN  
insert into T_RLO_REFUSAL_LIST_OUTSIDER(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, rlo_id, RLO_CREATE_DATE_DT, RLO_REQ_DATE_DT, rlo_start_date_dt, rlo_end_date_dt, rlo_update_mode, rlo_name, rlo_address, rlo_direction, rlo_im_refused) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, seq_rlo_id.nextval, trunc(sysdate), trunc(sysdate), trunc(sysdate), trunc(sysdate), 'A', vOUTSIDER, vOUTSIDER_ADDRESS, 'O', 1 from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vREFUSED_CIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'OUTSIDER_REFUSE_INMATE - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'OUTSIDER_REFUSE_INMATE - Other record','vERM:'||vERM,vERR);  
    commit;

END OUTSIDER_REFUSE_INMATE;

PROCEDURE REQUEST_SEM (
      vCIN		IN VARCHAR2
  )
  IS
  --vINM NUMBER;
  --vADM NUMBER;
  --vINSCODE VARCHAR2(8);
  vMRQID NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100); 
BEGIN  
  --vADM := GET_ADMNO( vTO_CIN);
  --vINM := GET_INMNO( vCIN);
  --vINSCODE := GET_INSCODE( vTO_CIN);
  select nvl(max(mrq_id),0)+1 into vMRQID from t_mrq_request;
insert into T_MRQ_REQUEST(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, ins_code, mrq_start_date_dt, mrq_end_date_dt, mrq_id, mrq_prn, mrq_sendto_adm_no, mrq_sendto_inm_no, mrq_sendto_ins_code, mrq_reg_date_dt, mrq_type, mrq_qty, mrq_update_mode) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, ins_code, trunc(sysdate), trunc(sysdate), vMRQID, vCIN, null, null, null, trunc(sysdate), 'SEM', 1, 'A' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_SEM - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_SEM - Other record','vERM:'||vERM,vERR);  
    commit;
END REQUEST_SEM;

PROCEDURE REQUEST_QAH (
    vINSCODE IN VARCHAR2,
      vCORR		IN VARCHAR2
  )
  IS
  --vINM NUMBER;
  --vADM NUMBER;
  --vINSCODE VARCHAR2(8);
  vMRQID NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100);  
BEGIN  
  --vADM := GET_ADMNO( vTO_CIN);
  --vINM := GET_INMNO( vCIN);
  --vINSCODE := GET_INSCODE( vTO_CIN);
  select nvl(max(mrq_id),0)+1 into vMRQID from t_mrq_request;
insert into T_MRQ_REQUEST(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, ins_code, mrq_start_date_dt, mrq_end_date_dt, mrq_id, mrq_prn, mrq_sendto_adm_no, mrq_sendto_inm_no, mrq_sendto_ins_code, mrq_reg_date_dt, mrq_type, mrq_qty, mrq_update_mode, mrq_inmate_type) values
( null, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, null, vINSCODE, trunc(sysdate), trunc(sysdate), vMRQID, null, null, null, null, trunc(sysdate), 'QAH', 1, 'A', vCORR);
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_QAH - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'REQUEST_QAH - Other record','vERM:'||vERM,vERR);  
    commit;

END REQUEST_QAH;

PROCEDURE SPECIAL_CARE_INM (
      vCIN		IN VARCHAR2
  )
  IS
  --vINM NUMBER;
  --vADM NUMBER;
  --vINSCODE VARCHAR2(8);
  vSALID NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100);   
BEGIN  
  --vADM := GET_ADMNO( vTO_CIN);
  --vINM := GET_INMNO( vCIN);
  --vINSCODE := GET_INSCODE( vTO_CIN);
  select nvl(max(sal_id),0)+1 into vSALID from t_sal_special_attention_list;
insert into t_sal_special_attention_list(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, sal_start_date_dt, sal_end_date_dt, sal_id, sal_mail_type, sal_issued_by, sal_create_date_dt, sal_update_mode, sal_description) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, trunc(sysdate), trunc(sysdate), vSALID, 'INM', 'MM_ADM', trunc(sysdate), 'A', ' ' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'SPECIAL_CARE_INM - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'SPECIAL_CARE_INM - Other record','vERM:'||vERM,vERR);  
    commit;
END SPECIAL_CARE_INM;

PROCEDURE SPECIAL_CARE_OUM (
      vCIN		IN VARCHAR2
  )
  IS
  --vINM NUMBER;
  --vADM NUMBER;
  --vINSCODE VARCHAR2(8);
  vSALID NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100);   
BEGIN  
  --vADM := GET_ADMNO( vTO_CIN);
  --vINM := GET_INMNO( vCIN);
  --vINSCODE := GET_INSCODE( vTO_CIN);
  select nvl(max(sal_id),0)+1 into vSALID from t_sal_special_attention_list;
insert into t_sal_special_attention_list(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, sal_start_date_dt, sal_end_date_dt, sal_id, sal_mail_type, sal_issued_by, sal_create_date_dt, sal_update_mode, sal_description) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, trunc(sysdate), trunc(sysdate), vSALID, 'OUM', 'MM_ADM', trunc(sysdate), 'A', ' ' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'SPECIAL_CARE_OUM - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'SPECIAL_CARE_OUM - Other record','vERM:'||vERM,vERR);  
    commit;
END SPECIAL_CARE_OUM;

PROCEDURE MAIL_RECEIVE (
      vCIN		IN VARCHAR2
  )
  IS
  vMax NUMBER;
  vNUM NUMBER;
  vINM NUMBER;
  vADM NUMBER;
  vSAL NUMBER;
  vERR NUMBER;
  vERM VARCHAR2(100);  
BEGIN  

select nvl(count(*),0) into vNUM from t_inm_inmate i, T_SAL_SPECIAL_ATTENTION_LIST s 
where case i.INM_CIN when 0 then i.INM_REM_SER_NO||'-'||LPAD(to_char(i.INM_REM_YR),2,'0') else to_char(i.INM_CIN) end=vCIN and i.inm_curr_ind='Y'
and i.inm_no=s.inm_no and i.adm_no=s.adm_no and to_char(s.SAL_START_DATE_DT,'yyyyMMdd')<=to_char(sysdate,'yyyyMMdd') and  to_char(sysdate,'yyyyMMdd')<=to_char(s.SAL_START_DATE_DT,'yyyyMMdd')
and s.SAL_MAIL_TYPE in ('ALL','INM');  

insert into t_iml_in_mail(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, ins_code, mfd_id, mlt_id, iml_collect_date_dt, iml_id, iml_prn, iml_envelope, ml_reg_date_dt, iml_letter, iml_postmark_qty, iml_in_sal, update_mode) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, ins_code, 5, 1, trunc(sysdate), seq_iml_id.nextval, vCIN, 0, trunc(sysdate), 1, 1, case when vNUM>0 then 1 else 0 end, 'A' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';  

EXCEPTION

    WHEN NO_DATA_FOUND THEN

            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'MAIL_RECEIVE - No record','vERM:'||vERM,vERR);    
    commit;
	WHEN OTHERS THEN
            vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'MAIL_RECEIVE - Other record','vERM:'||vERM,vERR);  
    commit;

END MAIL_RECEIVE;  

PROCEDURE MAIL_SEND (
      vCIN		IN VARCHAR2
  )
  IS
  vMax NUMBER;
  vNUM NUMBER;
  vINM NUMBER;
  vADM NUMBER;
BEGIN  
  select nvl(max(oml_id),0)+1 into vMax from t_oml_out_mail;

select nvl(count(*),0) into vNUM from t_inm_inmate i, T_SAL_SPECIAL_ATTENTION_LIST s 
where case i.INM_CIN when 0 then i.INM_REM_SER_NO||'-'||LPAD(to_char(i.INM_REM_YR),2,'0') else to_char(i.INM_CIN) end=vCIN and i.inm_curr_ind='Y'
and i.inm_no=s.inm_no and i.adm_no=s.adm_no and to_char(s.SAL_START_DATE_DT,'yyyyMMdd')<=to_char(sysdate,'yyyyMMdd') and  to_char(sysdate,'yyyyMMdd')<=to_char(s.SAL_START_DATE_DT,'yyyyMMdd')
and s.SAL_MAIL_TYPE in ('ALL','OUM');  

insert into t_oml_out_mail(adm_no, created_by, created_ts, updated_by, updated_ts, inm_no, ins_code, mfd_id, mlt_id, oml_collect_date_dt, oml_id, oml_postage, oml_stamp_type, oml_prn, oml_prn_for_sorting, oml_reg_date_dt, oml_used_quota, oml_out_sal, oml_mail_dest, oml_res_mail_clerk, update_mode) 
select adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp, inm_no, ins_code, 15, 9, trunc(sysdate), vMax, 2.2, 'P', vCIN, LPAD(vCIN, 8, '0'), trunc(sysdate), 1, case when vNUM>0 then 1 else 0 end, 11, 'ADMIN_MM', 'A' from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';  
vINM := GET_INMNO(vCIN);
vADM := GET_ADMNO(vCIN);
select nvl(msq_id,0) into vNUM from T_MSQ_STAMP_QUOTA where to_char(msq_collect_date_dt,'yyyyMMdd')=to_char(sysdate,'yyyyMMdd') and inm_no=vINM and adm_no=vADM;
If vNUM > 0 Then
insert into T_MSQ_STAMP_QUOTA (inm_no, adm_no, created_ts, created_by, updated_ts, updated_by, msq_qty, msq_id, msq_collect_date_dt, quota_dt)
values(vINM, vADM, systimestamp, 'ADMIN_MM', systimestamp, 'ADMIN_MM', 1, seq_msq_id.nextval, trunc(sysdate), trunc(sysdate));
Else
  update T_MSQ_STAMP_QUOTA set updated_ts=systimestamp, updated_by='ADMIN_MM', msq_qty=msq_qty+1 where msq_id=vNUM;
End If;
EXCEPTION
	WHEN NO_DATA_FOUND THEN
		DBMS_Output.Put_Line('MAIL_SEND NO DATA FOUND');
	WHEN OTHERS THEN
		DBMS_Output.Put_Line('MAIL_SEND '||SQLCODE||' '||SUBSTR(SQLERRM, 1,200));



END MAIL_SEND;   

PROCEDURE MAIL_PATCH_COLLECT_DT (
      vCIN		IN VARCHAR2,
      vFM_DT    IN VARCHAR2,
      vTO_DT    IN VARCHAR2
  )
  IS
vERR NUMBER;
vERM VARCHAR2(100);
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vINM :=GET_INMNO(vCIN);
vADM :=GET_ADMNO(vCIN);

UPDATE t_msq_stamp_quota set msq_collect_date_dt=to_date(vTO_DT,'yyyyMMdd') where inm_no=vINM and adm_no=vADM and trunc(msq_collect_date_dt)=to_date(vFM_DT,'yyyyMMdd');

UPDATE t_oml_out_mail set oml_collect_date_dt=to_date(vTO_DT,'yyyyMMdd') where oml_prn=vCIN and trunc(oml_collect_date_dt)=to_date(vFM_DT,'yyyyMMdd');
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        vERR:=SQLCODE;
        vERM:=SUBSTR(SQLERRM, 1,100);
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'MAIL_PATCH_COLLECT_DT - No record','vERM:'||vERM,vERR);

    WHEN OTHERS THEN
        vERR:=SQLCODE;
        vERM:=SUBSTR(SQLERRM, 1,100);
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'MAIL_PATCH_COLLECT_DT - Other record','vERM:'||vERM,vERR);    

 
END MAIL_PATCH_COLLECT_DT ;   

FUNCTION PIC_STATUS_DATE (
      vCIN		IN VARCHAR2,
      vDATE IN VARCHAR2
  )RETURN T_INM_INMATE.INM_STATUS%TYPE
  IS
vINM_STATUS T_INM_INMATE.INM_STATUS%TYPE;   
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
vNUM NUMBER;
vCHAR VARCHAR2(1);
BEGIN  
vINM_STATUS:='P';

    BEGIN
    select nvl(count(*),0) into vNUM from t_inm_inmate i 
    where case i.INM_CIN when 0 then i.INM_REM_SER_NO||'-'||LPAD(to_char(i.INM_REM_YR),2,'0') else to_char(i.INM_CIN) end=vCIN 
    and trunc(case i.INM_CIN when 0 then i.INM_REM_DIS_DA_DT else INM_CON_DIS_DA_DT end) = to_date(vDATE,'yyyyMMdd');

    IF vNUM > 0 THEN
        RETURN 'D';
    END IF;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','No discharge found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','Other discharge error');    
    END;
    --select PKG_FAST_TRACK.PIC_STATUS_DATE('162168','19960914') from dual;

    BEGIN
    select nvl(count(*),0) into vNUM 
    from t_inm_inmate i join t_tra_transfer l on i.inm_no=l.inm_no and i.adm_no=l.adm_no 
    where case i.INM_CIN when 0 then i.INM_REM_SER_NO||'-'||LPAD(to_char(i.INM_REM_YR),2,'0') else to_char(i.INM_CIN) end=vCIN 
    and trunc(l.tra_date_dt) = to_date(vDATE,'yyyyMMdd');

    IF vNUM > 0 THEN
        RETURN 'T';
    END IF;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','No transfer found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','Other transfer error');    
    END;
    --select PKG_FAST_TRACK.PIC_STATUS_DATE('5360','19960320') from dual;

    BEGIN
    select nvl(count(*),0) into vNUM 
    from t_inm_inmate i join t_loc_location l on i.inm_no=l.inm_no and i.adm_no=l.adm_no 
    where case i.INM_CIN when 0 then i.INM_REM_SER_NO||'-'||LPAD(to_char(i.INM_REM_YR),2,'0') else to_char(i.INM_CIN) end=vCIN 
    and trunc(l.LOC_DATE_ASSIG_DT) = to_date(vDATE,'yyyyMMdd');

    IF vNUM > 0 THEN
        RETURN 'Z';
    END IF;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','No hospital found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','Other hospital error');    
    END;
    --select PKG_FAST_TRACK.PIC_STATUS_DATE('152899','19951216') from dual;

    --B C E N S
    BEGIN
    select ino_out_code into vCHAR 
    from t_inm_inmate i join t_ino_in_out l on i.inm_no=l.inm_no and i.adm_no=l.adm_no 
    where case i.INM_CIN when 0 then i.INM_REM_SER_NO||'-'||LPAD(to_char(i.INM_REM_YR),2,'0') else to_char(i.INM_CIN) end=vCIN 
    and trunc(l.INO_DATE_OUT_DT) = to_date(vDATE,'yyyyMMdd');

    --IF vNUM > 0 THEN
        RETURN vCHAR;
    --END IF;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','No out found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'PIC_STATUS_DATE','Other out error');    
    END;    
    --select PKG_FAST_TRACK.PIC_STATUS_DATE('17256','19970108') from dual;
RETURN vINM_STATUS;
END PIC_STATUS_DATE;


PROCEDURE OLD_DATE_OUT_T_WOR (
      P_RUN_DATE		IN Date,
      REFERENCE_DATE IN VARCHAR2,
      FM_DAY		IN Number,
      TO_DAY		IN Number
  )
  IS
vBEFORE_DT DATE;
vCOUNT NUMBER;
vINS_CODE t_inm_inmate.ins_code%TYPE;
vEVW_WORKSHOP_CODE t_ews_ev_workshop.EVW_WORKSHOP_CODE%TYPE;
vEWS_WORKSHOP_SEQ t_ews_ev_workshop.EWS_WORKSHOP_SEQ%TYPE;


vFrom_DT VARCHAR2(8);
vTo_DT	VARCHAR2(8);
BEGIN

If REFERENCE_DATE = 'SYSDATE' Then
  select to_char(sysdate + FM_DAY,'yyyyMMdd'), to_char(sysdate + TO_DAY,'yyyyMMdd') into vFrom_DT, vTo_DT from dual;
ELSE
  select to_char(to_date(REFERENCE_DATE,'yyyyMMdd')+ FM_DAY,'yyyyMMdd'), to_char(to_date(REFERENCE_DATE,'yyyyMMdd')+ TO_DAY,'yyyyMMdd') into vFrom_DT, vTo_DT from dual;
END IF;


select to_date(vFrom_DT,'yyyyMMdd') - 1 into vBEFORE_DT from dual ;
    BEGIN

FOR d IN (
select vBEFORE_DT + rownum workdate from dual connect by vBEFORE_DT + rownum <= to_date(vTo_DT,'yyyyMMdd')
--union
--select wor_dat_assig_dt workdate from t_wor_work where trunc(created_ts)>= trunc(to_date(vTO_DT,'yyyyMMdd')) and trunc(created_ts)<= trunc(to_date(vTO_DT,'yyyyMMdd'))
) loop

--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR','DAY', d.workdate-vBEFORE_DT);
    -- DISCHARGE
    FOR a IN (
    select i.inm_no, i.adm_no from t_inm_inmate i where trunc(i.inm_rem_dis_da_dt)=trunc(d.workdate) or trunc(i.inm_con_dis_da_dt)=trunc(d.workdate)
    order by 1, 2
    ) loop

    BEGIN
    select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    if vCOUNT = 1 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT_T_WOR', updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - D',to_char(a.inm_no)||' - '||to_char( a.adm_no),vCOUNT);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','No discharge found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','Other discharge error');    
    END;
    end loop;
    -- TRANSFER
    FOR a IN (
    select i.inm_no, i.adm_no from t_tra_transfer i where trunc(i.tra_date_dt)=trunc(d.workdate) 
    order by 1, 2
    ) loop

    BEGIN
    select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    if vCOUNT = 1 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT_T_WOR', updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - T',to_char(a.inm_no)||' - '||to_char( a.adm_no),vCOUNT);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','No transfer found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','Other transfer error');    
    END;
    end loop;
    -- LOCATION
    FOR a IN (
    select i.inm_no, i.adm_no from t_loc_location i where trunc(i.loc_date_assig_dt)=trunc(d.workdate) 
    order by 1, 2
    ) loop

    BEGIN
    select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    if vCOUNT = 1 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT_T_WOR', updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - L',to_char(a.inm_no)||' - '||to_char( a.adm_no),vCOUNT);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','No location found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','Other location error');    
    END;
    end loop;
    -- B C E N S
    FOR a IN (
    select i.inm_no, i.adm_no from t_ino_in_out i where trunc(i.ino_date_out_dt)=trunc(d.workdate) 
    order by 1, 2
    ) loop

    BEGIN
    select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    if vCOUNT = 1 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT_T_WOR', updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - I',to_char(a.inm_no)||' - '||to_char( a.adm_no),vCOUNT);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','No inout found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR','Other inout error');    
    END;
    end loop;

    -- T_WOR_WORK
    FOR a IN (
    select i.inm_no, i.adm_no, i.wop_pa_job_code, i.wor_ser_no from T_WOR_WORK i where trunc(i.wor_dat_assig_dt)=trunc(d.workdate) 
	and rownum <2
    order by 1, 2, 4 desc
    ) loop

    BEGIN
    select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) <= trunc(d.workdate);

    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - W',to_char(a.inm_no)||' - '||to_char( a.adm_no),vCOUNT);


    if vCOUNT = 0 Then

        select ins_code into vINS_CODE from t_inm_inmate where inm_no=a.inm_no and adm_no=a.adm_no;

        select EVW_WORKSHOP_CODE into vEVW_WORKSHOP_CODE from t_z_evw_ev_workshop where joc_job_code=a.wop_pa_job_code and evw_status='A';

        select nvl(max(EWS_WORKSHOP_SEQ),0)+1 into vEWS_WORKSHOP_SEQ from T_EWS_EV_WORKSHOP where inm_no=a.inm_no and adm_no=a.adm_no;

        insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
        EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        values(a.inm_no, a.adm_no, vEWS_WORKSHOP_SEQ,'A',vINS_CODE,vEVW_WORKSHOP_CODE,
        trunc(d.workdate),'Y',a.wor_ser_no,
        systimestamp, 'DATE_OUT_T_WOR', systimestamp, 'DATE_OUT_T_WOR');

    Elsif vCOUNT = 1 Then    


        select EVW_WORKSHOP_CODE into vEVW_WORKSHOP_CODE from t_z_evw_ev_workshop where joc_job_code=a.wop_pa_job_code and evw_status='A';
        select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null 
        and trunc(ews_in_date_dt) <= trunc(d.workdate) and EVW_WORKSHOP_CODE=vEVW_WORKSHOP_CODE;
        If vCOUNT = 0 Then
              select ins_code into vINS_CODE from t_inm_inmate where inm_no=a.inm_no and adm_no=a.adm_no;
              update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT_T_WOR', updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
              select nvl(max(EWS_WORKSHOP_SEQ),0)+1 into vEWS_WORKSHOP_SEQ from T_EWS_EV_WORKSHOP where inm_no=a.inm_no and adm_no=a.adm_no;
              insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
                  EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
                  CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
                  values(a.inm_no, a.adm_no, vEWS_WORKSHOP_SEQ,'A',vINS_CODE,vEVW_WORKSHOP_CODE,
                  trunc(d.workdate),'Y',a.wor_ser_no,
                  systimestamp, 'DATE_OUT_T_WOR', systimestamp, 'DATE_OUT_T_WOR');
        End If;
    End if;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR - No work','wop_pa_job_code:'||a.wop_pa_job_code);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR - Other work','wop_pa_job_code:'||a.wop_pa_job_code);    
    END;
    end loop;
end loop;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR - No date','vBEFORE_DT:'||to_char(vBEFORE_DT,'yyyyMMdd'));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR - Other date','vBEFORE_DT:'||to_char(vBEFORE_DT,'yyyyMMdd'));    
    END;   
END  OLD_DATE_OUT_T_WOR;

FUNCTION GET_EWS_WORKSHOP_CODE(
	JOB_CODE IN T_WOR_WORK.WOP_PA_JOB_CODE%type
	) RETURN T_EWS_EV_WORKSHOP.EVW_WORKSHOP_CODE%TYPE
	is
vEVW_WORKSHOP_CODE T_EWS_EV_WORKSHOP.EVW_WORKSHOP_CODE%TYPE:=0;
vERR NUMBER:=0;
vERM VARCHAR2(100);
begin

	select EVW_WORKSHOP_CODE into vEVW_WORKSHOP_CODE from t_z_evw_ev_workshop where joc_job_code=JOB_CODE and evw_status='A';
	return vEVW_WORKSHOP_CODE;
	EXCEPTION
        WHEN NO_DATA_FOUND THEN
			vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_EWS_WORKSHOP_CODE - No record','vERM:'||vERM,vERR);

        WHEN OTHERS THEN
			vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_EWS_WORKSHOP_CODE - Other record','vERM:'||vERM,vERR);    



END GET_EWS_WORKSHOP_CODE; 

PROCEDURE DATE_OUT_T_WOR_DEL_T_EWS (
      FM_DT		IN Date,
      TO_DT		IN Date
  )
  is
vERR NUMBER:=0;
vERM VARCHAR2(100);  
vWOR_WORKSHOP_CODE T_EWS_EV_WORKSHOP.EVW_WORKSHOP_CODE%TYPE:=0;
begin

for p in (select inm_no from t_wor_work w where trunc(wor_dat_assig_dt)>=trunc(FM_DT) and trunc(wor_dat_assig_dt)<=trunc(TO_DT)
group by inm_no order by inm_no )
loop
	begin
	for w in (select w.inm_no WOR_INM_NO, w.adm_no WOR_ADM_NO, w.wor_ser_no, trim(w.wop_pa_job_code) WOR_WORKSHOP_CODE, trunc(w.wor_dat_assig_dt) assign_dt, 
	s.inm_no WORKSHOP_INM_NO, s.adm_no WORKSHOP_ADM_NO, s.ews_workshop_seq, trunc(s.ews_in_date_dt) in_dt, trunc(s.ews_out_date_dt) out_dt, s.EVW_WORKSHOP_CODE WORKSHOP_CODE 
	from t_wor_work w full outer join T_EWS_EV_WORKSHOP s on w.inm_no=s.inm_no and w.adm_no=s.adm_no and w.wor_ser_no=s.wor_ser_no
	where (w.inm_no=p.inm_no or s.inm_no=p.inm_no)
	--and trunc(w.wor_dat_assig_dt)>=trunc(FM_DT) and trunc(wor_dat_assig_dt)<=trunc(TO_DT) 
	and s.ews_status='A'
	order by WOR_INM_NO,WOR_ADM_NO,w.wor_ser_no,assign_dt)
	loop

		IF w.WOR_INM_NO is null then

			Update T_EWS_EV_WORKSHOP set ews_status='D', updated_by='DEL_T_EWS'||' - '||updated_by, updated_ts=systimestamp
			where inm_no=w.WORKSHOP_INM_NO and adm_no=w.WORKSHOP_ADM_NO and ews_workshop_seq=w.ews_workshop_seq and ews_status='A'; 
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - extra workshop',to_char(w.WORKSHOP_INM_NO)||' - '||to_char( w.WORKSHOP_ADM_NO),w.ews_workshop_seq);	
		Else
			vWOR_WORKSHOP_CODE:=GET_EWS_WORKSHOP_CODE(w.WOR_WORKSHOP_CODE);

			If vWOR_WORKSHOP_CODE <> w.WORKSHOP_CODE then

			Update T_EWS_EV_WORKSHOP set ews_status='D', updated_by='DEL_T_EWS'||' - '||updated_by, updated_ts=systimestamp
			where inm_no=w.WORKSHOP_INM_NO and adm_no=w.WORKSHOP_ADM_NO and ews_workshop_seq=w.ews_workshop_seq and ews_status='A'; 
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - workshop_code:'||w.WORKSHOP_CODE||' - not matched',to_char(w.WORKSHOP_INM_NO)||' - '||to_char( w.WORKSHOP_ADM_NO),w.ews_workshop_seq);	

			Elsif vWOR_WORKSHOP_CODE = w.WORKSHOP_CODE AND w.assign_dt <> w.in_dt Then


			Update T_EWS_EV_WORKSHOP set ews_status='D', updated_by='DEL_T_EWS'||' - '||updated_by, updated_ts=systimestamp
			where inm_no=w.WORKSHOP_INM_NO and adm_no=w.WORKSHOP_ADM_NO and ews_workshop_seq=w.ews_workshop_seq and ews_status='A'; 
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - in_dt'||to_char(w.in_dt,'yyyyMMdd')||' - not matched',to_char(w.WORKSHOP_INM_NO)||' - '||to_char( w.WORKSHOP_ADM_NO),w.ews_workshop_seq);	
			Elsif vWOR_WORKSHOP_CODE = w.WORKSHOP_CODE AND trunc(w.assign_dt) = trunc(w.in_dt) and w.out_dt is not null Then


			Update T_EWS_EV_WORKSHOP set ews_out_date_dt=null, updated_by='DEL_T_EWS'||' - '||updated_by, updated_ts=systimestamp
			where inm_no=w.WORKSHOP_INM_NO and adm_no=w.WORKSHOP_ADM_NO and ews_workshop_seq=w.ews_workshop_seq and ews_status='A'; 
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - '||to_char(w.out_dt,'yyyyMMdd')||' - activate',to_char(w.WORKSHOP_INM_NO)||' - '||to_char( w.WORKSHOP_ADM_NO),w.ews_workshop_seq);	
			End If;
		End If;		

	End loop;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
			vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
			Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - No w','vERM:'||vERM,vERR);

		WHEN OTHERS THEN
			vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
			Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - Other w','vERM:'||vERM,vERR);   


	End;
end loop;
EXCEPTION
	WHEN NO_DATA_FOUND THEN
		vERR:=SQLCODE;
		vERM:=SUBSTR(SQLERRM, 1,100);
		Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - No p','vERM:'||vERM,vERR);

	WHEN OTHERS THEN
		vERR:=SQLCODE;
		vERM:=SUBSTR(SQLERRM, 1,100);
		Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'DATE_OUT_T_WOR_DEL_T_EWS - Other p','vERM:'||vERM,vERR);   

END DATE_OUT_T_WOR_DEL_T_EWS; 

PROCEDURE DATE_OUT_T_WOR (
      P_RUN_DATE		IN Date,
      REFERENCE_DATE IN VARCHAR2,
      FM_DAY		IN Number,
      TO_DAY		IN Number
  )
  IS
vBEFORE_DT DATE;
vCOUNT NUMBER;
vINS_CODE t_inm_inmate.ins_code%TYPE;
vEVW_WORKSHOP_CODE t_ews_ev_workshop.EVW_WORKSHOP_CODE%TYPE;
vEWS_WORKSHOP_SEQ t_ews_ev_workshop.EWS_WORKSHOP_SEQ%TYPE;


vFrom_DT VARCHAR2(8);
vTo_DT	VARCHAR2(8);
vERR NUMBER:=0;
vERM VARCHAR2(100);
vEWS_IN_DATE_DT VARCHAR2(8);
BEGIN

If REFERENCE_DATE = 'SYSDATE' Then
  select to_char(sysdate + FM_DAY,'yyyyMMdd'), to_char(sysdate + TO_DAY,'yyyyMMdd') into vFrom_DT, vTo_DT from dual;
ELSE
  select to_char(to_date(REFERENCE_DATE,'yyyyMMdd')+ FM_DAY,'yyyyMMdd'), to_char(to_date(REFERENCE_DATE,'yyyyMMdd')+ TO_DAY,'yyyyMMdd') into vFrom_DT, vTo_DT from dual;
END IF;

DATE_OUT_T_WOR_DEL_T_EWS(vFrom_DT,sysdate);

select to_date(vFrom_DT,'yyyyMMdd') - 1 into vBEFORE_DT from dual ;
    BEGIN

FOR d IN (
select vBEFORE_DT + rownum workdate from dual connect by trunc(vBEFORE_DT + rownum) <= trunc(to_date(vTo_DT,'yyyyMMdd'))
--union
--select wor_dat_assig_dt workdate from t_wor_work where trunc(created_ts)>= trunc(to_date(vTO_DT,'yyyyMMdd')) and trunc(created_ts)<= trunc(to_date(vTO_DT,'yyyyMMdd'))
) loop

--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR','DAY', d.workdate-vBEFORE_DT);
    -- DISCHARGE
    FOR a IN (
    select i.inm_no, i.adm_no, count(*) cnt from t_inm_inmate i, t_ews_ev_workshop w 
	where (trunc(i.inm_rem_dis_da_dt)=trunc(d.workdate) or trunc(i.inm_con_dis_da_dt)=trunc(d.workdate))
	and i.inm_no=w.inm_no and i.adm_no=w.adm_no and w.ews_out_date_dt is null and trunc(w.ews_in_date_dt) < trunc(d.workdate)  and w.ews_status='A'
    group by i.inm_no, i.adm_no 
	--order by 1, 2
    ) loop

    BEGIN
    --select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    if a.cnt > 0 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT - '||updated_by, updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate) and ews_status='A';
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - D',to_char(a.inm_no)||' - '||to_char( a.adm_no),a.cnt);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - No discharge','d.workdate', to_number(to_char(d.workdate,'yyyyMMdd')));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - Other discharge','d.workdate', to_number(to_char(d.workdate,'yyyyMMdd')));    
    END;
    end loop;

    -- LOCATION
	/*
    FOR a IN (
    select i.inm_no, i.adm_no, count(*) cnt from t_loc_location i, t_ews_ev_workshop w where trunc(i.loc_date_assig_dt)=trunc(d.workdate) 
	and i.inm_no=w.inm_no and i.adm_no=w.adm_no and w.ews_out_date_dt is null and trunc(w.ews_in_date_dt) < trunc(d.workdate)
    group by i.inm_no, i.adm_no 
	--order by 1, 2
    ) loop

    BEGIN
    if a.cnt > 0 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT - '||updated_by, updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - L',to_char(a.inm_no)||' - '||to_char( a.adm_no),a.cnt);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - No location','d.workdate', to_number(to_char(d.workdate,'yyyyMMdd')));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - Other location','d.workdate', to_number(to_char(d.workdate,'yyyyMMdd')));    
    END;
    end loop;
	*/
    -- B C E N S
    FOR a IN (
    select i.inm_no, i.adm_no, count(*) cnt from t_ino_in_out i, t_ews_ev_workshop w where trunc(i.ino_date_out_dt)=trunc(d.workdate) 
	and i.inm_no=w.inm_no and i.adm_no=w.adm_no and w.ews_out_date_dt is null and trunc(w.ews_in_date_dt) < trunc(d.workdate)  and w.ews_status='A'
    group by i.inm_no, i.adm_no 
	--order by 1, 2
    ) loop

    BEGIN
    --select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
    if a.cnt > 0 Then
        update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT - '||updated_by, updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate)  and ews_status='A';
    End if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - I',to_char(a.inm_no)||' - '||to_char( a.adm_no),a.cnt);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - No inout','d.workdate', to_number(to_char(d.workdate,'yyyyMMdd')));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - Other inout','d.workdate', to_number(to_char(d.workdate,'yyyyMMdd')));    
    END;
    end loop;

    -- T_WOR_WORK
    FOR a IN (
	select i.inm_no, i.adm_no, i.wop_pa_job_code, i.wor_ser_no from T_WOR_WORK i join
		(select w.inm_no, w.adm_no, max(w.wor_ser_no) wor_ser_no from T_WOR_WORK w where trunc(w.wor_dat_assig_dt)=trunc(d.workdate) 
		group by w.inm_no, w.adm_no) w
		on w.inm_no=i.inm_no and w.adm_no=i.adm_no and w.wor_ser_no=i.wor_ser_no
		where trunc(i.wor_dat_assig_dt)=trunc(d.workdate)
    ) loop

    BEGIN
	--Active workshop
    select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) <= trunc(d.workdate) and ews_status='A';

    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'DATE_OUT_T_WOR - '||to_char(d.workdate,'yyyyMMdd')||' - W',to_char(a.inm_no)||' - '||to_char( a.adm_no),vCOUNT);


    if vCOUNT = 0 Then

        select ins_code into vINS_CODE from t_inm_inmate where inm_no=a.inm_no and adm_no=a.adm_no;

        select EVW_WORKSHOP_CODE into vEVW_WORKSHOP_CODE from t_z_evw_ev_workshop where joc_job_code=a.wop_pa_job_code and evw_status='A';

        select nvl(max(EWS_WORKSHOP_SEQ),0)+1 into vEWS_WORKSHOP_SEQ from T_EWS_EV_WORKSHOP where inm_no=a.inm_no and adm_no=a.adm_no and ews_status='A';

        insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
        EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        values(a.inm_no, a.adm_no, 
		--vEWS_WORKSHOP_SEQ
		a.wor_ser_no,
		'A',vINS_CODE,vEVW_WORKSHOP_CODE,
        trunc(d.workdate),'Y',a.wor_ser_no,
        systimestamp, 'DATE_OUT_T_WOR', systimestamp, 'DATE_OUT');
	/*
    Elsif vCOUNT = 1 Then    


        select EVW_WORKSHOP_CODE into vEVW_WORKSHOP_CODE from t_z_evw_ev_workshop where joc_job_code=a.wop_pa_job_code and evw_status='A';
        select nvl(count(*),0) into vCOUNT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null 
        and trunc(ews_in_date_dt) <= trunc(d.workdate) and EVW_WORKSHOP_CODE=vEVW_WORKSHOP_CODE;
        If vCOUNT = 0 Then
              select ins_code into vINS_CODE from t_inm_inmate where inm_no=a.inm_no and adm_no=a.adm_no;
              update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT_T_WOR', updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) < trunc(d.workdate);
              select nvl(max(EWS_WORKSHOP_SEQ),0)+1 into vEWS_WORKSHOP_SEQ from T_EWS_EV_WORKSHOP where inm_no=a.inm_no and adm_no=a.adm_no;
              insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
                  EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
                  CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
                  values(a.inm_no, a.adm_no, vEWS_WORKSHOP_SEQ,'A',vINS_CODE,vEVW_WORKSHOP_CODE,
                  trunc(d.workdate),'Y',a.wor_ser_no,
                  systimestamp, 'DATE_OUT_T_WOR', systimestamp, 'DATE_OUT_T_WOR');
        End If;
		*/
    Elsif vCOUNT > 0 Then   

        select EVW_WORKSHOP_CODE into vEVW_WORKSHOP_CODE from t_z_evw_ev_workshop where joc_job_code=a.wop_pa_job_code and evw_status='A';

		--Active desired workshop
        select nvl(to_char(ews_in_date_dt,'yyyyMMdd'),'0') into vEWS_IN_DATE_DT from t_ews_ev_workshop where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null 
        and trunc(ews_in_date_dt) <= trunc(d.workdate) and ews_status='A' and EVW_WORKSHOP_CODE=vEVW_WORKSHOP_CODE 
		and rownum <2 order by ews_in_date_dt desc;
		--date out not desired workshop and insert desired workshop
        If vEWS_IN_DATE_DT = '0' Then
              select ins_code into vINS_CODE from t_inm_inmate where inm_no=a.inm_no and adm_no=a.adm_no;
              update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT - '||updated_by, updated_ts=systimestamp where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and trunc(ews_in_date_dt) <= trunc(d.workdate) and ews_status='A';
              select nvl(max(EWS_WORKSHOP_SEQ),0)+1 into vEWS_WORKSHOP_SEQ from T_EWS_EV_WORKSHOP where inm_no=a.inm_no and adm_no=a.adm_no;
              insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
                  EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
                  CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
                  values(a.inm_no, a.adm_no, 
				  --vEWS_WORKSHOP_SEQ,
				  a.wor_ser_no,
				  'A',vINS_CODE,vEVW_WORKSHOP_CODE,
                  trunc(d.workdate),'Y',a.wor_ser_no,
                  systimestamp, 'DATE_OUT_T_WOR', systimestamp, 'DATE_OUT');
		ELSE
		--date out extra desired workshop 
		      select ins_code into vINS_CODE from t_inm_inmate where inm_no=a.inm_no and adm_no=a.adm_no;
              update t_ews_ev_workshop set ews_out_date_dt = trunc(d.workdate), updated_by='DATE_OUT - '||updated_by, updated_ts=systimestamp 
			  where inm_no=a.inm_no and adm_no=a.adm_no and ews_out_date_dt is null and to_char(ews_in_date_dt,'yyyyMMdd') < vEWS_IN_DATE_DT and ews_status='A';

        End If;	

    End if;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR - No work','wop_pa_job_code:'||a.wop_pa_job_code);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR - Other work','wop_pa_job_code:'||a.wop_pa_job_code);    
    END;
    end loop;
end loop;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
			vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'DATE_OUT_T_WOR - No date','vERM:'||vERM,vERR);
			--DBMS_Output.Put_Line('DATE_OUT_T_WOR '||SQLCODE||' '||SUBSTR(SQLERRM, 1,200));
        WHEN OTHERS THEN
			vERR:=SQLCODE;
			vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'DATE_OUT_T_WOR - Other date','vERM:'||vERM,vERR);    
			--DBMS_Output.Put_Line('DATE_OUT_T_WOR '||SQLCODE||' '||SUBSTR(SQLERRM, 1,200));
    END; 

END  DATE_OUT_T_WOR;

PROCEDURE DATE_OUT_T_WOR_FALLBACK (
      vRUN_DT		IN VARCHAR2
  )
  IS
BEGIN

    delete from T_EWS_EV_WORKSHOP where created_by='DATE_OUT_T_WOR' and trunc(created_ts)=trunc(to_date(vRUN_DT,'yyyyMMdd'));

    update T_EWS_EV_WORKSHOP set EWS_OUT_DATE_DT=null, updated_by=substr(updated_by,12) where updated_by like 'DATE_OUT - %' and EWS_OUT_DATE_DT is not null and trunc(updated_ts)=trunc(to_date(vRUN_DT,'yyyyMMdd')) and ews_status='A';



	For d in (select to_date(substr(dbg_msgs,28,8),'yyyyMMdd') out_dt, to_number(substr(dbg_string,1,instr(dbg_string,' - ')-1)) inm_no, 
	to_number(substr(dbg_string,instr(dbg_string,' - ')+3)) adm_no,	dbg_value ews_workshop_seq from t_dbg_debug where substr(dbg_msgs,1,24)='DATE_OUT_T_WOR_DEL_T_EWS' and substr(dbg_msgs,39)='activate' and trunc(created_ts)=trunc(to_date(vRUN_DT,'yyyyMMdd')))
	loop
		Update T_EWS_EV_WORKSHOP set EWS_OUT_DATE_DT=d.out_dt, updated_by=substr(updated_by,13) where updated_by like 'DEL_T_EWS - %' and EWS_OUT_DATE_DT is null and ews_status='A' and trunc(updated_ts)=trunc(to_date(vRUN_DT,'yyyyMMdd')) and inm_no=d.inm_no and adm_no=d.adm_no and ews_workshop_seq=d.ews_workshop_seq;

	end loop;

	Update T_EWS_EV_WORKSHOP set ews_status='A', updated_by=substr(updated_by,13) where updated_by like 'DEL_T_EWS - %' and ews_status='D' and trunc(updated_ts)=trunc(to_date(vRUN_DT,'yyyyMMdd'));



EXCEPTION
    WHEN NO_DATA_FOUND THEN
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR_FALLBACK','No date found');
    WHEN OTHERS THEN
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'DATE_OUT_T_WOR_FALLBACK','Other date error');    

END DATE_OUT_T_WOR_FALLBACK; 

PROCEDURE MOBILE_MOVE(
      vMOBILE_ID		IN VARCHAR2,
      vTO_INS_CODE IN  VARCHAR2
  )
  IS
BEGIN

update t_z_mbd_mobile_device set ins_code=vTO_INS_CODE where mbd_id=vMOBILE_ID;
update t_mdl_mail_allowed_work_pary set ins_code=vTO_INS_CODE where mobile_usr_id=vMOBILE_ID;

EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'MOBILE_MOVE','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'MOBILE_MOVE','Other record error');

END MOBILE_MOVE;

PROCEDURE ASSIGN_MOBILE_ROLE (
      vMACADDRESS		IN VARCHAR2,
      vMOBILE_ROLE IN  VARCHAR2
  )
  IS
  vMOBILE_ID T_Z_MBR_MOBILE_ROLE.MBD_ID%TYPE;
  vSER_NO T_Z_MBR_MOBILE_ROLE.mbr_ser_no%TYPE;
BEGIN  
select d.mbd_id, nvl(max(r.mbr_ser_no),0)+1 into vMOBILE_ID, vSER_NO from T_Z_MBD_MOBILE_DEVICE d LEFT JOIN T_Z_MBR_MOBILE_ROLE r on d.mbd_id=r.mbd_id where d.mbd_adpt_addr=vMACADDRESS group by d.mbd_id;
insert into T_Z_MBR_MOBILE_ROLE (created_ts, created_by, updated_ts, updated_by, mbd_id, mbr_ser_no, urr_id) values 
(systimestamp, 'ADM', systimestamp, 'ADM', vMOBILE_ID, vSER_NO, vMOBILE_ROLE );
EXCEPTION
  WHEN NO_DATA_FOUND THEN
		DBMS_Output.Put_Line('ASSIGN_MOBILE_ROLE NO DATA FOUND');
	WHEN OTHERS THEN
		DBMS_Output.Put_Line('ASSIGN_MOBILE_ROLE '||SQLCODE||' '||SUBSTR(SQLERRM, 1,200));
END ASSIGN_MOBILE_ROLE;   



PROCEDURE ASSIGN_USER_ROLE (
      vUSRID		IN VARCHAR2,
      vINSCODE IN VARCHAR2,
      vUSER_ROLE IN  VARCHAR2,
      vUSI_ADMIN_LEVEL IN T_Z_USI_USER_INST.USI_ADMIN_LEVEL%TYPE
  )
  IS   
    vSER_NO T_Z_UIR_USER_INST_ROLE.uir_ser_no%TYPE;
BEGIN    
select nvl(max(r.uir_ser_no),0)+1 into vSER_NO from T_Z_UIR_USER_INST_ROLE r where r.usr_id=vUSRID and r.ins_code=vINSCODE 
--group by r.usr_id
;
If vSER_NO = 1 Then
    insert into T_Z_USI_USER_INST (created_ts, created_by, updated_ts, updated_by, usr_id, ins_code, usi_admin_level) values 
    (systimestamp, 'ADM', systimestamp, 'ADM', vUSRID, vINSCODE, vUSI_ADMIN_LEVEL ); 
End If;
insert into T_Z_UIR_USER_INST_ROLE (created_ts, created_by, updated_ts, updated_by, usr_id, uir_ser_no, urr_id,ins_code) values 
(systimestamp, 'ADM', systimestamp, 'ADM', vUSRID, vSER_NO, vUSER_ROLE,vINSCODE );  
EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'ASSIGN_USER_ROLE','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'ASSIGN_USER_ROLE','Other record error');

END ASSIGN_USER_ROLE;   

PROCEDURE COPY_USER_ROLE (
      vUSRID		IN VARCHAR2,
      vFROM_USRID IN VARCHAR2
  )
  IS
--vSER_NO T_Z_UIR_USER_INST_ROLE.uir_ser_no%TYPE; 
vCOUNT NUMBER:=0;
BEGIN

FOR r IN (select f.ins_code, f.urr_id, i.usi_admin_level from T_Z_UIR_USER_INST_ROLE f 
join T_Z_USI_USER_INST i on f.ins_code=i.ins_code and f.usr_id = i.usr_id
where f.USR_ID=vFROM_USRID 
and not exists(select * from T_Z_UIR_USER_INST_ROLE t where t.USR_ID=vUSRID and f.ins_code=t.ins_code and f.urr_id=t.urr_id))
LOOP
    pkg_fast_track.ASSIGN_USER_ROLE(vUSRID,r.ins_code,r.urr_id,r.usi_admin_level);
    vCOUNT:=vCOUNT+1;
    insert into t_dbg_debug (created_ts,dbg_msgs,dbg_string)values(systimestamp,'COPY_USER_ROLE - ' || r.ins_code ||' - '||vCount,r.urr_id);
END LOOP;

END COPY_USER_ROLE;

-- no use because it cannot create record in LDAP
PROCEDURE CREATE_USER (
      vUSRID		IN VARCHAR2,
      vURRID    IN VARCHAR2,
      vINSCODE  IN VARCHAR2
  )
  IS
BEGIN  
insert into T_Z_USR_USERID (USR_ID, USR_NAME, INS_CODE, USR_SUSPENSE, created_by, created_ts, updated_by, updated_ts) values (vUSRID, vUSRID, vINSCODE, 'N', 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp );
insert into T_Z_USI_USER_INST (USR_ID, INS_CODE, USI_ADMIN_LEVEL, created_by, created_ts, updated_by, updated_ts) values (vUSRID, vINSCODE, case when instr(vURRID,'ADM')>0 then 1 else 0 end,'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp );
insert into T_Z_UIR_USER_INST_ROLE (USR_ID, INS_CODE, UIR_SER_NO, URR_ID, created_by, created_ts, updated_by, updated_ts) values (vUSRID, vINSCODE, 1, vURRID, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp );
END CREATE_USER;  

PROCEDURE URINE_REQUEST (
      vCIN		IN VARCHAR2,
      vGOVT   IN VARCHAR2
  )
  IS
  vRNUM NUMBER;
  vTNUM NUMBER;
BEGIN  
select nvl(max(substr(tre_req_ser_no,4)),0)+1 into vRNUM from t_tre_urt_request where tre_req_ser_no like 'R'||to_char(sysdate,'yy')||'%';
insert into t_tre_urt_request(tre_id, ins_code, tre_request_by, tre_request_ts, tre_execute_date, tre_req_ser_no, tre_gov_lab, created_by, created_ts, updated_by, updated_ts) select 
seq_tre_id.nextval, ins_code, 'ADMIN_MM', systimestamp, to_char(sysdate,'yyyyMMdd'), 'R'||to_char(sysdate,'yy')||LPAD(to_char(vRNUM),6, '0'), vGOVT, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';
select nvl(max(substr(tes_test_no,3)),0)+1 into vTNUM from T_TES_URT_TEST where tes_test_no like to_char(sysdate,'yy')||'%';
insert into T_TES_URT_TEST(tes_id, tes_test_no, tre_req_ser_no, inm_no, adm_no, created_by, created_ts, updated_by, updated_ts)  
select seq_tes_id.nextval, to_char(sysdate,'yy')||LPAD(to_char(vTNUM),6, '0'), 'R'||to_char(sysdate,'yy')||LPAD(to_char(vRNUM),6, '0'), inm_no, adm_no, 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp from t_inm_inmate where case INM_CIN when 0 then INM_REM_SER_NO||'-'||LPAD(to_char(INM_REM_YR),2,'0') else to_char(INM_CIN) end=vCIN and inm_curr_ind='Y';

insert into T_TED_URT_TEST_DETL(ted_id,tes_test_no, dpe_drug_type, created_by, created_ts, updated_by, updated_ts) values
(seq_ted_id.nextval, to_char(sysdate,'yy')||LPAD(to_char(vTNUM),6, '0'), 'BE', 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp);
insert into T_TED_URT_TEST_DETL(ted_id,tes_test_no, dpe_drug_type, created_by, created_ts, updated_by, updated_ts) values
(seq_ted_id.nextval, to_char(sysdate,'yy')||LPAD(to_char(vTNUM),6, '0'), 'OP', 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp);

insert into t_ter_urt_test_reason(ter_id, tes_test_no, ztr_test_reason, ter_detail_reason, created_by, created_ts, updated_by, updated_ts) values
(seq_ter_id.nextval, to_char(sysdate,'yy')||LPAD(to_char(vTNUM),6, '0'), 'BT', 'Before Transfer-out', 'ADMIN_MM', systimestamp, 'ADMIN_MM', systimestamp);
END URINE_REQUEST;

PROCEDURE URINE_PRINT_ESCORT (
      vTRE_REQ_SER_NO		IN VARCHAR2
  )
  IS
  sNUM NUMBER;
BEGIN
select u.usr_service_id into sNUM from T_Z_UIR_USER_INST_ROLE i, t_z_usr_userid u, t_tre_urt_request r 
where i.urr_id='URT_ES' and i.usr_id=r.created_by and i.usr_id=u.usr_id and i.ins_code=r.ins_code and r.TRE_REQ_SER_NO=vTRE_REQ_SER_NO;
update t_tre_urt_request set tre_escort_by_sn=sNUM, tre_prt_label_key='AAAA', tre_escort_prt_by=created_by, tre_escort_prt_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp where TRE_REQ_SER_NO=vTRE_REQ_SER_NO;
END URINE_PRINT_ESCORT;

PROCEDURE URINE_PRINT_LABEL (
      vTRE_REQ_SER_NO		IN VARCHAR2
  )
  IS
  sNUM NUMBER;
BEGIN
update t_tre_urt_request set tre_label_prt_by=created_by, tre_label_prt_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp where TRE_REQ_SER_NO=vTRE_REQ_SER_NO;
END URINE_PRINT_LABEL;

PROCEDURE URINE_SUBMIT (
      vTES_TEST_NO		IN VARCHAR2
  )
  IS
  sNUM NUMBER;
  lNUM NUMBER;
  vGOV_LAB VARCHAR2(1);
BEGIN
select u.usr_service_id, r.tre_gov_lab into sNUM, vGOV_LAB from T_Z_UIR_USER_INST_ROLE i, t_z_usr_userid u, T_TES_URT_TEST t, t_tre_urt_request r 
where i.urr_id='URT_SV' and i.usr_id=t.created_by and i.usr_id=u.usr_id and i.ins_code=r.ins_code and t.TES_TEST_NO=vTES_TEST_NO and t.TRE_REQ_SER_NO=r.TRE_REQ_SER_NO;  

IF vGOV_LAB = 'N' THEN
select nvl(max(substr(urb_log_no,4)),0)+1 into lNUM from t_urb_urine_box where urb_log_no like 'U'||to_char(sysdate,'yy')||'%';

insert into t_urb_urine_box(ins_code, created_by, created_ts, updated_by, updated_ts, urb_id, urb_lock_by, urb_lock_no, urb_lock_ts, urb_log_no, urb_send_to_ins_code, urb_send_by, urb_send_date, urb_send_ts)
select r.ins_code, t.created_by, systimestamp, t.created_by, systimestamp, seq_urb_id.nextval, t.created_by, 99, systimestamp, 'U'||to_char(sysdate,'yy')||LPAD(lNUM,6,'0'), r.ins_code, t.created_by, to_char(sysdate,'yyyyMMdd'), systimestamp from T_TES_URT_TEST t, t_tre_urt_request r where t.TES_TEST_NO=vTES_TEST_NO and t.TRE_REQ_SER_NO=r.TRE_REQ_SER_NO;

update T_TES_URT_TEST set urb_log_no= 'U'||to_char(sysdate,'yy')||LPAD(lNUM,6,'0'), tes_collect_by=created_by, tes_collect_super_sn=sNUM, tes_collect_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp where TES_TEST_NO=vTES_TEST_NO;

ELSE
update T_TES_URT_TEST set tes_gov_lab_req='Y', tes_gov_lab_req_by=created_by, tes_gov_lab_req_ts=systimestamp, tes_collect_by=created_by, tes_collect_super_sn=sNUM, tes_collect_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp where TES_TEST_NO=vTES_TEST_NO;

END IF;

END URINE_SUBMIT; 

PROCEDURE URINE_RECEIVE (
      vURB_LOG_NO		IN VARCHAR2
  )
  IS
BEGIN
update t_urb_urine_box set urb_receive_by='ADMIN_MM', urb_receive_specimen='Y', urb_receive_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp  where urb_log_no=vURB_LOG_NO;
update T_TES_URT_TEST set tes_received='Y', tes_test_by=created_by, tes_test_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp  where urb_log_no=vURB_LOG_NO;
END URINE_RECEIVE;

PROCEDURE URINE_PREP_TEST (
      vTES_TEST_NO		IN VARCHAR2
  )
  IS
BEGIN
update T_TES_URT_TEST set tes_gov_prep_by=created_by, tes_gov_prep_ts=systimestamp, updated_by=created_by, updated_ts=systimestamp  where TES_TEST_NO=vTES_TEST_NO;
END URINE_PREP_TEST;

PROCEDURE URINE_CONF_TEST (
      vTES_TEST_NO		IN VARCHAR2
  )
  IS
BEGIN
update T_TES_URT_TEST set tes_gov_send_by=created_by, tes_gov_send_ts=systimestamp, tes_gov_lab_no=1, updated_by=created_by, updated_ts=systimestamp  where TES_TEST_NO=vTES_TEST_NO;
END URINE_CONF_TEST;

PROCEDURE URINE_TEST_RESULT (
      vTES_TEST_NO		IN VARCHAR2,
      -- Y/N
      vRESULT IN VARCHAR2
  )
  IS
  sNUM NUMBER;
  --lNUM NUMBER;
  vGOV_LAB VARCHAR2(1);
BEGIN
select u.usr_service_id, r.tre_gov_lab into sNUM, vGOV_LAB from T_Z_UIR_USER_INST_ROLE i, t_z_usr_userid u, T_TES_URT_TEST t, t_tre_urt_request r 
where i.urr_id='URT_SV' and i.usr_id=t.created_by and i.usr_id=u.usr_id and i.ins_code=r.ins_code and t.TES_TEST_NO=vTES_TEST_NO and t.TRE_REQ_SER_NO=r.TRE_REQ_SER_NO;  

IF vGOV_LAB = 'Y' THEN
update T_TES_URT_TEST set tes_gov_lab_result=case vRESULT when 'Y' then 'Positive' else 'Negative' end, updated_by=created_by, updated_ts=systimestamp  where TES_TEST_NO=vTES_TEST_NO;
insert into t_glr_gov_lab_result(created_by, created_ts, updated_by, updated_ts, dpe_drug_type, glr_id, glr_test_result, TES_TEST_NO)
select t.created_by, systimestamp, t.created_by, systimestamp, r.dpe_drug_type, seq_glr_id.nextval, vRESULT, r.TES_TEST_NO from T_TES_URT_TEST t, T_TED_URT_TEST_DETL r where t.TES_TEST_NO=vTES_TEST_NO and t.TES_TEST_NO=r.TES_TEST_NO;
END IF;
END URINE_TEST_RESULT;


function timediff(
   ts1   in timestamp,
   ts2   in timestamp,
   units in varchar2)
/* units - ms=millisecond ss=second, mi=minute, hr=hour, dy=day */
return number
is diff interval day(9) to second(9) := ts1 - ts2;
begin
return (
extract(day from diff)*24*60*60 +
extract(hour from diff)*60*60 +
extract(minute from diff)*60 +
extract(second from diff) ) /
case (lower(units))
when 'ms' then 1/1000
when 'ss' then 1
when 'mi' then 60
when 'hr' then 60*60
when 'dy' then 24*60*60
else null
end;
end timediff;

PROCEDURE MISSED_T_IMF (
      vIML_ID		IN T_IML_IN_MAIL.IML_ID%TYPE
  )
  IS
  nLetter NUMBER;
  nCard NUMBER;
  nPhoto NUMBER;
  nOther NUMBER;
  nMLT_ID NUMBER;
  nLetterRoot NUMBER:=1;
  nCardRoot NUMBER:=1;
  nPhotoRoot NUMBER:=1;
  nOtherRoot NUMBER:=1;
  nFound NUMBER:=0;

  nCLP_ID T_IMF_IN_MAIL_FOLLOWUP.CLP_ID%TYPE;
  sIMF_REASON_CODE T_IMF_IN_MAIL_FOLLOWUP.IMF_REASON_CODE%TYPE;
  sIMF_ITEM T_IMF_IN_MAIL_FOLLOWUP.IMF_ITEM%TYPE;
  nIMF_ITEM_QTY T_IMF_IN_MAIL_FOLLOWUP.IMF_ITEM_QTY%TYPE;
  sIMF_ITEM_NAME T_IMF_IN_MAIL_FOLLOWUP.IMF_ITEM_NAME%TYPE;
  nIMF_ROOT T_IMF_IN_MAIL_FOLLOWUP.IMF_ROOT%TYPE;


BEGIN
select nvl(iml_letter,0), nvl(iml_card,0), nvl(iml_photo,0), nvl(iml_other,0), mlt_id into nLetter, nCard, nPhoto, nOther, nMLT_ID from t_iml_in_mail where iml_id=vIML_ID;

--AWOS followup 32
If nOther > 0 Then
nCLP_ID := 32;
sIMF_REASON_CODE := 'AWOS';
sIMF_ITEM := 'O';
nIMF_ITEM_QTY := nOther;
sIMF_ITEM_NAME := 'Other';
nIMF_ROOT := nOtherRoot;
nOtherRoot := 0;
insert into t_imf_in_mail_followup( imf_id, iml_id, imf_collect_date_dt, ins_code, clp_id, imf_reason_code, imf_created_by, imf_create_date_dt, imf_item, imf_item_qty, imf_item_name, imf_root, created_by, created_ts,updated_by, updated_ts)
select seq_imf_id.nextval, vIML_ID, iml_collect_date_dt, ins_code, nCLP_ID, sIMF_REASON_CODE, created_by, sysdate, sIMF_ITEM, nIMF_ITEM_QTY, sIMF_ITEM_NAME, nIMF_ROOT, created_by, systimestamp, created_by, systimestamp from t_iml_in_mail where iml_id=vIML_ID;
nFound := nFound +1;
End IF;

--mail type followup
If nMLT_ID=5 or nMLT_ID=6 or nMLT_ID=2 or nMLT_ID=3 Then
If nMLT_ID = 5 Then
nCLP_ID := 28;
sIMF_REASON_CODE := 'SOLI';
ELSIF nMLT_ID = 6 Then
nCLP_ID := 30;
sIMF_REASON_CODE := 'SP';
ELSIF nMLT_ID = 2 Then
nCLP_ID := 6;
sIMF_REASON_CODE := 'GOVT';
ELSIF nMLT_ID = 3 Then
nCLP_ID := 18;
sIMF_REASON_CODE := 'ITOI';
END IF;

If nLetter > 0 Then
sIMF_ITEM := 'L';
nIMF_ITEM_QTY := nLetter;
sIMF_ITEM_NAME := '';
nIMF_ROOT := nLetterRoot;
nLetterRoot := 0;
insert into t_imf_in_mail_followup( imf_id, iml_id, imf_collect_date_dt, ins_code, clp_id, imf_reason_code, imf_created_by, imf_create_date_dt, imf_item, imf_item_qty, imf_item_name, imf_root, created_by, created_ts,updated_by, updated_ts)
select seq_imf_id.nextval, vIML_ID, iml_collect_date_dt, ins_code, nCLP_ID, sIMF_REASON_CODE, created_by, sysdate, sIMF_ITEM, nIMF_ITEM_QTY, sIMF_ITEM_NAME, nIMF_ROOT, created_by, systimestamp, created_by, systimestamp from t_iml_in_mail where iml_id=vIML_ID;
nFound := nFound +1;
End IF;
If nCard > 0 Then
sIMF_ITEM := 'C';
nIMF_ITEM_QTY := nCard;
sIMF_ITEM_NAME := '';
nIMF_ROOT := nCardRoot;
nCardRoot := 0;
insert into t_imf_in_mail_followup( imf_id, iml_id, imf_collect_date_dt, ins_code, clp_id, imf_reason_code, imf_created_by, imf_create_date_dt, imf_item, imf_item_qty, imf_item_name, imf_root, created_by, created_ts,updated_by, updated_ts)
select seq_imf_id.nextval, vIML_ID, iml_collect_date_dt, ins_code, nCLP_ID, sIMF_REASON_CODE, created_by, sysdate, sIMF_ITEM, nIMF_ITEM_QTY, sIMF_ITEM_NAME, nIMF_ROOT, created_by, systimestamp, created_by, systimestamp from t_iml_in_mail where iml_id=vIML_ID;
nFound := nFound +1;
End IF;
If nPhoto > 0 Then
sIMF_ITEM := 'P';
nIMF_ITEM_QTY := nPhoto;
sIMF_ITEM_NAME := '';
nIMF_ROOT := nPhotoRoot;
nPhotoRoot := 0;
insert into t_imf_in_mail_followup( imf_id, iml_id, imf_collect_date_dt, ins_code, clp_id, imf_reason_code, imf_created_by, imf_create_date_dt, imf_item, imf_item_qty, imf_item_name, imf_root, created_by, created_ts,updated_by, updated_ts)
select seq_imf_id.nextval, vIML_ID, iml_collect_date_dt, ins_code, nCLP_ID, sIMF_REASON_CODE, created_by, sysdate, sIMF_ITEM, nIMF_ITEM_QTY, sIMF_ITEM_NAME, nIMF_ROOT, created_by, systimestamp, created_by, systimestamp from t_iml_in_mail where iml_id=vIML_ID;
nFound := nFound +1;
End IF;
If nOther > 0 Then
sIMF_ITEM := 'O';
nIMF_ITEM_QTY := nOther;
sIMF_ITEM_NAME := '';
nIMF_ROOT := nOtherRoot;
nOtherRoot := 0;
insert into t_imf_in_mail_followup( imf_id, iml_id, imf_collect_date_dt, ins_code, clp_id, imf_reason_code, imf_created_by, imf_create_date_dt, imf_item, imf_item_qty, imf_item_name, imf_root, created_by, created_ts,updated_by, updated_ts)
select seq_imf_id.nextval, vIML_ID, iml_collect_date_dt, ins_code, nCLP_ID, sIMF_REASON_CODE, created_by, sysdate, sIMF_ITEM, nIMF_ITEM_QTY, sIMF_ITEM_NAME, nIMF_ROOT, created_by, systimestamp, created_by, systimestamp from t_iml_in_mail where iml_id=vIML_ID;
nFound := nFound +1;
END IF;

End IF;


If nFound = 0 Then
nCLP_ID := 55050;
sIMF_REASON_CODE := 'COPR';

If nLetter > 0 Then
sIMF_ITEM := 'L';
nIMF_ITEM_QTY := nLetter;
sIMF_ITEM_NAME := 'Letter';
nIMF_ROOT := nLetterRoot;
nLetterRoot := 0;
End If;
If nCard > 0 Then
sIMF_ITEM := 'C';
nIMF_ITEM_QTY := nCard;
sIMF_ITEM_NAME := 'Card';
nIMF_ROOT := nCardRoot;
nCardRoot := 0;
End If;
If nPhoto > 0 Then
sIMF_ITEM := 'P';
nIMF_ITEM_QTY := nPhoto;
sIMF_ITEM_NAME := 'Photo';
nIMF_ROOT := nPhotoRoot;
nPhotoRoot := 0;
End IF;
insert into t_imf_in_mail_followup( imf_id, iml_id, imf_collect_date_dt, ins_code, clp_id, imf_reason_code, imf_created_by, imf_create_date_dt, imf_item, imf_item_qty, imf_item_name, imf_root, created_by, created_ts,updated_by, updated_ts)
select seq_imf_id.nextval, vIML_ID, iml_collect_date_dt, ins_code, nCLP_ID, sIMF_REASON_CODE, created_by, sysdate, sIMF_ITEM, nIMF_ITEM_QTY, sIMF_ITEM_NAME, nIMF_ROOT, created_by, systimestamp, created_by, systimestamp from t_iml_in_mail where iml_id=vIML_ID;

nFound := nFound +1;
End IF;



END MISSED_T_IMF  ; 


PROCEDURE MISSED_T_IMF_LAST (
      vCollectDate		IN VARCHAR2,
      vInsCode IN VARCHAR2
  )IS

  --vINS_CODE t_iml_in_mail.ins_code%TYPE;
  --vIML_COLLECT_DATE_DT t_iml_in_mail.iml_collect_date_dt%TYPE;
  --vIML_PRN t_iml_in_mail.iml_prn%TYPE;
  --CUR_MISSED_T_IMF PKG_SP_SELECT_REF_CURSOR.CUR_SELECT_REF_CURSOR;
  vCount Number:=0;
BEGIN  
--open CUR_MISSED_T_IMF for
--select i.ins_code, i.iml_collect_date_dt, i.iml_prn from t_iml_in_mail i where iml_followup=1 and trunc(iml_collect_date_dt)>=to_date('20230923','yyyyMMdd') and trunc(iml_collect_date_dt)<=to_date(vCollectDate,'yyyyMMdd') and mfd_id=4 and not exists(select * from t_imf_in_mail_followup f where i.iml_id=f.iml_id)
--order by i.ins_code, i.iml_collect_date_dt, i.iml_prn;

FOR r IN (select i.ins_code, i.iml_collect_date_dt, i.iml_prn, i.iml_id from t_iml_in_mail i where iml_followup=1 and i.ins_code=vInsCode and trunc(iml_collect_date_dt)>=to_date('20230923','yyyyMMdd') and trunc(iml_collect_date_dt)<=to_date(vCollectDate,'yyyyMMdd') and mfd_id=4 and not exists(select * from t_imf_in_mail_followup f where i.iml_id=f.iml_id)
order by i.ins_code, i.iml_collect_date_dt, i.iml_prn)
LOOP
    pkg_fast_track.MISSED_T_IMF(r.iml_id);
    vCount := vCount + 1;
    insert into t_dbg_debug values(systimestamp,'MISSED_T_IMF_LAST - ' || r.ins_code ||' - '||vCount,r.iml_prn,r.iml_id);
END LOOP;


END MISSED_T_IMF_LAST;

PROCEDURE T_OML_BLANK_ADM_NO (
      vOML_ID		IN T_OML_OUT_MAIL.OML_ID%TYPE
  )
  IS
vADM_NO T_OML_OUT_MAIL.ADM_NO%TYPE;
BEGIN

    select i.adm_no into vADM_NO from t_inm_inmate i, t_oml_out_mail m where i.inm_cin=m.oml_prn and i.inm_curr_ind='Y' and m.oml_id=vOML_ID;

    update T_OML_OUT_MAIL set adm_no=vADM_NO where oml_id=vOML_ID;
EXCEPTION 
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_OML_BLANK_ADM_NO','No data found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_OML_BLANK_ADM_NO','Other error');

END T_OML_BLANK_ADM_NO;

PROCEDURE T_OML_BLANK_ADM_NO_DATE (
      vCollectDate		IN VARCHAR2,
      vInsCode IN VARCHAR2
  )IS

  --vINS_CODE t_iml_in_mail.ins_code%TYPE;
  --vIML_COLLECT_DATE_DT t_iml_in_mail.iml_collect_date_dt%TYPE;
  --vIML_PRN t_iml_in_mail.iml_prn%TYPE;
  --CUR_MISSED_T_IMF PKG_SP_SELECT_REF_CURSOR.CUR_SELECT_REF_CURSOR;
  vCount Number:=0;
BEGIN  
--open CUR_MISSED_T_IMF for
--select i.ins_code, i.iml_collect_date_dt, i.iml_prn from t_iml_in_mail i where iml_followup=1 and trunc(iml_collect_date_dt)>=to_date('20230923','yyyyMMdd') and trunc(iml_collect_date_dt)<=to_date(vCollectDate,'yyyyMMdd') and mfd_id=4 and not exists(select * from t_imf_in_mail_followup f where i.iml_id=f.iml_id)
--order by i.ins_code, i.iml_collect_date_dt, i.iml_prn;

FOR r IN (select i.ins_code, i.oml_collect_date_dt, i.oml_prn, i.oml_id 
from t_oml_out_mail i 
where i.ins_code=vInsCode and trunc(oml_collect_date_dt)=to_date(vCollectDate,'yyyyMMdd') 
and i.adm_no is null
order by i.ins_code, i.oml_collect_date_dt, i.oml_prn)
LOOP
    pkg_fast_track.T_OML_BLANK_ADM_NO(r.oml_id);
    vCount := vCount + 1;
    insert into t_dbg_debug values(systimestamp,'T_OML_BLANK_ADM_NO_DATE - ' || r.ins_code ||' - '||vCount,r.oml_prn,r.oml_id);
END LOOP;


END T_OML_BLANK_ADM_NO_DATE;

PROCEDURE PATCH_IML_PRN_FOR_SORTING 
IS
BEGIN
--length = 10 and there is a '-', e.g. 0012345-67
update t_iml_in_mail set iml_prn_for_sorting=REPLACE(iml_prn_for_sorting, '-') where trunc(created_ts)>=to_date('20230923','yyyyMMdd') and length(iml_prn_for_sorting)=10 and iml_prn_for_sorting like '_______-__';
--length < 9 
update t_iml_in_mail set iml_prn_for_sorting=lpad(iml_prn_for_sorting,9,substr(iml_prn_for_sorting,1,1) ) where trunc(created_ts)>=to_date('20230923','yyyyMMdd') and length(iml_prn_for_sorting)<9;

END PATCH_IML_PRN_FOR_SORTING;

PROCEDURE PATCH_OML_PRN_FOR_SORTING 
IS
BEGIN
--length = 10 and there is a '-', e.g. 0012345-67
update t_oml_out_mail set oml_prn_for_sorting=REPLACE(oml_prn_for_sorting, '-') where trunc(created_ts)>=to_date('20230923','yyyyMMdd') and length(oml_prn_for_sorting)=10 and oml_prn_for_sorting like '_______-__';
--length < 9 
update t_oml_out_mail set oml_prn_for_sorting=lpad(oml_prn_for_sorting,9,substr(oml_prn_for_sorting,1,1) ) where trunc(created_ts)>=to_date('20230923','yyyyMMdd') and length(oml_prn_for_sorting)<9;

END PATCH_OML_PRN_FOR_SORTING;

PROCEDURE DAILY_DISCHARGED_TRANSFERRED_BAILED_NOT_RETURNING 

IS
vCount NUMBER:=0;
BEGIN
FOR r IN (select inm_cin from t_inm_inmate where ins_code='CCCI' and inm_curr_ind='Y' and inm_status='P' and created_by='ADMIN_MM' and rownum <5 order by inm_cin desc
)
LOOP
    IF vCount=0 Then
        pkg_fast_track.convert_discharged(r.inm_cin);
    ELSIF vCount=1 Then
        pkg_fast_track.convert_transferred(r.inm_cin,'LCK');  
    ELSIF vCount=2 Then
        pkg_fast_track.convert_not_returning(r.inm_cin);  
    ELSIF vCount=3 Then
        pkg_fast_track.convert_bailed(r.inm_cin);   
    --ELSIF vCount=4 Then
        --pkg_fast_track.convert_remand_convict(r.inm_cin);     
    END IF;    
    vCount := vCount + 1;
END LOOP;

END DAILY_DISCHARGED_TRANSFERRED_BAILED_NOT_RETURNING; 

PROCEDURE EXPLODE (
--exec pkg_fast_track.explode('1234,56,7,89,0,',',');

--exec pkg_fast_track.explode(',1234,56,7,89,0',',');
      vIMPLODED_STRING		IN VARCHAR2,
      vDELIMITER IN VARCHAR2
  )

IS
vSTRING VARCHAR2(2000):=vIMPLODED_STRING;
vSTRING_LENGTH NUMBER;
vDELIMITER_LENGTH NUMBER;
--vStart NUMBER;
--vEnd NUMBER;
vFoundS NUMBER;
vFoundE NUMBER;
vToken VARCHAR2(10);
BEGIN

--vIMPLODED_STRING := ',1234,56,789';
--vDELIMITER := ',';
vSTRING_LENGTH := length(vSTRING);
vDELIMITER_LENGTH := length(vDELIMITER);




WHILE vSTRING_LENGTH > 0
LOOP
   --vStart := 1;
   vFoundS := instr(vSTRING,vDELIMITER,1);
   If vFoundS > 1 Then
        vToken := substr(vSTRING,1,vFoundS-1);
        insert into t_dbg_debug values(systimestamp,'EXPLODE',vToken,null);
        commit;
        --insert into t_dbg_debug values(systimestamp,'EXPLODE','Token',to_number(vToken));
        vSTRING := substr(vSTRING,vFoundS+vDELIMITER_LENGTH-1);
        --last delimiter or ending delimiter
        If vSTRING=vDELIMITER Then 
            vSTRING:='';
        END IF;
   ElsIf vFoundS = 1 Then     
        vFoundE := instr(vSTRING,vDELIMITER,vFoundS+vDELIMITER_LENGTH);
        If vFoundE < 1 Then
            vToken := substr(vSTRING,vFoundS+vDELIMITER_LENGTH);
            insert into t_dbg_debug values(systimestamp,'EXPLODE',vToken,null);
            commit;
            --insert into t_dbg_debug values(systimestamp,'EXPLODE','Token',to_number(vToken));
            vSTRING := '';        
        Elsif vFoundE > 1 Then
            vToken := substr(vSTRING,vFoundS+vDELIMITER_LENGTH,vFoundE-vFoundS-1);
            insert into t_dbg_debug values(systimestamp,'EXPLODE',vToken,null);
            commit;
            --insert into t_dbg_debug values(systimestamp,'EXPLODE','Token',to_number(vToken));
            vSTRING := substr(vSTRING,vFoundE+vDELIMITER_LENGTH);         
        End If;
   End If;     
   vSTRING_LENGTH := nvl(length(vSTRING),0);
END LOOP;
END EXPLODE;  



PROCEDURE EXPLODE_MAX (
--exec pkg_fast_track.explode_max(',1234,56,7,89,0,12,34,567,8,90,123,456,78,90,',',',10);

--exec pkg_fast_track.explode_max('1234,56,7,89,0,12,34,567,8,90,123,456,78,90',',',10);

--exec pkg_fast_track.explode_max(',1234,56,7,89,0,12,34,567,8,90,123,456,78,90',',',10);

--exec pkg_fast_track.explode_max('1234,56,7,89,0,12,34,567,8,90,123,456,78,90,',',',10);
      vIMPLODED_STRING		IN VARCHAR2,
      vDELIMITER IN VARCHAR2,
--MAX 3900      
      vMaximum IN NUMBER
  )

IS
vSTRING VARCHAR2(4000):=vIMPLODED_STRING;
vSTRING_LENGTH NUMBER;
vDELIMITER_LENGTH NUMBER;
--vStart NUMBER;
--vEnd NUMBER;
vFoundS NUMBER;
vFoundE NUMBER;
vToken VARCHAR2(10);
vSUBSTRING VARCHAR2(4000);
--vSUBSTRING_LENGTH NUMBER;
BEGIN

--vIMPLODED_STRING := ',1234,56,789';
--vDELIMITER := ',';
vSTRING_LENGTH := length(vSTRING);
vDELIMITER_LENGTH := length(vDELIMITER);




WHILE vSTRING_LENGTH > 0
LOOP

    vSUBSTRING := substr(vSTRING,1,vMaximum);
    vFoundS := instr(vSUBSTRING,vDELIMITER,1);
    vFoundE := instr(vSUBSTRING,vDELIMITER,-1);
    If vFoundS = vFoundE Then
        vSUBSTRING := vSUBSTRING || vDELIMITER;
        vSTRING := substr(vSTRING,vMaximum + 1); 
    Else
        If vFoundS = 1 Then
            vSUBSTRING := substr(vSUBSTRING,1,vFoundE+vDELIMITER_LENGTH-1);
            vSTRING := substr(vSTRING,vFoundE);
        Else
            vSUBSTRING := vDELIMITER || substr(vSUBSTRING,1,vFoundE+vDELIMITER_LENGTH-1);
            vSTRING := substr(vSTRING,vFoundE);
        End IF;
    END IF;


    insert into t_dbg_debug values(systimestamp,'EXPLODE_MAX',vSUBSTRING,null);
    commit;

    If vSTRING=vDELIMITER||vDELIMITER or vSTRING=vDELIMITER  Then 
        vSTRING := '';
    END IF;   
   vSTRING_LENGTH := nvl(length(vSTRING),0);
END LOOP;



END EXPLODE_MAX;  

FUNCTION GET_EOG_ORG_CODE (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type
  )RETURN T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%TYPE
  IS
vEOG_ORG_CODE  T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%TYPE:=0;
BEGIN
	BEGIN
		select eog_org_code into vEOG_ORG_CODE
				from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';

		Return vEOG_ORG_CODE;
	EXCEPTION
	    WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'GET_EOG_ORG_CODE','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'GET_EOG_ORG_CODE','Other org error');
	END;	
    commit;
END GET_EOG_ORG_CODE  ;

  FUNCTION GET_EOS_SUBJECT_CODE (
  vEOG_ORG_CODE IN T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%TYPE,
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type
  )RETURN T_EPE_EV_ORG_PERIOD.EOS_SUBJECT_CODE%TYPE
  IS
vEOS_SUBJECT_CODE  T_EPE_EV_ORG_PERIOD.EOS_SUBJECT_CODE%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.EOS_SUBJECT_TYPE%TYPE;
BEGIN

    BEGIN    
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and trim(eos_desc)=vEOS_DESC ;
        return vEOS_SUBJECT_CODE;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'GET_EOS_SUBJECT_CODE - No subject','vEOG_ORG_CODE',vEOG_ORG_CODE);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'GET_EOS_SUBJECT_CODE - Other subject','vEOG_ORG_CODE',vEOG_ORG_CODE);

    END; 
    commit;
END GET_EOS_SUBJECT_CODE  ;	

PROCEDURE EDU_014_NEW_COURSE_EXAM (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  ) IS
vEPE_PERIOD_SEQ T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE; 
BEGIN

vEPE_PERIOD_SEQ := PKG_FAST_TRACK.T_EPE_INSERT(vEOG_ORG_NAME, vEOS_DESC, vINS_CODE, 'E', vFM_DT);

END EDU_014_NEW_COURSE_EXAM;


FUNCTION T_EPE_INSERT (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vEV_FLAG IN T_EPE_EV_ORG_PERIOD.EV_FLAG%TYPE,
      vFM_DT IN VARCHAR2
  ) RETURN T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE
  IS
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;
vSUBJECT_CODE t_z_eos_ev_org_subject.EOS_SUBJECT_CODE%TYPE:=0;
BEGIN

    BEGIN
        select nvl(max(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and trim(s.eos_desc)=vEOS_DESC
        --and DECODE(o.EDU_VT_FLAG,'B',vEV_FLAG)=vEV_FLAG
        and trunc(EPE_DATE_TO_DT) >= trunc(to_date(vFM_DT,'yyyyMMdd')) 
        and p.EV_FLAG=vEV_FLAG
        ;
	EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EPE_INSERT - No period','vEOG_ORG_NAME:'||vEOG_ORG_NAME); 
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EPE_INSERT - Other period','vEOG_ORG_NAME:'||vEOG_ORG_NAME);

--	WHEN NO_DATA_FOUND THEN
--		DBMS_Output.Put_Line('GET_INMNO NO DATA FOUND');
--	WHEN OTHERS THEN
--		DBMS_Output.Put_Line('GET_INMNO '||SQLCODE||' '||SUBSTR(SQLERRM, 1,200));
    END;	

	BEGIN
        IF arg_period_seq = 0 Then
                select nvl(max(EPE_PERIOD_SEQ),0)+1
                into arg_period_seq
                from T_EPE_EV_ORG_PERIOD p
                join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
                join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
                where o.eog_org_name=vEOG_ORG_NAME
                and trim(s.eos_desc)=vEOS_DESC
                and p.EV_FLAG=vEV_FLAG
                ;
          insert into T_EPE_EV_ORG_PERIOD
  ( EOG_ORG_CODE, EOS_SUBJECT_CODE, EV_FLAG, EPE_PERIOD_SEQ, EPE_DATE_FM_DT, EPE_DATE_TO_DT,
  INS_CODE, EPE_OIC, EPE_FEE, CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY, EPE_STATUS
  , EPE_REMARK , EPE_VENUE , KIOSK_FLAG
  , EPE_FP_FLAG  -- 2014-02-24
  , EPE_APPLICATION_DEADLINE
  , EPE_MAX_NUM_STUDENT, EPE_MIN_NUM_STUDENT
  )
  select o.EOG_ORG_CODE, s.EOS_SUBJECT_CODE, vEV_FLAG, arg_period_seq, sysdate + 14, sysdate + 14, vINS_CODE, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'), NVL(s.FEE,499),
  systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'), systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'), 'A', 'Fast Track', null, 'N', NVL(s.FP_Flag,'F'),
  sysdate + 14, 5, 1
  from t_z_eog_ev_org o 
  left join t_z_eos_ev_org_subject s on o.eog_org_code=s.eog_org_code 
  where o.eog_org_name=vEOG_ORG_NAME
  and trim(s.eos_desc)=vEOS_DESC;
        END IF;
		Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EPE_INSERT - '||vEOG_ORG_NAME,'arg_period_seq',arg_period_seq); 

        return arg_period_seq;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EPE_INSERT - No record','vEOG_ORG_NAME:'||vEOG_ORG_NAME); 
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EPE_INSERT - Other record','vEOG_ORG_NAME:'||vEOG_ORG_NAME);

--	WHEN NO_DATA_FOUND THEN
--		DBMS_Output.Put_Line('GET_INMNO NO DATA FOUND');
--	WHEN OTHERS THEN
--		DBMS_Output.Put_Line('GET_INMNO '||SQLCODE||' '||SUBSTR(SQLERRM, 1,200));
    END;


END T_EPE_INSERT;

PROCEDURE EDU_041_NEW_COURSE_EXAM_ENROLL (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS

BEGIN


PKG_FAST_TRACK.T_EVE_INSERT(vCIN,vEOG_ORG_NAME,vEOS_DESC,vINS_CODE,vFM_DT,'E');

END EDU_041_NEW_COURSE_EXAM_ENROLL ;

PROCEDURE T_EVE_INSERT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vEV_FLAG IN T_EPE_EV_ORG_PERIOD.EV_FLAG%TYPE
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and trim(s.eos_desc)=vEOS_DESC
        and p.ins_code=vINS_CODE
        --and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT - No period','vFM_DT',to_number(vFM_DT));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT - Other period','vFM_DT',to_number(vFM_DT));

    END;


	If arg_period_seq > 0 THEN
    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_EVE_EV_EXAM m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.eve_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and trim(s.eos_desc)=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT - No exam','arg_period_seq',arg_period_seq);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT - Other exam','arg_period_seq',arg_period_seq);

    END;
	End If;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_INSERT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_INSERT','Other org error');

    END;   */ 
    BEGIN    
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and trim(eos_desc)=vEOS_DESC ;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
             Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp,'T_EVE_INSERT - No subject','EOG_ORG_CODE',vEOG_ORG_CODE);

        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp,'T_EVE_INSERT - Other subject','EOG_ORG_CODE',vEOG_ORG_CODE);

    END; 
    BEGIN


        If vCOUNT =0 THEN

            select NVL(max(EVE_EXAM_SEQ),0)+1
            into vEVE_EXAM_SEQ
            from T_EVE_EV_EXAM 
            where inm_no=vINM and adm_no=vADM;
/*
            insert into T_EVE_EV_EXAM (inm_no, adm_no, eog_org_code, eve_exam_status, eve_first_time_ou_flag, eve_apply_status,
            eve_apply_date_dt,ins_code,EVE_ENROLL_DATE_DT,EVE_ENROLL_INS_CODE,eve_status,eve_exam_seq,eos_subject_code,ev_flag,
            eve_attend_flag,eve_period_seq,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, decode(vEOS_SUBJECT_TYPE, 'E', 'U', vEOS_SUBJECT_TYPE), 'N', 'E',
            to_date(vFM_DT,'yyyyMMdd'),vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),vINS_CODE,'A',vEVE_EXAM_SEQ,vEOS_SUBJECT_CODE,vEV_FLAG,
            '-',arg_period_seq,systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'), systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'));
*/
            insert into T_EVE_EV_EXAM (inm_no, adm_no, eog_org_code, eve_exam_status, eve_first_time_ou_flag, eve_apply_status,
            eve_apply_date_dt,ins_code,EVE_ENROLL_DATE_DT,EVE_ENROLL_INS_CODE,eve_status,eve_exam_seq,eos_subject_code,ev_flag,
            eve_attend_flag,eve_period_seq,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, 'U', 'N', 'E',
            to_date(vFM_DT,'yyyyMMdd'),vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),vINS_CODE,'A',vEVE_EXAM_SEQ,vEOS_SUBJECT_CODE,vEV_FLAG,
            '-',arg_period_seq,systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'), systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'));

        ELSE
            update T_EVE_EV_EXAM set EVE_ENROLL_DATE_DT=to_date(vFM_DT,'yyyyMMdd'),EVE_ENROLL_INS_CODE=vINS_CODE,
            eve_apply_status='E',updated_ts=systimestamp,updated_by=decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR')
            where eve_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT','arg_period_seq',arg_period_seq);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT - No record','arg_period_seq',arg_period_seq);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'T_EVE_INSERT - Other record','arg_period_seq',arg_period_seq);

    END;


END T_EVE_INSERT ;

PROCEDURE EDU_042_NEW_COURSE_EXAM_PERFORMANCE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS

BEGIN
PKG_FAST_TRACK.T_EVE_UPDATE(vCIN,vEOG_ORG_NAME,vEOS_DESC,vINS_CODE,vFM_DT,'E');


END EDU_042_NEW_COURSE_EXAM_PERFORMANCE ;

PROCEDURE T_EVE_UPDATE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vEV_FLAG IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
vERR NUMBER;
vERM VARCHAR2(100);
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_UPDATE','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_UPDATE','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_EVE_EV_EXAM m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.eve_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_UPDATE','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_UPDATE','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_UPDATE','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'T_EVE_UPDATE','Other org error');

    END;*/    
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT > 0 THEN

            update T_EVE_EV_EXAM set eve_attend_flag='A',eve_result_code=15,eve_result_date_dt=to_date(vFM_DT,'yyyyMMdd'),
            PICS_TYPE='C',CERT_RECV='N',updated_ts=systimestamp, updated_by=decode(EV_FLAG,'E','USR_EDU','V','USR_VTR')
            where eve_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        ELSE

		    select NVL(max(EVE_EXAM_SEQ),0)+1
            into vEVE_EXAM_SEQ
            from T_EVE_EV_EXAM 
            where inm_no=vINM and adm_no=vADM;

            insert into T_EVE_EV_EXAM (inm_no, adm_no, eog_org_code, eve_exam_status, eve_first_time_ou_flag, eve_apply_status,
            eve_apply_date_dt,ins_code,EVE_ENROLL_DATE_DT,EVE_ENROLL_INS_CODE,eve_status,eve_exam_seq,eos_subject_code,ev_flag,
            eve_attend_flag,eve_result_code,eve_result_date_dt,eve_period_seq,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, 'U', 'N', 'E',
            to_date(vFM_DT,'yyyyMMdd'),vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),vINS_CODE,'A',vEVE_EXAM_SEQ,vEOS_SUBJECT_CODE,vEV_FLAG,
            '-',15,to_date(vFM_DT,'yyyyMMdd'),arg_period_seq,systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'), systimestamp, decode(vEV_FLAG,'E','USR_EDU','V','USR_VTR'));


        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_EVE_UPDATE - No record','vERM:'||vERM,vERR);   
        WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_EVE_UPDATE - Other record','vERM:'||vERM,vERR); 

    END;


END T_EVE_UPDATE ;

PROCEDURE EDU_030_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vFUND_TYPE IN  T_EFF_EV_FUND_EXAM.eft_fund_type%type
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID T_EFR_EV_FUND.EFR_ID%TYPE:=0;
vCOUNT NUMBER:=0;
vEXAM_COUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;

--vEPE_PERIOD_SEQ T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%Type:=0;
--vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;
V_OFFICIAL_NO_2 T_EFR_EV_FUND.official_no%type:=0;
vEXAM_TYPE VARCHAR(1);
vEFF_ID T_EFF_EV_FUND_EXAM.EFF_ID%TYPE:=0;

vEVE_PERIOD_SEQ T_EVE_EV_EXAM.EVE_PERIOD_SEQ%TYPE:=0;

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);

    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No period','vFM_DT',to_number(vFM_DT));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other period','vFM_DT',to_number(vFM_DT));

    END;

    BEGIN
        select NVL(count(*),0), max(efr_id)
        into vCOUNT, vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No fund found','vEVP_ID',vEVP_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other fund error','vEVP_ID',vEVP_ID);

    END;
    BEGIN
        SELECT NVL(MAX(SERIAL_NO),0) + 1 INTO V_SERIAL_NO_2 FROM T_EFR_EV_FUND where serial_no<1000000000;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No serial found','V_SERIAL_NO_2',V_SERIAL_NO_2);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other serial error','V_SERIAL_NO_2',V_SERIAL_NO_2);

    END;
    BEGIN
        SELECT NVL(MAX(OFFICIAL_NO),0) + 1 INTO V_OFFICIAL_NO_2 FROM T_EFR_EV_FUND where OFFICIAL_NO<1000000000;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No official found','V_OFFICIAL_NO_2',V_OFFICIAL_NO_2);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other official error','V_OFFICIAL_NO_2',V_OFFICIAL_NO_2);

    END;

    BEGIN
		If vEOG_ORG_NAME = 'B' or vEOG_ORG_NAME = 'O' Then
			vEXAM_TYPE := vEOG_ORG_NAME;
/*
            BEGIN
                select NVL(eff_id,0)
                into vEFF_ID
                from T_EFF_EV_FUND_EXAM m
                where 
                m.efr_id=vEFR_ID and m.exam_type=vEXAM_TYPE;

            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No exam','vEFR_ID',vEFR_ID);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other exam','vEFR_ID',vEFR_ID);

            END;      
*/			

		ELSE	
			vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
			vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
			--vEVP_ID := EDU_059_SAVE(vINS_CODE,vFM_DT);
            vEVE_PERIOD_SEQ := T_EPE_INSERT(vEOG_ORG_NAME, vEOS_DESC, vINS_CODE, 'E', vFM_DT);
			SELECT EOS_SUBJECT_TYPE INTO vEXAM_TYPE FROM t_z_eos_ev_org_subject 
			where eog_org_code=vEOG_ORG_CODE and eos_subject_code=vEOS_SUBJECT_CODE;

/*
            BEGIN
                select NVL(eff_id,0)
                into vEFF_ID
                from T_EFF_EV_FUND_EXAM m
                where 
                m.efr_id=vEFR_ID and m.exam_type=vEXAM_TYPE
                and m.eog_org_code=vEOG_ORG_CODE and m.eos_subject_code=vEOS_SUBJECT_CODE and m.epe_period_seq=vEVP_ID;

            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No exam','vEFR_ID',vEFR_ID);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other exam','vEFR_ID',vEFR_ID);

            END;
*/
			--BEGIN
			select EVE_EXAM_SEQ
            into vEVE_EXAM_SEQ
            from T_EVE_EV_EXAM 
            where inm_no=vINM and adm_no=vADM
			and eog_org_code=vEOG_ORG_CODE 
			and eos_subject_code=vEOS_SUBJECT_CODE
			and eve_period_seq=vEVE_PERIOD_SEQ;
			/*
			EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No exam','vEVE_PERIOD_SEQ',vEVE_PERIOD_SEQ);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other exam','vEVE_PERIOD_SEQ',vEVE_PERIOD_SEQ);

            END;
			*/
		END IF;	

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - No subject found','vEVE_PERIOD_SEQ',vEVE_PERIOD_SEQ);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND - Other subject error','vEVE_PERIOD_SEQ',vEVE_PERIOD_SEQ);

    END;



    BEGIN



        If vCOUNT = 0 THEN
			select seq_efr_id.nextval into vEFR_ID from dual;

            insert into t_efr_ev_fund(efr_id,inm_no,adm_no,ins_code,FUND_APPL_DATE_DT,
            --serial_no,
            efr_fund_status,wf_flag,
            period_code,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY,official_no)            
            values(vEFR_ID,vINM,vADM,vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),
            --V_SERIAL_NO_2,
            'A','A',
            vEVP_ID,systimestamp,'USR_EDU',systimestamp,'USR_EDU',V_OFFICIAL_NO_2);


			insert into T_EFF_EV_FUND_EXAM (eff_id,efr_id,apply_amount,course_level,family_assistance_amount,property_amount,
			created_ts,created_by,updated_ts,updated_by,
			eog_org_code,eos_subject_code,epe_period_seq,exam_seq_no,ev_flag,exam_type,eft_fund_type)
			values (seq_eff_id.nextval,vEFR_ID,decode(vEXAM_TYPE,'C',1111,'E',111,'B',11,'O',1),1,199,99,
			systimestamp,'USR_EDU',systimestamp,'USR_EDU',
			decode(vEXAM_TYPE,'B',null,'O',null,vEOG_ORG_CODE),
			decode(vEXAM_TYPE,'B',null,'O',null,vEOS_SUBJECT_CODE),
			decode(vEXAM_TYPE,'B',null,'O',null,vEVP_ID),
			decode(vEXAM_TYPE,'B',null,'O',null,vEVE_EXAM_SEQ),'E',vEXAM_TYPE,vFUND_TYPE);
        Else
			If vEXAM_TYPE = 'B' or vEXAM_TYPE ='O' Then 
			Select NVL(count(*),0) into vEXAM_COUNT
			from T_EFF_EV_FUND_EXAM m
			where m.efr_id = vEFR_ID and exam_type = vEXAM_TYPE;
			ELSE
			Select NVL(count(*),0) into vEXAM_COUNT
			from T_EFF_EV_FUND_EXAM m
			where m.efr_id = vEFR_ID and eog_org_code = vEOG_ORG_CODE and eos_subject_code = vEOS_SUBJECT_CODE
			and epe_period_seq=vEVE_PERIOD_SEQ;

			End if;

			If vEXAM_COUNT = 0 THEN

				insert into T_EFF_EV_FUND_EXAM (eff_id,efr_id,apply_amount,course_level,family_assistance_amount,property_amount,
				created_ts,created_by,updated_ts,updated_by,
				eog_org_code,eos_subject_code,epe_period_seq,exam_seq_no,ev_flag,exam_type,eft_fund_type)
				values (seq_eff_id.nextval,vEFR_ID,decode(vEXAM_TYPE,'C',1111,'E',111,'B',11,'O',1),1,199,99,
				systimestamp,'USR_EDU',systimestamp,'USR_EDU',
				decode(vEXAM_TYPE,'B',null,'O',null,vEOG_ORG_CODE),
				decode(vEXAM_TYPE,'B',null,'O',null,vEOS_SUBJECT_CODE),
				decode(vEXAM_TYPE,'B',null,'O',null,vEVP_ID),
				decode(vEXAM_TYPE,'B',null,'O',null,vEVE_EXAM_SEQ),'E',vEXAM_TYPE,vFUND_TYPE);			
			Else
				update T_EFF_EV_FUND_EXAM set eft_fund_type=vFUND_TYPE where eff_id=vEFF_ID;
			End If;
		END IF;
			Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','vEFR_ID',vEFR_ID);


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','Other record error');

    END;


END EDU_030_COURSE_EXAM_FUND ;

PROCEDURE EDU_030_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
--vCOUNT NUMBER:=0;
--vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
--vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
--vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
--V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;
--V_OFFICIAL_NO_2 T_EFR_EV_FUND.official_no%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','Other period error');

    END;
    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','Other fund error');

    END;

    BEGIN

        If vEFR_ID > 0 THEN

            update t_efr_ev_fund set wf_flag='B', updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_030_COURSE_EXAM_FUND','Other record error');

    END;


END EDU_030_COURSE_EXAM_FUND_SUBMIT ; 
PROCEDURE EDU_033_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      --vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      --vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
vCOUNT NUMBER:=0;
--vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
--vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
--vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
--vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
--V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;
--V_OFFICIAL_NO_2 T_EFR_EV_FUND.official_no%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND - No period','vFM_DT', to_number(vFM_DT));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND - Other period','vFM_DT', to_number(vFM_DT));

    END;
    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND-No fund','vEVP_ID',vEVP_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND-Other fund','vEVP_ID',vEVP_ID);

    END;

    BEGIN
        select NVL(count(*),0) into vCOUNT
        from t_erf_ev_recom_fund where efr_id=vEFR_ID;
        If vCOUNT = 0 THEN

            insert into t_erf_ev_recom_fund(erf_id,efr_id,eligibility_no,capability_no,beneficial_no,learning_attitude_no,
            total_score,recom_reasons,recom_remark,
            recom_user_id_step1,recom_step1_date,recom_status_step1,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
            values(seq_erf_id.nextval,vEFR_ID,1,1,1,1,
            70,'Total score of 25 or more','The applicant was motivated to learn and eligible for enrolling in the course/ examination.',
            'USR_EDU',sysdate,'Y',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND','Other record error');

    END;


END EDU_033_COURSE_EXAM_FUND;
PROCEDURE EDU_033_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
--vCOUNT NUMBER:=0;
--vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
--vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
--vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
--vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
--V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;
--V_OFFICIAL_NO_2 T_EFR_EV_FUND.official_no%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND_SUBMIT','Other period error');

    END;
    BEGIN
        select efr_id
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND_SUBMIT','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND_SUBMIT','Other fund error');

    END;

    BEGIN

        If vEFR_ID > 0 THEN

            update t_efr_ev_fund set wf_flag='C', updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_033_COURSE_EXAM_FUND_SUBMIT','Other record error');

    END;


END EDU_033_COURSE_EXAM_FUND_SUBMIT ; 


PROCEDURE EDU_061_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
vCOUNT NUMBER:=0;
--vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
--vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
--vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
--vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
--V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;
--V_OFFICIAL_NO_2 T_EFR_EV_FUND.official_no%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','Other period error');

    END;

    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','Other fund error');

    END;

    BEGIN
        select NVL(count(*),0) into vCOUNT
        from t_erf_ev_recom_fund where efr_id=vEFR_ID;
        If vCOUNT > 0 THEN

            update t_erf_ev_recom_fund set recom_course_fees='R',applied_amount=500,agreement_amount=0,family_amount=0,
            deducting_amount=500,recom_user_id_step2='USR_EDU',recom_step2_date=sysdate,recom_status_step2='Y',
            recom_remark_step2='The applicant was in need of financial assistance due to insufficient financial support from family/friends'
            where efr_id=vEFR_ID;
        Else    
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','EFR_ID',vEFR_ID);

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','Other record error');

    END;


END EDU_061_COURSE_EXAM_FUND;  
--preferable
PROCEDURE EDU_061_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
	  vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vFUND_TYPE IN T_EFF_EV_FUND_EXAM.eft_fund_type%type
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
vCOUNT NUMBER:=0;
vEOG_ORG_CODE T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%Type:=0;
vEOS_SUBJECT_CODE T_EPE_EV_ORG_PERIOD.EOS_SUBJECT_CODE%Type:=0;
vEPE_PERIOD_SEQ T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%Type:=0;
vEFF_ID T_EFF_EV_FUND_EXAM.EFF_ID%TYPE:=0;

vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
vEVE_PERIOD_SEQ T_EVE_EV_EXAM.EVE_PERIOD_SEQ%type:=0;
vEXAM_TYPE VARCHAR(1);

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);

    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - No period','vFM_DT',to_number(vFM_DT));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - Other period','vFM_DT',to_number(vFM_DT));

    END;

    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - No fund','vEVP_ID',vEVP_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - Other fund','vEVP_ID',vEVP_ID);

    END;

	BEGIN
        select NVL(count(*),0) into vCOUNT
        from t_erf_ev_recom_fund where efr_id=vEFR_ID;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - No recom','vEFR_ID',vEFR_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - Other recom','vEFR_ID',vEFR_ID);

    END;



    BEGIN
		If vEOG_ORG_NAME = 'B' or vEOG_ORG_NAME = 'O' Then
			vEXAM_TYPE := vEOG_ORG_NAME;

            BEGIN
                select NVL(eff_id,0)
                into vEFF_ID
                from T_EFF_EV_FUND_EXAM m
                where 
                m.efr_id=vEFR_ID and m.exam_type=vEXAM_TYPE;

            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - No exam','vEFR_ID',vEFR_ID);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - Other exam','vEFR_ID',vEFR_ID);

            END;           

		ELSE	
			vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
			vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
			--vEVP_ID := EDU_059_SAVE(vINS_CODE,vFM_DT);
            vEVE_PERIOD_SEQ := T_EPE_INSERT(vEOG_ORG_NAME, vEOS_DESC, vINS_CODE, 'E', vFM_DT);
			SELECT EOS_SUBJECT_TYPE INTO vEXAM_TYPE FROM t_z_eos_ev_org_subject 
			where eog_org_code=vEOG_ORG_CODE and eos_subject_code=vEOS_SUBJECT_CODE;


            BEGIN
                select NVL(eff_id,0)
                into vEFF_ID
                from T_EFF_EV_FUND_EXAM m
                where 
                m.efr_id=vEFR_ID and m.exam_type=vEXAM_TYPE
                and m.eog_org_code=vEOG_ORG_CODE and m.eos_subject_code=vEOS_SUBJECT_CODE and m.epe_period_seq=vEVP_ID;

            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - No exam','vEFR_ID',vEFR_ID);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - Other exam','vEFR_ID',vEFR_ID);

            END;


			select EVE_EXAM_SEQ
            into vEVE_EXAM_SEQ
            from T_EVE_EV_EXAM 
            where inm_no=vINM and adm_no=vADM
			and eog_org_code=vEOG_ORG_CODE 
			and eos_subject_code=vEOS_SUBJECT_CODE
			and eve_period_seq=vEVE_PERIOD_SEQ;
		END IF;	

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - No subject found','vEVE_PERIOD_SEQ',vEVE_PERIOD_SEQ);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND - Other subject error','vEVE_PERIOD_SEQ',vEVE_PERIOD_SEQ);

    END;


    BEGIN

		If vEFF_ID = 0 THEN


            insert into T_EFF_EV_FUND_EXAM (eff_id,efr_id,apply_amount,course_level,family_assistance_amount,property_amount,
			created_ts,created_by,updated_ts,updated_by,
			eog_org_code,eos_subject_code,epe_period_seq,exam_seq_no,ev_flag,exam_type,eft_fund_type)
			values (seq_eff_id.nextval,vEFR_ID,decode(vEXAM_TYPE,'C',1111,'E',111,'B',11,'O',1),1,199,99,
			systimestamp,'USR_EDU',systimestamp,'USR_EDU',
			decode(vEXAM_TYPE,'B',null,'O',null,vEOG_ORG_CODE),
			decode(vEXAM_TYPE,'B',null,'O',null,vEOS_SUBJECT_CODE),
			decode(vEXAM_TYPE,'B',null,'O',null,vEVP_ID),
			decode(vEXAM_TYPE,'B',null,'O',null,vEVE_EXAM_SEQ),'E',vEXAM_TYPE,vFUND_TYPE);
        else    
            update T_EFF_EV_FUND_EXAM set eft_fund_type=vFUND_TYPE where eff_id=vEFF_ID;
		End If;

        If vCOUNT > 0 THEN

            update t_erf_ev_recom_fund set recom_course_fees='R',applied_amount=500,agreement_amount=0,family_amount=0,
            deducting_amount=500,recom_user_id_step2='USR_EDU',recom_step2_date=sysdate,recom_status_step2='Y',
            recom_remark_step2='The applicant was in need of financial assistance due to insufficient financial support from family/friends'
            where efr_id=vEFR_ID;
        Else    
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','EFR_ID',vEFR_ID);

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_061_COURSE_EXAM_FUND','Other record error');

    END;


END EDU_061_COURSE_EXAM_FUND;  


PROCEDURE EDU_062_COURSE_EXAM_FUND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
vCOUNT NUMBER:=0;
--vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
--vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
--vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
--vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
--V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;
--V_OFFICIAL_NO_2 T_EFR_EV_FUND.official_no%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','Other period error');

    END;

    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','Other fund error');

    END;

    BEGIN
        select NVL(count(*),0) into vCOUNT
        from t_erf_ev_recom_fund where efr_id=vEFR_ID;
        If vCOUNT > 0 THEN

            update t_erf_ev_recom_fund set behavior_grading=25,ins_grading=30,attitude_assigned_grading=20,
            attitude_rehab_grading=25,
            grade_level=1,recom_user_id_step3='USR_EDU',recom_step3_date=sysdate,recom_status_step3='Y',
            recom_remark_step3='The overall institutional performance of the applicant was satisfactory.'
            where efr_id=vEFR_ID;
            update t_efr_ev_fund set ins_performance_grading='A++'
            where efr_id=vEFR_ID;
        Else    
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','EFR_ID',vEFR_ID);

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND','Other record error');

    END;


END EDU_062_COURSE_EXAM_FUND;   

PROCEDURE EDU_062_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND_SUBMIT','Other period error');

    END;
    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND_SUBMIT','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND_SUBMIT','Other fund error');

    END;

    BEGIN

        If vEFR_ID > 0 THEN

            update t_efr_ev_fund set wf_flag='D', updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_062_COURSE_EXAM_FUND_SUBMIT','Other record error');

    END;


END EDU_062_COURSE_EXAM_FUND_SUBMIT ;  
PROCEDURE EDU_063_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 


BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','Other period error');

    END;
    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','Other fund error');

    END;

    BEGIN



        If vEFR_ID > 0 THEN

            update t_efr_ev_fund set wf_flag='E', recomm_vetting_committee='Y',reason_of_recommendation='The applicant was motivated to learn and eligible for enrolling in the course/ examination.
The applicant was in need of financial assistance due to insufficient financial support from family/friends
The overall institutional performance of the applicant was satisfactory.',updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;
            update t_erf_ev_recom_fund set recom_status_step4='Y',recom_step2_3_flag='X',updated_by='USR_EDU', updated_ts=systimestamp
            where efr_id=vEFR_ID;
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','Other record error');

    END;


END EDU_063_COURSE_EXAM_FUND_SUBMIT ; 

PROCEDURE EDU_063_COURSE_EXAM_FUND_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
	  vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vFUND_TYPE IN T_EFF_EV_FUND_EXAM.eft_fund_type%type
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
vEOG_ORG_CODE T_EPE_EV_ORG_PERIOD.EOG_ORG_CODE%Type:=0;
vEOS_SUBJECT_CODE T_EPE_EV_ORG_PERIOD.EOS_SUBJECT_CODE%Type:=0;
vEPE_PERIOD_SEQ T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%Type:=0;
vEFF_ID T_EFF_EV_FUND_EXAM.EFF_ID%TYPE:=0;
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
vEXAM_TYPE VARCHAR(1);

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
--vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
--vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN

        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';
	/*
	    select NVL(max(p.EPE_PERIOD_SEQ),0)
        into vEPE_PERIOD_SEQ
        from T_EPE_EV_ORG_PERIOD p
        where trunc(p.epe_date_fm_dt)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.epe_date_to_dt)>=to_date(vFM_DT,'yyyyMMdd')
		and p.EOG_ORG_CODE=vEOG_ORG_CODE
		and p.EOS_SUBJECT_CODE=vEOS_SUBJECT_CODE
		and p.ins_code=vINS_CODE
        and epe_status='A';
    */
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT - No period','vFM_DT',vFM_DT);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT - Other period','vFM_DT',vFM_DT);

    END;
    BEGIN
        select NVL(efr_id,0)
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT - No fund','vEVP_ID',vEVP_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT - Other fund','vEVP_ID',vEVP_ID);

    END;

	BEGIN
        select NVL(eff_id,0)
        into vEFF_ID
        from T_EFF_EV_FUND_EXAM m
        where 
        m.efr_id=vEFR_ID;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT - No exam','vEFR_ID',vEFR_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT - Other exam','vEFR_ID',vEFR_ID);

    END;

    BEGIN
		If vEOG_ORG_NAME = 'B' or vEOG_ORG_NAME = 'O' Then
			vEXAM_TYPE := vEOG_ORG_NAME;

            BEGIN
                select NVL(eff_id,0)
                into vEFF_ID
                from T_EFF_EV_FUND_EXAM m
                where 
                m.efr_id=vEFR_ID and m.exam_type=vEXAM_TYPE;

            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND - No exam','vEFR_ID',vEFR_ID);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND - Other exam','vEFR_ID',vEFR_ID);

            END;           

		ELSE	
			vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
			vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
			--vEVP_ID := EDU_059_SAVE(vINS_CODE,vFM_DT);
            vEVP_ID := T_EPE_INSERT(vEOG_ORG_NAME, vEOS_DESC, vINS_CODE, 'E', vFM_DT);
			SELECT EOS_SUBJECT_TYPE INTO vEXAM_TYPE FROM t_z_eos_ev_org_subject 
			where eog_org_code=vEOG_ORG_CODE and eos_subject_code=vEOS_SUBJECT_CODE;


            BEGIN
                select NVL(eff_id,0)
                into vEFF_ID
                from T_EFF_EV_FUND_EXAM m
                where 
                m.efr_id=vEFR_ID and m.exam_type=vEXAM_TYPE
                and m.eog_org_code=vEOG_ORG_CODE and m.eos_subject_code=vEOS_SUBJECT_CODE and m.epe_period_seq=vEVP_ID;

            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND - No exam','vEFR_ID',vEFR_ID);
                WHEN OTHERS THEN
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND - Other exam','vEFR_ID',vEFR_ID);

            END;


			select EVE_EXAM_SEQ
            into vEVE_EXAM_SEQ
            from T_EVE_EV_EXAM 
            where inm_no=vINM and adm_no=vADM
			and eog_org_code=vEOG_ORG_CODE 
			and eos_subject_code=vEOS_SUBJECT_CODE
			and eve_period_seq=vEVP_ID;
		END IF;	

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND - No subject found','vEVP_ID',vEVP_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND - Other subject error','vEVP_ID',vEVP_ID);

    END;




    BEGIN
/*
		If vEFF_ID = 0 THEN

			insert into T_EFF_EV_FUND_EXAM (eff_id,efr_id,apply_amount,course_level,family_assistance_amount,property_amount,
			created_ts,created_by,updated_ts,updated_by,
			eog_org_code,eos_subject_code,epe_period_seq,ev_flag,exam_type,eft_fund_type)
			values (seq_eff_id.nextval,vEFR_ID,499,1,199,99,
			systimestamp,'USR_EDU',systimestamp,'USR_EDU',
			vEOG_ORG_CODE,vEOS_SUBJECT_CODE,vEPE_PERIOD_SEQ,'E','E',vFUND_TYPE);
		End If;
*/        
        If vEFF_ID = 0 THEN


            insert into T_EFF_EV_FUND_EXAM (eff_id,efr_id,apply_amount,course_level,family_assistance_amount,property_amount,
			created_ts,created_by,updated_ts,updated_by,
			eog_org_code,eos_subject_code,epe_period_seq,exam_seq_no,ev_flag,exam_type,eft_fund_type)
			values (seq_eff_id.nextval,vEFR_ID,decode(vEXAM_TYPE,'C',1111,'E',111,'B',11,'O',1),1,199,99,
			systimestamp,'USR_EDU',systimestamp,'USR_EDU',
			decode(vEXAM_TYPE,'B',null,'O',null,vEOG_ORG_CODE),
			decode(vEXAM_TYPE,'B',null,'O',null,vEOS_SUBJECT_CODE),
			decode(vEXAM_TYPE,'B',null,'O',null,vEVP_ID),
			decode(vEXAM_TYPE,'B',null,'O',null,vEVE_EXAM_SEQ),'E',vEXAM_TYPE,vFUND_TYPE);
        else    
            update T_EFF_EV_FUND_EXAM set eft_fund_type=vFUND_TYPE where eff_id=vEFF_ID;
		End If;

        If vEFR_ID > 0 THEN

            update t_efr_ev_fund set wf_flag='E', recomm_vetting_committee='Y',reason_of_recommendation='The applicant was motivated to learn and eligible for enrolling in the course/ examination.
The applicant was in need of financial assistance due to insufficient financial support from family/friends
The overall institutional performance of the applicant was satisfactory.',updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;
            update t_erf_ev_recom_fund set recom_status_step4='Y',recom_step2_3_flag='X',updated_by='USR_EDU', updated_ts=systimestamp
            where efr_id=vEFR_ID;
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_063_COURSE_EXAM_FUND_SUBMIT','Other record error');

    END;


END EDU_063_COURSE_EXAM_FUND_SUBMIT ; 

PROCEDURE EDU_064_COURSE_EXAM_FUND_ENDORSE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_064_COURSE_EXAM_FUND_ENDORSE','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_064_COURSE_EXAM_FUND_ENDORSE','Other period error');

    END;
    BEGIN
        select efr_id
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_064_COURSE_EXAM_FUND_ENDORSE','No fund found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_064_COURSE_EXAM_FUND_ENDORSE','Other fund error');

    END;

    BEGIN

        If vEFR_ID > 0 THEN

            update t_efr_ev_fund set wf_flag='F',updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_064_COURSE_EXAM_FUND_ENDORSE','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_064_COURSE_EXAM_FUND_ENDORSE','Other record error');

    END;


END EDU_064_COURSE_EXAM_FUND_ENDORSE ;  

PROCEDURE EDU_034_COURSE_EXAM_FUND_COMPLETE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE:=0;  
vEFR_ID t_efr_ev_fund.EFR_ID%TYPE:=0; 
V_SERIAL_NO_2 T_EFR_EV_FUND.serial_no%type:=0;

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_034_COURSE_EXAM_FUND_COMPLETE - No period','vFM_DT',to_number(vFM_DT));
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_034_COURSE_EXAM_FUND_COMPLETE - Other period','vFM_DT',to_number(vFM_DT));

    END;
    BEGIN
        select efr_id
        into vEFR_ID
        from t_efr_ev_fund m
        where 
        m.ins_code=vINS_CODE
        and m.period_code=vEVP_ID
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_034_COURSE_EXAM_FUND_COMPLETE - No fund','vEVP_ID',vEVP_ID);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_034_COURSE_EXAM_FUND_COMPLETE - Other fund','vEVP_ID',vEVP_ID);

    END;

    BEGIN

        If vEFR_ID > 0 THEN
            --SELECT NVL(max(SERIAL_NO), 0)+1 INTO V_SERIAL_NO_2 FROM T_EFR_EV_FUND;
            SELECT NVL(max(SERIAL_NO), 0)+1 INTO V_SERIAL_NO_2 FROM T_EFR_EV_FUND where serial_no<1000000000;

            update t_efr_ev_fund set wf_flag='Z',approve_flag='Y',approve_date=sysdate,serial_no=V_SERIAL_NO_2,
            updated_by='USR_EDU', updated_ts=systimestamp where efr_id=vEFR_ID;

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_034_COURSE_EXAM_FUND_COMPLETE - No record found','V_SERIAL_NO_2',V_SERIAL_NO_2);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'EDU_034_COURSE_EXAM_FUND_COMPLETE - Other record error','V_SERIAL_NO_2',V_SERIAL_NO_2);

    END;


END EDU_034_COURSE_EXAM_FUND_COMPLETE ;  


PROCEDURE EDU_COURSE_EXAM_FUND_COMPLETE_4_PIC (
      vCIN_1 IN t_inm_inmate.inm_cin%type,
      vCIN_2 IN t_inm_inmate.inm_cin%type,
	  vCIN_3 IN t_inm_inmate.inm_cin%type,
      vCIN_4 IN t_inm_inmate.inm_cin%type,
	  vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type
  )IS

  	  vEOG_ORG_NAME_1	 t_z_eog_ev_org.eog_org_name%type := 'CUHKSCS';
      vEOS_DESC_1  t_z_eos_ev_org_subject.eos_desc%type := '820199 Event Management';
	  vEOG_ORG_NAME_2	 t_z_eog_ev_org.eog_org_name%type := 'OU';
      vEOS_DESC_2  t_z_eos_ev_org_subject.eos_desc%type := 'BIS B870 Electronic Commerce for Managers';
	  vEOG_ORG_NAME_3	 t_z_eog_ev_org.eog_org_name%type := 'HKIAAT';
      vEOS_DESC_3  t_z_eos_ev_org_subject.eos_desc%type := 'Auditing';
      vFM_DT  VARCHAR2(8) := to_char(sysdate,'yyyyMMdd');
	  vFUND_TYPE VARCHAR2(1) := 'A';
BEGIN
edu_014_new_course_exam(vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT);
edu_014_new_course_exam(vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT);
edu_014_new_course_exam(vEOG_ORG_NAME_3,vEOS_DESC_3,vINS_CODE,vFM_DT);

edu_041_new_course_exam_enroll(vCIN_1,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT);
edu_041_new_course_exam_enroll(vCIN_2,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT);
edu_041_new_course_exam_enroll(vCIN_3,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT);
edu_041_new_course_exam_enroll(vCIN_4,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT);

edu_041_new_course_exam_enroll(vCIN_1,vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT);
edu_041_new_course_exam_enroll(vCIN_2,vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT);
edu_041_new_course_exam_enroll(vCIN_3,vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT);

edu_041_new_course_exam_enroll(vCIN_1,vEOG_ORG_NAME_3,vEOS_DESC_3,vINS_CODE,vFM_DT);
edu_041_new_course_exam_enroll(vCIN_2,vEOG_ORG_NAME_3,vEOS_DESC_3,vINS_CODE,vFM_DT);


--T_EFR_EV_FUND T_EFF_EV_FUND_EXAM
edu_030_course_exam_fund(vCIN_1,'B','',vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_2,'B','',vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_3,'B','',vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_4,'B','',vINS_CODE,vFM_DT,vFUND_TYPE);

edu_030_course_exam_fund(vCIN_1,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_2,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_3,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_4,vEOG_ORG_NAME_1,vEOS_DESC_1,vINS_CODE,vFM_DT,vFUND_TYPE);

edu_030_course_exam_fund(vCIN_1,vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_2,vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_3,vEOG_ORG_NAME_2,vEOS_DESC_2,vINS_CODE,vFM_DT,vFUND_TYPE);

edu_030_course_exam_fund(vCIN_1,vEOG_ORG_NAME_3,vEOS_DESC_3,vINS_CODE,vFM_DT,vFUND_TYPE);
edu_030_course_exam_fund(vCIN_2,vEOG_ORG_NAME_3,vEOS_DESC_3,vINS_CODE,vFM_DT,vFUND_TYPE);

edu_030_course_exam_fund(vCIN_1,'O','',vINS_CODE,vFM_DT,vFUND_TYPE);

--t_erf_ev_recom_fund
edu_033_course_exam_fund(vCIN_1,vINS_CODE,vFM_DT);
edu_033_course_exam_fund(vCIN_2,vINS_CODE,vFM_DT);
edu_033_course_exam_fund(vCIN_3,vINS_CODE,vFM_DT);
edu_033_course_exam_fund(vCIN_4,vINS_CODE,vFM_DT);

edu_034_course_exam_fund_complete(vCIN_1,vINS_CODE,vFM_DT);
edu_034_course_exam_fund_complete(vCIN_2,vINS_CODE,vFM_DT);
edu_034_course_exam_fund_complete(vCIN_3,vINS_CODE,vFM_DT);
edu_034_course_exam_fund_complete(vCIN_4,vINS_CODE,vFM_DT);






END EDU_COURSE_EXAM_FUND_COMPLETE_4_PIC;  

PROCEDURE EDU_005_WAIT_FOR_CLASS_ALLOC (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EVS_EV_CASE.edu_ins_code%type
)IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVS_ID T_EVS_EV_CASE.EVS_ID%TYPE:=0;  
vEVD_ID T_EVD_EV_CASE_DETAILS.EVD_ID%TYPE:=0; 

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(max(p.EVS_ID),0)
        into vEVS_ID
        from T_EVS_EV_CASE p
        where         
        p.inm_no=vINM
        and p.adm_no=vADM
        and evs_status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_005_WAIT_FOR_CLASS_ALLOC','No case found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_005_WAIT_FOR_CLASS_ALLOC','Other case error');

    END;

    BEGIN

        If vEVS_ID = 0 THEN
            select seq_evs_id.nextval into vEVS_ID from dual;
            insert into T_EVS_EV_CASE(evs_id,inm_no,adm_no,edu_ins_code,vtu_ins_code,evs_status,
            created_ts,created_by,updated_ts,updated_by)
            values(vEVS_ID,vINM,vADM,vINS_CODE,vINS_CODE,'A',
            systimestamp,'USR_EDU',systimestamp,'USR_EDU');

            insert into T_EVD_EV_CASE_DETAILS(evs_id,evd_id,evd_ins_code,evd_remark,evd_recomm_class,curr_ind,evd_status,
            created_ts,created_by,updated_ts,updated_by)
            values(vEVS_ID,seq_evd_id.nextval,vINS_CODE,'Fast Track remarks',20,'Y','A',
            systimestamp,'USR_EDU',systimestamp,'USR_EDU');

        ELSE
                select NVL(max(p.EVD_ID),0)
                into vEVD_ID
                from T_EVD_EV_CASE_DETAILS p
                where         
                p.evs_id=vEVS_ID;
                      If vEVD_ID = 0 THEN
            insert into T_EVD_EV_CASE_DETAILS(evs_id,evd_id,evd_ins_code,evd_remark,evd_recomm_class,curr_ind,evd_status,
            created_ts,created_by,updated_ts,updated_by)
            values(vEVS_ID,seq_evd_id.nextval,vINS_CODE,'Fast Track remarks',20,'Y','A',
            systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      END IF;
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_005_WAIT_FOR_CLASS_ALLOC','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_005_WAIT_FOR_CLASS_ALLOC','Other record error');

    END;


END EDU_005_WAIT_FOR_CLASS_ALLOC ;

PROCEDURE EDU_002_ATTAINMENT_TEST_PERFORMANCE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EVD_EV_CASE_DETAILS.evd_ins_code%type,
      vCLASS IN VARCHAR2,
      vTEST_DT IN VARCHAR2
)IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEVS_ID T_EVS_EV_CASE.EVS_ID%TYPE:=0;  
vEVD_ID T_EVD_EV_CASE_DETAILS.EVD_ID%TYPE:=0; 
vEVD_RECOMM_CLASS T_EVD_EV_CASE_DETAILS.EVD_RECOMM_CLASS%TYPE:=0;
vCOUNT NUMBER:=0;

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select max(p.EVS_ID)
        into vEVS_ID
        from T_EVS_EV_CASE p
        where         
        p.inm_no=vINM
        --and p.adm_no=vADM
        and evs_status='A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','No case found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','Other case error');

    END;
      BEGIN
        select cma_master_code into vEVD_RECOMM_CLASS from t_z_cma_code_master where cma_master_type='EV_CLASS' and cma_status='A' and cma_c1=vCLASS;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','No class found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','Other class error');

    END;
    BEGIN

        select NVL(max(p.EVD_ID),0)
        into vEVD_ID
        from T_EVD_EV_CASE_DETAILS p
        where         
        p.evs_id=vEVS_ID
        and p.EVD_RECOMM_CLASS=vEVD_RECOMM_CLASS
        and evd_status='A';

        If vEVD_ID = 0 THEN
                select NVL(max(p.EVD_ID),0)
                into vEVD_ID
                from T_EVD_EV_CASE_DETAILS p
                where         
                p.evs_id=vEVS_ID
                and p.EVD_RECOMM_CLASS is null
                and curr_ind='Y'
                and evd_status='A';

            If vEVD_ID > 0 THEN
            -- update class, insert
                select NVL(count(*),0)
                into vCOUNT
                from T_ASR_ATTAINMENT_TEST_RESULT p
                where         
                p.evd_id=vEVD_ID;

                      If vCOUNT = 0 THEN
                      update T_EVD_EV_CASE_DETAILS set EVD_RECOMM_CLASS=vEVD_RECOMM_CLASS,evd_test_date_dt=to_date(vTEST_DT,'yyyyMMdd'),updated_ts=systimestamp,updated_by='USR_EDU' where evd_id=vEVD_ID;
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,1,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,2,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,3,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      ELSE
                        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','Record found');
                      END IF;
            ELSE
            -- insert, insert
                      select seq_evd_id.nextval into vEVD_ID from dual;
                        insert into T_EVD_EV_CASE_DETAILS(evs_id,evd_id,evd_ins_code,evd_remark,evd_recomm_class,evd_test_date_dt,curr_ind,evd_status,
            created_ts,created_by,updated_ts,updated_by)
            values(vEVS_ID,vEVD_ID,vINS_CODE,'Fast Track remarks',vEVD_RECOMM_CLASS,to_date(vTEST_DT,'yyyyMMdd'),'Y','A',
            systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,1,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,2,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,3,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
            END IF;
        ELSE
          -- update date, insert
                select NVL(count(*),0)
                into vCOUNT
                from T_ASR_ATTAINMENT_TEST_RESULT p
                where         
                p.evd_id=vEVD_ID;

                      If vCOUNT = 0 THEN
                      update T_EVD_EV_CASE_DETAILS set evd_test_date_dt=to_date(vTEST_DT,'yyyyMMdd'),updated_ts=systimestamp,updated_by='USR_EDU' where evd_id=vEVD_ID;
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,1,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,2,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      insert into T_ASR_ATTAINMENT_TEST_RESULT(asr_id,evd_id,att_test_subject_code,test_subject_result,att_result_level,
                      created_ts,created_by,updated_ts,updated_by)
                      values(seq_asr_id.nextval,vEVD_ID,3,99,18,
                      systimestamp,'USR_EDU',systimestamp,'USR_EDU');
                      ELSE
                        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','Record found');
                      END IF;
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_002_ATTAINMENT_TEST_PERFORMANCE','Other record error');

    END;


END EDU_002_ATTAINMENT_TEST_PERFORMANCE ;

PROCEDURE EDU_006_CLASS_ALLOC (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EVD_EV_CASE_DETAILS.evd_ins_code%type,
      vCLASS IN VARCHAR2
)IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;

vCLASS_SEQ_NO T_ECA_EDU_CLASS_ALLOC.class_seq_no%TYPE:=0;  

vEDC_CLASS_CODE T_ECA_EDU_CLASS_ALLOC.EDC_CLASS_CODE%TYPE:=0;

vCOUNT NUMBER:=0;

BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select NVL(count(*),0) into vCOUNT 
        from T_ECA_EDU_CLASS_ALLOC b 
        where b.inm_no = vINM 
        and b.adm_no = vADM
        and b.OUT_DATE_DT is null 
        and b.ECA_STATUS = 'A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','No class found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other class error');

    END;
    IF vCOUNT>0 THEN
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Class found error');
        RETURN;
    END IF;
    BEGIN
        select NVL(count(*),0) into vCOUNT 
        from T_TEV_TRANSFER_EVU c 
        WHERE c.inm_no = vINM
        AND c.adm_no = vADM
        AND c.TEV_STATUS = 'A' 
        AND c.ins_code = VINS_CODE;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','No transfer found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other transfer error');

    END;
    IF vCOUNT=0 THEN
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Transfer not found error');
        RETURN;
    END IF;
    BEGIN
        select NVL(count(*),0) into vCOUNT 
        from T_EVS_EV_CASE d 
        WHERE d.inm_no = vINM
        AND d.adm_no = vADM 
        AND d.EVS_STATUS = 'A';


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','No case found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other case error');

    END;
    IF vCOUNT=0 THEN
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Case not found error');
        RETURN;
    END IF;
    BEGIN
        select cma_master_code into vEDC_CLASS_CODE from t_z_cma_code_master where cma_master_type='EV_CLASS' and cma_status='A' and cma_c1=vCLASS;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','No class found');
            RETURN;
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other class error');

    END;
    BEGIN
        select NVL(count(*),0) 
        into vCOUNT
        from T_Z_ECI_EDU_CLASS_INST where ins_code=vINS_CODE
        and eci_status='A'
        and edc_class_code=vEDC_CLASS_CODE;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','No inst class found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other inst class error');

    END;
    IF vCOUNT=0 THEN
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Inst Class not found error');
        RETURN;
    END IF;
    BEGIN
        select NVL(max(class_seq_no),0)+1 
        into vCLASS_SEQ_NO
        from t_eca_edu_class_alloc 
        where 
        inm_no=vINM
        and adm_no=vADM
        and eca_status='A'
        and edc_class_code=vEDC_CLASS_CODE;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','No alloc found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other alloc error');

    END;
    BEGIN
    insert into t_eca_edu_class_alloc(eca_id,inm_no,adm_no,class_seq_no,ins_code,edc_class_code,in_date_dt,work_seq_no,eca_status,remark,
    created_ts,created_by,updated_ts,updated_by)
    values(seq_eca_id.nextval,vINM,vADM,vCLASS_SEQ_NO,vINS_CODE,vEDC_CLASS_CODE,sysdate,0,'A','Fast Track remark',
    systimestamp,'USR_EDU',systimestamp,'USR_EDU');                      
    EXCEPTION
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_006_CLASS_ALLOC','Other record error');

    END;
END EDU_006_CLASS_ALLOC ;

FUNCTION EDU_059_SAVE (
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )RETURN T_Z_EVP_EV_PERIOD.EVP_ID%TYPE
IS
vEVP_ID T_Z_EVP_EV_PERIOD.EVP_ID%TYPE;
BEGIN

    BEGIN
        select NVL(max(p.EVP_ID),0)
        into vEVP_ID
        from T_Z_EVP_EV_PERIOD p
        where trunc(p.evp_period_start_date)<=to_date(vFM_DT,'yyyyMMdd')
        and trunc(p.evp_period_end_date)>=to_date(vFM_DT,'yyyyMMdd')
        and status='A';

        if vEVP_ID = 0 THEN
            select seq_evp_id.nextval into vEVP_ID from dual;
            insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
            remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
            values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
            'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        END IF;
        RETURN vEVP_ID;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_059_SAVE','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'EDU_059_SAVE','Other period error');

    END;


END EDU_059_SAVE;

PROCEDURE VTR_075_NEW_COURSE_EXAM (
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  ) IS
vEPE_PERIOD_SEQ T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE;
BEGIN

vEPE_PERIOD_SEQ := PKG_FAST_TRACK.T_EPE_INSERT(vEOG_ORG_NAME, vEOS_DESC, vINS_CODE, 'V', vFM_DT);

END VTR_075_NEW_COURSE_EXAM;

PROCEDURE VTR_004_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp, 'VTR_004_SUBMIT','vEOG_ORG_CODE',vEOG_ORG_CODE);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value)values (systimestamp, 'VTR_004_SUBMIT','vEOS_SUBJECT_CODE',vEOS_SUBJECT_CODE);
            --commit;

    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_ENROLL_FLAG='E',ENR_STATUS='T',ENR_WITHDRAW='N',BAS_CIN=TO_NUMBER(vCIN),
            ENR_FAIL_REASON_1='N',ENR_FAIL_REASON_2='N',ENR_FAIL_REASON_3='N',ENR_FAIL_REASON_4='N',
            ENR_FAIL_REASON_5='N',ENR_FAIL_REASON_6='N',ENR_FAIL_REASON_7='N',
            ENR_APPLICATION_DT=to_date(vFM_DT,'yyyyMMdd'),
            ENR_REMARK='Fast track',PICS_TYPE='C',updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_SUBMIT','Other subject error');

    END;


END VTR_004_SUBMIT ;

PROCEDURE VTR_035_EVALUATE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME  and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,ENR_EVAL_FROM_PIC,ENR_EVAL_SUMMARY,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,'Fast Track evaluation from PIC','Fast Track evaluation summary',
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_ENROLL_FLAG='E',ENR_STATUS='T',ENR_WITHDRAW='N',BAS_CIN=TO_NUMBER(vCIN),
            ENR_FAIL_REASON_1='N',ENR_FAIL_REASON_2='N',ENR_FAIL_REASON_3='N',ENR_FAIL_REASON_4='N',
            ENR_FAIL_REASON_5='N',ENR_FAIL_REASON_6='N',ENR_FAIL_REASON_7='N',
            ENR_APPLICATION_DT=to_date(vFM_DT,'yyyyMMdd'),
            ENR_REMARK='Fast track',PICS_TYPE='C',ENR_EVAL_FROM_PIC='Fast Track evaluation from PIC',ENR_EVAL_SUMMARY='Fast Track evaluation summary',
            updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_035_EVALUATE','Other subject error');

    END;


END VTR_035_EVALUATE ;

PROCEDURE VTR_050_WITHDRAW (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,ENR_WITHDRAW,ENR_WITHDRAW_DATE_DT,ENR_WITHDRAW_REASON,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,'S',to_date(vFM_DT,'yyyyMMdd'),'Fast Track withdraw reason',
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_ENROLL_FLAG='E',ENR_STATUS='T',BAS_CIN=TO_NUMBER(vCIN),
            ENR_FAIL_REASON_1='N',ENR_FAIL_REASON_2='N',ENR_FAIL_REASON_3='N',ENR_FAIL_REASON_4='N',
            ENR_FAIL_REASON_5='N',ENR_FAIL_REASON_6='N',ENR_FAIL_REASON_7='N',
            ENR_APPLICATION_DT=to_date(vFM_DT,'yyyyMMdd'),
            ENR_REMARK='Fast track',PICS_TYPE='C',
            ENR_WITHDRAW='S',ENR_WITHDRAW_DATE_DT=to_date(vFM_DT,'yyyyMMdd'),ENR_WITHDRAW_REASON='Fast Track withdraw reason',
            updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_050_WITHDRAW','Other subject error');

    END;


END VTR_050_WITHDRAW ;


PROCEDURE VTR_051_WITHDRAW_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,ENR_WITHDRAW,ENR_WITHDRAW_DATE_DT,ENR_WITHDRAW_REASON,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast Track withdrawal approved', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,'Y',sysdate,'Fast Track withdraw reason',
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_ENROLL_FLAG='E',ENR_STATUS='T',BAS_CIN=TO_NUMBER(vCIN),
            ENR_FAIL_REASON_1='N',ENR_FAIL_REASON_2='N',ENR_FAIL_REASON_3='N',ENR_FAIL_REASON_4='N',
            ENR_FAIL_REASON_5='N',ENR_FAIL_REASON_6='N',ENR_FAIL_REASON_7='N',
            ENR_APPLICATION_DT=to_date(vFM_DT,'yyyyMMdd'),
            ENR_REMARK='Fast Track withdrawal approved',PICS_TYPE='C',
            ENR_WITHDRAW='Y',ENR_WITHDRAW_DATE_DT=sysdate,ENR_WITHDRAW_REASON='Fast Track withdraw reason',
            updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_051_WITHDRAW_APPROVE','Other subject error');

    END;


END VTR_051_WITHDRAW_APPROVE ;


PROCEDURE VTR_004_REJECT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'F',
            'Y','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_ENROLL_FLAG='F',ENR_STATUS='T',ENR_WITHDRAW='N',BAS_CIN=TO_NUMBER(vCIN),
            ENR_FAIL_REASON_1='Y',ENR_FAIL_REASON_2='N',ENR_FAIL_REASON_3='N',ENR_FAIL_REASON_4='N',
            ENR_FAIL_REASON_5='N',ENR_FAIL_REASON_6='N',ENR_FAIL_REASON_7='N',
            ENR_APPLICATION_DT=to_date(vFM_DT,'yyyyMMdd'),
            ENR_REMARK='Fast track',PICS_TYPE='C',updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_004_REJECT','Other subject error');

    END;


END VTR_004_REJECT ;

PROCEDURE VTR_019_APPEAL (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,ENR_APPEAL,ENR_APPEAL_REASON,ENR_APPEAL_DT,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'F',
            'Y','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,'S','Fast track appeal',to_date(vFM_DT,'yyyyMMdd'),
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set enr_status='T',enr_enroll_flag='F',ENR_FAIL_REASON_1='Y',ENR_WITHDRAW='N',ENR_APPEAL='S',ENR_APPEAL_REASON='Fast track appeal',
            ENR_APPEAL_DT=to_date(vFM_DT,'yyyyMMdd'),
            updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_019_APPEAL','Other subject error');

    END;


END VTR_019_APPEAL ;

PROCEDURE VTR_020_APPEAL_RECOMMEND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,ENR_APPEAL,ENR_APPEAL_REASON,ENR_APPEAL_DT,ENR_APPEAL_RECOMMEND_REMARK,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'F',
            'Y','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,'R','Fast track appeal',to_date(vFM_DT,'yyyyMMdd'),'Fast track appeal recommend',
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set enr_status='T',enr_enroll_flag='F',ENR_FAIL_REASON_1='Y',ENR_WITHDRAW='N',ENR_APPEAL='R',ENR_APPEAL_REASON='Fast track appeal',ENR_APPEAL_RECOMMEND_REMARK='Fast track appeal recommend',
            ENR_APPEAL_DT=to_date(vFM_DT,'yyyyMMdd'),
            updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_020_APPEAL_RECOMMEND','Other subject error');

    END;


END VTR_020_APPEAL_RECOMMEND ;

PROCEDURE VTR_021_APPEAL_GRANTED (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,ENR_APPEAL,ENR_APPEAL_REASON,ENR_APPEAL_DT,ENR_APPEAL_RECOMMEND_REMARK,ENR_APP_STATUS,ENR_GM_REMARK,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'F',
            'Y','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'T',arg_period_seq,vEOS_SUBJECT_CODE,'A','Fast track appeal',to_date(vFM_DT,'yyyyMMdd'),'Fast track appeal recommend','R','Fast track appeal granted',
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set enr_status='T',enr_enroll_flag='F',ENR_FAIL_REASON_1='Y',ENR_WITHDRAW='N',ENR_APPEAL='A',ENR_APPEAL_REASON='Fast track appeal',ENR_APPEAL_RECOMMEND_REMARK='Fast track appeal recommend',ENR_APP_STATUS='R',ENR_GM_REMARK='Fast track appeal granted',
            ENR_APPEAL_DT=to_date(vFM_DT,'yyyyMMdd'),
            updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_021_APPEAL_GRANTED','Other subject error');

    END;


END VTR_021_APPEAL_GRANTED ;

--obsolete
PROCEDURE VTR_006_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'R',arg_period_seq,vEOS_SUBJECT_CODE,
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_STATUS='R',updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','Other subject error');

    END;

    BEGIN
        select nvl(count(*),0) into vCOUNT from T_WRU_WORK_RECOMMEND_USER 
        where inm_no=vINM and adm_no=vADM and wkr_ser_no=arg_period_seq 
        and usr_id='USR_WVT' and wru_status='R' and wru_type='V';
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_WRU_WORK_RECOMMEND_USER (inm_no, adm_no, wkr_ser_no, usr_id, wru_status,wru_type,wru_recommend_dt,
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, arg_period_seq, 'USR_WVT', 'R','V',to_date(vFM_DT,'yyyyMMdd'),
            systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
        ELSE
            update T_WRU_WORK_RECOMMEND_USER set wru_recommend_dt=to_date(vFM_DT,'yyyyMMdd'),updated_ts=systimestamp,updated_by='USR_WVT'
            where inm_no=vINM and adm_no=vADM and wkr_ser_no=arg_period_seq 
            and usr_id='USR_WVT' and wru_status='R' and wru_type='V'; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_SUBMIT','Other record error');

    END;


END VTR_006_SUBMIT ;

PROCEDURE VTR_006_RECOMMEND (
      --vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vUSR_RECOMMEND IN VARCHAR2
  )IS

arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','Other period error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','Other org error');

    END;    */
    /*BEGIN

        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','Other subject error');

    END; */
    BEGIN
        For r in (select e.inm_no, e.adm_no, r.usr_id
        from T_ENR_EV_ENROLL e
        LEFT JOIN T_WRU_WORK_RECOMMEND_USER r on e.inm_no=r.inm_no and e.adm_no=r.adm_no and e.epe_period_seq=r.wkr_ser_no
        and e.eos_subject_code=r.eos_subject_code and e.eog_org_code=r.eog_org_code
        and r.usr_id=vUSR_RECOMMEND and r.wru_status='R' and r.wru_type='V'
        where e.epe_period_seq=arg_period_seq 
        and e.eos_subject_code=vEOS_SUBJECT_CODE and e.eog_org_code=vEOG_ORG_CODE
        )
        loop
        If r.usr_id is null THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_WRU_WORK_RECOMMEND_USER (inm_no, adm_no, wkr_ser_no, usr_id, wru_status,wru_type,wru_recommend_dt,
            eos_subject_code,eog_org_code,
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(r.inm_no, r.adm_no, arg_period_seq, vUSR_RECOMMEND, 'R','V',to_date(vFM_DT,'yyyyMMdd'),
            vEOS_SUBJECT_CODE,vEOG_ORG_CODE,
            systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
        ELSE
            update T_WRU_WORK_RECOMMEND_USER set wru_recommend_dt=to_date(vFM_DT,'yyyyMMdd'),updated_ts=systimestamp,updated_by='USR_WVT'
            where inm_no=r.inm_no and adm_no=r.adm_no and wkr_ser_no=arg_period_seq 
            and eos_subject_code=vEOS_SUBJECT_CODE and eog_org_code=vEOG_ORG_CODE
            and usr_id='USR_WVT' and wru_status='R' and wru_type='V'; 
        End if;
        end loop;



    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_006_RECOMMEND','Other record error');

    END;


END VTR_006_RECOMMEND;


PROCEDURE VTR_008_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'C',arg_period_seq,vEOS_SUBJECT_CODE,
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_STATUS='C',updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_008_SUBMIT','Other subject error');

    END;


END VTR_008_SUBMIT ;

PROCEDURE VTR_025_APPTITUDE_TEST (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vTEST_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;

vCOUNT NUMBER:=0;

vEVW_WORKSHOP_CODE T_EWS_EV_WORKSHOP.evw_workshop_code%type:=0;
--vEWS_WORKSHOP_SEQ T_EWS_EV_WORKSHOP.EWS_WORKSHOP_SEQ%TYPE:=0;
vEAT_TEST_NO t_eat_ev_apt_test.EAT_TEST_NO%TYPE:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select EVW_WORKSHOP_CODE
        into vEVW_WORKSHOP_CODE
        from T_Z_EVW_EV_WORKSHOP p
        where EVW_STATUS='A'
        and JOC_JOB_CODE=vJOC_JOB_CODE;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_025_APPTITUDE_TEST','No workshop code found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_025_APPTITUDE_TEST','Other workshop code error');

    END;

    BEGIN
        select NVL(MAX(EAT_TEST_NO),0)+1
        into vEAT_TEST_NO
        from t_eat_ev_apt_test w
        where 
        inm_no=vINM
        and adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','No workshop seq found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','Other workshop seq error');

    END;
    BEGIN    


        insert into t_eat_ev_apt_test(inm_no,adm_no,EAT_TEST_NO,ins_code,EAT_TEST_DATE_DT,
        EAT_SCORE_R,EAT_SCORE_I,EAT_SCORE_A,EAT_SCORE_S,EAT_SCORE_E,EAT_SCORE_C,EAT_RECOMM_WORKSHOP,EAT_REMARK,
        EAT_CURR_IND,EAT_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        values(vINM,vADM,vEAT_TEST_NO,vINS_CODE,to_date(vTEST_DT,'yyyyMMdd'),
        10,20,30,40,50,60,vEVW_WORKSHOP_CODE,'Fast track',
        'Y','A',
        systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_025_APPTITUDE_TEST','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_025_APPTITUDE_TEST','Other record error');

    END;


END VTR_025_APPTITUDE_TEST ;  

PROCEDURE VTR_012_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;

vCOUNT NUMBER:=0;

vEVW_WORKSHOP_CODE T_EWS_EV_WORKSHOP.evw_workshop_code%type:=0;
vEWS_WORKSHOP_SEQ T_EWS_EV_WORKSHOP.EWS_WORKSHOP_SEQ%TYPE:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select EVW_WORKSHOP_CODE
        into vEVW_WORKSHOP_CODE
        from T_Z_EVW_EV_WORKSHOP p
        where EVW_STATUS='A'
        and JOC_JOB_CODE=vJOC_JOB_CODE;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','No workshop code found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','Other workshop code error');

    END;

    BEGIN
        select NVL(MAX(EWS_WORKSHOP_SEQ),0)+1
        into vEWS_WORKSHOP_SEQ
        from T_EWS_EV_WORKSHOP w
        where 
        inm_no=vINM
        and adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','No workshop seq found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','Other workshop seq error');

    END;
    BEGIN    
        insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
        EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        values(vINM, vADM, vEWS_WORKSHOP_SEQ,'S',vINS_CODE,vEVW_WORKSHOP_CODE,
        to_date(vFM_DT,'yyyyMMdd'),'Y',vEWS_WORKSHOP_SEQ,
        systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_012_SUBMIT','Other record error');

    END;


END VTR_012_SUBMIT ; 

--obsolete
PROCEDURE VTR_013_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;

vCOUNT NUMBER:=0;

vEVW_WORKSHOP_CODE T_EWS_EV_WORKSHOP.evw_workshop_code%type:=0;
vEWS_WORKSHOP_SEQ T_EWS_EV_WORKSHOP.EWS_WORKSHOP_SEQ%TYPE:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select EVW_WORKSHOP_CODE
        into vEVW_WORKSHOP_CODE
        from T_Z_EVW_EV_WORKSHOP p
        where EVW_STATUS='A'
        and JOC_JOB_CODE=vJOC_JOB_CODE;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_SUBMIT','vJOC_JOB_CODE:'||vJOC_JOB_CODE);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_SUBMIT','Other workshop code error');

    END;

    BEGIN
        select NVL(MAX(EWS_WORKSHOP_SEQ),0)
        into vEWS_WORKSHOP_SEQ
        from T_EWS_EV_WORKSHOP w
        where 
        inm_no=vINM
        and adm_no=vADM
        and ews_curr_ind='Y'
        and ins_code=vINS_CODE
        and evw_workshop_code=vEVW_WORKSHOP_CODE
        and to_char(EWS_IN_DATE_DT,'yyyyMMdd')=vFM_DT;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_SUBMIT','No workshop seq found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_SUBMIT','Other workshop seq error');

    END;
    BEGIN    
        If vEWS_WORKSHOP_SEQ = 0 Then
            insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
            EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
            values(vINM, vADM, vEWS_WORKSHOP_SEQ,'R',vINS_CODE,vEVW_WORKSHOP_CODE,
            to_date(vFM_DT,'yyyyMMdd'),'Y',vEWS_WORKSHOP_SEQ,
            systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
        Else
            update T_EWS_EV_WORKSHOP set EWS_STATUS='R',updated_ts=systimestamp,updated_by='USR_WVT'
            where 
            inm_no=vINM
            and adm_no=vADM
            and ews_curr_ind='Y'
            and ins_code=vINS_CODE
            and evw_workshop_code=vEVW_WORKSHOP_CODE
            and to_char(EWS_IN_DATE_DT,'yyyyMMdd')=vFM_DT;
        End if;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_SUBMIT','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_SUBMIT','Other record error');

    END;


END VTR_013_SUBMIT ;


PROCEDURE VTR_013_RECOMMEND (
      --vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vUSR_RECOMMEND IN VARCHAR2
  )IS
  --vINM t_inm_inmate.inm_no%type :=0;
  --vADM t_inm_inmate.adm_no%type :=0;

vCOUNT NUMBER:=0;

vEVW_WORKSHOP_CODE T_EWS_EV_WORKSHOP.evw_workshop_code%type:=0;
vEWS_WORKSHOP_SEQ T_EWS_EV_WORKSHOP.EWS_WORKSHOP_SEQ%TYPE:=0;
BEGIN
--vINM:=GET_INMNO(vCIN);
--vADM:=GET_ADMNO(vCIN);
    BEGIN
        select EVW_WORKSHOP_CODE
        into vEVW_WORKSHOP_CODE
        from T_Z_EVW_EV_WORKSHOP p
        where EVW_STATUS='A'
        and JOC_JOB_CODE=vJOC_JOB_CODE;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','vJOC_JOB_CODE:'||vJOC_JOB_CODE);
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','Other workshop code error');

    END;

    BEGIN
        select NVL(MAX(EWS_WORKSHOP_SEQ),0)
        into vEWS_WORKSHOP_SEQ
        from T_EWS_EV_WORKSHOP w
        where 
        --inm_no=vINM
        --and adm_no=vADM
        --and ews_curr_ind='Y'
        --and 
		ins_code=vINS_CODE
        and evw_workshop_code=vEVW_WORKSHOP_CODE
        and to_char(EWS_IN_DATE_DT,'yyyyMMdd')=vFM_DT;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','No workshop seq found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','Other workshop seq error');

    END;
	/*
    BEGIN    
        If vEWS_WORKSHOP_SEQ = 0 Then
            insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
            EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
            values(vINM, vADM, vEWS_WORKSHOP_SEQ,'R',vINS_CODE,vEVW_WORKSHOP_CODE,
            to_date(vFM_DT,'yyyyMMdd'),'Y',vEWS_WORKSHOP_SEQ,
            systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
        Else
            update T_EWS_EV_WORKSHOP set EWS_STATUS='R',updated_ts=systimestamp,updated_by='USR_WVT'
            where 
            inm_no=vINM
            and adm_no=vADM
            and ews_curr_ind='Y'
            and ins_code=vINS_CODE
            and evw_workshop_code=vEVW_WORKSHOP_CODE
            and to_char(EWS_IN_DATE_DT,'yyyyMMdd')=vFM_DT;
        End if;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','Other record error');

    END;T_EWS_EV_WORKSHOP
	*/
	BEGIN
        For r in (select e.inm_no, e.adm_no, r.usr_id, e.wor_ser_no
        from T_EWS_EV_WORKSHOP e
        LEFT JOIN T_WRU_WORK_RECOMMEND_USER r on e.inm_no=r.inm_no and e.adm_no=r.adm_no and e.wor_ser_no=r.wkr_ser_no
        --and e.eos_subject_code=r.eos_subject_code and e.eog_org_code=r.eog_org_code
        and r.usr_id=vUSR_RECOMMEND and r.wru_status='R' and r.wru_type='V'
        where 
		e.EWS_WORKSHOP_SEQ=vEWS_WORKSHOP_SEQ 
        and e.EVW_WORKSHOP_CODE=vEVW_WORKSHOP_CODE and e.INS_CODE=vINS_CODE
        )
        loop
        If r.usr_id is null THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_WRU_WORK_RECOMMEND_USER (inm_no, adm_no, wkr_ser_no, usr_id, wru_status,wru_type,wru_recommend_dt,
            eos_subject_code,eog_org_code,
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(r.inm_no, r.adm_no, r.wor_ser_no, vUSR_RECOMMEND, 'R','V',to_date(vFM_DT,'yyyyMMdd'),
            vEVW_WORKSHOP_CODE,vEWS_WORKSHOP_SEQ,
            systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
        ELSE
            update T_WRU_WORK_RECOMMEND_USER set wru_recommend_dt=to_date(vFM_DT,'yyyyMMdd'),updated_ts=systimestamp,updated_by='USR_WVT'
            where inm_no=r.inm_no and adm_no=r.adm_no and wkr_ser_no=r.wor_ser_no 
            and eos_subject_code=vEVW_WORKSHOP_CODE and eog_org_code=vEWS_WORKSHOP_SEQ
            and usr_id='USR_WVT' and wru_status='R' and wru_type='V'; 
        End if;
        end loop;



    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_013_RECOMMEND','Other record error');

    END;


END VTR_013_RECOMMEND ;

PROCEDURE VTR_014_RECOMMEND_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vJOC_JOB_CODE	IN T_Z_EVW_EV_WORKSHOP.JOC_JOB_CODE%type,     
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;

vCOUNT NUMBER:=0;

vEVW_WORKSHOP_CODE T_EWS_EV_WORKSHOP.evw_workshop_code%type:=0;
vEWS_WORKSHOP_SEQ T_EWS_EV_WORKSHOP.EWS_WORKSHOP_SEQ%TYPE:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select EVW_WORKSHOP_CODE
        into vEVW_WORKSHOP_CODE
        from T_Z_EVW_EV_WORKSHOP p
        where EVW_STATUS='A'
        and JOC_JOB_CODE=vJOC_JOB_CODE;


    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_014_RECOMMEND_APPROVE','No workshop code found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_014_RECOMMEND_APPROVE','Other workshop code error');

    END;

    BEGIN
        select NVL(MAX(EWS_WORKSHOP_SEQ),0)
        into vEWS_WORKSHOP_SEQ
        from T_EWS_EV_WORKSHOP w
        where 
        inm_no=vINM
        and adm_no=vADM
        and ews_curr_ind='Y'
        and ins_code=vINS_CODE
        and evw_workshop_code=vEVW_WORKSHOP_CODE
        and to_char(EWS_IN_DATE_DT,'yyyyMMdd')=vFM_DT;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_014_RECOMMEND_APPROVE','No workshop seq found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_014_RECOMMEND_APPROVE','Other workshop seq error');

    END;
    BEGIN    
        If vEWS_WORKSHOP_SEQ = 0 Then
            insert into T_EWS_EV_WORKSHOP(inm_no, adm_no, EWS_WORKSHOP_SEQ, EWS_STATUS,ins_code,EVW_WORKSHOP_CODE,
            EWS_IN_DATE_DT,ews_curr_ind,wor_ser_no,
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
            values(vINM, vADM, vEWS_WORKSHOP_SEQ,'A',vINS_CODE,vEVW_WORKSHOP_CODE,
            to_date(vFM_DT,'yyyyMMdd'),'Y',vEWS_WORKSHOP_SEQ,
            systimestamp, 'USR_WVT', systimestamp, 'USR_WVT');
        Else
            update T_EWS_EV_WORKSHOP set EWS_STATUS='A',updated_ts=systimestamp,updated_by='USR_WVT'
            where 
            inm_no=vINM
            and adm_no=vADM
            and ews_curr_ind='Y'
            and ins_code=vINS_CODE
            and evw_workshop_code=vEVW_WORKSHOP_CODE
            and to_char(EWS_IN_DATE_DT,'yyyyMMdd')=vFM_DT;
        End if;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_014_RECOMMEND_APPROVE','No record found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_014_RECOMMEND_APPROVE','Other record error');

    END;


END VTR_014_RECOMMEND_APPROVE ;

PROCEDURE VTR_015_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','No period found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','Other period error');

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','No exam found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','Other exam error');

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'H',arg_period_seq,vEOS_SUBJECT_CODE,
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_STATUS='H',updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','No subject found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_015_SUBMIT','Other subject error');

    END;


END VTR_015_SUBMIT ;

PROCEDURE VTR_016_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
arg_period_seq T_EPE_EV_ORG_PERIOD.EPE_PERIOD_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
vERR number;
vERM VARCHAR2(100);
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
vEOG_ORG_CODE:=GET_EOG_ORG_CODE(vEOG_ORG_NAME);
vEOS_SUBJECT_CODE:=GET_EOS_SUBJECT_CODE(vEOG_ORG_CODE,vEOS_DESC);
    BEGIN
        select NVL(MAX(EPE_PERIOD_SEQ),0)
        into arg_period_seq
        from T_EPE_EV_ORG_PERIOD p
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and to_char(p.epe_date_fm_dt,'yyyyMMdd')<=vFM_DT;
        --and to_char(p.epe_date_to_dt,'yyyyMMdd')>=vFM_DT;

        --if arg_period_seq = 0 THEN
        --    select seq_evp_id.nextval into vEVP_ID from dual;
        --    insert into T_Z_EVP_EV_PERIOD(evp_period_desc,evp_period_start_date,evp_period_end_date,
        --    remark,evp_id,status,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY)
        --    values(substr(vFM_DT,1,4)||'-'||substr(vFM_DT,5,2),to_date(substr(vFM_DT,1,6)||'01','yyyyMMdd'),to_date(substr(vFM_DT,1,6)||'28','yyyyMMdd'),
        --    'Registration Period : '||substr(vFM_DT,1,6)||'01'||' - '||substr(vFM_DT,1,6)||'28',vEVP_ID,'A',systimestamp,'USR_EDU',systimestamp,'USR_EDU');
        --END IF;

	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_016_SUBMIT - No period','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_016_SUBMIT - Other period','vERM:'||vERM,vERR);

    END;

    BEGIN
        select NVL(count(*),0)
        into vCOUNT
        from T_ENR_EV_ENROLL m
        join T_EPE_EV_ORG_PERIOD p on p.eog_org_code=m.eog_org_code and p.eos_subject_code=m.eos_subject_code 
        and p.epe_period_seq=m.epe_period_seq and p.ins_code=m.ins_code
        join t_z_eog_ev_org o on p.eog_org_code=o.eog_org_code
        join t_z_eos_ev_org_subject s on p.eog_org_code=s.eog_org_code and p.eos_subject_code=s.eos_subject_code
        where o.eog_org_name=vEOG_ORG_NAME
        and s.eos_desc=vEOS_DESC
        and p.ins_code=vINS_CODE
        and p.epe_period_seq=arg_period_seq
        and m.inm_no=vINM
        and m.adm_no=vADM;

	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_016_SUBMIT - No exam','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_016_SUBMIT - Other exam','vERM:'||vERM,vERR); 

    END;
	/*
    BEGIN    
        select eog_org_code into vEOG_ORG_CODE
        from t_z_eog_ev_org where eog_org_name=vEOG_ORG_NAME and eog_status='A';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_016_SUBMIT','No org found');
        WHEN OTHERS THEN
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string)values (systimestamp, 'VTR_016_SUBMIT','Other org error');

    END;    */
    BEGIN
	/*
        select eos_subject_code, eos_subject_type into vEOS_SUBJECT_CODE, vEOS_SUBJECT_TYPE
        from t_z_eos_ev_org_subject where eog_org_code=vEOG_ORG_CODE and eos_desc=vEOS_DESC;*/
        If vCOUNT = 0 THEN

            --select NVL(max(EVE_EXAM_SEQ),0)+1
            --into vEVE_EXAM_SEQ
            --from T_EVE_EV_EXAM 
            --where inm_no=vINM and adm_no=vADM;

            insert into T_ENR_EV_ENROLL (inm_no, adm_no, eog_org_code, bas_cin, enr_remark, enr_withdraw, enr_enroll_flag,
            ENR_FAIL_REASON_1,ENR_FAIL_REASON_2,ENR_FAIL_REASON_3,ENR_FAIL_REASON_4,ENR_FAIL_REASON_5,ENR_FAIL_REASON_6,ENR_FAIL_REASON_7,
            ins_code,ENR_APPLICATION_DT,enr_status,epe_period_seq,eos_subject_code,
            pics_type,CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(vINM, vADM, vEOG_ORG_CODE, TO_NUMBER(vCIN), 'Fast track', 'N', 'E',
            'N','N','N','N','N','N','N',
            vINS_CODE,to_date(vFM_DT,'yyyyMMdd'),'A',arg_period_seq,vEOS_SUBJECT_CODE,
            'C',systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');
        ELSE
            update T_ENR_EV_ENROLL set ENR_STATUS='A',updated_ts=systimestamp,updated_by='USR_VTR'
            where epe_period_seq=arg_period_seq and eos_subject_code=vEOS_SUBJECT_CODE
            and eog_org_code=vEOG_ORG_CODE and ins_code=vINS_CODE
            and inm_no=vINM and adm_no=vADM; 
        END IF;

	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_016_SUBMIT - No record','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_016_SUBMIT - Other record','vERM:'||vERM,vERR);  
    END;


END VTR_016_SUBMIT ;

PROCEDURE VTR_022_ENROLL (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS

BEGIN


PKG_FAST_TRACK.T_EVE_INSERT(vCIN,vEOG_ORG_NAME,vEOS_DESC,vINS_CODE,vFM_DT,'V');

END VTR_022_ENROLL ;  

PROCEDURE VTR_023_PERFORMANCE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vEOG_ORG_NAME	IN t_z_eog_ev_org.eog_org_name%type,     
      vEOS_DESC IN t_z_eos_ev_org_subject.eos_desc%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2
  )IS

BEGIN


PKG_FAST_TRACK.T_EVE_UPDATE(vCIN,vEOG_ORG_NAME,vEOS_DESC,vINS_CODE,vFM_DT,'V');

END VTR_023_PERFORMANCE ;


PROCEDURE VTR_086_EMPLOYMENT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
	  vTO_DT IN VARCHAR2
  )IS
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
vEMPL_SEQ t_emp_ev_employment.EMPL_SEQ%TYPE:=0;  
vCOUNT NUMBER:=0;
vEOG_ORG_CODE t_z_eog_ev_org.eog_org_code%TYPE:=0;
vEOS_SUBJECT_CODE t_z_eos_ev_org_subject.eos_subject_code%TYPE:=0;
vEOS_SUBJECT_TYPE t_z_eos_ev_org_subject.eos_subject_type%TYPE:='';
vEVE_EXAM_SEQ T_EVE_EV_EXAM.eve_exam_seq%type:=0;
vERR Number;
vERM VARCHAR2(100);
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
	begin
		select nvl(max(empl_seq),0) + 1 into vEMPL_SEQ from t_emp_ev_employment;

	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_086_EMPLOYMENT - No seq','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_086_EMPLOYMENT - Other seq','vERM:'||vERM,vERR);   
	END;

	begin


	Insert into t_emp_ev_employment (inm_no, adm_no, empl_seq, status, ins_code,
	occu_code,comp_name,empl_name,self_empl,start_dt,end_dt,salary,address,
	CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
	)values(vINM,vADM,vEMPL_SEQ,'A',vINS_CODE,
	3,'Janitor Company Limited','Chan Tai Man, Victor','N',to_date(vFM_DT,'yyyyMMdd'),to_date(vTO_DT,'yyyyMMdd'),8000,'Long Address for testing Purposes, Long Address for testing Purposes, Long Address for testing Purposes, Long Address for testing Purposes, Long Address for testing Purposes',
	systimestamp, 'USR_VTR', systimestamp, 'USR_VTR');

	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_086_EMPLOYMENT - No record','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_086_EMPLOYMENT - Other record','vERM:'||vERM,vERR);    
	END;

END VTR_086_EMPLOYMENT ;

FUNCTION T_SUC_INSERT (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_SUC_SUP_CASE.SUC_CASE_NO%TYPE
  IS
  vERR Number;
vERM VARCHAR2(100);
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
  vBAS_ADM t_inm_inmate.adm_no%type :=0;
  vNAME t_bas_inmate_basic.bas_name%type;
  vAGE NUMBER;
  vSUC_CASE_NO t_suc_sup_case.SUC_CASE_NO%type :=0;
  vCLC_CODE t_inm_inmate.clc_code%type :=0;
  vINM_CON_ADM_DT t_inm_inmate.inm_con_adm_da_dt%type;
  vINM_CON_DIS_DA_DT t_inm_inmate.INM_CON_DIS_DA_DT%type;
  vPGM_CODE t_suc_sup_case.pgm_code%type;
  
BEGIN

vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
select inm_no, bas_con_adm_no, bas_name, Round((sysdate - to_date(bas_dob,'yyyyMMdd'))/365.24)
into vINM, vBAS_ADM, vNAME, vAGE
from t_bas_inmate_basic where bas_cin=vCIN;
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SUC_INSERT - bas','vAGE',vAGE);
--commit;
    EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - No bas','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - Other bas','vERM:'||vERM,vERR);    
    END;
    BEGIN
select clc_code, inm_con_adm_da_dt, INM_CON_DIS_DA_DT into vCLC_CODE,vINM_CON_ADM_DT,vINM_CON_DIS_DA_DT from t_inm_inmate where inm_no=vINM and inm_curr_ind='Y';
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string) values (systimestamp, 'T_SUC_INSERT - inm','vCLC_CODE:'||vCLC_CODE);
--commit;
    EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - No inm','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - Other inm','vERM:'||vERM,vERR);    
    END;
    BEGIN
select max(suc_case_no)+1 into vSUC_CASE_NO from  t_suc_sup_case;
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SUC_INSERT - case','vSUC_CASE_NO',vSUC_CASE_NO);
--commit;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - No case','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - Other case','vERM:'||vERM,vERR);    
    END;
    
        BEGIN
select PGM_CODE into vPGM_CODE from  t_z_age_age_code where age_code=case when vAGE < 18 then 'YA' else 'AD' end and rownum<2;
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SUC_INSERT - case','vSUC_CASE_NO',vSUC_CASE_NO);
--commit;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - No age','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - Other age','vERM:'||vERM,vERR);    
    END;
    BEGIN
    /*
insert into t_suc_sup_case(suc_case_no,inm_no,adm_no,bas_con_adm_no,inm_cin,team_inst_code,clc_code,suc_status,
pgm_code,age_code,
suc_name,in_out,inm_con_adm_dt,INM_CON_DIS_DA_DT)
values(vSUC_CASE_NO,vINM,vADM,vBAS_ADM,vCIN,vINS_CODE,vCLC_CODE,'C',
case vCLC_CODE when 'TC' then 'TC' when 'RTC' then 'TC'
when 'DC' then 'DC' when 'YADC' then 'DC' when 'RDC' then 'DC'
when 'YP' then 'YP' when 'YDA' then 'YP' when 'P' then 'YP'
when 'RDA' then 'DATC'
else 'PRSS' end,
case when vAGE < 18 then 'YA' else 'AD' end,
vNAME,'O',vINM_CON_ADM_DT,vINM_CON_DIS_DA_DT);
*/
insert into t_suc_sup_case(suc_case_no,inm_no,adm_no,bas_con_adm_no,inm_cin,team_inst_code,clc_code,suc_status,
pgm_code,age_code,
suc_name,in_out,inm_con_adm_dt,INM_CON_DIS_DA_DT)
values(vSUC_CASE_NO,vINM,vADM,vBAS_ADM,vCIN,vINS_CODE,vCLC_CODE,'C',
vPGM_CODE,
case when vAGE < 18 then 'YA' else 'AD' end,
vNAME,'O',vINM_CON_ADM_DT,vINM_CON_DIS_DA_DT);
--commit;

	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - No record','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SUC_INSERT - Other record','vERM:'||vERM,vERR);    

    END;
commit;
Return vSUC_CASE_NO;
END T_SUC_INSERT; 


PROCEDURE T_SEM_INSERT (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
  vERR Number;
vERM VARCHAR2(100);
  vINM t_inm_inmate.inm_no%type :=0;
  vADM t_inm_inmate.adm_no%type :=0;
  vSUC_CASE_NO t_suc_sup_case.SUC_CASE_NO%type :=0;
  vSEM_EMPL_SEQ t_sem_SUP_EMPLOYMENT.SEM_EMPL_SEQ%Type;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
select nvl(max(suc_case_no),0) into vSUC_CASE_NO from  t_suc_sup_case where inm_no=vINM and adm_no=vADM;

--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SEM_INSERT - case','vSUC_CASE_NO',vSUC_CASE_NO);
--commit;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SEM_INSERT - No case','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SEM_INSERT - Other case','vERM:'||vERM,vERR);    
    END;
    If vSUC_CASE_NO = 0 then
        vSUC_CASE_NO:=T_SUC_INSERT(vCIN, vINS_CODE);
        --Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SEM_INSERT - suc','vSUC_CASE_NO',vSUC_CASE_NO);
    END if;
    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SEM_INSERT - suc','vSUC_CASE_NO',vSUC_CASE_NO);
    
    BEGIN
select nvl(max(SEM_EMPL_SEQ),0)+1 into vSEM_EMPL_SEQ from  t_sem_sup_employment where inm_no=vINM and adm_no=vADM;
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value) values (systimestamp, 'T_SEM_INSERT - sem','vSEM_EMPL_SEQ',vSEM_EMPL_SEQ);
--commit;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SEM_INSERT - No sem','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SEM_INSERT - Other sem','vERM:'||vERM,vERR);    
    END;  
    
    BEGIN
insert into t_sem_sup_employment(sem_id,suc_case_no,SEM_EMPL_SEQ,inm_no,adm_no,team_inst_code,sem_status,
ROC_CODE,RER_REGI_CODE,ARA_AREA_CODE,DIST_CODE,SBD_SUBDIST_CODE,COMP_NAME,START_DT,salary,address
)values(seq_sem_id.nextval,vSUC_CASE_NO,vSEM_EMPL_SEQ,vINM,vADM,vINS_CODE,'A',
18,'L','HK',14,'CTL','ABC COMPANY LIMITED',to_date(vFM_DT,'yyyyMMdd'),9000,'告士打道72號'
);
commit;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SEM_INSERT - No record','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'T_SEM_INSERT - Other record','vERM:'||vERM,vERR);    
    END;  
END T_SEM_INSERT;

PROCEDURE VTR_001_4_ADD (
      vJOB_CODE IN T_WOR_WORK.WOP_PA_JOB_CODE%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )
  IS
vCount Number;  
vERR Number;
vERM VARCHAR2(100);
vEVW_WORKSHOP_CODE T_Z_EVW_EV_WORKSHOP.EVW_WORKSHOP_CODE%TYPE;
BEGIN
vEVW_WORKSHOP_CODE:=GET_EWS_WORKSHOP_CODE( vJOB_CODE);
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_001_4_ADD','vEVW_WORKSHOP_CODE',vEVW_WORKSHOP_CODE);


    BEGIN
    
        select count(*) into vCount from T_Z_EWI_EV_WORKSHOP_INST where EVW_WORKSHOP_CODE=vEVW_WORKSHOP_CODE and ins_code=vINS_CODE;
    EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_001_4_ADD - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_001_4_ADD - Other count','vERM:'||vERM,vERR);    

    END;

    If vCount = 0 then
        insert into T_Z_EWI_EV_WORKSHOP_INST(EVW_WORKSHOP_CODE,INS_CODE,EWI_REPORT_IND,EWI_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vEVW_WORKSHOP_CODE,vINS_CODE,'Y','A',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND');
    
    Else
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_001_4_ADD','vCount',vCount);
    
    End if;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_001_4_ADD - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'VTR_001_4_ADD - Other record','vERM:'||vERM,vERR);    

END VTR_001_4_ADD;  

PROCEDURE IND_003_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;  
vERR Number;
vERM VARCHAR2(100);  
vINM t_inm_inmate.inm_no%type :=0;
vADM t_inm_inmate.adm_no%type :=0;
vWKR_SER_NO t_wkr_work_request.WKR_SER_NO%TYPE;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select count(*) into vCount from t_wkr_work_request where inm_no=vINM and adm_no=vADM and 
    trunc(wkr_request_date_dt)=to_date(vFM_DT,'yyyyMMdd');


    EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_003_SUBMIT - No count','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_003_SUBMIT - Other count','vERM:'||vERM,vERR);    
    

    END;
    

    
    if vCount = 0 then
    
       BEGIN
            select NVL(MAX(WKR_SER_NO),0)+1
            into vWKR_SER_NO
            from t_wkr_work_request w
            where 
            inm_no=vINM
            and adm_no=vADM;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                        vERR:=SQLCODE;
                    vERM:=SUBSTR(SQLERRM, 1,100);
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_003_SUBMIT - No count','vERM:'||vERM,vERR);    
    
            WHEN OTHERS THEN
                        vERR:=SQLCODE;
                    vERM:=SUBSTR(SQLERRM, 1,100);
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_003_SUBMIT - Other count','vERM:'||vERM,vERR);    
        

        END;
    
    
    
        insert into t_wkr_work_request(inm_no,adm_no,WKR_SER_NO,ins_code,
        wol_locat,wop_pa_job_code,jwp_code,ear_grad_code,p_ear_grad_code,wkr_scenario,
        wkr_dat_assig_dt,wkr_request_date_dt,wkr_assign_rec_type,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vINM,vADM,vWKR_SER_NO,vINS_CODE,
        'B1L','ME','004',2,2,3,
        to_date(vFM_DT,'yyyyMMdd'),to_date(vFM_DT,'yyyyMMdd'),'S',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND');
    else
        update t_wkr_work_request set wkr_assign_rec_type='S', updated_ts=systimestamp, updated_by='USR_IND'
        where inm_no=vINM and adm_no=vADM and trunc(wkr_request_date_dt)=to_date(vFM_DT,'yyyyMMdd');
    end if;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_003_SUBMIT - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_003_SUBMIT - Other record','vERM:'||vERM,vERR);    

END IND_003_SUBMIT; 

PROCEDURE IND_004_SAVE (
      vUSR_ID IN t_wvt_wvtab.usr_id%type,
      vWVT_MEMBER_TYPE IN varchar2,
      vINS_CODE IN t_wvt_wvtab.ins_code%type
  )
  IS
vCount Number;  
vERR Number;
vERM VARCHAR2(100);  
vID Number;
BEGIN

If vWVT_MEMBER_TYPE='M' or vWVT_MEMBER_TYPE='C' Then

    select count(*) into vCount from t_wvt_wvtab where ins_code=vINS_CODE and wvt_member_type=vWVT_MEMBER_TYPE
    and usr_id=vUSR_ID;

    If vCount = 0 Then
        insert into t_wvt_wvtab(wvt_seq,ins_code,wvt_member_type,usr_id,
        created_ts, created_by, updated_ts, updated_by
        )
        values(SEQ_WVT_WVTAB_ID.nextval,vINS_CODE,vWVT_MEMBER_TYPE,vUSR_ID,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
    Else
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_004_SAVE','vCount',vCount);    

    End if;
Elsif vWVT_MEMBER_TYPE='2'  Then
    
    select count(*) into vCount from t_wvt_wvtab where ins_code=vINS_CODE and wvt_member_type='M'
    and usr_id_sec=vUSR_ID;
    If vCount = 0 Then

        select nvl(min(wvt_seq),0) into vID from t_wvt_wvtab where ins_code=vINS_CODE and wvt_member_type='M'
        and usr_id_sec is null; 
        
        If vID = 0 then
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_004_SAVE','vCount',vCount);    

        Else
            update t_wvt_wvtab set usr_id_sec=vUSR_ID where wvt_seq=vID;
        
        End if;
    
    Else
        Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_004_SAVE','vCount',vCount);    

    End if;

End if;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_004_SAVE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_004_SAVE - Other record','vERM:'||vERM,vERR);    

END IND_004_SAVE;   

PROCEDURE IND_005_RECOMMEND (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;  
vERR Number;
vERM VARCHAR2(100);  
vINM t_inm_inmate.inm_no%type :=0;
vADM t_inm_inmate.adm_no%type :=0;
vWKR_SER_NO t_wkr_work_request.WKR_SER_NO%TYPE;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select count(*) into vCount from t_wkr_work_request where inm_no=vINM and adm_no=vADM and 
    trunc(wkr_request_date_dt)=to_date(vFM_DT,'yyyyMMdd');


    EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_005_RECOMMEND - No count','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_005_RECOMMEND - Other count','vERM:'||vERM,vERR);    
    

    END;
    

    
    if vCount = 0 then
    
       BEGIN
            select NVL(MAX(WKR_SER_NO),0)+1
            into vWKR_SER_NO
            from t_wkr_work_request w
            where 
            inm_no=vINM
            and adm_no=vADM;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                        vERR:=SQLCODE;
                    vERM:=SUBSTR(SQLERRM, 1,100);
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_005_RECOMMEND - No count','vERM:'||vERM,vERR);    
    
            WHEN OTHERS THEN
                        vERR:=SQLCODE;
                    vERM:=SUBSTR(SQLERRM, 1,100);
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_005_RECOMMEND - Other count','vERM:'||vERM,vERR);    
        

        END;
    
    
    
        insert into t_wkr_work_request(inm_no,adm_no,WKR_SER_NO,ins_code,
        wol_locat,wop_pa_job_code,jwp_code,ear_grad_code,p_ear_grad_code,wkr_scenario,
        wkr_dat_assig_dt,wkr_request_date_dt,wkr_assign_rec_type,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vINM,vADM,vWKR_SER_NO,vINS_CODE,
        'BAL','MH','002',2,2,3,
        to_date(vFM_DT,'yyyyMMdd'),to_date(vFM_DT,'yyyyMMdd'),'R',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND');
    else
        update t_wkr_work_request set wkr_assign_rec_type='R', updated_ts=systimestamp, updated_by='USR_IND'
        where inm_no=vINM and adm_no=vADM and trunc(wkr_request_date_dt)=to_date(vFM_DT,'yyyyMMdd');
    end if;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_005_RECOMMEND - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_005_RECOMMEND - Other record','vERM:'||vERM,vERR);    

END IND_005_RECOMMEND; 

PROCEDURE IND_006_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type
      ,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;  
vERR Number;
vERM VARCHAR2(100);  
vINM t_inm_inmate.inm_no%type :=0;
vADM t_inm_inmate.adm_no%type :=0;
vWKR_SER_NO t_wkr_work_request.WKR_SER_NO%TYPE;
BEGIN
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    BEGIN
        select count(*) into vCount from t_wkr_work_request where inm_no=vINM and adm_no=vADM and 
    trunc(wkr_request_date_dt)=to_date(vFM_DT,'yyyyMMdd');


    EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_006_APPROVE - No count','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_006_APPROVE - Other count','vERM:'||vERM,vERR);    
    

    END;
    

    
    if vCount = 0 then
    
       BEGIN
            select NVL(MAX(WKR_SER_NO),0)+1
            into vWKR_SER_NO
            from t_wkr_work_request w
            where 
            inm_no=vINM
            and adm_no=vADM;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                        vERR:=SQLCODE;
                    vERM:=SUBSTR(SQLERRM, 1,100);
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_006_APPROVE - No count','vERM:'||vERM,vERR);    
    
            WHEN OTHERS THEN
                        vERR:=SQLCODE;
                    vERM:=SUBSTR(SQLERRM, 1,100);
                    Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_006_APPROVE - Other count','vERM:'||vERM,vERR);    
        

        END;
    
    
    
        insert into t_wkr_work_request(inm_no,adm_no,WKR_SER_NO,ins_code,
        wol_locat,wop_pa_job_code,jwp_code,ear_grad_code,p_ear_grad_code,wkr_scenario,
        wkr_dat_assig_dt,wkr_request_date_dt,wkr_assign_rec_type,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vINM,vADM,vWKR_SER_NO,vINS_CODE,
        'BAL','EM','001',2,2,3,
        to_date(vFM_DT,'yyyyMMdd'),to_date(vFM_DT,'yyyyMMdd'),'A',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND');
    else
        update t_wkr_work_request set wkr_assign_rec_type='A', updated_ts=systimestamp, updated_by='USR_IND'
        where inm_no=vINM and adm_no=vADM and trunc(wkr_request_date_dt)=to_date(vFM_DT,'yyyyMMdd');
    end if;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_006_APPROVE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_006_APPROVE - Other record','vERM:'||vERM,vERR);    

END IND_006_APPROVE; 

PROCEDURE IND_018_SAVE (
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vWOL_LOC_DESC IN T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )
  IS
vCount Number;  
vERR Number;
vERM VARCHAR2(100);  
BEGIN
    BEGIN
        select count(*) into vCount from T_Z_WOL_WORK_LOC_CODE where WOL_LOCAT=vWOL_LOCAT and INS_CODE=vINS_CODE;


    EXCEPTION
		WHEN NO_DATA_FOUND THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_018_SAVE - No count','vERM:'||vERM,vERR);    

		WHEN OTHERS THEN
					vERR:=SQLCODE;
				vERM:=SUBSTR(SQLERRM, 1,100);
				Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_018_SAVE - Other count','vERM:'||vERM,vERR);    
    

    END;
    
    if vCount = 0 then
    
        insert into T_Z_WOL_WORK_LOC_CODE(INS_CODE,WOL_LOCAT,WOL_LOC_DESC,REP_IND,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vINS_CODE,vWOL_LOCAT,vWOL_LOC_DESC,'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND');
    end if;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_018_SAVE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_018_SAVE - Other record','vERM:'||vERM,vERR);    
    
END IND_018_SAVE; 

FUNCTION GET_WOL_LOC_DESC (
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type
  
  IS
vERR Number;
vERM VARCHAR2(100);  
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
BEGIN
    select WOL_LOC_DESC into vWOL_LOC_DESC from T_Z_WOL_WORK_LOC_CODE where ins_code=vINS_CODE and WOL_LOCAT=vWOL_LOCAT;
    return vWOL_LOC_DESC;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOL_LOC_DESC - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOL_LOC_DESC - Other record','vERM:'||vERM,vERR);    
    
END GET_WOL_LOC_DESC;

FUNCTION GET_WOC_CODE (
      vWOL_LOC_DESC IN T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_Z_WOC_WORKSHOP_CODE.WOC_CODE%type
  IS
vERR Number;
vERM VARCHAR2(100);  
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%type;  
BEGIN
select WOC_CODE into vWOC_CODE from T_Z_WOC_WORKSHOP_CODE where ins_code=vINS_CODE and WOC_DESC=vWOL_LOC_DESC;
    return vWOC_CODE;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOC_CODE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOC_CODE - Other record','vERM:'||vERM,vERR);    

END GET_WOC_CODE;

FUNCTION GET_WOCCODE (
      vWOL_LOCAT IN T_Z_WOC_WORKSHOP_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return T_Z_WOC_WORKSHOP_CODE.WOC_CODE%type
  IS
vERR Number;
vERM VARCHAR2(100);  
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%type;  
BEGIN
select WOC_CODE into vWOC_CODE from T_Z_WOC_WORKSHOP_CODE where ins_code=vINS_CODE and WOL_LOCAT=vWOL_LOCAT and WOC_STATUS='A';
    return vWOC_CODE;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOC_CODE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOC_CODE - Other record','vERM:'||vERM,vERR);    

END GET_WOCCODE;

FUNCTION GET_WOP_PA_JOB_CODE (
      vWOL_LOCAT IN T_Z_WOC_WORKSHOP_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type
  )Return t_z_tjm_trade_job_mapping.WOP_PA_JOB_CODE%type
    IS
vERR Number;
vERM VARCHAR2(100);  
vWOP_PA_JOB_CODE t_z_tjm_trade_job_mapping.WOP_PA_JOB_CODE%type;  
BEGIN
select b.WOP_PA_JOB_CODE into vWOP_PA_JOB_CODE 
from T_Z_WOC_WORKSHOP_CODE a
JOIN t_z_tjm_trade_job_mapping b on a.woc_trade = b.trade_code
where a.ins_code=vINS_CODE and a.WOL_LOCAT=vWOL_LOCAT and a.WOC_STATUS='A';
    return vWOP_PA_JOB_CODE;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOP_PA_JOB_CODE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'GET_WOP_PA_JOB_CODE - Other record','vERM:'||vERM,vERR);    

END GET_WOP_PA_JOB_CODE;  

PROCEDURE IND_021_REQUEST (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_IOW_ID Number;
vIOW_ID T_IOW_IND_OT_WORK.IOW_ID%TYPE;
vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_iow_id.nextval into vIOW_ID from dual;
        BEGIN
        insert into T_IOW_IND_OT_WORK(IOW_ID,INS_CODE,EVW_WORKSHOP_CODE,IOW_STATUS,
        IOW_NUM_INMATE_REQUIRED,IOW_STAFF_ON_DUTY,IOW_RANK,IOW_JUSTIFICATION,IOW_OT_DATE_DT,IOW_REQUEST_DT,IOW_REQUEST_BY,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vIOW_ID,vINS_CODE,vWOC_CODE,'P',
        10,'Chan Tai Man','OFFR','It is a test',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),'USR_A',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - No record IOW','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - Other record IOW','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vIOD_ID,vIOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - Other record IOD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(iow_id) into vMAX_IOW_ID from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
        
        update T_IOW_IND_OT_WORK set IOW_STATUS='P',IOW_REQUEST_BY='USR_A',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where IOW_ID=vMAX_IOW_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vMAX_IOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - Other record IOD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_021_REQUEST - Other record','vERM:'||vERM,vERR);    

END  IND_021_REQUEST; 

PROCEDURE IND_022_RECOMMEND (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_IOW_ID Number;
vIOW_ID T_IOW_IND_OT_WORK.IOW_ID%TYPE;
vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_iow_id.nextval into vIOW_ID from dual;
        BEGIN
        insert into T_IOW_IND_OT_WORK(IOW_ID,INS_CODE,EVW_WORKSHOP_CODE,IOW_STATUS,
        IOW_NUM_INMATE_REQUIRED,IOW_STAFF_ON_DUTY,IOW_RANK,IOW_JUSTIFICATION,IOW_OT_DATE_DT,IOW_REQUEST_DT,IOW_REQUEST_BY,IOW_VETTING_REMARK,IOW_VETTING_DT,IOW_VETTING_BY,IOW_RECOMMENDATION,IOW_RECOMM_DT,IOW_RECOMM_BY,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vIOW_ID,vINS_CODE,vWOC_CODE,'B',
        10,'Chan Tai Man','OFFR','It is a test',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),'USR_A','Test Vetting',TO_DATE(vFM_DT,'yyyyMMdd'),'USR_B','Test Recommendation',TO_DATE(vFM_DT,'yyyyMMdd'),'USR_C',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - No record IOW','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - Other record IOW','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vIOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - Other record IOD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(iow_id) into vMAX_IOW_ID from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
        
        update T_IOW_IND_OT_WORK set IOW_STATUS='B',IOW_REQUEST_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_REQUEST_BY='USR_A',IOW_VETTING_REMARK='Test Vetting',IOW_VETTING_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_VETTING_BY='USR_B',IOW_RECOMMENDATION='Test Recommendation',IOW_RECOMM_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_RECOMM_BY='USR_C',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where IOW_ID=vMAX_IOW_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vMAX_IOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - Other record IOD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_RECOMMEND - Other record','vERM:'||vERM,vERR);    

END  IND_022_RECOMMEND; 


PROCEDURE IND_022_2_VETTING (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_IOW_ID Number;
vIOW_ID T_IOW_IND_OT_WORK.IOW_ID%TYPE;
vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_iow_id.nextval into vIOW_ID from dual;
        BEGIN
        insert into T_IOW_IND_OT_WORK(IOW_ID,INS_CODE,EVW_WORKSHOP_CODE,IOW_STATUS,
        IOW_NUM_INMATE_REQUIRED,IOW_STAFF_ON_DUTY,IOW_RANK,IOW_JUSTIFICATION,IOW_OT_DATE_DT,IOW_REQUEST_DT,IOW_REQUEST_BY,IOW_VETTING_REMARK,IOW_VETTING_DT,IOW_VETTING_BY,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vIOW_ID,vINS_CODE,vWOC_CODE,'B',
        10,'Chan Tai Man','OFFR','It is a test',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),'USR_A','Test Vetting',TO_DATE(vFM_DT,'yyyyMMdd'),'USR_B',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - No record IOW','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - Other record IOW','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vIOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - Other record IOD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(iow_id) into vMAX_IOW_ID from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
        
        update T_IOW_IND_OT_WORK set IOW_STATUS='B',IOW_REQUEST_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_REQUEST_BY='USR_A',IOW_VETTING_REMARK='Test Vetting',IOW_VETTING_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_VETTING_BY='USR_B',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where IOW_ID=vMAX_IOW_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vMAX_IOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - Other record IOD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_022_2_VETTING - Other record','vERM:'||vERM,vERR);    

END  IND_022_2_VETTING; 

PROCEDURE IND_023_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_IOW_ID Number;
vIOW_ID T_IOW_IND_OT_WORK.IOW_ID%TYPE;
vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_iow_id.nextval into vIOW_ID from dual;
        BEGIN
        insert into T_IOW_IND_OT_WORK(IOW_ID,INS_CODE,EVW_WORKSHOP_CODE,IOW_STATUS,
        IOW_NUM_INMATE_REQUIRED,IOW_STAFF_ON_DUTY,IOW_RANK,IOW_JUSTIFICATION,IOW_OT_DATE_DT,IOW_REQUEST_DT,IOW_REQUEST_BY,IOW_VETTING_REMARK,IOW_VETTING_DT,IOW_VETTING_BY,IOW_RECOMMENDATION,IOW_RECOMM_DT,IOW_RECOMM_BY,IOW_REMARK,IOW_APPROVAL_DT,IOW_APPROVED_BY,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vIOW_ID,vINS_CODE,vWOC_CODE,'A',
        10,'Chan Tai Man','OFFR','It is a test',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),'USR_A','Test Vetting',TO_DATE(vFM_DT,'yyyyMMdd'),'USR_B','Test Recommendation',TO_DATE(vFM_DT,'yyyyMMdd'),'USR_C','Test Approve',TO_DATE(vFM_DT,'yyyyMMdd'),'USR_D',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - No record IOW','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - Other record IOW','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vIOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - Other record IOD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(iow_id) into vMAX_IOW_ID from T_IOW_IND_OT_WORK where ins_code=vINS_CODE and evw_workshop_code=vWOC_CODE and iow_OT_date_dt >= to_date(vFM_DT,'yyyyMMdd');
        
        update T_IOW_IND_OT_WORK set IOW_STATUS='A',IOW_REQUEST_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_REQUEST_BY='USR_A',IOW_VETTING_REMARK='Test Vetting',IOW_VETTING_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_VETTING_BY='USR_B',IOW_RECOMMENDATION='Test Recommendation',IOW_RECOMM_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_RECOMM_BY='USR_C',IOW_REMARK='Test Approve',IOW_APPROVAL_DT=to_date(vFM_DT,'yyyyMMdd'),IOW_APPROVED_BY='USR_D',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where IOW_ID=vMAX_IOW_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN
        insert into T_IOD_IND_OT_WORK_DETAIL(IOD_ID,IOW_ID,INM_NO,ADM_NO,IOD_STATUS,
        CAD_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,IOD_ATTEND_FLAG,IOD_OT_WORK_EARNING,IOD_OT_DUR_FROM_DT,IOD_OT_DUR_TO_DT,IOD_CONSENT,--IOD_CAL_STATUS,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_iod_id.nextval,vMAX_IOW_ID,vINM,vADM,'A',
        'C','DS',1,'Y',10.1,TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+180,'Y',--'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - No record IOD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - Other record IOD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_023_APPROVE - Other record','vERM:'||vERM,vERR);    

END  IND_023_APPROVE; 

PROCEDURE IND_030_SUBMIT (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_WPH_ID Number;
vWPH_ID T_WPH_WORK_PERFORMANCE_HEADER.WPH_ID%TYPE;
--vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_WPH_WORK_PERFORMANCE_HEADER where ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_wph_id.nextval into vWPH_ID from dual;
        BEGIN
        insert into T_WPH_WORK_PERFORMANCE_HEADER(WPH_ID,INS_CODE,WOC_CODE,WPH_STATUS,
        WPH_ASS_DT,WPH_ASS_PERIOD_FR_DT,WPH_ASS_PERIOD_TO_DT,WOL_LOCAT,WPH_SUBMIT_USER_ID,WPH_SUBMIT_USER_RANK,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vWPH_ID,vINS_CODE,vWOC_CODE,'P',
        TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+30,vWOL_LOCAT,'USR_A','OFFR',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - No record WPH','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - Other record WPH','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into t_wpd_work_performance_detail(WPD_ID,WPH_ID,INM_NO,ADM_NO,WPD_STATUS,WOR_DAT_ASSIG_DT,WPD_EFFECTIVE_DATE_DT,
        EAR_GRAD_CODE,WPD_PER_ATT,WPD_PER_SKI,WPD_PER_DIS,WPD_PER_QUA,WPD_PER_SAF,WPD_OA_PER,WPD_CH_ASSIG_FLAG,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_wpd_id.nextval,vWPH_ID,vINM,vADM,'P',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),
        'DS',1,1,1,1,1,1,2,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - No record WPD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - Other record WPD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(wph_id) into vMAX_WPH_ID from T_WPH_WORK_PERFORMANCE_HEADER where  ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
        
        update T_WPH_WORK_PERFORMANCE_HEADER set WPH_STATUS='P',WPH_SUBMIT_USER_ID='USR_A',WPH_SUBMIT_USER_RANK='OFFR',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where WPH_ID=vMAX_WPH_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN

        insert into t_wpd_work_performance_detail(WPD_ID,WPH_ID,INM_NO,ADM_NO,WPD_STATUS,WOR_DAT_ASSIG_DT,WPD_EFFECTIVE_DATE_DT,
        EAR_GRAD_CODE,WPD_PER_ATT,WPD_PER_SKI,WPD_PER_DIS,WPD_PER_QUA,WPD_PER_SAF,WPD_OA_PER,WPD_CH_ASSIG_FLAG,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_wpd_id.nextval,vMAX_WPH_ID,vINM,vADM,'P',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),
        'DS',1,1,1,1,1,1,2,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - No record WPD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - Other record WPD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_030_SUBMIT - Other record','vERM:'||vERM,vERR);    

END  IND_030_SUBMIT;  

PROCEDURE IND_031_CONFIRM (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_WPH_ID Number;
vWPH_ID T_WPH_WORK_PERFORMANCE_HEADER.WPH_ID%TYPE;
--vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_WPH_WORK_PERFORMANCE_HEADER where ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_wph_id.nextval into vWPH_ID from dual;
        BEGIN
        insert into T_WPH_WORK_PERFORMANCE_HEADER(WPH_ID,INS_CODE,WOC_CODE,WPH_STATUS,
        WPH_ASS_DT,WPH_ASS_PERIOD_FR_DT,WPH_ASS_PERIOD_TO_DT,WOL_LOCAT,WPH_SUBMIT_USER_ID,WPH_SUBMIT_USER_RANK,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vWPH_ID,vINS_CODE,vWOC_CODE,'C',
        TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+30,vWOL_LOCAT,'USR_A','OFFR',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - No record WPH','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - Other record WPH','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into t_wpd_work_performance_detail(WPD_ID,WPH_ID,INM_NO,ADM_NO,WPD_STATUS,WOR_DAT_ASSIG_DT,WPD_EFFECTIVE_DATE_DT,
        EAR_GRAD_CODE,WPD_PER_ATT,WPD_PER_SKI,WPD_PER_DIS,WPD_PER_QUA,WPD_PER_SAF,WPD_OA_PER,WPD_CH_ASSIG_FLAG,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_wpd_id.nextval,vWPH_ID,vINM,vADM,'C',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),
        'DS',1,1,1,1,1,1,2,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - No record WPD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - Other record WPD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(wph_id) into vMAX_WPH_ID from T_WPH_WORK_PERFORMANCE_HEADER where  ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
        
        update T_WPH_WORK_PERFORMANCE_HEADER set WPH_STATUS='C',WPH_SUBMIT_USER_ID='USR_A',WPH_SUBMIT_USER_RANK='OFFR',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where WPH_ID=vMAX_WPH_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN

        insert into t_wpd_work_performance_detail(WPD_ID,WPH_ID,INM_NO,ADM_NO,WPD_STATUS,WOR_DAT_ASSIG_DT,WPD_EFFECTIVE_DATE_DT,
        EAR_GRAD_CODE,WPD_PER_ATT,WPD_PER_SKI,WPD_PER_DIS,WPD_PER_QUA,WPD_PER_SAF,WPD_OA_PER,WPD_CH_ASSIG_FLAG,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_wpd_id.nextval,vMAX_WPH_ID,vINM,vADM,'C',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),
        'DS',1,1,1,1,1,1,2,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - No record WPD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - Other record WPD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_CONFIRM - Other record','vERM:'||vERM,vERR);    

END  IND_031_CONFIRM; 


PROCEDURE IND_031_2_RECOMMEND (
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN T_EPE_EV_ORG_PERIOD.ins_code%type,
      vFM_DT IN VARCHAR2,
      vUSR_RECOMMEND IN VARCHAR2
  )
  IS

vCount NUMBER:=0;
vERR Number;
vERM VARCHAR2(100);   
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vWPH_ID t_iow_ind_ot_work.iow_id%type;

BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOC_CODE(vWOL_LOC_DESC,vINS_CODE);

Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'IND_031_2_RECOMMEND - Start','vWOC_CODE',vWOC_CODE); 

    BEGIN


        select nvl(max(wph_id),0) into vWPH_ID from T_WPH_WORK_PERFORMANCE_HEADER where  ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
        
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'IND_031_2_RECOMMEND - Start','vWPH_ID',vWPH_ID); 

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);        
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'IND_031_2_RECOMMEND - No period','vERM:'||vERM,vERR); 
        WHEN OTHERS THEN
            vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);         
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'IND_031_2_RECOMMEND - Other period','vERM:'||vERM,vERR); 

    END;

    BEGIN
        For r in (select e.inm_no, e.adm_no, t.usr_id 
        from t_wpd_work_performance_detail e, t_wvt_wvtab t
        where e.wph_id=vWPH_ID  and t.ins_code=vINS_CODE and t.wvt_member_type='M' and t.usr_id=vUSR_RECOMMEND
        )
        loop
        
        select count(*) into vCount from T_WRU_WORK_RECOMMEND_USER where wkr_ser_no=vWPH_ID and inm_no=r.inm_no and
        adm_no=r.adm_no and usr_id=r.usr_id;
        
        If vCount = 0 THEN

            insert into T_WRU_WORK_RECOMMEND_USER (inm_no, adm_no, wkr_ser_no, usr_id, wru_status,wru_type,wru_recommend_dt,     
            CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY) 
            values(r.inm_no, r.adm_no, vWPH_ID, r.usr_id, 'R','P',to_date(vFM_DT,'yyyyMMdd'),
            systimestamp, 'USR_IND', systimestamp, 'USR_IND');
        ELSE
            update T_WRU_WORK_RECOMMEND_USER set wru_recommend_dt=to_date(vFM_DT,'yyyyMMdd'),updated_ts=systimestamp,updated_by='USR_IND'
            where inm_no=r.inm_no and adm_no=r.adm_no and wkr_ser_no=vWPH_ID 
            and usr_id='USR_IND' and wru_status='R' and wru_type='P'; 
        End if;
        end loop;



    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);        
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'IND_031_2_RECOMMEND - No record','vERM:'||vERM,vERR); 
        WHEN OTHERS THEN
            vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);         
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string, dbg_value)values (systimestamp, 'IND_031_2_RECOMMEND - Other record','vERM:'||vERM,vERR); 

    END;


END IND_031_2_RECOMMEND;

PROCEDURE IND_031_3_APPROVE (
      vCIN IN t_inm_inmate.inm_cin%type,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   
vMAX_WPH_ID Number;
vWPH_ID T_WPH_WORK_PERFORMANCE_HEADER.WPH_ID%TYPE;
--vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;
vINM T_INM_INMATE.INM_NO%TYPE;
vADM T_INM_INMATE.ADM_NO%TYPE;
vWOP_PA_JOB_CODE t_z_tjm_trade_job_mapping.WOP_PA_JOB_CODE%type; 
BEGIN
vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
vWOC_CODE:=GET_WOCCODE(vWOL_LOC_DESC,vINS_CODE);
vINM:=GET_INMNO(vCIN);
vADM:=GET_ADMNO(vCIN);
    
    BEGIN
    select count(*) into vCount from T_WPH_WORK_PERFORMANCE_HEADER where ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then
    --T_IOW_IND_OT_WORK
    --T_IOD_IND_OT_WORK_DETAIL
        select seq_wph_id.nextval into vWPH_ID from dual;
        BEGIN
        insert into T_WPH_WORK_PERFORMANCE_HEADER(WPH_ID,INS_CODE,WOC_CODE,WPH_STATUS,
        WPH_ASS_DT,WPH_ASS_PERIOD_FR_DT,WPH_ASS_PERIOD_TO_DT,WOL_LOCAT,WPH_SUBMIT_USER_ID,WPH_SUBMIT_USER_RANK,WPH_APPROVE_USER_ID,WPH_APPROVE_USER_RANK,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vWPH_ID,vINS_CODE,vWOC_CODE,'A',
        TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd')+30,vWOL_LOCAT,'USR_A','OFFR','USR_B','OFFR',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No record WPH','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other record WPH','vERM:'||vERM,vERR);    
    
        END;    
        BEGIN
        insert into t_wpd_work_performance_detail(WPD_ID,WPH_ID,INM_NO,ADM_NO,WPD_STATUS,WOR_DAT_ASSIG_DT,WPD_EFFECTIVE_DATE_DT,
        EAR_GRAD_CODE,WPD_PER_ATT,WPD_PER_SKI,WPD_PER_DIS,WPD_PER_QUA,WPD_PER_SAF,WPD_OA_PER,WPD_CH_ASSIG_FLAG,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_wpd_id.nextval,vWPH_ID,vINM,vADM,'C',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),
        'DS',1,1,1,1,1,1,2,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No record WPD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other record WPD','vERM:'||vERM,vERR);    
    
        END;      
    Else
        BEGIN
        select max(wph_id) into vMAX_WPH_ID from T_WPH_WORK_PERFORMANCE_HEADER where  ins_code=vINS_CODE and woc_code=vWOC_CODE and wph_ass_period_to_dt >= to_date(vFM_DT,'yyyyMMdd') and wph_ass_period_fr_dt <= to_date(vFM_DT,'yyyyMMdd');
        
        update T_WPH_WORK_PERFORMANCE_HEADER set WPH_STATUS='A',WPH_SUBMIT_USER_ID='USR_A',WPH_SUBMIT_USER_RANK='OFFR',WPH_APPROVE_USER_ID='USR_B',WPH_APPROVE_USER_RANK='OFFR',UPDATED_TS=systimestamp,UPDATED_BY='USR_IND'
        where WPH_ID=vMAX_WPH_ID;
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No max','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other max','vERM:'||vERM,vERR);    
    
        END;

        BEGIN

        insert into t_wpd_work_performance_detail(WPD_ID,WPH_ID,INM_NO,ADM_NO,WPD_STATUS,WOR_DAT_ASSIG_DT,WPD_EFFECTIVE_DATE_DT,
        EAR_GRAD_CODE,WPD_PER_ATT,WPD_PER_SKI,WPD_PER_DIS,WPD_PER_QUA,WPD_PER_SAF,WPD_OA_PER,WPD_CH_ASSIG_FLAG,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_wpd_id.nextval,vMAX_WPH_ID,vINM,vADM,'C',TO_DATE(vFM_DT,'yyyyMMdd'),TO_DATE(vFM_DT,'yyyyMMdd'),
        'DS',1,1,1,1,1,1,2,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No record WPD','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other record WPD','vERM:'||vERM,vERR);    
    
        END;        
        
    End If;
    
    Begin
    select count(*) into vCount from T_WOR_WORK where ins_code=vINS_CODE and inm_no=vINM and adm_no=vADM ;
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No work','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other work','vERM:'||vERM,vERR);    

    END;
    if vCount = 0 then
    vWOP_PA_JOB_CODE:=GET_WOP_PA_JOB_CODE(vWOL_LOCAT,vINS_CODE);
    
        insert into t_wor_work(INM_NO,ADM_NO,WOR_SER_NO,INS_CODE,WOR_DAT_ASSIG_DT, WOR_ASSIGN_REC_TYPE,
        WOL_LOCAT,WOP_PA_JOB_CODE,EAR_GRAD_CODE,P_EAR_GRAD_CODE,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(vINM,vADM,1,vINS_CODE,TO_DATE(vFM_DT,'yyyyMMdd'), 'S',
        vWOL_LOCAT,vWOP_PA_JOB_CODE,1,1,
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_031_3_APPROVE - Other record','vERM:'||vERM,vERR);    

END  IND_031_3_APPROVE; 

PROCEDURE IND_033_TOOL_SAVE (
      vTOL_SERIAL_NO IN t_tol_workshop_tool.TOL_SERIAL_NO%TYPE,
      vWOL_LOCAT IN T_Z_WOL_WORK_LOC_CODE.WOL_LOCAT%type,
      vINS_CODE IN t_inm_inmate.ins_code%type,
      vFM_DT IN varchar2
  )
  IS
vCount Number;
vERR Number;
vERM VARCHAR2(100);   

--vIOD_ID Number;
vWOL_LOC_DESC T_Z_WOL_WORK_LOC_CODE.WOL_LOC_DESC%type;
vWOC_CODE T_Z_WOC_WORKSHOP_CODE.WOC_CODE%TYPE;

BEGIN
--vWOL_LOC_DESC:=GET_WOL_LOC_DESC(vWOL_LOCAT,vINS_CODE);
--Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string )values (systimestamp, 'IND_033_TOOL_SAVE - Start','vWOL_LOC_DESC:'||vWOL_LOC_DESC);    

vWOC_CODE:=GET_WOCCODE(vWOL_LOCAT,vINS_CODE);
Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string )values (systimestamp, 'IND_033_TOOL_SAVE - Start','vWOC_CODE:'||vWOC_CODE);    

    
    BEGIN
    select count(*) into vCount from t_tol_workshop_tool where ins_code=vINS_CODE and woc_code=vWOC_CODE and TOL_SERIAL_NO = vTOL_SERIAL_NO;
    EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_033_TOOL_SAVE - No count','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_033_TOOL_SAVE - Other count','vERM:'||vERM,vERR);    

    END;
    IF vCount = 0 Then

        BEGIN
        insert into t_tol_workshop_tool(TOL_ID,INS_CODE,WOC_CODE,TOL_STATUS,TOL_SERIAL_NO,WOL_LOCAT,TOL_REG_DT,
        TOL_DESC,TOL_REMARK,TOL_MO_LIST,TOL_MIN_FIT_LV,TOOL_BOX_CODE,TOL_HC_LIST,
        CREATED_TS, CREATED_BY, UPDATED_TS, UPDATED_BY
        )values(seq_tol_id.nextval,vINS_CODE,vWOC_CODE,'A',vTOL_SERIAL_NO,vWOL_LOCAT,TO_DATE(vFM_DT,'yyyyMMdd'),
        'GOOD TOOL','GOOD TOOL','Y','1','1','N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND'
        );
        EXCEPTION 
        WHEN NO_DATA_FOUND THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_033_TOOL_SAVE - No record WPH','vERM:'||vERM,vERR);    
    
        WHEN OTHERS THEN
                    vERR:=SQLCODE;
                vERM:=SUBSTR(SQLERRM, 1,100);
                Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_033_TOOL_SAVE - Other record WPH','vERM:'||vERM,vERR);    
    
        END;    
    End If;
    
EXCEPTION 
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_033_TOOL_SAVE - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_033_TOOL_SAVE - Other record','vERM:'||vERM,vERR);    
  
END IND_033_TOOL_SAVE;

PROCEDURE IND_035_TOOL_CHECKOUT (
      vUSR_ID IN t_tio_workshop_tool_chk_in_out.tio_chk_out_by_usr_id%type,
      vTOL_SERIAL_NO IN t_tol_workshop_tool.TOL_SERIAL_NO%type,
      --vTOL_EID IN t_tio_workshop_tool_chk_in_out.TOL_EID%type,
      vFM_DT IN varchar2
  )
    IS
vCount Number;
vERR Number;
vERM VARCHAR2(100); 
vINM number;
vADM number;
BEGIN
vINM := GET_INMNO( vUSR_ID);
vADM := GET_ADMNO( vUSR_ID);
FOR r in (select tol_id, ins_code, woc_code, tol_eid from t_tol_workshop_tool where TOL_SERIAL_NO =vTOL_SERIAL_NO and tol_status <>'D')
LOOP

    insert into t_tio_workshop_tool_chk_in_out(tio_id, tol_id, ins_code, woc_code, tol_eid, tio_chk_out_dt, tio_chk_out_by_inm_no,tio_chk_out_by_adm_no,tio_status,
        tio_archived,
        created_ts, created_by, updated_ts, updated_by)
    values(seq_tio_id.nextval, r.tol_id, r.ins_code, r.woc_code, r.tol_eid, to_date(vFM_DT,'yyyyMMdd'), vINM,vADM,'O',
        'N',
        systimestamp, 'USR_IND', systimestamp, 'USR_IND');
END LOOP;

EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_035_TOOL_CHECKOUT - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_035_TOOL_CHECKOUT - Other record','vERM:'||vERM,vERR);    



END IND_035_TOOL_CHECKOUT;  

PROCEDURE IND_036_TOOL_CHECKIN (
      vUSR_ID IN t_tio_workshop_tool_chk_in_out.tio_chk_out_by_usr_id%type,
      vTOL_SERIAL_NO IN t_tol_workshop_tool.TOL_SERIAL_NO%type,
      --vTOL_EID IN t_tio_workshop_tool_chk_in_out.TOL_EID%type,
      vFM_DT IN varchar2
  )
IS
vCount Number;
vERR Number;
vERM VARCHAR2(100); 

vINM number;
vADM number;
BEGIN
vINM := GET_INMNO( vUSR_ID);
vADM := GET_ADMNO( vUSR_ID);

FOR r in (select c.tio_id
from t_tol_workshop_tool t, t_tio_workshop_tool_chk_in_out c  where t.TOL_SERIAL_NO =vTOL_SERIAL_NO and t.tol_status <>'D'
and t.tol_id=c.tol_id and c.tio_chk_in_dt is null
)
LOOP
    update t_tio_workshop_tool_chk_in_out set tio_chk_in_dt=to_date(vFM_DT,'yyyyMMdd'), tio_chk_in_by_inm_no=vINM, tio_chk_in_by_adm_no=vADM, tio_status='I' where tio_id=r.tio_id;
END LOOP;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_036_TOOL_CHECKIN - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_036_TOOL_CHECKIN - Other record','vERM:'||vERM,vERR);    



END IND_036_TOOL_CHECKIN;  

PROCEDURE IND_040_TOOL_VERIFY (
      vWOC_CODE IN t_wvh_ws_tool_verify_header.WOC_CODE%type,
	  vTOOL_BOX_CODE IN t_tol_workshop_tool.TOOL_BOX_CODE%type,
      vINS_CODE IN t_wvh_ws_tool_verify_header.INS_CODE%type,      
      vTASK IN t_wvh_ws_tool_verify_header.wvh_type%type,
      vFM_DT IN varchar2,
      vUSR_ID IN t_wvh_ws_tool_verify_header.wvh_req_usr_id%type      
  )
IS
vERR Number;
vERM VARCHAR2(100); 
vWVH_ID Number;
BEGIN  
select seq_wvh_id.nextval into vWVH_ID from dual;


insert into t_wvh_ws_tool_verify_header(wvh_id, ins_code, woc_code, wvh_req_usr_id, wvh_req_dt, wvh_status, wvh_type, tool_box_code,
created_ts,created_by,updated_ts,updated_by)
values(vWVH_ID, vINS_CODE, vWOC_CODE, vUSR_ID, to_date(vFM_DT,'yyyyMMdd'), 'P', vTask, vTOOL_BOX_CODE,
systimestamp,'USR_IND',systimestamp,'USR_IND');
FOR t in (select tol_id, tol_eid from t_tol_workshop_tool where tol_status<>'D' and ins_code=vINS_CODE and woc_code=vWOC_CODE and tool_box_code=vTOOL_BOX_CODE)
LOOP
    insert into t_wvd_ws_tool_verify_detail(wvh_id, wvd_id, tol_id, tol_eid, wvd_status,
    created_ts,created_by,updated_ts,updated_by)
    values(vWVH_ID, seq_wvd_id.nextval, t.tol_id, t.tol_eid, 'A',
    systimestamp,'USR_IND',systimestamp,'USR_IND');
END LOOP;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_040_TOOL_VERIFY - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_040_TOOL_VERIFY - Other record','vERM:'||vERM,vERR);    



END IND_040_TOOL_VERIFY;  

PROCEDURE IND_040_2_TOOL_VERIFY_CONFIRM (
      vWOC_CODE IN t_wvh_ws_tool_verify_header.WOC_CODE%type,
	  vTOOL_BOX_CODE IN t_tol_workshop_tool.TOOL_BOX_CODE%type,
      vINS_CODE IN t_wvh_ws_tool_verify_header.INS_CODE%type,      
      vTASK IN t_wvh_ws_tool_verify_header.wvh_type%type,
      vFM_DT IN varchar2,
      vUSR_ID IN t_wvh_ws_tool_verify_header.wvh_req_usr_id%type,
      vCONFIRM_USR_ID IN t_wvh_ws_tool_verify_header.wvh_confirm_usr_id%type       
  )
IS
vERR Number;
vERM VARCHAR2(100); 
vWVH_ID Number:=0;
BEGIN  

select wvh_id into vWVH_ID from t_wvh_ws_tool_verify_header 
where woc_code=vWOC_CODE and tool_box_code=vTOOL_BOX_CODE and ins_code=vINS_CODE and wvh_type=vTask and wvh_req_usr_id=vUSR_ID and wvh_req_dt=to_date(vFM_DT,'yyyyMMdd') and wvh_status='P';

If vWVH_ID <> 0 then
update t_wvh_ws_tool_verify_header set wvh_confirm_usr_id =vCONFIRM_USR_ID, wvh_confirm_dt=sysdate, wvh_status='C' where wvh_id=vWVH_ID;

END if;
  
EXCEPTION
    WHEN NO_DATA_FOUND THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_040_2_TOOL_VERIFY_CONFIRM - No record','vERM:'||vERM,vERR);    

    WHEN OTHERS THEN
                vERR:=SQLCODE;
            vERM:=SUBSTR(SQLERRM, 1,100);
            Insert into t_dbg_debug (created_ts, dbg_msgs, dbg_string,dbg_value )values (systimestamp, 'IND_040_2_TOOL_VERIFY_CONFIRM - Other record','vERM:'||vERM,vERR);    



END IND_040_2_TOOL_VERIFY_CONFIRM;  

END PKG_FAST_TRACK;