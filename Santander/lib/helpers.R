clean_names <- function(train_df){
  good_names <- c('fecha_dato' = "fetch_date",
                  'ncodpers' = "customer_code",
                  'ind_empleado' = "employee_index",
                  'pais_residencia' = "country_residence",
                  'sexo' = "sex", 
                  'age' = "age", 
                  'fecha_alta' = "signup_date", 
                  'ind_nuevo' = "new_customer", 
                  'antiguedad' = "seniority", 
                  'indrel' = "completed_month",
                  'ult_fec_cli_1t' = "leave_date",
                  'indrel_1mes' = "customer_type",
                  'tiprel_1mes' = "customer_relation", 
                  'indresi' = "residence_same_as_bank", 
                  'indext' = "foreigner", 
                  'conyuemp' = "employee_spouse", 
                  'canal_entrada' = "join_channel", 
                  'indfall' = "dead",
                  'tipodom' = "addres_type",
                  'cod_prov' = "province_code",
                  'nomprov' = "province_name",
                  'ind_actividad_cliente' = "activity_index",
                  'renta' = "gross_income",
                  'segmento' = "segment",
                  'ind_ahor_fin_ult1' = 'ind_ahor_fin_ult1', 'ind_aval_fin_ult1' = 'ind_aval_fin_ult1', 'ind_cco_fin_ult1' = 'ind_cco_fin_ult1', 'ind_cder_fin_ult1' = 'ind_cder_fin_ult1', 'ind_cno_fin_ult1' = 'ind_cno_fin_ult1', 'ind_ctju_fin_ult1' = 'ind_ctju_fin_ult1', 'ind_ctma_fin_ult1' = 'ind_ctma_fin_ult1', 'ind_ctop_fin_ult1' = 'ind_ctop_fin_ult1', 'ind_ctpp_fin_ult1' = 'ind_ctpp_fin_ult1', 'ind_deco_fin_ult1' = 'ind_deco_fin_ult1', 'ind_deme_fin_ult1' = 'ind_deme_fin_ult1', 'ind_dela_fin_ult1' = 'ind_dela_fin_ult1', 'ind_ecue_fin_ult1' = 'ind_ecue_fin_ult1', 'ind_fond_fin_ult1' = 'ind_fond_fin_ult1', 'ind_hip_fin_ult1' = 'ind_hip_fin_ult1', 'ind_plan_fin_ult1' = 'ind_plan_fin_ult1', 'ind_pres_fin_ult1' = 'ind_pres_fin_ult1', 'ind_reca_fin_ult1' = 'ind_reca_fin_ult1', 'ind_tjcr_fin_ult1' = 'ind_tjcr_fin_ult1', 'ind_valo_fin_ult1' = 'ind_valo_fin_ult1', 'ind_viv_fin_ult1' = 'ind_viv_fin_ult1', 'ind_nomina_ult1' = 'ind_nomina_ult1', 'ind_nom_pens_ult1' = 'ind_nom_pens_ult1', 'ind_recibo_ult1' = 'ind_recibo_ult1')
  
  colnames(train_df) <- good_names[colnames(train_df)]
  return(train_df)
}
