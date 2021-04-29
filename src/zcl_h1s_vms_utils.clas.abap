class ZCL_H1S_VMS_UTILS definition
  public
  final
  create public .

public section.

  types:
    ty_t_records TYPE RANGE OF dd07l-domvalue_l .
  types:
    ty_t_config type TABLE OF VLC_CONF_OUT_ASSIGNMENT .
  types:
    ty_t_atinn type range of ATINN .
  types:
    ty_t_atnam type range of ATnam .
  types:
    BEGIN OF ty_record_range,
        sign(1)   TYPE c,
        option(2) TYPE c,
        low       TYPE zvms_iftyp,
        high      TYPE zvms_iftyp,
      END OF ty_record_range .
  types:
    ty_t_record_range TYPE TABLE OF ty_record_range .
  types:
    ty_t_rcrd_details TYPE TABLE OF zvmsm_vpcint .

  constants GC_ACTION_SYNCPOST type C value 'S' ##NO_TEXT.
  constants GC_ACTION_ASYNCPOST type C value 'A' ##NO_TEXT.
  constants GC_ACTION_NOPOSTING type C value 'N' ##NO_TEXT.
  constants GC_XFLAG type C value 'X' ##NO_TEXT.
  constants GC_FLAG_1 type C value '1' ##NO_TEXT.
  constants GC_FLAG_2 type C value '2' ##NO_TEXT.
  constants GC_FLAG_3 type C value '3' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(RV_INST) type ref to ZCL_H1S_VMS_UTILS .
  methods GET_OUTBOUND_VPC_RECD_DETAILS
    importing
      !IT_RCDTYP type TY_T_RECORD_RANGE
    exporting
      !ET_RCD_DETAILS type TY_T_RCRD_DETAILS
    raising
      ZCX_NO_RECORD_FOUND .
  methods SET_ACTION
    importing
      value(IV_ACTION) type BAPIVEHIACTION-VEHICLEACTION
      !IT_GUIDS type ZTVMS_VEHIACTIONDATAITEM
    exporting
      !ET_RETURN type BAPIRET2_T .
  methods GET_RECORD_TYPES
    importing
      !IT_RECORD_TYPES type TY_T_RECORD_RANGE
    exporting
      !ET_RECORD_TYPES type ZTECC_DD07L .
  methods ON_VEHICLE_ISOLATION
    importing
      !IV_COMMIT type C
      !IV_ACTION_COUNTER type SYTABIX
      !IV_DIALOGUE_ALLOWED type C
      !IS_INCOMING_ACTION type VLCC_CVLC03_PS
      !IS_ELEMENTARY_ACTION type VLCC_CVLC03_PS
      !IT_VLCSTATUS type VLCSTATUS_T
      !IT_VLCBAPICU type VLCBAPICU_T
    changing
      !CT_VLCDIAVEHI type VLCDIAVEHI_T
      !CT_VLCDIAVEHI_OK type VLCDIAVEHI_T
      !CT_VLCDIAVEHI_NO type VLCDIAVEHI_T
      !CS_VLCACTDATA_CS type VLCACTDATA
      !CT_VLCH_MSSG_CT type VLCH_MSSG_PT
      !CT_VLCBAPICU1_IT type VLCBAPICU_T
    exceptions
      ACTION_NOT_COMPL_PERFORMED
      ACTION_NOT_PERFORMED .
  class-methods GET_BAPI_EXTENSION_VELO
    importing
      !IS_DATA type BAPI_TE_VLCVEHICLE
    exporting
      !ET_EXTENSION type BAPIPAREX_T
    exceptions
      PARSING_ERROR .
  methods GET_INCOMING_ACTION
    importing
      !IV_INCOMING_ACTION type VLC_ACTION
    exporting
      !ES_INCOMING_ACTION type VLCC_CVLC03_PS
      !ES_ELEMENTARY_ACTION type VLCC_CVLC03_PS
      !XT_VLCH_MSSG type VLCH_MSSG_PT
    exceptions
      ACTION_NOT_DEFINED .
  class-methods GET_PARAMETER_VALUES
    importing
      value(IT_PARAMETER_NAME) type ZTVMS_ACTION
    exporting
      value(ET_PARAM_VALUES) type ZTVMS_PARAM_TAXGL .
  class-methods CHECK_CONFIG_MATCH
    importing
      !IT_CONFIG type VLCCONFIG_T
    exporting
      !ET_MATCHING_CHAR type TY_T_ATINN
      !ET_MATCHING_VALUE type TY_T_ATNAM
    exceptions
      PARTIAL_MATCH_NOT_POSSIBLE .
protected section.
private section.

  constants GC_FUNC_ID type IF_FDT_TYPES=>ID value '00163E1D3A421ED7B8FCB743F0EB6DB1' ##NO_TEXT.
  class-data GO_REF type ref to ZCL_H1S_VMS_UTILS .
  constants GC_GUID type VLC_VEHICLE_ID_TYPE value 'G' ##NO_TEXT.
  constants GC_RECORD type DOMNAME value 'ZIFTYP_D' ##NO_TEXT.
  class-data:
    gt_fifo type standard table of zvmsc_fifo_char .
ENDCLASS.



CLASS ZCL_H1S_VMS_UTILS IMPLEMENTATION.


METHOD check_config_match.

    DATA lt_config TYPE SORTED TABLE OF conf_out WITH NON-UNIQUE KEY atnam.

    TYPES : BEGIN OF lty_int,
              atnam TYPE atnam,
            END OF lty_int.
    DATA: lt_sd TYPE STANDARD TABLE OF lty_int.
    CLEAR et_matching_char.

    IF gt_fifo IS INITIAL.
      SELECT * FROM zvmsc_fifo_char
        INTO TABLE @gt_fifo
         WHERE characteristic <> @space.
      IF sy-subrc <> 0.
        CLEAR gt_fifo.
      ENDIF.
    ENDIF.

    IF gt_fifo IS INITIAL.
      RAISE partial_match_not_possible.
    ENDIF.

    lt_config = it_config.


    LOOP AT gt_fifo INTO DATA(ls_fifo).
      READ TABLE lt_config INTO DATA(ls_config)
        WITH KEY atnam = ls_fifo-characteristic.
      IF sy-subrc <> 0.
        IF ls_fifo-altcharacteristic IS NOT INITIAL.
          READ TABLE lt_config INTO ls_config
            WITH KEY atnam = ls_fifo-altcharacteristic.
          IF sy-subrc <> 0.
                   APPEND INITIAL LINE TO et_matching_value ASSIGNING FIELD-SYMBOL(<ls_match1>).
        <ls_match1>-sign = 'I'.
        <ls_match1>-option = 'EQ'.
        <ls_match1>-low = ls_fifo-characteristic.
        <ls_match1>-high = ' '.
          ELSE.
            APPEND INITIAL LINE TO et_matching_value ASSIGNING FIELD-SYMBOL(<ls_match3>).
            <ls_match3>-sign = 'I'.
            <ls_match3>-option = 'EQ'.
            <ls_match3>-low = ls_fifo-altcharacteristic.
            <ls_match3>-high =  ls_fifo-altcharacteristic.
          ENDIF.
        ELSE.
            APPEND INITIAL LINE TO et_matching_value ASSIGNING FIELD-SYMBOL(<ls_match4>).
            <ls_match4>-sign = 'I'.
            <ls_match4>-option = 'EQ'.
            <ls_match4>-low = ls_fifo-characteristic.
            <ls_match4>-high = ' '.
        ENDIF.
      ELSE.
            APPEND INITIAL LINE TO et_matching_value ASSIGNING FIELD-SYMBOL(<ls_match2>).
*        APPEND INITIAL LINE TO et_matching_char ASSIGNING <ls_match>.
         <ls_match2>-sign = 'I'.
        <ls_match2>-option = 'EQ'.
        <ls_match2>-low = ls_fifo-characteristic.
        <ls_match2>-high = ls_config-atnam.
      ENDIF.
      CLEAR:ls_config.
    ENDLOOP.



*    LOOP AT gt_fifo INTO DATA(ls_fifo).
*      READ TABLE lt_config INTO DATA(ls_config)
*        WITH KEY atnam = ls_fifo-characteristic.
*      IF sy-subrc <> 0.
*
*        IF ls_fifo-altcharacteristic IS NOT INITIAL.
*          READ TABLE lt_config INTO ls_config
*            WITH KEY atnam = ls_fifo-altcharacteristic.
*          IF sy-subrc <> 0.
*            RAISE partial_match_not_possible.
*          ELSE.
*            APPEND INITIAL LINE TO et_matching_char ASSIGNING FIELD-SYMBOL(<ls_match>).
*            <ls_match>-sign = 'I'.
*            <ls_match>-option = 'EQ'.
*            <ls_match>-low = ls_config-atinn.
*          ENDIF.
*        ELSE.
*          RAISE partial_match_not_possible.
*        ENDIF.
*      ELSE.
*        APPEND INITIAL LINE TO et_matching_char ASSIGNING <ls_match>.
*        <ls_match>-sign = 'I'.
*        <ls_match>-option = 'EQ'.
*        <ls_match>-low = ls_config-atinn.
*      ENDIF.
*      CLEAR:ls_config.
*    ENDLOOP.

    IF et_matching_value IS INITIAL.
      RAISE partial_match_not_possible.
    ELSE.
      SORT et_matching_value BY low.
      DELETE ADJACENT DUPLICATES FROM et_matching_value COMPARING low.
    ENDIF.


  ENDMETHOD.


method CONSTRUCTOR.
  endmethod.


METHOD GET_BAPI_EXTENSION_VELO.

  TYPES ty_s_extension TYPE c LENGTH 960.

  DATA ls_ext_temp TYPE ty_s_extension.

  CLEAR et_extension.

  cl_abap_container_utilities=>fill_container_c(
     EXPORTING
       im_value               = is_data
     IMPORTING
       ex_container           = ls_ext_temp
     EXCEPTIONS
         illegal_parameter_type = 1
         OTHERS                 = 2 ).

  IF sy-subrc <> 0.
    RAISE parsing_error.
  ENDIF.

  APPEND INITIAL LINE TO et_extension ASSIGNING FIELD-SYMBOL(<ls_bapiext>).
  <ls_bapiext>-structure  = 'BAPI_TE_VLCVEHICLE'.
  <ls_bapiext>-valuepart1 = ls_ext_temp(240).
  <ls_bapiext>-valuepart2 = ls_ext_temp+240(240).
  <ls_bapiext>-valuepart3 = ls_ext_temp+480(240).
  <ls_bapiext>-valuepart4 = ls_ext_temp+720(240).

ENDMETHOD.


METHOD get_incoming_action.

*    DATA: ls_cvlc03 TYPE cvlc03.
*
**--> is the action defined ?
*    CALL FUNCTION 'VELO04_READ_CVLC03'
*      EXPORTING
*        action_iv          = iv_incoming_action
*      IMPORTING
*        action_es          = ls_cvlc03
*      EXCEPTIONS
*        action_not_defined = 1
*        OTHERS             = 2.
*
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'VELO03_FILL_ERROR_TABLE'
*        EXPORTING
*          msgid_iv     = sy-msgid
*          msgty_iv     = sy-msgty
*          msgno_iv     = sy-msgno
*          msgv1_iv     = sy-msgv1
*          msgv2_iv     = sy-msgv2
*          msgv3_iv     = sy-msgv3
*          msgv4_iv     = sy-msgv4
*        TABLES
*          vlch_mssg_ct = xt_vlch_mssg.
*      RAISE action_not_defined.
*      RETURN.
*    ENDIF.
*
*    MOVE-CORRESPONDING ls_cvlc03 TO es_incoming_action.
*    MOVE-CORRESPONDING ls_cvlc03 TO es_elementary_action.
*
**--> read the action-shorttext
*    CALL FUNCTION 'VELO04_READ_CVLC03T'
*      EXPORTING
*        action_iv     = es_incoming_action-aktion
*      IMPORTING
*        actiontext_ev = es_incoming_action-aktiont
*      EXCEPTIONS
*        no_action     = 1
*        OTHERS        = 2.
*
*    IF sy-subrc <> 0.
*      es_incoming_action-aktiont = es_incoming_action-aktion.
*      es_elementary_action-aktiont = es_elementary_action-aktion.
*    ENDIF.

  ENDMETHOD.


METHOD get_instance.
    IF go_ref IS NOT BOUND.
*     If reference is not instantiated create a new instance
      go_ref = NEW zcl_h1s_vms_utils( ).
      rv_inst = go_ref.
    ELSE.
*     If already exists send the same instance
      rv_inst = go_ref.
    ENDIF.
  ENDMETHOD.


method GET_OUTBOUND_VPC_RECD_DETAILS.
**   Get all details against a Record type from the custom table
    SELECT mandt,
           iftyp,
           pstatus1,
           pstatus2,
           sstatus1,
           sstatus2,
           action,
           actionchck,
           actionchck1 INTO TABLE @et_rcd_details
                      FROM zvmsm_vpcint
                      WHERE iftyp IN @it_rcdtyp. "Records Types
    IF sy-subrc = 0.
      SORT et_rcd_details ASCENDING BY iftyp.
    else.
**      If no entry is found then raise an exception.
      RAISE EXCEPTION TYPE ZCX_no_record_found.
    ENDIF.
  endmethod.


METHOD get_parameter_values.
    DATA : lo_admin_data   TYPE REF TO if_fdt_admin_data,
           lo_function     TYPE REF TO if_fdt_function,
           lo_context      TYPE REF TO if_fdt_context,
           lo_result       TYPE REF TO if_fdt_result,
           lt_param        TYPE ztvms_param_taxgl,
           lt_param_values TYPE ztvms_param_taxgl.

    IF it_parameter_name IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_fdt_factory=>get_instance_generic( EXPORTING iv_id = gc_func_id "00163E1D3A421ED7B8FCB743F0EB6DB1
                                              IMPORTING eo_instance = lo_admin_data ).
      CATCH cx_fdt_input  ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF lo_admin_data IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lo_function ?= lo_admin_data.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF lo_function IS NOT BOUND.
      RETURN.
    ENDIF.

*Set input parameter
    TRY.
        lo_context ?= lo_function->get_process_context( ).
      CATCH cx_fdt_config ##NO_HANDLER.
        RETURN.
      CATCH cx_fdt_input ##NO_HANDLER.
        RETURN.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF lo_context IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT it_parameter_name ASSIGNING FIELD-SYMBOL(<ls_param_name>).

      TRY.
          lo_context->set_value( iv_name = 'ACTION'
                                 ia_value = <ls_param_name> ).
          lo_function->process( EXPORTING io_context = lo_context
                                IMPORTING eo_result = lo_result ).
          IF lo_result IS NOT BOUND.
            CONTINUE.
          ENDIF.
          lo_result->get_value( IMPORTING ea_value = lt_param_values ).
        CATCH cx_fdt_input ##NO_HANDLER.
        CATCH cx_fdt ##NO_HANDLER.
      ENDTRY.

*Setting Parameter name in the table
      LOOP AT lt_param_values ASSIGNING FIELD-SYMBOL(<ls_param_values>).
        <ls_param_values>-action = <ls_param_name>.
      ENDLOOP.

      APPEND LINES OF lt_param_values TO lt_param.
      CLEAR : lt_param_values.
    ENDLOOP.

    IF lt_param IS NOT INITIAL.
      et_param_values = lt_param.
    ENDIF.
  ENDMETHOD.


METHOD get_record_types.
**    Get the domain  values for all the message record types.
    CLEAR et_record_types.
    SELECT domname,
           as4local,
           valpos,
           as4vers,
           domvalue_l,
           domvalue_h,
           appval INTO TABLE @et_record_types
                  FROM dd07l
                  WHERE domname = @gc_record "ZIFTYP_D
                  AND   domvalue_l IN @it_record_types. "Record types
    IF sy-subrc <> 0.
      CLEAR et_record_types.
    ENDIF.
  ENDMETHOD.


METHOD on_vehicle_isolation.

*--> define the vehicle-tables/structure

    DATA: lt_vlcdiavehi    TYPE TABLE OF vlcdiavehi,
          lt_vlcdiavehi_ok TYPE TABLE OF vlcdiavehi,
          lv_msgv1         TYPE sy-msgv1,
          lv_err           TYPE flag,
          lv_tabix         TYPE sy-tabix,
          lv_succ          TYPE flag.

*--> RFC Destinations of performed actions

    DATA: lt_rfcdest TYPE vlc_rfcdest_t.
    DATA: ls_rfcdest TYPE rfcdest.

*--> action handler posting variants

*-- Call the standard set single action for every vehicle. Commit or
*   rollback for individual vehicles so that partial processing of
*   the input vehicle list is possible. After processing set the error
*   flag in vehicle list to indicate success or failure and update
*   the error log for sucess and error messages for every vehicle.

    LOOP AT ct_vlcdiavehi_ok INTO DATA(ls_vlcdiavehi).

      lv_tabix = sy-tabix.

* Lock the vehicle so that there is no other process acting on the
* vehicle. In case the vehicle is already locked, this additional
* locking will not cause any additional problems.

      CALL FUNCTION 'ENQUEUE_E_VLCVEHICLE'
        EXPORTING
          mode_vlcvehicle = 'E'
          mandt           = sy-mandt
          vguid           = ls_vlcdiavehi-vguid
          x_vguid         = ' '
          _scope          = '1'
          _wait           = 'X'
          _collect        = ' '
        EXCEPTIONS
          foreign_lock    = 1
          system_failure  = 2
          OTHERS          = 3.

      REFRESH lt_vlcdiavehi.
      APPEND ls_vlcdiavehi TO lt_vlcdiavehi.

*** first step: execute the action for all vehicles in table
*   vlcdiavehi_ok_lt.

      CALL FUNCTION 'VELO09_SET_SINGLE_ACTION'
        EXPORTING
          incoming_action_is   = is_incoming_action
          elementary_action_is = is_elementary_action
          dialogue_allowed_iv  = iv_dialogue_allowed
        IMPORTING
          rfcdest_et           = lt_rfcdest
        TABLES
          vlcdiavehi_ct        = lt_vlcdiavehi
          vlcstatus_it         = it_vlcstatus
          vlch_mssg_ct         = ct_vlch_mssg_ct
          vlcbapicu_it         = ct_vlcbapicu1_it
        CHANGING
          vlcactdata_cs        = cs_vlcactdata_cs
        EXCEPTIONS
          action_not_performed = 1
          OTHERS               = 2.

      IF sy-subrc = 0.
*** second step: no error till now -> commit work - save on DB

        CASE iv_commit.
          WHEN gc_action_asyncpost.    " asynchronous posting
            COMMIT WORK.
          WHEN gc_action_syncpost.     " synchronous posting
            COMMIT WORK AND WAIT.

          WHEN gc_action_noposting.    " no posting / no commit work
          WHEN OTHERS.
            COMMIT WORK.
        ENDCASE.

* --> Commit in other systems which are concerned
        LOOP AT lt_rfcdest INTO ls_rfcdest.

          CASE iv_commit.
            WHEN gc_action_asyncpost.    " asynchronous posting

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                DESTINATION ls_rfcdest.

            WHEN gc_action_syncpost.     " synchronous posting

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                DESTINATION ls_rfcdest
                EXPORTING
                  wait = gc_xflag.

            WHEN gc_action_noposting. " no posting / no commit work here
            WHEN OTHERS.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                DESTINATION ls_rfcdest.

          ENDCASE.
        ENDLOOP.

*** third step: create success-messages for every vehicles in table
*   vlcdiavehi_ct and set the success flag.
        CALL FUNCTION 'VELO09_SET_SUCCESS_MESSAGE'
          EXPORTING
            incoming_action_is   = is_incoming_action
            elementary_action_is = is_elementary_action
          TABLES
            vlcdiavehi_ok_it     = lt_vlcdiavehi
            vlch_mssg_ct         = ct_vlch_mssg_ct.

*--> fourth step: set the success flag and update export tables

        READ TABLE lt_vlcdiavehi ASSIGNING FIELD-SYMBOL(<ls1_vlcdiavehi>)
                                 WITH KEY vguid = ls_vlcdiavehi-vguid.

        MOVE gc_flag_1 TO <ls1_vlcdiavehi>-xerror."1=Green,2=Yellow,3=red


        READ TABLE ct_vlcdiavehi ASSIGNING FIELD-SYMBOL(<ls2_vlcdiavehi>)
                                 WITH KEY vguid = ls_vlcdiavehi-vguid.

*        MODIFY vlcdiavehi_ct FROM vlcdiavehi_ls INDEX sy-tabix.
        MOVE-CORRESPONDING <ls1_vlcdiavehi> TO <ls2_vlcdiavehi>.
        lv_succ = abap_true.                          "Atleast one vehicle is successful


        READ TABLE ct_vlcdiavehi_ok ASSIGNING FIELD-SYMBOL(<ls_ok>) INDEX lv_tabix.
        IF sy-subrc = 0 AND <ls1_vlcdiavehi> IS ASSIGNED.
          MOVE-CORRESPONDING <ls1_vlcdiavehi> TO <ls_ok>.
        ENDIF.

**        Start
*          DATA ls_vlcdiavehi_ok TYPE vlcdiavehi.
*          LOOP AT lt_vlcdiavehi INTO ls_vlcdiavehi_ok.
*            MODIFY ct_vlcdiavehi_ok  FROM ls_vlcdiavehi_ok .
*          ENDLOOP.
**        End.

      ELSE.

*** fifth step: create error messages for every vehicle in table
*   vlcdiavehi_ok_ct

        CALL FUNCTION 'VELO09_MULTI_ERROR_MESSAGES'
          EXPORTING
            incoming_action_is   = is_incoming_action
            elementary_action_is = is_elementary_action
          TABLES
            vlcdiavehi_ok_it     = lt_vlcdiavehi
            vlch_mssg_ct         = ct_vlch_mssg_ct.

**** sixth step: perform rollback work

        ROLLBACK WORK.

* --> Rollback in other systems which are concerned

        LOOP AT lt_rfcdest INTO ls_rfcdest.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            DESTINATION ls_rfcdest.

        ENDLOOP.


*--> seventh step: set the error flag
        MOVE gc_flag_3 TO ls_vlcdiavehi-xerror.                     "1=Green,2=Yellow,3=red
        MODIFY ct_vlcdiavehi FROM ls_vlcdiavehi TRANSPORTING xerror
                             WHERE vguid = ls_vlcdiavehi-vguid.

        APPEND ls_vlcdiavehi TO ct_vlcdiavehi_no.
        lv_err = abap_true.                                         "Atleast one vehicle in error..
      ENDIF.
*--> dequeue the vehicle
      CALL FUNCTION 'DEQUEUE_E_VLCVEHICLE'
        EXPORTING
          mode_vlcvehicle = 'E'
          mandt           = sy-mandt
          vguid           = ls_vlcdiavehi-vguid
          x_vguid         = ' '
          _scope          = '1'
          _synchron       = ' '
          _collect        = ' '.
    ENDLOOP.

*--> Raise error message
    IF lv_succ IS NOT INITIAL AND lv_err IS NOT INITIAL.
      MESSAGE e033(velo) WITH is_incoming_action-aktiont RAISING action_not_compl_performed.
    ELSEIF lv_succ IS INITIAL AND lv_err IS NOT INITIAL.
      MESSAGE e033(velo) WITH is_incoming_action-aktiont RAISING action_not_performed.
    ENDIF.


*      CALL FUNCTION 'VELO03_FILL_ERROR_TABLE'
*           EXPORTING
*                msgid_iv     =
*                msgty_iv     =
*                msgno_iv     =
*                msgv1_iv     =
*           TABLES
*                vlch_mssg_ct = vlch_mssg_ct.


*     Update flag for all the vehicles.
*        MOVE flag_3_gc TO vlcdiavehi_ls-xerror."1=Green,2=Yellow,3=red
*        MODIFY vlcdiavehi_ct FROM vlcdiavehi_ls TRANSPORTING xerror
*                                    WHERE xerror = space.
*
**     Vehicle Isolation not allowed for action &1.
*        MESSAGE e982() WITH incoming_action_is-aktiont
*        RAISING action_not_performed.

  ENDMETHOD.


METHOD set_action.
    DATA: ls_vehiactiondata_head  TYPE bapivehiactiondata_head,
          lt_config_ref           TYPE TABLE OF bapiconfig_references,
          lt_config_data          TYPE TABLE OF bapicucfg,
          lt_config_instance      TYPE TABLE OF bapicuins,
          lt_charsvalues          TYPE TABLE OF bapicuval,
          ls_vehicle_id_type      TYPE bapivehicle_id_type.


    ls_vehicle_id_type-vehicle_id_type = gc_guid.


*** To execute action on the selected GUIDs.
    CALL FUNCTION 'BAPI_VEHICLE_CHANGE_MULTIPLE'
      EXPORTING
        vehicleaction        = iv_action "Action to execute
        vehiactiondata_head  = ls_vehiactiondata_head
        vehicle_id_type      = ls_vehicle_id_type "G
      tables
        vehiactiondata_item  = it_guids "GUIDs
        config_references    = lt_config_ref
        config_data          = lt_config_data
        config_instances     = lt_config_instance
        config_charsvalues   = lt_charsvalues
        return               = et_return.

  ENDMETHOD.
ENDCLASS.
