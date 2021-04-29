class ZCL_FEH_HM_BACK_ORDER_CONF definition
  public
  create public .

public section.

  interfaces IF_ECH_ACTION .

  class-methods CREATE
    returning
      value(RO_FEH) type ref to ZCL_FEH_HM_BACK_ORDER_CONF .
  methods EXECUTE
    importing
      !IO_BACK_ORDER type ref to ZCL_MM_BACK_ORDER_CONF
      !IO_REF_REGISTRATION type ref to CL_FEH_REGISTRATION optional
    raising
      resumable(ZCX_STANDARD_MESSAGE_FAULT1)
      CX_AI_SYSTEM_FAULT .
protected section.

  class-data GO_ECH_ACTION type ref to ZCL_FEH_HM_BACK_ORDER_CONF .
private section.
ENDCLASS.



CLASS ZCL_FEH_HM_BACK_ORDER_CONF IMPLEMENTATION.


METHOD CREATE.
    IF NOT go_ech_action IS BOUND.
      CREATE OBJECT go_ech_action.
    ENDIF.
    ro_feh = go_ech_action.
  ENDMETHOD.


METHOD execute.
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*DESCRIPTION : This method process the FEH and Proxy Class
* AUTHOR     : C5223321
* DATE       : 11/09/2017
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*   Data Definition
    DATA:
      lv_objkey       TYPE ech_dte_objkey,
      ls_bapiret2     TYPE bapiret2,
      ls_standarddata TYPE zexchange_fault_data,
      ls_logdata      TYPE zexchange_log_data,
      lo_fehreg       TYPE REF TO cl_feh_registration.

*   Local Constants
    CONSTANTS:
      lc_high      TYPE c LENGTH 4             VALUE 'HIGH',
      lc_errcat    TYPE ech_dte_error_category VALUE 'CON',
      lc_objcat    TYPE ech_dte_objcat         VALUE '1',
      lc_objtype   TYPE ech_dte_objtype        VALUE 'ZOBJTY_BOC',
      lc_component TYPE ech_dte_component      VALUE 'ZFEH_BACKORDERCONF',
      lc_process   TYPE ech_dte_process        VALUE 'ZFEH_BACKORDERCONF_BP'.

***************************************************************
** Process the Order Confirmations
***************************************************************
    TRY.
        io_back_order->on_proxy_invoke( ).

      CATCH zcx_mm_back_ord_exp ##NO_HANDLER.

****************************************************************
** Log the errors to FEH
****************************************************************
        DATA(ls_object) = VALUE ech_str_object( objcat  = 1
                                              objtype = lc_objtype
                                              objkey  = io_back_order->gv_feh_key ).

        READ TABLE io_back_order->gt_appl_log INTO ls_bapiret2 INDEX 1.
        IF sy-subrc = 0. "Will always be

          TRY.
              io_ref_registration->collect( i_single_bo      = io_back_order->gs_feh_payload
                                            i_component      = lc_component
                                            i_process        = lc_process
                                            i_error_category = lc_errcat
                                            i_main_message   = ls_bapiret2
                                            i_messages       = io_back_order->gt_appl_log
                                            i_main_object    = ls_object ).

            CATCH cx_ai_system_fault INTO DATA(ls_fault).
              RAISE EXCEPTION ls_fault.
          ENDTRY.
        ENDIF.

        RAISE EXCEPTION TYPE zcx_standard_message_fault1.
    ENDTRY.

  ENDMETHOD.


METHOD IF_ECH_ACTION~FAIL.
    CALL METHOD cl_feh_registration=>s_fail
      EXPORTING
        i_data             = i_data
      IMPORTING
        e_execution_failed = e_execution_failed
        e_return_message   = e_return_message.
  ENDMETHOD.


METHOD IF_ECH_ACTION~FINALIZE_AFTER_RETRY_ERROR ##NEEDED.
  ENDMETHOD.


METHOD IF_ECH_ACTION~FINISH.
    CALL METHOD cl_feh_registration=>s_finish
      EXPORTING
        i_data             = i_data
      IMPORTING
        e_execution_failed = e_execution_failed
        e_return_message   = e_return_message.
  ENDMETHOD.


METHOD IF_ECH_ACTION~NO_ROLLBACK_ON_RETRY_ERROR ##NEEDED.
  ENDMETHOD.


METHOD if_ech_action~retry.
    DATA: lo_feh          TYPE REF TO cl_feh_registration,
          lt_post_mapping TYPE ZSMM_BACK_ORDER_PAYLOAD.

    CLEAR:e_execution_failed,
          e_return_message.

*************************************
* Get reprocessing FEH Instance
*************************************
    lo_feh = cl_feh_registration=>s_retry( i_error_object_id = i_error_object_id ).

*************************************
* Retrieve reprocessed content
*************************************
    lo_feh->retrieve_data(
     EXPORTING
       i_data = i_data
     IMPORTING
       e_post_mapping_data = lt_post_mapping ).

    IF lt_post_mapping IS INITIAL.
      e_execution_failed = abap_true.
      e_return_message-type       = 'E'.
      e_return_message-id         = 'ZP2P'.
      e_return_message-number     = '453'.
      IF 1 = 2.
        MESSAGE e453(zvms).
      ENDIF.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------
*     Get the Preload for Processing Order Confirmations
*--------------------------------------------------------------------
    TRY.
        CALL METHOD zcl_mm_back_order_conf=>get_preload_confirmations
          EXPORTING
            io_data  = REF #( lt_post_mapping )
          IMPORTING
            et_order = DATA(lt_order_process).
      CATCH zcx_mm_back_ord_exp INTO DATA(lo_exp).
        READ TABLE lo_exp->gt_return INDEX 1 INTO DATA(ls_return).
        IF sy-subrc = 0.
*Raise error
          e_execution_failed = abap_true.
          e_return_message-type       = ls_return-type.
          e_return_message-id         = ls_return-id.
          e_return_message-number     = ls_return-NUMBER.
        ENDIF.
        RETURN.
    ENDTRY.


    TRY.
        DATA(lo_back_order_ref) = lt_order_process[ 1 ]-back_conf.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.  "Will never happen
        e_execution_failed = abap_true.
        e_return_message-type       = 'E'.
        e_return_message-id         = 'ZP2P'.
        e_return_message-number     = '453'.
        IF 1 = 2.
          MESSAGE e453(zp2p).
        ENDIF.
        RETURN.
    ENDTRY.

********************************************************************************
* Re-Process actions based external PO reference -
* ( Only one HPO will be reprocessed at a time )
********************************************************************************
    TRY.
        me->execute( io_ref_registration = lo_feh
                     io_back_order       = lo_back_order_ref ).

      CATCH cx_ai_system_fault ##NO_HANDLER.
        e_execution_failed = abap_true.
        e_return_message-type       = 'E'.
        e_return_message-id         = 'ZP2P'.
        e_return_message-number     = '457'.
        e_return_message-message_v1 = lo_back_order_ref->gv_feh_key.
        IF 1 = 2.
          MESSAGE e457(zp2p) WITH lo_back_order_ref->gv_feh_key.
        ENDIF.
        RETURN.

      CATCH zcx_standard_message_fault1 ##NO_HANDLER.
    ENDTRY.

    lo_feh->resolve_retry( IMPORTING e_execution_failed = e_execution_failed
                                     e_return_message   = e_return_message ).
  ENDMETHOD.


METHOD IF_ECH_ACTION~S_CREATE.
    r_action_class = create( ).
  ENDMETHOD.
ENDCLASS.
