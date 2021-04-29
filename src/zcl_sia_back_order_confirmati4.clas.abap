class ZCL_SIA_BACK_ORDER_CONFIRMATI4 definition
  public
  create public .

public section.

  interfaces ZII_SIA_BACK_ORDER_CONFIRMATI3 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SIA_BACK_ORDER_CONFIRMATI4 IMPLEMENTATION.


METHOD zii_sia_back_order_confirmati3~sia_back_order_confirmation62.
************************************************************************
*  Program Title       : Interface for Back Order Confirmation         *
*  Author              : Palta,Aniket                                  *
*  Developer           : Balasubramani Moorthy (C5223321)              *
*  WRICEF ID           : DS_I035                                       *
*  Description:        : Processing File 62                            *
*  Type:               : Proxy Interface program (Inbound)             *
*  Run Frequency       :                                               *
************************************************************************
* MODIFICATION HISTORY                                                 *
*  Change Request #    :                                               *
*  Author              :                                               *
*  Changed By          :                                               *
*  Modification Date   :                                               *
*  Description         :                                               *
************************************************************************
    SET UPDATE TASK LOCAL.

*Data Declarations
    DATA: ls_bapiret TYPE bapiret2,
          lt_bapiret TYPE bapiret2_t,
          lo_feh     TYPE REF TO cl_feh_registration.

*Get Proxy data from File
    DATA(ls_input)       = input-mt_back_order_confirmation62.
    DATA(ls_po_ord_conf) = ls_input-order_confirmation.

*"Blank Normal function
*'X', only create new materials and supercession rules.
*Do not update the purchase order or it's confirmations.
*    zcl_mm_back_order_conf=>gv_upd_mas_data = ls_po_ord_conf-update_master_data_only.

*--------------------------------------------------------------------
*    Map Input data
*--------------------------------------------------------------------
    TRY.
        zcl_mm_back_order_conf=>map_input( EXPORTING it_input = ls_po_ord_conf-item[]
                                                     iv_master_flag = ls_po_ord_conf-update_master_data_only
                                           IMPORTING et_order = DATA(lt_order) ).
      CATCH zcx_mm_back_ord_exp INTO DATA(lo_exp).
        cl_proxy_fault=>raise( exception_class_name = 'ZCX_STANDARD_MESSAGE_FAULT1'
                               bapireturn_tab       = lo_exp->gt_return
                               automatic_retry      = space ).
    ENDTRY.

*--------------------------------------------------------------------
*     Get the Preload for Processing Order Confirmations
*--------------------------------------------------------------------
    TRY.
        CALL METHOD zcl_mm_back_order_conf=>get_preload_confirmations
          EXPORTING
            io_data  = REF #( lt_order )
          IMPORTING
            et_order = DATA(lt_order_process).

      CATCH zcx_mm_back_ord_exp INTO lo_exp.

        cl_proxy_fault=>raise( exception_class_name = 'ZCX_STANDARD_MESSAGE_FAULT1'
                               bapireturn_tab       = lo_exp->gt_return
                               automatic_retry      = space ).

    ENDTRY.

*--------------------------------------------------------------------
*  Loop the Order Confirmation for FEH based on the External
*  PO reference
*--------------------------------------------------------------------
    LOOP AT lt_order_process INTO DATA(ls_order).

      TRY.
          IF lo_feh IS NOT BOUND.
            lo_feh = cl_feh_registration=>s_initialize( is_single = abap_false ).
          ENDIF.

        CATCH cx_ai_system_fault ##NO_HANDLER.

        "No FEH Instance , Terminate Immediately
        cl_proxy_fault=>raise( exception_class_name = 'ZCX_STANDARD_MESSAGE_FAULT1'
                               bapireturn_tab       = VALUE bapirettab( ( type = 'E' id = 'ZP2P' number = '454' ) )
                               automatic_retry      = space ).

          if 1 = 2.
            message e454(zp2p).
          endif.
      ENDTRY.
*--------------------------------------------------------------------
*    Call FEH
*--------------------------------------------------------------------
      TRY.
          zcl_feh_hm_back_order_conf=>create( )->execute( io_back_order       = ls_order-back_conf
                                                          io_ref_registration = lo_feh ).
        CATCH zcx_standard_message_fault1 ##NO_HANDLER.
          DATA(lv_error) = abap_true.

          MESSAGE e456(zp2p) WITH ls_order-back_conf->gv_feh_key INTO data(ls_message).
          ls_bapiret-type       = sy-msgty.
          ls_bapiret-id         = sy-msgid.
          ls_bapiret-number     = sy-msgno.
          ls_bapiret-message_v1 = sy-msgv1.
          APPEND ls_bapiret TO lt_bapiret.

        CATCH cx_ai_system_fault ##NO_HANDLER.
          lv_error = abap_true.
          MESSAGE e457(zp2p) WITH ls_order-back_conf->gv_feh_key INTO ls_message.
          ls_bapiret-type       = sy-msgty.
          ls_bapiret-id         = sy-msgid.
          ls_bapiret-number     = sy-msgno.
          ls_bapiret-message_v1 = sy-msgv1.
          APPEND ls_bapiret TO lt_bapiret.
      ENDTRY.
    ENDLOOP.

    IF lv_error = abap_true.
      lo_feh->if_ws_transaction_callback~callback( ).
      cl_proxy_fault=>raise( exception_class_name = 'ZCX_STANDARD_MESSAGE_FAULT1'
                             bapireturn_tab       = lt_bapiret
                             automatic_retry      = space ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
