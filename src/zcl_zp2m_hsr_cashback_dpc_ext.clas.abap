class ZCL_ZP2M_HSR_CASHBACK_DPC_EXT definition
  public
  inheriting from ZCL_ZP2M_HSR_CASHBACK_DPC
  create public .

public section.
protected section.

  methods PROCESSPAYMENTSE_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZP2M_HSR_CASHBACK_DPC_EXT IMPLEMENTATION.


  METHOD processpaymentse_update_entity.

    DATA: ls_data TYPE zsp2m_bshbk_hsr_data.

*-->Read the Entity
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

*--> Process the data
    DATA(lo_obj) = NEW zcl_p2m_cashback_payment( is_cshbk_hsr_data = ls_data ).
    CHECK lo_obj IS BOUND.

    lo_obj->get_data( iv_cshbk_hsr = ls_data-flag ).
    ls_data-referencekey = lo_obj->gs_cshbk_hsr_data-referencekey.
*--> Return the messages
    LOOP AT lo_obj->gt_return ASSIGNING FIELD-SYMBOL(<ls_bapit_ret>).
      /iwbep/if_mgw_conv_srv_runtime~get_message_container( )->add_message(
              EXPORTING
                iv_msg_type               = <ls_bapit_ret>-type
                iv_msg_id                 = <ls_bapit_ret>-id
                iv_msg_number             = <ls_bapit_ret>-number
                iv_msg_text               = <ls_bapit_ret>-message
                iv_add_to_response_header = abap_true
            ).
      EXIT.
    ENDLOOP.

    er_entity = ls_data.
  ENDMETHOD.
ENDCLASS.
