class ZCL_SD_BRIDGESTONE_INTF definition
  public
  final
  create public .

public section.

  constants GC_ORDER_REASON type CHAR3 value '153' ##NO_TEXT.
  constants GC_MVMT_TYPE_161 type BWART value '161' ##NO_TEXT.
  constants GC_ITEMCAT type CHAR1 value 'S' ##NO_TEXT.
  constants GC_ACCTAASCAT type CHAR1 value 'W' ##NO_TEXT.
  constants GC_PRICING_TYPE type KNPRS value 'C' ##NO_TEXT.
  constants GC_CURRENCY type WAERS value 'AUD' ##NO_TEXT.
  constants GC_CONDITION_TYPE type KSCHA value 'PMP0' ##NO_TEXT.
  data GS_RECORD type ZSO2C_BRIDGESTONE_INT .
  data GT_ITEM_TEMP type ZTO2C_BRIDGESTONE_INT .
  constants GC_OBJECT type BALOBJ_D value 'ZINT' ##NO_TEXT.
  constants GC_EXTID type BALNREXT value 'RECEIVE_BRIDGESTONE' ##NO_TEXT.
  constants GC_SUB_OBJECT type BALSUBOBJ value 'RECEIVE_BRIDGESTONE' ##NO_TEXT.
  constants GC_EXCEP_TEXT type STRING value 'An Exception has occured. For further details go to SLG1 application log with external id "BRIDGSTONE_INT".' ##NO_TEXT.
  constants GC_SEVERITY type STRING value 'High' ##NO_TEXT.
  data GV_CREDIT_MEMO type CHAR1 .
  constants GC_CREDIT_MEMO type AUART value 'CR' ##NO_TEXT.
  data GS_BST_RETURN type ZMMC_BST_RETURN .
  constants GC_TRUE_X type BAPIFLAG-BAPIFLAG value 'X' ##NO_TEXT.
  constants GC_FAIL_E type CHAR1 value 'E' ##NO_TEXT.
  constants GC_PASS_S type CHAR1 value 'S' ##NO_TEXT.

  methods ON_PROXY_INVOKE
    exporting
      !ES_EXCEPTION type ZEXCHANGE_FAULT_DATA3
      !ET_RETURN type BAPIRET2_TAB .
  methods CONSTRUCTOR
    importing
      !IT_INPUT type ZMT_BRIDGE_STONE_INVOICE1 .
protected section.
private section.

  constants GC_MVT_IND type KZBEW value 'B' ##NO_TEXT.
  constants GC_GOODSMVMT_CODE type GM_CODE value '01' ##NO_TEXT.
  constants GC_AUART type AUART value 'ZDLY' ##NO_TEXT.
  constants GC_VKORG type VKORG value '1000' ##NO_TEXT.
  constants GC_VTWEG type VTWEG value '10' ##NO_TEXT.
  constants GC_SPART type SPART value '30' ##NO_TEXT.
  constants GC_MVMT_TYPE type BWART value '101' ##NO_TEXT.
  constants GC_MSG_CLASS type SYMSGID value 'ZO2C_01' ##NO_TEXT.
  constants GC_PARVW_AG type PARVW value 'AG' ##NO_TEXT.
  constants GC_MSG_TYPES type CHAR3 value 'WIS' ##NO_TEXT.
  data GT_INPUT type ZMT_BRIDGE_STONE_INVOICE1 .

  methods CREATE_SO
    exporting
      !ET_SO_RETURN type BAPIRET2_TAB
      !EV_VBELN type VBELN .
  methods SET_DATA
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  methods CREATE_PO
    importing
      !IV_VBELN type VBELN
    exporting
      !ET_PO_RETURN type BAPIRET2_TAB
      !EV_EBELN type EBELN .
  methods POST_GOODS_RECIEPT
    importing
      !IV_EBELN type EBELN
    exporting
      !ET_GR_RETURN type BAPIRET2_TAB
      !EV_MBLNR type MBLNR .
ENDCLASS.



CLASS ZCL_SD_BRIDGESTONE_INTF IMPLEMENTATION.


  method CONSTRUCTOR.

*-->Map the incoming data to the class attribute
    gt_input = it_input.
  endmethod.


  METHOD create_po.

*----------------------------------------------------------------------*
* DESCRIPTION:
* Create PO for the PR generated
* AUTHOR: I064763
*----------------------------------------------------------------------*

    DATA: lv_vbeln     TYPE vbeln,
          lv_banfn     TYPE banfn,
          lt_lifnr     TYPE RANGE OF lifnr,
          lt_ebeln     TYPE RANGE OF ebeln,
          lt_pritem    TYPE TABLE OF bapieban,
          ls_poitem    TYPE bapimepoitem,
          lt_poitem    TYPE TABLE OF bapimepoitem,
          lt_poitemx   TYPE TABLE OF bapimepoitemx,
          ls_poheaderx TYPE bapimepoheaderx,
          ls_poheader  TYPE bapimepoheader.

    CLEAR:ev_ebeln, et_po_return.

*Checking credit memo indicator if yes then create PO directly
    IF gv_credit_memo = abap_true.

*Checking if po already exists for the invoice number
      SELECT ebeln
        FROM ekko
        INTO ev_ebeln
        UP TO 1 ROWS
        WHERE ebeln IN lt_ebeln
          AND ihrez = gs_record-lfbnr.
      ENDSELECT.
      IF sy-subrc = 0 AND ev_ebeln IS NOT INITIAL.
        et_po_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                             id         = gc_msg_class
                                             number     = '006'
                                             message_v1 = ev_ebeln
                                             message_v2 = iv_vbeln ) ).
        RETURN.
      ENDIF.

      IF gs_bst_return IS INITIAL.
        SELECT mandt,
               lifnr,
               bsart,
               werks,
               bwart,
               ekorg,
               ekgrp
          FROM zmmc_bst_return
          INTO @DATA(ls_bst_return)
          UP TO 1 ROWS
          WHERE lifnr IN @lt_lifnr.
        ENDSELECT.
        IF sy-subrc = 0.
          gs_bst_return = ls_bst_return.
        ENDIF.
      ELSE.
        ls_bst_return = gs_bst_return.
      ENDIF.
*Filling Header Details
      ls_poheader-doc_type = ls_bst_return-bsart.
      ls_poheader-purch_org = ls_bst_return-ekorg.
      ls_poheader-pur_group = ls_bst_return-ekgrp.
      ls_poheader-vendor = ls_bst_return-lifnr.
      ls_poheader-ref_1      = gs_record-lfbnr.
*Conversion Exit
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = iv_vbeln
        IMPORTING
          output = lv_vbeln.
      ls_poheader-our_ref    = lv_vbeln.

      ls_poheaderx-doc_type  = abap_true.
      ls_poheaderx-purch_org = abap_true.
      ls_poheaderx-pur_group = abap_true.
      ls_poheaderx-vendor    = abap_true.
      ls_poheaderx-ref_1     = abap_true.
      ls_poheaderx-our_ref   = abap_true.

**Item details
      DATA(lv_item_num) = 00000.
      LOOP AT gt_item_temp INTO DATA(ls_temp).
        lv_item_num = lv_item_num + 10.
        ls_poitem-po_item   = lv_item_num.
        ls_poitem-material  = ls_temp-matnr.
        ls_poitem-plant     = ls_bst_return-werks.
        ls_poitem-quantity  = abs( ls_temp-kwmeng ).
        ls_poitem-net_price = ls_temp-netpr.
        ls_poitem-item_cat   = gc_itemcat.
        ls_poitem-acctasscat = gc_acctaascat.
        ls_poitem-customer  = ls_temp-kunnr.
        ls_poitem-ret_item  = abap_true.
        ls_poitem-po_price  = 1.
        APPEND ls_poitem TO lt_poitem.
        CLEAR ls_poitem.
      ENDLOOP.

*Itemx details
      lt_poitemx = VALUE #( FOR ls_poitem1 IN lt_poitem ( po_item   = ls_poitem1-po_item
                                                         material  = abap_true
                                                         plant     = abap_true
                                                         quantity  = abap_true
                                                         net_price = abap_true
                                                         item_cat  = abap_true
                                                         acctasscat = abap_true
                                                         customer   = abap_true
                                                         ret_item   = abap_true
                                                         po_price  = abap_true ) ) .

    ELSE.
*--> Get the PR number generated for the SO
*--> Select restricted to 1 row as only 1 PR will be generated per SO
      SELECT single banfn
        FROM vbep
        INTO lv_banfn
        WHERE vbeln = iv_vbeln
          AND posnr = '10'.

*      SELECT banfn
*  UP TO 1 ROWS
*  FROM vbep
*  INTO @DATA(lv_banfn)
*  WHERE vbeln = @iv_vbeln.
*ENDSELECT.

      IF sy-subrc <> 0 OR lv_banfn IS INITIAL.
        et_po_return = VALUE bapiret2_tab( ( type       = gc_fail_e
                                             id         = gc_msg_class
                                             number     = '004'
                                             message_v1 = iv_vbeln ) ).
        RETURN.
      ENDIF.

*--> Get the details of the PR
      CALL FUNCTION 'BAPI_REQUISITION_GETDETAIL'
        EXPORTING
          number            = lv_banfn
        TABLES
          requisition_items = lt_pritem.

*--> Check if a PO exists for the PR
      TRY .
          DATA(ls_pritem_t) = lt_pritem[ 1 ].
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.

*--> Carry out the further processing only if the PO is not created; else skip to GR.
      IF ls_pritem_t-po_number IS NOT INITIAL.
*--> Export the PO number
        ev_ebeln = ls_pritem_t-po_number.
        et_po_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                             id         = gc_msg_class
                                             number     = '006'
                                             message_v1 = ls_pritem_t-po_number
                                             message_v2 = iv_vbeln ) ).
        RETURN.
      ENDIF.
*--> fill the item details for the PO
      lt_poitem = VALUE #( FOR ls_pritem IN lt_pritem ( po_item   = ls_pritem-preq_item
                                                        preq_item = ls_pritem-preq_item
                                                        preq_no   = lv_banfn
                                                        po_price  = 1
                                                      ) ).

*--> Update the unit price from the incoming file
      LOOP AT lt_poitem ASSIGNING FIELD-SYMBOL(<ls_item>).
        TRY .
            DATA(ls_pritem_temp) = lt_pritem[  preq_item = <ls_item>-po_item ].
            DATA(ls_file_item) = gt_item_temp[ matnr = ls_pritem_temp-material ].
            <ls_item>-net_price = ls_file_item-netpr.
          CATCH cx_sy_itab_line_not_found .             "#EC NO_HANDLER
        ENDTRY.
      ENDLOOP.
      UNASSIGN <ls_item>.

*--> fill the update indicators
      lt_poitemx = VALUE #( FOR ls_pritem IN lt_pritem (  po_item   = ls_pritem-preq_item
                                                          preq_item = abap_true
                                                          preq_no   = abap_true
                                                          net_price = abap_true
                                                          po_price  = abap_true
                                                        ) ).
*Header details
      ls_poheader-ref_1 = gs_record-lfbnr.
      ls_poheaderx-ref_1 = abap_true.
    ENDIF.
*--> Call the bapi for PO creation
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_poheader
        poheaderx        = ls_poheaderx
        no_price_from_po = gc_true_x     "'X'
      IMPORTING
        exppurchaseorder = ev_ebeln
      TABLES
        return           = et_po_return
        poitem           = lt_poitem
        poitemx          = lt_poitemx.

*--> Check processing status
    DELETE et_po_return WHERE type CA gc_msg_types.
    IF NOT et_po_return IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
*--> Add the SO number message to return structure
      IF NOT ev_ebeln  IS INITIAL.
        et_po_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                             id         = gc_msg_class
                                             number     = '010'
                                             message_v1 = ev_ebeln
                                             message_v2 = iv_vbeln ) ).
      ENDIF.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD create_so.

*----------------------------------------------------------------------*
*
* DESCRIPTION:
* Create SO for the incoming details
* AUTHOR: I064763
*----------------------------------------------------------------------*

    DATA: ls_order_header_in      TYPE bapisdhd1,
          ls_order_header_inx     TYPE bapisdhd1x,
          lt_order_items_in       TYPE TABLE OF bapisditm,
          lt_order_items_inx      TYPE TABLE OF bapisditmx,
          lt_order_schedules_in   TYPE TABLE OF bapischdl,
          ls_order_schedules_in   TYPE bapischdl,
          lt_order_schedules_inx  TYPE TABLE OF bapischdlx,
          ls_order_schedules_inx  TYPE bapischdlx,
          lt_order_partner        TYPE TABLE OF bapiparnr,
          ls_order_items_in       TYPE bapisditm,
          ls_order_items_inx      TYPE bapisditmx,
          ls_order_partner        TYPE bapiparnr,
          ls_logic_switch         TYPE bapisdls,
          lt_order_conditions_in  TYPE TABLE OF bapicond,
          ls_order_conditions_in  TYPE bapicond,
          lt_order_conditions_inx TYPE TABLE OF bapicondx,
          ls_order_conditions_inx TYPE bapicondx,
          lv_item_num             TYPE posnr_va VALUE '0010',
*CR018
          ls_head                 TYPE bapisdhead1,
          ls_headx                TYPE bapisdhead1x,
          lt_partner              TYPE STANDARD TABLE OF bapipartnr,
          lt_item                 TYPE STANDARD TABLE OF bapisditem,
          lt_itemx                TYPE STANDARD TABLE OF bapisditemx,
          lt_cond                 TYPE STANDARD TABLE OF bapicondition.

    CLEAR: ev_vbeln, et_so_return.

*--> check whether the SO already exists or not
    SELECT vbeln,
           auart
      UP TO 1 ROWS
      FROM vbak
      INTO ( @ev_vbeln, @DATA(lv_auart) )
      WHERE audat = @gs_record-audat
       AND  ihrez = @gs_record-lfbnr
       AND  kunnr = @gs_record-kunnr.
    ENDSELECT.
    IF sy-subrc = 0.
*Order type conversion exit
      CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
        EXPORTING
          input  = gc_credit_memo
        IMPORTING
          output = ls_head-doc_type.
*Checking the sales order type as CR or not
      IF lv_auart = ls_head-doc_type.
        gv_credit_memo = abap_true.
      ENDIF.

      et_so_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                           id         = gc_msg_class
                                           number     = '005'
                                           message_v1 = ev_vbeln
                                           message_v2 = gs_record-kunnr
                                           message_v3 = gs_record-lfbnr ) ).
      RETURN.
    ELSE.

*-->Fill the header details
      ls_order_header_in-doc_type   = gc_auart.
      ls_order_header_in-sales_org  = gc_vkorg.
      ls_order_header_in-distr_chan = gc_vtweg.
      ls_order_header_in-division   = gc_spart.
      ls_order_header_in-purch_no_c = gs_record-bstkd.
      ls_order_header_in-doc_date   = gs_record-audat.
      ls_order_header_in-ref_1      = gs_record-lfbnr.

*-->Fill the update indicator
      ls_order_header_inx-doc_type   = abap_true.
      ls_order_header_inx-sales_org  = abap_true.
      ls_order_header_inx-distr_chan = abap_true.
      ls_order_header_inx-division   = abap_true.
      ls_order_header_inx-purch_no_c = abap_true.
      ls_order_header_inx-doc_date   = abap_true.
      ls_order_header_inx-ref_1      = abap_true.

      LOOP AT gt_item_temp INTO DATA(ls_item).

*--> Fill the item details
        ls_order_items_in-itm_number = lv_item_num.
        ls_order_items_in-material   = ls_item-matnr.
        ls_order_items_in-short_text = ls_item-arktx.
*CR18
*Checking negative quantity if yes then change the order type to CR
        IF ls_item-kwmeng < 0.
          gv_credit_memo = abap_true.
        ENDIF.
        ls_order_items_in-target_qty = abs( ls_item-kwmeng ).
        ls_order_items_in-purch_no_c = ls_item-bstkd.
        APPEND ls_order_items_in TO lt_order_items_in.

*--> Fill the item update indicator
        ls_order_items_inx-itm_number = lv_item_num.
        ls_order_items_inx-material   = abap_true.
        ls_order_items_inx-short_text = abap_true.
        ls_order_items_inx-target_qty = abap_true.
        ls_order_items_inx-purch_no_c = abap_true.
        APPEND ls_order_items_inx TO lt_order_items_inx.

*--> fill schedule lines
        ls_order_schedules_in-itm_number = lv_item_num.
        ls_order_schedules_in-sched_line = sy-tabix.
        ls_order_schedules_in-req_qty    = abs( ls_item-kwmeng ).
        APPEND ls_order_schedules_in TO lt_order_schedules_in.

*--> fill update flags for schedule lines
        ls_order_schedules_inx-itm_number = lv_item_num.
        ls_order_schedules_inx-sched_line = sy-tabix.
        ls_order_schedules_inx-req_qty    = abap_true.
        ls_order_schedules_inx-updateflag = abap_true.
        APPEND ls_order_schedules_inx TO lt_order_schedules_inx.

*--> Update the conditions for manual pricing
        TRY.
            DATA(ls_file_item) = gt_item_temp[ matnr = ls_item-matnr ].
            ls_order_conditions_in-itm_number  = lv_item_num.
            ls_order_conditions_in-cond_type   = gc_condition_type.
            ls_order_conditions_in-cond_value  = ls_file_item-netpr.
            ls_order_conditions_in-currency    = gc_currency.
            ls_order_conditions_in-cond_updat  = abap_true.
            APPEND ls_order_conditions_in TO lt_order_conditions_in.

*--> Update the update flags for manual pricing
            ls_order_conditions_inx-itm_number  = lv_item_num.
            ls_order_conditions_inx-cond_type   = gc_condition_type.
            ls_order_conditions_inx-updateflag  = abap_true.
            ls_order_conditions_inx-cond_value  = abap_true.
            ls_order_conditions_inx-currency    = abap_true.
            APPEND ls_order_conditions_inx TO lt_order_conditions_inx.

          CATCH cx_sy_itab_line_not_found.              "#EC NO_HANDLER
        ENDTRY.

*--> increment the counter for item number by 10 for each iteration
        lv_item_num = lv_item_num + 10.

        CLEAR: ls_order_items_in,ls_order_items_inx,ls_order_schedules_in,ls_order_schedules_inx,ls_item, ls_order_conditions_in, ls_order_conditions_inx.
      ENDLOOP.

*--> fill partner details for the header level
      ls_order_partner-partn_role = gc_parvw_ag.

      ls_order_partner-partn_numb = gs_record-kunnr.

      APPEND ls_order_partner TO lt_order_partner.

*--> Set the pricing type
      ls_logic_switch-pricing = gc_pricing_type.

*--> Call the BAPI for SO creation
*CR18
      IF gv_credit_memo IS INITIAL.
        CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
          EXPORTING
            order_header_in      = ls_order_header_in
            order_header_inx     = ls_order_header_inx
            logic_switch         = ls_logic_switch
          IMPORTING
            salesdocument        = ev_vbeln
          TABLES
            return               = et_so_return
            order_items_in       = lt_order_items_in
            order_items_inx      = lt_order_items_inx
            order_partners       = lt_order_partner
            order_schedules_in   = lt_order_schedules_in
            order_schedules_inx  = lt_order_schedules_inx
            order_conditions_in  = lt_order_conditions_in
            order_conditions_inx = lt_order_conditions_inx.
      ELSE.
        CLEAR:ls_head,ls_headx,lt_item,lt_itemx,lt_partner,lt_cond.
*Move related values to our structure
        ls_head    = CORRESPONDING #( ls_order_header_in ).
        ls_head-ord_reason = gc_order_reason."'153'. "Order reason
*Order type conversion exit
        CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
          EXPORTING
            input  = gc_credit_memo
          IMPORTING
            output = ls_head-doc_type.

        ls_headx   = CORRESPONDING #( ls_order_header_inx ).
        ls_headx-ord_reason = abap_true. "Order reason
        lt_item    = CORRESPONDING #( lt_order_items_in ).
        lt_itemx   = CORRESPONDING #( lt_order_items_inx ).
        lt_partner = CORRESPONDING #( lt_order_partner ).
        lt_cond    = CORRESPONDING #( lt_order_conditions_in ).

*Creating sales order with type CR( credit memo request )
*--Unreleased FM is used as 'BAPI_SALESORDER_CREATEFROMDAT2' works only
*--for certain Business object and Sales doc. Category.
        CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA1'
          EXPORTING
            sales_header_in     = ls_head
            sales_header_inx    = ls_headx
          IMPORTING
            salesdocument_ex    = ev_vbeln
          TABLES
            return              = et_so_return
            sales_items_in      = lt_item
            sales_items_inx     = lt_itemx
            sales_partners      = lt_partner
            sales_conditions_in = lt_cond.
      ENDIF.

*--> Check processing status
      DELETE et_so_return WHERE type CA gc_msg_types.
      IF NOT et_so_return IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
*--> Add the SO number message to return structure
        IF NOT ev_vbeln  IS INITIAL.
          et_so_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                               id         = gc_msg_class
                                               number     = '009'
                                               message_v1 = ev_vbeln
                                               message_v2 = gs_record-bstkd ) ).
        ENDIF.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD on_proxy_invoke.

*--> Call the method to process the data.
    CLEAR: et_return,es_exception.
    me->set_data( IMPORTING et_return   = et_return ).

*--> Log the messages to the application log
    IF et_return IS NOT INITIAL.
      DATA(lo_util) = NEW zcl_h1s_generic_utils( ).
      IF lo_util IS BOUND.
        lo_util->set_bal_log( EXPORTING  iv_extid       = me->gc_extid
                                         iv_object      = me->gc_object
                                         iv_sub_object  = me->gc_sub_object
                                         it_msg_tab     = et_return
                                         iv_commit_flag = abap_true
                              EXCEPTIONS "cx_alert      = 1
                                         OTHERS        = 2 ).

        IF sy-subrc <> 0.
          MESSAGE s025(zp2p) DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

*--> Raise exception if any error messages are there.
      IF line_exists( et_return[ type = gc_fail_e ] ).
        es_exception-fault_text   = gc_excep_text.
        es_exception-fault_detail = VALUE zexchange_log_data_tab3( ( severity = gc_severity ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD post_goods_reciept.

*----------------------------------------------------------------------*
* DESCRIPTION:
* Post the Goods Reciept for the PO
* AUTHOR: I064763
*----------------------------------------------------------------------*

    DATA: ls_po_header     TYPE bapiekkol,
          lt_po_items      TYPE TABLE OF bapiekpo,
          ls_header        TYPE bapi2017_gm_head_01,
          ls_goodsmvt_code TYPE bapi2017_gm_code,
          lt_item          TYPE TABLE OF  bapi2017_gm_item_create,
          lt_po_item_hist  TYPE TABLE OF bapiekbe.

    CLEAR: et_gr_return,ev_mblnr.

    IF NOT iv_ebeln IS INITIAL.

*-->Get the details for the PO
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder   = iv_ebeln
          items           = abap_true
          history         = abap_true
        IMPORTING
          po_header       = ls_po_header
        TABLES
          po_items        = lt_po_items
          po_item_history = lt_po_item_hist.

*--> Check if the GR has already been done for the PO; if so exit and  do nothing
      SORT lt_po_item_hist BY mat_doc DESCENDING.
      TRY .
          DATA(ls_po_item_hist) = lt_po_item_hist[ 1 ].
        CATCH cx_sy_itab_line_not_found.                "#EC NO_HANDLER
      ENDTRY.

*--> Carry out the further processing only if the GR is not complete
      IF ls_po_item_hist-mat_doc IS NOT INITIAL AND ( ls_po_item_hist-move_type = gc_mvmt_type OR
                                                      ls_po_item_hist-move_type = gc_mvmt_type_161 ).
*--> Export the Material document number and populate the messages
        ev_mblnr = ls_po_item_hist-mat_doc.
        et_gr_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                             id         = gc_msg_class
                                             number     = '007'
                                             message_v1 = iv_ebeln
                                             message_v2 = ev_mblnr ) ).
        RETURN.
      ENDIF.

*--> Fill Header details
      ls_header-doc_date   = gs_record-bldat.
      ls_header-pstng_date = sy-datum.
      ls_header-ref_doc_no = gs_record-lfbnr.

*--> Fill item level details
      lt_item = VALUE #( FOR ls_poitem IN lt_po_items (  material   = ls_poitem-material
                                                         entry_qnt  = ls_poitem-quantity
                                                         entry_uom  = ls_poitem-unit
                                                         po_number  = ls_po_header-po_number
                                                         po_item    = ls_poitem-po_item
                                                         move_type  = gc_mvmt_type
                                                         mvt_ind    = gc_mvt_ind ) ).

*--> Call the bapi for GR creation
      ls_goodsmvt_code-gm_code = gc_goodsmvmt_code.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_header
          goodsmvt_code    = ls_goodsmvt_code
        IMPORTING
          materialdocument = ev_mblnr
        TABLES
          goodsmvt_item    = lt_item
          return           = et_gr_return.

      DELETE et_gr_return WHERE type CA gc_msg_types.
      IF NOT et_gr_return IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        et_gr_return = VALUE bapiret2_tab( ( type       = gc_pass_s
                                             id         = gc_msg_class
                                             number     = '008'
                                             message_v1 = ev_mblnr
                                             message_v2 = iv_ebeln ) ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_data.
*----------------------------------------------------------------------*
*
* DESCRIPTION:
* The incoming records are mapped to the corresponding SAP structure
* AUTHOR: I064763
*----------------------------------------------------------------------*

    CLEAR:et_return,gs_record.

    READ TABLE gt_input-mt_bridge_stone_invoice-recordset INTO DATA(ls_recordset) INDEX 1.
    IF sy-subrc = 0.
      DATA(lt_data) = ls_recordset-item[].
      gt_item_temp   = CORRESPONDING #( lt_data     MAPPING  kunnr  = dealer_number
                                                             audat  = date_ordered
                                                             lfbnr  = bs_invoice_number
                                                             bstkd  = dealer_order_ref
                                                             bldat  = date_despacthed
                                                             matnr  = part_number
                                                             arktx  = description
                                                             kwmeng = quantity_picked
                                                             netpr  = unit_price ).
*--> conversion exit for material
      LOOP AT gt_item_temp ASSIGNING FIELD-SYMBOL(<ls_item>).
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = <ls_item>-matnr
          IMPORTING
            output       = <ls_item>-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
          CLEAR <ls_item>-matnr.
        ENDIF.

*Conversion exit
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <ls_item>-kunnr
          IMPORTING
            output = <ls_item>-kunnr.

      ENDLOOP.
*--> Read the first record into global structure to map the header-level details at each stage
      TRY .
          gs_record = gt_item_temp[ 1 ].
          gs_record-audat = gs_record-audat+6(4) && gs_record-audat+3(2) && gs_record-audat+0(2).
          gs_record-bldat = gs_record-bldat+6(4) && gs_record-bldat+3(2) && gs_record-bldat+0(2).
        CATCH cx_sy_itab_line_not_found.               "#EC NO_HANDLER.
      ENDTRY.

*Clearing Credit memo indicator
      CLEAR:gv_credit_memo.

*--> call the method for SO creation
      me->create_so( IMPORTING et_so_return = DATA(lt_return)
                               ev_vbeln     = DATA(lv_vbeln) ).
      APPEND LINES OF lt_return TO et_return.
      CLEAR lt_return.

*--> Call the method for PO creation if the SO is available
      IF lv_vbeln IS NOT INITIAL.
        me->create_po( EXPORTING iv_vbeln     = lv_vbeln
                       IMPORTING et_po_return = lt_return
                                 ev_ebeln     = DATA(lv_ebeln) ).
        APPEND LINES OF lt_return TO et_return.
        CLEAR lt_return.

*--> Call the method for GR if po was created
        IF lv_ebeln IS NOT INITIAL.
          me->post_goods_reciept( EXPORTING iv_ebeln     = lv_ebeln
                                  IMPORTING et_gr_return = lt_return
                                            ev_mblnr     = DATA(lv_mblnr) ).
          APPEND LINES OF lt_return TO et_return.
          CLEAR lt_return.
          IF lv_mblnr IS NOT INITIAL.
            CLEAR lv_mblnr.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: gt_item_temp.
**  ENDAT.
      CLEAR: lv_vbeln,lv_ebeln, gs_record.
**    ENDLOOP.
    ENDIF.
**    UNASSIGN <ls_item>.
  ENDMETHOD.
ENDCLASS.
