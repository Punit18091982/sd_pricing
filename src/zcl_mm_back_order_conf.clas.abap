CLASS zcl_mm_back_order_conf DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      ty_item_t TYPE SORTED TABLE OF bapimepoitem WITH NON-UNIQUE KEY material_long .
    TYPES:
      ty_t_conf TYPE SORTED TABLE OF bapiekes WITH NON-UNIQUE KEY po_item .
    TYPES ebeln TYPE ebeln .
    TYPES:
      BEGIN OF ty_s_ekes_tab,
        ebeln	        TYPE ebeln,
        ebelp	        TYPE ebelp,
        etens	        TYPE etens,
        ebtyp	        TYPE ebtyp,
        eindt         TYPE bbein,
        menge	        TYPE bbmng,
*        zzunique_key1  TYPE zmm_uniquekey1,
*        zzunique_key2  TYPE zmm_uniquekey2,
*        zzunique_key3  TYPE zmm_uniquekey3,
        zzprocessdate TYPE refxcn_jobd,
      END OF ty_s_ekes_tab .
    TYPES ebelp TYPE ebelp .
    TYPES:
      ty_t_ekes_tab TYPE SORTED TABLE OF ty_s_ekes_tab  WITH NON-UNIQUE KEY ebeln ebelp etens ebtyp .
    TYPES etens TYPE etens .
    TYPES:
      BEGIN OF ty_s_info_rec,
        infnr      TYPE infnr,
        matnr      TYPE matnr,
        lifnr      TYPE elifn,
        ekorg      TYPE ekorg,
        aut_source TYPE aut_source,
        mwskz      TYPE mwskz,
      END OF ty_s_info_rec .
    TYPES ebtyp TYPE ebtyp .
    TYPES:
      ty_t_info_rec TYPE STANDARD TABLE OF ty_s_info_rec .
    TYPES eindt TYPE bbein .
    TYPES:
      BEGIN OF ty_s_werks,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        bwkey TYPE bwkey,
        bwtar TYPE bwtar_d,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
      END OF ty_s_werks .
    TYPES menge TYPE bbmng .
    TYPES:
      ty_t_werks TYPE STANDARD TABLE OF ty_s_werks .
    TYPES:
      BEGIN OF ty_s_matdata,
        matnr  TYPE matnr,
        meins  TYPE meins,
        picnum TYPE pic_picnum,
        werks  TYPE werks_d,
        lgort  TYPE lgort_d,
      END OF ty_s_matdata .
*    TYPES zzunique_key1 TYPE zmm_uniquekey1 .
    TYPES:
      ty_t_matdata TYPE SORTED TABLE OF ty_s_matdata WITH NON-UNIQUE KEY matnr .
*    TYPES zzunique_key2 TYPE zmm_uniquekey2 .
    TYPES:
      BEGIN OF ty_s_mbew,
        matnr TYPE matnr,
        bwkey TYPE bwkey,
        bwtar TYPE bwtar_d,
      END OF ty_s_mbew .
*    TYPES zzunique_key3 TYPE zmm_uniquekey3 .
    TYPES:
      ty_t_mbew TYPE STANDARD TABLE OF ty_s_mbew .
    TYPES zzprocessdate TYPE refxcn_jobd .
    TYPES:
      BEGIN OF ty_s_mvke,
        matnr TYPE matnr,
        vkorg TYPE vkorg,
        vtweg TYPE vtweg,
      END OF ty_s_mvke .
    TYPES:
      ty_t_mvke TYPE STANDARD TABLE OF ty_s_mvke .
    TYPES:
      BEGIN OF ty_s_matnr,
        matnr TYPE matnr,
      END OF ty_s_matnr .
    TYPES:
      ty_t_matnr TYPE STANDARD TABLE OF ty_s_matnr .
    TYPES:
      BEGIN OF ty_s_unsez,
        unsez TYPE unsez,
      END OF ty_s_unsez .
    TYPES:
      BEGIN OF ty_s_po,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        matnr TYPE matnr,
        bukrs TYPE bukrs,
        unsez TYPE unsez,
        werks TYPE ewerk,
        netpr TYPE netpr,
        uebpo TYPE uebpo,
        uptyp TYPE uptyp,
        bsart TYPE esart,
        lifnr TYPE lifnr,
        ekorg TYPE ekorg,
      END OF  ty_s_po .
    TYPES:
      BEGIN OF ty_s_ekes,

        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        etens TYPE etens,
        ebtyp TYPE ebtyp,
        eindt TYPE bbein,
        lpein TYPE bblpe,
        uzeit TYPE bbuze,
        erdat TYPE bberd,
        ezeit TYPE bbeze,
        menge TYPE bbmng,
        dabmg TYPE dabmg,
        estkz TYPE bbest,
        kzdis TYPE kzdis,

      END OF  ty_s_ekes .
    TYPES:
      BEGIN OF ty_s_confirmation,
        ebeln TYPE  ebeln,
        ebelp TYPE  ebelp,
        etens	TYPE etens,
        ebtyp	TYPE ebtyp,
        eindt	TYPE bbein,
        lpein	TYPE bblpe,
        uzeit	TYPE bbuze,
        erdat	TYPE bberd,
        ezeit	TYPE bbeze,
        menge	TYPE bbmng,
        dabmg	TYPE dabmg,
        estkz	TYPE bbest,
        loekz	TYPE bbloe,
        kzdis	TYPE kzdis,
      END OF ty_s_confirmation .
    TYPES:
      ty_t_confirmation TYPE STANDARD TABLE OF ty_s_confirmation .
    TYPES:
      ty_t_po TYPE SORTED TABLE OF ty_s_po WITH NON-UNIQUE KEY unsez .
    TYPES:
      ty_t_po_mat TYPE SORTED TABLE OF ty_s_po WITH NON-UNIQUE KEY matnr .
    TYPES:
      ty_t_eta TYPE STANDARD TABLE OF zmmc_boleadtime .
    TYPES:
      ty_t_unsez TYPE STANDARD TABLE OF ty_s_unsez .
    TYPES:
      ty_t_ekes TYPE STANDARD TABLE OF ty_s_ekes .
    TYPES ty_s_input TYPE zsmm_back_order_confirmation .                          "ZDT_BACK_ORDER_CONFIRMATION621 .
    TYPES:
      ty_t_input TYPE STANDARD TABLE OF ty_s_input .
    TYPES:
      BEGIN OF ty_s_back_ord,
        back_conf TYPE REF TO zcl_mm_back_order_conf,
      END OF ty_s_back_ord .
    TYPES:
      ty_t_back_ord TYPE STANDARD TABLE OF ty_s_back_ord .
    TYPES:
      ty_t_bopoupdate TYPE STANDARD TABLE OF zmmc_bopoupdate .
    TYPES:
      BEGIN OF ty_sort_seq.
        INCLUDE  TYPE zsmm_back_order_confirmation.
    TYPES:   sort_seq TYPE i.
    TYPES: END OF ty_sort_seq .
    TYPES:
      BEGIN OF ty_s_vendor,
        lifnr TYPE lifnr,
        land1 TYPE land1_gp,
        ekorg TYPE ekorg,
        bstae TYPE bstae,
      END OF ty_s_vendor .
    TYPES:
      ty_t_vendor TYPE STANDARD TABLE OF ty_s_vendor .
    TYPES:
      BEGIN OF ty_pr_data,
        preq_no    TYPE banfn,
        preq_item  TYPE bnfpo,
        delete_ind TYPE eloek,
        quantity   TYPE bamng,
      END OF ty_pr_data .
    TYPES:
      ty_t_pr TYPE STANDARD TABLE OF ty_pr_data .

    DATA gv_feh_key TYPE ech_dte_objkey READ-ONLY .
    CONSTANTS gc_delete_ind TYPE c VALUE 'L' ##NO_TEXT.
    CONSTANTS gc_valid_to TYPE kodatbi VALUE '99991231' ##NO_TEXT.
    CONSTANTS gc_cond_no TYPE knumh VALUE '$000000001' ##NO_TEXT.
    CONSTANTS gc_tab_304 TYPE kotabnr VALUE '304' ##NO_TEXT.
    CONSTANTS gc_tab TYPE kotabnr VALUE '018' ##NO_TEXT.
    CONSTANTS gc_app_v TYPE kappl VALUE 'V' ##NO_TEXT.
    CONSTANTS gc_app TYPE kappl VALUE 'M' ##NO_TEXT.
    CONSTANTS gc_cond_type_sd TYPE kscha VALUE 'PPR0' ##NO_TEXT.
    CONSTANTS gc_cond_type TYPE kscha VALUE 'PB00' ##NO_TEXT.
    CONSTANTS gc_cond_usg TYPE kvewe VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_esokz TYPE char1 VALUE '0' ##NO_TEXT.
    CONSTANTS gc_stat_0300 TYPE char5 VALUE '0300' ##NO_TEXT.
    CONSTANTS gc_alloc_0500 TYPE char5 VALUE '0500' ##NO_TEXT.
    CONSTANTS gc_bo_0400 TYPE char5 VALUE '0400' ##NO_TEXT.
    CONSTANTS gc_classtype TYPE char3 VALUE '001' ##NO_TEXT.
    CONSTANTS gc_conf_code_0001 TYPE char5 VALUE '0001' ##NO_TEXT.
    CONSTANTS gc_conf_type_ab TYPE char2 VALUE 'AB' ##NO_TEXT.
    CONSTANTS gc_conf_type_bo TYPE char2 VALUE 'BO' ##NO_TEXT.
    CONSTANTS gc_dat_cat TYPE char1 VALUE '1' ##NO_TEXT.
    CONSTANTS gc_delete TYPE char1 VALUE 'D' ##NO_TEXT.
    CONSTANTS gc_error TYPE char1 VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_extid TYPE balnrext VALUE 'GIFT_CONFIRM' ##NO_TEXT.
    CONSTANTS gc_langu_en TYPE char2 VALUE 'EN' ##NO_TEXT.
    CONSTANTS gc_mara TYPE char5 VALUE 'MARA' ##NO_TEXT.
    CONSTANTS gc_null TYPE char5 VALUE '00000' ##NO_TEXT.
    CONSTANTS gc_obj TYPE balobj_d VALUE 'ZINT' ##NO_TEXT.
    CONSTANTS gc_order_cancel_5300 TYPE char5 VALUE '5300' ##NO_TEXT.
    CONSTANTS gc_order_cancel_ab_5500 TYPE char5 VALUE '5500' ##NO_TEXT.
    CONSTANTS gc_order_cancel_bo_5400 TYPE char5 VALUE '5400' ##NO_TEXT.
    CONSTANTS gc_ord_cancel_5200 TYPE char5 VALUE '5200' ##NO_TEXT.
    CONSTANTS gc_piccat TYPE char2 VALUE '01' ##NO_TEXT.
    CONSTANTS gc_sign_eq TYPE char2 VALUE 'EQ' ##NO_TEXT.
    CONSTANTS gc_sign_i TYPE char1 VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_subobj TYPE balsubobj VALUE 'GIFT_CONFIRM' ##NO_TEXT.
    CONSTANTS gc_sub_cat TYPE char1 VALUE '8' ##NO_TEXT.
    CONSTANTS gc_success TYPE char1 VALUE 'S' ##NO_TEXT.
    CONSTANTS gc_update TYPE char1 VALUE 'U' ##NO_TEXT.
    CONSTANTS gc_warn TYPE char1 VALUE 'W' ##NO_TEXT.
    CONSTANTS gc_msgclass TYPE char5 VALUE 'ZP2P' ##NO_TEXT.
    DATA gt_appl_log TYPE bapiret2_t READ-ONLY .
    DATA gs_feh_payload TYPE zsmm_back_order_payload READ-ONLY .
    CONSTANTS gc_un_sc TYPE char1 VALUE '-' ##NO_TEXT.
    CONSTANTS gc_ab_date TYPE datum VALUE '20400101' ##NO_TEXT.

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !ir_data TYPE REF TO ztmm_back_order_confirmation62 .
    CLASS-METHODS get_preload_confirmations
      IMPORTING
        !io_data  TYPE REF TO zsmm_back_order_payload                                           "ty_t_input
      EXPORTING
        !et_order TYPE ty_t_back_ord
      RAISING
        zcx_mm_back_ord_exp .
    METHODS on_proxy_invoke
      RAISING
        zcx_mm_back_ord_exp .
    CLASS-METHODS map_input
      IMPORTING
        VALUE(it_input) TYPE zdt_back_order_confirmati_tab9
        !iv_master_flag TYPE char1
      EXPORTING
        VALUE(et_order) TYPE zsmm_back_order_payload
      RAISING
        zcx_mm_back_ord_exp .
PROTECTED SECTION.
PRIVATE SECTION.

  CONSTANTS gc_curr TYPE konwa VALUE 'AUD' ##NO_TEXT.
  DATA gt_back_conf_data TYPE ztmm_back_order_confirmation62 .
  CLASS-DATA gv_upd_mas_data TYPE char1 .
  CLASS-DATA gt_vendor TYPE ty_t_vendor .
  DATA gv_app_log_id TYPE balnrext .
  DATA gv_total_ordqty TYPE bstmg .
  DATA gv_ord_mat_qty TYPE bstmg .
  DATA gv_con_mat_qty TYPE bstmg .
  DATA gt_pr_data TYPE ty_t_pr .
  DATA gt_po_item TYPE bapimepoitem_tp .
  DATA gt_poitemx TYPE bapimepoitemx_tp .
  DATA gt_poconfirmation TYPE ty_t_conf .
  DATA gt_poschedule TYPE bapieket_tp .
  DATA gt_poitem TYPE ty_item_t .
  DATA gs_poheader TYPE bapimepoheader .
  CONSTANTS gc_kappl TYPE char1 VALUE 'M' ##NO_TEXT.
  CONSTANTS gc_kschl TYPE char4 VALUE 'PB00' ##NO_TEXT.
  CONSTANTS gc_datbi TYPE dats VALUE '99991231' ##NO_TEXT.
  CLASS-DATA gt_info_rec TYPE ty_t_info_rec .
  CLASS-DATA gt_po_details TYPE ty_t_po .
  CLASS-DATA gt_matdata TYPE ty_t_matdata .
  CLASS-DATA gt_status_code TYPE trg_char4 .
  CLASS-DATA gt_mbew TYPE ty_t_mbew .
  CLASS-DATA gt_mvke TYPE ty_t_mvke .
  DATA gv_meins TYPE meins .
  CLASS-DATA gt_eta_details TYPE ty_t_eta .
  CLASS-DATA gt_sales_code TYPE ztmm_sales_code .
  DATA gt_conf_add_ab TYPE bapimeconf_t_detail .
  DATA gt_conf_addx_ab TYPE bapimeconf_t_detailx .
  DATA gt_yekes TYPE mmpr_uekes .

  METHODS get_eta
    IMPORTING
      !is_item               TYPE zsmm_back_order_confirmation
      VALUE(iv_ordered_item) TYPE ebelp
    EXPORTING
      VALUE(ev_date)         TYPE datum .
  METHODS update_po_without_supersession
    IMPORTING
      !is_item               TYPE zsmm_back_order_confirmation
      VALUE(iv_ordered_item) TYPE ebelp .
  CLASS-METHODS get_sales_code
    EXPORTING
      !et_sales_code TYPE ztmm_sales_code .
  METHODS insert_supersessionchain
    IMPORTING
      VALUE(iv_part_no)           TYPE char40
      VALUE(iv_superseed_part_no) TYPE char40
      VALUE(it_picpsrl)           TYPE vt_picpsrl
    EXPORTING
      VALUE(et_return)            TYPE bapiret2_t
      VALUE(ev_pic_num)           TYPE pic_picnum .
  METHODS add_create_supersessionchain
    IMPORTING
      VALUE(iv_part_no)           TYPE char40
      VALUE(iv_superseed_part_no) TYPE char40
      VALUE(it_picpsrl)           TYPE vt_picpsrl
    EXPORTING
      VALUE(ev_pic_num)           TYPE pic_picnum .
  METHODS post_po .
  METHODS fill_x_struct_planningdata
    IMPORTING
      !is_planningdata  TYPE bapi_mpgd
    EXPORTING
      !es_planningdatax TYPE bapi_mpgdx .
  METHODS fill_x_struct_plantdata
    IMPORTING
      !is_plantdata  TYPE bapi_marc
    EXPORTING
      !es_plantdatax TYPE bapi_marcx .
  METHODS fill_x_struct_clientdata
    IMPORTING
      !is_clientdata  TYPE bapi_mara
    EXPORTING
      !es_clientdatax TYPE bapi_marax .
  METHODS fill_x_struct_forecast
    IMPORTING
      !is_forecast  TYPE bapi_mpop
    EXPORTING
      !es_forecastx TYPE bapi_mpopx .
  METHODS fill_x_struct_saledata
    IMPORTING
      !is_saledata  TYPE bapi_mvke
    EXPORTING
      !es_saledatax TYPE bapi_mvkex .
  METHODS fill_x_struct_uom
    IMPORTING
      !is_uom  TYPE bapi_marm
    EXPORTING
      !es_uomx TYPE bapi_marmx .
  METHODS fill_x_struct_storageloc
    IMPORTING
      !is_storagelocdata  TYPE bapi_mard
    EXPORTING
      !es_storagelocdatax TYPE bapi_mardx .
  METHODS fill_x_struct_storagetypedata
    IMPORTING
      !is_storagetypedata  TYPE bapi_mlgt
    EXPORTING
      !es_storagetypedatax TYPE bapi_mlgtx .
  METHODS fill_x_struct_valuationdata
    IMPORTING
      !is_valuationdata  TYPE bapi_mbew
    EXPORTING
      !es_valuationdatax TYPE bapi_mbewx .
  METHODS fill_x_struct_warehouse
    IMPORTING
      !is_warehousedata  TYPE bapi_mlgn
    EXPORTING
      !es_warehousedatax TYPE bapi_mlgnx .
  METHODS create_classification
    IMPORTING
      VALUE(iv_super_matnr) TYPE matnr
      VALUE(iv_matnr)       TYPE matnr .
  METHODS get_supersessionchain
    IMPORTING
      VALUE(iv_matnr)                 TYPE matnr
      VALUE(iv_superseed_part_no)     TYPE matnr
    EXPORTING
      !ev_sup_session_not_exists      TYPE boole_d
      !ev_sup_session_exists_ord_mat  TYPE boole_d
      !ev_sup_session_exists_conf_mat TYPE boole_d
      !ev_sup_different_ord_conf_mat  TYPE boole_d
      !ev_sup_ord_mat_not_exists      TYPE boole_d
      !et_picpsrl                     TYPE vt_picpsrl .
  METHODS get_po_conf_det
    IMPORTING
      !iv_sub_code          TYPE char2
    EXPORTING
      VALUE(es_po_conf_det) TYPE zsmm_back_ord_po_config .
  METHODS log_mesg
    IMPORTING
      VALUE(iv_type)   TYPE bapi_mtype DEFAULT sy-msgty
      VALUE(iv_id)     TYPE symsgid DEFAULT sy-msgid
      VALUE(iv_number) TYPE symsgno DEFAULT sy-msgno
      VALUE(iv_v1)     TYPE symsgv DEFAULT sy-msgv1
      VALUE(iv_v2)     TYPE symsgv DEFAULT sy-msgv2
      VALUE(iv_v3)     TYPE symsgv DEFAULT sy-msgv3
      VALUE(iv_v4)     TYPE symsgv DEFAULT sy-msgv4 .
  METHODS update_po_with_supersession
    IMPORTING
      !is_item               TYPE zsmm_back_order_confirmation
      VALUE(iv_ordered_item) TYPE ebelp .
  METHODS process_supersession_chain
    IMPORTING
      VALUE(iv_ordered_matnr)   TYPE char40
      VALUE(iv_confirmed_matnr) TYPE char40
    EXPORTING
      VALUE(ev_picnum)          TYPE pic_picnum .
  METHODS create_supersession_matnr
    IMPORTING
      VALUE(iv_ordered_matnr)   TYPE char40
      VALUE(iv_description)     TYPE maktg
      VALUE(iv_confirmed_matnr) TYPE char40 .
  METHODS create_supersessionchain
    IMPORTING
      VALUE(iv_ordered_matnr)   TYPE matnr
      VALUE(iv_confirmed_matnr) TYPE matnr
    EXPORTING
      VALUE(ev_pic_num)         TYPE pic_picnum .
  METHODS process_status_codes
    IMPORTING
      VALUE(iv_ordered_matnr)     TYPE matnr
      VALUE(iv_confirmed_matnr)   TYPE matnr
      VALUE(iv_order_status_code) TYPE char4
      VALUE(iv_fob)               TYPE zmm_dec_7_2
      VALUE(iv_process_qty)       TYPE bstmg
      VALUE(iv_ord_itemno)        TYPE ebelp
      VALUE(iv_eta)               TYPE dats .
  METHODS process_order_status_5200
    IMPORTING
      VALUE(iv_superseed_part_no) TYPE matnr
      VALUE(iv_ord_itemno)        TYPE ebelp
    EXPORTING
      VALUE(et_item)              TYPE bapimepoitem_tp
      VALUE(et_itemx)             TYPE bapimepoitemx_tp
      VALUE(et_pr_data)           TYPE ty_t_pr .
  METHODS process_order_status_5400
    IMPORTING
      VALUE(iv_confirmed_material) TYPE matnr
      VALUE(iv_process_qty)        TYPE bstmg OPTIONAL
      VALUE(iv_ord_itemno)         TYPE ebelp
    EXPORTING
      VALUE(et_item)               TYPE bapimepoitem_tp
      VALUE(et_itemx)              TYPE bapimepoitemx_tp
      VALUE(et_pr_data)            TYPE ty_t_pr .
  METHODS process_order_status_5500
    IMPORTING
      VALUE(iv_confirmed_material) TYPE matnr
      VALUE(iv_process_qty)        TYPE bstmg OPTIONAL
      VALUE(iv_ord_itemno)         TYPE ebelp
    EXPORTING
      VALUE(et_item)               TYPE bapimepoitem_tp
      VALUE(et_itemx)              TYPE bapimepoitemx_tp
      VALUE(et_pr_data)            TYPE ty_t_pr .
  METHODS process_order_status_0500
    IMPORTING
      VALUE(iv_ordered_matnr)   TYPE matnr
      VALUE(iv_confirmed_matnr) TYPE matnr
      VALUE(iv_fob)             TYPE zmm_dec_7_2
      VALUE(iv_process_qty)     TYPE bstmg OPTIONAL
      VALUE(iv_eta)             TYPE dats OPTIONAL
    EXPORTING
      VALUE(et_item)            TYPE bapimepoitem_tp
      VALUE(et_itemx)           TYPE bapimepoitemx_tp
      VALUE(et_conf_add)        TYPE bapimeconf_t_detail
      VALUE(et_conf_addx)       TYPE bapimeconf_t_detailx .
  METHODS process_order_status_0400
    IMPORTING
      VALUE(iv_ordered_matnr)   TYPE matnr
      VALUE(iv_confirmed_matnr) TYPE matnr
      VALUE(iv_fob)             TYPE zmm_dec_7_2
      VALUE(iv_process_qty)     TYPE bstmg OPTIONAL
      VALUE(iv_eta)             TYPE dats
    EXPORTING
      VALUE(et_item)            TYPE bapimepoitem_tp
      VALUE(et_itemx)           TYPE bapimepoitemx_tp
      VALUE(et_conf_add)        TYPE bapimeconf_t_detail
      VALUE(et_conf_addx)       TYPE bapimeconf_t_detailx .
  METHODS create_pricing_conditions
    IMPORTING
      VALUE(iv_lifnr)   TYPE elifn
      VALUE(iv_ekorg)   TYPE ekorg
      VALUE(iv_matnr)   TYPE matnr
      VALUE(iv_fob)     TYPE bprei OPTIONAL
      VALUE(iv_meins)   TYPE meins
      VALUE(iv_old_mat) TYPE matnr OPTIONAL
    EXPORTING
      VALUE(et_return)  TYPE bapiret2_t  ##NEEDED.
  METHODS create_po
    IMPORTING
      !is_po_det      TYPE zsmm_back_order_confirmation
    RETURNING
      VALUE(rv_po_no) TYPE bapimepoheader-po_number .
  METHODS validate_po_item_quantity
    IMPORTING
      !is_order_data   TYPE ty_s_input
    RETURNING
      VALUE(rv_failed) TYPE boolean .
  METHODS init .
  METHODS error_logged
    RETURNING
      VALUE(rv_flag) TYPE boolean .
  METHODS update_pr_quantity .
  METHODS set_po_detail
    IMPORTING
      VALUE(iv_ebeln) TYPE ebeln .
ENDCLASS.



CLASS ZCL_MM_BACK_ORDER_CONF IMPLEMENTATION.


  METHOD add_create_supersessionchain.

*Data Declarations
    DATA: ls_pic_list    TYPE pic01_ts_pic_interface,
          ls_old_picpsrl TYPE v_picpsrl,
          ls_new_picpsrl TYPE v_picpsrl,
          lv_pic_num     TYPE pic_picnum,
          lv_super_matnr TYPE matnr,
          lt_old_picpsrl TYPE STANDARD TABLE OF v_picpsrl,
          lt_new_picpsrl TYPE STANDARD TABLE OF v_picpsrl,
          lt_old_pic     TYPE STANDARD TABLE OF v_picmrl,
          lt_new_pic     TYPE STANDARD TABLE OF v_picmrl,
          lt_smesg       TYPE STANDARD TABLE OF smesg.

    CLEAR : ev_pic_num.

    lv_super_matnr = iv_superseed_part_no.

    LOOP AT it_picpsrl INTO DATA(ls_picsrl).

      ls_old_picpsrl = ls_picsrl.
      APPEND ls_old_picpsrl TO lt_old_picpsrl.

      lv_pic_num     = ls_picsrl-picnum.
      DATA(lv_seqnr) = ls_new_picpsrl-seqnr.
      DATA(lv_datfr) = ls_new_picpsrl-datfr.
*Part number
      ls_new_picpsrl-piccat   =  ls_picsrl-piccat.
      ls_new_picpsrl-matnr    =  ls_picsrl-matnr.
      ls_new_picpsrl-seqnr    =  ls_picsrl-seqnr.
      ls_new_picpsrl-datfr    =  ls_picsrl-datfr.
      ls_new_picpsrl-inttype  =  ls_picsrl-inttype.
      ls_new_picpsrl-piccode  =  ls_picsrl-piccode.
      ls_new_picpsrl-picnum   =  ls_picsrl-picnum.

      APPEND ls_new_picpsrl TO lt_new_picpsrl.
      CLEAR ls_picsrl.

    ENDLOOP.

*supersession Material
    ls_new_picpsrl-piccat =  ls_picsrl-piccat.
    ls_new_picpsrl-matnr  =  lv_super_matnr.
    ls_new_picpsrl-seqnr  =  lv_seqnr + 1.

    IF lv_datfr = sy-datum.
      ls_new_picpsrl-datfr  =  sy-datum + 1.
    ELSE.
      ls_new_picpsrl-datfr  =  sy-datum.
    ENDIF.

    ls_new_picpsrl-inttype = gc_sign_i. "'I'.
    ls_new_picpsrl-piccode = gc_conf_code_0001. "0001

    APPEND ls_new_picpsrl TO lt_new_picpsrl.
    CLEAR  ls_new_picpsrl.

    ls_pic_list-new_picpsrl = lt_new_picpsrl.
    ls_pic_list-old_picpsrl = lt_old_picpsrl.
    ls_pic_list-picnum =   lv_pic_num.

*Create Supersession material chain
*--Unreleased FM as no released API found to update supercession
    CALL FUNCTION 'PIC01_MAINTAIN_VPICPSRL'
      EXPORTING
        is_pic_list       = ls_pic_list
      IMPORTING
        e_picnum          = lv_pic_num
      TABLES
        it_old_vpicmrl    = lt_old_pic
        it_new_vpicmrl    = lt_new_pic
        it_error          = lt_smesg
      EXCEPTIONS
        missing_parameter = 1
        error_input       = 2
        OTHERS            = 3.
    IF sy-subrc = 0.
      ev_pic_num = lv_pic_num .
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.


    CLEAR:gv_upd_mas_data,
          gt_vendor,
          gt_sales_code,
          gt_info_rec,
          gt_po_details,
          gt_matdata,
          gt_status_code,
          gt_mbew,
          gt_mvke,
          gt_eta_details.

*Get Sales_code
    get_sales_code( IMPORTING et_sales_code = gt_sales_code ).

*Get order status code
    gt_status_code = VALUE trg_char4( ( sign = gc_sign_i option = gc_sign_eq low = gc_bo_0400 )
                                      ( sign = gc_sign_i option = gc_sign_eq low = gc_alloc_0500 )
                                      ( sign = gc_sign_i option = gc_sign_eq low = gc_ord_cancel_5200 )
                                      ( sign = gc_sign_i option = gc_sign_eq low = gc_order_cancel_5300 )
                                      ( sign = gc_sign_i option = gc_sign_eq low = gc_order_cancel_bo_5400 )
                                      ( sign = gc_sign_i option = gc_sign_eq low = gc_order_cancel_ab_5500 ) ).

  ENDMETHOD.


  METHOD constructor.


    DATA:lt_sort_seq  TYPE TABLE OF ty_sort_seq.

    CLEAR:me->gv_feh_key,
          me->gv_app_log_id,
          me->gt_back_conf_data.

    ASSIGN ir_data->* TO FIELD-SYMBOL(<lt_tab>).
    IF <lt_tab> IS ASSIGNED.
      me->gt_back_conf_data = <lt_tab>.
    ENDIF.

*Set the logging keys
    READ TABLE me->gt_back_conf_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX 1.
    IF sy-subrc = 0.
      IF <ls_data>-subs_to = '70'.
        me->gv_feh_key = 'AUH' && '-' && <ls_data>-external_po.
      ELSEIF <ls_data>-subs_to = '71'.
        me->gv_feh_key = 'MPE' && '-' && <ls_data>-external_po.
      ELSE.
        me->gv_feh_key = <ls_data>-external_po.
      ENDIF.
*      me->gv_feh_key = <ls_data>-external_po.
*      me->gv_app_log_id = <ls_data>-external_po.
      me->gv_app_log_id = me->gv_feh_key.
    ENDIF.

*--> Sort sequence on PO,Materials and Status code
    MOVE-CORRESPONDING me->gt_back_conf_data TO lt_sort_seq.

    LOOP AT lt_sort_seq ASSIGNING FIELD-SYMBOL(<ls_sort_seq>).
      CASE <ls_sort_seq>-order_status_code.
        WHEN gc_ord_cancel_5200.
          <ls_sort_seq>-sort_seq = '1'.
        WHEN gc_stat_0300.
          <ls_sort_seq>-sort_seq = '2'.
        WHEN gc_bo_0400.
          <ls_sort_seq>-sort_seq = '3'.
        WHEN gc_alloc_0500.
          <ls_sort_seq>-sort_seq = '4'.
        WHEN gc_order_cancel_5300.
          <ls_sort_seq>-sort_seq = '5'.
        WHEN gc_order_cancel_bo_5400.
          <ls_sort_seq>-sort_seq = '6'.
        WHEN gc_order_cancel_ab_5500.
          <ls_sort_seq>-sort_seq = '7'.
        WHEN OTHERS.
          "Do Nothing
      ENDCASE.
    ENDLOOP.

    SORT lt_sort_seq BY external_po ordered_material sort_seq.
    CLEAR: me->gt_back_conf_data[].
    MOVE-CORRESPONDING lt_sort_seq TO me->gt_back_conf_data.


  ENDMETHOD.


  METHOD create_classification.

*Data Declarations
    DATA :lv_objectkey     TYPE bapi1003_key-object,
          lv_sup_objectkey TYPE bapi1003_key-object,
          lv_objecttable   TYPE bapi1003_key-objecttable,
          lv_classtype     TYPE bapi1003_key-classtype,
          lt_allochar      TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
          lt_valuesnum     TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
          lt_valuescurr    TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
          lt_return        TYPE STANDARD TABLE OF bapiret2,
          lv_status        TYPE bapi1003_key-status,
          lt_alloclist     TYPE STANDARD TABLE OF bapi1003_alloc_list.

    lv_objectkey     = iv_matnr.
    lv_sup_objectkey = iv_super_matnr.
    lv_objecttable   = gc_mara. "'MARA'.
    lv_classtype     = gc_classtype.  "'001'.

*Get Classes
    CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
      EXPORTING
        objectkey_imp   = lv_objectkey
        objecttable_imp = lv_objecttable
        classtype_imp   = lv_classtype
      TABLES
        alloclist       = lt_alloclist
        return          = lt_return.

*Check if classes are exists for material
    READ TABLE lt_return TRANSPORTING NO FIELDS
          WITH KEY type = gc_sign_i."'I'

    IF sy-subrc IS INITIAL.
      " No Characterstics Values found for Material
      MESSAGE w100(zp2p) WITH lv_objectkey INTO DATA(lv_message).
      me->log_mesg( ).

    ELSE.

**BAPI - 'BAPI_OBJCL_CREATE' is called inside the loop
*It may affect performance and Implicit COMMIT is performed

*Get Class name for list
      LOOP AT lt_alloclist ASSIGNING FIELD-SYMBOL(<ls_alloclist>).

        CLEAR: lt_return,lv_status,lt_valuesnum,lt_allochar,lt_valuescurr.

*Get Classification details
        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objectkey       = <ls_alloclist>-object
            objecttable     = <ls_alloclist>-objtyp
            classnum        = <ls_alloclist>-classnum
            classtype       = <ls_alloclist>-classtype
          IMPORTING
            status          = lv_status
          TABLES
            allocvaluesnum  = lt_valuesnum
            allocvalueschar = lt_allochar
            allocvaluescurr = lt_valuescurr
            return          = lt_return.

*Check whether classification details available
        READ TABLE lt_return TRANSPORTING NO FIELDS
                 WITH KEY type = gc_error."'E'.
        IF sy-subrc = 0.
          APPEND LINES OF lt_return TO me->gt_appl_log.
          EXIT.
        ELSE.
          CLEAR: lv_status,lt_return.

*Create classifications
          CALL FUNCTION 'BAPI_OBJCL_CREATE'
            EXPORTING
              objectkeynew    = lv_sup_objectkey
              objecttablenew  = <ls_alloclist>-objtyp
              classnumnew     = <ls_alloclist>-classnum
              classtypenew    = <ls_alloclist>-classtype
              status          = gc_dat_cat "'1'
              keydate         = sy-datum
            IMPORTING
              classif_status  = lv_status
            TABLES
              allocvaluesnum  = lt_valuesnum
              allocvalueschar = lt_allochar
              allocvaluescurr = lt_valuescurr
              return          = lt_return.
          IF line_exists( lt_return[ type = 'E' ] ).
            APPEND LINES OF lt_return TO me->gt_appl_log.
            EXIT.
          ENDIF.

        ENDIF.

      ENDLOOP.

      IF me->error_logged( ) = abap_false.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
             wait = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_po.

    DATA:
      lv_itemno     TYPE ebelp,
      ls_po_header  TYPE bapimepoheader,
      ls_po_headerx	TYPE bapimepoheaderx,
      ls_poitem     TYPE bapimepoitem,
      ls_poitemx    TYPE bapimepoitemx,
      ls_schedule   TYPE bapimeposchedule,
      ls_schedulex  TYPE bapimeposchedulx,
      lt_poitem	    TYPE bapimepoitem_tp,
      lt_poitemx    TYPE bapimepoitemx_tp,
      lt_poschdule  TYPE bapimeposchedule_tp,
      lt_poschdulex TYPE bapimeposchedulx_tp,
      lv_po_num     TYPE bapimepoheader-po_number.

*Get PO config data for creating PO
    me->get_po_conf_det( EXPORTING iv_sub_code    = is_po_det-subs_to
                         IMPORTING es_po_conf_det = DATA(ls_po_conf_det) ).

    IF ls_po_conf_det IS INITIAL.
*Subsidiary Code &1 is not maintained for HPO &2
      MESSAGE e462(zp2p) WITH is_po_det-subs_to is_po_det-external_po INTO DATA(lv_message) ##NEEDED.
      me->log_mesg( ).
      RETURN.
    ENDIF.

*Read subsidiary code for Vendor
    SELECT subs_code, to_subs, lifnr
       FROM zmmc_subs_code
       INTO TABLE @DATA(lt_subs_code)
       WHERE subs_code = @is_po_det-subs_from.

    IF sy-subrc <> 0.
*Subsidiary Code &1 is not maintained for HPO &2
      MESSAGE e462(zp2p) WITH is_po_det-subs_from is_po_det-external_po INTO lv_message.
      me->log_mesg( ).
      RETURN.
    ELSE.
      SORT lt_subs_code BY subs_code  to_subs ASCENDING.
    ENDIF.

*Get Currency based on vendor and Purchase org.
    IF lt_subs_code[] IS NOT INITIAL.
      SELECT lifnr,
             ekorg,
             waers
        FROM lfm1
        INTO TABLE @DATA(lt_lfm1)
        FOR ALL ENTRIES IN @lt_subs_code
        WHERE lifnr = @lt_subs_code-lifnr
        AND   ekorg = @ls_po_conf_det-ekorg
        ORDER BY PRIMARY KEY.
      IF sy-subrc <> 0.
        CLEAR lt_lfm1.
      ENDIF.
    ENDIF.

    IF me->gt_back_conf_data IS NOT INITIAL.

      SELECT kappl,kschl,lifnr,matnr,ekorg,esokz,datbi,datab,knumh
          FROM a018
          INTO TABLE @DATA(lt_a018)
          FOR ALL ENTRIES IN @gt_back_conf_data
          WHERE kappl = @gc_kappl "'M'
            AND kschl = @gc_kschl "'PB00'
            AND matnr = @gt_back_conf_data-ordered_material
            AND datbi = @gc_datbi                           "'99991231'
            AND datab LE @sy-datum.

      IF sy-subrc = 0.
        SORT lt_a018 BY lifnr matnr ASCENDING.

        IF lt_a018 IS NOT INITIAL.
*Get price for the condition records
          SELECT knumh,
                 kopos,
                 kbetr
          FROM konp
          INTO TABLE @DATA(lt_konp)
          FOR ALL ENTRIES IN @lt_a018
          WHERE knumh = @lt_a018-knumh.
          IF sy-subrc = 0.
            SORT lt_konp BY knumh ASCENDING.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    DATA(lt_payload) = me->gt_back_conf_data.
    DELETE ADJACENT DUPLICATES FROM lt_payload COMPARING ordered_material.

    LOOP AT lt_payload ASSIGNING FIELD-SYMBOL(<ls_conf_data>).

*Subsidiary Code is different in HPO &1
      IF is_po_det-subs_to <> <ls_conf_data>-subs_to OR is_po_det-subs_from <> <ls_conf_data>-subs_from.
        MESSAGE e463(zp2p) WITH is_po_det-external_po INTO lv_message.
        me->log_mesg( ).
        CONTINUE.
      ENDIF.

*Checking Material whether it is available or not
      READ TABLE gt_matdata
          INTO DATA(ls_matdata)
          WITH KEY matnr = <ls_conf_data>-ordered_material.
      IF sy-subrc <> 0.
        MESSAGE e464(zp2p) WITH <ls_conf_data>-ordered_material INTO lv_message.
        me->log_mesg( ).
        CONTINUE.
      ENDIF.

*Read vendor from the To subsidiary code
      READ TABLE lt_subs_code INTO DATA(ls_subs_code)
           WITH KEY subs_code = is_po_det-subs_from
                    to_subs   = is_po_det-subs_to
           BINARY SEARCH.
      IF sy-subrc <> 0.
        MESSAGE e465(zp2p) WITH is_po_det-subs_from INTO lv_message.
        me->log_mesg( ).
        CONTINUE.
      ENDIF.

*Check for Pricing Condition record for material
      READ TABLE lt_a018 INTO DATA(ls_a018)
          WITH KEY lifnr = ls_subs_code-lifnr
                   matnr = <ls_conf_data>-ordered_material
          BINARY SEARCH.
      IF sy-subrc <> 0.
        MESSAGE e421(zp2p) WITH <ls_conf_data>-ordered_material ls_subs_code-lifnr INTO DATA(lv_dummy) ##NEEDED.
        me->log_mesg( ).
        CONTINUE.
      ELSE.
        READ TABLE lt_konp INTO DATA(ls_konp)
           WITH KEY knumh = ls_a018-knumh
           BINARY SEARCH.
        IF ls_konp IS INITIAL OR ls_konp-kbetr <= 0.
*Net price could not be determined for Material &1 and Vendor &2
          MESSAGE e421(zp2p) WITH <ls_conf_data>-ordered_material ls_subs_code-lifnr INTO lv_dummy.
          me->log_mesg( ).
          CONTINUE.
        ENDIF.
      ENDIF.

*===============
*  PO Header
*===============
      ls_po_header-doc_type   = 'ZAIR'.
      ls_po_header-creat_date = sy-datum.
      ls_po_header-created_by = sy-uname.
      ls_po_header-vendor     = ls_subs_code-lifnr.
      ls_po_header-langu      = sy-langu.
      ls_po_header-purch_org  = ls_po_conf_det-ekorg.
      ls_po_header-comp_code  = ls_po_conf_det-bukrs.
      ls_po_header-our_ref    = is_po_det-external_po.
      ls_po_header-pur_group  = ls_po_conf_det-ekgrp.

*Get Currency from Vendor and Pur.org
      READ TABLE lt_lfm1 INTO DATA(ls_lfm1) WITH KEY lifnr = ls_po_header-vendor
                                                     ekorg = ls_po_header-purch_org
                                                     BINARY SEARCH.

      IF sy-subrc = 0 AND ls_lfm1-waers IS NOT INITIAL.
        ls_po_header-currency = ls_lfm1-waers.
      ENDIF.

      ls_po_header-doc_date    = sy-datum.

      ls_po_headerx-comp_code  = ls_po_headerx-doc_type  = ls_po_headerx-creat_date
    = ls_po_headerx-created_by = ls_po_headerx-vendor    = ls_po_headerx-our_ref
    = ls_po_headerx-langu      = ls_po_headerx-pur_group = ls_po_headerx-purch_org
    = ls_po_headerx-currency   = ls_po_headerx-doc_date  = abap_true.

*===============
*  PO Item
*===============
      lv_itemno = lv_itemno + 1.

*Read Info records to get Tax code
      READ TABLE gt_info_rec INTO DATA(ls_info_rec) WITH KEY matnr = <ls_conf_data>-ordered_material
                                                             lifnr = ls_po_header-vendor
                                                             ekorg = ls_po_header-purch_org
                                                             BINARY SEARCH.
      IF ls_info_rec-mwskz IS INITIAL.
        MESSAGE e420(zp2p) INTO lv_dummy WITH <ls_conf_data>-ordered_material ls_subs_code-lifnr.
        me->log_mesg( ).
        CONTINUE.
      ELSE.
        ls_poitem-tax_code = ls_info_rec-mwskz.
      ENDIF.

      ls_poitem-po_item  = lv_itemno.
      ls_poitem-material = <ls_conf_data>-ordered_material.
      ls_poitem-plant    = ls_po_conf_det-werks_d.
      ls_poitem-stge_loc = ls_po_conf_det-lgort_d.
      ls_poitem-quantity = <ls_conf_data>-ordered_quantity.
      ls_poitem-po_unit  = ls_matdata-meins.
      APPEND ls_poitem TO lt_poitem.
      CLEAR ls_poitem.

*===============
*  PO ItemX
*===============
      ls_poitemx-po_item  = lv_itemno.
      ls_poitemx-po_itemx = ls_poitemx-material = ls_poitemx-plant
    = ls_poitemx-stge_loc = ls_poitemx-quantity = ls_poitemx-po_unit
    = ls_poitemx-info_rec = ls_poitemx-tax_code = abap_true .
      APPEND ls_poitemx TO lt_poitemx.
      CLEAR ls_poitemx.

*=====================
*PO schedule
*=====================
      ls_schedule-po_item       = lv_itemno.
      ls_schedule-delivery_date = sy-datum.
      APPEND ls_schedule TO lt_poschdule.
      CLEAR ls_schedule.

      ls_schedulex-po_item       = lv_itemno.
      ls_schedulex-po_itemx      = abap_true.
      ls_schedulex-delivery_date = abap_true.
      APPEND ls_schedulex TO lt_poschdulex.

      CLEAR:ls_konp,ls_lfm1,ls_info_rec,ls_matdata,ls_subs_code,ls_a018,ls_schedulex.

    ENDLOOP.

    IF me->error_logged( ) = abap_false.
*PO Create
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = ls_po_header
          poheaderx        = ls_po_headerx
        IMPORTING
          exppurchaseorder = lv_po_num
        TABLES
          return           = me->gt_appl_log
          poitem           = lt_poitem
          poitemx          = lt_poitemx
          poschedule       = lt_poschdule
          poschedulex      = lt_poschdulex.

      IF me->error_logged( ) = abap_false.

*HPO &1 is not available in SAP,Creating New PO...!
        MESSAGE e461(zp2p) WITH ls_po_header-our_ref INTO lv_dummy.
        me->log_mesg( ).

        rv_po_no = lv_po_num.
        MESSAGE s372(zp2p) WITH lv_po_num INTO lv_dummy.
        me->log_mesg( ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_pricing_conditions.

*Data Declarations
    DATA: ls_bapicondct TYPE  bapicondct,
          ls_return     TYPE  bapiret2,
          lv_matnr      TYPE char18,
          lv_ekorg      TYPE ekorg,
          lv_vendor     TYPE elifn,
          lv_esokz      TYPE esokz,
          lt_return     TYPE STANDARD TABLE OF bapiret2,
          ls_komg       TYPE e1komg,
          ls_konh       TYPE e1konh,
          ls_konp       TYPE e1konp,
          lv_docnum     TYPE edidc-docnum.


*--> Begin of insert for H1S-8347
*Get Material
*Accepts only char18
*Conversion for Part no
    lv_matnr = iv_matnr.
*--> Get condition record for ordered material
    SELECT kappl,
           kschl,
           vkorg,
           vtweg,
           matnr,
           kfrst,
           datbi,
           datab,
           knumh
      FROM a304
      INTO TABLE @DATA(lt_cond)
      WHERE kappl = @gc_app_v
      AND kschl = @gc_cond_type_sd
      AND matnr = @iv_old_mat
      AND kfrst = @space
      AND datbi GE @sy-datlo
      AND datab LE @sy-datlo.
    IF sy-subrc = 0  AND lt_cond IS NOT INITIAL.
*--> Get Material Price from KONP
      SELECT knumh,kopos,kbetr,konwa
        FROM konp
        INTO TABLE @DATA(lt_price)
        FOR ALL ENTRIES IN @lt_cond
        WHERE knumh = @lt_cond-knumh
        AND kappl = @gc_app_v
        AND kschl = @gc_cond_type_sd.
      IF sy-subrc = 0.
        SORT lt_price BY knumh.
      ENDIF.

*--> Fill the IDOC to create condition
      LOOP AT lt_cond INTO DATA(ls_cond).
        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumh = ls_cond-knumh BINARY SEARCH.
        IF sy-subrc = 0 AND ls_price-kbetr IS NOT INITIAL.
*--> Update SD pricing condition
          ls_komg-kvewe   = gc_cond_usg. "A
          ls_komg-kotabnr = gc_tab_304.  "304
          ls_komg-kappl   = gc_app_v.    "V.
          ls_komg-kschl   = gc_cond_type_sd."PPR0.

*--> Varkey
          CONCATENATE ls_cond-vkorg ls_cond-vtweg lv_matnr INTO DATA(lv_varkey) RESPECTING BLANKS.

          ls_komg-vakey = lv_varkey.
          ls_komg-matnr = lv_matnr.
          ls_komg-vkorg = ls_cond-vkorg.

          ls_konh-knumh = gc_cond_no.  "'$000000001'.
*          ls_konh-datab = sy-datlo - 365.
           ls_konh-datab = ls_cond-datab.
          ls_konh-datbi = ls_cond-datbi. "'99991231'.
*          ls_konh-datbi = gc_valid_to. "'99991231'.

          ls_konp-kschl = gc_cond_type_sd."'PPR0'.
          ls_konp-kbetr = ls_price-kbetr.
          ls_konp-konwa = ls_price-konwa.
          ls_konp-kpein = gc_dat_cat. "1
          ls_konp-kmein = me->gv_meins.

*-->Update Pricing Conditions
          CALL FUNCTION 'Z_SD_PRICE_COND_UPD'
            EXPORTING
              is_komg    = ls_komg
              is_konh    = ls_konh
              is_konp    = ls_konp
            IMPORTING
              et_return  = lt_return
              ev_idocnum = lv_docnum.

*Read Log from Pricing update
          LOOP AT lt_return INTO ls_return WHERE type CA 'AEX'.
            IF ls_return-type IS NOT INITIAL AND ls_return-id IS NOT INITIAL AND ls_return-number IS NOT INITIAL.
              APPEND ls_return TO et_return.
            ENDIF.
          ENDLOOP.
          IF sy-subrc = 0.
            MESSAGE w729(zp2p) WITH lv_docnum INTO DATA(lv_dummy) ##NEEDED.
*Message when condition record is not created
            ls_return-id         = gc_msgclass. "'ZP2P'.
            ls_return-number     = '729'.
            ls_return-type       = gc_warn."'E'.
            ls_return-message_v1 = lv_docnum.

            APPEND ls_return TO et_return.
            CLEAR  ls_return.

            MESSAGE w720(zp2p) WITH lv_matnr INTO lv_dummy.
*Message when condition record is not created
            ls_return-id         = gc_msgclass. "'ZP2P'.
            ls_return-number     = '720'.
            ls_return-type       = gc_warn."'E'.
            ls_return-message_v1 = lv_matnr.

            APPEND ls_return TO et_return.
            CLEAR  ls_return.
          ELSE.
            CLEAR et_return.
            MESSAGE s727(zp2p) WITH lv_matnr gc_cond_type_sd INTO lv_dummy.
*Message when Material condition record created succesfully
            ls_return-id         = gc_msgclass."'ZP2P'.
            ls_return-number     = '727'.
            ls_return-type       = gc_success."'S'.
            ls_return-message_v1 = lv_matnr.
            ls_return-message_v2 = gc_cond_type_sd.

            APPEND ls_return TO et_return.
            CLEAR  ls_return.
          ENDIF.

        ELSE.
          ls_return-id         = gc_msgclass."'ZP2P'.
          ls_return-number     = '728'.
          ls_return-type       = gc_warn."'W'.
          ls_return-message_v1 = ls_cond-vkorg.

          APPEND ls_return TO et_return.
          CLEAR  ls_return.
        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD create_supersessionchain.


*Data Declarations
    DATA: ls_pic_list    TYPE pic01_ts_pic_interface,
          ls_new_picpsrl TYPE v_picpsrl,
          lv_pic_num     TYPE pic_picnum,
          lv_part_no     TYPE matnr,
          ls_return      TYPE bapiret2,
          lv_super_matnr TYPE matnr,
          lt_new_picpsrl TYPE STANDARD TABLE OF v_picpsrl,
          lt_old_pic     TYPE STANDARD TABLE OF v_picmrl,
          lt_new_pic     TYPE STANDARD TABLE OF v_picmrl,
          lt_smesg       TYPE STANDARD TABLE OF smesg.


    CLEAR : ls_pic_list,
            lt_new_picpsrl,
            ls_new_picpsrl,
            lt_old_pic,
            lt_new_pic,
            lt_smesg,
            lv_pic_num,
            lv_part_no,
            lv_super_matnr,
            ev_pic_num.

    lv_part_no     = iv_ordered_matnr.
    lv_super_matnr = iv_confirmed_matnr.


*Part Number
    ls_new_picpsrl-piccat = gc_piccat.
    ls_new_picpsrl-matnr  = lv_part_no.
    ls_new_picpsrl-seqnr  = 0001.

    APPEND ls_new_picpsrl TO lt_new_picpsrl.

*supersession Material
    ls_new_picpsrl-piccat  = gc_piccat.
    ls_new_picpsrl-matnr   = lv_super_matnr.
    ls_new_picpsrl-piccode = gc_conf_code_0001.
    ls_new_picpsrl-seqnr   = ls_new_picpsrl-seqnr + 1.
    ls_new_picpsrl-datfr   = sy-datum.
    ls_new_picpsrl-inttype = gc_sign_i."'I'.

    APPEND ls_new_picpsrl TO lt_new_picpsrl.
    CLEAR ls_new_picpsrl.

    ls_pic_list-new_picpsrl = lt_new_picpsrl.

*Create Supersession material chain
*--Unreleased FM as no released API found to update supercession
    CALL FUNCTION 'PIC01_MAINTAIN_VPICPSRL'
      EXPORTING
        is_pic_list       = ls_pic_list
      IMPORTING
        e_picnum          = lv_pic_num
      TABLES
        it_old_vpicmrl    = lt_old_pic
        it_new_vpicmrl    = lt_new_pic
        it_error          = lt_smesg
      EXCEPTIONS
        missing_parameter = 1
        error_input       = 2
        OTHERS            = 3.
    IF sy-subrc = 0.
      ev_pic_num = lv_pic_num .
    ENDIF.

    LOOP AT lt_smesg ASSIGNING FIELD-SYMBOL(<ls_smesg>).
      IF <ls_smesg>-msgty IS NOT INITIAL AND <ls_smesg>-arbgb IS NOT INITIAL AND <ls_smesg>-txtnr IS NOT INITIAL.
        CLEAR ls_return.
        ls_return-type = <ls_smesg>-msgty.
        ls_return-id   = <ls_smesg>-arbgb.
        ls_return-number = <ls_smesg>-txtnr.
        ls_return-message_v1 = <ls_smesg>-msgv1.
        ls_return-message_v2 = <ls_smesg>-msgv2.
        ls_return-message_v3 = <ls_smesg>-msgv3.
        ls_return-message_v4 = <ls_smesg>-msgv4.
        APPEND ls_return TO me->gt_appl_log.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method create_supersession_matnr.
*"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*DESCRIPTION : Creating Supersession Material Master
*"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
* Data Declarations
    data:
      lt_werks                   type ty_t_werks,
      lv_matnr1                  type matnr18,
      lv_matnr                   type matnr,
      lv_super_matnr             type matnr,
      ls_clidaout                type bapi_mara,
      ls_clidaoutx               type bapi_marax,
      ls_headdata                type bapimathead,
      ls_get_clientdata          type bapi_mara_ga,
      ls_get_plantdata           type bapi_marc_ga,
      ls_plantdata               type bapi_marc,
      ls_plantdatax              type bapi_marcx,
      ls_get_forecastparameters  type bapi_mpop_ga,
      ls_forecastparameters      type bapi_mpop,
      ls_forecastparametersx     type bapi_mpopx,
      ls_get_planningdata        type bapi_mpgd_ga,
      ls_planningdata            type bapi_mpgd,
      ls_planningdatax           type bapi_mpgdx,
      ls_get_storagelocationdata type bapi_mard_ga,
      ls_storagelocationdata     type bapi_mard,
      ls_storagelocationdatax    type bapi_mardx,
      ls_get_valuationdata       type bapi_mbew_ga,
      ls_valuationdata           type /cwm/bapi_mbew,
      ls_valuationdatax          type bapi_mbewx,
      ls_val_data                type bapi_mbew,
      ls_valuationdatacwmx       type /cwm/bapi_mbewx,
      ls_get_warehousenumberdata type bapi_mlgn_ga,
      ls_warehousenumberdata     type bapi_mlgn,
      ls_warehousenumberdatax    type bapi_mlgnx,
      ls_get_salesdata           type bapi_mvke_ga,
      ls_salesdata               type bapi_mvke,
      ls_salesdatax              type bapi_mvkex,
      ls_get_storagetypedata     type bapi_mlgt_ga,
      ls_storagetypedata         type bapi_mlgt,
      ls_storagetypedatax        type bapi_mlgtx,
      ls_mat_desc                type bapi_makt,
      ls_uom                     type bapi_marm,
      lt_get_desc                type standard table of bapi_makt_ga,
      lt_get_uom                 type standard table of bapi_marm_ga,
      lt_get_tax                 type standard table of bapi_mlan_ga,
      lt_get_returnmes           type standard table of bapireturn,
      lt_mat_desc                type standard table of bapi_makt,
      lt_uom                     type standard table of bapi_marm,
      lt_uomx                    type standard table of bapi_marmx,
      lt_returntab               type bapiret2_t.

    data lt_mlan type table of bapi_mlan.
    data lv_error type boolean.
    data ls_return type bapiret2.
    data ls_matdata1 type ty_s_matdata.
    data lv_warehousenumber type lgnum.

    clear lt_werks.

    lv_matnr       = iv_ordered_matnr.
    lv_super_matnr = iv_confirmed_matnr.

*----------------  Find material master details to copy for supersession material ----------------
*Get Plants
    lt_werks = value #( for ls_matdata in gt_matdata
                        where ( matnr = iv_ordered_matnr )
                       ( werks = ls_matdata-werks
                         lgort = ls_matdata-lgort ) ).

    if lt_werks is initial.
      return.
    else.
      sort lt_werks by werks lgort ascending.   "HW3-166
      delete adjacent duplicates from lt_werks comparing werks lgort.  "HW3-166


      loop at lt_werks assigning field-symbol(<ls_werks>).
*Material valuation
        read table gt_mbew assigning field-symbol(<ls_mbew>)
                              with key matnr = iv_ordered_matnr
                                       bwkey = <ls_werks>-werks
                              binary search.
        if sy-subrc = 0.
          <ls_werks>-bwkey = <ls_mbew>-bwkey.
          <ls_werks>-bwtar = <ls_mbew>-bwtar.
        endif.
      endloop.
    endif.

*Read Material data
    read table gt_matdata assigning field-symbol(<ls_matdata>)
                          with key matnr = iv_ordered_matnr.
    if sy-subrc = 0.

      loop at lt_werks into data(ls_werks).

*     Get Material details
        lv_matnr1 = iv_ordered_matnr.

        loop at gt_mvke assigning field-symbol(<ls_mvke>).

          clear lv_warehousenumber.

          select single * from t320 into @data(ls_t320) where werks = @ls_werks-werks and
                                                              lgort = @ls_werks-lgort.
          if sy-subrc = 0.
            lv_warehousenumber = ls_t320-lgnum.
          endif.

          call function 'BAPI_MATERIAL_GET_ALL'
            exporting
              material            = lv_matnr1     "Part no
              val_area            = ls_werks-bwkey
              val_type            = ls_werks-bwtar
              plant               = ls_werks-werks
              stge_loc            = ls_werks-lgort
              salesorg            = <ls_mvke>-vkorg
              distr_chan          = <ls_mvke>-vtweg
              whsenumber          = lv_warehousenumber
            importing
              clientdata          = ls_get_clientdata
              plantdata           = ls_get_plantdata
              forecastparameters  = ls_get_forecastparameters
              planningdata        = ls_get_planningdata
              storagelocationdata = ls_get_storagelocationdata
              valuationdata       = ls_get_valuationdata
              warehousenumberdata = ls_get_warehousenumberdata
              salesdata           = ls_get_salesdata
              storagetypedata     = ls_get_storagetypedata
            tables
              materialdescription = lt_get_desc
              unitsofmeasure      = lt_get_uom
              taxclassifications  = lt_get_tax
              return              = lt_get_returnmes.

          if line_exists( lt_get_returnmes[ type = 'E' ] ).
            lv_error = 'X'.
            loop at lt_get_returnmes into data(ls_return1).
              call function 'BALW_RETURN_TO_RET2'
                exporting
                  return_in = ls_return1
                importing
                  return_ou = ls_return.
              append ls_return to me->gt_appl_log.
            endloop.
            exit.
          endif.

          clear ls_return.

* Header Data: Material and View to maintain
          ls_headdata                   = corresponding #( ls_get_clientdata )   ##ENH_OK.
          ls_headdata-material          = iv_confirmed_matnr.
          ls_headdata-basic_view        = abap_true.
          ls_headdata-sales_view        = abap_true.
          ls_headdata-purchase_view     = abap_true.
          ls_headdata-mrp_view          = abap_true.
          ls_headdata-work_sched_view   = abap_true.
          ls_headdata-prt_view          = abap_true.
          ls_headdata-storage_view      = abap_true.
          ls_headdata-quality_view      = abap_true.
          ls_headdata-account_view      = abap_true.
          ls_headdata-cost_view         = abap_true.
          ls_headdata-warehouse_view    = abap_true.
          ls_headdata-inp_fld_check     = gc_warn."'W'.
          ls_headdata-material_external = abap_true.
          ls_headdata-material_guid     = abap_true.
          ls_headdata-material_version  = abap_true.
          ls_headdata-material_long     = iv_confirmed_matnr.

*Client data
          ls_clidaout             = corresponding #( ls_get_clientdata )  ##ENH_OK.
          ls_clidaout-pl_ref_mat  = iv_ordered_matnr. "Reference material

*Fill the X structure for Clientdata
          me->fill_x_struct_clientdata( exporting is_clientdata  = ls_clidaout
                                        importing es_clientdatax = ls_clidaoutx ).

*Plant data
          ls_plantdata = corresponding #( ls_get_plantdata ) ##ENH_OK.

*Plant datax
          me->fill_x_struct_plantdata( exporting is_plantdata  = ls_plantdata
                                       importing es_plantdatax = ls_plantdatax ).

          ls_plantdatax-plant              = ls_werks-werks.

*Forecastparameters
          ls_forecastparameters = corresponding #( ls_get_forecastparameters ).

*Forecastparametersx
          me->fill_x_struct_forecast( exporting is_forecast  = ls_forecastparameters
                                      importing es_forecastx = ls_forecastparametersx ).

          ls_forecastparametersx-plant     = ls_get_forecastparameters-plant.


*Planningdata
          ls_planningdata = corresponding #( ls_get_planningdata ).

*Planningdatax
          me->fill_x_struct_planningdata( exporting is_planningdata  = ls_planningdata
                                          importing es_planningdatax = ls_planningdatax ).

          ls_planningdatax-plant           =  ls_get_planningdata-plant.


*Storagelocationdata
          ls_storagelocationdata = corresponding #( ls_get_storagelocationdata ).

*Storagelocationdatax
          me->fill_x_struct_storageloc( exporting is_storagelocdata  = ls_storagelocationdata
                                        importing es_storagelocdatax = ls_storagelocationdatax ).

          ls_storagelocationdatax-plant    = ls_storagelocationdata-plant.

*Storage Locationx- storage location
          ls_storagelocationdatax-stge_loc = ls_storagelocationdata-stge_loc.


*Valuationdata
          ls_val_data = corresponding #( ls_get_valuationdata )   ##ENH_OK.

*Valuationx
          me->fill_x_struct_valuationdata( exporting is_valuationdata  = ls_val_data
                                           importing es_valuationdatax = ls_valuationdatax ).

          ls_valuationdatax-val_area   = ls_werks-werks.

*Warehousenumberdata
*          ls_warehousenumberdata = CORRESPONDING #( ls_get_warehousenumberdata ).


*Warehousenumberdatax
*          me->fill_x_struct_warehouse( EXPORTING is_warehousedata  = ls_warehousenumberdata      "HW3-166
*                                       IMPORTING es_warehousedatax = ls_warehousenumberdatax ).  "HW3-166
*
*          ls_warehousenumberdatax-whse_no =  ls_get_warehousenumberdata-whse_no.

          if lv_warehousenumber is not initial.                                                 "HW3-166
            ls_warehousenumberdata-whse_no    = ls_get_warehousenumberdata-whse_no."HW3-166
            ls_warehousenumberdata-stgesector = ls_get_warehousenumberdata-stgesector.           "HW3-166
            ls_warehousenumberdata-placement  = ls_get_warehousenumberdata-placement."HW3-166
            ls_warehousenumberdata-withdrawal = ls_get_warehousenumberdata-withdrawal."HW3-166

            ls_warehousenumberdatax-whse_no    = ls_get_warehousenumberdata-whse_no."HW3-166
            ls_warehousenumberdatax-stgesector = abap_true."HW3-166
            ls_warehousenumberdatax-placement  = abap_true."HW3-166
            ls_warehousenumberdatax-withdrawal = abap_true."HW3-166
          endif.

*Salesdata
          ls_salesdata = corresponding #( ls_get_salesdata ).

*Salesdatax
          me->fill_x_struct_saledata( exporting is_saledata  = ls_salesdata
                                      importing es_saledatax = ls_salesdatax ).

          ls_salesdatax-sales_org  = ls_get_salesdata-sales_org.
          ls_salesdatax-distr_chan = ls_get_salesdata-distr_chan.

*Storagetypedata
          ls_storagetypedata = corresponding #( ls_get_storagetypedata ).

*Storagetypedatax
          me->fill_x_struct_storagetypedata( exporting is_storagetypedata  = ls_storagetypedata
                                             importing es_storagetypedatax = ls_storagetypedatax ).

          ls_storagetypedatax-whse_no     = ls_get_storagetypedata-whse_no.
          ls_storagetypedatax-stge_type   = ls_get_storagetypedata-stge_type.


          if lt_mat_desc is initial.
*Material Description
            ls_mat_desc-langu = gc_langu_en."'EN'.

            ls_mat_desc-matl_desc = iv_description. "Superseeding material
*Appending Material Description
            append ls_mat_desc to lt_mat_desc.
            clear ls_mat_desc.
          endif.


          if lt_uom is initial and lt_get_uom is not initial.
*base uom,ISO and wt
            read table lt_get_uom assigning field-symbol(<ls_uom>)
                                  index 1.
            if sy-subrc = 0.
              ls_uom              = corresponding #( <ls_uom> )   ##ENH_OK.
*UOMX
              me->fill_x_struct_uom( exporting is_uom  = ls_uom
                                     importing es_uomx = data(ls_uomx) ).

              ls_uom-alt_unit  = ls_get_clientdata-base_uom.
              ls_uomx-alt_unit = ls_get_clientdata-base_uom.

              ls_uom-alt_unit_iso = ls_get_clientdata-base_uom_iso.
              ls_uomx-alt_unit_iso = ls_get_clientdata-base_uom_iso.

              ls_uom-unit_of_wt   = ls_get_clientdata-unit_of_wt.

*UOM
              append ls_uom to lt_uom.
              clear ls_uom.
*UOMX
              append ls_uomx to lt_uomx.
              clear ls_uomx.

            endif.
          endif.

          loop at lt_get_tax into data(ls_tax).
            append initial line to lt_mlan assigning field-symbol(<fs_mlan>).
            <fs_mlan>-depcountry = ls_tax-depcountry.
            <fs_mlan>-depcountry_iso = ls_tax-depcountry_iso.
            <fs_mlan>-tax_type_1 = ls_tax-tax_type_1.
            <fs_mlan>-taxclass_1 = ls_tax-taxclass_1.
          endloop.

*Creating Supersession Material reference to part no
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = ls_headdata
              clientdata           = ls_clidaout
              clientdatax          = ls_clidaoutx
              plantdata            = ls_plantdata
              plantdatax           = ls_plantdatax
              forecastparameters   = ls_forecastparameters
              forecastparametersx  = ls_forecastparametersx
              planningdata         = ls_planningdata
              planningdatax        = ls_planningdatax
              storagelocationdata  = ls_storagelocationdata
              storagelocationdatax = ls_storagelocationdatax
              valuationdata        = ls_val_data
              valuationdatax       = ls_valuationdatax
              warehousenumberdata  = ls_warehousenumberdata
              warehousenumberdatax = ls_warehousenumberdatax
              salesdata            = ls_salesdata
              salesdatax           = ls_salesdatax
              storagetypedata      = ls_storagetypedata
              storagetypedatax     = ls_storagetypedatax
              valuationdatacwm     = ls_valuationdata
              valuationdatacwmx    = ls_valuationdatacwmx
            importing
              return               = ls_return
            tables
              materialdescription  = lt_mat_desc
              taxclassifications   = lt_mlan
              unitsofmeasure       = lt_uom
              unitsofmeasurex      = lt_uomx
              returnmessages       = lt_returntab.

*-->Exit from Loop and do not process further
          if ls_return-type = 'E'.
            lv_error = 'X'.
            append lines of lt_returntab to me->gt_appl_log.
            exit.
          endif.

          clear : "ls_werks,
                  ls_get_clientdata,ls_get_plantdata,
                  ls_get_forecastparameters,ls_get_planningdata,ls_get_storagelocationdata,
                  ls_get_valuationdata,ls_get_warehousenumberdata,ls_get_salesdata,
                  ls_get_storagetypedata,lt_get_desc,lt_get_uom,lt_get_tax,lt_get_returnmes,
                  ls_headdata,ls_clidaout,ls_clidaoutx,ls_plantdata,ls_plantdatax,
                  ls_forecastparameters,ls_forecastparametersx,ls_planningdata,
                  ls_planningdatax,ls_storagelocationdata,ls_storagelocationdatax,
                  ls_val_data,ls_valuationdatax,ls_warehousenumberdata,
                  ls_warehousenumberdatax,ls_salesdata,ls_salesdatax,
                  ls_storagetypedata,ls_storagetypedatax,ls_valuationdata,
                  ls_valuationdatacwmx,lt_mat_desc,lt_uom,lt_uomx,lt_mlan,lt_get_tax.
        endloop.

        if lv_error = 'X'.
          exit.
        endif.

      endloop.

      if me->error_logged( ) = abap_true.
*-->Exit from Loop and do not process further
        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else.

* Commit to release the locks
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = abap_true.

*--> Insert the new material to GT_MATDATA so that same material will not be processed
        ls_matdata1-matnr = iv_confirmed_matnr.
        insert ls_matdata1 into table gt_matdata.

*Create Classification for material
        me->create_classification( exporting iv_super_matnr = lv_super_matnr
                                             iv_matnr       = lv_matnr ).
      endif.

    endif.

  endmethod.


  method ERROR_LOGGED.
    rv_flag = boolc( line_exists( me->gt_appl_log[ type = 'E' ] ) or
                     line_exists( me->gt_appl_log[ type = 'A' ] ) or
                     line_exists( me->gt_appl_log[ type = 'X' ] ) ).
  endmethod.


  METHOD fill_x_struct_clientdata.

    CLEAR es_clientdatax.

    IF is_clientdata-del_flag IS NOT INITIAL.
      es_clientdatax-del_flag = abap_true.
    ENDIF.

    IF is_clientdata-matl_group IS NOT INITIAL.
      es_clientdatax-matl_group = abap_true.
    ENDIF.

    IF is_clientdata-old_mat_no IS NOT INITIAL.
      es_clientdatax-old_mat_no = abap_true.
    ENDIF.

    IF is_clientdata-base_uom IS NOT INITIAL.
      es_clientdatax-base_uom = abap_true.
    ENDIF.

    IF is_clientdata-base_uom_iso IS NOT INITIAL.
      es_clientdatax-base_uom_iso = abap_true.
    ENDIF.

    IF is_clientdata-po_unit IS NOT INITIAL.
      es_clientdatax-po_unit = abap_true.
    ENDIF.

    IF is_clientdata-po_unit_iso IS NOT INITIAL.
      es_clientdatax-po_unit_iso = abap_true.
    ENDIF.

    IF is_clientdata-document IS NOT INITIAL.
      es_clientdatax-document = abap_true.
    ENDIF.


    IF is_clientdata-doc_type IS NOT INITIAL.
      es_clientdatax-doc_type = abap_true.
    ENDIF.

    IF is_clientdata-doc_vers IS NOT INITIAL.
      es_clientdatax-doc_vers = abap_true.
    ENDIF.

    IF is_clientdata-doc_format IS NOT INITIAL.
      es_clientdatax-doc_format = abap_true.
    ENDIF.

    IF is_clientdata-page_no IS NOT INITIAL.
      es_clientdatax-page_no = abap_true.
    ENDIF.

    IF is_clientdata-prod_memo IS NOT INITIAL.
      es_clientdatax-prod_memo = abap_true.
    ENDIF.

    IF is_clientdata-pageformat IS NOT INITIAL.
      es_clientdatax-pageformat = abap_true.
    ENDIF.

    IF is_clientdata-size_dim IS NOT INITIAL.
      es_clientdatax-size_dim = abap_true.
    ENDIF.

    IF is_clientdata-basic_matl IS NOT INITIAL.
      es_clientdatax-basic_matl = abap_true.
    ENDIF.

    IF is_clientdata-std_descr IS NOT INITIAL.
      es_clientdatax-std_descr = abap_true.
    ENDIF.

    IF is_clientdata-dsn_office IS NOT INITIAL.
      es_clientdatax-dsn_office = abap_true.
    ENDIF.

    IF is_clientdata-pur_valkey IS NOT INITIAL.
      es_clientdatax-pur_valkey = abap_true.
    ENDIF.

    IF is_clientdata-net_weight IS NOT INITIAL.
      es_clientdatax-net_weight = abap_true.
    ENDIF.

    IF is_clientdata-unit_of_wt IS NOT INITIAL.
      es_clientdatax-unit_of_wt = abap_true.
    ENDIF.


    IF is_clientdata-unit_of_wt IS NOT INITIAL.
      es_clientdatax-unit_of_wt = abap_true.
    ENDIF.

    IF is_clientdata-unit_of_wt_iso IS NOT INITIAL.
      es_clientdatax-unit_of_wt_iso = abap_true.
    ENDIF.

    IF is_clientdata-container IS NOT INITIAL.
      es_clientdatax-container = abap_true.
    ENDIF.

    IF is_clientdata-stor_conds IS NOT INITIAL.
      es_clientdatax-stor_conds = abap_true.
    ENDIF.

    IF is_clientdata-temp_conds IS NOT INITIAL.
      es_clientdatax-temp_conds = abap_true.
    ENDIF.

    IF is_clientdata-unit_of_wt IS NOT INITIAL.
      es_clientdatax-unit_of_wt = abap_true.
    ENDIF.

    IF is_clientdata-trans_grp IS NOT INITIAL.
      es_clientdatax-trans_grp = abap_true.
    ENDIF.
    IF is_clientdata-haz_mat_no IS NOT INITIAL.
      es_clientdatax-haz_mat_no = abap_true.
    ENDIF.

    IF is_clientdata-division IS NOT INITIAL.
      es_clientdatax-division = abap_true.
    ENDIF.

    IF is_clientdata-competitor IS NOT INITIAL.
      es_clientdatax-competitor = abap_true.
    ENDIF.
    IF is_clientdata-qty_gr_gi IS NOT INITIAL.
      es_clientdatax-qty_gr_gi = abap_true.
    ENDIF.

    IF is_clientdata-proc_rule IS NOT INITIAL.
      es_clientdatax-proc_rule = abap_true.
    ENDIF.

    IF is_clientdata-sup_source IS NOT INITIAL.
      es_clientdatax-sup_source = abap_true.
    ENDIF.

    IF is_clientdata-sup_source IS NOT INITIAL.
      es_clientdatax-sup_source = abap_true.
    ENDIF.

    IF is_clientdata-season IS NOT INITIAL.
      es_clientdatax-season = abap_true.
    ENDIF.

    IF is_clientdata-label_type IS NOT INITIAL.
      es_clientdatax-label_type = abap_true.
    ENDIF.

    IF is_clientdata-label_form IS NOT INITIAL.
      es_clientdatax-label_form = abap_true.
    ENDIF.

    IF is_clientdata-prod_hier IS NOT INITIAL.
      es_clientdatax-prod_hier = abap_true.
    ENDIF.

    IF is_clientdata-cad_id IS NOT INITIAL.
      es_clientdatax-cad_id = abap_true.
    ENDIF.

    IF is_clientdata-allowed_wt IS NOT INITIAL.
      es_clientdatax-allowed_wt = abap_true.
    ENDIF.

    IF is_clientdata-pack_wt_un IS NOT INITIAL.
      es_clientdatax-pack_wt_un = abap_true.
    ENDIF.

    IF is_clientdata-sup_source IS NOT INITIAL.
      es_clientdatax-sup_source = abap_true.
    ENDIF.

    IF is_clientdata-pack_wt_un_iso IS NOT INITIAL.
      es_clientdatax-pack_wt_un_iso = abap_true.
    ENDIF.

    IF is_clientdata-allwd_vol IS NOT INITIAL.
      es_clientdatax-allwd_vol = abap_true.
    ENDIF.

    IF is_clientdata-pack_vo_un IS NOT INITIAL.
      es_clientdatax-pack_vo_un = abap_true.
    ENDIF.

    IF is_clientdata-pack_vo_un_iso IS NOT INITIAL.
      es_clientdatax-pack_vo_un_iso = abap_true.
    ENDIF.

    IF is_clientdata-wt_tol_lt IS NOT INITIAL.
      es_clientdatax-wt_tol_lt = abap_true.
    ENDIF.


    IF is_clientdata-vol_tol_lt IS NOT INITIAL.
      es_clientdatax-vol_tol_lt = abap_true.
    ENDIF.


    IF is_clientdata-var_ord_un IS NOT INITIAL.
      es_clientdatax-var_ord_un = abap_true.
    ENDIF.

    IF is_clientdata-batch_mgmt IS NOT INITIAL.
      es_clientdatax-batch_mgmt = abap_true.
    ENDIF.

    IF is_clientdata-sh_mat_typ IS NOT INITIAL.
      es_clientdatax-sh_mat_typ = abap_true.
    ENDIF.

    IF is_clientdata-stack_fact IS NOT INITIAL.
      es_clientdatax-stack_fact = abap_true.
    ENDIF.

    IF is_clientdata-mat_grp_sm IS NOT INITIAL.
      es_clientdatax-mat_grp_sm = abap_true.
    ENDIF.

    IF is_clientdata-authoritygroup IS NOT INITIAL.
      es_clientdatax-authoritygroup = abap_true.
    ENDIF.

    IF is_clientdata-qm_procmnt IS NOT INITIAL.
      es_clientdatax-qm_procmnt = abap_true.
    ENDIF.

    IF is_clientdata-catprofile IS NOT INITIAL.
      es_clientdatax-catprofile = abap_true.
    ENDIF.

    IF is_clientdata-minremlife IS NOT INITIAL.
      es_clientdatax-minremlife = abap_true.
    ENDIF.

    IF is_clientdata-shelf_life IS NOT INITIAL.
      es_clientdatax-shelf_life = abap_true.
    ENDIF.

    IF is_clientdata-stor_pct IS NOT INITIAL.
      es_clientdatax-stor_pct = abap_true.
    ENDIF.

    IF is_clientdata-pur_status IS NOT INITIAL.
      es_clientdatax-pur_status = abap_true.
    ENDIF.

    IF is_clientdata-sal_status IS NOT INITIAL.
      es_clientdatax-sal_status = abap_true.
    ENDIF.

    IF is_clientdata-pvalidfrom IS NOT INITIAL.
      es_clientdatax-pvalidfrom = abap_true.
    ENDIF.

    IF is_clientdata-svalidfrom IS NOT INITIAL.
      es_clientdatax-svalidfrom = abap_true.
    ENDIF.

    IF is_clientdata-envt_rlvt IS NOT INITIAL.
      es_clientdatax-envt_rlvt = abap_true.
    ENDIF.

    IF is_clientdata-prod_alloc IS NOT INITIAL.
      es_clientdatax-prod_alloc = abap_true.
    ENDIF.

    IF is_clientdata-qual_dik IS NOT INITIAL.
      es_clientdatax-qual_dik = abap_true.
    ENDIF.

    IF is_clientdata-manu_mat IS NOT INITIAL.
      es_clientdatax-manu_mat = abap_true.
    ENDIF.

    IF is_clientdata-mfr_no IS NOT INITIAL.
      es_clientdatax-mfr_no = abap_true.
    ENDIF.

    IF is_clientdata-inv_mat_no IS NOT INITIAL.
      es_clientdatax-inv_mat_no = abap_true.
    ENDIF.

    IF is_clientdata-manuf_prof IS NOT INITIAL.
      es_clientdatax-manuf_prof = abap_true.
    ENDIF.

    IF is_clientdata-hazmatprof IS NOT INITIAL.
      es_clientdatax-hazmatprof = abap_true.
    ENDIF.

    IF is_clientdata-high_visc IS NOT INITIAL.
      es_clientdatax-high_visc = abap_true.
    ENDIF.

    IF is_clientdata-old_mat_no IS NOT INITIAL.
      es_clientdatax-old_mat_no = abap_true.
    ENDIF.

    IF is_clientdata-looseorliq IS NOT INITIAL.
      es_clientdatax-looseorliq = abap_true.
    ENDIF.

    IF is_clientdata-closed_box IS NOT INITIAL.
      es_clientdatax-closed_box = abap_true.
    ENDIF.

    IF is_clientdata-po_unit IS NOT INITIAL.
      es_clientdatax-po_unit = abap_true.
    ENDIF.

    IF is_clientdata-appd_b_rec IS NOT INITIAL.
      es_clientdatax-appd_b_rec = abap_true.
    ENDIF.

    IF is_clientdata-matcmpllvl IS NOT INITIAL.
      es_clientdatax-matcmpllvl = abap_true.
    ENDIF.


    IF is_clientdata-doc_type IS NOT INITIAL.
      es_clientdatax-doc_type = abap_true.
    ENDIF.

    IF is_clientdata-par_eff IS NOT INITIAL.
      es_clientdatax-par_eff = abap_true.
    ENDIF.

    IF is_clientdata-round_up_rule_expiration_date IS NOT INITIAL.
      es_clientdatax-round_up_rule_expiration_date = abap_true.
    ENDIF.

    IF is_clientdata-period_ind_expiration_date IS NOT INITIAL.
      es_clientdatax-period_ind_expiration_date = abap_true.
    ENDIF.

    IF is_clientdata-prod_composition_on_packaging IS NOT INITIAL.
      es_clientdatax-prod_composition_on_packaging = abap_true.
    ENDIF.

    IF is_clientdata-item_cat IS NOT INITIAL.
      es_clientdatax-item_cat = abap_true.
    ENDIF.

    IF is_clientdata-haz_mat_no_external IS NOT INITIAL.
      es_clientdatax-haz_mat_no_external = abap_true.
    ENDIF.

    IF is_clientdata-haz_mat_no_guid IS NOT INITIAL.
      es_clientdatax-haz_mat_no_guid = abap_true.
    ENDIF.

    IF is_clientdata-haz_mat_no_version IS NOT INITIAL.
      es_clientdatax-haz_mat_no_version = abap_true.
    ENDIF.

    IF is_clientdata-inv_mat_no_external IS NOT INITIAL.
      es_clientdatax-inv_mat_no_external = abap_true.
    ENDIF.

    IF is_clientdata-inv_mat_no_guid IS NOT INITIAL.
      es_clientdatax-inv_mat_no_guid = abap_true.
    ENDIF.

    IF is_clientdata-inv_mat_no_version IS NOT INITIAL.
      es_clientdatax-inv_mat_no_version = abap_true.
    ENDIF.

    IF is_clientdata-material_fixed IS NOT INITIAL.
      es_clientdatax-material_fixed = abap_true.
    ENDIF.


    IF is_clientdata-cm_relevance_flag IS NOT INITIAL.
      es_clientdatax-cm_relevance_flag = abap_true.
    ENDIF.

    IF is_clientdata-unit_of_wt_iso IS NOT INITIAL.
      es_clientdatax-unit_of_wt_iso = abap_true.
    ENDIF.

    IF is_clientdata-sled_bbd IS NOT INITIAL.
      es_clientdatax-sled_bbd = abap_true.
    ENDIF.

    IF is_clientdata-gtin_variant IS NOT INITIAL.
      es_clientdatax-gtin_variant = abap_true.
    ENDIF.

    IF is_clientdata-serialization_level IS NOT INITIAL.
      es_clientdatax-serialization_level = abap_true.
    ENDIF.

    IF is_clientdata-unit_of_wt IS NOT INITIAL.
      es_clientdatax-unit_of_wt = abap_true.
    ENDIF.

    IF is_clientdata-pl_ref_mat IS NOT INITIAL.
      es_clientdatax-pl_ref_mat = abap_true.
    ENDIF.
    IF is_clientdata-haz_mat_no IS NOT INITIAL.
      es_clientdatax-haz_mat_no = abap_true.
    ENDIF.

    IF is_clientdata-extmatlgrp IS NOT INITIAL.
      es_clientdatax-extmatlgrp = abap_true.
    ENDIF.

    IF is_clientdata-uomusage IS NOT INITIAL.
      es_clientdatax-uomusage = abap_true.
    ENDIF.


    IF is_clientdata-gds_relevant IS NOT INITIAL.
      es_clientdatax-gds_relevant = abap_true.
    ENDIF.

    IF is_clientdata-pl_ref_mat_external IS NOT INITIAL.
      es_clientdatax-pl_ref_mat_external = abap_true.
    ENDIF.

    IF is_clientdata-pl_ref_mat_guid IS NOT INITIAL.
      es_clientdatax-pl_ref_mat_guid = abap_true.
    ENDIF.

    IF is_clientdata-pl_ref_mat_version IS NOT INITIAL.
      es_clientdatax-pl_ref_mat_version = abap_true.
    ENDIF.

    IF is_clientdata-we_origin_acceptance IS NOT INITIAL.
      es_clientdatax-we_origin_acceptance = abap_true.
    ENDIF.

    IF is_clientdata-std_hu_type IS NOT INITIAL.
      es_clientdatax-std_hu_type = abap_true.
    ENDIF.

    IF is_clientdata-pilferable IS NOT INITIAL.
      es_clientdatax-pilferable = abap_true.
    ENDIF.

    IF is_clientdata-whse_storage_condition IS NOT INITIAL.
      es_clientdatax-whse_storage_condition = abap_true.
    ENDIF.

    IF is_clientdata-cad_id IS NOT INITIAL.
      es_clientdatax-cad_id = abap_true.
    ENDIF.

    IF is_clientdata-whse_material_group IS NOT INITIAL.
      es_clientdatax-whse_material_group = abap_true.
    ENDIF.

    IF is_clientdata-handling_indicator IS NOT INITIAL.
      es_clientdatax-handling_indicator = abap_true.
    ENDIF.

    IF is_clientdata-haz_mat_relevant IS NOT INITIAL.
      es_clientdatax-haz_mat_relevant = abap_true.
    ENDIF.

    IF is_clientdata-hu_type IS NOT INITIAL.
      es_clientdatax-hu_type = abap_true.
    ENDIF.

    IF is_clientdata-variable_tare_weight IS NOT INITIAL.
      es_clientdatax-variable_tare_weight = abap_true.
    ENDIF.

    IF is_clientdata-pack_vo_un IS NOT INITIAL.
      es_clientdatax-pack_vo_un = abap_true.
    ENDIF.

    IF is_clientdata-max_allowed_capacity IS NOT INITIAL.
      es_clientdatax-max_allowed_capacity = abap_true.
    ENDIF.

    IF is_clientdata-overcapacity_tolerance IS NOT INITIAL.
      es_clientdatax-overcapacity_tolerance = abap_true.
    ENDIF.


    IF is_clientdata-max_allowed_length IS NOT INITIAL.
      es_clientdatax-max_allowed_length = abap_true.
    ENDIF.

    IF is_clientdata-max_allowed_width IS NOT INITIAL.
      es_clientdatax-max_allowed_width = abap_true.
    ENDIF.

    IF is_clientdata-max_allowed_heigth IS NOT INITIAL.
      es_clientdatax-max_allowed_heigth = abap_true.
    ENDIF.

    IF is_clientdata-batch_mgmt IS NOT INITIAL.
      es_clientdatax-batch_mgmt = abap_true.
    ENDIF.

    IF is_clientdata-max_dimension_un IS NOT INITIAL.
      es_clientdatax-max_dimension_un = abap_true.
    ENDIF.

    IF is_clientdata-max_dimension_un_iso IS NOT INITIAL.
      es_clientdatax-max_dimension_un_iso = abap_true.
    ENDIF.

    IF is_clientdata-countryori IS NOT INITIAL.
      es_clientdatax-countryori = abap_true.
    ENDIF.

    IF is_clientdata-countryori_iso IS NOT INITIAL.
      es_clientdatax-countryori_iso = abap_true.
    ENDIF.

    IF is_clientdata-matfrgtgrp IS NOT INITIAL.
      es_clientdatax-matfrgtgrp = abap_true.
    ENDIF.

    IF is_clientdata-quarantine_period IS NOT INITIAL.
      es_clientdatax-quarantine_period = abap_true.
    ENDIF.

    IF is_clientdata-quarantine_period_un IS NOT INITIAL.
      es_clientdatax-quarantine_period_un = abap_true.
    ENDIF.

    IF is_clientdata-quarantine_period_un_iso IS NOT INITIAL.
      es_clientdatax-quarantine_period_un_iso = abap_true.
    ENDIF.

    IF is_clientdata-quality_insp_grp IS NOT INITIAL.
      es_clientdatax-quality_insp_grp = abap_true.
    ENDIF.

    IF is_clientdata-serial_number_profile IS NOT INITIAL.
      es_clientdatax-serial_number_profile = abap_true.
    ENDIF.

    IF is_clientdata-ewm_cw_tolerance_group IS NOT INITIAL.
      es_clientdatax-ewm_cw_tolerance_group = abap_true.
    ENDIF.

    IF is_clientdata-ewm_cw_input_control IS NOT INITIAL.
      es_clientdatax-ewm_cw_input_control = abap_true.
    ENDIF.

    IF is_clientdata-pckging_smartform IS NOT INITIAL.
      es_clientdatax-pckging_smartform = abap_true.
    ENDIF.

    IF is_clientdata-pacod IS NOT INITIAL.
      es_clientdatax-pacod = abap_true.
    ENDIF.

    IF is_clientdata-dg_pckging_status IS NOT INITIAL.
      es_clientdatax-dg_pckging_status = abap_true.
    ENDIF.

    IF is_clientdata-adjust_profile IS NOT INITIAL.
      es_clientdatax-adjust_profile = abap_true.
    ENDIF.

    IF is_clientdata-medium IS NOT INITIAL.
      es_clientdatax-medium = abap_true.
    ENDIF.

    IF is_clientdata-nsnid IS NOT INITIAL.
      es_clientdatax-nsnid = abap_true.
    ENDIF.

    IF is_clientdata-physical_commodity IS NOT INITIAL.
      es_clientdatax-physical_commodity = abap_true.
    ENDIF.

    IF is_clientdata-segmentation_structure IS NOT INITIAL.
      es_clientdatax-segmentation_structure = abap_true.
    ENDIF.

    IF is_clientdata-segmentation_strategy IS NOT INITIAL.
      es_clientdatax-segmentation_strategy = abap_true.
    ENDIF.

    IF is_clientdata-segmentation_relevance IS NOT INITIAL.
      es_clientdatax-segmentation_relevance = abap_true.
    ENDIF.

    IF is_clientdata-anp IS NOT INITIAL.
      es_clientdatax-anp = abap_true.
    ENDIF.

    IF is_clientdata-old_mat_no_long IS NOT INITIAL.
      es_clientdatax-old_mat_no_long = abap_true.
    ENDIF.

    IF is_clientdata-haz_mat_no_long IS NOT INITIAL.
      es_clientdatax-haz_mat_no_long = abap_true.
    ENDIF.

    IF is_clientdata-inv_mat_no_long IS NOT INITIAL.
      es_clientdatax-inv_mat_no_long = abap_true.
    ENDIF.

    IF is_clientdata-pl_ref_mat_long IS NOT INITIAL.
      es_clientdatax-pl_ref_mat_long = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD fill_x_struct_forecast.
*"""""""""""""""""""""""""""""""""""""""""""""""""""""
*DESCRIPTION : FIll X structure for Forecast
* AUTHOR     : C5223321
* DATE       : 16/12/2016
*"""""""""""""""""""""""""""""""""""""""""""""""""""""

    CLEAR es_forecastx.

    IF is_forecast-plant IS NOT INITIAL.
      es_forecastx-plant = abap_true.
    ENDIF.

    IF is_forecast-fore_prof IS NOT INITIAL.
      es_forecastx-fore_prof = abap_true.
    ENDIF.

    IF is_forecast-model_si IS NOT INITIAL.
      es_forecastx-model_si = abap_true.
    ENDIF.
    IF is_forecast-model_sp IS NOT INITIAL.
      es_forecastx-model_sp = abap_true.
    ENDIF.

    IF is_forecast-param_opt IS NOT INITIAL.
      es_forecastx-param_opt = abap_true.
    ENDIF.
    IF is_forecast-optim_lev IS NOT INITIAL.
      es_forecastx-optim_lev = abap_true.
    ENDIF.

    IF is_forecast-initialize IS NOT INITIAL.
      es_forecastx-initialize = abap_true.
    ENDIF.

    IF is_forecast-fore_model IS NOT INITIAL.
      es_forecastx-fore_model = abap_true.
    ENDIF.

    IF is_forecast-alpha_fact IS NOT INITIAL.
      es_forecastx-alpha_fact = abap_true.
    ENDIF.

    IF is_forecast-beta_fact IS NOT INITIAL.
      es_forecastx-beta_fact = abap_true.
    ENDIF.

    IF is_forecast-gamma_fact IS NOT INITIAL.
      es_forecastx-gamma_fact = abap_true.
    ENDIF.

    IF is_forecast-delta_fact IS NOT INITIAL.
      es_forecastx-delta_fact = abap_true.
    ENDIF.

    IF is_forecast-tracklimit IS NOT INITIAL.
      es_forecastx-tracklimit = abap_true.
    ENDIF.

    IF is_forecast-fore_date IS NOT INITIAL.
      es_forecastx-fore_date = abap_true.
    ENDIF.

    IF is_forecast-hist_vals IS NOT INITIAL.
      es_forecastx-hist_vals = abap_true.
    ENDIF.

    IF is_forecast-plant IS NOT INITIAL.
      es_forecastx-plant = abap_true.
    ENDIF.

    IF is_forecast-init_pds IS NOT INITIAL.
      es_forecastx-init_pds = abap_true.
    ENDIF.

    IF is_forecast-season_pds IS NOT INITIAL.
      es_forecastx-season_pds = abap_true.
    ENDIF.

    IF is_forecast-expost_pds IS NOT INITIAL.
      es_forecastx-expost_pds = abap_true.
    ENDIF.

    IF is_forecast-fore_pds IS NOT INITIAL.
      es_forecastx-fore_pds = abap_true.
    ENDIF.

    IF is_forecast-fixed_pds IS NOT INITIAL.
      es_forecastx-fixed_pds = abap_true.
    ENDIF.

    IF is_forecast-wtg_group IS NOT INITIAL.
      es_forecastx-wtg_group = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD fill_x_struct_planningdata.


    CLEAR es_planningdatax.

    IF is_planningdata-plant IS NOT INITIAL.
      es_planningdatax-plant = abap_true.
    ENDIF.

    IF is_planningdata-plng_matl IS NOT INITIAL.
      es_planningdatax-plng_matl = abap_true.
    ENDIF.

    IF is_planningdata-plng_plant IS NOT INITIAL.
      es_planningdatax-plng_plant = abap_true.
    ENDIF.

    IF is_planningdata-convfactor IS NOT INITIAL.
      es_planningdatax-convfactor = abap_true.
    ENDIF.

    IF is_planningdata-plng_matl_external IS NOT INITIAL.
      es_planningdatax-plng_matl_external = abap_true.
    ENDIF.
    IF is_planningdata-plng_matl_guid IS NOT INITIAL.
      es_planningdatax-plng_matl_guid = abap_true.
    ENDIF.

    IF is_planningdata-plng_matl_version IS NOT INITIAL.
      es_planningdatax-plng_matl_version = abap_true.
    ENDIF.

    IF is_planningdata-plng_matl_long IS NOT INITIAL.
      es_planningdatax-plng_matl_long = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD fill_x_struct_plantdata.

    CLEAR es_plantdatax.

    IF is_plantdata-plant IS NOT INITIAL.
      es_plantdatax-plant = abap_true.
    ENDIF.

    IF is_plantdata-del_flag IS NOT INITIAL.
      es_plantdatax-del_flag = abap_true.
    ENDIF.


    IF is_plantdata-abc_id IS NOT INITIAL.
      es_plantdatax-abc_id = abap_true.
    ENDIF.


    IF is_plantdata-crit_part IS NOT INITIAL.
      es_plantdatax-crit_part = abap_true.
    ENDIF.


    IF is_plantdata-pur_group IS NOT INITIAL.
      es_plantdatax-pur_group = abap_true.
    ENDIF.

    IF is_plantdata-issue_unit IS NOT INITIAL.
      es_plantdatax-issue_unit = abap_true.
    ENDIF.

    IF is_plantdata-issue_unit_iso IS NOT INITIAL.
      es_plantdatax-issue_unit_iso = abap_true.
    ENDIF.

    IF is_plantdata-mrpprofile IS NOT INITIAL.
      es_plantdatax-mrpprofile = abap_true.
    ENDIF.

    IF is_plantdata-mrp_type IS NOT INITIAL.
      es_plantdatax-mrp_type = abap_true.
    ENDIF.

    IF is_plantdata-mrp_ctrler IS NOT INITIAL.
      es_plantdatax-mrp_ctrler = abap_true.
    ENDIF.

    IF is_plantdata-plnd_delry IS NOT INITIAL.
      es_plantdatax-plnd_delry = abap_true.
    ENDIF.

    IF is_plantdata-gr_pr_time IS NOT INITIAL.
      es_plantdatax-gr_pr_time = abap_true.
    ENDIF.

    IF is_plantdata-period_ind IS NOT INITIAL.
      es_plantdatax-period_ind = abap_true.
    ENDIF.

    IF is_plantdata-assy_scrap IS NOT INITIAL.
      es_plantdatax-assy_scrap = abap_true.
    ENDIF.

    IF is_plantdata-lotsizekey IS NOT INITIAL.
      es_plantdatax-lotsizekey = abap_true.
    ENDIF.

    IF is_plantdata-proc_type IS NOT INITIAL.
      es_plantdatax-proc_type = abap_true.
    ENDIF.

    IF is_plantdata-spproctype IS NOT INITIAL.
      es_plantdatax-spproctype = abap_true.
    ENDIF.

    IF is_plantdata-reorder_pt IS NOT INITIAL.
      es_plantdatax-reorder_pt = abap_true.
    ENDIF.

    IF is_plantdata-safety_stk IS NOT INITIAL.
      es_plantdatax-safety_stk  = abap_true.
    ENDIF.
    IF is_plantdata-minlotsize IS NOT INITIAL.
      es_plantdatax-minlotsize = abap_true.
    ENDIF.

    IF is_plantdata-fixed_lot IS NOT INITIAL.
      es_plantdatax-fixed_lot = abap_true.
    ENDIF.

    IF is_plantdata-round_val IS NOT INITIAL.
      es_plantdatax-round_val = abap_true.
    ENDIF.

    IF is_plantdata-max_stock IS NOT INITIAL.
      es_plantdatax-max_stock = abap_true.
    ENDIF.

    IF is_plantdata-ord_costs IS NOT INITIAL.
      es_plantdatax-ord_costs = abap_true.
    ENDIF.

    IF is_plantdata-dep_req_id IS NOT INITIAL.
      es_plantdatax-dep_req_id = abap_true.
    ENDIF.

    IF is_plantdata-stor_costs IS NOT INITIAL.
      es_plantdatax-stor_costs = abap_true.
    ENDIF.

    IF is_plantdata-alt_bom_id IS NOT INITIAL.
      es_plantdatax-alt_bom_id  = abap_true.
    ENDIF.

    IF is_plantdata-discontinu IS NOT INITIAL.
      es_plantdatax-discontinu = abap_true.
    ENDIF.

    IF is_plantdata-eff_o_day IS NOT INITIAL.
      es_plantdatax-eff_o_day = abap_true.
    ENDIF.

    IF is_plantdata-follow_up IS NOT INITIAL.
      es_plantdatax-follow_up = abap_true.
    ENDIF.
    IF is_plantdata-grp_reqmts IS NOT INITIAL.
      es_plantdatax-grp_reqmts = abap_true.
    ENDIF.


    IF is_plantdata-mixed_mrp IS NOT INITIAL.
      es_plantdatax-mixed_mrp = abap_true.
    ENDIF.

    IF is_plantdata-sm_key IS NOT INITIAL.
      es_plantdatax-sm_key = abap_true.
    ENDIF.

    IF is_plantdata-backflush IS NOT INITIAL.
      es_plantdatax-backflush = abap_true.
    ENDIF.

    IF is_plantdata-production_scheduler IS NOT INITIAL.
      es_plantdatax-production_scheduler = abap_true.
    ENDIF.

    IF is_plantdata-proc_time IS NOT INITIAL.
      es_plantdatax-proc_time = abap_true.
    ENDIF.

    IF is_plantdata-setuptime IS NOT INITIAL.
      es_plantdatax-setuptime = abap_true.
    ENDIF.

    IF is_plantdata-interop IS NOT INITIAL.
      es_plantdatax-interop = abap_true.
    ENDIF.

    IF is_plantdata-base_qty IS NOT INITIAL.
      es_plantdatax-base_qty = abap_true.
    ENDIF.

    IF is_plantdata-inhseprodt IS NOT INITIAL.
      es_plantdatax-inhseprodt = abap_true.
    ENDIF.

    IF is_plantdata-stgeperiod IS NOT INITIAL.
      es_plantdatax-stgeperiod = abap_true.
    ENDIF.

    IF is_plantdata-stge_pd_un IS NOT INITIAL.
      es_plantdatax-stge_pd_un = abap_true.
    ENDIF.

    IF is_plantdata-stge_pd_un_iso IS NOT INITIAL.
      es_plantdatax-stge_pd_un_iso = abap_true.
    ENDIF.

    IF is_plantdata-over_tol IS NOT INITIAL.
      es_plantdatax-over_tol = abap_true.
    ENDIF.

    IF is_plantdata-unlimited IS NOT INITIAL.
      es_plantdatax-unlimited = abap_true.
    ENDIF.

    IF is_plantdata-under_tol IS NOT INITIAL.
      es_plantdatax-under_tol = abap_true.
    ENDIF.

    IF is_plantdata-replentime IS NOT INITIAL.
      es_plantdatax-replentime = abap_true.
    ENDIF.

    IF is_plantdata-replace_pt IS NOT INITIAL.
      es_plantdatax-replace_pt = abap_true.
    ENDIF.

    IF is_plantdata-ind_post_to_insp_stock IS NOT INITIAL.
      es_plantdatax-ind_post_to_insp_stock = abap_true.
    ENDIF.

    IF is_plantdata-ctrl_key IS NOT INITIAL.
      es_plantdatax-ctrl_key = abap_true.
    ENDIF.

    IF is_plantdata-doc_reqd IS NOT INITIAL.
      es_plantdatax-doc_reqd = abap_true.
    ENDIF.

    IF is_plantdata-loadinggrp IS NOT INITIAL.
      es_plantdatax-loadinggrp = abap_true.
    ENDIF.

    IF is_plantdata-batch_mgmt IS NOT INITIAL.
      es_plantdatax-batch_mgmt = abap_true.
    ENDIF.

    IF is_plantdata-quotausage IS NOT INITIAL.
      es_plantdatax-quotausage = abap_true.
    ENDIF.

    IF is_plantdata-serv_level IS NOT INITIAL.
      es_plantdatax-serv_level = abap_true.
    ENDIF.

    IF is_plantdata-split_ind IS NOT INITIAL.
      es_plantdatax-split_ind = abap_true.
    ENDIF.

    IF is_plantdata-availcheck IS NOT INITIAL.
      es_plantdatax-availcheck = abap_true.
    ENDIF.


    IF is_plantdata-fy_variant IS NOT INITIAL.
      es_plantdatax-fy_variant = abap_true.
    ENDIF.

    IF is_plantdata-corr_fact IS NOT INITIAL.
      es_plantdatax-corr_fact = abap_true.
    ENDIF.


    IF is_plantdata-setup_time IS NOT INITIAL.
      es_plantdatax-setup_time = abap_true.
    ENDIF.


    IF is_plantdata-base_qty_plan IS NOT INITIAL.
      es_plantdatax-base_qty_plan = abap_true.
    ENDIF.


    IF is_plantdata-ship_proc_time IS NOT INITIAL.
      es_plantdatax-ship_proc_time = abap_true.
    ENDIF.

    IF is_plantdata-sup_source IS NOT INITIAL.
      es_plantdatax-sup_source = abap_true.
    ENDIF.

    IF is_plantdata-auto_p_ord IS NOT INITIAL.
      es_plantdatax-auto_p_ord = abap_true.
    ENDIF.

    IF is_plantdata-sourcelist IS NOT INITIAL.
      es_plantdatax-sourcelist = abap_true.
    ENDIF.

    IF is_plantdata-comm_code IS NOT INITIAL.
      es_plantdatax-comm_code = abap_true.
    ENDIF.

    IF is_plantdata-countryori IS NOT INITIAL.
      es_plantdatax-countryori = abap_true.
    ENDIF.

    IF is_plantdata-countryori_iso IS NOT INITIAL.
      es_plantdatax-countryori_iso = abap_true.
    ENDIF.

    IF is_plantdata-regionorig IS NOT INITIAL.
      es_plantdatax-regionorig = abap_true.
    ENDIF.

    IF is_plantdata-comm_co_un IS NOT INITIAL.
      es_plantdatax-comm_co_un = abap_true.
    ENDIF.

    IF is_plantdata-comm_co_un_iso IS NOT INITIAL.
      es_plantdatax-comm_co_un_iso = abap_true.
    ENDIF.

    IF is_plantdata-expimpgrp IS NOT INITIAL.
      es_plantdatax-expimpgrp = abap_true.
    ENDIF.

    IF is_plantdata-profit_ctr IS NOT INITIAL.
      es_plantdatax-profit_ctr = abap_true.
    ENDIF.

    IF is_plantdata-ppc_pl_cal IS NOT INITIAL.
      es_plantdatax-ppc_pl_cal = abap_true.
    ENDIF.

    IF is_plantdata-rep_manuf IS NOT INITIAL.
      es_plantdatax-rep_manuf = abap_true.
    ENDIF.

    IF is_plantdata-pl_ti_fnce IS NOT INITIAL.
      es_plantdatax-pl_ti_fnce  = abap_true.
    ENDIF.
    IF is_plantdata-consummode IS NOT INITIAL.
      es_plantdatax-consummode = abap_true.
    ENDIF.

    IF is_plantdata-bwd_cons IS NOT INITIAL.
      es_plantdatax-bwd_cons = abap_true.
    ENDIF.

    IF is_plantdata-fwd_cons IS NOT INITIAL.
      es_plantdatax-fwd_cons = abap_true.
    ENDIF.

    IF is_plantdata-alternative_bom IS NOT INITIAL.
      es_plantdatax-alternative_bom = abap_true.
    ENDIF.

    IF is_plantdata-bom_usage IS NOT INITIAL.
      es_plantdatax-bom_usage = abap_true.
    ENDIF.

    IF is_plantdata-planlistgrp IS NOT INITIAL.
      es_plantdatax-planlistgrp = abap_true.
    ENDIF.

    IF is_plantdata-planlistcnt IS NOT INITIAL.
      es_plantdatax-planlistcnt = abap_true.
    ENDIF.

    IF is_plantdata-lot_size IS NOT INITIAL.
      es_plantdatax-lot_size  = abap_true.
    ENDIF.

    IF is_plantdata-specprocty IS NOT INITIAL.
      es_plantdatax-specprocty = abap_true.
    ENDIF.

    IF is_plantdata-prod_unit IS NOT INITIAL.
      es_plantdatax-prod_unit = abap_true.
    ENDIF.

    IF is_plantdata-prod_unit_iso IS NOT INITIAL.
      es_plantdatax-prod_unit_iso = abap_true.
    ENDIF.

    IF is_plantdata-iss_st_loc IS NOT INITIAL.
      es_plantdatax-iss_st_loc = abap_true.
    ENDIF.

    IF is_plantdata-mrp_group IS NOT INITIAL.
      es_plantdatax-mrp_group = abap_true.
    ENDIF.

    IF is_plantdata-comp_scrap IS NOT INITIAL.
      es_plantdatax-comp_scrap = abap_true.
    ENDIF.

    IF is_plantdata-cert_type IS NOT INITIAL.
      es_plantdatax-cert_type = abap_true.
    ENDIF.

    IF is_plantdata-cycle_time IS NOT INITIAL.
      es_plantdatax-cycle_time = abap_true.
    ENDIF.

    IF is_plantdata-covprofile IS NOT INITIAL.
      es_plantdatax-covprofile = abap_true.
    ENDIF.

    IF is_plantdata-cc_ph_inv IS NOT INITIAL.
      es_plantdatax-cc_ph_inv = abap_true.
    ENDIF.

    IF is_plantdata-variance_key IS NOT INITIAL.
      es_plantdatax-variance_key = abap_true.
    ENDIF.

    IF is_plantdata-serno_prof IS NOT INITIAL.
      es_plantdatax-serno_prof = abap_true.
    ENDIF.

    IF is_plantdata-repmanprof IS NOT INITIAL.
      es_plantdatax-repmanprof = abap_true.
    ENDIF.

    IF is_plantdata-neg_stocks IS NOT INITIAL.
      es_plantdatax-neg_stocks = abap_true.
    ENDIF.

    IF is_plantdata-qm_rgmts IS NOT INITIAL.
      es_plantdatax-qm_rgmts = abap_true.
    ENDIF.

    IF is_plantdata-plng_cycle IS NOT INITIAL.
      es_plantdatax-plng_cycle = abap_true.
    ENDIF.

    IF is_plantdata-round_prof IS NOT INITIAL.
      es_plantdatax-round_prof = abap_true.
    ENDIF.

    IF is_plantdata-refmatcons IS NOT INITIAL.
      es_plantdatax-refmatcons = abap_true.
    ENDIF.

    IF is_plantdata-d_to_ref_m IS NOT INITIAL.
      es_plantdatax-d_to_ref_m = abap_true.
    ENDIF.

    IF is_plantdata-mult_ref_m IS NOT INITIAL.
      es_plantdatax-mult_ref_m = abap_true.
    ENDIF.

    IF is_plantdata-auto_reset IS NOT INITIAL.
      es_plantdatax-auto_reset = abap_true.
    ENDIF.

    IF is_plantdata-replace_pt IS NOT INITIAL.
      es_plantdatax-replace_pt = abap_true.
    ENDIF.

    IF is_plantdata-ex_cert_id IS NOT INITIAL.
      es_plantdatax-ex_cert_id = abap_true.
    ENDIF.

    IF is_plantdata-ex_cert_no_new IS NOT INITIAL.
      es_plantdatax-ex_cert_no_new = abap_true.
    ENDIF.

    IF is_plantdata-ex_cert_dt IS NOT INITIAL.
      es_plantdatax-ex_cert_dt = abap_true.
    ENDIF.

    IF is_plantdata-milit_id IS NOT INITIAL.
      es_plantdatax-milit_id = abap_true.
    ENDIF.

    IF is_plantdata-insp_int IS NOT INITIAL.
      es_plantdatax-insp_int = abap_true.
    ENDIF.

    IF is_plantdata-co_product IS NOT INITIAL.
      es_plantdatax-co_product = abap_true.
    ENDIF.

    IF is_plantdata-plan_strgp IS NOT INITIAL.
      es_plantdatax-plan_strgp = abap_true.
    ENDIF.

    IF is_plantdata-sloc_exprc IS NOT INITIAL.
      es_plantdatax-sloc_exprc = abap_true.
    ENDIF.

    IF is_plantdata-bulk_mat IS NOT INITIAL.
      es_plantdatax-bulk_mat = abap_true.
    ENDIF.


    IF is_plantdata-cc_fixed IS NOT INITIAL.
      es_plantdatax-cc_fixed = abap_true.
    ENDIF.

    IF is_plantdata-determ_grp IS NOT INITIAL.
      es_plantdatax-determ_grp = abap_true.
    ENDIF.

    IF is_plantdata-qm_authgrp IS NOT INITIAL.
      es_plantdatax-qm_authgrp = abap_true.
    ENDIF.

    IF is_plantdata-task_list_type IS NOT INITIAL.
      es_plantdatax-task_list_type = abap_true.
    ENDIF.

    IF is_plantdata-pur_status IS NOT INITIAL.
      es_plantdatax-pur_status = abap_true.
    ENDIF.

    IF is_plantdata-prodprof IS NOT INITIAL.
      es_plantdatax-prodprof = abap_true.
    ENDIF.
    IF is_plantdata-safty_t_id IS NOT INITIAL.
      es_plantdatax-safty_t_id = abap_true.
    ENDIF.

    IF is_plantdata-safetytime IS NOT INITIAL.
      es_plantdatax-safetytime = abap_true.
    ENDIF.

    IF is_plantdata-plord_ctrl IS NOT INITIAL.
      es_plantdatax-plord_ctrl = abap_true.
    ENDIF.

    IF is_plantdata-batchentry IS NOT INITIAL.
      es_plantdatax-batchentry = abap_true.
    ENDIF.

    IF is_plantdata-pvalidfrom IS NOT INITIAL.
      es_plantdatax-pvalidfrom = abap_true.
    ENDIF.

    IF is_plantdata-matfrgtgrp IS NOT INITIAL.
      es_plantdatax-matfrgtgrp = abap_true.
    ENDIF.

    IF is_plantdata-prodverscs IS NOT INITIAL.
      es_plantdatax-prodverscs = abap_true.
    ENDIF.

    IF is_plantdata-mat_cfop IS NOT INITIAL.
      es_plantdatax-mat_cfop  = abap_true.
    ENDIF.

    IF is_plantdata-eu_list_no IS NOT INITIAL.
      es_plantdatax-eu_list_no = abap_true.
    ENDIF.

    IF is_plantdata-eu_mat_grp IS NOT INITIAL.
      es_plantdatax-eu_mat_grp = abap_true.
    ENDIF.

    IF is_plantdata-cas_no IS NOT INITIAL.
      es_plantdatax-cas_no = abap_true.
    ENDIF.

    IF is_plantdata-prodcom_no IS NOT INITIAL.
      es_plantdatax-prodcom_no = abap_true.
    ENDIF.

    IF is_plantdata-ctrl_code IS NOT INITIAL.
      es_plantdatax-ctrl_code = abap_true.
    ENDIF.

    IF is_plantdata-jit_relvt IS NOT INITIAL.
      es_plantdatax-jit_relvt = abap_true.
    ENDIF.

    IF is_plantdata-mat_grp_trans IS NOT INITIAL.
      es_plantdatax-mat_grp_trans = abap_true.
    ENDIF.

    IF is_plantdata-handlg_grp IS NOT INITIAL.
      es_plantdatax-handlg_grp = abap_true.
    ENDIF.

    IF is_plantdata-supply_area IS NOT INITIAL.
      es_plantdatax-supply_area = abap_true.
    ENDIF.

    IF is_plantdata-fair_share_rule IS NOT INITIAL.
      es_plantdatax-fair_share_rule = abap_true.
    ENDIF.

    IF is_plantdata-push_distrib IS NOT INITIAL.
      es_plantdatax-push_distrib = abap_true.
    ENDIF.

    IF is_plantdata-deploy_horiz IS NOT INITIAL.
      es_plantdatax-deploy_horiz = abap_true.
    ENDIF.

    IF is_plantdata-min_lot_size IS NOT INITIAL.
      es_plantdatax-min_lot_size = abap_true.
    ENDIF.

    IF is_plantdata-max_lot_size IS NOT INITIAL.
      es_plantdatax-max_lot_size = abap_true.
    ENDIF.

    IF is_plantdata-fix_lot_size IS NOT INITIAL.
      es_plantdatax-fix_lot_size = abap_true.
    ENDIF.

    IF is_plantdata-lot_increment IS NOT INITIAL.
      es_plantdatax-lot_increment = abap_true.
    ENDIF.

    IF is_plantdata-prod_conv_type IS NOT INITIAL.
      es_plantdatax-prod_conv_type  = abap_true.
    ENDIF.

    IF is_plantdata-distr_prof IS NOT INITIAL.
      es_plantdatax-distr_prof = abap_true.
    ENDIF.

    IF is_plantdata-period_profile_safety_time IS NOT INITIAL.
      es_plantdatax-period_profile_safety_time  = abap_true.
    ENDIF.

    IF is_plantdata-fxd_price IS NOT INITIAL.
      es_plantdatax-fxd_price = abap_true.
    ENDIF.

    IF is_plantdata-avail_check_all_proj_segments IS NOT INITIAL.
      es_plantdatax-avail_check_all_proj_segments = abap_true.
    ENDIF.

    IF is_plantdata-overallprf IS NOT INITIAL.
      es_plantdatax-overallprf = abap_true.
    ENDIF.

    IF is_plantdata-mrp_relevancy_dep_requirements IS NOT INITIAL.
      es_plantdatax-mrp_relevancy_dep_requirements = abap_true.
    ENDIF.

    IF is_plantdata-min_safety_stk IS NOT INITIAL.
      es_plantdatax-min_safety_stk = abap_true.
    ENDIF.

    IF is_plantdata-no_costing IS NOT INITIAL.
      es_plantdatax-no_costing = abap_true.
    ENDIF.

    IF is_plantdata-unit_group IS NOT INITIAL.
      es_plantdatax-unit_group = abap_true.
    ENDIF.

    IF is_plantdata-follow_up_external IS NOT INITIAL.
      es_plantdatax-follow_up_external = abap_true.
    ENDIF.

    IF is_plantdata-follow_up_guid IS NOT INITIAL.
      es_plantdatax-follow_up_guid = abap_true.
    ENDIF.

    IF is_plantdata-follow_up_version IS NOT INITIAL.
      es_plantdatax-follow_up_version = abap_true.
    ENDIF.

    IF is_plantdata-refmatcons_external IS NOT INITIAL.
      es_plantdatax-refmatcons_external = abap_true.
    ENDIF.

    IF is_plantdata-refmatcons_guid IS NOT INITIAL.
      es_plantdatax-refmatcons_guid = abap_true.
    ENDIF.


    IF is_plantdata-refmatcons_version IS NOT INITIAL.
      es_plantdatax-refmatcons_version = abap_true.
    ENDIF.

    IF is_plantdata-rotation_date IS NOT INITIAL.
      es_plantdatax-rotation_date = abap_true.
    ENDIF.

    IF is_plantdata-original_batch_flag IS NOT INITIAL.
      es_plantdatax-original_batch_flag = abap_true.
    ENDIF.

    IF is_plantdata-original_batch_ref_material IS NOT INITIAL.
      es_plantdatax-original_batch_ref_material  = abap_true.
    ENDIF.

    IF is_plantdata-original_batch_ref_material_e IS NOT INITIAL.
      es_plantdatax-original_batch_ref_material_e = abap_true.
    ENDIF.

    IF is_plantdata-original_batch_ref_material_v IS NOT INITIAL.
      es_plantdatax-original_batch_ref_material_v = abap_true.
    ENDIF.

    IF is_plantdata-original_batch_ref_material_g IS NOT INITIAL.
      es_plantdatax-original_batch_ref_material_g = abap_true.
    ENDIF.

    IF is_plantdata-iuid_relevant IS NOT INITIAL.
      es_plantdatax-iuid_relevant = abap_true.
    ENDIF.

    IF is_plantdata-iuid_type IS NOT INITIAL.
      es_plantdatax-iuid_type = abap_true.
    ENDIF.

    IF is_plantdata-uid_iea IS NOT INITIAL.
      es_plantdatax-uid_iea = abap_true.
    ENDIF.

    IF is_plantdata-segmentation_strategy IS NOT INITIAL.
      es_plantdatax-segmentation_strategy = abap_true.
    ENDIF.

    IF is_plantdata-segmentation_status IS NOT INITIAL.
      es_plantdatax-segmentation_status = abap_true.
    ENDIF.

    IF is_plantdata-consumption_priority IS NOT INITIAL.
      es_plantdatax-consumption_priority = abap_true.
    ENDIF.

    IF is_plantdata-discrete_batch_flag IS NOT INITIAL.
      es_plantdatax-discrete_batch_flag = abap_true.
    ENDIF.

    IF is_plantdata-stock_protection_ind IS NOT INITIAL.
      es_plantdatax-stock_protection_ind = abap_true.
    ENDIF.

    IF is_plantdata-default_stock_segment IS NOT INITIAL.
      es_plantdatax-default_stock_segment = abap_true.
    ENDIF.

    IF is_plantdata-adv_plng_ind IS NOT INITIAL.
      es_plantdatax-adv_plng_ind = abap_true.
    ENDIF.

    IF is_plantdata-follow_up_long IS NOT INITIAL.
      es_plantdatax-follow_up_long = abap_true.
    ENDIF.

    IF is_plantdata-refmatcons_long IS NOT INITIAL.
      es_plantdatax-refmatcons_long = abap_true.
    ENDIF.

    IF is_plantdata-original_batch_ref_material_l IS NOT INITIAL.
      es_plantdatax-original_batch_ref_material_l = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD fill_x_struct_saledata.

    CLEAR es_saledatax.

    IF is_saledata-sales_org IS NOT INITIAL.
      es_saledatax-sales_org = abap_true.
    ENDIF.

    IF is_saledata-distr_chan IS NOT INITIAL.
      es_saledatax-distr_chan = abap_true.
    ENDIF.

    IF is_saledata-del_flag IS NOT INITIAL.
      es_saledatax-del_flag = abap_true.
    ENDIF.

    IF is_saledata-matl_stats IS NOT INITIAL.
      es_saledatax-matl_stats = abap_true.
    ENDIF.

    IF is_saledata-rebate_grp IS NOT INITIAL.
      es_saledatax-rebate_grp = abap_true.
    ENDIF.

    IF is_saledata-comm_group IS NOT INITIAL.
      es_saledatax-comm_group = abap_true.
    ENDIF.

    IF is_saledata-cash_disc IS NOT INITIAL.
      es_saledatax-cash_disc = abap_true.
    ENDIF.

    IF is_saledata-sal_status IS NOT INITIAL.
      es_saledatax-sal_status = abap_true.
    ENDIF.

    IF is_saledata-valid_from IS NOT INITIAL.
      es_saledatax-valid_from = abap_true.
    ENDIF.

    IF is_saledata-min_order IS NOT INITIAL.
      es_saledatax-min_order = abap_true.
    ENDIF.

    IF is_saledata-min_dely IS NOT INITIAL.
      es_saledatax-min_dely = abap_true.
    ENDIF.

    IF is_saledata-min_mto IS NOT INITIAL.
      es_saledatax-min_mto = abap_true.
    ENDIF.

    IF is_saledata-dely_unit IS NOT INITIAL.
      es_saledatax-dely_unit = abap_true.
    ENDIF.

    IF is_saledata-dely_uom IS NOT INITIAL.
      es_saledatax-dely_uom = abap_true.
    ENDIF.

    IF is_saledata-dely_uom_iso IS NOT INITIAL.
      es_saledatax-dely_uom_iso = abap_true.
    ENDIF.

    IF is_saledata-sales_unit IS NOT INITIAL.
      es_saledatax-sales_unit = abap_true.
    ENDIF.

    IF is_saledata-sales_unit_iso IS NOT INITIAL.
      es_saledatax-sales_unit_iso = abap_true.
    ENDIF.

    IF is_saledata-item_cat IS NOT INITIAL.
      es_saledatax-item_cat = abap_true.
    ENDIF.

    IF is_saledata-delyg_plnt IS NOT INITIAL.
      es_saledatax-delyg_plnt = abap_true.
    ENDIF.

    IF is_saledata-prod_hier IS NOT INITIAL.
      es_saledatax-prod_hier = abap_true.
    ENDIF.

    IF is_saledata-pr_ref_mat IS NOT INITIAL.
      es_saledatax-pr_ref_mat = abap_true.
    ENDIF.

    IF is_saledata-mat_pr_grp IS NOT INITIAL.
      es_saledatax-mat_pr_grp = abap_true.
    ENDIF.

    IF is_saledata-acct_assgt IS NOT INITIAL.
      es_saledatax-acct_assgt = abap_true.
    ENDIF.

    IF is_saledata-matl_grp_1 IS NOT INITIAL.
      es_saledatax-matl_grp_1 = abap_true.
    ENDIF.

    IF is_saledata-matl_grp_2 IS NOT INITIAL.
      es_saledatax-matl_grp_2 = abap_true.
    ENDIF.

    IF is_saledata-matl_grp_3 IS NOT INITIAL.
      es_saledatax-matl_grp_3 = abap_true.
    ENDIF.

    IF is_saledata-matl_grp_4 IS NOT INITIAL.
      es_saledatax-matl_grp_4 = abap_true.
    ENDIF.

    IF is_saledata-matl_grp_5 IS NOT INITIAL.
      es_saledatax-matl_grp_5 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_1 IS NOT INITIAL.
      es_saledatax-prod_att_1 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_2 IS NOT INITIAL.
      es_saledatax-prod_att_2 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_3 IS NOT INITIAL.
      es_saledatax-prod_att_3 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_4 IS NOT INITIAL.
      es_saledatax-prod_att_4 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_5 IS NOT INITIAL.
      es_saledatax-prod_att_5 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_6 IS NOT INITIAL.
      es_saledatax-prod_att_6 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_7 IS NOT INITIAL.
      es_saledatax-prod_att_7 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_8 IS NOT INITIAL.
      es_saledatax-prod_att_8 = abap_true.
    ENDIF.

    IF is_saledata-prod_att_9 IS NOT INITIAL.
      es_saledatax-prod_att_9 = abap_true.
    ENDIF.


    IF is_saledata-prod_att10 IS NOT INITIAL.
      es_saledatax-prod_att10 = abap_true.
    ENDIF.

    IF is_saledata-round_prof IS NOT INITIAL.
      es_saledatax-round_prof = abap_true.
    ENDIF.

    IF is_saledata-var_sales_un IS NOT INITIAL.
      es_saledatax-var_sales_un = abap_true.
    ENDIF.

    IF is_saledata-unit_group IS NOT INITIAL.
      es_saledatax-unit_group = abap_true.
    ENDIF.

    IF is_saledata-pr_ref_mat_external IS NOT INITIAL.
      es_saledatax-pr_ref_mat_external = abap_true.
    ENDIF.

    IF is_saledata-pr_ref_mat_guid IS NOT INITIAL.
      es_saledatax-pr_ref_mat_guid = abap_true.
    ENDIF.

    IF is_saledata-pr_ref_mat_version IS NOT INITIAL.
      es_saledatax-pr_ref_mat_version = abap_true.
    ENDIF.

    IF is_saledata-pr_ref_mat_long IS NOT INITIAL.
      es_saledatax-pr_ref_mat_long = abap_true.
    ENDIF.



  ENDMETHOD.


  METHOD fill_x_struct_storageloc.

    CLEAR es_storagelocdatax.

    IF is_storagelocdata-plant IS NOT INITIAL.
      es_storagelocdatax-plant = abap_true.
    ENDIF.

    IF is_storagelocdata-stge_loc IS NOT INITIAL.
      es_storagelocdatax-stge_loc = abap_true.
    ENDIF.

    IF is_storagelocdata-del_flag IS NOT INITIAL.
      es_storagelocdatax-del_flag = abap_true.
    ENDIF.

    IF is_storagelocdata-mrp_ind IS NOT INITIAL.
      es_storagelocdatax-mrp_ind = abap_true.
    ENDIF.

    IF is_storagelocdata-spec_proc IS NOT INITIAL.
      es_storagelocdatax-spec_proc = abap_true.
    ENDIF.

    IF is_storagelocdata-reorder_pt IS NOT INITIAL.
      es_storagelocdatax-reorder_pt = abap_true.
    ENDIF.

    IF is_storagelocdata-repl_qty IS NOT INITIAL.
      es_storagelocdatax-repl_qty = abap_true.
    ENDIF.

    IF is_storagelocdata-stge_bin IS NOT INITIAL.
      es_storagelocdatax-stge_bin = abap_true.
    ENDIF.

    IF is_storagelocdata-pickg_area IS NOT INITIAL.
      es_storagelocdatax-pickg_area = abap_true.
    ENDIF.

    IF is_storagelocdata-inv_corr_fac IS NOT INITIAL.
      es_storagelocdatax-inv_corr_fac = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD fill_x_struct_storagetypedata.

    CLEAR es_storagetypedatax.

    IF is_storagetypedata-whse_no IS NOT INITIAL.
      es_storagetypedatax-whse_no = abap_true.
    ENDIF.

    IF is_storagetypedata-stge_type IS NOT INITIAL.
      es_storagetypedatax-stge_type = abap_true.
    ENDIF.

    IF is_storagetypedata-del_flag IS NOT INITIAL.
      es_storagetypedatax-del_flag = abap_true.
    ENDIF.

    IF is_storagetypedata-stge_bin IS NOT INITIAL.
      es_storagetypedatax-stge_bin = abap_true.
    ENDIF.

    IF is_storagetypedata-max_sb_qty IS NOT INITIAL.
      es_storagetypedatax-max_sb_qty = abap_true.
    ENDIF.

    IF is_storagetypedata-min_sb_qty IS NOT INITIAL.
      es_storagetypedatax-min_sb_qty = abap_true.
    ENDIF.

    IF is_storagetypedata-ctrl_qty IS NOT INITIAL.
      es_storagetypedatax-ctrl_qty = abap_true.
    ENDIF.

    IF is_storagetypedata-replen_qty IS NOT INITIAL.
      es_storagetypedatax-replen_qty = abap_true.
    ENDIF.

    IF is_storagetypedata-pick_area IS NOT INITIAL.
      es_storagetypedatax-pick_area = abap_true.
    ENDIF.

    IF is_storagetypedata-round_qty IS NOT INITIAL.
      es_storagetypedatax-round_qty = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD fill_x_struct_uom.

    CLEAR es_uomx.

    IF is_uom-alt_unit IS NOT INITIAL.
      es_uomx-alt_unit = abap_true.
    ENDIF.

    IF is_uom-alt_unit_iso IS NOT INITIAL.
      es_uomx-alt_unit_iso = abap_true.
    ENDIF.

    IF is_uom-numerator IS NOT INITIAL.
      es_uomx-numerator = abap_true.
    ENDIF.

    IF is_uom-denominatr IS NOT INITIAL.
      es_uomx-denominatr = abap_true.
    ENDIF.

    IF is_uom-ean_upc IS NOT INITIAL.
      es_uomx-ean_upc = abap_true.
    ENDIF.

    IF is_uom-ean_cat IS NOT INITIAL.
      es_uomx-ean_cat = abap_true.
    ENDIF.

    IF is_uom-length IS NOT INITIAL.
      es_uomx-length = abap_true.
    ENDIF.

    IF is_uom-width IS NOT INITIAL.
      es_uomx-width = abap_true.
    ENDIF.

    IF is_uom-height IS NOT INITIAL.
      es_uomx-height = abap_true.
    ENDIF.

    IF is_uom-unit_dim IS NOT INITIAL.
      es_uomx-unit_dim = abap_true.
    ENDIF.

    IF is_uom-unit_dim_iso IS NOT INITIAL.
      es_uomx-unit_dim_iso = abap_true.
    ENDIF.

    IF is_uom-volume IS NOT INITIAL.
      es_uomx-volume = abap_true.
    ENDIF.

    IF is_uom-volumeunit IS NOT INITIAL.
      es_uomx-volumeunit = abap_true.
    ENDIF.

    IF is_uom-volumeunit_iso IS NOT INITIAL.
      es_uomx-volumeunit_iso = abap_true.
    ENDIF.

    IF is_uom-gross_wt IS NOT INITIAL.
      es_uomx-gross_wt = abap_true.
    ENDIF.

    IF is_uom-unit_of_wt IS NOT INITIAL.
      es_uomx-unit_of_wt = abap_true.
    ENDIF.

    IF is_uom-unit_of_wt_iso IS NOT INITIAL.
      es_uomx-unit_of_wt_iso = abap_true.
    ENDIF.

    IF is_uom-sub_uom IS NOT INITIAL.
      es_uomx-sub_uom = abap_true.
    ENDIF.

    IF is_uom-sub_uom IS NOT INITIAL.
      es_uomx-sub_uom = abap_true.
    ENDIF.

    IF is_uom-sub_uom_iso IS NOT INITIAL.
      es_uomx-sub_uom_iso = abap_true.
    ENDIF.

    IF is_uom-gtin_variant IS NOT INITIAL.
      es_uomx-gtin_variant = abap_true.
    ENDIF.

    IF is_uom-nesting_factor IS NOT INITIAL.
      es_uomx-nesting_factor = abap_true.
    ENDIF.

    IF is_uom-maximum_stacking IS NOT INITIAL.
      es_uomx-maximum_stacking = abap_true.
    ENDIF.

    IF is_uom-capacity_usage IS NOT INITIAL.
      es_uomx-capacity_usage = abap_true.
    ENDIF.

    IF is_uom-ewm_cw_uom_type IS NOT INITIAL.
      es_uomx-ewm_cw_uom_type = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD fill_x_struct_valuationdata.

    CLEAR es_valuationdatax.

    IF is_valuationdata-val_area IS NOT INITIAL.
      es_valuationdatax-val_area = abap_true.
    ENDIF.

    IF is_valuationdata-val_type IS NOT INITIAL.
      es_valuationdatax-val_type = abap_true.
    ENDIF.


    IF is_valuationdata-del_flag IS NOT INITIAL.
      es_valuationdatax-del_flag = abap_true.
    ENDIF.


    IF is_valuationdata-price_ctrl IS NOT INITIAL.
      es_valuationdatax-price_ctrl = abap_true.
    ENDIF.

    IF is_valuationdata-moving_pr IS NOT INITIAL.
      es_valuationdatax-moving_pr = abap_true.
    ENDIF.

    IF is_valuationdata-std_price IS NOT INITIAL.
      es_valuationdatax-std_price = abap_true.
    ENDIF.

    IF is_valuationdata-price_unit IS NOT INITIAL.
      es_valuationdatax-price_unit = abap_true.
    ENDIF.

    IF is_valuationdata-val_class IS NOT INITIAL.
      es_valuationdatax-val_class = abap_true.
    ENDIF.

    IF is_valuationdata-pr_ctrl_pp IS NOT INITIAL.
      es_valuationdatax-pr_ctrl_pp = abap_true.
    ENDIF.

    IF is_valuationdata-mov_pr_pp IS NOT INITIAL.
      es_valuationdatax-mov_pr_pp = abap_true.
    ENDIF.
    IF is_valuationdata-std_pr_pp IS NOT INITIAL.
      es_valuationdatax-std_pr_pp = abap_true.
    ENDIF.

    IF is_valuationdata-pr_unit_pp IS NOT INITIAL.
      es_valuationdatax-pr_unit_pp = abap_true.
    ENDIF.

    IF is_valuationdata-vclass_pp IS NOT INITIAL.
      es_valuationdatax-vclass_pp = abap_true.
    ENDIF.

    IF is_valuationdata-pr_ctrl_py IS NOT INITIAL.
      es_valuationdatax-pr_ctrl_py = abap_true.
    ENDIF.

    IF is_valuationdata-mov_pr_py IS NOT INITIAL.
      es_valuationdatax-mov_pr_py = abap_true.
    ENDIF.

    IF is_valuationdata-std_pr_py IS NOT INITIAL.
      es_valuationdatax-std_pr_py = abap_true.
    ENDIF.

    IF is_valuationdata-vclass_py IS NOT INITIAL.
      es_valuationdatax-vclass_py = abap_true.
    ENDIF.

    IF is_valuationdata-pr_unit_py IS NOT INITIAL.
      es_valuationdatax-pr_unit_py = abap_true.
    ENDIF.

    IF is_valuationdata-val_cat IS NOT INITIAL.
      es_valuationdatax-val_cat = abap_true.
    ENDIF.

    IF is_valuationdata-future_pr IS NOT INITIAL.
      es_valuationdatax-future_pr = abap_true.
    ENDIF.

    IF is_valuationdata-valid_from IS NOT INITIAL.
      es_valuationdatax-valid_from = abap_true.
    ENDIF.


    IF is_valuationdata-taxprice_1 IS NOT INITIAL.
      es_valuationdatax-taxprice_1 = abap_true.
    ENDIF.

    IF is_valuationdata-commprice1 IS NOT INITIAL.
      es_valuationdatax-commprice1 = abap_true.
    ENDIF.

    IF is_valuationdata-taxprice_3 IS NOT INITIAL.
      es_valuationdatax-taxprice_3 = abap_true.
    ENDIF.

    IF is_valuationdata-commprice3 IS NOT INITIAL.
      es_valuationdatax-commprice3 = abap_true.
    ENDIF.

    IF is_valuationdata-plnd_price IS NOT INITIAL.
      es_valuationdatax-plnd_price = abap_true.
    ENDIF.

    IF is_valuationdata-plndprice1 IS NOT INITIAL.
      es_valuationdatax-plndprice1 = abap_true.
    ENDIF.

    IF is_valuationdata-plndprice2 IS NOT INITIAL.
      es_valuationdatax-plndprice2 = abap_true.
    ENDIF.

    IF is_valuationdata-plndprice3 IS NOT INITIAL.
      es_valuationdatax-plndprice3 = abap_true.
    ENDIF.
    IF is_valuationdata-plndprdate1 IS NOT INITIAL.
      es_valuationdatax-plndprdate1 = abap_true.
    ENDIF.

    IF is_valuationdata-plndprdate2 IS NOT INITIAL.
      es_valuationdatax-plndprdate2 = abap_true.
    ENDIF.

    IF is_valuationdata-plndprdate3 IS NOT INITIAL.
      es_valuationdatax-plndprdate3 = abap_true.
    ENDIF.

    IF is_valuationdata-lifo_fifo IS NOT INITIAL.
      es_valuationdatax-lifo_fifo = abap_true.
    ENDIF.

    IF is_valuationdata-poolnumber IS NOT INITIAL.
      es_valuationdatax-poolnumber = abap_true.
    ENDIF.

    IF is_valuationdata-taxprice_2 IS NOT INITIAL.
      es_valuationdatax-taxprice_2 = abap_true.
    ENDIF.

    IF is_valuationdata-commprice2 IS NOT INITIAL.
      es_valuationdatax-commprice2 = abap_true.
    ENDIF.

    IF is_valuationdata-deval_ind IS NOT INITIAL.
      es_valuationdatax-deval_ind = abap_true.
    ENDIF.

    IF is_valuationdata-orig_group IS NOT INITIAL.
      es_valuationdatax-orig_group = abap_true.
    ENDIF.

    IF is_valuationdata-overhead_grp IS NOT INITIAL.
      es_valuationdatax-overhead_grp = abap_true.
    ENDIF.

    IF is_valuationdata-qty_struct IS NOT INITIAL.
      es_valuationdatax-qty_struct = abap_true.
    ENDIF.

    IF is_valuationdata-ml_active IS NOT INITIAL.
      es_valuationdatax-ml_active = abap_true.
    ENDIF.

    IF is_valuationdata-ml_settle IS NOT INITIAL.
      es_valuationdatax-ml_settle = abap_true.
    ENDIF.

    IF is_valuationdata-orig_mat IS NOT INITIAL.
      es_valuationdatax-orig_mat = abap_true.
    ENDIF.

    IF is_valuationdata-vm_so_stk IS NOT INITIAL.
      es_valuationdatax-vm_so_stk = abap_true.
    ENDIF.

    IF is_valuationdata-vm_p_stock IS NOT INITIAL.
      es_valuationdatax-vm_p_stock = abap_true.
    ENDIF.

    IF is_valuationdata-matl_usage IS NOT INITIAL.
      es_valuationdatax-matl_usage = abap_true.
    ENDIF.

    IF is_valuationdata-mat_origin IS NOT INITIAL.
      es_valuationdatax-mat_origin = abap_true.
    ENDIF.

    IF is_valuationdata-in_house IS NOT INITIAL.
      es_valuationdatax-in_house = abap_true.
    ENDIF.

    IF is_valuationdata-tax_cml_un IS NOT INITIAL.
      es_valuationdatax-tax_cml_un = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD fill_x_struct_warehouse.

    CLEAR es_warehousedatax.

    IF is_warehousedata-whse_no IS NOT INITIAL.
      es_warehousedatax-whse_no = abap_true.
    ENDIF.

    IF is_warehousedata-del_flag IS NOT INITIAL.
      es_warehousedatax-del_flag = abap_true.
    ENDIF.

    IF is_warehousedata-stgesector IS NOT INITIAL.
      es_warehousedatax-stgesector = abap_true.
    ENDIF.

    IF is_warehousedata-placement IS NOT INITIAL.
      es_warehousedatax-placement = abap_true.
    ENDIF.

    IF is_warehousedata-withdrawal IS NOT INITIAL.
      es_warehousedatax-withdrawal = abap_true.
    ENDIF.

    IF is_warehousedata-l_equip_1 IS NOT INITIAL.
      es_warehousedatax-l_equip_1 = abap_true.
    ENDIF.

    IF is_warehousedata-l_equip_2 IS NOT INITIAL.
      es_warehousedatax-l_equip_2 = abap_true.
    ENDIF.

    IF is_warehousedata-l_equip_3 IS NOT INITIAL.
      es_warehousedatax-l_equip_3 = abap_true.
    ENDIF.

    IF is_warehousedata-leq_unit_1 IS NOT INITIAL.
      es_warehousedatax-leq_unit_1 = abap_true.
    ENDIF.

    IF is_warehousedata-leq_unit_1_iso IS NOT INITIAL.
      es_warehousedatax-leq_unit_1_iso = abap_true.
    ENDIF.

    IF is_warehousedata-leq_unit_2 IS NOT INITIAL.
      es_warehousedatax-leq_unit_2 = abap_true.
    ENDIF.

    IF is_warehousedata-leq_unit_2_iso IS NOT INITIAL.
      es_warehousedatax-leq_unit_2_iso = abap_true.
    ENDIF.

    IF is_warehousedata-leq_unit_3 IS NOT INITIAL.
      es_warehousedatax-leq_unit_3 = abap_true.
    ENDIF.

    IF is_warehousedata-leq_unit_3_iso IS NOT INITIAL.
      es_warehousedatax-leq_unit_3_iso = abap_true.
    ENDIF.

    IF is_warehousedata-unittype_1 IS NOT INITIAL.
      es_warehousedatax-unittype_1 = abap_true.
    ENDIF.

    IF is_warehousedata-unittype_2 IS NOT INITIAL.
      es_warehousedatax-unittype_2 = abap_true.
    ENDIF.

    IF is_warehousedata-unittype_3 IS NOT INITIAL.
      es_warehousedatax-unittype_3 = abap_true.
    ENDIF.

    IF is_warehousedata-wm_unit IS NOT INITIAL.
      es_warehousedatax-wm_unit = abap_true.
    ENDIF.

    IF is_warehousedata-wm_unit_iso IS NOT INITIAL.
      es_warehousedatax-wm_unit_iso = abap_true.
    ENDIF.

    IF is_warehousedata-add_to_stk IS NOT INITIAL.
      es_warehousedatax-add_to_stk = abap_true.
    ENDIF.

    IF is_warehousedata-block_stge IS NOT INITIAL.
      es_warehousedatax-block_stge = abap_true.
    ENDIF.

    IF is_warehousedata-msg_to_im IS NOT INITIAL.
      es_warehousedatax-msg_to_im = abap_true.
    ENDIF.

    IF is_warehousedata-spec_mvmt IS NOT INITIAL.
      es_warehousedatax-spec_mvmt = abap_true.
    ENDIF.

    IF is_warehousedata-capy_usage IS NOT INITIAL.
      es_warehousedatax-capy_usage = abap_true.
    ENDIF.

    IF is_warehousedata-procure_un IS NOT INITIAL.
      es_warehousedatax-procure_un = abap_true.
    ENDIF.

    IF is_warehousedata-procure_un_iso IS NOT INITIAL.
      es_warehousedatax-procure_un_iso = abap_true.
    ENDIF.

    IF is_warehousedata-stge_type IS NOT INITIAL.
      es_warehousedatax-stge_type = abap_true.
    ENDIF.

    IF is_warehousedata-ref_unit IS NOT INITIAL.
      es_warehousedatax-ref_unit = abap_true.
    ENDIF.

    IF is_warehousedata-2step_pick IS NOT INITIAL.
      es_warehousedatax-2step_pick = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD get_eta.

    DATA lv_arrival_date TYPE datum.

    CONSTANTS: lc_au  TYPE scal-fcalid VALUE 'AU'.

    CLEAR ev_date.

    lv_arrival_date = is_item-arrival_date.

    IF is_item-order_status_code = gc_bo_0400 AND lv_arrival_date <= sy-datum. "M- DECK915045
      ev_date = '20400101'.
    ELSE.

      IF lv_arrival_date IS INITIAL.

*Reading Delivery Date
        READ TABLE me->gt_poschedule ASSIGNING FIELD-SYMBOL(<ls_sch>)
                 WITH KEY po_item = iv_ordered_item.
        IF sy-subrc = 0.
          ev_date = <ls_sch>-deliv_date.
        ENDIF.

      ELSE.
*--> Get the ETA details from Z table
        READ TABLE gt_eta_details INTO DATA(ls_eta_details)
              WITH KEY bukrs = me->gs_poheader-comp_code
                       ekorg = me->gs_poheader-purch_org
                       lifnr = me->gs_poheader-vendor
                       bsart = me->gs_poheader-doc_type
              BINARY SEARCH.
        IF sy-subrc = 0.
*--> Add Local Transport Lead Time
          CALL FUNCTION 'END_TIME_DETERMINE'
            EXPORTING
              duration                   = ls_eta_details-locleadtime
              factory_calendar           = lc_au
            IMPORTING
              end_date                   = ev_date
            CHANGING
              start_date                 = lv_arrival_date
            EXCEPTIONS
              factory_calendar_not_found = 1
              date_out_of_calendar_range = 2
              date_not_valid             = 3
              unit_conversion_error      = 4
              si_unit_missing            = 5
              parameters_no_valid        = 6
              OTHERS                     = 7.
          IF sy-subrc <> 0.
            MESSAGE e566(zp2p) INTO DATA(lv_message).
            me->log_mesg( ).
          ENDIF.
*--> Warehouse Binning Lead Time
          CALL FUNCTION 'END_TIME_DETERMINE'
            EXPORTING
              duration                   = ls_eta_details-wbleadtime
              factory_calendar           = lc_au
            IMPORTING
              end_date                   = ev_date
            CHANGING
              start_date                 = ev_date
            EXCEPTIONS
              factory_calendar_not_found = 1
              date_out_of_calendar_range = 2
              date_not_valid             = 3
              unit_conversion_error      = 4
              si_unit_missing            = 5
              parameters_no_valid        = 6
              OTHERS                     = 7.
          IF sy-subrc <> 0.
            MESSAGE e565(zp2p) INTO lv_message.
            me->log_mesg( ).
          ENDIF.
        ELSE.
          MESSAGE e541(zp2p) INTO lv_message.
          me->log_mesg( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_po_conf_det.

    DATA :lo_admin_data TYPE REF TO if_fdt_admin_data,
          lo_function   TYPE REF TO if_fdt_function,
          lo_context    TYPE REF TO if_fdt_context,
          lo_result     TYPE REF TO if_fdt_result.

    cl_fdt_factory=>get_instance_generic( EXPORTING iv_id ='00163E1D3A421EE7AA830236EDB6DA83'
                                          IMPORTING eo_instance = lo_admin_data ).

    lo_function ?= lo_admin_data.
    IF lo_function IS NOT BOUND.
      RETURN.
    ENDIF.


    lo_context ?= lo_function->get_process_context( ).

    lo_context->set_value( iv_name = 'SUB_CODE' ia_value = iv_sub_code ).

    TRY.
        lo_function->process( EXPORTING io_context = lo_context
          IMPORTING eo_result = lo_result ).

        lo_result->get_value( IMPORTING ea_value = es_po_conf_det ).

        IF es_po_conf_det IS NOT INITIAL.
          es_po_conf_det-sub_code = iv_sub_code.
        ENDIF.

      CATCH cx_fdt INTO data(lx_fdt) ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD get_preload_confirmations.

*Data Declarations
    DATA:
      lt_unsez           TYPE TABLE OF ty_s_unsez,
      lt_matnr           TYPE ty_t_matnr,
      lt_po_details_tmp  TYPE TABLE OF ty_s_po,
      lt_back_order_data TYPE ztmm_back_order_confirmation62.

    FIELD-SYMBOLS:<lt_ord_data> TYPE zsmm_back_order_payload.

    CLEAR et_order.

*Assigning the reference data to table
    ASSIGN io_data->* TO <lt_ord_data>.

    IF <lt_ord_data> IS NOT ASSIGNED OR <lt_ord_data> IS INITIAL.
      IF 1 = 2.
        MESSAGE e453(zp2p).                             "#EC CI_CONV_OK
      ENDIF.
      RAISE EXCEPTION TYPE zcx_mm_back_ord_exp
        EXPORTING
          gt_return = VALUE bapiret2_t( ( type = 'E' id = 'ZP2P' number = '453' ) ).
    ENDIF.

    zcl_mm_back_order_conf=>gv_upd_mas_data = <lt_ord_data>-master_flag.

    SORT <lt_ord_data>-payload BY external_po.

*  Processing the data based on external PO reference
    LOOP AT <lt_ord_data>-payload INTO DATA(ls_back_ord).

      APPEND INITIAL LINE TO lt_unsez ASSIGNING FIELD-SYMBOL(<ls_unsez>).
      <ls_unsez>-unsez = ls_back_ord-external_po.

      APPEND INITIAL LINE TO lt_matnr ASSIGNING FIELD-SYMBOL(<ls_matnr>).
      <ls_matnr>-matnr = ls_back_ord-ordered_material.

      APPEND INITIAL LINE TO lt_matnr ASSIGNING <ls_matnr>.
      <ls_matnr>-matnr = ls_back_ord-confirmed_pomaterial.

      APPEND ls_back_ord TO lt_back_order_data.

      AT END OF external_po.
        APPEND INITIAL LINE TO et_order ASSIGNING FIELD-SYMBOL(<ls_order>).
        <ls_order>-back_conf = NEW #( REF #( lt_back_order_data ) ).
        CLEAR lt_back_order_data.
      ENDAT.

    ENDLOOP.

    SORT lt_unsez BY unsez.
    DELETE ADJACENT DUPLICATES FROM lt_unsez COMPARING unsez.

    SORT lt_matnr BY matnr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.

    IF lt_unsez IS NOT INITIAL.

*    Get PO data
      SELECT a~ebeln,                       "EC CI_NO_TRANSFORM
             b~ebelp,
             b~matnr,
             a~bukrs,
             a~unsez,
             b~werks,
             b~netpr,
             b~uebpo,
             b~uptyp,
             a~bsart,
             a~lifnr,
             a~ekorg
       FROM ekko AS a
       INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
       INTO TABLE @gt_po_details
       FOR ALL ENTRIES IN @lt_unsez
       WHERE a~unsez =  @lt_unsez-unsez
       AND   b~loekz =  @abap_false
       AND   b~elikz =  @abap_false ##DB_FEATURE_MODE[TABLE_LEN_MAX1].

      IF sy-subrc = 0.
        lt_po_details_tmp = gt_po_details.
        SORT lt_po_details_tmp BY bukrs ekorg lifnr bsart.
        DELETE ADJACENT DUPLICATES FROM lt_po_details_tmp COMPARING bukrs ekorg lifnr bsart.
      ENDIF.

    ENDIF.

*--> Get the ETA details from Z table
    IF lt_po_details_tmp IS NOT INITIAL.
      SELECT * FROM zmmc_boleadtime
        INTO TABLE @gt_eta_details
        FOR ALL ENTRIES IN @lt_po_details_tmp
        WHERE bukrs = @lt_po_details_tmp-bukrs
          AND ekorg = @lt_po_details_tmp-ekorg
          AND lifnr = @lt_po_details_tmp-lifnr
          AND bsart = @lt_po_details_tmp-bsart
          ORDER BY PRIMARY KEY.
      IF sy-subrc <> 0.
        CLEAR gt_eta_details.
      ENDIF.
    ENDIF.

    lt_po_details_tmp[] = gt_po_details[].
    SORT lt_po_details_tmp BY lifnr ekorg.
    DELETE ADJACENT DUPLICATES FROM lt_po_details_tmp COMPARING lifnr ekorg.

*--> Get Vendor Details and conf.Control key for info record
    IF lt_po_details_tmp[] IS NOT INITIAL.
      SELECT li~lifnr,
             li~land1,
             lf~ekorg,
             lf~bstae
        FROM lfa1 AS li
        INNER JOIN lfm1 AS lf
        ON lf~lifnr = li~lifnr
        INTO TABLE @gt_vendor
        FOR ALL ENTRIES IN @lt_po_details_tmp
        WHERE li~lifnr = @lt_po_details_tmp-lifnr
        AND   lf~ekorg = @lt_po_details_tmp-ekorg.
      IF sy-subrc = 0.
        SORT gt_vendor BY lifnr ekorg.
      ENDIF.
    ENDIF.

* Read material data
    IF lt_matnr IS NOT INITIAL.
*Getting material,Plant and storage location details
      SELECT ma~matnr,                             "#EC CI_NO_TRANSFORM
             ma~meins,
             ma~picnum,
             mc~werks,
             md~lgort
        FROM mara AS ma INNER JOIN nsdm_v_marc AS mc ON mc~matnr = ma~matnr
        INNER JOIN nsdm_v_mard AS md ON md~matnr = mc~matnr and md~werks = mc~werks        "HW3-166
        INTO TABLE @gt_matdata
        FOR ALL ENTRIES IN @lt_matnr
        WHERE ma~matnr = @lt_matnr-matnr
          AND mc~lvorm = @space.

      IF sy-subrc = 0.
        DATA(lt_material) = gt_matdata[].
        DELETE ADJACENT DUPLICATES FROM lt_material COMPARING matnr.
      ENDIF.

      IF lt_material IS NOT INITIAL.
*Get Material Valuation details
        SELECT matnr,                              "#EC CI_NO_TRANSFORM
               bwkey,
               bwtar
        FROM v_mbew_md
        INTO TABLE @gt_mbew
        FOR ALL ENTRIES IN @lt_material
        WHERE matnr = @lt_material-matnr.

        IF sy-subrc = 0.
          SORT gt_mbew BY matnr bwkey ASCENDING.
        ENDIF.

*Get Sales area for Material
        SELECT matnr,                              "#EC CI_NO_TRANSFORM
               vkorg,
               vtweg
        FROM mvke
        INTO TABLE @gt_mvke
        FOR ALL ENTRIES IN @lt_material
        WHERE matnr = @lt_material-matnr.

        IF sy-subrc = 0.
          SORT gt_mvke BY matnr ASCENDING.
        ENDIF.

*Get vendor codes based ON inforecords
        SELECT a~infnr,                            "#EC CI_NO_TRANSFORM
               a~matnr,
               a~lifnr,
               b~ekorg,
               b~aut_source,
               b~mwskz
         FROM eina AS a
         INNER JOIN eine AS b
         ON b~infnr = a~infnr
         INTO TABLE @gt_info_rec
         FOR ALL ENTRIES IN @lt_material
         WHERE a~matnr = @lt_material-matnr
          AND  b~esokz = '0'.
        IF sy-subrc = 0.
          SORT gt_info_rec BY matnr lifnr ekorg ASCENDING.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_sales_code.

    DATA : lt_bukrs TYPE bukrs_ran_itab.

    CLEAR et_sales_code.

*Get Sales Code for Purchase material status and sales material status
    SELECT sales_code
           bukrs
           mstae
           mstav
    FROM zmmc_sales_code
    INTO TABLE et_sales_code
    WHERE sales_code <> space
    AND   bukrs      IN lt_bukrs.

    IF sy-subrc = 0.
      SORT et_sales_code BY sales_code ASCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD get_supersessionchain.

*Data Declaration
    DATA : lv_matnr            TYPE matnr,
           lv_super_matnr      TYPE matnr,
           lv_partno_exists    TYPE boole_d,
           lv_super_mat_exists TYPE boole_d,
           lt_picpsrl          TYPE STANDARD TABLE OF v_picpsrl.

    CLEAR :  lv_matnr,
             lv_super_matnr,
             lv_partno_exists,
             lv_super_mat_exists,
             lt_picpsrl,
             ev_sup_session_not_exists,
             ev_sup_session_exists_ord_mat,
             ev_sup_session_exists_conf_mat,
             ev_sup_different_ord_conf_mat,
             ev_sup_ord_mat_not_exists,
             et_picpsrl.

    lv_matnr       = iv_matnr. "Ordered Material
    lv_super_matnr = iv_superseed_part_no. " Supersession Material


*Check Supersessionchain for Ordered Material
    CALL FUNCTION 'PIC01_GET_MEMBER_SINGLE'
      EXPORTING
        i_matnr           = lv_matnr
      TABLES
        et_picpsrl        = et_picpsrl
      EXCEPTIONS
        notfound_matnr    = 1
        notfound_pic      = 2
        missing_parameter = 3
        OTHERS            = 4.

    IF sy-subrc = 0.

      SORT et_picpsrl BY matnr ASCENDING.

      DATA(lv_lines) = lines( et_picpsrl ).

      IF lv_lines > 1.

*check for Part no
        READ TABLE et_picpsrl ASSIGNING FIELD-SYMBOL(<ls_picpsrl>)
                  WITH KEY matnr = lv_matnr BINARY SEARCH.
        IF sy-subrc = 0.
          lv_partno_exists = abap_true.
        ENDIF.

*Check for supersession matnr
        READ TABLE et_picpsrl ASSIGNING <ls_picpsrl>
                 WITH KEY matnr =  lv_super_matnr BINARY SEARCH.
        IF sy-subrc = 0.
          lv_super_mat_exists = abap_true.
        ENDIF.

*Supersession chain already exist and both Supersession Material master
*and Material master available in Part Number are in same Supersession chains
        IF lv_partno_exists IS NOT INITIAL AND
            lv_super_mat_exists IS NOT INITIAL.

          ev_sup_session_exists_conf_mat = abap_true.

        ELSE.
*Supersession chain already exist for Superseeding material and in Part Number chains are different
*Get Supersessionchain for supersession material
          CALL FUNCTION 'PIC01_GET_MEMBER_SINGLE'
            EXPORTING
              i_matnr           = lv_super_matnr
            TABLES
              et_picpsrl        = lt_picpsrl
            EXCEPTIONS
              notfound_matnr    = 1
              notfound_pic      = 2
              missing_parameter = 3
              OTHERS            = 4.

          IF sy-subrc = 0.
            ev_sup_different_ord_conf_mat = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

*Check Whether the Supersession Chain exists for Partno as
*Supersession
        IF lv_partno_exists IS NOT INITIAL.
*Check whether Part no is the supersession Material
          READ TABLE et_picpsrl
               ASSIGNING FIELD-SYMBOL(<ls_pic>)
                   WITH KEY matnr   = lv_matnr BINARY SEARCH.

          IF sy-subrc = 0.
*supersession CHAIN already exist for Part no
            ev_sup_session_exists_ord_mat = abap_true.
          ENDIF.
        ENDIF.

      ELSE.
*Supersession chain already exist for Part no
        ev_sup_session_exists_ord_mat = abap_true.
      ENDIF.

    ELSE.
*Check Supersessionchain for Supersession Material
      CALL FUNCTION 'PIC01_GET_MEMBER_SINGLE'
        EXPORTING
          i_matnr           = lv_super_matnr
        TABLES
          et_picpsrl        = et_picpsrl
        EXCEPTIONS
          notfound_matnr    = 1
          notfound_pic      = 2
          missing_parameter = 3
          OTHERS            = 4.

      IF sy-subrc = 0.
*Supersession chain does not exist for ordered Material
        ev_sup_ord_mat_not_exists = abap_true.
      ELSE.
*Supersession chain does not exist
        ev_sup_session_not_exists = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD init.
    CLEAR:me->gs_poheader,
          me->gt_poitem[],
          me->gt_poschedule[],
          me->gt_poconfirmation[],
          me->gt_po_item[],
          me->gt_poitemx[],
          me->gt_appl_log[],
          me->gs_feh_payload,
          me->gs_feh_payload-payload[],
          me->gv_meins,
          me->gt_pr_data,
          me->gt_conf_add_ab,
          me->gt_conf_addx_ab,
          me->gt_yekes,
          me->gv_total_ordqty,
          me->gv_ord_mat_qty,
          me->gv_con_mat_qty.
  ENDMETHOD.


  METHOD insert_supersessionchain.

*Data Declarations
    DATA: ls_pic_list    TYPE pic01_ts_pic_interface,
          ls_new_picpsrl TYPE v_picpsrl,
          lv_pic_num     TYPE pic_picnum,
          lv_matnr       TYPE matnr,
          lv_seqnr       TYPE seqnr,
          lt_old_picpsrl TYPE STANDARD TABLE OF v_picpsrl,
          lt_new_picpsrl TYPE STANDARD TABLE OF v_picpsrl,
          lt_old_pic     TYPE STANDARD TABLE OF v_picmrl,
          lt_new_pic     TYPE STANDARD TABLE OF v_picmrl,
          lt_smesg       TYPE STANDARD TABLE OF smesg,
          ls_return      TYPE bapiret2.

    CLEAR:ev_pic_num,et_return.

    lt_old_picpsrl = lt_new_picpsrl = it_picpsrl.

    READ TABLE lt_new_picpsrl INTO DATA(ls_picpsrl) WITH KEY matnr = lv_matnr.
    IF sy-subrc = 0.
      ls_new_picpsrl         = ls_picpsrl.
      ls_new_picpsrl-matnr   =  iv_part_no.
      ls_new_picpsrl-seqnr   =  ls_picpsrl-seqnr - 1.
      ls_new_picpsrl-datfr   =  ls_picpsrl-datfr - 1.
      ls_new_picpsrl-inttype = gc_sign_i. "'I'.
      ls_new_picpsrl-piccode = gc_conf_code_0001.
      lv_pic_num             = ls_picpsrl-picnum.
      APPEND ls_new_picpsrl TO lt_new_picpsrl.
      CLEAR ls_new_picpsrl.
    ENDIF.

    CLEAR : lv_seqnr.

    LOOP AT lt_new_picpsrl ASSIGNING FIELD-SYMBOL(<ls_new>).
      lv_seqnr = lv_seqnr +  1.
      <ls_new>-seqnr = lv_seqnr.
    ENDLOOP.

    SORT lt_new_picpsrl BY seqnr ASCENDING.

    ls_pic_list-new_picpsrl = lt_new_picpsrl.
    ls_pic_list-old_picpsrl = lt_old_picpsrl.
    ls_pic_list-picnum =   lv_pic_num.

*Create Supersession material chain
*--Unreleased FM as no released API found to update supercession
    CALL FUNCTION 'PIC01_MAINTAIN_VPICPSRL'
      EXPORTING
        is_pic_list       = ls_pic_list
      IMPORTING
        e_picnum          = lv_pic_num
      TABLES
        it_old_vpicmrl    = lt_old_pic
        it_new_vpicmrl    = lt_new_pic
        it_error          = lt_smesg
      EXCEPTIONS
        missing_parameter = 1
        error_input       = 2
        OTHERS            = 3.
    IF sy-subrc = 0.
      ev_pic_num = lv_pic_num .
    ENDIF.

    LOOP AT lt_smesg ASSIGNING FIELD-SYMBOL(<ls_smesg>).
      IF <ls_smesg>-msgty IS NOT INITIAL AND <ls_smesg>-arbgb IS NOT INITIAL AND <ls_smesg>-txtnr IS NOT INITIAL.
        CLEAR ls_return.
        ls_return-type = <ls_smesg>-msgty.
        ls_return-id   = <ls_smesg>-arbgb.
        ls_return-number = <ls_smesg>-txtnr.
        ls_return-message_v1 = <ls_smesg>-msgv1.
        ls_return-message_v2 = <ls_smesg>-msgv2.
        ls_return-message_v3 = <ls_smesg>-msgv3.
        ls_return-message_v4 = <ls_smesg>-msgv4.
        APPEND ls_return TO me->gt_appl_log.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD log_mesg.
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*DESCRIPTION : Log messages
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA: ls_return TYPE bapiret2.

    CLEAR ls_return.

    ls_return-id         = iv_id.
    ls_return-type       = iv_type.
    ls_return-number     = iv_number.
    ls_return-message_v1 = iv_v1.
    ls_return-message_v2 = iv_v2.
    ls_return-message_v3 = iv_v3.
    ls_return-message_v4 = iv_v4.

    APPEND ls_return TO me->gt_appl_log.

  ENDMETHOD.


  METHOD map_input.

    DATA:
      lv_matnr TYPE matnr,
      lv_ebelp TYPE ebelp,
      ls_input TYPE ty_s_input.

    CLEAR et_order.

    LOOP AT it_input INTO DATA(ls_input_data).
      TRY.
          ls_input = CORRESPONDING #( ls_input_data ) ##ENH_OK.
        CATCH cx_sy_conversion_no_number ##NO_HANDLER.
          IF 1 = 2.
            MESSAGE e452(zp2p).
          ENDIF.
          RAISE EXCEPTION TYPE zcx_mm_back_ord_exp
            EXPORTING
              gt_return = VALUE bapiret2_t( ( type = 'E' id = 'ZP2P' number = '452' ) ).
      ENDTRY.

*   Conversion for Part no
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = ls_input_data-ordered_material
        IMPORTING
          output       = lv_matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF sy-subrc IS INITIAL.
        ls_input-ordered_material = lv_matnr.
      ENDIF.

      IF ls_input_data-confirmed_pomaterial IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = ls_input_data-confirmed_pomaterial
          IMPORTING
            output       = lv_matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
        IF sy-subrc IS INITIAL.
          ls_input-confirmed_pomaterial = lv_matnr.
        ENDIF.
      else.
        ls_input_data-confirmed_pomaterial = ls_input_data-ordered_material.
      ENDIF.

      " Conversion for Item nos
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_input_data-ordered_poitem
        IMPORTING
          output = lv_ebelp.

      ls_input-ordered_poitem = lv_ebelp.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_input_data-confirmed_poitem
        IMPORTING
          output = lv_ebelp.

      ls_input-confirmed_poitem = lv_ebelp.

      APPEND ls_input TO et_order-payload.
      CLEAR: ls_input.

    ENDLOOP.

    et_order-master_flag = iv_master_flag.

  ENDMETHOD.


  METHOD on_proxy_invoke.
*Data Declarations
    DATA:
      lv_extid              TYPE balnrext,
      lv_material_5200      TYPE matnr,
      lv_material           TYPE matnr,
      lv_duplicate_material TYPE matnr,
      lv_count              TYPE i,
      ls_log                LIKE LINE OF me->gt_appl_log,
      lv_ebelp              TYPE ebelp.

    FIELD-SYMBOLS:<ls_order> TYPE ty_s_input.

    me->init( ).

*Get Honda PO no from file
    READ TABLE me->gt_back_conf_data INTO DATA(ls_po_det) INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_po_details INTO DATA(ls_po_check)
        WITH KEY unsez = ls_po_det-external_po.
      IF sy-subrc <> 0.
        CLEAR ls_po_check.
      ENDIF.
    ENDIF.

*If not available create PO
    IF ls_po_check IS INITIAL.

      DATA(lv_po_no) = me->create_po( ls_po_det ).

      IF lv_po_no IS NOT INITIAL.
        MESSAGE e540(zp2p) WITH lv_po_no INTO DATA(lv_message) ##NEEDED.
        me->log_mesg( ).
      ENDIF.

    ELSE.

      me->set_po_detail( ls_po_check-ebeln ).

*Process COonfirmations
      LOOP AT me->gt_back_conf_data ASSIGNING <ls_order>.

        IF <ls_order>-order_status_code = gc_stat_0300 OR <ls_order>-order_status_code = gc_order_cancel_5300.
          CONTINUE.
        ENDIF.

        AT NEW ordered_material.  "already sorted in constructor as needed
*--> Validate PO Item Quantity
          IF me->validate_po_item_quantity( <ls_order> ) = abap_true.
*--> Do not process the PO item
            lv_material = <ls_order>-ordered_material.
            CONTINUE.
          ENDIF.
        ENDAT.

*--> In case of same material skip it
        IF lv_material = <ls_order>-ordered_material.
          CONTINUE.
        ENDIF.

*--> In case status code 5200 process for any Material do not process other status codes
        IF lv_material_5200 = <ls_order>-ordered_material.
*Cannot Process Order status Code &1 for HPO no &2 Material &3 As 5200 Processed
          MESSAGE w532(zp2p) WITH <ls_order>-order_status_code <ls_order>-external_po
                                  <ls_order>-ordered_material  INTO lv_message.
          me->log_mesg( ).
          CONTINUE.
        ENDIF.

*--------------------------------------------------------------------------------------------
*Check for only Order Status Code 0400,0500,5200,5300,5400 and 5500
        IF <ls_order>-order_status_code NOT IN gt_status_code.
*Cannot Process Order status Code &1 for PC no &2 Material &3 Item No &4
          MESSAGE e416(zp2p) WITH <ls_order>-order_status_code <ls_order>-external_po
                                  <ls_order>-ordered_material  <ls_order>-ordered_poitem INTO lv_message.
          me->log_mesg( ).
          CONTINUE.
        ENDIF.

*--------------------------------------------------------------------------------------------
*Order Cancelling for 5200
        IF <ls_order>-order_status_code = gc_ord_cancel_5200
              OR <ls_order>-order_status_code = gc_order_cancel_5300.
*Processing for HPO &1 Material &2 Ord Status Code &3 and Cancel Quan &4
          MESSAGE s469(zp2p) WITH <ls_order>-external_po <ls_order>-ordered_material
                                  <ls_order>-order_status_code <ls_order>-processed_quantity
                             INTO lv_message.
        ELSE.
*Processing for HPO &1 Material &2 Ord.Status Code &3 and Conf.Quan &4
          MESSAGE s458(zp2p) WITH <ls_order>-external_po <ls_order>-ordered_material
                                  <ls_order>-order_status_code <ls_order>-processed_quantity
                             INTO lv_message.
        ENDIF.

        me->log_mesg( ).

* Get PO details for this PCNo
        CLEAR:me->gv_meins,lv_ebelp.

        " Note purchase order number
        MESSAGE i369(zp2p) WITH me->gs_poheader-po_number INTO lv_message.
        me->log_mesg( ).

*---------------------------------------------------------------------------------------------
*Check Ordered material exists
        READ TABLE gt_matdata ASSIGNING FIELD-SYMBOL(<ls_mat>)
                           WITH KEY matnr = <ls_order>-ordered_material.

        IF sy-subrc IS NOT INITIAL.
          " Material Number &1 Not Found
          MESSAGE e211(zp2p) WITH <ls_order>-ordered_material INTO lv_message.
          me->log_mesg( ).
          CONTINUE.
        ELSE.
          me->gv_meins = <ls_mat>-meins.
        ENDIF.
*------------------------------------------------------------------------------------------------
*Check whether Part no Exists in PO
        READ TABLE me->gt_poitem INTO DATA(ls_po)
              WITH KEY material_long = <ls_order>-ordered_material.
        IF sy-subrc <> 0.
          MESSAGE e371(zp2p) WITH <ls_order>-ordered_material me->gs_poheader-po_number INTO lv_message.
          me->log_mesg( ).
          CONTINUE.
        ELSE.
          lv_ebelp = ls_po-po_item.
        ENDIF.
*------------------------------------------------------------------------------------------------
*Check for Multiple Materials available in PO
        IF <ls_order>-ordered_material = lv_duplicate_material.
          CONTINUE.
        ENDIF.

        CLEAR lv_count.
        LOOP AT me->gt_poitem INTO ls_po WHERE material_long = <ls_order>-ordered_material AND
                                               si_cat <> gc_sub_cat.
          lv_count = lv_count + 1.
          IF lv_count > 1.
            lv_duplicate_material = <ls_order>-ordered_material.
*Multiple PO Line Items Exists for Material &1 in SAP PO &2
            MESSAGE e423(zp2p) WITH <ls_order>-ordered_material me->gs_poheader-po_number INTO lv_message.
            me->log_mesg( ).
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_count > 1.
          CONTINUE.
        ENDIF.
*----------------------------------------------------------------------------------------
        IF <ls_order>-processed_quantity <= 0.
*Processed Quantity is 0 for PC No &1 Material &2 and ETA &3
          MESSAGE e414(zp2p) WITH <ls_order>-external_po
                                  <ls_order>-ordered_material
                                  <ls_order>-arrival_date INTO lv_message.
          me->log_mesg( ).
          CONTINUE.
        ENDIF.

*------------------------------------------------------------------------------------------
        IF <ls_order>-substitution_mark = 'E'.  "Supersession should be setup

          IF <ls_order>-confirmed_pomaterial IS INITIAL.
*Conf.PO Mat is blank Supersession not Possible for Ord.Material &1
            MESSAGE e415(zp2p) WITH <ls_order>-ordered_material INTO lv_message.
            me->log_mesg( ).
            CONTINUE.
          ENDIF.

          IF <ls_order>-confirmed_pomaterial  = <ls_order>-ordered_material.
*Ordered Material &1 and Confirmed Material &2 are same in Supersession
            MESSAGE e730(zp2p) WITH <ls_order>-ordered_material <ls_order>-confirmed_pomaterial INTO lv_message.
            me->log_mesg( ).
            CONTINUE.
          ENDIF.


*Update PO with Supersession
          IF me->error_logged( ) = abap_false.
            me->update_po_with_supersession( is_item = <ls_order>
                                             iv_ordered_item = lv_ebelp ).
          ENDIF.

        ELSE.

*Update PO without Supersession
          IF me->error_logged( ) = abap_false.
            me->update_po_without_supersession( is_item = <ls_order>
                                                iv_ordered_item = lv_ebelp ).
          ENDIF.

        ENDIF.

*--> Store the Ordered Material in case of status code 5200 as we will not process other status codes after 5200
        IF <ls_order>-order_status_code = gc_ord_cancel_5200.
          lv_material_5200 = <ls_order>-ordered_material.
        ENDIF.

      ENDLOOP.

*--> Change and Update PO
      IF me->error_logged( ) = abap_false.
        me->post_po( ).
      ENDIF.

    ENDIF.

*Application log
    IF me->gt_appl_log IS NOT INITIAL.

      DATA(lo_obj) = NEW zcl_h1s_generic_utils( ).

      lv_extid = gc_extid && gc_un_sc && me->gv_app_log_id.

      CALL METHOD zcl_h1s_generic_utils=>save_appl_log
        EXPORTING
          iv_extid       = lv_extid
          iv_object      = gc_obj
          iv_sub_object  = gc_subobj
          it_msg_tab     = me->gt_appl_log
          iv_commit_flag = 'X'.

      " Setting Application LOG
*      lo_obj->set_bal_log( iv_extid       = lv_extid
*                           iv_object      = gc_obj
*                           iv_sub_object  = gc_subobj
*                           it_msg_tab     = me->gt_appl_log
*                           iv_commit_flag = abap_true ).

      IF me->error_logged( ) = abap_true.

        me->gs_feh_payload-payload = me->gt_back_conf_data.
        me->gs_feh_payload-master_flag = gv_upd_mas_data.

        IF ls_po_check IS NOT INITIAL.
          ls_log-id         = 'ZP2P'.
          ls_log-type       = 'E'.
          ls_log-number     = '369'.
          ls_log-message_v1 = me->gs_poheader-po_number.
          INSERT ls_log INTO me->gt_appl_log INDEX 1.
          IF 1 = 2.
            MESSAGE e369(zp2p) WITH me->gs_poheader-po_number INTO lv_message.
          ENDIF.
        ENDIF.

        DELETE me->gt_appl_log WHERE type = 'I' OR type = 'W' OR type = 'S'.
        RAISE EXCEPTION TYPE zcx_mm_back_ord_exp.

      ENDIF.

    ENDIF.

  ENDMETHOD.


METHOD post_po.


  DATA: lv_error       TYPE c,
        lv_commit      TYPE c,
        lt_return      TYPE bapiret2_t,
        lt_item        TYPE bapimeconf_t_item,
        ls_yekes       TYPE uekes,
        lv_delete_conf TYPE c,
        lt_xekes       TYPE TABLE OF uekes,
        lt_yekes       TYPE TABLE OF uekes.

*************************************************************************************************************
*--> Delete confirmations
*************************************************************************************************************
  IF me->gt_yekes IS NOT INITIAL.

    lv_delete_conf = abap_true.

    SORT me->gt_yekes BY ebelp etens.
    DELETE ADJACENT DUPLICATES FROM me->gt_yekes COMPARING ebelp etens.

    CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
      EXPORTING
        i_ebeln = me->gs_poheader-po_number
      TABLES
        xekes   = lt_xekes " Empty table
        yekes   = gt_yekes.

    lv_commit = abap_true.
  ENDIF.

*************************************************************************************************************
*--> PO change
*************************************************************************************************************
  IF me->gt_po_item IS NOT INITIAL.

    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = me->gs_poheader-po_number
      TABLES
        return        = lt_return
        poitem        = me->gt_po_item
        poitemx       = me->gt_poitemx.

    LOOP AT lt_return INTO DATA(ls_return).
      IF ls_return-type CA 'EAX'.
        lv_error = 'X'.
      ENDIF.
      APPEND ls_return TO me->gt_appl_log.
    ENDLOOP.

    IF lv_error = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RETURN.
    ELSE.
      lv_commit = abap_true.
    ENDIF.
  ENDIF.

*************************************************************************************************************
*--> Update PR Details
*************************************************************************************************************
  IF lv_commit = abap_true.

    IF me->gt_pr_data IS NOT INITIAL.
      update_pr_quantity( ).
    ENDIF.

    IF lv_delete_conf = abap_true.
      MESSAGE s569(zp2p) INTO DATA(lv_message).
      me->log_mesg( ).
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

  CLEAR lt_return.

*************************************************************************************************************
*--> Add AB confirmations
*************************************************************************************************************
  IF me->gt_conf_add_ab IS NOT INITIAL .

    CALL FUNCTION 'ME_PO_CONFIRM'
      EXPORTING
        document_no   = me->gs_poheader-po_number
        item          = lt_item
        confirmation  = me->gt_conf_add_ab
        confirmationx = me->gt_conf_addx_ab
      IMPORTING
        return        = lt_return.

    LOOP AT lt_return INTO ls_return.
      IF ls_return-type CA 'EAX'.
        lv_error = 'X'.
      ENDIF.
      APPEND ls_return TO me->gt_appl_log.
    ENDLOOP.

    IF lv_error = 'X'.
      MESSAGE e557(zp2p) INTO lv_message ##NEEDED.
      me->log_mesg( ).
      ROLLBACK WORK.
      RETURN.
    ELSE.
      MESSAGE s562(zp2p) INTO lv_message ##NEEDED.
      me->log_mesg( ).
      COMMIT WORK.
    ENDIF.

  ENDIF.

ENDMETHOD.


  METHOD process_order_status_0400.

*Data Declarations
    DATA:
      lv_docnum    TYPE edidc-docnum,
      lv_fob_new   TYPE bprei,
      lv_itemno    TYPE ebelp,
      ls_poitemx   TYPE bapimepoitemx,
      ls_e1einam   TYPE  e1einam,
      ls_e1einem   TYPE  e1einem,
      ls_extension TYPE  z1mm_auts,
      ls_poitem    TYPE bapimepoitem,
      lt_ret       TYPE bapiret2_t,
      ls_conf_add  TYPE bapimeconfdetail,
      ls_conf_addx TYPE bapimeconfdetailx.

    CLEAR:et_item,et_itemx,et_conf_add,et_conf_addx.

    READ TABLE me->gt_po_item
      INTO DATA(ls_item)
      WITH KEY material_long = iv_confirmed_matnr.
    IF sy-subrc <> 0.
      READ TABLE me->gt_poitem
        INTO ls_item
        WITH KEY material_long = iv_confirmed_matnr.
    ENDIF.

*Check whether supersession material or Ordered Material is available
*to read the PO items
    IF ls_item IS INITIAL.

*Read ordered material no
      READ TABLE me->gt_poitem
        INTO ls_item
        WITH KEY material_long = iv_ordered_matnr.

*If Superseeding material is available then update superseeding material
*in PO Else Update the Part no
      IF sy-subrc = 0.
        lv_itemno = ls_item-po_item + 1.
      ENDIF.

*--> Check for full superseeding or partial
      IF gv_total_ordqty = gv_con_mat_qty.
        DATA(lv_full_sup) = abap_true.
      ENDIF.

*-Create New confirmed material
      ls_poitem-po_item = lv_itemno.
      ls_poitem-hl_item = ls_item-po_item.
      ls_poitem-si_cat = gc_sub_cat.
      ls_poitem-material = iv_confirmed_matnr.
      ls_poitem-material_long = iv_confirmed_matnr.
      ls_poitem-plant = ls_item-plant.
      ls_poitem-conf_ctrl = ls_item-conf_ctrl.
      ls_poitem-stge_loc = ls_item-stge_loc.
      ls_poitem-quantity = gv_con_mat_qty.
      ls_poitem-preq_no = ls_item-preq_no.
      ls_poitem-preq_item = ls_item-preq_item.
      IF iv_fob <> 0.
        ls_poitem-net_price = iv_fob.
      ELSE.
        ls_poitem-net_price = ls_item-net_price.
      ENDIF.
      ls_poitem-ematerial = iv_confirmed_matnr.
      ls_poitem-tax_code = ls_item-tax_code.
      APPEND ls_poitem TO et_item.

      ls_poitemx-po_itemx = abap_true.
      ls_poitemx-po_item = lv_itemno.
      ls_poitemx-hl_item   = abap_true.
      ls_poitemx-si_cat    = abap_true.
      ls_poitemx-material  = abap_true.
      ls_poitemx-material_long = abap_true.
      ls_poitemx-plant     = abap_true.
      ls_poitemx-conf_ctrl = abap_true.
      ls_poitemx-stge_loc  = abap_true.
      ls_poitemx-quantity  = abap_true.
      ls_poitemx-net_price = abap_true.
      ls_poitemx-ematerial = abap_true.
      ls_poitemx-tax_code  = abap_true.
      APPEND ls_poitemx TO et_itemx.

      ls_conf_add-item_no = lv_itemno.
      ls_conf_add-conf_category = gc_conf_type_ab.
      ls_conf_add-deliv_date_typ = 'D'.
      ls_conf_add-deliv_date = iv_eta.
      ls_conf_add-quantity = iv_process_qty.
      APPEND ls_conf_add TO et_conf_add.

      ls_conf_addx-item_no = lv_itemno.
      ls_conf_addx-item_nox = abap_true.
      ls_conf_addx-conf_category = abap_true.
      ls_conf_addx-deliv_date_typ = abap_true.
      ls_conf_addx-deliv_date = abap_true.
      ls_conf_addx-quantity = abap_true.
      ls_conf_addx-mpn = abap_true.
      APPEND ls_conf_addx TO et_conf_addx.

*-move details from main item to new one
      IF ( ls_item-quantity - iv_process_qty ) <> 0 AND lv_full_sup = abap_false.

        ls_poitem-po_item = lv_itemno + 1.
        ls_poitem-hl_item = ls_item-po_item."Highest Item
        ls_poitem-si_cat = gc_sub_cat. "'8'.
        ls_poitem-material = iv_ordered_matnr.
        ls_poitem-material_long  = iv_ordered_matnr.
        ls_poitem-plant = ls_item-plant.
        ls_poitem-conf_ctrl = ls_item-conf_ctrl.
        ls_poitem-stge_loc = ls_item-stge_loc.
        ls_poitem-quantity = gv_ord_mat_qty.
        ls_poitem-net_price = ls_item-net_price.
        ls_poitem-ematerial = iv_ordered_matnr.
        ls_poitem-tax_code = ls_item-tax_code.
        ls_poitem-preq_no = ls_item-preq_no.
        ls_poitem-preq_item = ls_item-preq_item.
        APPEND ls_poitem TO et_item.

        ls_poitemx-po_itemx  = abap_true.
        ls_poitemx-po_item   = lv_itemno + 1.
        ls_poitemx-hl_item   = abap_true.
        ls_poitemx-si_cat    = abap_true.
        ls_poitemx-material  = abap_true.
        ls_poitemx-material_long  = abap_true.
        ls_poitemx-plant     = abap_true.
        ls_poitemx-conf_ctrl = abap_true.
        ls_poitemx-stge_loc  = abap_true.
        ls_poitemx-quantity  = abap_true.
        ls_poitemx-net_price = abap_true.
        ls_poitemx-ematerial  = abap_true.
        ls_poitemx-tax_code  = abap_true.
        APPEND ls_poitemx TO et_itemx.

*-Moving the confirmation from higher level to partialy supersedded item
        LOOP AT gt_conf_add_ab ASSIGNING FIELD-SYMBOL(<ls_conf_add>) WHERE item_no = ls_item-po_item.
          <ls_conf_add>-item_no = lv_itemno + 1.
        ENDLOOP.

      ENDIF.

*--> Get Vendor Country for info record and conf.Control key
      READ TABLE gt_vendor INTO DATA(ls_vendor)
        WITH KEY lifnr = me->gs_poheader-vendor
                 ekorg = me->gs_poheader-purch_org
                 BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR ls_vendor.
      ENDIF.

*Map Info record to Update AUT_SOURCE
*Header
      ls_e1einam-matnr = iv_confirmed_matnr.
      ls_e1einam-lifnr = me->gs_poheader-vendor.
      ls_e1einam-meins = ls_item-po_unit.
      ls_e1einam-lmein = ls_item-po_unit.
*Item
      ls_e1einem-ekorg = me->gs_poheader-purch_org.
      ls_e1einem-ekgrp = me->gs_poheader-pur_group.
      ls_e1einem-waers = me->gs_poheader-currency.
      ls_e1einem-bstae = ls_vendor-bstae.
      ls_e1einem-uebtk = abap_true.

      IF ls_vendor-land1 = 'AU'.
        ls_e1einem-mwskz = 'P1'.
      ELSE.
        ls_e1einem-mwskz = 'P0'.
      ENDIF.

*Extension item
      ls_extension-aut_source = abap_true.

    ELSE.

      READ TABLE me->gt_po_item
        INTO ls_item
        WITH KEY material_long = iv_confirmed_matnr
                 si_cat   = '8'.
      IF sy-subrc <> 0.
        READ TABLE me->gt_poitem
          INTO ls_item
          WITH KEY material_long = iv_confirmed_matnr
                   si_cat   = '8'.
        IF sy-subrc <> 0.
          READ TABLE me->gt_po_item
            INTO ls_item
            WITH KEY material_long = iv_confirmed_matnr.
          IF sy-subrc <> 0.
            READ TABLE me->gt_poitem
              INTO ls_item
              WITH KEY material_long = iv_confirmed_matnr.
          ENDIF.
        ENDIF.
      ENDIF.

      IF sy-subrc = 0.

        ls_conf_add-item_no = ls_item-po_item.
        ls_conf_add-conf_category = gc_conf_type_ab.
        ls_conf_add-deliv_date_typ = 'D'.
        ls_conf_add-deliv_date = iv_eta.
        ls_conf_add-quantity = iv_process_qty.
        APPEND ls_conf_add TO et_conf_add.
        CLEAR ls_conf_add.

        ls_conf_addx-item_no = ls_item-po_item.
        ls_conf_addx-item_nox = abap_true.
        ls_conf_addx-conf_category = abap_true.
        ls_conf_addx-deliv_date_typ = abap_true.
        ls_conf_addx-deliv_date = abap_true.
        ls_conf_addx-quantity = abap_true.
        ls_conf_addx-mpn = abap_true.
        APPEND ls_conf_addx TO et_conf_addx.
        CLEAR ls_conf_addx.

*--> Update Price in case of any difference
        IF iv_fob <> ls_item-net_price AND iv_fob > 0.
          ls_poitem-net_price = iv_fob.
*--> Check if the Item is already available, if exists update price other wise append new line
          READ TABLE me->gt_po_item TRANSPORTING NO FIELDS WITH KEY po_item = ls_item-po_item.
          IF sy-subrc = 0.
            MODIFY me->gt_po_item INDEX sy-tabix FROM ls_poitem TRANSPORTING net_price .
          ELSE.
            ls_poitem-po_item = ls_item-po_item.
            ls_poitem-hl_item = ls_item-hl_item.
            ls_poitem-si_cat = ls_item-si_cat.
            ls_poitem-material = ls_item-material.
            ls_poitem-material_long  = ls_item-material_long.
            ls_poitem-plant = ls_item-plant.
            ls_poitem-conf_ctrl = ls_item-conf_ctrl.
            ls_poitem-stge_loc = ls_item-stge_loc.
            ls_poitem-quantity = ls_item-quantity.
            ls_poitem-ematerial = ls_item-ematerial.
            ls_poitem-tax_code = ls_item-tax_code.
            ls_poitem-preq_no = ls_item-preq_no.
            ls_poitem-preq_item = ls_item-preq_item.
            APPEND ls_poitem TO et_item.
          ENDIF.
*--> Update the PO Itemx
          ls_poitemx-net_price = abap_true.
          READ TABLE me->gt_poitemx TRANSPORTING NO FIELDS WITH KEY po_item = ls_item-po_item.
          IF sy-subrc = 0.
            MODIFY me->gt_poitemx INDEX sy-tabix FROM ls_poitemx TRANSPORTING net_price .
          ELSE.
            ls_poitemx-po_itemx  = abap_true.
            ls_poitemx-po_item   = ls_item-po_item.
            ls_poitemx-hl_item   = abap_true.
            ls_poitemx-si_cat    = abap_true.
            ls_poitemx-material  = abap_true.
            ls_poitemx-material_long  = abap_true.
            ls_poitemx-plant     = abap_true.
            ls_poitemx-conf_ctrl = abap_true.
            ls_poitemx-stge_loc  = abap_true.
            ls_poitemx-quantity  = abap_true.
            ls_poitemx-ematerial  = abap_true.
            ls_poitemx-tax_code  = abap_true.
            APPEND ls_poitemx TO et_itemx.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*Update Info record for AUT_SOURCE when supersession material is created
*Calling the FM to create Purchase infor records
    IF ls_e1einam IS NOT INITIAL AND ls_e1einem IS NOT INITIAL AND ls_extension IS NOT INITIAL.
      CALL FUNCTION 'Z_MM_PUR_INFO_RECORD_CREATE'
        EXPORTING
          is_e1einam   = ls_e1einam
          is_e1einem   = ls_e1einem
          is_extension = ls_extension
          iv_commit    = abap_true
        IMPORTING
          et_return    = lt_ret
          ev_idocnum   = lv_docnum.
      LOOP AT lt_ret INTO DATA(ls_ret) WHERE type CA 'EAX'.
        IF ls_ret-type IS NOT INITIAL AND ls_ret-id IS NOT INITIAL AND ls_ret-number IS NOT INITIAL.
          APPEND ls_ret TO me->gt_appl_log.
        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD process_order_status_0500.

*Data Declarations
    DATA:
      lv_fob_new   TYPE bprei,
      lv_docnum    TYPE edidc-docnum,
      lv_itemno    TYPE ebelp,
      ls_poitemx   TYPE bapimepoitemx,
      ls_e1einam   TYPE  e1einam,
      ls_e1einem   TYPE  e1einem,
      ls_extension TYPE  z1mm_auts,
      ls_poitem    TYPE bapimepoitem,
      lt_ret       TYPE bapiret2_t,
      ls_conf_add  TYPE bapimeconfdetail,
      ls_conf_addx TYPE bapimeconfdetailx.

    CLEAR:et_item,et_itemx,et_conf_add,et_conf_addx.

    READ TABLE me->gt_po_item
      INTO DATA(ls_item)
      WITH KEY material_long = iv_confirmed_matnr.
    IF sy-subrc <> 0.
      READ TABLE me->gt_poitem
        INTO ls_item
        WITH KEY material_long = iv_confirmed_matnr.
    ENDIF.
*Check whether supersession material or Ordered Material is available
*to read the PO items
    IF ls_item IS INITIAL.

*Read ordered material no
      READ TABLE me->gt_poitem INTO ls_item WITH KEY material_long = iv_ordered_matnr.

*If Superseeding material is available then update superseeding material
*in PO Else Update the Part no
      IF sy-subrc = 0.
        lv_itemno = ls_item-po_item + 1.
      ENDIF.

*--> Check for full superseeding or partial
      IF gv_total_ordqty = gv_con_mat_qty.
        DATA(lv_full_sup) = abap_true.
      ENDIF.

*-Create New confirmed material
      ls_poitem-po_item = lv_itemno.
      ls_poitem-hl_item = ls_item-po_item.
      ls_poitem-si_cat = gc_sub_cat. "'8'.
      ls_poitem-material = iv_confirmed_matnr.
      ls_poitem-material_long = iv_confirmed_matnr.
      ls_poitem-plant = ls_item-plant.
      ls_poitem-conf_ctrl = ls_item-conf_ctrl.
      ls_poitem-stge_loc = ls_item-stge_loc.
      ls_poitem-quantity = gv_con_mat_qty.
      ls_poitem-preq_no = ls_item-preq_no.
      ls_poitem-preq_item = ls_item-preq_item.
      IF iv_fob <> 0.
        ls_poitem-net_price = iv_fob.
      ELSE.
        ls_poitem-net_price = ls_item-net_price.
      ENDIF.
      ls_poitem-ematerial = iv_confirmed_matnr.
      ls_poitem-tax_code = ls_item-tax_code.
      APPEND ls_poitem TO et_item.

      ls_poitemx-po_itemx = abap_true.
      ls_poitemx-po_item = lv_itemno.
      ls_poitemx-hl_item   = abap_true.
      ls_poitemx-si_cat    = abap_true.
      ls_poitemx-material  = abap_true.
      ls_poitemx-material_long  = abap_true.
      ls_poitemx-plant     = abap_true.
      ls_poitemx-conf_ctrl = abap_true.
      ls_poitemx-stge_loc  = abap_true.
      ls_poitemx-quantity  = abap_true.
      ls_poitemx-net_price = abap_true.
      ls_poitemx-ematerial = abap_true.
      ls_poitemx-tax_code  = abap_true.
      APPEND ls_poitemx TO et_itemx.

*-Create Confirmation for newly created confirmed material
      ls_conf_add-item_no = lv_itemno.
      ls_conf_add-conf_category = gc_conf_type_ab.
      ls_conf_add-deliv_date_typ = 'D'.
      ls_conf_add-deliv_date = iv_eta.
      ls_conf_add-quantity = iv_process_qty.
      APPEND ls_conf_add TO et_conf_add.

      ls_conf_addx-item_no = lv_itemno.
      ls_conf_addx-item_nox = abap_true.
      ls_conf_addx-conf_category = abap_true.
      ls_conf_addx-deliv_date_typ = abap_true.
      ls_conf_addx-deliv_date = abap_true.
      ls_conf_addx-quantity = abap_true.
      ls_conf_addx-mpn = abap_true.
      APPEND ls_conf_addx TO et_conf_addx.
      CLEAR ls_conf_addx.

*-move details from main item to new one
      IF ( ls_item-quantity - iv_process_qty ) <> 0 AND lv_full_sup = abap_false.

        ls_poitem-po_item = lv_itemno + 1.
        ls_poitem-hl_item = ls_item-po_item."Highest Item
        ls_poitem-si_cat = gc_sub_cat. "'8'.
        ls_poitem-material = iv_ordered_matnr.
        ls_poitem-material_long = iv_ordered_matnr.
        ls_poitem-plant = ls_item-plant.
        ls_poitem-conf_ctrl = ls_item-conf_ctrl.
        ls_poitem-stge_loc = ls_item-stge_loc.
        ls_poitem-quantity = gv_ord_mat_qty.
        ls_poitem-net_price = ls_item-net_price.
        ls_poitem-ematerial = iv_ordered_matnr.
        ls_poitem-tax_code = ls_item-tax_code.
        ls_poitem-preq_no = ls_item-preq_no.
        ls_poitem-preq_item = ls_item-preq_item.
        APPEND ls_poitem TO et_item.

        ls_poitemx-po_itemx = abap_true.
        ls_poitemx-po_item = lv_itemno + 1.
        ls_poitemx-hl_item   = abap_true.
        ls_poitemx-si_cat    = abap_true.
        ls_poitemx-material  = abap_true.
        ls_poitemx-material_long = abap_true.
        ls_poitemx-plant     = abap_true.
        ls_poitemx-conf_ctrl = abap_true.
        ls_poitemx-stge_loc  = abap_true.
        ls_poitemx-quantity  = abap_true.
        ls_poitemx-net_price = abap_true.
        ls_poitemx-ematerial = abap_true.
        ls_poitemx-tax_code  = abap_true.
        APPEND ls_poitemx TO et_itemx.

*-Moving the confirmation from higher level item to partially superseeded item
        LOOP AT gt_conf_add_ab ASSIGNING FIELD-SYMBOL(<ls_conf_add>) WHERE item_no = ls_item-po_item.
          <ls_conf_add>-item_no = lv_itemno + 1.
        ENDLOOP.

      ENDIF.

*--> Get Vendor Country for info record and conf.Control key
      READ TABLE gt_vendor INTO DATA(ls_vendor)
            WITH KEY lifnr = me->gs_poheader-vendor
                     ekorg = me->gs_poheader-purch_org
            BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR ls_vendor.
      ENDIF.

*Map Info record to Update AUT_SOURCE
*Header
      CLEAR:ls_e1einam,ls_e1einem,ls_extension.
      ls_e1einam-matnr = iv_confirmed_matnr.
      ls_e1einam-lifnr = me->gs_poheader-vendor.
      ls_e1einam-meins = ls_item-po_unit.
      ls_e1einam-lmein = ls_item-po_unit.
*Item
      ls_e1einem-ekorg = me->gs_poheader-purch_org.
      ls_e1einem-ekgrp = me->gs_poheader-pur_group.
      ls_e1einem-waers = me->gs_poheader-currency.
      ls_e1einem-bstae = ls_vendor-bstae.
      ls_e1einem-uebtk = abap_true.

      IF ls_vendor-land1 = 'AU'.
        ls_e1einem-mwskz = 'P1'.
      ELSE.
        ls_e1einem-mwskz = 'P0'.
      ENDIF.

*Extension item
      ls_extension-aut_source = abap_true.

    ELSE.

      READ TABLE me->gt_po_item
        INTO ls_item
        WITH KEY material_long = iv_confirmed_matnr
                 si_cat   = '8'.
      IF sy-subrc <> 0.
        READ TABLE me->gt_poitem
          INTO ls_item
          WITH KEY material_long = iv_confirmed_matnr
                   si_cat   = '8'.
        IF sy-subrc <> 0.
          READ TABLE me->gt_po_item
            INTO ls_item
            WITH KEY material_long = iv_confirmed_matnr.
          IF sy-subrc <> 0.
            READ TABLE me->gt_poitem
              INTO ls_item
              WITH KEY material_long = iv_confirmed_matnr.
          ENDIF.
        ENDIF.
      ENDIF.

      IF sy-subrc = 0.
*--> AB Confirmations
        ls_conf_add-item_no = ls_item-po_item.
        ls_conf_add-conf_category = gc_conf_type_ab.
        ls_conf_add-deliv_date_typ = 'D'.
        ls_conf_add-deliv_date = iv_eta.
        ls_conf_add-quantity = iv_process_qty.
        APPEND ls_conf_add TO et_conf_add.
        CLEAR ls_conf_add.

        ls_conf_addx-item_no = ls_item-po_item.
        ls_conf_addx-item_nox = abap_true.
        ls_conf_addx-conf_category = abap_true.
        ls_conf_addx-deliv_date_typ = abap_true.
        ls_conf_addx-deliv_date = abap_true.
        ls_conf_addx-quantity = abap_true.
        ls_conf_addx-mpn = abap_true.
        APPEND ls_conf_addx TO et_conf_addx.
        CLEAR ls_conf_addx.

*--> Update Price in case of any difference
        IF iv_fob <> ls_item-net_price AND iv_fob > 0.
          ls_poitem-net_price = iv_fob.
*--> Check if the Item is already available, if exists update price other wise append new line
          READ TABLE me->gt_po_item TRANSPORTING NO FIELDS WITH KEY po_item = ls_item-po_item.
          IF sy-subrc = 0.
            MODIFY me->gt_po_item INDEX sy-tabix FROM ls_poitem TRANSPORTING net_price .
          ELSE.
            ls_poitem-po_item = ls_item-po_item.
            ls_poitem-hl_item = ls_item-hl_item.
            ls_poitem-si_cat = ls_item-si_cat.
            ls_poitem-material = ls_item-material.
            ls_poitem-material_long = ls_item-material_long.
            ls_poitem-plant = ls_item-plant.
            ls_poitem-conf_ctrl = ls_item-conf_ctrl.
            ls_poitem-stge_loc = ls_item-stge_loc.
            ls_poitem-quantity = ls_item-quantity.
            ls_poitem-preq_no = ls_item-preq_no.
            ls_poitem-preq_item = ls_item-preq_item.
            ls_poitem-ematerial = ls_item-ematerial.
            ls_poitem-tax_code = ls_item-tax_code.
            ls_poitem-preq_no = ls_item-preq_no.
            ls_poitem-preq_item = ls_item-preq_item.
            APPEND ls_poitem TO et_item.
          ENDIF.
*--> Update the PO Itemx
          ls_poitemx-net_price =   abap_true.
          READ TABLE me->gt_poitemx TRANSPORTING NO FIELDS WITH KEY po_item = ls_item-po_item.
          IF sy-subrc = 0.
            MODIFY me->gt_poitemx INDEX sy-tabix FROM ls_poitemx TRANSPORTING net_price.
          ELSE.
            ls_poitemx-po_itemx  = abap_true.
            ls_poitemx-po_item   = ls_item-po_item.
            ls_poitemx-hl_item   = abap_true.
            ls_poitemx-si_cat    = abap_true.
            ls_poitemx-material  = abap_true.
            ls_poitemx-material_long  = abap_true.
            ls_poitemx-plant     = abap_true.
            ls_poitemx-conf_ctrl = abap_true.
            ls_poitemx-stge_loc  = abap_true.
            ls_poitemx-quantity  = abap_true.
            ls_poitemx-ematerial  = abap_true.
            ls_poitemx-tax_code  = abap_true.
            APPEND ls_poitemx TO et_itemx.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

*Update Info record for AUT_SOURCE when supersession material is created
    IF ls_e1einam IS NOT INITIAL AND ls_e1einem IS NOT INITIAL AND ls_extension IS NOT INITIAL.
*Calling the FM to create Purchase infor records
      CALL FUNCTION 'Z_MM_PUR_INFO_RECORD_CREATE'
        EXPORTING
          is_e1einam   = ls_e1einam
          is_e1einem   = ls_e1einem
          is_extension = ls_extension
          iv_commit    = abap_true
        IMPORTING
          et_return    = lt_ret
          ev_idocnum   = lv_docnum.
      LOOP AT lt_ret INTO DATA(ls_ret) WHERE type CA 'EAX'.
        IF ls_ret-type IS NOT INITIAL AND ls_ret-id IS NOT INITIAL AND ls_ret-number IS NOT INITIAL.
          APPEND ls_ret TO me->gt_appl_log.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD process_order_status_5200.


    DATA:ls_poitemx TYPE bapimepoitemx,
         ls_poitem  TYPE bapimepoitem,
         ls_pr_data TYPE ty_pr_data.


    CLEAR : et_item,et_itemx,et_pr_data.

    READ TABLE gt_po_item INTO DATA(ls_item) WITH KEY material_long = iv_superseed_part_no
                                                hl_item  = iv_ord_itemno
                                                si_cat   = '8'.
    IF sy-subrc <> 0.
      TRY.
          ls_item = me->gt_poitem[ material_long = iv_superseed_part_no
                                   hl_item  = iv_ord_itemno
                                   si_cat   = '8' ].
        CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
          TRY.
              ls_item = me->gt_po_item[ material_long = iv_superseed_part_no ].
            CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
              TRY.
                  ls_item = me->gt_poitem[ material_long = iv_superseed_part_no ].
                CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
              ENDTRY.
          ENDTRY.

      ENDTRY.
    ENDIF.

    IF ls_item IS NOT INITIAL.
      ls_poitem-po_item = ls_item-po_item.
      ls_poitem-delete_ind = gc_delete_ind.
      ls_poitem-no_more_gr = 'X'.
      APPEND ls_poitem TO et_item.

      ls_poitemx-delete_ind = abap_true.
      ls_poitemx-no_more_gr = abap_true.
      ls_poitemx-po_item    = ls_poitem-po_item.
      APPEND ls_poitemx TO et_itemx.

*--> Update PR Related Data
      IF ls_item-preq_no IS INITIAL.
*--> Get the PR number from Higher level Item
        READ TABLE gt_po_item INTO DATA(ls_item_pr) WITH KEY po_item  = ls_item-hl_item.
        IF sy-subrc <> 0.
          TRY.
              ls_item_pr = me->gt_poitem[ po_item  = ls_item-hl_item ].
            CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        IF ls_item_pr IS NOT INITIAL.
          ls_pr_data-preq_no = ls_item_pr-preq_no.
          ls_pr_data-preq_item = ls_item_pr-preq_item.
          ls_pr_data-quantity = ls_poitem-quantity.
        ENDIF.

      ELSE.
        ls_pr_data-preq_no = ls_item-preq_no.
        ls_pr_data-preq_item = ls_item-preq_item.
        ls_pr_data-quantity = ls_poitem-quantity.
      ENDIF.
      ls_pr_data-delete_ind = abap_true.
      APPEND ls_pr_data TO et_pr_data.
    ENDIF.

  ENDMETHOD.


  METHOD process_order_status_5400.
*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*Data Declarations
    DATA:
      ls_poitemx TYPE bapimepoitemx,
      ls_poitem  TYPE bapimepoitem,
      ls_pr_data TYPE ty_pr_data.

    CLEAR :et_item,et_itemx,et_pr_data.


    READ TABLE gt_po_item INTO DATA(ls_item) WITH KEY material_long = iv_confirmed_material
                                                hl_item  = iv_ord_itemno
                                                si_cat   = '8'.
    IF sy-subrc <> 0.
      TRY.
          ls_item = me->gt_poitem[ material_long = iv_confirmed_material
                                         hl_item  = iv_ord_itemno
                                         si_cat   = '8' ].
        CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
          TRY.
              ls_item = me->gt_po_item[ material_long = iv_confirmed_material ].
            CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
              TRY.
                  ls_item = me->gt_poitem[ material_long = iv_confirmed_material ].
                CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
              ENDTRY.
          ENDTRY.

      ENDTRY.
    ENDIF.


    IF ls_item IS NOT INITIAL.

      ls_poitem-po_item   = ls_item-po_item.
      DATA(lv_quantity) = ls_item-quantity - iv_process_qty.
      IF lv_quantity = 0.
        ls_poitem-quantity = ls_item-quantity.
        ls_poitem-delete_ind = gc_delete_ind.
        ls_poitem-no_more_gr = abap_true.
        ls_pr_data-delete_ind = abap_true.
      ELSE.
        ls_poitem-quantity = lv_quantity.
      ENDIF.

      ls_poitemx-po_item   = ls_item-po_item.
      ls_poitemx-quantity  = abap_true.
      ls_poitemx-po_itemx  = abap_true.
      IF lv_quantity = 0.
        ls_poitemx-delete_ind = abap_true.
        ls_poitemx-no_more_gr = abap_true.
      ENDIF.

      READ TABLE me->gt_po_item TRANSPORTING NO FIELDS WITH KEY po_item   = ls_item-po_item.
      IF sy-subrc = 0.
        MODIFY me->gt_po_item INDEX sy-tabix FROM ls_poitem TRANSPORTING quantity delete_ind no_more_gr.
      ELSE.
        APPEND ls_poitem TO et_item.
      ENDIF.

      READ TABLE me->gt_poitemx TRANSPORTING NO FIELDS WITH KEY po_item   = ls_item-po_item.
      IF sy-subrc = 0.
        MODIFY me->gt_poitemx INDEX sy-tabix FROM ls_poitemx TRANSPORTING quantity delete_ind no_more_gr.
      ELSE.
        APPEND ls_poitemx TO et_itemx.
      ENDIF.

*--> Update PR Related Data
      IF ls_item-preq_no IS INITIAL.
*--> Get the PR number from Higher level Item
        READ TABLE gt_po_item INTO DATA(ls_item_pr) WITH KEY po_item  = ls_item-hl_item.
        IF sy-subrc <> 0.
          TRY.
*--> cannot use sorted key because the search is based on higher level item
              ls_item_pr = me->gt_poitem[ po_item  = ls_item-hl_item ].
            CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        IF ls_item_pr IS NOT INITIAL.
          ls_pr_data-preq_no = ls_item_pr-preq_no.
          ls_pr_data-preq_item = ls_item_pr-preq_item.
          ls_pr_data-quantity = ls_poitem-quantity.
          APPEND ls_pr_data TO et_pr_data.
        ENDIF.

      ELSE.
        ls_pr_data-preq_no = ls_item-preq_no.
        ls_pr_data-preq_item = ls_item-preq_item.
        ls_pr_data-quantity = ls_poitem-quantity.
        APPEND ls_pr_data TO et_pr_data.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD process_order_status_5500.

*Data Declarations
    DATA:
      ls_poitemx TYPE bapimepoitemx,
      ls_poitem  TYPE bapimepoitem,
      ls_pr_data TYPE ty_pr_data.

    CLEAR : et_item,et_itemx,et_pr_data.


    READ TABLE gt_po_item INTO DATA(ls_item) WITH KEY material_long = iv_confirmed_material
                                                hl_item  = iv_ord_itemno
                                                si_cat   = '8'.
    IF sy-subrc <> 0.
      TRY.
          ls_item = me->gt_poitem[ material_long = iv_confirmed_material
                                         hl_item  = iv_ord_itemno
                                         si_cat   = '8' ].
        CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
          TRY.
              ls_item = me->gt_po_item[ material_long = iv_confirmed_material ].
            CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
              TRY.
                  ls_item = me->gt_poitem[ material_long = iv_confirmed_material ].
                CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
              ENDTRY.
          ENDTRY.

      ENDTRY.
    ENDIF.

    IF ls_item IS NOT INITIAL.

      ls_poitem-po_item   = ls_item-po_item.
      DATA(lv_quantity) = ls_item-quantity - iv_process_qty.
      IF lv_quantity = 0.
        ls_poitem-quantity = ls_item-quantity.
        ls_poitem-delete_ind = gc_delete_ind.
        ls_poitem-no_more_gr = abap_true.
        ls_pr_data-delete_ind = abap_true.
      ELSE.
        ls_poitem-quantity = lv_quantity.
      ENDIF.

      ls_poitemx-po_item   = ls_item-po_item.
      ls_poitemx-quantity  = abap_true.
      ls_poitemx-po_itemx  = abap_true.
      IF lv_quantity = 0.
        ls_poitemx-delete_ind = abap_true.
        ls_poitemx-no_more_gr = abap_true.
      ENDIF.

      READ TABLE me->gt_po_item TRANSPORTING NO FIELDS WITH KEY po_item   = ls_item-po_item.
      IF sy-subrc = 0.
        MODIFY me->gt_po_item INDEX sy-tabix FROM ls_poitem TRANSPORTING quantity delete_ind no_more_gr.
      ELSE.
        APPEND ls_poitem TO et_item.
      ENDIF.

      READ TABLE me->gt_poitemx TRANSPORTING NO FIELDS WITH KEY po_item   = ls_item-po_item.
      IF sy-subrc = 0.
        MODIFY me->gt_poitemx INDEX sy-tabix FROM ls_poitemx TRANSPORTING quantity delete_ind no_more_gr.
      ELSE.
        APPEND ls_poitemx TO et_itemx.
      ENDIF.


*--> Update PR Related Data
      IF ls_item-preq_no IS INITIAL.
*--> Get the PR number from Higher level Item
        READ TABLE gt_po_item INTO DATA(ls_item_pr) WITH KEY po_item  = ls_item-hl_item.
        IF sy-subrc <> 0.
          TRY.
*--> cannot use sorted key because the search is based on higher level item
              ls_item_pr = me->gt_poitem[ po_item  = ls_item-hl_item ].
            CATCH cx_sy_itab_line_not_found   ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        IF ls_item_pr IS NOT INITIAL.
          ls_pr_data-preq_no = ls_item_pr-preq_no.
          ls_pr_data-preq_item = ls_item_pr-preq_item.
          ls_pr_data-quantity = ls_poitem-quantity.
          APPEND ls_pr_data TO et_pr_data.
        ENDIF.
      ELSE.
        ls_pr_data-preq_no = ls_item-preq_no.
        ls_pr_data-preq_item = ls_item-preq_item.
        ls_pr_data-quantity = ls_poitem-quantity.
        APPEND ls_pr_data TO et_pr_data.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD process_status_codes.

    DATA: ls_conf_del  TYPE bapimeconfdetail,
          ls_conf_delx TYPE bapimeconfdetailx,
          ls_yekes     TYPE uekes.

*-Delete all the AB's
    LOOP AT me->gt_poitem INTO DATA(ls_item) WHERE material_long = iv_confirmed_matnr OR
                                                   material_long = iv_ordered_matnr.

      LOOP AT me->gt_poconfirmation INTO DATA(ls_confirmation) WHERE po_item = ls_item-po_item AND
                                                                     ( conf_type = 'AB' OR conf_type = 'BO' ) .
        ls_yekes-ebeln = me->gs_poheader-po_number.
        ls_yekes-ebelp = ls_confirmation-po_item .
        ls_yekes-etens = ls_confirmation-conf_ser.
        ls_yekes-kz = 'D'.
        APPEND ls_yekes TO gt_yekes .
        CLEAR ls_yekes.
      ENDLOOP.
    ENDLOOP.

*Logic for Order status code
    CASE iv_order_status_code.
*B/O
      WHEN gc_bo_0400.   " Add to backorder

        CALL METHOD me->process_order_status_0400
          EXPORTING
            iv_ordered_matnr   = iv_ordered_matnr
            iv_confirmed_matnr = iv_confirmed_matnr
            iv_fob             = iv_fob
            iv_process_qty     = iv_process_qty
            iv_eta             = iv_eta
          IMPORTING
            et_item            = DATA(lt_poitem)
            et_itemx           = DATA(lt_poitemx)
            et_conf_add        = DATA(lt_conf_add)
            et_conf_addx       = DATA(lt_conf_addx).

*Allocation
      WHEN gc_alloc_0500. "0500

        CALL METHOD me->process_order_status_0500
          EXPORTING
            iv_ordered_matnr   = iv_ordered_matnr
            iv_confirmed_matnr = iv_confirmed_matnr
            iv_fob             = iv_fob
            iv_process_qty     = iv_process_qty
            iv_eta             = iv_eta
          IMPORTING
            et_item            = lt_poitem
            et_itemx           = lt_poitemx
            et_conf_add        = lt_conf_add
            et_conf_addx       = lt_conf_addx.

*Order Cancel (Order Error)
      WHEN gc_ord_cancel_5200.                              "gc_5200

        CALL METHOD me->process_order_status_5200
          EXPORTING
            iv_superseed_part_no = iv_confirmed_matnr
            iv_ord_itemno        = iv_ord_itemno
          IMPORTING
            et_item              = lt_poitem
            et_itemx             = lt_poitemx
            et_pr_data           = DATA(lt_pr_data).

*Order Cancel (B/O CUT)
      WHEN gc_order_cancel_bo_5400.

        CALL METHOD me->process_order_status_5400
          EXPORTING
            iv_confirmed_material = iv_confirmed_matnr
            iv_process_qty        = iv_process_qty
            iv_ord_itemno         = iv_ord_itemno
          IMPORTING
            et_item               = lt_poitem
            et_itemx              = lt_poitemx
            et_pr_data            = lt_pr_data.


*Order Cancel (Allocation Cancel)
      WHEN gc_order_cancel_ab_5500.

        CALL METHOD me->process_order_status_5500
          EXPORTING
            iv_confirmed_material = iv_confirmed_matnr
            iv_process_qty        = iv_process_qty
            iv_ord_itemno         = iv_ord_itemno
          IMPORTING
            et_item               = lt_poitem
            et_itemx              = lt_poitemx
            et_pr_data            = lt_pr_data.

      WHEN OTHERS.
    ENDCASE.

*--> Store the data in global tables
    APPEND LINES OF lt_poitem    TO me->gt_po_item.
    APPEND LINES OF lt_poitemx   TO me->gt_poitemx.
    APPEND LINES OF lt_conf_add  TO me->gt_conf_add_ab.
    APPEND LINES OF lt_conf_addx TO me->gt_conf_addx_ab.
    APPEND LINES OF lt_pr_data   TO me->gt_pr_data.

  ENDMETHOD.


  METHOD process_supersession_chain.

    CLEAR ev_picnum.

*Check for supersession material
    me->get_supersessionchain(
      EXPORTING
        iv_matnr                       = iv_ordered_matnr
        iv_superseed_part_no           = iv_confirmed_matnr
      IMPORTING
        ev_sup_session_not_exists      = DATA(lv_sup_session_not_exists)
        ev_sup_session_exists_ord_mat  = DATA(lv_sup_session_exists_ord_mat)
        ev_sup_session_exists_conf_mat = DATA(lv_sup_session_exists_conf_mat)
        ev_sup_different_ord_conf_mat  = DATA(lv_sup_different_ord_conf_mat)
        ev_sup_ord_mat_not_exists      = DATA(lv_sup_ord_mat_not_exists)
        et_picpsrl                     = DATA(lt_picpsrl) ).

*Supersession chain does not exist
    IF lv_sup_session_not_exists = abap_true.
*Create Supersession Chain
      me->create_supersessionchain(
        EXPORTING
          iv_ordered_matnr     = iv_ordered_matnr
          iv_confirmed_matnr   = iv_confirmed_matnr
        IMPORTING
          ev_pic_num           = ev_picnum ).
    ENDIF.

*Supersession chain already exists for the part no and supersession material
    IF lv_sup_session_exists_conf_mat = abap_true AND lt_picpsrl IS NOT INITIAL.
      DATA(ls_pic_num) = lt_picpsrl[ 1 ].
      ev_picnum = ls_pic_num-picnum.
*Message when Supersession chain created or extended
      MESSAGE s075(zp2p) WITH ev_picnum INTO data(lv_dummy) ##NEEDED.
      me->log_mesg( ).
      RETURN.
    ENDIF.

*Supersession chain already exist for Material master available in Part Number
    IF lv_sup_session_exists_ord_mat = abap_true.
*Change Supersession Chain
      me->add_create_supersessionchain(
         EXPORTING
           iv_part_no           = iv_ordered_matnr
           iv_superseed_part_no = iv_confirmed_matnr
           it_picpsrl           = lt_picpsrl
         IMPORTING
           ev_pic_num           = ev_picnum ).
    ENDIF.

*Supersession chain for the superseeding material and part no are different
    IF lv_sup_different_ord_conf_mat = abap_true.
      MESSAGE e109(zp2p) WITH iv_ordered_matnr iv_confirmed_matnr INTO lv_dummy ##NEEDED.
      me->log_mesg( ).
    ENDIF.

*Supersession chain not exists for ordered Material
    IF lv_sup_ord_mat_not_exists = abap_true.
*Insert Supersession Chain
      me->insert_supersessionchain(
         EXPORTING
           iv_part_no           = iv_ordered_matnr
           iv_superseed_part_no = iv_confirmed_matnr
           it_picpsrl           = lt_picpsrl
         IMPORTING
           ev_pic_num           = ev_picnum ).

    ENDIF.

    IF ev_picnum IS NOT INITIAL.
* Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

*Message when Supersession chain created
      MESSAGE s075(zp2p) WITH ev_picnum INTO lv_dummy ##NEEDED.
      me->log_mesg( ).
    ENDIF.

  ENDMETHOD.


METHOD set_po_detail.

*- In the earlier code, BAPI_PO_GETDETAIL1 was used for PO selection
*- This bapi selects all the details of the PO which was unnecessary and slow
*- so the bapi BAPI_PO_GETDETAIL is used now with required details only.
*- Due to time constraints and the changes will result in major restructuring
*- So the data from BAPI_PO_GETDETAIL is mapped to the global structures of BAPI_PO_GETDETAIL1,
*- this is because changing the staructure type will result in major changes across the class

  DATA ls_header TYPE bapiekkol.
  DATA lt_items TYPE TABLE OF bapiekpo.
  DATA ls_po_item TYPE bapimepoitem.
  DATA lt_confirmations TYPE TABLE OF bapiekes.
  DATA lt_return TYPE TABLE OF bapireturn.

  CALL FUNCTION 'BAPI_PO_GETDETAIL'
    EXPORTING
      purchaseorder         = iv_ebeln
      items                 = 'X'
      schedules             = 'X'
      confirmations         = 'X'
    IMPORTING
      po_header             = ls_header
    TABLES
      po_items              = lt_items
      po_item_schedules     = me->gt_poschedule
      po_item_confirmations = lt_confirmations
      return                = lt_return.

  SORT me->gt_poschedule BY po_item ASCENDING deliv_date DESCENDING.
  me->gt_poconfirmation = lt_confirmations.

  me->gs_poheader-po_number = ls_header-po_number.
  me->gs_poheader-vendor = ls_header-vendor.
  me->gs_poheader-purch_org = ls_header-purch_org.
  me->gs_poheader-pur_group = ls_header-pur_group.
  me->gs_poheader-currency = ls_header-currency.
  me->gs_poheader-comp_code = ls_header-co_code.
  me->gs_poheader-doc_type = ls_header-doc_type.

  LOOP AT lt_items INTO DATA(ls_item) WHERE delete_ind IS INITIAL.
    ls_po_item-material_long = ls_item-material_long.
    ls_po_item-po_item = ls_item-po_item.
    ls_po_item-si_cat = ls_item-si_cat.
    ls_po_item-hl_item = ls_item-hl_item.
    ls_po_item-quantity = ls_item-quantity.
    ls_po_item-plant = ls_item-plant.
    ls_po_item-conf_ctrl = ls_item-conf_ctrl.
    ls_po_item-stge_loc = ls_item-store_loc.
    ls_po_item-preq_no = ls_item-preq_no.
    ls_po_item-preq_item = ls_item-preq_item.
    ls_po_item-net_price = ls_item-net_price.
    ls_po_item-tax_code = ls_item-tax_code.
    ls_po_item-po_unit = ls_item-unit.
    ls_po_item-ematerial = ls_item-material.
    ls_po_item-material = ls_item-material.
    INSERT ls_po_item INTO TABLE me->gt_poitem.
  ENDLOOP.

ENDMETHOD.


  METHOD update_po_without_supersession.


    IF  ( is_item-order_status_code = gc_alloc_0500 OR is_item-order_status_code = gc_bo_0400 ). "M- DECK915045
      me->get_eta( EXPORTING is_item = is_item  iv_ordered_item = iv_ordered_item
                   IMPORTING ev_date = DATA(lv_date) ).
    ELSE.
      lv_date = gc_ab_date.
    ENDIF.

    IF me->error_logged( ) = abap_false.

      me->process_status_codes(
         EXPORTING
           iv_ordered_matnr     = is_item-ordered_material
           iv_confirmed_matnr   = is_item-confirmed_pomaterial
           iv_order_status_code = is_item-order_status_code
           iv_fob               = is_item-confirmed_fobprice
           iv_process_qty       = is_item-processed_quantity
           iv_ord_itemno        = iv_ordered_item
           iv_eta               = lv_date ).

    ENDIF.

  ENDMETHOD.


  METHOD update_po_with_supersession.


*Check For superseeding matnr
    READ TABLE gt_matdata ASSIGNING FIELD-SYMBOL(<ls_mat>)
                       WITH KEY matnr = is_item-confirmed_pomaterial.

    IF sy-subrc IS NOT INITIAL.

*Create supersession material
      me->create_supersession_matnr(
         EXPORTING
           iv_ordered_matnr     = is_item-ordered_material
           iv_description       = is_item-confirmed_description
           iv_confirmed_matnr   = is_item-confirmed_pomaterial )." Supersession Material

      IF me->error_logged( ) = abap_false.

*Check and Create supersession material
        me->process_supersession_chain(
           EXPORTING
             iv_ordered_matnr   = is_item-ordered_material
             iv_confirmed_matnr = is_item-confirmed_pomaterial " Supersession Material
           IMPORTING
             ev_picnum          = DATA(lv_pic_num) ).

**--> Begin of insert for H1S-8347
        CALL METHOD me->create_pricing_conditions
          EXPORTING
            iv_lifnr   = me->gs_poheader-vendor
            iv_ekorg   = me->gs_poheader-purch_org
            iv_matnr   = is_item-confirmed_pomaterial
            iv_old_mat = is_item-ordered_material
            iv_meins   = me->gv_meins
          IMPORTING
            et_return  = DATA(lt_return).
        IF lt_return IS NOT INITIAL.
          APPEND LINES OF lt_return TO me->gt_appl_log.
          CLEAR lt_return.
        ENDIF.
**--> End of insert for H1S-8347

      ENDIF.

    ELSE.

*Check and create supersession material
      me->process_supersession_chain(
        EXPORTING
          iv_ordered_matnr     = is_item-ordered_material
          iv_confirmed_matnr   = is_item-confirmed_pomaterial " Supersession Material
        IMPORTING
          ev_picnum            = lv_pic_num ).

    ENDIF.

*GV_UPD_MAS_DATA = 'X' then Only create new materials and supercession rules.
*Do not update the purchase order or it's confirmations.
    IF me->error_logged( ) = abap_false AND lv_pic_num IS NOT INITIAL AND gv_upd_mas_data IS INITIAL.

      IF is_item-order_status_code = gc_alloc_0500 ."OR is_item-order_status_code = gc_bo_0400 )."M-DECK915661
        me->get_eta( EXPORTING is_item = is_item
                               iv_ordered_item = iv_ordered_item
                     IMPORTING ev_date = DATA(lv_date) ).
      ELSE.
        lv_date = gc_ab_date.
      ENDIF.

      IF me->error_logged( ) = abap_false.

        me->process_status_codes(
           EXPORTING
             iv_ordered_matnr     = is_item-ordered_material
             iv_confirmed_matnr   = is_item-confirmed_pomaterial " Supersession Material
             iv_order_status_code = is_item-order_status_code
             iv_fob               = is_item-confirmed_fobprice
             iv_process_qty       = is_item-processed_quantity
             iv_ord_itemno        = iv_ordered_item
             iv_eta               = lv_date ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD update_pr_quantity.


    DATA: lt_return TYPE TABLE OF bapiret2,
          lt_item   TYPE TABLE OF bapimereqitemimp,
          ls_item   TYPE bapimereqitemimp,
          ls_itemx  TYPE bapimereqitemx,
          lt_itemx  TYPE TABLE OF bapimereqitemx.

    SORT gt_pr_data BY preq_no preq_item.
    DELETE ADJACENT DUPLICATES FROM gt_pr_data COMPARING preq_no preq_item.

    LOOP AT me->gt_pr_data INTO DATA(ls_pr_data).

*--> Fill PR ITem Details
      ls_item-preq_item = ls_itemx-preq_item = ls_pr_data-preq_item.
      ls_itemx-preq_itemx = abap_true.

*--> Delete Indicator
      IF ls_pr_data-delete_ind = abap_true.
        ls_item-delete_ind = abap_true.
        ls_itemx-delete_ind = abap_true.
      ELSE.
*--> Quantity
        ls_item-quantity = ls_pr_data-quantity.
        ls_itemx-quantity = abap_true.
      ENDIF.
      APPEND: ls_item  TO lt_item,
              ls_itemx TO lt_itemx.
*--> Call the BAPI to Update PR
      CALL FUNCTION 'BAPI_PR_CHANGE'
        EXPORTING
          number  = ls_pr_data-preq_no
        TABLES
          return  = lt_return
          pritem  = lt_item
          pritemx = lt_itemx.

      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CO 'AEX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
*--> PR not updated
        MESSAGE w564(zp2p) WITH ls_pr_data-preq_no INTO DATA(lv_dummy).
      ELSE.
*--> PR updated
        MESSAGE s563(zp2p) WITH ls_pr_data-preq_no INTO lv_dummy.
      ENDIF.
      me->log_mesg( ).

      CLEAR: lt_return,lt_itemx,lt_item.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_po_item_quantity.


    DATA: lt_po_s TYPE SORTED TABLE OF zsmm_back_order_confirmation WITH NON-UNIQUE KEY ordered_material confirmed_pomaterial.
    DATA: lv_sum  TYPE zsmm_back_order_confirmation-processed_quantity.

    CLEAR: gv_total_ordqty,gv_ord_mat_qty,gv_con_mat_qty.

    IF is_order_data-order_status_code = gc_ord_cancel_5200.
      RETURN.
    ENDIF.

    lt_po_s = me->gt_back_conf_data.

    READ TABLE me->gt_poitem
      INTO DATA(ls_item)
      WITH KEY material_long = is_order_data-ordered_material
               si_cat = space.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    gv_total_ordqty = ls_item-quantity.

    LOOP AT lt_po_s INTO DATA(ls_po_s) WHERE ordered_material = is_order_data-ordered_material.
      lv_sum = lv_sum + ls_po_s-processed_quantity.

      IF ls_po_s-substitution_mark = 'E'.
        IF ls_po_s-order_status_code = gc_bo_0400 OR ls_po_s-order_status_code = gc_alloc_0500.
          gv_con_mat_qty = gv_con_mat_qty + ls_po_s-processed_quantity.
        ENDIF.
      ELSE.
        IF ls_po_s-order_status_code = gc_bo_0400 OR ls_po_s-order_status_code = gc_alloc_0500.
          gv_ord_mat_qty = gv_ord_mat_qty + ls_po_s-processed_quantity.
        ENDIF.
      ENDIF.

      IF ls_po_s-order_status_code = gc_order_cancel_bo_5400 OR ls_po_s-order_status_code = gc_order_cancel_ab_5500 .
        gv_total_ordqty = gv_total_ordqty - ls_po_s-processed_quantity.
      ENDIF.
    ENDLOOP.

    rv_failed = boolc( lv_sum <> ls_item-quantity ).

    IF gv_total_ordqty <> gv_ord_mat_qty + gv_con_mat_qty.
      rv_failed = abap_true.
    ENDIF.

* Quantity Mismatch
    IF rv_failed = abap_true.
      MESSAGE e535(zp2p) WITH  me->gs_poheader-po_number is_order_data-ordered_material
                              ls_item-quantity lv_sum INTO DATA(lv_message) ##NEEDED.
      me->log_mesg( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
