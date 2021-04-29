class ZCL_WM_CONNOTE_GENERATION definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_dn_header,
        vbeln               TYPE vbeln_vl,
        vstel               TYPE vstel,
        kunnr               TYPE kunwe,
        anzpk               TYPE anzpk,
        lifex               TYPE lifex,
        zzcarr_service      TYPE zsd_carserv,
        zzcarr_acc_no       TYPE zsd_car_acc_no,
        zzconnote_prefix    TYPE zsd_con_prefix,
        zzconnote_indc      TYPE zsd_connote_indc,
        zzconnote_num_range TYPE zsd_connote_num_range,
      END OF ty_dn_header .
  types:
    BEGIN OF ty_service,
        carrier           TYPE zsd_lifnr,
        service           TYPE zsd_service,
        pack_name         TYPE zsd_pack_name,
        carr_acc_no       TYPE zsd_car_acc_no,
        connote_prefix    TYPE zsd_con_prefix,
        connote_indc      TYPE zsd_connote_indc,
        connote_num_range TYPE zsd_connote_num_range,
      END OF ty_service .
  types:
    ty_t_contoneno TYPE SORTED TABLE OF zsdd_connoteno WITH NON-UNIQUE KEY deliveryno .
  types:
    ty_t_service   TYPE STANDARD TABLE OF ty_service .

  class-data GV_LIFEX type LIFEX .

  class-methods GET_REFERENCE
    importing
      !IV_DELIVERY type VBELN_VL
      !IV_SERVICE type ZSD_CARSERV optional
    returning
      value(RV_REF) type ref to ZCL_WM_CONNOTE_GENERATION .
  methods CONSTRUCTOR
    importing
      !IV_DELIVERY type VBELN_VL
      !IV_SERVICE type ZSD_CARSERV optional .
  methods CLEAR_OBJECT .
  methods EXECUTE
    importing
      !IV_HU_EXT type EXIDV
      !IV_HU_INT type VENUM optional
      !IV_ACTION type CHAR1 optional
      !IV_HU_ITEM type VEPOS optional
    exceptions
      CONNOTE_NOT_GENERATED
      DN_UPDATE_FAILED
      HU_UPDATE_FAILED .
  methods GENERATE_CONNOTE_NUMBER
    importing
      !IS_DN_HEADER type TY_DN_HEADER
    exporting
      !EV_CONNOTENO type ZSD_CONNOTE
      !EV_DN_UPDATE type CHAR1 .
  methods EXECUTE_MUL_HU
    importing
      !IT_HU_DET type HUM_REHANG_HU_T
      !IS_VBKOK type VBKOK optional
    exceptions
      CONNOTE_NOT_GENERATED
      DN_UPDATE_FAILED
      HU_UPDATE_FAILED
      GOODS_ISSUE_FAILED .
protected section.
private section.

  constants GC_0102 type TDID value '0102' ##NO_TEXT.
  constants GC_VBBK type TDOBJECT value 'VBBK' ##NO_TEXT.
  data GV_URL type AD_URISCR .
  class-data GV_REF type ref to ZCL_WM_CONNOTE_GENERATION .
  data GS_DN_HEADER type TY_DN_HEADER .
  data GS_CONNOTENO type ZSDD_CONNOTENO .
  data GT_CONNOTENO type TY_T_CONTONENO .
  data GT_SERVICE type TY_T_SERVICE .
  constants GC_CONSNUMBER type AD_CONSNUM value '001' ##NO_TEXT.
  constants GC_FORWARDING_AGENT type PARVW value 'SP' ##NO_TEXT.
  constants GC_URI_TYPE type AD_URITYPE value 'ROU' ##NO_TEXT.
  constants GC_O type C value 'O' ##NO_TEXT.
  constants GC_N type C value 'N' ##NO_TEXT.
  constants GC_Y type C value 'Y' ##NO_TEXT.

  methods UPDATE_DELIVERY
    importing
      !IV_CONNOTENO type ZSD_CONNOTE
      !IV_DN_UPDATE type CHAR1
      !IV_ACTION type CHAR1 optional
      !IT_HU_DET type HUM_REHANG_HU_T optional
      !IS_VBKOK type VBKOK optional
    exporting
      !EV_RETURN type CHAR1 .
  methods UPDATE_HU
    importing
      !IV_HU_EXT type EXIDV
      !IV_HU_INT type VENUM optional
      !IV_CONNOTENO type ZSD_CONNOTE
      !IV_HU_ITEM type VEPOS optional
    exporting
      !EV_RETURN type CHAR1 .
  methods UPDATE_MUL_HU
    importing
      !IV_CONNOTENO type ZSD_CONNOTE
      !IT_HU_DET type HUM_REHANG_HU_T
      !IS_VBKOK type VBKOK
    exporting
      !EV_RETURN type CHAR1 .
  methods GOODS_ISSUE
    importing
      !IS_VBKOK type VBKOK optional
    exporting
      !EV_RETURN type CHAR1 .
ENDCLASS.



CLASS ZCL_WM_CONNOTE_GENERATION IMPLEMENTATION.


  METHOD clear_object.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to clear class reference
* Developer: I345486
*----------------------------------------------------------------------*
    FREE gv_ref.
  ENDMETHOD.


  METHOD constructor.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Constructor Method to get Delivery header details and Connote data
* Developer: I345486
*----------------------------------------------------------------------*

    DATA:lt_bapiaduri  TYPE STANDARD TABLE OF bapiaduri,
         ls_header_ext TYPE zzwm_likp_cust_field.

    CLEAR:gs_connoteno, gs_dn_header,gt_connoteno[],gv_url.
*--> Get Delivery header data
    SELECT SINGLE vbeln                 "Delivery
                  vstel                 "Shpping Point
                  kunnr                 "Ship to
                  anzpk                 "No of Packages
                  lifex                 "Ext.Document
                  zzcarr_service        "Service Code
                  zzcarr_acc_no         "Account number
                  zzconnote_prefix      "Connote Prefix
                  zzconnote_indc        "Connote Indicator
                  zzconnote_num_range   "Number range object
      FROM likp
      INTO gs_dn_header
      WHERE vbeln = iv_delivery.
    IF sy-subrc = 0.
*--> Get the forwarding agent details for the Delivery
      SELECT lifnr
        FROM vbpa UP TO 1 ROWS
        INTO @DATA(lv_vendor)
        WHERE vbeln = @iv_delivery
         AND  parvw = @gc_forwarding_agent.
      ENDSELECT.
      IF sy-subrc = 0.
*--> Get the carrier service details from ZSDM_FA_SVC_DATA
        SELECT  carrier,
                service,
                pack_name,
                carr_acc_no,
                connote_prefix,
                connote_indc,
                connote_num_range
          FROM  zsdm_fa_svc_data
          INTO TABLE @gt_service
          WHERE carrier = @lv_vendor.
        IF sy-subrc = 0.
          SORT gt_service BY service.
        ENDIF.
      ENDIF.
*--> if number range is initial, update it from Z table
      IF iv_service IS NOT INITIAL.
        READ TABLE gt_service INTO DATA(ls_service) WITH KEY service = iv_service BINARY SEARCH.
        IF sy-subrc = 0.
          gs_dn_header-zzcarr_service      = ls_service-service.
          gs_dn_header-zzcarr_acc_no       = ls_service-carr_acc_no.
          gs_dn_header-zzconnote_prefix    = ls_service-connote_prefix.
          gs_dn_header-zzconnote_indc      = ls_service-connote_indc.
          gs_dn_header-zzconnote_num_range = ls_service-connote_num_range.

*--> Call the singleton class to set the medium of updating the
          DATA(lo_ui) =  zcl_sd_initiate_lbl_creation=>get_object( ).
          lo_ui->set_medium( EXPORTING iv_medium = 'F' ).

*--> Set Delivery Extension for Carrier service.
          ls_header_ext-zzcarr_service = iv_service.
          zcl_wm_delivery_extension=>set_deli_extension(
            EXPORTING
              is_set_header_ext = ls_header_ext ).
        ENDIF.
      ENDIF.

*--> get data from ZSDD_CONNOTENO for Consolidated case
      IF gs_dn_header-zzconnote_indc = gc_y.
*--> In case of Consolidation case multiple DN will be assigned to same connote no
        SELECT * FROM zsdd_connoteno
          INTO TABLE gt_connoteno
          WHERE shippingpoint = gs_dn_header-vstel               "Shpping Point
            AND servicecode   = gs_dn_header-zzcarr_service      "Service Code
            AND shipto        = gs_dn_header-kunnr               "Ship to
            AND accountno     = gs_dn_header-zzcarr_acc_no       "Account No
            AND object        = gs_dn_header-zzconnote_num_range "Number range object
            AND connotedate   = sy-datum                         "Connote date
            AND connotestatus = gc_o.                            "Connote status
        IF sy-subrc = 0.
          gs_connoteno = gt_connoteno[ 1 ].
        ENDIF.
      ELSE.
*--> Get data for non consolidated case
        SELECT SINGLE *
          FROM zsdd_connoteno
          INTO gs_connoteno
          WHERE shippingpoint = gs_dn_header-vstel             "Shpping Point
            AND servicecode   = gs_dn_header-zzcarr_service    "Servoce Code
            AND shipto        = gs_dn_header-kunnr             "Ship to
            AND accountno     = gs_dn_header-zzcarr_acc_no     "Account No
            AND deliveryno    = gs_dn_header-vbeln             "Delivery
            AND connotedate   = sy-datum.                      "Connote date
        IF sy-subrc <> 0.
          CLEAR gs_connoteno.
        ENDIF.
      ENDIF.

      IF lv_vendor IS NOT INITIAL.
*--> Get the tracking URL for the forwarding agent.
        CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
          EXPORTING
            businesspartner = lv_vendor
          TABLES
            bapiaduri       = lt_bapiaduri.
*--> Get the tracking url to update tracking number in Delivery header text.
        IF lt_bapiaduri IS NOT INITIAL.
          SORT lt_bapiaduri BY uri_type consnumber.
          READ TABLE lt_bapiaduri INTO DATA(ls_bapiaduri)
                                  WITH KEY uri_type   = gc_uri_type   "ROU'
                                           consnumber = gc_consnumber
                                  BINARY SEARCH.
          IF sy-subrc = 0.
            gv_url = ls_bapiaduri-uri.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD execute.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to start the Call sequence of Methods
* Developer: I345486
*----------------------------------------------------------------------*


*--> Call the Method to Generate a Connote Number
    generate_connote_number(
      EXPORTING
        is_dn_header = gs_dn_header
      IMPORTING
        ev_connoteno = DATA(lv_connoteno)
        ev_dn_update = DATA(lv_dn_update) ).

    IF lv_connoteno IS INITIAL.
      RAISE connote_not_generated.
    ENDIF.

*--> Call the Method to Update the DN with Connote number
    update_delivery(
      EXPORTING
        iv_connoteno = lv_connoteno
        iv_dn_update = lv_dn_update
        iv_action    = iv_action
      IMPORTING
        ev_return    = DATA(lv_return) ).

    IF lv_return IS NOT INITIAL.
      RAISE dn_update_failed.
    ENDIF.

*--> Call the Method to Update HU with Article details
    update_hu(
      EXPORTING
        iv_hu_ext    = iv_hu_ext
        iv_hu_int    = iv_hu_int
        iv_connoteno = lv_connoteno
        iv_hu_item   = iv_hu_item
      IMPORTING
        ev_return    = lv_return ).

    IF lv_return IS NOT INITIAL.
      RAISE hu_update_failed.
    ENDIF.

    COMMIT WORK AND WAIT.

*--> Destroy the reference
    clear_object( ).


  ENDMETHOD.


  METHOD execute_mul_hu.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to start the Call sequence of Methods
* Developer: I345486
*----------------------------------------------------------------------*


*--> Call the Method to Generate a Connote Number
    generate_connote_number(
      EXPORTING
        is_dn_header = gs_dn_header
      IMPORTING
        ev_connoteno = DATA(lv_connoteno)
        ev_dn_update = DATA(lv_dn_update) ).

    IF lv_connoteno IS INITIAL.
      RAISE connote_not_generated.
    ENDIF.

*--> Call the Method to Update the DN with Connote number
    update_delivery(
      EXPORTING
        iv_connoteno  = lv_connoteno
        iv_dn_update  = lv_dn_update
        is_vbkok      = is_vbkok
        it_hu_det     = it_hu_det
      IMPORTING
        ev_return    = DATA(lv_return) ).

    IF lv_return IS NOT INITIAL.
      RAISE dn_update_failed.
    ENDIF.

*--> Call the Method to Update HU with Artical details
    update_mul_hu(
      EXPORTING
        iv_connoteno = lv_connoteno
        is_vbkok     = is_vbkok
        it_hu_det    = it_hu_det
      IMPORTING
        ev_return    = lv_return ).

    IF lv_return IS NOT INITIAL.
      RAISE hu_update_failed.
    ENDIF.

    COMMIT WORK AND WAIT.

*--> Do Goods Issue
    goods_issue( EXPORTING is_vbkok  = is_vbkok
                 IMPORTING ev_return = lv_return ).

    IF lv_return IS NOT INITIAL.
      ROLLBACK WORK.
      RAISE goods_issue_failed.
    ENDIF.

*--> Destroy the reference
    clear_object( ).


  ENDMETHOD.


  METHOD generate_connote_number.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to Generate Connote Number
* Developer: I345486
*----------------------------------------------------------------------*

    DATA: lv_return    TYPE nrreturn ##NEEDED .

    CLEAR:ev_dn_update, ev_connoteno.

*--> In case if the Connote number is not available then create a new one
    IF is_dn_header-zzconnote_num_range IS NOT INITIAL
      AND gs_connoteno-connoteno IS INITIAL.

*--> Generate a Connote number
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = is_dn_header-zzconnote_num_range
        IMPORTING
          number                  = ev_connoteno
          returncode              = lv_return
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc = 0 AND is_dn_header-zzconnote_indc = gc_n.     "Non Consolidation Delivery
        DATA(lv_status) = space.
      ELSEIF sy-subrc = 0 AND is_dn_header-zzconnote_indc = gc_y. "Consolidation Delivery
        lv_status = gc_o."Open
      ENDIF.


*--> update Z table ZSDD_CONNOTENO with DN and Connote number
      DATA(ls_connoteno) = VALUE zsdd_connoteno( shippingpoint = is_dn_header-vstel                " Shipping Point
                                                 servicecode   = is_dn_header-zzcarr_service       " Service Code
                                                 shipto        = is_dn_header-kunnr                " Ship to
                                                 accountno     = is_dn_header-zzcarr_acc_no        " Account no
                                                 deliveryno    = is_dn_header-vbeln                " Delivery
                                                 object        = is_dn_header-zzconnote_num_range  " Number range onject
                                                 connoteno     = ev_connoteno                      " Connote number
                                                 connotedate   = sy-datum                          " Connote date
                                                 connotestatus = lv_status ).                      " Connote status
      IF gs_connoteno IS INITIAL.
        gs_connoteno = ls_connoteno.
      ENDIF.
    ELSEIF gs_connoteno-connoteno IS NOT INITIAL.
      ev_connoteno = gs_connoteno-connoteno.
*--> In case of consolidated Delivery case, Use the existing Connote number which is
*    already avalibale for the same date and same ship to and update the DN details
      READ TABLE gt_connoteno TRANSPORTING NO FIELDS WITH KEY deliveryno = is_dn_header-vbeln.
      IF sy-subrc <> 0 AND is_dn_header-zzconnote_indc = gc_y.
*--> update Z table ZSDD_CONNOTENO with DN and Connote number
        ls_connoteno = VALUE zsdd_connoteno( shippingpoint = is_dn_header-vstel                " Shipping Point
                                             servicecode   = is_dn_header-zzcarr_service       " Service Code
                                             shipto        = is_dn_header-kunnr                " Ship to
                                             accountno     = is_dn_header-zzcarr_acc_no        " Account no
                                             deliveryno    = is_dn_header-vbeln                " Delivery
                                             object        = gs_connoteno-object               " Number range onject
                                             connoteno     = gs_connoteno-connoteno            " Connote number
                                             connotedate   = sy-datum                          " Connote date
                                             connotestatus = gc_o ).                           " Connote status
      ENDIF.
    ENDIF.


    IF ls_connoteno IS NOT INITIAL.
*--> Fill the below exporting field to update the DN with Connote number
      ev_dn_update = abap_true.

*--> Update Connote number details in ZSDD_CONNOTENO
      CALL FUNCTION 'Z_O2C_UPDATE_CONNOTENO'
        EXPORTING
          is_connoteno = ls_connoteno.
      IF sy-subrc <> 0 ##FM_SUBRC_OK.
        CLEAR ev_connoteno.
      ENDIF.

    ENDIF.

    IF is_dn_header-lifex IS INITIAL.
      ev_dn_update = abap_true.
    ENDIF.
    CLEAR:ls_connoteno,lv_return.

*--> Update the global variable LIFEX
    gv_lifex = is_dn_header-zzconnote_prefix && ev_connoteno.
    CONDENSE gv_lifex NO-GAPS.
  ENDMETHOD.


  METHOD get_reference.
*----------------------------------------------------------------------*
* DS_E275: Connote Number Generation
* DESCRIPTION:
* Method will export the Class reference
* Developer: I345486
*----------------------------------------------------------------------*
*--> Export the Reference of the Class
    IF gv_ref IS NOT BOUND.
      gv_ref = NEW zcl_wm_connote_generation( iv_delivery = iv_delivery
                                              iv_service  = iv_service ).
    ENDIF.

    rv_ref = gv_ref.
  ENDMETHOD.


  METHOD goods_issue.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to do Goods Issue for DN
* Developer: I345486
*----------------------------------------------------------------------*

    DATA: lt_prot  TYPE tab_prott.

    CLEAR ev_return.

*--> Do the goods issue
    IF is_vbkok IS NOT INITIAL.
      CALL FUNCTION 'WS_DELIVERY_UPDATE'
        EXPORTING
          vbkok_wa = is_vbkok
          synchron = abap_true
          commit   = abap_true
          delivery = is_vbkok-vbeln
        TABLES
          prot     = lt_prot.

      LOOP AT lt_prot INTO DATA(ls_prot) WHERE msgty CA 'AEX'.
        MESSAGE ID ls_prot-msgid TYPE ls_prot-msgty NUMBER ls_prot-msgno
                WITH ls_prot-msgv1  ls_prot-msgv2 ls_prot-msgv3 ls_prot-msgv4
                INTO DATA(lv_dummy).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        ev_return = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD update_delivery.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to update Delivery with Connote number
* Developer: I345486
*----------------------------------------------------------------------*
* IV_ACTION = 'P', then method called from Shipment label print program
* hence we are not updating Delivery with Connote number in LIFEX field,
* insted it will be filled in global variable GV_LIFEX which will be used
* to update Delivery after PGI DS_E232
*----------------------------------------------------------------------*


    DATA: ls_header_data TYPE vbkok,
          ls_text_header TYPE thead,
          ls_new_header  TYPE thead ##NEEDED,
          lv_function    TYPE c     ##NEEDED,
          lt_port        TYPE tab_prott,
          lt_tline       TYPE STANDARD TABLE OF tline.

    CLEAR ev_return.

*    Call the BAPI - not for the PRINT button execution - 232 Development
    IF iv_action IS INITIAL.
*--> Update the  Ext.Delivery field with Connote number in DN header
      IF is_vbkok IS INITIAL.
        ls_header_data-vbeln    = gs_dn_header-vbeln.
        ls_header_data-vbeln_vl = gs_dn_header-vbeln.
      ELSE.
        ls_header_data = is_vbkok.
      ENDIF.
      IF iv_dn_update = abap_true.
        ls_header_data-lifex    = gs_dn_header-zzconnote_prefix && iv_connoteno.
        CONDENSE ls_header_data-lifex NO-GAPS.
      ENDIF.
      CLEAR ls_header_data-wabuc.
*--> BAPI_OUTB_DELIVERY_CHANGE does not contain LIFEX field in header structure and also Bapi it is not released,
*    No other Bapi is not available to change LIFEX field in DN header. Also LIFEX field doest have any relationship with
*    other fields in other table so using the below FM to update the data.

      CALL FUNCTION 'WS_DELIVERY_UPDATE'
        EXPORTING
          vbkok_wa          = ls_header_data
          delivery          = ls_header_data-vbeln
          synchron          = abap_true
        TABLES
          prot              = lt_port
          it_handling_units = it_hu_det.
*--> Chekc the errors
      LOOP AT lt_port TRANSPORTING NO FIELDS WHERE msgty CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        ev_return = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF iv_dn_update = abap_true.
*--> Fill the tracking link details
      APPEND VALUE tline( tdline = gv_url && gs_dn_header-zzconnote_prefix && iv_connoteno ) TO lt_tline.
      ls_text_header-tdobject = gc_vbbk.            "VBBK
      ls_text_header-tdname   = gs_dn_header-vbeln. "Delivery number
      ls_text_header-tdid     = gc_0102.            "0102 - Transport information
      ls_text_header-tdspras  = sy-langu.

*--> Update the details to Delivery header text
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header    = ls_text_header
        IMPORTING
          function  = lv_function
          newheader = ls_new_header
        TABLES
          lines     = lt_tline
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          object    = 4
          OTHERS    = 5.
      IF sy-subrc = 0.
        CALL FUNCTION 'COMMIT_TEXT'.
        CLEAR:ls_text_header,lt_tline.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD update_hu.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to update HU with Artical number
* Developer: I345486
*----------------------------------------------------------------------*

    DATA: lt_venum      TYPE hum_venum_t,
          lt_messages   TYPE huitem_messages_t ##NEEDED,
          ls_object     TYPE hum_object,
          lt_new_values TYPE hum_update_header_t.

    CONSTANTS: lc_pic    TYPE char3 VALUE 'PIC',
               lc_object TYPE char10 VALUE 'ZINDTOLL'.

    CLEAR ev_return.

*--> Get the Internal Handling Unit Number
    SELECT venum,exidv2,vpobj,vpobjkey
      INTO @DATA(ls_hu_head)
      FROM vekp UP TO 1 ROWS
      WHERE exidv = @iv_hu_ext.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR ls_hu_head.
    ENDIF.
    IF iv_hu_int IS INITIAL.
      DATA(lv_venum) = ls_hu_head-venum.
    ELSE.
      lv_venum = iv_hu_int.
    ENDIF.

*--> Delete Current DN as data is already selected in CONSTRUCTOR
    DELETE gt_connoteno WHERE deliveryno = gs_dn_header-vbeln.

*--> In case of Consolidation case get Number of packages from Delivery header
    IF gt_connoteno[] IS NOT INITIAL
      AND gs_dn_header-zzconnote_indc = gc_y.
      SELECT vbeln,
             anzpk
        FROM likp
        INTO TABLE @DATA(lt_likp)
        FOR ALL ENTRIES IN @gt_connoteno
        WHERE vbeln = @gt_connoteno-deliveryno.
      IF sy-subrc <> 0.
        CLEAR lt_likp.
      ENDIF.
    ENDIF.

***************************************************************************
* BAPI_HU_CHANGE_HEADER will allow Packing Object (VPOBJ) of type ‘05’,06’ and ’12’ only.
* But in our case Packing Object will be ‘01’ ie; Outbound delivery.
* No other released FM/BAPI found to update HU header details,So using below FM's
* which are approved to use
***************************************************************************
    APPEND VALUE hum_venum( venum = lv_venum ) TO lt_venum.
*--> Call the FM to fill HU details in Buffer table which will be used in FM 'HU_HEADER_UPDATE'
    CALL FUNCTION 'V51P_FILL_GT'
      EXPORTING
        it_venum    = lt_venum
      EXCEPTIONS
        hu_locked   = 1
        no_hu_found = 2
        fatal_error = 3
        OTHERS      = 4.
    IF sy-subrc = 0.
      IF iv_hu_item IS INITIAL.
*--> find the number of Pacakges in DN
        IF gs_dn_header-zzconnote_indc = gc_n. " Non Consolidation case; only 1 DN
          DATA(lv_no_of_pack) = gs_dn_header-anzpk.
        ELSE."Consolidation case; Multiple DN's
          lv_no_of_pack = REDUCE i( INIT x = 0 FOR ls_likp IN lt_likp
                                               NEXT x = x + ls_likp-anzpk ) .
          lv_no_of_pack = lv_no_of_pack + gs_dn_header-anzpk."Add current DN Packages
        ENDIF.
      ELSE.
        lv_no_of_pack = iv_hu_item.
      ENDIF.
*--> In case Carrier Service is empty use PICK
      IF gs_dn_header-zzcarr_service IS INITIAL.
        gs_dn_header-zzcarr_service = lc_pic.
      ENDIF.

*--> Assign Article ID code to Contents field in HU header
      IF gs_dn_header-zzconnote_num_range = lc_object.
        DATA(lv_value) = gs_dn_header-zzconnote_prefix && iv_connoteno && lv_no_of_pack+2(3).
      ELSE.
        lv_value = gs_dn_header-zzconnote_prefix && iv_connoteno && gs_dn_header-zzcarr_service && lv_no_of_pack.
      ENDIF.
      CONDENSE lv_value NO-GAPS.

*--> Fill the HU field details to update
      APPEND VALUE hum_update_header( hdl_unit_itid = lv_venum
                                      hdl_unit_exid = iv_hu_ext
                                      field_name    = CONV #( 'INHALT' )
                                      field_value   = lv_value
                                      ) TO lt_new_values.

      IF ls_hu_head-exidv2 IS INITIAL.
*--> Pass Connote number in EXIDV2 if it is empty
        APPEND VALUE hum_update_header( hdl_unit_itid = lv_venum
                                        hdl_unit_exid = iv_hu_ext
                                        field_name    = CONV #( 'EXIDV2' )
                                        field_value   = lv_value
                                        ) TO lt_new_values.
      ENDIF.

*--> Call the FM to Update HU deader details
      CALL FUNCTION 'HU_HEADER_UPDATE'
        EXPORTING
          it_new_values = lt_new_values
        IMPORTING
          et_messages   = lt_messages
        EXCEPTIONS
          not_possible  = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        ev_return = abap_true.
      ELSE.
        LOOP AT lt_messages TRANSPORTING NO FIELDS WHERE msgty CA 'AEX'.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          ls_object-object = ls_hu_head-vpobj.
          ls_object-objkey = ls_hu_head-vpobjkey.

          CALL FUNCTION 'HU_POST'
            EXPORTING
              if_synchron    = ' '
              if_no_messages = ' '
              is_object      = ls_object.
        ELSE.
          ev_return = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      ev_return = abap_true.
    ENDIF.
    CLEAR: lt_messages[],lt_new_values[],lv_venum,
           lt_likp[],lt_venum[],lv_value,ls_hu_head.
  ENDMETHOD.


   METHOD update_mul_hu.
*----------------------------------------------------------------------*
* DS_E275: Connote Number generation
* DESCRIPTION:
* Method to update HU with Artical number
* Developer: I345486
*----------------------------------------------------------------------*
* From RF program for Star track multiple HU's can be created at a time,
* in that case LT_HU_DET will be filled with all new HU details nad Flag
* IV_ASSIGN_HU_DN will be set from RF program. loop through all HUs and
* update connote number in it, assignment of HU to DN will be done in
* UPDATE_DELIVERY method if LT_HU_DET is filled.
*----------------------------------------------------------------------*


     DATA: lt_venum       TYPE hum_venum_t,
           lt_messages    TYPE huitem_messages_t ##NEEDED,
           ls_object      TYPE hum_object,
           lv_no_of_pack  TYPE anzpk,
           lt_port        TYPE tab_prott,
           lt_new_values  TYPE hum_update_header_t,
           lv_t_hu_weight TYPE i,
           lv_hu_weight   TYPE i.

     CONSTANTS: lc_pic    TYPE char3  VALUE 'PIC',
                lc_object TYPE char10 VALUE 'ZINDTOLL'.

     CLEAR ev_return.

*--> Delete Current DN as data is already selected in CONSTRUCTOR
     DELETE gt_connoteno WHERE deliveryno = gs_dn_header-vbeln.

*--> In case of Consolidation case get Number of packages from Delivery header
     IF gt_connoteno[] IS NOT INITIAL
       AND gs_dn_header-zzconnote_indc = gc_y.
       SELECT vbeln,
              anzpk
         FROM likp
         INTO TABLE @DATA(lt_likp)
         FOR ALL ENTRIES IN @gt_connoteno
         WHERE vbeln = @gt_connoteno-deliveryno.
       IF sy-subrc <> 0.
         CLEAR lt_likp.
       ENDIF.
     ENDIF.

*--> Get total no of HU count
     DATA(lv_hu_count) = lines( it_hu_det ).
***************************************************************************
* BAPI_HU_CHANGE_HEADER will allow Packing Object (VPOBJ) of type ‘05’,06’ and ’12’ only.
* But in our case Packing Object will be ‘01’ ie; Outbound delivery.
* No other released FM/BAPI found to update HU header details,So using below FM's
* which are approved to use
***************************************************************************
     LOOP AT it_hu_det INTO DATA(ls_hu_head).
       DATA(lv_tabix) = sy-tabix.
       APPEND VALUE hum_venum( venum = ls_hu_head-top_hu_internal ) TO lt_venum.
*--> Call the FM to fill HU details in Buffer table which will be used in FM 'HU_HEADER_UPDATE'
       CALL FUNCTION 'V51P_FILL_GT'
         EXPORTING
           it_venum    = lt_venum
         EXCEPTIONS
           hu_locked   = 1
           no_hu_found = 2
           fatal_error = 3
           OTHERS      = 4.
       IF sy-subrc = 0.

*--> find the number of Pacakges in DN
         IF gs_dn_header-zzconnote_indc = gc_n. " Non Consolidation case; only 1 DN
           lv_no_of_pack = lv_tabix.
         ELSE."Consolidation case; Multiple DN's
           lv_no_of_pack = REDUCE i( INIT x = 0 FOR ls_likp IN lt_likp
                                                NEXT x = x + ls_likp-anzpk ) .

           lv_no_of_pack = lv_no_of_pack + lv_tabix.
         ENDIF.

*--> In case Carrier Service is empty use PICK
         IF gs_dn_header-zzcarr_service IS INITIAL.
           gs_dn_header-zzcarr_service = lc_pic.
         ENDIF.

*--> Assign Article ID code to Contents field in HU header
         IF gs_dn_header-zzconnote_num_range = lc_object.
           DATA(lv_value) = gs_dn_header-zzconnote_prefix && iv_connoteno && lv_no_of_pack+2(3).
         ELSE.
           lv_value = gs_dn_header-zzconnote_prefix && iv_connoteno && gs_dn_header-zzcarr_service && lv_no_of_pack.
         ENDIF.
         CONDENSE lv_value NO-GAPS.

*--> Calculate HU weight based on DN weight
         CLEAR lv_hu_weight.
         IF lv_hu_count = lv_tabix.
           lv_hu_weight = is_vbkok-brgew - lv_t_hu_weight.
         ELSE.
           lv_hu_weight = is_vbkok-brgew / lv_hu_count.
           lv_t_hu_weight = lv_t_hu_weight + lv_hu_weight.
         ENDIF.

         IF lv_hu_weight = 0.
           lv_hu_weight = 1.
         ENDIF.
*--> Fill the HU field details to update
         APPEND VALUE hum_update_header( hdl_unit_itid = ls_hu_head-top_hu_internal
                                         hdl_unit_exid = ls_hu_head-top_hu_external
                                         field_name    = CONV #( 'INHALT' )
                                         field_value   = lv_value
                                         ) TO lt_new_values.

*--> Pass Connote number in EXIDV2 if it is empty
         APPEND VALUE hum_update_header( hdl_unit_itid = ls_hu_head-top_hu_internal
                                         hdl_unit_exid = ls_hu_head-top_hu_external
                                         field_name    = CONV #( 'EXIDV2' )
                                         field_value   = lv_value
                                         ) TO lt_new_values.
*--> Update Total Weight in HU
         APPEND VALUE hum_update_header( hdl_unit_itid = ls_hu_head-top_hu_internal
                                         hdl_unit_exid = ls_hu_head-top_hu_external
                                         field_name    = CONV #( 'BRGEW' )
                                         field_value   = lv_hu_weight
                                         ) TO lt_new_values.

         APPEND VALUE hum_update_header( hdl_unit_itid = ls_hu_head-top_hu_internal
                                         hdl_unit_exid = ls_hu_head-top_hu_external
                                         field_name    = CONV #( 'TARAG' )
                                         field_value   = lv_hu_weight
                                         ) TO lt_new_values.

*--> Call the FM to Update HU deader details
         CALL FUNCTION 'HU_HEADER_UPDATE'
           EXPORTING
             it_new_values = lt_new_values
           IMPORTING
             et_messages   = lt_messages
           EXCEPTIONS
             not_possible  = 1
             OTHERS        = 2.
         IF sy-subrc <> 0.
           ev_return = abap_true.
         ELSE.
           LOOP AT lt_messages TRANSPORTING NO FIELDS WHERE msgty CA 'AEX'.
             EXIT.
           ENDLOOP.
           IF sy-subrc <> 0.
             ls_object-object = '01'."ls_hu_head-vpobj.
             ls_object-objkey = ls_hu_head-rfbel.

             CALL FUNCTION 'HU_POST'
               EXPORTING
                 if_synchron    = ' '
                 if_no_messages = ' '
                 is_object      = ls_object.
           ELSE.
             ev_return = abap_true.
           ENDIF.
         ENDIF.
       ELSE.
         ev_return = abap_true.
       ENDIF.
       CLEAR: lt_messages[],lt_new_values[],
              lt_venum[],lv_value,ls_hu_head.
     ENDLOOP.

   ENDMETHOD.
ENDCLASS.
