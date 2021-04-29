class ZCL_H1S_GENERIC_UTILS definition
  public
  final
  create public .

public section.

  types:
    ty_range_tab type range of werks_d .
  types:
    BEGIN OF ty_dat,
        jjjj(4) ,
        mm(2) ,
        tt(2) ,
      END OF ty_dat .
  types:
    BEGIN OF ty_hdat,
        jjjj(4) ,
        mm(2) ,
        tt(2) ,
      END OF ty_hdat .
  types:
    BEGIN OF ty_partners,
        sign(1)   TYPE c,
        option(2) TYPE c,
        low       TYPE bu_partner,
        high      TYPE bu_partner,
      END OF ty_partners .
  types:
    ty_t_partners TYPE TABLE OF ty_partners .
  types:
    BEGIN OF ty_whid,
        whid TYPE zmm_whid,
      END OF ty_whid .
  types:
    BEGIN OF ty_whid_range,
        sign(1)   TYPE c,
        option(2) TYPE c,
        low       TYPE zmm_whid,
        high      TYPE zmm_whid,
      END OF ty_whid_range .
  types:
    ty_t_whid TYPE TABLE OF ty_whid .
  types:
    ty_t_plnt_sloc TYPE table of t_werks .
  types:
    ty_t_char TYPE TABLE OF zvca_char_cds .
  types:
    ty_t_klart TYPE RANGE OF klassenart .
  types:
    ty_t_class TYPE RANGE OF klasse_d .
  types:
    ty_t_objek TYPE RANGE OF cuobn .
  types:
    ty_t_atinn TYPE RANGE OF atinn .
  types:
    ty_t_atzhl TYPE RANGE OF wzaehl .
  types:
    ty_t_mafid TYPE RANGE OF klmaf .
  types:
    ty_t_atwrt TYPE RANGE OF atwrt .
  types:
    ty_t_datuv TYPE RANGE OF datuv .
  types:
    ty_t_lkenz TYPE RANGE OF lkenz .
  types:
    ty_t_datub TYPE RANGE OF datub .

  constants GC_XLS type STRING value '.XLS' ##NO_TEXT.
  constants GC_DEFDIR type STRING value 'C:\' ##NO_TEXT.
  constants GC_X type BOOLEAN value 'X' ##NO_TEXT.
  constants GC_E type CHAR1 value 'E' ##NO_TEXT.
  constants GC_OBJECT type TDOBJECTGR value 'GRAPHICS' ##NO_TEXT.
  constants GC_ID type TDIDGR value 'BMAP' ##NO_TEXT.
  constants GC_BTYPE type TDBTYPE value 'BCOL' ##NO_TEXT.
  constants GC_BPCAT type GPA1T_FPM value '11' ##NO_TEXT.
  constants GC_REMIT_FONAME type FPNAME value 'ZAF_FI_REMIT_ADV' ##NO_TEXT.
  constants GC_AR_INV type FPNAME value 'ZAF_FI_AR_INV' ##NO_TEXT.
  constants GC_ACN type PARTY value 'SAPI01' ##NO_TEXT.
  constants GC_MAXROWS type I value 1000 ##NO_TEXT.
  constants GC_MAXCOLS type I value 70 ##NO_TEXT.
  constants GC_RANGE_SIGN type CHAR1 value 'I' ##NO_TEXT.
  constants GC_RANGE_OPTION type CHAR2 value 'EQ' ##NO_TEXT.
  constants GC_ZPPRDMNTH type CHAR9 value 'ZPPRDM' ##NO_TEXT.
  constants GC_WERKS type CHAR5 value 'WERK' ##NO_TEXT.
  constants GC_KUNNR type CHAR5 value 'KUNNR' ##NO_TEXT.
  constants GC_ZRELMNTH type CHAR8 value 'ZRELMN' ##NO_TEXT.
  constants GC_GENLOGID type CHAR20 value 'H1S_APPLOG' ##NO_TEXT.
  constants GC_WAIT type BAPIWAIT value 'X' ##NO_TEXT.
  constants GC_MSGID type SYMSGID value 'ZTECH01' ##NO_TEXT.
  constants GC_PRO_HTTPS type STRING value 'HTTPS' ##NO_TEXT.
  constants GC_SUCCESS type SY-MSGTY value 'S' ##NO_TEXT.
  constants GC_ERROR_MSGTY type SY-MSGTY value 'E' ##NO_TEXT.
  constants GC_APPLICATION_ID type IF_FDT_TYPES=>ID value '00163E1D3A421ED7BDE4015FD406EFBC' ##NO_TEXT.

  methods GET_CURRENCY
    importing
      !VENDOR type LIFNR
      !EKORG type EKORG
    exporting
      !CURR type KONWA .
  methods CONSTRUCTOR .
  methods IMPORT_EXCEL_TO_INTERNAL_TABLE
    importing
      value(IV_FILENAME) type CHAR256 optional
      !IV_ROWS type I optional
      !IV_COLUMNS type I optional
    exporting
      !ET_RESULT type ANY TABLE .
  methods FILE_OPEN_DIALOG
    importing
      value(IV_FILEXT) type STRING
    exporting
      value(EV_FILENAME) type CHAR256 .
  methods GET_PRINT_LOGOS
    exporting
      !EV_XSTRING type XSTRING
    changing
      !XS_LOGO type ZCAC_LOGOS
    exceptions
      TOO_FEW_PARAMETERS
      LOGOS_NOT_FOUND
      ERROR_GRAPHIC_TOBMP .
  methods GET_CONTACT_ACC_DETAILS
    exporting
      !EV_ACCOUNT_NAME type FCLM_BAM_DESCRIPTION
      !EV_BANK_NAME type FIBL_TXT50
      !EV_BSB type BANKL
      !EV_BANKN type BANKN
    changing
      !XS_CONTACT type ZFIC_CONTACTDET
    exceptions
      TOO_FEW_PARAMETERS
      CONTACT_DET_NOT_FOUND
      BSB_NOT_FOUND
      ACCOUNT_NOT_FOUND .
  methods GET_VENDOR_CODE
    importing
      !IV_LIFNR type LIFNR optional
      !IV_VMS type BOOLEAN optional
    changing
      !XV_LAND1 type LAND1_GP optional
    returning
      value(RV_ORDER_CODE) type CHAR3
    exceptions
      ORDER_CODE_NOT_FOUND
      COUNTRY_KEY_NOT_FOUND
      TOO_FEW_PARAMETER .
  methods GET_COMP_ADDRESS
    importing
      !IV_BUKRS type BUKRS
    exporting
      !ES_COMPCODE_DET type BAPI0002_2
      !ES_COMPCODE_ADD type BAPI0002_3
      !ES_RETURN type BAPIRETURN
      !EV_ABN type STCEG
      !EV_ACN type PAVAL
      !ES_FORMATTED_ADD type RFKORD_S_ADDRESS .
  methods SET_BAL_LOG
    importing
      !IV_EXTID type BALNREXT
      !IV_OBJECT type BALOBJ_D default 'ZH1SDEV'
      !IV_SUB_OBJECT type BALSUBOBJ default 'ZH1SDEV_GEN'
      !IV_LOG_HANDLE type BALLOGHNDL optional
      !IT_MSG_TAB type BAPIRET2_T
      !IV_COMMIT_FLAG type CHAR1 default 'X'
    exporting
      !ET_RETURN type BAPIRET2_T
    exceptions
      CX_ALERT .
  methods SEND_ATTACHMENT_IN_EMAIL
    importing
      value(IV_SUBJECT) type SO_OBJ_DES optional
      value(IV_ATTACHMENT_SUBJECT) type SOOD-OBJDES optional
      value(IV_ATTACHMENT_TYPE) type SOODK-OBJTP optional
      value(IV_ATTACHMENT_CONTENT_HEX) type SOLIX_TAB optional
      value(IV_BODY) type SOLI_TAB optional
      value(IV_RECEPIENT) type ADR6-SMTP_ADDR
    raising
      CX_ADDRESS_BCS
      CX_DOCUMENT_BCS
      CX_SEND_REQ_BCS .
  methods GET_AUH
    exporting
      !ET_AUH_RANGE type RSIS_T_RANGE .
  methods GET_MPE
    exporting
      !ET_MPE_RANGE type RSIS_T_RANGE .
  methods GET_JULIAN_DATE
    importing
      !IV_DATE type DATS
    exporting
      !EV_JULIAN_DATE type NUMC5 .
  methods GET_PARTNER_DETAILS
    importing
      !IT_PARTNERS type TY_T_PARTNERS
      !IV_SSFID type AD_SSFID2 optional
    exporting
      !ET_PARTNER_DETAILS type ZTECC_PARTNERS
    raising
      ZCX_PARTNERS_NOT_FOUND .
  methods GET_GOBACK_MONTH
    importing
      !IV_CURRDATE like SY-DATUM
      !IV_BACKMONTH type NUMC3
    exporting
      !EV_NEWDATE like SY-DATUM .
  methods GET_OBJ_CHAR
    importing
      !IT_KLART type TY_T_KLART optional
      !IT_CLASS type TY_T_CLASS optional
      !IT_OBJEK type TY_T_OBJEK optional
      !IT_ATINN type TY_T_ATINN optional
      !IT_ATZHL type TY_T_ATZHL optional
      !IT_MAFID type TY_T_MAFID optional
      !IT_ATWRT type TY_T_ATWRT optional
      !IT_DATUV type TY_T_DATUV optional
      !IT_LKENZ type TY_T_LKENZ optional
      !IT_DATUB type TY_T_DATUB optional
    exporting
      !ET_OBJK_CHAR type TY_T_CHAR
    exceptions
      NO_INPUT .
  methods GET_PLANT_ADD_DETAILS
    importing
      value(IT_WERKS) type T_WERKS
    exporting
      value(ET_PLANT_DETAILS) type ZTMM_PLANT_DETAILS
    exceptions
      PLANT_DOES_NOT_EXIST
      ADDRESS_DOES_NOT_EXIST .
  methods GET_SERVICE_URL
    importing
      !IREF_SERVER type ref to IF_HTTP_SERVER optional
      !IV_ALWAYS_HTTPS type ABAP_BOOL default ABAP_FALSE
      !IV_PATH type STRING optional
      !IT_PARAMETERS type TIHTTPNVP optional
    exporting
      value(ES_RETURN) type BAPIRET2
      value(EV_URL) type STRING .
  methods SEND_EMAIL
    importing
      !IV_EMAIL_TO type AD_SMTPADR
      !IV_SUBJECT type SO_OBJ_DES optional
      !IV_SENSITIVITY type SO_OBJ_SNS optional
      !IV_IMPORTANCE type BCS_DOCIMP optional
      !IT_EMAIL_BODY type SOLI_TAB optional
      !IV_TYPE type SO_OBJ_TP optional
      !IT_CONTENT_HEX type SOLIX_TAB optional
      !IV_LANGUAGE type SO_OBJ_LA optional
      !IV_LENGTH type SO_OBJ_LEN optional
      !IV_ATTTACHMENT_TYPE type SOODK-OBJTP optional
      !IT_ATTACHMENT_CONTENT type SOLI_TAB optional
      !IV_ATTACHMENT_SUBJECT type SOOD-OBJDES optional
      !IT_ATTACHMENT_CONTENT_HEX type SOLIX_TAB optional
      !IV_ATTACHMENT_FLAG type FLAG optional
      !IV_COMMIT type CHAR1 default 'X'
    exporting
      !ET_RETURN type BAPIRET2_TAB .
  methods GET_SYS_HOSTID
    importing
      !IREF_SERVER type ref to IF_HTTP_SERVER optional
      !IV_ALWAYS_HTTPS type ABAP_BOOL default ABAP_FALSE
      !IV_HOST type ZCA_HOST optional
      !IV_PATH type STRING optional
      !IT_PARAMETERS type TIHTTPNVP optional
    exporting
      value(ES_RETURN) type BAPIRET2
      value(EV_URL) type STRING .
  methods GET_HASH_VAL
    importing
      !IV_PARAM type STRING
      !IV_HASH_ALGO type STRING default 'SHA256'
    exporting
      value(EV_HASHED_CONTENT) type STRING
    raising
      CX_ABAP_X509_CERTIFICATE .
  methods GET_PLNT_STLOC_FRM_WHID
    importing
      !IT_WHID type TY_T_WHID
    exporting
      !ET_PLNT_STLOC type TY_T_PLNT_SLOC .
  class-methods GET_COMPANY_CODE_WTY_OBJ
    importing
      value(IV_OBJNR) type J_OBJNR
    returning
      value(RV_BUKRS) type WERKS_D .
  methods GET_BP_STD_ADRS
    importing
      !IT_PARTNERS type TY_T_PARTNERS
      !IV_SSFID type AD_SSFID2 optional
    exporting
      !ET_PARTNER_DETAILS type ZTECC_PARTNERS
    raising
      ZCX_PARTNERS_NOT_FOUND .
  class-methods CHECK_DELIVERY_LOCK_MANIFEST
    importing
      !IV_VBELN type VBELN_VL
      !IV_CHECK_TWICE type BOOLEAN optional .
  class-methods SAVE_APPL_LOG
    importing
      !IV_EXTID type BALNREXT
      !IV_OBJECT type BALOBJ_D
      !IV_SUB_OBJECT type BALSUBOBJ
      !IV_LOG_HANDLE type BALLOGHNDL optional
      !IT_MSG_TAB type BAPIRET2_T
      !IV_COMMIT_FLAG type CHAR1 default 'X'
    exporting
      !ET_RETURN type BAPIRET2_T .
  class-methods GET_MANIFEST_FORMATTING
    returning
      value(RT_FORMAT) type ZTWM_CBU_FORMATTING .
  class-methods GET_CONTAINER_ORDER_PLANTS
    exporting
      !ET_PLANTS type TY_RANGE_TAB .
  class-methods GET_SHIPMENT_CLUBBING_PLANTS
    exporting
      !ET_PLANTS type TY_RANGE_TAB .
  class-methods GET_LEADTIME_DISCOUNT_PARAMS
    exporting
      !EV_LEAD_DAYS type I
      !EV_MAX_SO_QTY type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_H1S_GENERIC_UTILS IMPLEMENTATION.


method check_delivery_lock_manifest.

*- Latest Change - Collision simulation check was not working, hence executing with mode 'E' and if successfull dequeuing it immediately

********************************************************************************************************************************************
* Below we check if all the locks of the deliveries are released before we proceed to shipment process
* Since unreleased function modules are used to create deliveries, the lock mechanism happening is unsure
* There are some scope 2 locks still active which are not yet released, when the control reaches the shipment process, the lock still exists.

* So we loop on the deliveries created and check for the locks and sleep for 5 seconds per deliveries hoping that the lock will be released.
* This should solve the problem 99%. If the locks are not yet released after the below code, the next operration will result in error
********************************************************************************************************************************************

  "In brief, the process sleeps for 5 seconds for a lock to get released

  "We are trying a 10 seconds lock simulaiton here ( if IV_CHECK_TWICE = 'X' )

  call function 'ENQUEUE_EVVBLKE'
    exporting
*     mode_likp      = 'V'       " No actual lock, only simulating collision check
      vbeln          = iv_vbeln
      _wait          = abap_true " Wait/Sleep for 5 seconds as per system default
    exceptions
      foreign_lock   = 1
      system_failure = 2
      others         = 3.

  if sy-subrc <> 0.
    if iv_check_twice = abap_true.
      zcl_h1s_generic_utils=>check_delivery_lock_manifest( iv_vbeln = iv_vbeln
                                                           iv_check_twice = abap_false ).
    endif.
  else.
    call function 'DEQUEUE_EVVBLKE'
      exporting
        vbeln = iv_vbeln.
  endif.

endmethod.


method CONSTRUCTOR.

  endmethod.


METHOD file_open_dialog.

    DATA: lv_filter TYPE string,
          lt_files  TYPE filetable,
          lv_rcode  TYPE i,
          lv_action TYPE i.

    CONCATENATE  '*.' iv_filext INTO lv_filter.

* Call dialog to navigate to file
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        default_extension       = iv_filext  "'.xls'
        file_filter             = lv_filter  "'*.xls'
        initial_directory       = 'C:\'
      CHANGING
        file_table              = lt_files
        rc                      = lv_rcode
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.
      MESSAGE e208(00) WITH 'FILE_OPEN_DIALOG'.
    ENDIF.

* Only continue if User hasn't cancelled
    CHECK: lv_action = 0.

* Determine filename to open Excel document
    READ TABLE lt_files INDEX 1 INTO DATA(ls_files).
    IF sy-subrc = 0.
      CONCATENATE 'FILE://' ls_files-filename INTO ev_filename.
    ELSE.
      MESSAGE e208(00).
    ENDIF.  "sy-subrc = 0

  ENDMETHOD.


method GET_AUH.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to return list of AUH company codes
* AUTHOR: I320039
* DATE  : 8/12/2016
*----------------------------------------------------------------------*

  DATA(lt_auh_range) = VALUE rsis_t_range( ( SIGN = 'I' OPTION = 'EQ' LOW = '50AK' ) ).

  et_auh_range = lt_auh_range.

  endmethod.


METHOD get_bp_std_adrs.

    DATA: lv_timestamp TYPE bu_addr_valid_from,
          lv_name      TYPE char255.

    IF it_partners[] IS INITIAL AND iv_ssfid IS INITIAL.
      CLEAR et_partner_details.
      RETURN.
    ENDIF.


*** Get the timestamp for current date and time,
    GET TIME STAMP FIELD lv_timestamp.

    SELECT    a~partner,         "partner
              a~addrnumber,      "Address Number
              g~type,            "Type
              g~name_org1,        "First Name1
              g~name_org2,        "Last Name2
              g~name_org3,        "Last Name2
              g~name_org4,        "Last Name2
              g~name_last,       "Last Name2
              g~name_first,      "First Name1
              g~mc_name2,        "First Name1
              g~mc_name1,        "Last Name2
              b~house_num1,      "House No.
              b~street,          "Street
              b~str_suppl1,      "Street2
              b~str_suppl2,      "Street3
              b~city1,           "City
              b~post_code1,      "City postal code
              b~region,          "Region
              b~country,         "Country
              c~smtp_addr,       "Email
              d~tel_number,      "Telephone Number
              f~remark           "Other communication->SSFID->Notes
                      INTO TABLE @DATA(lt_partner_details)     "et_partner_details
                      FROM
                      but020 AS a
                      INNER JOIN but000 AS g
                      ON a~partner = g~partner
                      INNER JOIN adrc AS b
                      ON a~addrnumber = b~addrnumber
                      LEFT OUTER JOIN adr6 AS c
                      ON a~addrnumber = c~addrnumber
                      AND  c~flgdefault  = @abap_true
                      LEFT OUTER JOIN adr2 AS d
                      ON a~addrnumber = d~addrnumber
                      AND d~flgdefault  = @abap_true
                      LEFT OUTER JOIN adr11 AS e
                      ON  e~addrnumber = a~addrnumber
                      AND e~ssfid_srch = @iv_ssfid
                      LEFT OUTER JOIN adrt AS f
                      ON  f~addrnumber = a~addrnumber
                      AND f~consnumber = e~consnumber
                      AND f~langu      = @sy-langu
                      WHERE a~partner IN @it_partners
                      AND   a~addr_valid_from <= @lv_timestamp
                      AND   a~addr_valid_to >= @lv_timestamp.
    IF sy-subrc <> 0.
**      If no data found then rasie an exception.
      RAISE EXCEPTION TYPE zcx_partners_not_found.
    ELSE.
**   Sort the date based on partner number.
      LOOP AT lt_partner_details INTO DATA(ls_partner_details).
        APPEND INITIAL LINE TO et_partner_details ASSIGNING FIELD-SYMBOL(<ls_partner_details>).
        MOVE-CORRESPONDING ls_partner_details TO <ls_partner_details>.
        IF ls_partner_details-type = 1.
          <ls_partner_details>-name1 = ls_partner_details-name_last.
          <ls_partner_details>-name2 = ls_partner_details-name_first.
        ELSEIF ls_partner_details-type = 2.
          <ls_partner_details>-name2 = ls_partner_details-name_org1 .
          <ls_partner_details>-name1 = ls_partner_details-name_org2 .
        ENDIF.
      ENDLOOP.
      SORT et_partner_details ASCENDING BY partner.
**   MC_NAME1 and MC_NAME2 always comes in CAPITAL letter
*    For changing the case to CAMEL CASE the below logic is implemented
*** Below logic commented based JIRA bug H1S-9098
*     LOOP AT et_partner_details ASSIGNING FIELD-SYMBOL(<ls_partners>).
*        lv_name = <ls_partners>-name1 && '+' && <ls_partners>-name2.
*        CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
*          EXPORTING
*            input_string  = lv_name
*            separators    = '+'
*          IMPORTING
*            output_string = lv_name.
*        SPLIT lv_name AT '+'  INTO <ls_partners>-name1 <ls_partners>-name2.
*      ENDLOOP.
    ENDIF.
  ENDMETHOD.


METHOD get_company_code_wty_obj.

    DATA lv_vehicle TYPE vlcvehicle-vhcle.
    DATA lv_equnr TYPE equi-equnr.

    IF iv_objnr IS INITIAL.
      RETURN.
    ENDIF.

*-If the master warranty object number belongs to vehicle
    IF iv_objnr+0(2) = 'VH'.

      lv_vehicle = iv_objnr+2(*).

      SELECT werks FROM vlcvehicle UP TO 1 ROWS
        INTO @DATA(lv_werks)
        WHERE vhcle = @lv_vehicle.
      ENDSELECT.

*-If the master warranty object number belongs to Equipment
    ELSEIF iv_objnr+0(2) = 'IE'.

      lv_equnr = iv_objnr+2(*).

      SELECT SINGLE werk INTO @lv_werks
        FROM equi
        WHERE equnr = @lv_equnr.

    ENDIF.

    IF sy-subrc = 0.

      SELECT SINGLE bwkey INTO @DATA(lv_bwkey)
        FROM t001w WHERE werks = @lv_werks.

      IF sy-subrc = 0.

        SELECT SINGLE bukrs INTO @rv_bukrs
          FROM t001k WHERE bwkey = @lv_bwkey.

        IF sy-subrc <> 0.
          CLEAR rv_bukrs.
        ENDIF.

      ENDIF.

    ENDIF.




  ENDMETHOD.


METHOD get_comp_address.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to get the address of the company code
* AUTHOR: I064763
* DATE  : 05/12/2016
*----------------------------------------------------------------------*
    DATA: lv_index TYPE i VALUE 2.

*--> get the address for the company code
    CALL FUNCTION 'BAPI_COMPANYCODE_GETDETAIL'
      EXPORTING
        companycodeid       = iv_bukrs
      IMPORTING
        companycode_detail  = es_compcode_det
        companycode_address = es_compcode_add
        return              = es_return.

*--> Fetch the ABN for the company code
    SELECT SINGLE stceg
      FROM t001
      INTO @DATA(ls_stceg)
       WHERE bukrs = @iv_bukrs.
    IF sy-subrc <> 0.
      CLEAR ev_abn.
    ELSE.
      DO 16 TIMES.
        IF ev_abn IS INITIAL AND ls_stceg IS NOT INITIAL.
          ev_abn = ls_stceg(2).
        ELSE.
          IF ls_stceg+lv_index(3) IS INITIAL.
            EXIT.
          ELSE.
            ev_abn = ev_abn && ` ` && ls_stceg+lv_index(3).
            lv_index = lv_index + 3.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.

*--> Fetch the ACN for the company code
    SELECT SINGLE paval
      FROM t001z
      INTO @DATA(lv_paval)
      WHERE bukrs = @iv_bukrs
      AND   party = @gc_acn.
    IF sy-subrc <> 0.
      CLEAR ev_acn.
    ELSE.
      ev_acn = lv_paval.
    ENDIF.

*--> Format the address
    es_formatted_add-adrnr = es_compcode_add-addr_no.
    es_formatted_add-line1 = es_compcode_add-name && ` ` && es_compcode_add-name_2.
    es_formatted_add-line2 = 'ABN' && ` ` && ev_abn.
    es_formatted_add-line3 = 'ACN' && ` ` && ev_acn.
    es_formatted_add-line4 = es_compcode_add-street .
    es_formatted_add-line5 = es_compcode_add-city && `,` && es_compcode_add-postl_cod1.
    es_formatted_add-line6 = 'Ph:' && ` ` && es_compcode_add-tel1_numbr.
    es_formatted_add-line7 = 'Fax:' && ` ` && es_compcode_add-fax_number.
    es_formatted_add-line8 = es_compcode_add-adr_notes.


  ENDMETHOD.


method GET_CONTACT_ACC_DETAILS.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to get the Contact and the Account details
* AUTHOR: I045536
* DATE  : 02/11/2016
*----------------------------------------------------------------------*
* Check if initial
    IF xs_contact-bukrs IS INITIAL AND
       xs_contact-vkorg IS INITIAL AND
       xs_contact-fonam IS INITIAL.
      RAISE too_few_parameters.
    ENDIF.

* IF currency is empty, default it to AUD.
    IF xs_contact-currency IS INITIAL.
      xs_contact-currency = 'AUD'.
    ENDIF.


* Get Contact details
    SELECT SINGLE * FROM zfic_contactdet INTO xs_contact
      WHERE bukrs     = xs_contact-bukrs
        AND vkorg     = xs_contact-vkorg
        AND vtweg     = xs_contact-vtweg
        AND spart     = xs_contact-spart
        AND fonam     = xs_contact-fonam
        AND currency  = xs_contact-currency
        AND spras     = 'E' .  "sy-langu. HW2-247
    IF sy-subrc <> 0.
      RAISE contact_det_not_found.
    ENDIF.

* Get the BSB (BANKL) from table T012
    SELECT SINGLE a~bankl b~TEXT1
             FROM t012 AS a INNER JOIN t012t AS b
             ON   a~bukrs = b~bukrs
             INTO (ev_bsb, ev_bank_name)
             WHERE a~bukrs = xs_contact-bukrs
             AND   a~hbkid = xs_contact-hbkid
             AND   b~spras = sy-langu.
    IF sy-subrc <> 0.
      RAISE bsb_not_found.
    ENDIF.

* Get the Account No from FCLM_BAM_ACLINK2
    SELECT a~bankn b~description                                                 "#EC CI_NOFIRST
          FROM fclm_bam_aclink2 AS a
          INNER JOIN FCLM_BAM_AMD_T AS b
          ON a~acc_id = b~acc_id     UP TO 1 ROWS
          INTO (ev_bankn, ev_account_name)
          WHERE a~bukrs = xs_contact-bukrs
           AND  a~hbkid = xs_contact-hbkid
           AND  b~langu = sy-langu.
    ENDSELECT.
    IF sy-subrc <> 0 .
      RAISE account_not_found.
    ENDIF.
  endmethod.


METHOD get_container_order_plants.

    CLEAR et_plants.

    zcl_ca_global_param_helper=>get_parameter_values(
      EXPORTING
        it_parameter_name = VALUE wb2_char30( ( 'E244_CONTAINER_ORDER' ) )
      IMPORTING
        et_param_values   = DATA(lt_param_values) ).

    IF lt_param_values IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_param_values INTO DATA(ls_param).
      APPEND INITIAL LINE TO et_plants ASSIGNING FIELD-SYMBOL(<ls_werks>).
      <ls_werks>-option = 'EQ'.
      <ls_werks>-sign = 'I'.
      <ls_werks>-low = ls_param-werks.
    ENDLOOP.

  ENDMETHOD.


method GET_CURRENCY.
    select single waers                "I - DECK911100
       from lfm1                        "I - DECK911100
       into curr               "I - DECK911100
       where lifnr = vendor      "I - DECK911100
         and ekorg = ekorg.     "I - DECK911100
  endmethod.


METHOD get_goback_month.

    DATA: ls_dat  TYPE ty_dat,
          ls_hdat TYPE ty_dat.

    DATA:lv_newmm    TYPE p,
         lv_diffjjjj TYPE p.

    WRITE:  iv_currdate+0(4) TO ls_dat-jjjj,
            iv_currdate+4(2) TO  ls_dat-mm,
            iv_currdate+6(2) TO  ls_dat-tt.
    lv_diffjjjj =   ( ls_dat-mm + ( - iv_backmonth ) - 1 ) DIV 12.
    lv_newmm    =   ( ls_dat-mm + ( - iv_backmonth ) - 1 ) MOD 12 + 1.
    ls_dat-jjjj = ls_dat-jjjj +  lv_diffjjjj.

    IF lv_newmm < 10.
      WRITE '0' TO  ls_dat-mm+0(1).
      WRITE lv_newmm TO  ls_dat-mm+1(1).
    ELSE.
      WRITE lv_newmm TO  ls_dat-mm.
    ENDIF.
    IF ls_dat-tt > '28'.
      ls_hdat-tt = '01'.
      lv_newmm   = ( ls_dat-mm  )  MOD 12 + 1.
      ls_hdat-jjjj = ls_dat-jjjj + ( (  ls_dat-mm ) DIV 12 ).

      IF lv_newmm < 10.
        WRITE '0' TO ls_hdat-mm+0(1).
        WRITE lv_newmm TO ls_hdat-mm+1(1).
      ELSE.
        WRITE lv_newmm TO ls_hdat-mm.
      ENDIF.

      IF ls_dat-tt = '31'.
        ev_newdate = ls_hdat.
        ev_newdate = ev_newdate - 1.
      ELSE.
        IF ls_dat-mm = '02'.
          ev_newdate = ls_hdat.
          ev_newdate = ev_newdate - 1.
        ELSE.
          ev_newdate = ls_dat.
        ENDIF.
      ENDIF.
    ELSE.
      ev_newdate = ls_dat.
    ENDIF.

  ENDMETHOD.


METHOD get_hash_val.
*----------------------------------------------------------------*
*--Generate HASH token for <System>+<Installation>+<Parameter>
*----------------------------------------------------------------*

    DATA: lv_hash     TYPE string,
          lv_md5_hash TYPE md5_fields-hash.

    CLEAR ev_hashed_content.

    CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
      IMPORTING
        license_number = lv_hash.

    CONCATENATE sy-sysid lv_hash iv_param INTO lv_hash.

    IF iv_hash_algo CP 'SHA*'.

      TRY.

          cl_abap_message_digest=>calculate_hash_for_char(
          EXPORTING
            if_algorithm           =  iv_hash_algo       " Hash Algorithm
            if_data                =  lv_hash
          IMPORTING
            ef_hashstring          =  ev_hashed_content
        ).

        CATCH cx_abap_message_digest.
          CLEAR ev_hashed_content.
          RAISE EXCEPTION TYPE cx_abap_x509_certificate
            EXPORTING
              textid = cx_abap_x509_certificate=>parse_error.

        CATCH cx_sy_move_cast_error.

      ENDTRY.

    ELSEIF iv_hash_algo = 'MD5'.

*   MD5 hash: 32 characters
      CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
        EXPORTING
          data   = iv_param
        IMPORTING
          hash   = lv_md5_hash
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        CLEAR lv_md5_hash.
      ENDIF.

      ev_hashed_content = lv_md5_hash.

    ENDIF.

  ENDMETHOD.


method GET_JULIAN_DATE.
*"""""""""""""""""""""""""""""""""""""""""""""""""""""
*DESCRIPTION : Get Julian Date based on Current date
* AUTHOR     : C5223321
*"""""""""""""""""""""""""""""""""""""""""""""""""""""
*Data Declaration
    DATA: lv_days TYPE numc3,
          lv_date TYPE sy-datum.

*Concatenate the start date of year
    lv_date = iv_date(4) && '0101'.
*Get the difference between the days
    lv_days = iv_date - lv_date + 1.
*Get Julian date - > Concatenate based on import year and no of days
    DATA(lv_julian) = iv_date+3(1) && lv_days.

    ev_julian_date = lv_julian.

  endmethod.


method get_leadtime_discount_params.

    clear:ev_lead_days,ev_max_so_qty.

    zcl_ca_global_param_helper=>get_parameter_values(
      exporting
        it_parameter_name = value wb2_char30( ( 'E076_ZD05_LEAD_DAYS' )
                                              ( 'E076_ZD05_TOT_SO_QTY' ) )
      importing
        et_param_values   = data(lt_params) ).

    read table lt_params into data(ls_params) with key parameter_name = 'E076_ZD05_LEAD_DAYS'.
    if sy-subrc = 0.
      condense ls_params-value_field1 no-gaps.
      try.
          ev_lead_days = ls_params-value_field1.
        catch cx_sy_conversion_no_number ##NO_HANDLER.
      endtry.
    endif.

    read table lt_params into ls_params with key parameter_name = 'E076_ZD05_TOT_SO_QTY'.
    if sy-subrc = 0.
      condense ls_params-value_field1 no-gaps.
      try.
          ev_max_so_qty = ls_params-value_field1.
        catch cx_sy_conversion_no_number ##NO_HANDLER.
      endtry.
    endif.

  endmethod.


METHOD get_manifest_formatting.

    DATA : lo_admin_data TYPE REF TO if_fdt_admin_data,
           lo_function   TYPE REF TO if_fdt_function,
           lo_context    TYPE REF TO if_fdt_context,
           lo_result     TYPE REF TO if_fdt_result.

    TRY.
        cl_fdt_factory=>get_instance_generic( EXPORTING iv_id = gc_application_id    "00163E1D3A421EE7948476C7766382CF
                                              IMPORTING eo_instance = lo_admin_data ).
      CATCH cx_fdt_input  ##NO_HANDLER.
    ENDTRY.

    IF lo_admin_data IS BOUND.
      TRY.
          lo_function ?= lo_admin_data.
        CATCH cx_sy_move_cast_error ##NO_HANDLER.
      ENDTRY.
      IF lo_function IS BOUND.
        TRY.
            lo_context ?= lo_function->get_process_context( ).
          CATCH cx_fdt_config ##NO_HANDLER.
          CATCH cx_fdt_input ##NO_HANDLER.
          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.
        IF lo_context IS BOUND.
          TRY.
              lo_function->process( EXPORTING io_context = lo_context
                                    IMPORTING eo_result = lo_result ).
              IF lo_result IS BOUND.
                lo_result->get_value( IMPORTING ea_value = rt_format ).
              ENDIF.
            CATCH cx_fdt_input ##NO_HANDLER.
            CATCH cx_fdt ##NO_HANDLER.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


method GET_MPE.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to return list of MPE company codes
* AUTHOR: I320039
* DATE  : 8/12/2016
*----------------------------------------------------------------------*

  DATA(lt_mpe_range) = VALUE rsis_t_range( ( SIGN = 'I' OPTION = 'EQ' LOW = '50AL' ) ).

  et_mpe_range = lt_mpe_range.

  endmethod.


METHOD get_obj_char.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to get the characteristics of an object
* AUTHOR: I064763
* DATE  : 06/2/2016
*----------------------------------------------------------------------*

      SELECT *
        FROM zvca_char_cds
        INTO TABLE et_objk_char
        WHERE klart in it_klart
         AND  class in it_class
         AND  objek in it_objek
         AND  atinn in it_atinn
         AND  atzhl in it_atzhl
         AND  mafid in it_mafid
         AND  atwrt in it_atwrt
         AND  datuv in it_datuv
         AND  lkenz in it_lkenz
         AND  datub in it_datub.
        IF sy-subrc = 0.
          SORT et_objk_char.
        ENDIF.

  ENDMETHOD.


METHOD get_partner_details.

    DATA: lv_timestamp TYPE bu_addr_valid_from.

*** Get the timestamp for current date and time,
    GET TIME STAMP FIELD lv_timestamp.
*** Get valid address number from BUT020 and then partner details from ADRC and ADR6.
    SELECT a~partner,         "partner
           a~addrnumber,      "Address Number
           b~name1,           "Name1
           b~name2,           "Name2
           b~house_num1,      "House No.
           b~street,          "Street
           b~str_suppl1,      "Street2
           b~str_suppl2,      "Street3
           b~city1,           "City
           b~post_code1,      "City postal code
           b~region,          "Region
           b~country,         "Country
           c~smtp_addr,       "Email
           d~tel_number,      "Telephone Number
           f~remark           "Other communication->SSFID->Notes
                   INTO TABLE @et_partner_details
                   FROM
                   but000 AS g INNER JOIN but021_fs AS h "sujit
                   ON g~partner = h~partner "sujit
                   INNER JOIN but020 AS a
                   ON g~partner = a~partner
                   INNER JOIN adrc AS b
                   ON a~addrnumber = b~addrnumber
                   LEFT OUTER JOIN adr6 AS c
                   ON a~addrnumber = c~addrnumber
                   LEFT OUTER JOIN adr2 AS d
                   ON a~addrnumber = d~addrnumber
                   AND  d~valid_from <= @lv_timestamp
                   AND ( d~valid_to >= @lv_timestamp
                      OR d~valid_to  = ' ' )
                   LEFT OUTER JOIN adr11 AS e
                   ON  e~addrnumber = a~addrnumber
                   AND e~ssfid_srch = @iv_ssfid
                   LEFT OUTER JOIN adrt AS f
                   ON  f~addrnumber = a~addrnumber
                   AND f~consnumber = e~consnumber
                   AND f~langu      = @sy-langu
                   WHERE a~partner IN @it_partners
                   AND   a~addr_valid_from <= @lv_timestamp
                   AND   a~addr_valid_to >= @lv_timestamp
                   AND g~bpkind = '01'
                   AND h~adr_kind = 'ZVMS001'.

    DATA         : et_partner_details1 LIKE et_partner_details.  "HW2-140-I
    FIELD-SYMBOLS:<ls_partner_details>  TYPE zsecc_partners.     "HW2-140-I

    SELECT a~partner,         "partner
         a~addrnumber,      "Address Number
         b~name1,           "Name1
         b~name2,           "Name2
         b~house_num1,      "House No.
         b~street,          "Street
         b~str_suppl1,      "Street2
         b~str_suppl2,      "Street3
         b~city1,           "City
         b~post_code1,      "City postal code
         b~region,          "Region
         b~country,         "Country
         c~smtp_addr,       "Email
         d~tel_number,      "Telephone Number
         f~remark           "Other communication->SSFID->Notes
                 INTO TABLE @et_partner_details1
                 FROM
                 but000 AS g INNER JOIN but021_fs AS h "HW2-140-I
                 ON g~partner = h~partner "HW2-140-I
                 INNER JOIN but020 AS a
                 ON g~partner = a~partner
                 INNER JOIN adrc AS b
                 ON a~addrnumber = b~addrnumber
                 LEFT OUTER JOIN adr6 AS c
                 ON a~addrnumber = c~addrnumber
                 LEFT OUTER JOIN adr2 AS d
                 ON a~addrnumber = d~addrnumber
                 AND  d~valid_from <= @lv_timestamp
                 AND ( d~valid_to >= @lv_timestamp
                    OR d~valid_to  = ' ' )
                 LEFT OUTER JOIN adr11 AS e
                 ON  e~addrnumber = a~addrnumber
                 AND e~ssfid_srch = @iv_ssfid
                 LEFT OUTER JOIN adrt AS f
                 ON  f~addrnumber = a~addrnumber
                 AND f~consnumber = e~consnumber
                 AND f~langu      = @sy-langu
                 WHERE a~partner IN @it_partners
                 AND   a~addr_valid_from <= @lv_timestamp
                 AND   a~addr_valid_to >= @lv_timestamp
                 AND g~bpkind <> '01'.
*                   AND h~adr_kind = 'ZVMS001'.

    APPEND LINES OF et_partner_details1 TO et_partner_details.
"HW2-140-I
    IF et_partner_details IS NOT INITIAL.
      LOOP AT et_partner_details ASSIGNING <ls_partner_details>  .
        TRANSLATE  <ls_partner_details>-city1 TO UPPER CASE.
        MODIFY et_partner_details FROM <ls_partner_details>  TRANSPORTING city1.
      ENDLOOP.
    ENDIF.
"HW2-140-I
    IF sy-subrc <> 0.
**      If no data found then rasie an exception.
      RAISE EXCEPTION TYPE zcx_partners_not_found.
    ELSE.
**      Sort the date based on partner number.
      SORT et_partner_details ASCENDING BY partner.
    ENDIF.
  ENDMETHOD.


METHOD get_plant_add_details.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method collects all address details of plants given as input.
* AUTHOR: I330067
* DATE  : 27/02/2017
*----------------------------------------------------------------------*

    SELECT werks,
           adrnr
           INTO TABLE @DATA(lt_werks)
           FROM t001w
           FOR ALL ENTRIES IN @it_werks
           WHERE werks = @it_werks-table_line.

    IF sy-subrc = 0 AND lt_werks IS NOT INITIAL.

      SELECT addrnumber,
             name1,
             name2,
             name3,
             name4,
             name_text,
             name_co,
             city1,
             city2,
             post_code1,
             post_code2,
             post_code3,
             po_box,
             street,
             str_suppl1,
             str_suppl2,
             str_suppl3,
             country,
             region,
             tel_number,
             tel_extens,
             fax_number,
             fax_extens
             INTO TABLE @DATA(lt_adrc)
             FROM adrc
             FOR ALL ENTRIES IN @lt_werks
             WHERE addrnumber = @lt_werks-adrnr.

      IF sy-subrc = 0.
        et_plant_details = lt_adrc.
      ELSE.
        RAISE address_does_not_exist.
      ENDIF.
      ELSE.
        RAISE plant_does_not_exist.
    ENDIF.
  ENDMETHOD.


method GET_PLNT_STLOC_FRM_WHID.
***----------------------------------------------------------------------------------*
*** DESCRIPTION:
*** This method takes Warehouse ID and fetches the corresponding Plant and Storage Loc
*** vendor code
*** AUTHOR: I329916
*** DATE  : 24/12/2016
***------------------------------------------------------------------------------------*
**    DATA:
**          lt_whid_range TYPE TABLE OF ty_whid_range,
**          ls_whid_range TYPE ty_whid_range.
**
**    LOOP AT it_whid into DATA(ls_whid).
**     ls_whid_range-sign = gc_range_sign.
**     ls_whid_range-option = gc_range_option.
**     ls_whid_range-low = ls_whid-whid.
**     APPEND ls_whid_range TO lt_whid_range.
**     CLEAR ls_whid_range.
**     ENDLOOP.
**
**    SELECT  mandt,whid,werks,lgort
**              FROM zmmc_whid
**              INTO TABLE @ET_PLNT_STLOC
**              WHERE whid IN @LT_WHID_RANGE.
**   IF sy-subrc <> 0.
**     CLEAR ET_PLNT_STLOC.
**   ENDIF.
  endmethod.


METHOD get_print_logos.

* Global data declarations
   DATA : lv_logoname TYPE tdobname.

    "Check if initial
    IF xs_logo-bukrs IS INITIAL AND
       xs_logo-vkorg IS INITIAL AND
       xs_logo-fonam IS INITIAL.
      RAISE too_few_parameters.
    ENDIF.

    "Get logo
    SELECT SINGLE * FROM zcac_logos INTO xs_logo
    WHERE bukrs = xs_logo-bukrs
      AND vkorg = xs_logo-vkorg
      AND vtweg = xs_logo-vtweg
      AND spart = xs_logo-spart
      AND fonam = xs_logo-fonam
      AND spras = 'E' . "sy-langu.  "HW2-247
    IF sy-subrc <> 0.
      RAISE logos_not_found.
    ELSE.
      lv_logoname = xs_logo-logo1.
      CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
        EXPORTING
          p_object       = gc_object
          p_name         = lv_logoname
          p_id           = gc_id
          p_btype        = gc_btype
        RECEIVING
          p_bmp          = ev_xstring
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
       RAISE error_graphic_tobmp.
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD get_service_url.
*---------------------------------------------------------------------------------*
* Get SICF Service URL
* Method accepts path(SICF Node) and Paramters required as URL query string
* Complete URL is returned on successfull execution
*---------------------------------------------------------------------------------*
    CONSTANTS: lc_http     TYPE  string VALUE 'http',
               lc_https    TYPE  string VALUE 'https',
               lc_port_80  TYPE  char2  VALUE '80',
               lc_port_443 TYPE  char3  VALUE '443',
               lc_sp1      TYPE  char3  VALUE '://',
               lc_sp2      TYPE  char1  VALUE ':',
               lc_path1    TYPE  string VALUE '/sap/bc/theming/*',
               lc_vhost    TYPE  string VALUE '/sap/bc/theming'.

    DATA: lv_host             TYPE string,
          lv_port             TYPE string,
          lv_protocol         TYPE string,
          lv_parameter_string TYPE string,
          lv_app_path         TYPE string.

    IF iv_path CP lc_path1. "'/sap/bc/theming/*'.
      lv_app_path =  lc_vhost. " '/sap/bc/theming'. " This is required to get the right virtual host for the admin vhost
    ELSE.
      lv_app_path = iv_path.
    ENDIF.

    IF iv_always_https = abap_true.
      cl_http_server=>get_location(
        EXPORTING
          application = lv_app_path
          server      = iref_server
          protocol    = gc_pro_https     "'HTTPS'
        IMPORTING
          host          = lv_host
          port          = lv_port
          out_protocol  = lv_protocol
     ).
    ENDIF.

    " In case HTTPS is not available or not desired
    IF lv_host IS INITIAL.
      cl_http_server=>get_location(
        EXPORTING
          application = lv_app_path
          server      = iref_server
        IMPORTING
          host          = lv_host
          port          = lv_port
          out_protocol  = lv_protocol
     ).
    ENDIF.

    " In case HTTPURLLOC is maintained the protocol is returned in upper case
    " This is not understood by the theming engine
    TRANSLATE lv_protocol TO LOWER CASE.

    IF ( lv_protocol = lc_http AND lv_port = lc_port_80 ) OR ( lv_protocol = lc_https AND lv_port = lc_port_443 ).
      CONCATENATE lv_protocol lc_sp1 lv_host iv_path INTO ev_url.
    ELSE.
      CONCATENATE lv_protocol lc_sp1 lv_host lc_sp2 lv_port iv_path INTO ev_url.
    ENDIF.

    IF lv_protocol IS INITIAL OR lv_host IS INITIAL OR lv_port IS INITIAL.
      es_return-id      = gc_msgid.      "'ZTECH01'.
      es_return-type    = gc_e.          "'E'.
      es_return-number  = '0001'.
      es_return-message_v1 = ev_url.
      RETURN.
    ENDIF.

    lv_parameter_string = cl_http_utility=>fields_to_string( it_parameters ).
    IF lv_parameter_string IS NOT INITIAL.
      CONCATENATE ev_url '?' lv_parameter_string INTO ev_url.
    ENDIF.

  ENDMETHOD.


METHOD get_shipment_clubbing_plants.

    CLEAR et_plants.

    zcl_ca_global_param_helper=>get_parameter_values(
      EXPORTING
        it_parameter_name = VALUE wb2_char30( ( 'E244_SHPMNT_CLUB' ) )
      IMPORTING
        et_param_values   = DATA(lt_param_values) ).

    LOOP AT lt_param_values INTO DATA(ls_param).
      APPEND INITIAL LINE TO et_plants ASSIGNING FIELD-SYMBOL(<ls_werks>).
      <ls_werks>-option = 'EQ'.
      <ls_werks>-sign = 'I'.
      <ls_werks>-low = ls_param-werks.
    ENDLOOP.

  ENDMETHOD.


METHOD get_sys_hostid.
*---------------------------------------------------------------------------------*
* Get SICF Service URL
* Method accepts path(SICF Node) and Paramters required as URL query string
* Complete URL is returned on successfull execution
*---------------------------------------------------------------------------------*
    CONSTANTS: lc_http     TYPE  string VALUE 'http',
               lc_https    TYPE  string VALUE 'https',
               lc_port_80  TYPE  char2  VALUE '80',
               lc_port_443 TYPE  char3  VALUE '443',
               lc_sp1      TYPE  char3  VALUE '://',
               lc_sp2      TYPE  char1  VALUE ':',
               lc_path1    TYPE  string VALUE '/sap/bc/theming/*',
               lc_vhost    TYPE  string VALUE '/sap/bc/theming'.

    DATA: lv_host             TYPE string,
          lv_port             TYPE string,
          lv_protocol         TYPE string,
          lv_parameter_string TYPE string,
          lv_app_path         TYPE string.


    IF iv_path CP lc_path1. "'/sap/bc/theming/*'.
      lv_app_path =  lc_vhost. " '/sap/bc/theming'. " This is required to get the right virtual host for the admin vhost
    ELSE.
      lv_app_path = iv_path.
    ENDIF.

    IF iv_always_https = abap_true.
      cl_http_server=>get_location(
        EXPORTING
          application = lv_app_path
          server      = iref_server
          protocol    = gc_pro_https     "'HTTPS'
        IMPORTING
*          host          = lv_host
          port          = lv_port
          out_protocol  = lv_protocol
     ).
    ENDIF.

*--> Fetch the system host from the customizing table
    IF iv_host IS INITIAL.
      SELECT SINGLE host
        FROM zca_syshost
        INTO lv_host
        WHERE sysid = sy-sysid.
      IF sy-subrc <> 0.
        CLEAR lv_host.
      ENDIF.
    ELSE.
      lv_host = iv_host.
    ENDIF.

    " In case HTTPURLLOC is maintained the protocol is returned in upper case
    " This is not understood by the theming engine
    TRANSLATE lv_protocol TO LOWER CASE.

    IF ( lv_protocol = lc_http AND lv_port = lc_port_80 ) OR ( lv_protocol = lc_https AND lv_port = lc_port_443 ).
      CONCATENATE lv_protocol lc_sp1 lv_host iv_path INTO ev_url.
    ELSE.
      CONCATENATE lv_protocol lc_sp1 lv_host iv_path INTO ev_url.
    ENDIF.

    IF lv_protocol IS INITIAL OR lv_host IS INITIAL OR lv_port IS INITIAL.
      es_return-id      = gc_msgid.      "'ZTECH01'.
      es_return-type    = gc_e.          "'E'.
      es_return-number  = '001'.
      es_return-message_v1 = ev_url.
      RETURN.
    ENDIF.

    lv_parameter_string = cl_http_utility=>fields_to_string( it_parameters ).

    IF lv_parameter_string IS NOT INITIAL.
      CONCATENATE ev_url '?' lv_parameter_string INTO ev_url.
    ENDIF.

  ENDMETHOD.


method GET_VENDOR_CODE.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method takes in vendor number and fetches the corresponding
* vendor code
* AUTHOR: I320039
* DATE  : 1/12/2016
*----------------------------------------------------------------------*

*   temporary table to store vendor code values
    TYPES: BEGIN OF ty_vendor_code,
             land1      TYPE land1_gp,
             order_code TYPE char3,
           END OF ty_vendor_code.
    TYPES ty_t_vendor_code TYPE STANDARD TABLE OF ty_vendor_code WITH DEFAULT KEY.

    IF xv_land1 IS INITIAL AND iv_lifnr IS INITIAL.
      RAISE too_few_parameter.
    ELSEIF xv_land1 IS INITIAL.

*     Get country code based on vendor number
      SELECT SINGLE LAND1
                    FROM LFA1
                    INTO @xv_land1
                    WHERE lifnr = @iv_lifnr.
        IF sy-subrc <> 0.
          RAISE country_key_not_found.
        ENDIF.
    ENDIF.

        IF iv_vms = abap_true.
*         Fill table with VMS order codes
          DATA(lt_vendor_code) = VALUE ty_t_vendor_code( ( land1 = 'JP' order_code = '001' )
                                                         ( land1 = 'TH' order_code = '37A' )
                                                         ( land1 = 'US' order_code = '101' )
                                                         ( land1 = 'GB' order_code = '7A0' )
                                                        ).
          TRY .
*             Return order code
              rv_order_code = lt_vendor_code[ land1 = xv_land1 ]-order_code.
            CATCH cx_sy_itab_line_not_found.
              RAISE order_code_not_found.
          ENDTRY.

        ELSE.
*         Fill table with Parts order codes
          DATA(lt_vendor_code_1) = VALUE ty_t_vendor_code( ( land1 = 'JP' order_code = '05' )
                                                           ( land1 = 'TH' order_code = '60' )
                                                            ).
          TRY .
*             Return order code
              rv_order_code = lt_vendor_code_1[ land1 = xv_land1 ]-order_code.
            CATCH cx_sy_itab_line_not_found.
              RAISE order_code_not_found.
          ENDTRY.

      ENDIF.
  endmethod.


METHOD import_excel_to_internal_table.

* Define Screen Container
  DATA: lo_container TYPE REF TO cl_gui_custom_container.
  DATA: lo_error       TYPE REF TO i_oi_error,
        lo_control     TYPE REF TO i_oi_container_control,
        lo_document    TYPE REF TO i_oi_document_proxy,
        lo_spreadsheet TYPE REF TO i_oi_spreadsheet.

* Data declarations.
  DATA: lt_files    TYPE filetable,
        ls_files    TYPE file_table,
        lv_doc_name TYPE char256,
        lv_changed  TYPE int4,
        lv_rcode    TYPE int4,
        lt_ranges   TYPE soi_range_list,
        ls_ranges   TYPE soi_range_item,
        lt_data     TYPE soi_generic_table,
        ls_data     TYPE soi_generic_item,
        lv_action   TYPE int4.

  DATA: lref_res TYPE REF TO data,
        lr_res   TYPE REF TO data.
  FIELD-SYMBOLS: <lt_result> TYPE STANDARD TABLE.
  DATA:lv_col TYPE i.


  CLASS c_oi_errors DEFINITION LOAD.

* Create Instance control for container
  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = lo_control
      error   = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

* Create generic container linked to container in screen 100
  CREATE OBJECT lo_container
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE e208(00) WITH 'Error creating container'.
  ENDIF.

* Establish connection to GUI Control
  CALL METHOD lo_control->init_control
    EXPORTING
      r3_application_name = 'Excel Document Container'
      inplace_enabled     = gc_x
      parent              = lo_container
    IMPORTING
      error               = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

* Create Document Proxy
  CALL METHOD lo_control->get_document_proxy
    EXPORTING
      document_type  = soi_doctype_excel_sheet
    IMPORTING
      document_proxy = lo_document
      error          = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

  IF iv_filename IS INITIAL.
* Call dialog to navigate to file
    CALL METHOD me->file_open_dialog
      EXPORTING
        iv_filext   = gc_xls
      IMPORTING
        ev_filename = lv_doc_name.
  ELSE.
    lv_doc_name = iv_filename.
  ENDIF.
* Open Spreadsheet in SAPWORKDIR
  CALL METHOD lo_document->open_document
    EXPORTING
      open_inplace   = gc_x
      document_title = 'Excel'
      document_url   = lv_doc_name
      no_flush       = ''
    IMPORTING
      error          = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

* Open Spreadsheet interface
  CALL METHOD lo_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ''
    IMPORTING
      sheet_interface = lo_spreadsheet
      error           = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

  IF iv_rows IS NOT INITIAL.
    DATA(lv_rows) = iv_rows.
  ELSE.
    lv_rows = zcl_h1s_generic_utils=>gc_maxrows.
  ENDIF.

  IF iv_columns IS NOT INITIAL.
    DATA(lv_columns) = iv_columns.
  ELSE.
    lv_columns = zcl_h1s_generic_utils=>gc_maxcols.
  ENDIF.

* Set selection for 1000 rows
  CALL METHOD lo_spreadsheet->set_selection
    EXPORTING
      top     = 1
      left    = 1
      rows    = lv_rows
      columns = lv_columns.



* Define Range in spreadsheet
  CALL METHOD lo_spreadsheet->insert_range
    EXPORTING
      name     = 'Test'
      rows     = lv_rows
      columns  = lv_columns
      no_flush = ''
    IMPORTING
      error    = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

  ls_ranges-name    = 'Test'.
  ls_ranges-rows    = lv_rows.
  ls_ranges-columns = lv_columns.
  APPEND ls_ranges TO lt_ranges.

* Get data
  CALL METHOD lo_spreadsheet->get_ranges_data
    EXPORTING
      all      = ''
      no_flush = ''
    IMPORTING
      contents = lt_data
      error    = lo_error
    CHANGING
      ranges   = lt_ranges.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.
* Close document

* Close the document
  CALL METHOD lo_document->close_document
    EXPORTING
      do_save     = ''
      no_flush    = ''
    IMPORTING
      has_changed = lv_changed
      error       = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

* Clear Document Resources
  CALL METHOD lo_document->release_document
    EXPORTING
      no_flush = ''
    IMPORTING
      error    = lo_error.

  IF lo_error->has_failed = gc_x.
    CALL METHOD lo_error->raise_message
      EXPORTING
        type = gc_e.
  ENDIF.

* Clear table of file names
  FREE: lt_files,
        lo_control.


  CREATE DATA lr_res LIKE LINE OF et_result.
  ASSIGN lr_res->* TO FIELD-SYMBOL(<ls_result>).
  GET REFERENCE OF et_result INTO lref_res.
  ASSIGN lref_res->* TO <lt_result>.

* Display the data
  LOOP AT lt_data INTO ls_data.
    AT NEW row.
      APPEND <ls_result> TO <lt_result>.
      CLEAR <ls_result>.
    ENDAT.
    MOVE ls_data-column TO lv_col.
    ASSIGN COMPONENT lv_col OF STRUCTURE <ls_result> TO FIELD-SYMBOL(<ls_comp>).
    IF sy-subrc = 0.
      <ls_comp> = ls_data-value.
    ENDIF.
    CLEAR lv_col.
  ENDLOOP.

  IF sy-subrc = 0.
  ENDIF.


ENDMETHOD.


METHOD save_appl_log.

    DATA: ls_log_handle     TYPE balloghndl,
          ls_s_log          TYPE bal_s_log,
          ls_return         TYPE bapiret2,
          lv_connect_commit TYPE boolean,
          ls_bal_msg        TYPE bal_s_msg,
          lv_ext_no         TYPE bal_s_log-extnumber,
          lt_handle         TYPE bal_t_logh.

    ls_s_log-object    = iv_object.
    ls_s_log-subobject = iv_sub_object.
    ls_s_log-aldate    = sy-datum.
    ls_s_log-altime    = sy-uzeit.
    ls_s_log-aluser    = sy-uname.
    ls_s_log-alprog    = sy-repid.

    IF iv_extid IS NOT INITIAL.
      ls_s_log-extnumber = iv_extid.
    ELSE.
      ls_s_log-extnumber = gc_genlogid. "'H1S_APPLOG'.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_s_log
      IMPORTING
        e_log_handle            = ls_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        ls_return-type   = sy-msgty.
        ls_return-id     = sy-msgid.
        ls_return-number = sy-msgno.
        ls_return-message_v1 = sy-msgv1.
        ls_return-message_v2 = sy-msgv2.
        ls_return-message_v3 = sy-msgv3.
        ls_return-message_v4 = sy-msgv4.
        APPEND ls_return TO et_return.
        CLEAR ls_return.
      ENDIF.
      RETURN.
    ENDIF.

    LOOP AT it_msg_tab INTO DATA(ls_msg).

      CLEAR ls_bal_msg.

      ls_bal_msg-msgty = ls_msg-type.
      ls_bal_msg-msgid = ls_msg-id.
      ls_bal_msg-msgno = ls_msg-number.
      ls_bal_msg-msgv1 = ls_msg-message_v1.
      ls_bal_msg-msgv2 = ls_msg-message_v2.
      ls_bal_msg-msgv3 = ls_msg-message_v3.
      ls_bal_msg-msgv4 = ls_msg-message_v4.
      ls_bal_msg-detlevel = '1'.

      CASE ls_msg-type.
        WHEN 'A' OR 'E' OR 'X'.
          ls_bal_msg-probclass = '1'.
        WHEN 'W'.
          ls_bal_msg-probclass = '3'.
        WHEN OTHERS.
          "Do nothing
      ENDCASE.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = ls_log_handle
          i_s_msg          = ls_bal_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      IF sy-subrc NE 0.
        IF sy-msgid IS NOT INITIAL.
          ls_return-type   = sy-msgty.
          ls_return-id     = sy-msgid.
          ls_return-number = sy-msgno.
          ls_return-message_v1 = sy-msgv1.
          ls_return-message_v2 = sy-msgv2.
          ls_return-message_v3 = sy-msgv3.
          ls_return-message_v4 = sy-msgv4.
          APPEND ls_return TO et_return.
          CLEAR ls_return.
        ENDIF.
      ENDIF.

    ENDLOOP.

    INSERT ls_log_handle INTO TABLE lt_handle.

    IF iv_commit_flag = abap_true.
      lv_connect_commit = abap_true.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client             = sy-mandt
        i_save_all           = ' '
        i_2th_connection     = lv_connect_commit
        i_2th_connect_commit = lv_connect_commit          " Refer Note 1535132
        i_t_log_handle       = lt_handle
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.

    IF sy-subrc <> 0 AND sy-msgid IS NOT INITIAL.
      ls_return-type   = sy-msgty.
      ls_return-id     = sy-msgid.
      ls_return-number = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      APPEND ls_return TO et_return.
      CLEAR ls_return.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = ls_log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      ls_return-type   = sy-msgty.
      ls_return-id     = sy-msgid.
      ls_return-number = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      APPEND ls_return TO et_return.
      CLEAR ls_return.
    ENDIF.


  ENDMETHOD.


method SEND_ATTACHMENT_IN_EMAIL.

    DATA(lc_type) = 'RAW'.

    DATA(lref_send_request) = cl_bcs=>create_persistent( ).

    DATA(lref_document) = cl_document_bcs=>create_document(
                                                i_type = lc_type
                                                i_text = iv_body
                                                i_subject = iv_subject ).




      lref_document->add_attachment(
       EXPORTING
      i_attachment_type = IV_ATTACHMENT_TYPE
      i_attachment_subject = IV_ATTACHMENT_SUBJECT
      i_att_content_hex = IV_ATTACHMENT_CONTENT_HEX ).

    lref_send_request->set_document( lref_document ).


    lref_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address(
                                                           i_address_string = iv_recepient
                                                           ) ).


      DATA(l_sent_successfully) = lref_send_request->send( ).
  endmethod.


METHOD send_email.
****************************************************************
* Description: Send an email updting the below mentioned items
* Subject
* Email content
* Attachment
* Language
* Importance
* Author: Kangkana Deb
* Date: 09.03.2017
*****************************************************************
    TRY.
        "Create send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        DATA(lo_sender) = cl_sapuser_bcs=>create( sy-uname ).

*  "Email From..
        "Add sender to send request
        CALL METHOD lo_send_request->set_sender
          EXPORTING
            i_sender = lo_sender.

*  "Email TO..
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( iv_email_to ).

*  Add recipient to send request
        CALL METHOD lo_send_request->add_recipient
          EXPORTING
            i_recipient = lo_recipient
            i_express   = 'X'.
      CATCH cx_root.
    ENDTRY.
* "Email BODY

    TRY.
        DATA(lo_document) = cl_document_bcs=>create_document(
            EXPORTING
              i_type         = iv_type
              i_subject      = iv_subject
              i_length       = iv_length
              i_language     = iv_language
              i_importance   = iv_importance
              i_sensitivity  = iv_sensitivity
              i_text         = it_email_body
              i_hex          = it_content_hex ).

* Add the attachment
        IF iv_attachment_flag = abap_true.
          lo_document->add_attachment( EXPORTING i_attachment_type = iv_atttachment_type
            i_attachment_subject = iv_attachment_subject
            i_att_content_text   = it_attachment_content
            i_att_content_hex    = it_attachment_content_hex ).
        ENDIF.
*  "Add document to send request
        CALL METHOD lo_send_request->set_document( lo_document ).

* Send email
        DATA(lv_sent_to_all) = lo_send_request->send( EXPORTING i_with_error_screen = 'X' ).

        IF lv_sent_to_all = 'X'.
*          APPEND VALUE bapiret2(
*              type = gc_success
*              id   = 'ZVMS'
*              number = '322' "Email Sent
*              ) TO et_return.

*        WRITE 'Email sent!'.
        ENDIF.

        "Commit to send email
        if IV_COMMIT = 'X'.
           COMMIT WORK.
        endif.
      CATCH cx_root.

    ENDTRY.



  ENDMETHOD.


METHOD set_bal_log.
*************************************************************************
*  Program Title       : Save messages into application log(SLG1)       *
*  Developer           : Atul Mishra(SAP)                               *
*  Description:        : Save messages during inbound interfaces/Batch  *
*                        Runs/wherever applicable as per Requirement    *
*  Type:               : Utility Method                                 *
*  Run Frequency       : NA                                             *
*************************************************************************
* MODIFICATION HISTORY                                                  *
*  Change Request #    :                                                *
*  Author              :                                                *
*  Changed By          :                                                *
*  Modification Date   :                                                *
*  Description         :                                                *
*************************************************************************

    DATA:
      lv_log_handle     TYPE balloghndl,
      ls_log            TYPE bal_s_log,
      ls_bal_log        TYPE bal_s_msg,
      ls_msg            TYPE bapiret2,
      ls_mdef           TYPE bal_s_mdef,
      ls_return         TYPE bapiret2,
      lv_connect_commit TYPE boolean.


    ls_log-object    = iv_object.
    ls_log-subobject = iv_sub_object.

    IF iv_extid IS NOT INITIAL.
      ls_log-extnumber = iv_extid.
    ELSE.
      ls_log-extnumber = gc_genlogid. "'H1S_APPLOG'.
    ENDIF.

    IF iv_log_handle IS INITIAL.
*         Create the log with header data
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = ls_log
        IMPORTING
          e_log_handle            = lv_log_handle
        EXCEPTIONS
          log_header_inconsistent = 1
          OTHERS                  = 2.

      IF sy-subrc = 0.
        ls_mdef-log_handle = lv_log_handle.
      ELSE.
        IF sy-msgid IS NOT INITIAL.
          ls_return-type   = sy-msgty.
          ls_return-id     = sy-msgid.
          ls_return-number = sy-msgno.
          ls_return-message_v1 = sy-msgv1.
          ls_return-message_v2 = sy-msgv2.
          ls_return-message_v3 = sy-msgv3.
          ls_return-message_v4 = sy-msgv4.
          APPEND ls_return TO et_return.
          CLEAR ls_return.
        ENDIF.
      ENDIF.

    ELSE.
      ls_mdef-log_handle = iv_log_handle.
    ENDIF.

*      Set the default value
    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = ls_mdef
      EXCEPTIONS
        OTHERS           = 0.

*     The parameters set by message statement will be used
*     Add the message in the log
    LOOP AT it_msg_tab INTO ls_msg.
*     Define data of messages for Application Log
      IF ls_msg-type = gc_error_msgty.
        ls_bal_log-probclass = 1.
      ENDIF.
      ls_bal_log-msgty = ls_msg-type.
      ls_bal_log-msgid = ls_msg-id.
      ls_bal_log-msgno = ls_msg-number.
      ls_bal_log-msgv1 = ls_msg-message_v1.
      ls_bal_log-msgv2 = ls_msg-message_v2.
      ls_bal_log-msgv3 = ls_msg-message_v3.
      ls_bal_log-msgv4 = ls_msg-message_v4.
*     add this message to log file
*     (I_LOG_HANDLE is not specified, we want to add to the default log.
*     If it does not exist we do not care =>EXCEPTIONS log_not_found = 0)
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg       = ls_bal_log
        EXCEPTIONS
          log_not_found = 0
          OTHERS        = 1.
      IF sy-subrc <> 0 AND sy-msgid IS NOT INITIAL.
        ls_return-type   = sy-msgty.
        ls_return-id     = sy-msgid.
        ls_return-number = sy-msgno.
        ls_return-message_v1 = sy-msgv1.
        ls_return-message_v2 = sy-msgv2.
        ls_return-message_v3 = sy-msgv3.
        ls_return-message_v4 = sy-msgv4.
        APPEND ls_return TO et_return.
        CLEAR ls_return.
      ENDIF.

    ENDLOOP.

*--Commit is generally not required.Only in specific cases this may be used.
*--Due to Rollback or exceptions,Application log is not saved.
*--Independent saving of log is required at many places.
*--Note 1535132 can be reffered.
    CLEAR: lv_connect_commit.
    IF iv_commit_flag = abap_true.
      lv_connect_commit = abap_true.
    ENDIF.

*     save logs in the database
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all           = abap_true   "'X'
        i_2th_connection     = lv_connect_commit
        i_2th_connect_commit = lv_connect_commit          " Refer Note 1535132
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.
    IF sy-subrc <> 0 AND sy-msgid IS NOT INITIAL.
      ls_return-type   = sy-msgty.
      ls_return-id     = sy-msgid.
      ls_return-number = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      APPEND ls_return TO et_return.
      CLEAR ls_return.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
