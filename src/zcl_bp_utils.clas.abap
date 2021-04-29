class ZCL_BP_UTILS definition
  public
  final
  create public .

public section.

  methods GET_SUBCODE
    importing
      value(PARTNER) type BU_PARTNER
    exporting
      !SUBCODE type BU_ID_NUMBER
    exceptions
      ID_NOT_FOUND .
  methods GET_BPNUM_FROM_SUBCODE
    importing
      value(SUBCODE) type BU_ID_NUMBER
    exporting
      !PARTNER type BU_PARTNER
    exceptions
      PARTNER_NOT_FOUND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BP_UTILS IMPLEMENTATION.


METHOD get_bpnum_from_subcode.

*   Determine a unique business partner number from a Honda subsidiary code


    CONSTANTS: lc_subcode TYPE bapibus1006_identification_key-identificationcategory
                       VALUE 'ZLSUB'.

    DATA: lv_subcode TYPE bapibus1006_identification_key-identificationnumber,
          lt_partner TYPE TABLE OF bus_partner_guid,
          ls_partner TYPE bus_partner_guid,
          lt_return  TYPE TABLE OF bapiret2,
          ls_return  TYPE bapiret2,
          lv_count   TYPE i.


    CALL FUNCTION 'BUPA_PARTNER_GET_BY_IDNUMBER'
      EXPORTING
        iv_identificationcategory = lc_subcode
        iv_identificationnumber   = subcode
      TABLES
        t_partner_guid            = lt_partner
        et_return                 = lt_return.


    DESCRIBE TABLE lt_partner LINES lv_count.

    IF lv_count <> 1.
      RAISE partner_not_found.
    ENDIF.

    READ TABLE lt_partner INTO ls_partner INDEX 1.
    IF sy-subrc IS NOT INITIAL.
      RAISE partner_not_found.
    ENDIF.

    partner = ls_partner-partner.

  ENDMETHOD.


METHOD get_subcode.

*   Determine a Honda subsidiary code from partner number

*    CONSTANTS:  lc_subcode TYPE bu_id_type VALUE 'ZLSUB'.
*
*    DATA: lt_ident  TYPE TABLE OF bapibus1006_id_details,
*          ls_ident  TYPE bapibus1006_id_details,
*          lt_return TYPE TABLE OF bapiret2,
*          ls_return TYPE bapiret2.
*
*    CALL FUNCTION 'BUPA_IDENTIFICATIONDETAILS_GET'
*      EXPORTING
*        iv_partner              = partner
**       IV_PARTNER_GUID         =
*      TABLES
*        et_identificationdetail = lt_ident
*        et_return               = lt_return.
*
*    READ TABLE lt_ident INTO ls_ident WITH KEY identificationtype = lc_subcode.
*    IF sy-subrc IS NOT INITIAL.
*      RAISE id_not_found.
*    ENDIF.
*
*    subcode = ls_ident-identificationnumber.

  ENDMETHOD.
ENDCLASS.
