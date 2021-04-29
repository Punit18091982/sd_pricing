class ZCL_P2M_CASHBACK_PAYMENT definition
  public
  create public .

public section.

  types:
    TY_FINAL_ITAb TYPE STANDARD TABLE OF ZSP2M_CASHBACK_RCTI .
*  types:
*    ty_data_itab  TYPE STANDARD TABLE OF ZVCSHBKHSRDATA .
  types:
    TY_DATE_RANGE TYPE RANGE OF sy-datum .
  types:
    BEGIN OF ty_s_payment_method,
        lifnr TYPE lifnr,
        akont TYPE akont,
        zwels TYPE dzwels,
        zterm TYPE dzterm,
      END OF ty_s_payment_method .
  types:
    ty_t_payment_method TYPE STANDARD TABLE OF ty_s_payment_method .

  data GT_FINAL_ITAB type TY_FINAL_ITAB .
  constants GC_BUS_TRANS_RFBU type GLVOR value 'RFBU' ##NO_TEXT.
  data GT_PAYMENT_METHOD type TY_T_PAYMENT_METHOD .
  data GS_CSHBK_HSR_DATA type ZSP2M_BSHBK_HSR_DATA .
  constants GC_AEX type CHAR3 value 'AEX' ##NO_TEXT.
  data GT_RETURN type BAPIRET2_T .
  constants GC_SIGN type CHAR1 value 'I' ##NO_TEXT.
  constants GC_OPTION type CHAR2 value 'EQ' ##NO_TEXT.

*  methods POST_FI_DOC
*    importing
*      !IT_ITEM_TAB type ZTP2M_CSHBK_HSR
*      !IV_REFKEY type CHAR20 .
  methods UPDATE_EQUIPMENT
    importing
      !IV_EQUNR type EQUNR
      !IV_PROMO_FLAG type ZWTY_PROMO
      !IV_CASHBK_FLAG type ZWTY_CASH
      !IV_DISTINGUISHER type CHAR3
    exporting
      !ES_RETURN type BAPIRET2 .
  methods REVERSE_FI_DOC
    importing
      !IV_BUKRS type BUKRS optional
      !IV_GJAHR type GJAHR optional
      !IV_BELNR type BELNR_D .
  methods GET_DATA
    importing
      !IV_CSHBK_HSR type CHAR3 .
  methods CONSTRUCTOR
    importing
      !IS_CSHBK_HSR_DATA type ZSP2M_BSHBK_HSR_DATA .
  class-methods GET_GL_ACCOUNTS
    importing
      !IV_SALES_ORG type VKORG
      !IV_DIS_CHANNEL type VTWEG
      !IV_DOC_TYPE type BLART optional .
protected section.
private section.

  methods CHECK_DATA_IN_PROCESS .
ENDCLASS.



CLASS ZCL_P2M_CASHBACK_PAYMENT IMPLEMENTATION.


  method CHECK_DATA_IN_PROCESS.
  endmethod.


  method CONSTRUCTOR.
  endmethod.


  method GET_DATA.
  endmethod.


  method GET_GL_ACCOUNTS.
  endmethod.


  method REVERSE_FI_DOC.
  endmethod.


  method UPDATE_EQUIPMENT.
  endmethod.
ENDCLASS.
