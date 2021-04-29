class ZCL_SD_INITIATE_LBL_CREATION definition
  public
  final
  create private .

public section.

  data GV_MEDIUM type CHAR1 .
  data GV_ACTION type CHAR1 .

  class-methods GET_OBJECT
    returning
      value(RO_OBJECT) type ref to ZCL_SD_INITIATE_LBL_CREATION .
  methods SET_MEDIUM
    importing
      !IV_MEDIUM type CHAR1 optional
      !IV_ACTION type CHAR1 optional .
  methods CLEAR_OBJECT
    changing
      !RO_OBJECT type ref to ZCL_SD_INITIATE_LBL_CREATION .
protected section.
private section.

  class-data GR_SINGLETON type ref to ZCL_SD_INITIATE_LBL_CREATION .
ENDCLASS.



CLASS ZCL_SD_INITIATE_LBL_CREATION IMPLEMENTATION.


METHOD clear_object.
    CLEAR : ro_object.
  ENDMETHOD.


METHOD get_object.
    IF zcl_sd_initiate_lbl_creation=>gr_singleton IS NOT BOUND.
      CREATE OBJECT zcl_sd_initiate_lbl_creation=>gr_singleton.
    ENDIF.
      ro_object = zcl_sd_initiate_lbl_creation=>gr_singleton.

  ENDMETHOD.


METHOD set_medium.
    gv_medium = iv_medium.
    gv_action = iv_action.
  ENDMETHOD.
ENDCLASS.
