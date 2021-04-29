class ZCL_CA_GLOBAL_CONSTANTS definition
  public
  final
  create public .

public section.

  class-data GO_REF type ref to ZCL_CA_GLOBAL_CONSTANTS .
  constants GC_I type TVARV_SIGN value 'I' ##NO_TEXT.
  constants GC_OPT type TVARV_OPTI value 'EQ' ##NO_TEXT.
  constants GC_BUK_AUH type BUKRS value '50AK' ##NO_TEXT.
  constants GC_BUK_MPE type BUKRS value '50AL' ##NO_TEXT.
  constants GC_ORDTYPE_ZIND type BSART value 'ZIND' ##NO_TEXT.
  constants GC_ORD_ACCT_CATG type KNTTP value 'F' ##NO_TEXT.
  constants GC_WERKS_MPE type WERKS_D value '2030' ##NO_TEXT.
  constants GC_VKO_CBU_MPE type VKORG value '2000' ##NO_TEXT.
  constants GC_VKO_AUH type VKORG value '1000' ##NO_TEXT.
  constants GC_OBDEL_CATEG type CHAR1 value 'J' ##NO_TEXT.
  constants GC_IBDEL_CATEG type CHAR1 value '7' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(RV_INST) type ref to ZCL_CA_GLOBAL_CONSTANTS .
  methods GET_AUH
    exporting
      !ET_AUH_RANGE type RSIS_T_RANGE .
  methods GET_MPE
    exporting
      !ET_MPE_RANGE type RSIS_T_RANGE .
  class-methods GET_AUH_SORG
    returning
      value(RT_AUH_SORG) type RSIS_T_RANGE .
  class-methods GET_CBU_MPE_SORG
    returning
      value(RT_SORG) type RSIS_T_RANGE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_GLOBAL_CONSTANTS IMPLEMENTATION.


METHOD get_auh.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to return list of AUH company codes
* AUTHOR: I320039
* DATE  : 8/12/2016
*----------------------------------------------------------------------*

    DATA(lt_auh_range) = VALUE rsis_t_range( ( sign = gc_i option = gc_opt low = gc_buk_auh ) ).

    et_auh_range = lt_auh_range.

  ENDMETHOD.


METHOD get_auh_sorg.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to return list of AUH Sales Org
* AUTHOR: I067065
* DATE  : 6/11/2018
*----------------------------------------------------------------------*

    rt_auh_sorg = VALUE rsis_t_range( ( sign = gc_i option = gc_opt low = gc_vko_auh ) ).

  ENDMETHOD.


METHOD get_cbu_mpe_sorg.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to return list of CBU/MPE Sales Org
* AUTHOR: I067065
* DATE  : 6/11/2018
*----------------------------------------------------------------------*

    rt_sorg = VALUE rsis_t_range( ( sign = gc_i option = gc_opt low = gc_vko_cbu_mpe ) ).

  ENDMETHOD.


method GET_INSTANCE.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to ensure multiple instances are not created
* AUTHOR: I320039
* DATE  : 13/12/2016
*----------------------------------------------------------------------*

IF go_ref IS NOT BOUND.

* Create new instance if not available
  go_ref = NEW zcl_ca_global_constants( ).
  rv_inst = go_ref.

ELSE.

* return old instance if already created
  rv_inst = go_ref.

ENDIF.
  endmethod.


METHOD get_mpe.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to return list of MPE company codes
* AUTHOR: I320039
* DATE  : 8/12/2016
*----------------------------------------------------------------------*

    DATA(lt_mpe_range) = VALUE rsis_t_range( ( sign = gc_i option = gc_opt low = gc_buk_mpe ) ).

    et_mpe_range = lt_mpe_range.

  ENDMETHOD.
ENDCLASS.
