class ZCL_WM_DELIVERY_EXTENSION definition
  public
  final
  create public .

public section.

  class-data GT_EXTENSION_DATA type ZTWM_ZFIELD_L read-only .
  class-data GS_EXTENSION_HEADER type ZZWM_LIKP_CUST_FIELD read-only .

  class-methods GET_DELI_EXTENSION
    exporting
      !ET_GET_EXTENSION type ZTWM_ZFIELD_L
      !ES_GET_HEADER_EXT type ZZWM_LIKP_CUST_FIELD .
  class-methods SET_DELI_EXTENSION
    importing
      !IT_SET_EXTENSION type ZTWM_ZFIELD_L optional
      !IS_SET_HEADER_EXT type ZZWM_LIKP_CUST_FIELD optional .
  class-methods FREE_EXTENSION_DATA
    returning
      value(EV_SUBRC) type SUBRC .
  class-methods FREE_HEADER_EXT
    returning
      value(EV_SUBRC) type SUBRC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WM_DELIVERY_EXTENSION IMPLEMENTATION.


METHOD free_extension_data.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to delete first entry in the global table
* AUTHOR: C5223321
* DATE  : 22/05/2017
*----------------------------------------------------------------------*

    IF gt_extension_data IS NOT INITIAL.

        DELETE gt_extension_data INDEX 1.
        ev_subrc = sy-subrc.

    ENDIF.

  ENDMETHOD.


METHOD FREE_HEADER_EXT.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to clear the header structure after use
* AUTHOR: I320039
* DATE  : 10/26/2017
*----------------------------------------------------------------------*

    IF gs_extension_header IS NOT INITIAL.

        CLEAR: gs_extension_header.

    ENDIF.

  ENDMETHOD.


METHOD get_deli_extension.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to access data from the static read only table for
* access in BADI extension structure
* AUTHOR: C5223321
* DATE  : 22/05/2017
*----------------------------------------------------------------------*

    IF gt_extension_data IS NOT INITIAL.
      et_get_extension = gt_extension_data.
    ENDIF.

    IF gs_extension_header IS NOT INITIAL.
      es_get_header_ext = gs_extension_header.
    ENDIF.

  ENDMETHOD.


METHOD set_deli_extension.
*----------------------------------------------------------------------*
* DESCRIPTION:
* This method is used to set to a static read only table for access in
* BADI extension structure
* AUTHOR: C5223321
* DATE  : 22/05/2017
*----------------------------------------------------------------------*
    IF it_set_extension IS NOT INITIAL.
      gt_extension_data = it_set_extension.
    ENDIF.

    IF is_set_header_ext IS NOT INITIAL.
      gs_extension_header = is_set_header_ext.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
