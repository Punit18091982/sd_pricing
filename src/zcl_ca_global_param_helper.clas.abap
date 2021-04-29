class ZCL_CA_GLOBAL_PARAM_HELPER definition
  public
  final
  create public .

public section.

  types TY_S_PARAM type ZSCA_PARAM_STRUCT .
  types:
    ty_t_param TYPE STANDARD TABLE OF ty_s_param .
  types:
    BEGIN OF ty_s_range_param,
          param_name TYPE char5,
         END OF ty_s_range_param .
  types:
    ty_t_range_param TYPE STANDARD TABLE OF ty_s_range_param .

  constants GC_FUNC_ID type IF_FDT_TYPES=>ID value '00163E1D3A421ED7A2F6E22437F82D97' ##NO_TEXT.

  class-methods GET_PARAMETER_VALUES
    importing
      value(IT_PARAMETER_NAME) type WB2_CHAR30
    exporting
      value(ET_PARAM_VALUES) type ZTT_CA_PARAM_STRUCT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_GLOBAL_PARAM_HELPER IMPLEMENTATION.


  METHOD get_parameter_values.

*Data Declarations
    DATA : lo_admin_data   TYPE REF TO if_fdt_admin_data,
           lo_function     TYPE REF TO if_fdt_function,
           lo_context      TYPE REF TO if_fdt_context,
           lo_result       TYPE REF TO if_fdt_result,
           lt_param        TYPE ztt_ca_param_struct,
           lt_param_values TYPE ztt_ca_param_struct.

    IF it_parameter_name IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_fdt_factory=>get_instance_generic( EXPORTING iv_id = gc_func_id "00163E1D3A421ED7A2F6E22437F82D97'
                                              IMPORTING eo_instance = lo_admin_data ).
      CATCH cx_fdt_input  ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF lo_admin_data IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lo_function ?= lo_admin_data.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF lo_function IS NOT BOUND.
      RETURN.
    ENDIF.

*Set input parameter
    TRY.
        lo_context ?= lo_function->get_process_context( ).
      CATCH cx_fdt_config ##NO_HANDLER.
        RETURN.
      CATCH cx_fdt_input ##NO_HANDLER.
        RETURN.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
        RETURN.
    ENDTRY.

    IF lo_context IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT it_parameter_name ASSIGNING FIELD-SYMBOL(<ls_param_name>).

      TRY.
          lo_context->set_value( iv_name = 'PARAMETER_NAME'
                                 ia_value = <ls_param_name> ).
          lo_function->process( EXPORTING io_context = lo_context
                                IMPORTING eo_result = lo_result ).
          IF lo_result IS NOT BOUND.
            CONTINUE.
          ENDIF.
          lo_result->get_value( IMPORTING ea_value = lt_param_values ).
        CATCH cx_fdt_input ##NO_HANDLER.
        CATCH cx_fdt ##NO_HANDLER.
      ENDTRY.

*Setting Parameter name in the table
      LOOP AT lt_param_values ASSIGNING FIELD-SYMBOL(<ls_param_values>).
        <ls_param_values>-parameter_name = <ls_param_name>.
      ENDLOOP.

      APPEND LINES OF lt_param_values TO lt_param.
      CLEAR : lt_param_values.
    ENDLOOP.

    IF lt_param IS NOT INITIAL.
      et_param_values = lt_param.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
