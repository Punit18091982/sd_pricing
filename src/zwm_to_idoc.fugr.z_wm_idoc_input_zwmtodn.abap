FUNCTION Z_WM_IDOC_INPUT_ZWMTODN.
*"--------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWFAP_PAR-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWFAP_PAR-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"      OTHERS
*"--------------------------------------------------------------------
************************************************************************
*  Program Title       : IDOC for Transfer Order processing            *
*  Author              : Amit K Jaura(SAP)                             *
*  Developer           : Atul (SAP)                                    *
*  WRICEF ID           : DS_E81                                        *
*  Description:        : IDOC for Transfer Order processing            *
*  Type:               : Enhancement                                   *
*  Run Frequency       : NA                                            *
************************************************************************
  DATA: lv_error       TYPE flag,
        lt_idoc_status TYPE t_idoc_status.


*--Refresh Global variables..
  PERFORM data_refresh.

  SORT idoc_contrl  BY docnum.
  SORT idoc_data    BY docnum segnum.

  LOOP AT idoc_contrl.

    MOVE idoc_contrl TO gs_ikopf.

*--Segment Mapping..
    LOOP AT idoc_data INTO DATA(ls_segm) WHERE docnum = idoc_contrl-docnum.

      PERFORM zwmtodn_segment_mapping USING ls_segm.

    ENDLOOP.

*........Process IDOC to Post Transfer order........................................
    PERFORM zwmtodn_create_to USING     idoc_contrl-docnum
                              CHANGING  lv_error
                                        lt_idoc_status.
    IF lt_idoc_status[] IS NOT INITIAL.
      APPEND LINES OF lt_idoc_status TO idoc_status.
    ENDIF.

*--Error?
    IF lv_error = abap_true.
      ROLLBACK WORK.
      EXIT.
    ENDIF.

  ENDLOOP.


*----------------------------------------------------------------------*
*        FORM zwmtodn_segment_mapping                                  *
*----------------------------------------------------------------------*
*        Map IDOC segments                                             *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM zwmtodn_segment_mapping USING ps_isegm TYPE edidd.

  DATA: ls_toit TYPE zstodnit.

  CASE ps_isegm-segnam.

    WHEN gc_seg_to_header.
*--Header Segment
      CLEAR ztodnhr.
      MOVE ps_isegm-sdata TO ztodnhr.
      MOVE-CORRESPONDING ztodnhr TO gs_tohr.

    WHEN gc_seg_to_item.
*--Item Segment..
      CLEAR ztodnit.
      MOVE ps_isegm-sdata TO ztodnit.
      MOVE-CORRESPONDING ztodnit TO ls_toit.
      APPEND ls_toit TO gt_toit.

  ENDCASE.

ENDFORM.                     "zwmtodn_segment_mapping

*---------------------------------------------------------------------*
*       FORM ZWMTODN_CREATE_TO                                        *
*---------------------------------------------------------------------*
*       Post transfer order for delivery item                         *
*       Only one Delivery item needs to be processed in this case     *
*---------------------------------------------------------------------*
*       INPUT:   P_DOCNUM               IDOC-Nummer                   *
*                I_LTORH                IDOC-Kopf                     *
*                I_LTORI                Tabelle der IDOC-Positionen   *
*                                                                     *
*       OUTPUT:                                                       *
*                                                                     *
*---------------------------------------------------------------------*
FORM zwmtodn_create_to USING VALUE(p_docnum)
                       CHANGING lv_error     TYPE flag
                                ct_idoc_stat TYPE t_idoc_status.

  DATA: ls_delit     TYPE l03b_delit,
        lt_delit     TYPE l03b_delit_t,
        lv_tanum     TYPE tanum,
        lt_ltap      TYPE TABLE OF ltap_vb,
        ls_idoc_stat TYPE bdidocstat.

*--Create transfer order for given delivery lines..
  LOOP AT gt_toit ASSIGNING FIELD-SYMBOL(<fs_toit>).

    CLEAR: lt_delit[], ls_idoc_stat,lv_tanum,lt_ltap.

*--Get delivery details...
*-- Select with in loop. At a time only one entry will be available for delivery line.
    SELECT SINGLE kosta FROM lips INTO @DATA(lv_kosta)
                        WHERE vbeln = @gs_tohr-vbeln
                          AND posnr = @<fs_toit>-posnr.
    IF sy-subrc <> 0.

      MESSAGE e751(zp2p) WITH gs_tohr-vbeln <fs_toit>-posnr INTO DATA(lv_dummy).
      ls_idoc_stat-status = gc_idoc_status_error.    "51: Set IDOC status to error..
    ELSE.

      IF lv_kosta = gc_stat_c.   "'C'.       "Skip if Document is completly processed

        MESSAGE s749(zp2p) WITH gs_tohr-vbeln <fs_toit>-posnr INTO lv_dummy.
        ls_idoc_stat-status = gc_idoc_status_ok.    "53: Set IDOC status to success..

      ELSE.

*-- Proceed with TO Creation for One Delivery line..
        MOVE-CORRESPONDING <fs_toit> TO ls_delit.
        APPEND ls_delit TO lt_delit.

*--Post TO creation..
        CALL FUNCTION 'L_TO_CREATE_DN'
          EXPORTING
            i_lgnum                    = gs_tohr-lgnum
            i_vbeln                    = gs_tohr-vbeln
            i_refnr                    = gs_tohr-refnr
            i_squit                    = gs_tohr-squit
            i_nidru                    = gs_tohr-nidru
            i_drukz                    = gs_tohr-drukz
            i_ldest                    = gs_tohr-ldest
            i_komim                    = gs_tohr-komim
            i_einlm                    = gs_tohr-einlm
            i_einta                    = gs_tohr-einta
            i_nospl                    = gs_tohr-nospl
            i_update_task              = gs_tohr-update_task
            i_commit_work              = gs_tohr-commit_work
            i_bname                    = gs_tohr-bname
            i_teilk                    = gs_tohr-teilk
            i_solex                    = gs_tohr-solex
            "i_pernr                    = gs_tohr-pernr
            it_delit                   = lt_delit
          IMPORTING
            e_tanum                    = lv_tanum
            "E_TEILK                          =
          TABLES
            "T_LTAK                           =
            t_ltap_vb                  = lt_ltap
            "T_WMGRP_MSG                      =
          EXCEPTIONS
            foreign_lock               = 1
            dn_completed               = 2
            partial_delivery_forbidden = 3
            xfeld_wrong                = 4
            ldest_wrong                = 5
            drukz_wrong                = 6
            dn_wrong                   = 7
            squit_forbidden            = 8
            no_to_created              = 9
            teilk_wrong                = 10
            update_without_commit      = 11
            no_authority               = 12
            no_picking_allowed         = 13
            dn_hu_not_choosable        = 14
            input_error                = 15
            error_message              = 16
            OTHERS                     = 17.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          CASE sy-subrc.
            WHEN 01.
              ls_idoc_stat-tid = TEXT-e01.
            WHEN 02.
              ls_idoc_stat-tid = TEXT-e02.
            WHEN 03.
              ls_idoc_stat-tid = TEXT-e03.
            WHEN 04.
              ls_idoc_stat-tid = TEXT-e04.
            WHEN 05.
              ls_idoc_stat-tid = TEXT-e05.
            WHEN 06.
              ls_idoc_stat-tid = TEXT-e06.
            WHEN 07.
              ls_idoc_stat-tid = TEXT-e07.
            WHEN 08.
              ls_idoc_stat-tid = TEXT-e08.
            WHEN 09.
              ls_idoc_stat-tid = TEXT-e09.
            WHEN 10.
              ls_idoc_stat-tid = TEXT-e10.
            WHEN 11.
              ls_idoc_stat-tid = TEXT-e11.
            WHEN 12.
              ls_idoc_stat-tid = TEXT-e12.
            WHEN 13.
              ls_idoc_stat-tid = TEXT-e13.
            WHEN 14.
              ls_idoc_stat-tid = TEXT-e14.
            WHEN 15.
              ls_idoc_stat-tid = TEXT-e15.
            WHEN 16.
              ls_idoc_stat-tid = TEXT-e16.
            WHEN OTHERS.
              ls_idoc_stat-tid = 'Unkown Exception'(e17).
          ENDCASE.

          IF sy-msgid IS INITIAL.
            MESSAGE e750(zp2p) WITH gs_tohr-vbeln <fs_toit>-posnr INTO lv_dummy.
          ENDIF.

          ls_idoc_stat-status = gc_idoc_status_error.    "51: Set IDOC status to error..

        ELSE.

          MESSAGE s312(zp2p) WITH lv_tanum gs_tohr-vbeln INTO lv_dummy.
          ls_idoc_stat-status = gc_idoc_status_ok.        "53: Set IDOC status to success..

        ENDIF.                 "FM Subrc check

      ENDIF.                   "Document status check

    ENDIF.                     "Delivery validation

    ls_idoc_stat-docnum = p_docnum.
    ls_idoc_stat-uname  = sy-uname.
    ls_idoc_stat-routid = 'L_TO_CREATE_DN'.

    MOVE: sy-msgid TO ls_idoc_stat-msgid,
          sy-msgno TO ls_idoc_stat-msgno,
          sy-msgv1 TO ls_idoc_stat-msgv1,
          sy-msgv2 TO ls_idoc_stat-msgv2,
          sy-msgv3 TO ls_idoc_stat-msgv3,
          sy-msgv4 TO ls_idoc_stat-msgv4.

    APPEND ls_idoc_stat TO ct_idoc_stat.


  ENDLOOP.

ENDFORM.              "zwmtodn_create_to
*&---------------------------------------------------------------------*
*&      Form  DATA_REFRESH
*&---------------------------------------------------------------------*
*       refresh of status flags and tables
*----------------------------------------------------------------------*
FORM data_refresh.

  CLEAR:  ztodnhr,
          ztodnit,
          gs_ikopf,
          gs_isegm,
          gv_idoc_nummer,
          gt_toit[],
          gs_tohr.

ENDFORM.                    " DATA_REFRESH





ENDFUNCTION.
