FUNCTION Z_CA_MAINTAIN_OBJECTS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_OBJECTS) TYPE  ZTCA_ZCAD_OBJECTS
*"     REFERENCE(IV_UPD) TYPE  CHAR1
*"     REFERENCE(IV_COMMIT) TYPE  BAPIWAIT OPTIONAL
*"--------------------------------------------------------------------
IF iv_upd = gc_insert.
    INSERT zcad_objects FROM TABLE it_objects ACCEPTING DUPLICATE KEYS.
  ELSEIF iv_upd = gc_update.
    UPDATE zcad_objects FROM TABLE it_objects.
  ENDIF.

  IF sy-subrc = 0.
    IF iv_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.





ENDFUNCTION.
