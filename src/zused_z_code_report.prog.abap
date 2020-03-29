*&---------------------------------------------------------------------*
*& Report ZUSED_Z_CODE_REPORT
*&---------------------------------------------------------------------*
* There is no standard SAP Program to analyze the results of SCMON/SUSG
* You have to write your own as described in Section 2.2.2
* This is the most basic program I could write to display the data.
* You could add all sorts of bells and whistles, such as automatically
* reclassifying unused objects to a shadow "unused" package hierarchy
*&---------------------------------------------------------------------*
REPORT zused_z_code_report.
*--------------------------------------------------------------------*
* Global Definitions
*--------------------------------------------------------------------*
TYPES: BEGIN OF g_typ_alv_output,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         ddtext   TYPE char80,
         devclass TYPE tadir-devclass,
         used     TYPE icon_text,
       END OF   g_typ_alv_output.

TYPES: g_tt_alv_output TYPE STANDARD TABLE OF g_typ_alv_output WITH EMPTY KEY.

TYPES: BEGIN OF g_ty_used,
         progname TYPE susg_prog-progname,
         obj_type TYPE susg_prog-obj_type,
         obj_name TYPE susg_prog-obj_name,
       END OF g_ty_used.

TYPES: g_tt_used TYPE HASHED TABLE OF g_ty_used WITH UNIQUE KEY obj_type obj_name.

*--------------------------------------------------------------------*
* Class Defintions
*--------------------------------------------------------------------*
CLASS lcl_persistency_layer DEFINITION ##class_final.

  PUBLIC SECTION.
    METHODS:
      derive_all_objects  RETURNING VALUE(rt_alv_output)   TYPE g_tt_alv_output,
      derive_used_objects IMPORTING it_output              TYPE g_tt_alv_output
                          RETURNING VALUE(rt_used_objects) TYPE g_tt_used,
      text_name           IMPORTING id_obj_type           TYPE tadir-object
                                    id_obj_name           TYPE tadir-obj_name
                          RETURNING VALUE(rd_description) TYPE char80,
      is_an_include       IMPORTING id_obj_name         TYPE tadir-obj_name
                          RETURNING VALUE(rf_yes_it_is) TYPE abap_bool.

ENDCLASS.                    "lcl_persistency_layer DEFINITION

CLASS lcl_model DEFINITION  ##class_final.
  PUBLIC SECTION.
    DATA: mt_used              TYPE g_tt_used,
          mt_output            TYPE g_tt_alv_output,
          mo_persistency_layer TYPE REF TO lcl_persistency_layer.

    METHODS:
      constructor,
      derive_data,
      prepare_data_for_output.

ENDCLASS.

CLASS lcl_view DEFINITION  ##class_final.
  PUBLIC SECTION.
    METHODS: display IMPORTING it_output TYPE g_tt_alv_output.

ENDCLASS.

CLASS lcl_controller DEFINITION ##class_final.
  PUBLIC SECTION.
    CLASS-METHODS: main.

ENDCLASS.
*--------------------------------------------------------------------*
* Seelection Screen
*--------------------------------------------------------------------*
TABLES : tadir.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_devc FOR tadir-devclass OBLIGATORY.

SELECTION-SCREEN END OF BLOCK blk1.

*--------------------------------------------------------------------*
* Initialization
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_controller=>main( ).

*--------------------------------------------------------------------*
* Class Implementations
*--------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD derive_all_objects.
*--------------------------------------------------------------------*
* RETURNING VALUE(rt_alv_output)   TYPE g_tt_alv_output
*--------------------------------------------------------------------*
* Use Index DEV on TADIR on DEVCLASS
    SELECT object obj_name devclass ##too_many_itab_fields "in the world
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_alv_output
      WHERE devclass IN s_devc
      AND   object   IN ('PROG','FUGR','CLAS') "The only things SUSG tracks
      AND   delflag  EQ space
      ORDER BY PRIMARY KEY."#EC CI_GENBUFF

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD derive_used_objects.
*--------------------------------------------------------------------*
* IMPORTING it_output              TYPE g_tt_alv_output
* RETURNING VALUE(rt_used_objects) TYPE g_tt_used
*--------------------------------------------------------------------*
* Preconditions
    CHECK it_output[] IS NOT INITIAL.

* Use Index on PROGNAME - we want to exclude standard SAP ojects
    SELECT progname obj_type obj_name
      FROM susg_prog
      INTO CORRESPONDING FIELDS OF TABLE rt_used_objects
      FOR ALL ENTRIES IN it_output
      WHERE progname LIKE 'Z%'
      AND   obj_type EQ it_output-object
      AND   obj_name EQ it_output-obj_name."#EC CI_NOFIRST

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD text_name.
*--------------------------------------------------------------------*
* IMPORTING id_obj_type            TYPE tadir-object
*           id_obj_name            TYPE tadir-obj_name
* RETURNING VALUE(rd_description)  TYPE char80.
*--------------------------------------------------------------------*

    CASE id_obj_type.
      WHEN 'PROG'.
        SELECT SINGLE text FROM trdirt
        INTO  rd_description
        WHERE name  = id_obj_name
        AND   sprsl = sy-langu.
      WHEN 'FUGR'.
        SELECT SINGLE areat FROM tlibt
          INTO rd_description
          WHERE spras EQ sy-langu
          AND   area  EQ id_obj_name.
      WHEN 'CLAS'.
        SELECT SINGLE descript FROM seoclasstx
          INTO rd_description
          WHERE langu   EQ sy-langu
          AND   clsname EQ id_obj_name.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.

  METHOD is_an_include.
*--------------------------------------------------------------------*
* IMPORTING id_obj_name           TYPE tadir-obj_name
* RETURNING VALUE(rf_yes_it_is)   TYPE abap_bool
*--------------------------------------------------------------------*

    SELECT SINGLE subc FROM trdir
        INTO  @DATA(program_type)
        WHERE name  = @id_obj_name.

    IF sy-subrc EQ 0 AND program_type = 'I'.
      rf_yes_it_is = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_persistency_layer

CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT mo_persistency_layer.

  ENDMETHOD.

  METHOD derive_data.

    mt_output[] = mo_persistency_layer->derive_all_objects( ).
    mt_used[]   = mo_persistency_layer->derive_used_objects( mt_output ).

  ENDMETHOD.

  METHOD prepare_data_for_output.

    LOOP AT mt_output ASSIGNING FIELD-SYMBOL(<ls_output>).

      READ TABLE mt_used TRANSPORTING NO FIELDS
      WITH TABLE KEY obj_type = <ls_output>-object
                     obj_name = <ls_output>-obj_name.

      IF sy-subrc EQ 0.
        <ls_output>-used = icon_presence.
      ELSE.
        <ls_output>-used = icon_absence.
      ENDIF.

      <ls_output>-ddtext = mo_persistency_layer->text_name( id_obj_type = <ls_output>-object
                                                            id_obj_name = <ls_output>-obj_name ).

      "INCLUDES do not get picked up in SUSG just the mian program
      IF <ls_output>-object = 'PROG' AND
         mo_persistency_layer->is_an_include( <ls_output>-obj_name ) = abap_true.
        <ls_output>-ddtext = 'DELETE_ME'.
      ENDIF.

    ENDLOOP.

    DELETE mt_output WHERE ddtext = 'DELETE_ME'.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view IMPLEMENTATION.

  METHOD display.
*--------------------------------------------------------------------*
*   IMPORTING it_output TYPE g_tt_alv_output.
*--------------------------------------------------------------------*
    DATA(output_table) = it_output."Need copy due to CHANGING parameter

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_alv_grid)
          CHANGING
            t_table      = output_table ).
      CATCH cx_salv_msg INTO DATA(salv_error).
        DATA(error_message) = salv_error->get_text( ).
        MESSAGE error_message TYPE 'E'.
    ENDTRY.

    DATA(lo_columns) = lo_alv_grid->get_columns( ).
    lo_columns->set_optimize( if_salv_c_bool_sap=>true ).

    TRY.

        DATA(lo_column) = lo_columns->get_column( 'DDTEXT' ).
        lo_column->set_long_text( 'Object Description'(005) ).
        lo_column->set_short_text( 'Text Name'(004) ).

        lo_column = lo_columns->get_column( 'USED' ).
        lo_column->set_long_text( 'Used?'(003) ).

      CATCH cx_salv_not_found INTO DATA(not_found).
        error_message = |{ not_found->object } { not_found->key } { 'does not exist'(002) }|.
        MESSAGE error_message TYPE 'E'.
    ENDTRY.

    lo_alv_grid->display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_controller IMPLEMENTATION.

  METHOD main.

    DATA(lo_model) = NEW lcl_model( ).
    DATA(lo_view)  = NEW lcl_view( ).

    lo_model->derive_data( ).
    lo_model->prepare_data_for_output( ).
    lo_view->display( lo_model->mt_output ).

  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.

  s_devc[] = VALUE #( (
  option = 'CP'
  sign   = 'I'
  low    = 'Z*' ) ).

ENDFORM.
