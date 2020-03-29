*&---------------------------------------------------------------------*
*& Report Z_SWLT_TWO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_swlt_two.

START-OF-SELECTION.
* Local Variables
  DATA: monster_number TYPE zde_monster_number VALUE '0000000001',
        monster_header TYPE ztmonster_header.

  "Remove leading zeroes before output to user
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = monster_number
    IMPORTING
      output = monster_number.

  DATA(message) = |Problem with monster number { monster_number }|.
  MESSAGE message TYPE 'I'.

  "Now add the leading zeroes back before database read
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = monster_number
    IMPORTING
      output = monster_number.

  SELECT SINGLE *
    FROM ztmonster_header
    INTO CORRESPONDING FIELDS OF monster_header
    WHERE monster_number = monster_number.
