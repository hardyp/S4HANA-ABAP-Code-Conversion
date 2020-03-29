*&---------------------------------------------------------------------*
*& Report Z_SWLT_ONE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SWLT_ONE.

START-OF-SELECTION.

 SELECT *
    FROM ztmonster_header
    INTO TABLE @DATA(all_monsters).

    DATA(neurotic_monsters) = VALUE ztt_monster_header(
     FOR monster_details IN all_monsters WHERE ( sanity_percentage < 20 )
         ( name           = monster_details-name
           monster_number = monster_details-monster_number ) ).
