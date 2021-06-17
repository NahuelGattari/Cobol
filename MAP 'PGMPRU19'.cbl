 000911          TITLE 'CONSULTA DATOS CLIENTE'
 000912 MAP0219  DFHMSD TYPE=&SYSPARM,MODE=INOUT,CTRL=(FREEKB,FRSET),          *
 000913                LANG=COBOL,TIOAPFX=YES,COLOR=BLUE
 000914 MAP0219  DFHMDI SIZE=(24,80)
 000915          DFHMDF POS=(1,7),LENGTH=13,INITIAL='MENU CLIENTES',           *
 000916                HILIGHT=UNDERLINE
 000917          DFHMDF POS=(1,21),LENGTH=1,ATTRB=PROT
 000918 MAPA     DFHMDF POS=(1,60),LENGTH=12,INITIAL='T219-MAP0219',           *
 000919                HILIGHT=UNDERLINE,ATTRB=PROT
 000920          DFHMDF POS=(1,73),LENGTH=1,ATTRB=PROT
 000921
 000922 FECHA    DFHMDF POS=(2,61),LENGTH=10,ATTRB=PROT
 000923
 000924          DFHMDF POS=(3,8),LENGTH=18,ATTRB=PROT,                        *
 000925                INITIAL='SELECCIONAR OPCION'
 000926
 000929          DFHMDF POS=(5,10),LENGTH=20,INITIAL='TIPO DE DOCUMENTO  :'
 000930 TIPDOC   DFHMDF POS=(5,31),LENGTH=2,ATTRB=(IC,UNPROT,FSET),PICIN='XX', *
 000931                COLOR=YELLOW,HILIGHT=UNDERLINE
 000932          DFHMDF POS=(5,34),LENGTH=1,ATTRB=(ASKIP,PROT)
 000933
 000934          DFHMDF POS=(7,10),LENGTH=20,INITIAL='NUMERO DE DOCUMENTO:'
 000935 NRODOC   DFHMDF POS=(7,31),LENGTH=11,ATTRB=(NUM,UNPROT,FSET),          *
 000936                PICIN='XXXXXXXXXXX',COLOR=YELLOW,HILIGHT=UNDERLINE
 000937          DFHMDF POS=(7,43),LENGTH=1,ATTRB=PROT
 000962
 000963 MSG      DFHMDF POS=(22,4),LENGTH=72,PICOUT='X(72)',COLOR=GREEN,       *
 000964                ATTRB=(FSET,PROT)
 000965          DFHMDF POS=(22,77),LENGTH=1,ATTRB=(ASKIP,PROT)
 000966
 000967          DFHMDF POS=(24,3),LENGTH=75,INITIAL='PF1:ALTA  PF2:BAJA  PF3:M*
 000968                ODIFICACION  PF4:CONSULTA  PF5:LIMPIAR  PF12:SALIR'
 000970          DFHMDF POS=(24,79),LENGTH=1,ATTRB=(ASKIP,PROT)
 000971
 000972          DFHMSD TYPE=FINAL
 000973          END
 ****** **************************** Bottom of Data ****************************