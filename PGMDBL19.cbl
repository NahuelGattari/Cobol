 ****** ***************************** Top of Data ******************************
 000001        CBL TEST
 000002        IDENTIFICATION DIVISION.
 000003       *                                                        *
 000004        PROGRAM-ID PGMBL219.
 000005       **********************************************************
 000006       *                                                        *
 000007       *  PROGRAMA PARA SQL EMBEBIDO                            *
 000008       *                                                        *
 000009       **********************************************************
 000010       *      MANTENIMIENTO DE PROGRAMA                         *
 000011       **********************************************************
 000012       *  FECHA      *       DETALLE        *
 000013       **************************************
 000014       * 26/10/2020  *  PROGRAMA QUE LISTA  *
 000015       *             *  CADA CUENTA DEL     *
 000016       *             *      CLIENTE         *
 000017       **************************************
 000018        AUTHOR. NAHUEL GATTARI.
 000019        ENVIRONMENT DIVISION.
 000020        CONFIGURATION SECTION.
 000021        SPECIAL-NAMES.
 000022            DECIMAL-POINT IS COMMA.
 000023
 000024        INPUT-OUTPUT SECTION.
 000025        FILE-CONTROL.
 000026
 000027              SELECT SALIDA  ASSIGN SALIDA
 000028              FILE STATUS IS WS-CODE-SAL.
 000029
 000030        DATA DIVISION.
 000031        FILE SECTION.
 000032        FD SALIDA
 000033              BLOCK CONTAINS 0 RECORDS
 000034              RECORDING MODE IS F.
 000035
 000036        01 REG-SALIDA      PIC X(132).
 000037
 000038       **************************************
 000039        WORKING-STORAGE SECTION.
 000040       **************************************
 000041        77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
 000042
 000043        77  WS-CODE-SAL      PIC XX        VALUE SPACES.
 000044        77  FS-SQLCODE       PIC -999      VALUE ZEROS.
 000045
 000046       *********************
 000047       *    VARIABLES      *
 000048       *********************
 000049
 000050        77  WS-19000         PIC S9(05)V   USAGE COMP-3 VALUE 19000.
 000051        77  WS-19100         PIC S9(05)V   USAGE COMP-3 VALUE 19100.
 000052        77  WS-LINEA         PIC 9(02)     VALUE ZEROS.
 000053        77  WS-NRO-CLI       PIC S9(05)V   USAGE COMP-3.
 000054        77  WS-NRO-ALU       PIC 9(05)     VALUE ZEROS.
 000055        77  WS-ALU-IMP       PIC 9(05)     VALUE ZEROS.
 000056
 000057       ********************
 000058       *      FLAGS       *
 000059       ********************
 000060
 000061        01 WS-STATUS-FIN            PIC X  VALUE SPACES.
 000062           88  WS-FIN-LECTURA                  VALUE 'Y'.
 000063           88  WS-NO-FIN-LECTURA               VALUE 'N'.
 000064
 000065        01 WS-FLAG-CUENTA           PIC X   VALUE SPACES.
 000066           88 WS-FLAG-CUENTA-ON                VALUE 'Y'.
 000067           88 WS-FLAG-CUENTA-OFF               VALUE 'N'.
 000068
 000069       **************************
 000070       *         FECHA          *
 000071       **************************
 000072
 000073        01  WS-FECHA.
 000074            05  WS-FECHA-AA          PIC 9(04)    VALUE ZEROS.
 000075            05  WS-FECHA-MM          PIC 9(02)    VALUE ZEROS.
 000076            05  WS-FECHA-DD          PIC 9(02)    VALUE ZEROS.
 000077
 000078        01  WS-FECHA-IMP.
 000079            05  WS-FECHA-IMP-AA      PIC 9(04)    VALUE ZEROS.
 000080            05  WS-SEP1              PIC X(01)    VALUE '-'.
 000081            05  WS-FECHA-IMP-MM      PIC 9(02)    VALUE ZEROS.
 000082            05  WS-SEP2              PIC X(01)    VALUE '-'.
 000083            05  WS-FECHA-IMP-DD      PIC 9(02)    VALUE ZEROS.
 000084
 000085       ***********************************
 000086       *          CONTADORES             *
 000087       ***********************************
 000088
 000089        01  WS-CONT-ERRONEAS         PIC 9(03)    VALUE ZEROS.
 000090        01  WS-CONT-ENCONTRADAS      PIC 9(03)    VALUE ZEROS.
 000091        01  WS-CONT-NOENCONTRADAS    PIC 9(03)    VALUE ZEROS.
 000092        01  WS-CONT-GRABADOS         PIC 9(03)    VALUE ZEROS.
 000093        01  WS-CONT-CUENT            PIC 9(05)    VALUE ZEROS.
 000094        01  WS-CONT-PAGINA           PIC 9(02)    VALUE 01.
 000095
 000096       *******************************
 000097       *       LAYOUT CLIENTE        *
 000098       *******************************
 000099
 000100        01 WS-REG-CLIENTE.
 000101
 000102            05  FILLER              PIC X(08)    VALUE SPACES.
 000103            05  WS-CLI-TIP-DOC      PIC X(02)    VALUE SPACES.
 000104            05  FILLER              PIC X(09)    VALUE SPACES.
 000105            05  FILLER              PIC X(01)    VALUE '-'.
 000106            05  FILLER              PIC X(05)    VALUE SPACES.
 000107            05  WS-CLI-NRO-DOC      PIC X(11)    VALUE SPACES.
 000108            05  FILLER              PIC X(04)    VALUE SPACES.
 000109            05  FILLER              PIC X(01)    VALUE '-'.
 000110            05  FILLER              PIC X(05)    VALUE SPACES.
 000111            05  WS-CLI-NRO-CLIE     PIC X(05)    VALUE SPACES.
 000112            05  FILLER              PIC X(06)    VALUE SPACES.
 000113            05  FILLER              PIC X(01)    VALUE '-'.
 000114            05  FILLER              PIC X(05)    VALUE SPACES.
 000115            05  WS-CLI-NOMBRE       PIC X(30)    VALUE SPACES.
 000116            05  FILLER              PIC X(01)    VALUE SPACES.
 000117            05  FILLER              PIC X(01)    VALUE '-'.
 000118            05  FILLER              PIC X(05)    VALUE SPACES.
 000119            05  WS-CLI-APELLIDO     PIC X(30)    VALUE SPACES.
 000120            05  FILLER              PIC X(01)    VALUE SPACES.
 000121            05  FILLER              PIC X(01)    VALUE '-'.
 000122            05  FILLER              PIC X(05)    VALUE SPACES.
 000123            05  WS-CLI-FECHA-ALTA   PIC X(10)    VALUE SPACES.
 000124            05  FILLER              PIC X(01)    VALUE SPACES.
 000125            05  FILLER              PIC X(01)    VALUE '-'.
 000126            05  FILLER              PIC X(05)    VALUE SPACES.
 000127            05  WS-CLI-FECHA-BAJA   PIC X(10)    VALUE SPACES.
 000128            05  FILLER              PIC X(05)    VALUE SPACES.
 000129
 000130       *******************************
 000131       *       LAYOUT CUENTA         *
 000132       *******************************
 000133
 000134        01  WS-REG-CUENTA.
 000135
 000136            05  FILLER              PIC X(07)    VALUE SPACES.
 000137            05  WS-CUE-TIP-CUENTA   PIC X(02)    VALUE SPACES.
 000138            05  FILLER              PIC X(07)    VALUE SPACES.
 000139            05  FILLER              PIC X(01)    VALUE '|'.
 000140            05  FILLER              PIC X(01)    VALUE SPACES.
 000141            05  WS-CUE-NRO-CUENTA   PIC X(15)    VALUE SPACES.
 000142            05  FILLER              PIC X(01)    VALUE SPACES.
 000143            05  FILLER              PIC X(01)    VALUE '|'.
 000144            05  FILLER              PIC X(03)    VALUE SPACES.
 000145            05  WS-CUE-MONEDA       PIC X(02)    VALUE SPACES.
 000146            05  FILLER              PIC X(03)    VALUE SPACES.
 000147            05  FILLER              PIC X(01)    VALUE '|'.
 000148            05  FILLER              PIC X(01)    VALUE SPACES.
 000149            05  WS-CUE-CBU          PIC X(11)    VALUE SPACES.
 000150            05  FILLER              PIC X(01)    VALUE SPACES.
 000151            05  FILLER              PIC X(01)    VALUE '|'.
 000152            05  FILLER              PIC X(5)     VALUE SPACES.
 000153            05  WS-CUE-NRO-CLIE     PIC X(05)    VALUE SPACES.
 000154            05  FILLER              PIC X(01)    VALUE SPACES.
 000155            05  FILLER              PIC X(01)    VALUE '|'.
 000156            05  FILLER              PIC X(01)    VALUE SPACES.
 000157            05  WS-CUE-SALDO-ACT    PIC -ZZZ.ZZZ.999,99 VALUE ZEROS.
 000158            05  FILLER              PIC X(01)    VALUE SPACES.
 000159            05  FILLER              PIC X(01)    VALUE '|'.
 000160            05  FILLER              PIC X(02)    VALUE SPACES.
 000161            05  WS-CUE-FECHA-ACT    PIC X(10)    VALUE SPACES.
 000162            05  FILLER              PIC X(02)    VALUE SPACES.
 000163            05  FILLER              PIC X(01)    VALUE '|'.
 000164            05  FILLER              PIC X(05)    VALUE SPACES.
 000165            05  WS-CUE-FECHA-ULT    PIC X(10)    VALUE SPACES.
 000166            05  FILLER              PIC X(06)    VALUE SPACES.
 000167            05  FILLER              PIC X(01)    VALUE '|'.
 000168
 000169       ***********************************
 000170       *       TITULO Y SUBTITULO        *
 000171       ***********************************
 000172
 000173        01  WS-TITULO.
 000174            03  FILLER              PIC X(20)    VALUE  SPACES.
 000175            03  FILLER              PIC X(29)    VALUE
 000176             'LISTADO DE CLIENTES Y CUENTAS'.
 000177            03  FILLER              PIC X(05)    VALUE SPACES.
 000178            03  FILLER              PIC X(07)    VALUE 'FECHA: '.
 000179            03  WS-FECHA-HOY        PIC X(10)    VALUE SPACES.
 000180            03  FILLER              PIC X(05)    VALUE SPACES.
 000181            03  FILLER              PIC X(05)    VALUE 'ALU: '.
 000182            03  WS-ALU-IMPR         PIC X(05).
 000183            03  FILLER              PIC X(05)    VALUE SPACES.
 000184            03  FILLER              PIC X(16)    VALUE
 000185                   'NUMERO PAGINA: '.
 000186            03  WS-PAGINA           PIC Z9       VALUE ZEROS.
 000187            03  FILLER              PIC X(24)    VALUE SPACES.
 000188
 000189        01 WS-SUBTITULO.
 000190            05  WS-CORTA-LINEA.
 000191                10  FILLER    PIC X(54)    VALUE
 000192                '------------------------------------------------------'.
 000193                10  FILLER    PIC X(54)    VALUE
 000194                '------------------------------------------------------'.
 000195            05  WS-TIT-CLIENTE.
 000196                10  FILLER    PIC X(38)    VALUE SPACES.
 000197                10  FILLER    PIC X(07)    VALUE 'CLIENTE'.
 000198            05  WS-TIT-CUENTA.
 000199                10  FILLER    PIC X(38)    VALUE SPACES.
 000200                10  FILLER    PIC X(07)    VALUE 'CUENTAS'.
 000201            05  WS-NO-CUENTA.
 000202                10  FILLER    PIC X(34)    VALUE SPACES.
 000203                10  FILLER    PIC X(31)    VALUE
 000204                       'EL CLIENTE NO POSEE UNA CUENTA'.
 000205            05  WS-SUB-CLIENTE.
 000206                10  FILLER    PIC X(02)    VALUE SPACES.
 000207                10  FILLER    PIC X(17)    VALUE 'TIPO DE DOCUMENTO'.
 000208                10  FILLER    PIC X(02)    VALUE SPACES.
 000209                10  FILLER    PIC X(01)    VALUE  '|'.
 000210                10  FILLER    PIC X(02)    VALUE SPACES.
 000211                10  FILLER    PIC X(16)    VALUE 'NRO DE DOCUMENTO'.
 000212                10  FILLER    PIC X(02)    VALUE SPACES.
 000213                10  FILLER    PIC X(01)    VALUE  '|'.
 000214                10  FILLER    PIC X(02)    VALUE SPACES.
 000215                10  FILLER    PIC X(14)    VALUE 'NRO DE CLIENTE'.
 000216                10  FILLER    PIC X(02)    VALUE SPACES.
 000217                10  FILLER    PIC X(01)    VALUE  '|'.
 000218                10  FILLER    PIC X(14)    VALUE SPACES.
 000219                10  FILLER    PIC X(06)    VALUE 'NOMBRE'.
 000220                10  FILLER    PIC X(14)    VALUE SPACES.
 000221                10  FILLER    PIC X(01)    VALUE  '|'.
 000222                10  FILLER    PIC X(13)    VALUE SPACES.
 000223                10  FILLER    PIC X(08)    VALUE 'APELLIDO'.
 000224                10  FILLER    PIC X(13)    VALUE SPACES.
 000225                10  FILLER    PIC X(01)    VALUE  '|'.
 000226                10  FILLER    PIC X(02)    VALUE SPACES.
 000227                10  FILLER    PIC X(13)    VALUE 'FECHA DE ALTA'.
 000228                10  FILLER    PIC X(02)    VALUE SPACES.
 000229                10  FILLER    PIC X(01)    VALUE  '|'.
 000230                10  FILLER    PIC X(02)    VALUE SPACES.
 000231                10  FILLER    PIC X(13)    VALUE 'FECHA DE BAJA'.
 000232                10  FILLER    PIC X(02)    VALUE SPACES.
 000233            05 WS-SUB-CUENTA.
 000234                10  FILLER    PIC X(02)    VALUE SPACES.
 000235                10  FILLER    PIC X(14)    VALUE 'TIPO DE CUENTA'.
 000236                10  FILLER    PIC X(02)    VALUE SPACES.
 000237                10  FILLER    PIC X(01)    VALUE  '|'.
 000238                10  FILLER    PIC X(03)    VALUE SPACES.
 000239                10  FILLER    PIC X(13)    VALUE 'NRO DE CUENTA'.
 000240                10  FILLER    PIC X(03)    VALUE SPACES.
 000241                10  FILLER    PIC X(01)    VALUE  '|'.
 000242                10  FILLER    PIC X(02)    VALUE SPACES.
 000243                10  FILLER    PIC X(06)    VALUE 'MONEDA'.
 000244                10  FILLER    PIC X(02)    VALUE SPACES.
 000245                10  FILLER    PIC X(01)    VALUE  '|'.
 000246                10  FILLER    PIC X(06)    VALUE SPACES.
 000247                10  FILLER    PIC X(03)    VALUE 'CBU'.
 000248                10  FILLER    PIC X(06)    VALUE SPACES.
 000249                10  FILLER    PIC X(01)    VALUE  '|'.
 000250                10  FILLER    PIC X(02)    VALUE SPACES.
 000251                10  FILLER    PIC X(14)    VALUE 'NRO DE CLIENTE'.
 000252                10  FILLER    PIC X(02)    VALUE SPACES.
 000253                10  FILLER    PIC X(01)    VALUE  '|'.
 000254                10  FILLER    PIC X(04)    VALUE SPACES.
 000255                10  FILLER    PIC X(12)    VALUE 'SALDO ACTUAL'.
 000256                10  FILLER    PIC X(05)    VALUE SPACES.
 000257                10  FILLER    PIC X(01)    VALUE  '|'.
 000258                10  FILLER    PIC X(02)    VALUE SPACES.
 000259                10  FILLER    PIC X(12)    VALUE 'FECHA ACTUAL'.
 000260                10  FILLER    PIC X(02)    VALUE SPACES.
 000261                10  FILLER    PIC X(01)    VALUE  '|'.
 000262                10  FILLER    PIC X(02)    VALUE SPACES.
 000263                10  FILLER    PIC X(19)    VALUE 'FECHA ULTIMO CIERRE'.
 000264                10  FILLER    PIC X(02)    VALUE SPACES.
 000265                10  FILLER    PIC X(01)    VALUE  '|'.
 000266
 000267        77          FILLER    PIC X(26) VALUE '* VARIABLES SQL     *'.
 000268
 000269       **************************************
 000270       *     AREA DE COMUNICACION DB2       *
 000271       **************************************
 000272
 000273             EXEC SQL
 000274               INCLUDE SQLCA
 000275             END-EXEC.
 000276
 000277             EXEC SQL
 000278               INCLUDE TBCUE
 000279             END-EXEC.
 000280
 000281             EXEC SQL
 000282               INCLUDE TBCLI
 000283             END-EXEC.
 000284
 000285             EXEC SQL
 000286               DECLARE CUENTA_CURSOR CURSOR
 000287               FOR
 000288               SELECT TIPO_CUENTA,
 000289                      NRO_CUENTA,
 000290                      MONEDA,
 000291                      CBU,
 000292                      NRO_CLIENTE,
 000293                      SALDO_ACTUAL,
 000294                      FECHA_ACTUAL,
 000295                      FECHA_ULTIMO_CIERRE
 000296
 000297               FROM  ITPFBIO.TBCUENTAS
 000298
 000299               WHERE NRO_CLIENTE = :WS-NRO-CLI
 000300
 000301             END-EXEC.
 000302
 000303             EXEC SQL
 000304               DECLARE CLIENTE_CURSOR CURSOR
 000305               FOR
 000306               SELECT TIPO_DOCUMENTO,
 000307                      NRO_DOCUMENTO,
 000308                      NRO_CLIENTE,
 000309                      NOMBRE_CLIENTE,
 000310                      APELLIDO_CLIENTE,
 000311                      DOMICILIO,
 000312                      CIUDAD,
 000313                      CODIGO_POSTAL,
 000314                      NACIONALIDAD,
 000315                      FECHA_DE_ALTA,
 000316                      FECHA_DE_BAJA,
 000317                      ESTADO_CIVIL,
 000318                      SEXO,
 000319                      CORREO_ELECTRONICO,
 000320                      FECCHA_NACIMIENTO
 000321
 000322               FROM  ITPFBIO.TBCLIENT
 000323
 000324               WHERE NRO_CLIENTE > :WS-19000 AND
 000325                     NRO_CLIENTE < :WS-19100
 000326
 000327             END-EXEC.
 000328
 000329        77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.
 000330
 000331       ***************************************************************.
 000332        PROCEDURE DIVISION.
 000333       **************************************
 000334       *                                    *
 000335       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000336       *                                    *
 000337       **************************************
 000338        MAIN-PROGRAM.
 000339
 000340            PERFORM 1000-I-INICIO  THRU 1000-F-INICIO.
 000341
 000342            PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO
 000343                                   UNTIL WS-NO-FIN-LECTURA.
 000344
 000345            PERFORM 9999-I-FINAL   THRU 9999-F-FINAL.
 000346
 000347        F-MAIN-PROGRAM. GOBACK.
 000348
 000349       **************************************
 000350       *                                    *
 000351       *  CUERPO INICIO APERTURA ARCHIVOS   *
 000352       *                                    *
 000353       **************************************
 000354        1000-I-INICIO.
 000355
 000356            ACCEPT WS-FECHA FROM DATE YYYYMMDD.
 000357            ACCEPT WS-NRO-ALU FROM SYSIN.
 000358
 000359            MOVE WS-NRO-ALU    TO WS-ALU-IMP.
 000360            MOVE WS-ALU-IMP    TO WS-ALU-IMPR.
 000361
 000362            MOVE WS-FECHA-AA   TO WS-FECHA-IMP-AA.
 000363            MOVE WS-FECHA-MM   TO WS-FECHA-IMP-MM.
 000364            MOVE WS-FECHA-DD   TO WS-FECHA-IMP-DD.
 000365
 000366            MOVE 61            TO  WS-LINEA.
 000367
 000368            SET WS-FIN-LECTURA TO TRUE.
 000369
 000370            OPEN OUTPUT SALIDA.
 000371
 000372            MOVE SQLCODE TO FS-SQLCODE
 000373
 000374            IF WS-CODE-SAL    IS NOT EQUAL '00'
 000375               DISPLAY '* ERROR EN OPEN SALIDA  = ' WS-CODE-SAL
 000376               MOVE 9999 TO RETURN-CODE
 000377               SET  WS-NO-FIN-LECTURA TO TRUE
 000378            END-IF.
 000379
 000380            PERFORM  5500-I-IMPRIMIR     THRU 5500-F-IMPRIMIR.
 000381            PERFORM  2100-I-OPEN-CLIENTE THRU 2100-F-OPEN-CLIENTE.
 000382            PERFORM  2500-I-LEER-CLIENTE THRU 2500-F-LEER-CLIENTE.
 000383
 000384        1000-F-INICIO.   EXIT.
 000385
 000386       **************************************
 000387       *                                    *
 000388       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000389       *                                    *
 000390       **************************************
 000391        2000-I-PROCESO.
 000392
 000393            PERFORM  2200-I-OPEN-CUENTA   THRU 2200-F-OPEN-CUENTA.
 000394
 000395            PERFORM  7000-I-TIT-CUE       THRU 7000-F-TIT-CUE.
 000396
 000397            PERFORM  3000-I-LEER-CUENTA   THRU 3000-F-LEER-CUENTA
 000398                                    UNTIL  WS-FLAG-CUENTA-OFF.
 000399
 000400            PERFORM  7500-I-CERRAR-CUENTA THRU 7500-F-CERRAR-CUENTA.
 000401
 000402            PERFORM  2500-I-LEER-CLIENTE  THRU 2500-F-LEER-CLIENTE.
 000403
 000404            SET WS-FLAG-CUENTA-ON TO TRUE.
 000405
 000406        2000-F-PROCESO. EXIT.
 000407
 000408        2100-I-OPEN-CLIENTE.
 000409
 000410            EXEC SQL
 000411               OPEN CLIENTE_CURSOR
 000412            END-EXEC.
 000413
 000414            MOVE SQLCODE TO FS-SQLCODE
 000415
 000416            IF SQLCODE NOT EQUAL ZEROS
 000417               DISPLAY '* ERROR OPEN CLIENTE_CURSOR = ' FS-SQLCODE
 000418               MOVE 9999 TO RETURN-CODE
 000419               SET  WS-NO-FIN-LECTURA TO TRUE
 000420            END-IF.
 000421
 000422        2100-F-OPEN-CLIENTE. EXIT.
 000423
 000424        2200-I-OPEN-CUENTA.
 000425
 000426            EXEC SQL
 000427               OPEN CUENTA_CURSOR
 000428            END-EXEC.
 000429
 000430            MOVE SQLCODE TO FS-SQLCODE
 000431
 000432            IF SQLCODE NOT EQUAL ZEROS
 000433               DISPLAY '* ERROR OPEN CUENTA_CURSOR  = ' FS-SQLCODE
 000434               MOVE 9999 TO RETURN-CODE
 000435               SET  WS-NO-FIN-LECTURA TO TRUE
 000436            ELSE
 000437               INITIALIZE WS-CONT-CUENT
 000438            END-IF.
 000439
 000440        2200-F-OPEN-CUENTA. EXIT.
 000441
 000442        2500-I-LEER-CLIENTE.
 000443
 000444            EXEC SQL
 000445               FETCH  CLIENTE_CURSOR
 000446                      INTO
 000447                        :DCLTBCLIENT.WS-TIPO-DOCUMENTO,
 000448                        :DCLTBCLIENT.WS-NRO-DOCUMENTO,
 000449                        :DCLTBCLIENT.WS-NRO-CLIENTE,
 000450                        :DCLTBCLIENT.WS-NOMBRE-CLIENTE,
 000451                        :DCLTBCLIENT.WS-APELLIDO-CLIENTE,
 000452                        :DCLTBCLIENT.WS-DOMICILIO,
 000453                        :DCLTBCLIENT.WS-CIUDAD,
 000454                        :DCLTBCLIENT.WS-CODIGO-POSTAL,
 000455                        :DCLTBCLIENT.WS-NACIONALIDAD,
 000456                        :DCLTBCLIENT.WS-FECHA-DE-ALTA,
 000457                        :DCLTBCLIENT.WS-FECHA-DE-BAJA,
 000458                        :DCLTBCLIENT.WS-ESTADO-CIVIL,
 000459                        :DCLTBCLIENT.WS-SEXO,
 000460                        :DCLTBCLIENT.WS-CORREO-ELECTRONICO,
 000461                        :DCLTBCLIENT.WS-FECCHA-NACIMIENTO
 000462            END-EXEC.
 000463
 000464            MOVE SQLCODE TO FS-SQLCODE
 000465
 000466            EVALUATE SQLCODE
 000467
 000468              WHEN ZEROS
 000469               MOVE WS-NRO-CLIENTE TO WS-NRO-CLI
 000470               PERFORM 6000-I-TIT-CLIE THRU 6000-F-TIT-CLIE
 000471               PERFORM 6500-I-PRE-GRABAR-CLIE THRU 6500-F-PRE-GRABAR-CLIE
 000472               ADD 1 TO WS-CONT-ENCONTRADAS
 000473
 000474              WHEN +100
 000475               ADD 1 TO WS-CONT-ERRONEAS
 000476               SET WS-NO-FIN-LECTURA TO TRUE
 000477
 000478              WHEN OTHER
 000479               DISPLAY 'ERROR FETCH CLIENTE_CURSOR: '  FS-SQLCODE
 000480               SET WS-NO-FIN-LECTURA TO TRUE
 000481            END-EVALUATE.
 000482
 000483        2500-F-LEER-CLIENTE. EXIT.
 000484
 000485        3000-I-LEER-CUENTA.
 000486
 000487            EXEC SQL
 000488               FETCH  CUENTA_CURSOR
 000489                      INTO
 000490                        :DCLTBCUENTAS.WS-TIPO-CUENTA,
 000491                        :DCLTBCUENTAS.WS-NRO-CUENTA,
 000492                        :DCLTBCUENTAS.WS-MONEDA,
 000493                        :DCLTBCUENTAS.WS-CBU,
 000494                        :DCLTBCUENTAS.WS-CUE-NRO-CLIENTE,
 000495                        :DCLTBCUENTAS.WS-SALDO-ACTUAL,
 000496                        :DCLTBCUENTAS.WS-FECHA-ACTUAL,
 000497                        :DCLTBCUENTAS.WS-FECHA-ULTIMO-CIERRE
 000498
 000499            END-EXEC.
 000500
 000501            MOVE SQLCODE TO FS-SQLCODE
 000502
 000503            EVALUATE SQLCODE
 000504
 000505              WHEN ZEROS
 000506
 000507               PERFORM 5000-I-GRABAR-CUENTA THRU 5000-I-GRABAR-CUENTA
 000508               ADD 1 TO WS-CONT-ENCONTRADAS
 000509               ADD 1 TO WS-CONT-CUENT
 000510
 000511              WHEN +100
 000512
 000513               SET WS-FLAG-CUENTA-OFF TO TRUE
 000514
 000515               IF WS-CONT-CUENT EQUAL TO ZERO
 000516
 000517                WRITE REG-SALIDA FROM WS-NO-CUENTA
 000518
 000519                IF WS-CODE-SAL    IS NOT EQUAL '00'
 000520                  DISPLAY '* ERROR EN WRITE CLIENTE = ' WS-CODE-SAL
 000521                  MOVE 9999 TO RETURN-CODE
 000522                  SET  WS-NO-FIN-LECTURA TO TRUE
 000523                ELSE
 000524                  ADD  1 TO WS-CONT-PAGINA
 000525                END-IF
 000526               END-IF
 000527
 000528              WHEN OTHER
 000529               DISPLAY 'ERROR FETCH CUENTA_CURSOR: '  FS-SQLCODE
 000530               SET WS-NO-FIN-LECTURA TO TRUE
 000531            END-EVALUATE.
 000532
 000533        3000-F-LEER-CUENTA. EXIT.
 000534
 000535        3500-I-MOVER-CLIENTE.
 000536
 000537            MOVE WS-TIPO-DOCUMENTO         TO WS-CLI-TIP-DOC
 000538
 000539            MOVE WS-NRO-DOCUMENTO          TO WS-CLI-NRO-DOC
 000540
 000541            MOVE WS-NRO-CLIENTE            TO WS-CLI-NRO-CLIE
 000542
 000543            MOVE WS-NOMBRE-CLIENTE         TO WS-CLI-NOMBRE
 000544
 000545            MOVE WS-APELLIDO-CLIENTE       TO WS-CLI-APELLIDO
 000546
 000547            MOVE WS-FECHA-DE-ALTA          TO WS-CLI-FECHA-ALTA
 000548
 000549            MOVE WS-FECHA-DE-BAJA          TO WS-CLI-FECHA-BAJA.
 000550
 000551        3500-F-MOVER-CLIENTE. EXIT.
 000552
 000553        4000-I-MOVER-CUENTA.
 000554
 000555            MOVE WS-TIPO-CUENTA            TO WS-CUE-TIP-CUENTA
 000556
 000557            MOVE WS-NRO-CUENTA             TO WS-CUE-NRO-CUENTA
 000558
 000559            MOVE WS-MONEDA                 TO WS-CUE-MONEDA
 000560
 000561            MOVE WS-CBU                    TO WS-CUE-CBU
 000562
 000563            MOVE WS-CUE-NRO-CLIENTE        TO WS-CUE-NRO-CLIE
 000564
 000565            MOVE WS-SALDO-ACTUAL           TO WS-CUE-SALDO-ACT
 000566
 000567            MOVE WS-FECHA-ACTUAL           TO WS-CUE-FECHA-ACT
 000568
 000569            MOVE WS-FECHA-ULTIMO-CIERRE    TO WS-CUE-FECHA-ULT.
 000570
 000571        4000-F-MOVER-CUENTA. EXIT.
 000572
 000573       ************************
 000574       *    GRABAR SALIDA     *
 000575       ************************
 000576
 000577        4500-I-GRABAR-CLIENTE.
 000578
 000579            IF WS-LINEA GREATER 60
 000580
 000581              PERFORM 5500-I-IMPRIMIR THRU 5500-F-IMPRIMIR
 000582
 000583            END-IF.
 000584
 000585            PERFORM 3500-I-MOVER-CLIENTE THRU 3500-F-MOVER-CLIENTE
 000586
 000587            WRITE REG-SALIDA     FROM WS-REG-CLIENTE.
 000588            ADD  1      TO  WS-CONT-GRABADOS.
 000589
 000590            IF WS-CODE-SAL    IS NOT EQUAL '00'
 000591              DISPLAY '* ERROR EN WRITE CLIENTE = ' WS-CODE-SAL
 000592              MOVE 9999 TO RETURN-CODE
 000593              SET  WS-NO-FIN-LECTURA TO TRUE
 000594            END-IF.
 000595
 000596        4500-F-GRABAR-CLIENTE. EXIT.
 000597
 000598        5000-I-GRABAR-CUENTA.
 000599
 000600            IF WS-LINEA GREATER 60
 000601
 000602                PERFORM 5500-I-IMPRIMIR THRU 5500-F-IMPRIMIR
 000603
 000604            END-IF.
 000605
 000606            PERFORM 4000-I-MOVER-CUENTA THRU 4000-F-MOVER-CUENTA.
 000607
 000608            WRITE REG-SALIDA     FROM WS-REG-CUENTA.
 000609
 000610            ADD 1  TO  WS-CONT-GRABADOS.
 000611            ADD 1  TO  WS-CONT-PAGINA.
 000612
 000613            IF WS-CODE-SAL IS NOT EQUAL '00'
 000614              DISPLAY '* ERROR EN WRITE CUENTA = ' WS-CODE-SAL
 000615              MOVE 9999 TO RETURN-CODE
 000616              SET  WS-NO-FIN-LECTURA TO TRUE
 000617            END-IF.
 000618
 000619        5000-F-GRABAR-CUENTA.  EXIT.
 000620
 000621        5500-I-IMPRIMIR.
 000622
 000623             MOVE WS-CONT-PAGINA TO WS-PAGINA.
 000624             MOVE WS-FECHA-IMP TO WS-FECHA-HOY.
 000625             MOVE 1 TO WS-LINEA.
 000626             ADD  1 TO WS-CONT-PAGINA.
 000627
 000628             WRITE REG-SALIDA FROM WS-TITULO AFTER PAGE.
 000629
 000630             IF WS-CODE-SAL IS NOT EQUAL '00'
 000631               DISPLAY '* ERROR EN WRITE SALIDA  = ' WS-CODE-SAL
 000632               MOVE 9999 TO RETURN-CODE
 000633               SET  WS-NO-FIN-LECTURA TO TRUE
 000634             END-IF.
 000635
 000636        5500-F-IMPRIMIR. EXIT.
 000637
 000638        6000-I-TIT-CLIE.
 000639
 000640            WRITE REG-SALIDA FROM WS-CORTA-LINEA
 000641
 000642            IF WS-CODE-SAL IS NOT EQUAL '00'
 000643              DISPLAY '* ERROR EN GRABAR SEPARADOR LINEA = ' WS-CODE-SAL
 000644              MOVE 9999 TO RETURN-CODE
 000645              SET  WS-NO-FIN-LECTURA TO TRUE
 000646            ELSE
 000647              ADD 1 TO WS-CONT-PAGINA
 000648            END-IF
 000649
 000650            WRITE REG-SALIDA FROM WS-TIT-CLIENTE
 000651
 000652            IF WS-CODE-SAL IS NOT EQUAL '00'
 000653             DISPLAY '* ERROR EN GRABAR SEPARADOR CLIENTE = ' WS-CODE-SAL
 000654             MOVE 9999 TO RETURN-CODE
 000655             SET  WS-NO-FIN-LECTURA TO TRUE
 000656            ELSE
 000657              ADD  1 TO WS-CONT-PAGINA
 000658            END-IF.
 000659
 000660        6000-F-TIT-CLIE. EXIT.
 000661
 000662        6500-I-PRE-GRABAR-CLIE.
 000663
 000664            WRITE REG-SALIDA FROM WS-SUB-CLIENTE
 000665
 000666            IF WS-CODE-SAL IS NOT EQUAL '00'
 000667              DISPLAY '* ERROR EN GRABAR CLIENTE = ' WS-CODE-SAL
 000668              MOVE 9999 TO RETURN-CODE
 000669              SET  WS-NO-FIN-LECTURA TO TRUE
 000670            ELSE
 000671              ADD  1 TO WS-CONT-PAGINA
 000672              PERFORM  4500-I-GRABAR-CLIENTE THRU 4500-F-GRABAR-CLIENTE
 000673            END-IF.
 000674     
 000675        6500-F-PRE-GRABAR-CLIE. EXIT.        
 000676
 000677        7000-I-TIT-CUE.
 000678
 000679            WRITE REG-SALIDA FROM WS-TIT-CUENTA
 000680
 000681            IF WS-CODE-SAL IS NOT EQUAL '00'
 000682              DISPLAY '* ERROR EN GRABAR SEPARADOR CUENTA = ' WS-CODE-SAL
 000683              MOVE 9999 TO RETURN-CODE
 000684              SET  WS-NO-FIN-LECTURA TO TRUE
 000685            ELSE
 000686              ADD 1 TO WS-CONT-PAGINA
 000687            END-IF.
 000688
 000689        7000-F-TIT-CUE. EXIT.
 000690
 000691       **************************************
 000692       *                                    *
 000693       *  CUERPO FINAL CIERRE DE FILES      *
 000694       *                                    *
 000695       **************************************
 000696
 000697        7500-I-CERRAR-CUENTA.
 000698
 000699            EXEC SQL
 000700               CLOSE CUENTA_CURSOR
 000701            END-EXEC.
 000702
 000703            MOVE SQLCODE TO FS-SQLCODE
 000704
 000705            IF SQLCODE NOT EQUAL ZEROS
 000706               DISPLAY '* ERROR CLOSE CUENTA_CURSOR      = ' FS-SQLCODE
 000707               MOVE 9999 TO RETURN-CODE
 000708            END-IF.
 000709
 000710        7500-F-CERRAR-CUENTA. EXIT.
 000711
 000712        8000-I-CERRAR-CLIENTE.
 000713
 000714            EXEC SQL
 000715               CLOSE CLIENTE_CURSOR
 000716            END-EXEC.
 000717
 000718            MOVE SQLCODE TO FS-SQLCODE
 000719
 000720            IF SQLCODE NOT EQUAL ZEROS
 000721               DISPLAY '* ERROR CLOSE CLIENTE_CURSOR     = ' FS-SQLCODE
 000722               MOVE 9999 TO RETURN-CODE
 000723            END-IF.
 000724
 000725        8000-F-CERRAR-CLIENTE. EXIT.
 000726
 000727        9999-I-FINAL.
 000728
 000729            PERFORM 8000-I-CERRAR-CLIENTE THRU 8000-F-CERRAR-CLIENTE.
 000730
 000731            CLOSE SALIDA
 000732               IF WS-CODE-SAL  IS NOT EQUAL '00'
 000733                 DISPLAY '* ERROR EN CLOSE SALIDA  = ' WS-CODE-SAL
 000734                 MOVE 9999 TO RETURN-CODE
 000735              END-IF.
 000736
 000737       **********************************
 000738       *   MOSTRAR TOTALES DE CONTROL   *
 000739       **********************************
 000740
 000741            DISPLAY '  '.
 000742            DISPLAY 'NOVEDADES ENCONTRADAS: '     WS-CONT-ENCONTRADAS.
 000743            DISPLAY 'NOVEDADES NO ENCONTRADAS: '  WS-CONT-NOENCONTRADAS.
 000744            DISPLAY 'NOVEDADES ERRONEAS: '        WS-CONT-ERRONEAS.
 000745            DISPLAY 'TOTAL GRABADOS: '            WS-CONT-GRABADOS.
 000746
 000747        9999-F-FINAL. EXIT.
 ****** **************************** Bottom of Data ****************************