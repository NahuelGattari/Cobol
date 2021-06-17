 ****** ***************************** Top of Data ******************************
 000001        IDENTIFICATION DIVISION.
 000002       *                                                        *
 000003         PROGRAM-ID PGMAPA99.
 000004       **********************************************************
 000005       *      MANTENIMIENTO DE PROGRAMA                         *
 000006       **********************************************************
 000007       *   FECHA    *    DETALLE       *
 000008       ****************************************
 000009       * 2020/07/21 *  PROGRAMA ACTUALIZADOR  *
 000010       *            *       DE SALDO          *
 000011       ****************************************
 000012        AUTHOR. NAHUEL GATTARI.
 000013        ENVIRONMENT DIVISION.
 000014        CONFIGURATION SECTION.
 000015        SPECIAL-NAMES.
 000016            DECIMAL-POINT IS COMMA.
 000017
 000018        INPUT-OUTPUT SECTION.
 000019        FILE-CONTROL.
 000020              SELECT MAESTRO ASSIGN DDMAE
 000021              ORGANIZATION   IS INDEXED
 000022              ACCESS IS SEQUENTIAL
 000023              RECORD KEY IS WS-CLAVE-MAE
 000024              FILE STATUS IS WS-MAE-CODE.
 000025
 000026              SELECT NOVEDAD ASSIGN DDNOV
 000027                     FILE STATUS IS WS-NOV-CODE.
 000028
 000029              SELECT SALIDA  ASSIGN DDSAL
 000030              ORGANIZATION   IS INDEXED
 000031              ACCESS IS RANDOM
 000032              RECORD KEY IS WS-SALIDA
 000033              FILE STATUS IS WS-SAL-CODE.
 000034
 000035        DATA DIVISION.
 000036        FILE SECTION.
 000037        FD MAESTRO.
 000038
 000039        01 REG-MAESTRO.
 000040           03 WS-CLAVE-MAE PIC X(13).
 000041           03 FILLER       PIC X(37).
 000042
 000043        FD NOVEDAD
 000044             BLOCK CONTAINS 0 RECORDS
 000045             RECORDING MODE IS F.
 000046
 000047        01 REG-NOVEDAD    PIC X(50).
 000048
 000049        FD SALIDA.
 000050
 000051        01 REG-SALIDA.
 000052           03 WS-SALIDA    PIC X(13).
 000053           03 FILLER       PIC X(37).
 000054
 000055       **************************************
 000056        WORKING-STORAGE SECTION.
 000057       **************************************
 000058        77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
 000059        77  FILLER        PIC X(26) VALUE '* CODIGOS RETORNO FILES  *'.
 000060        77  WS-MAE-CODE      PIC XX    VALUE SPACES.
 000061        77  WS-NOV-CODE      PIC XX    VALUE SPACES.
 000062        77  WS-SAL-CODE      PIC XX    VALUE SPACES.
 000063        77  WS-PGMRUT        PIC X(8)  VALUE 'PGMRUT'.
 000064
 000065       ************
 000066       *CONTADORES*
 000067       ************
 000068        77  WS-CONT-MAE          PIC 9(8)     VALUE ZEROS.
 000069        77  WS-CONT-NOV          PIC 9(8)     VALUE ZEROS.
 000070        77  WS-CONT-GRA          PIC 9(8)     VALUE ZEROS.
 000071
 000072        01  WS-STATUS-FIN    PIC X.
 000073            88  WS-FIN-LECTURA         VALUE 'Y'.
 000074            88  WS-NO-FIN-LECTURA      VALUE 'N'.
 000075
 000076        01  WS-STA-MAE     PIC X.
 000077            88  WS-FIN-MAE           VALUE 'Y'.
 000078            88  WS-NO-FIN-MAE        VALUE 'N'.
 000079
 000080        01  WS-STA-NOV     PIC X.
 000081            88  WS-FIN-NOV           VALUE 'Y'.
 000082            88  WS-NO-FIN-NOV        VALUE 'N'.
 000083
 000084        01  WS-SALDO-TOTAL     PIC S9(7)V99 VALUE ZEROS.
 000085
 000086
 000087       **************************************
 000088       *         LAYOUT MAESTRO CLIENTES    *
 000089       *                                    *
 000090       *         LARGO 50 BYTES             *
 000091       *        VSAM KSDS KEY (1,13)        *
 000092       *                                    *
 000093       *        ALT KEY NRO-CLI  (18,3)     *
 000094       **************************************
 000095
 000096        01  WS-REG-MAESTRO.
 000097            03  WS-TIP-DOC            PIC X(02)    VALUE SPACES.
 000098            03  WS-NRO-DOC            PIC 9(11)    VALUE ZEROS.
 000099            03  FILLER                PIC X(04)    VALUE SPACES.
 000100            03  WS-CLI-NRO            PIC 9(03)    VALUE ZEROS.
 000101            03  WS-CLI-SALDO          PIC S9(09)V99 COMP-3 VALUE ZEROS.
 000102            03  WS-CLI-AAAAMMDD       PIC 9(08)    VALUE ZEROS.
 000103            03  FILLER                PIC X(16)    VALUE SPACES.
 000104
 000105
 000106       **************************************
 000107       *         LAYOUT MAESTRO NOVEDADES   *
 000108       *                                    *
 000109       *         LARGO 50 BYTES             *
 000110       *        VSAM KSDS KEY (1,13)        *
 000111       *                                    *
 000112       *        ALT KEY NRO-CLI  (18,3)     *
 000113       **************************************
 000114
 000115        01 WS-REG-NOVEDAD.
 000116           03 WS-CLAVE2.
 000117              05 WS-NOV-TIP-DOC        PIC X(02)     VALUE SPACES.
 000118              05 WS-NOV-NRO-DOC        PIC 9(11)     VALUE ZEROS.
 000119           03 WS-NOV-SUC            PIC 9(02)     VALUE ZEROS.
 000120           03 WS-NOV-TIPO           PIC 9(02)     VALUE ZEROS.
 000121           03 WS-NOV-NRO            PIC 9(03)     VALUE ZEROS.
 000122           03 WS-NOV-SALDO          PIC S9(09)V99 COMP-3 VALUE ZEROS.
 000123           03  FILLER               PIC X(24)     VALUE SPACES.
 000124
 000125       **************************************
 000126       *         LAYOUT SALIDA              *
 000127       *                                    *
 000128       *         LARGO 50 BYTES             *
 000129       *        VSAM KSDS KEY (1,13)        *
 000130       *                                    *
 000131       *        ALT KEY NRO-CLI  (18,3)     *
 000132       **************************************
 000133
 000134        01  WS-REG-SALIDA.
 000135            03  WS-TIP-DOC-SAL        PIC X(02)    VALUE SPACES.
 000136            03  WS-NRO-DOC-SAL        PIC 9(11)    VALUE ZEROS.
 000137            03  FILLER                PIC X(04)    VALUE SPACES.
 000138            03  WS-CLI-NRO-SAL        PIC 9(03)    VALUE ZEROS.
 000139            03  WS-CLI-SALDO-SAL      PIC S9(09)V99 COMP-3 VALUE ZEROS.
 000140            03  WS-CLI-AAAAMMDD-SAL   PIC 9(08)    VALUE ZEROS.
 000141            03  FILLER                PIC X(16)    VALUE SPACES.
 000142
 000143
 000144        01  WS-FECHA.
 000145            03  WS-ANIO    PIC 9(02)   VALUE ZEROS.
 000146            03  WS-MES     PIC 9(02)   VALUE ZEROS.
 000147            03  WS-DIA     PIC 9(02)   VALUE ZEROS.
 000148
 000149        LINKAGE SECTION.
 000150
 000151         01  LK-AREA.
 000152             03  LK-FECHA.
 000153                 05  SIGLO-ANIO.
 000154                     07 LK-SIGLO    PIC 99.
 000155                     07 LK-ANIO     PIC 99.
 000156                 05 LK-MES      PIC 99.
 000157                 05 LK-DIA      PIC 99.
 000158             03 FILLER      PIC X(22).
 000159
 000160       **************************************
 000161        PROCEDURE DIVISION USING LK-AREA.
 000162       **************************************
 000163       *                                    *
 000164       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000165       *                                    *
 000166       **************************************
 000167        MAIN-PROGRAM.
 000168
 000169            PERFORM 1000-INICIO  THRU   F-1000-INICIO.
 000170
 000171            PERFORM 2000-PROCESO  THRU  F-2000-PROCESO
 000172                    UNTIL WS-FIN-LECTURA.
 000173
 000174            PERFORM 9999-FINAL    THRU  F-9999-FINAL.
 000175
 000176        F-MAIN-PROGRAM. GOBACK.
 000177
 000178       **************************************
 000179       *                                    *
 000180       *  CUERPO INICIO APERTURA ARCHIVOS   *
 000181       *                                    *
 000182       **************************************
 000183        1000-INICIO.
 000184
 000185            ACCEPT WS-FECHA FROM DATE.
 000186            PERFORM 1500-FECHA   THRU F-1500-FECHA.
 000187            SET WS-NO-FIN-LECTURA TO TRUE.
 000188            MOVE 'NO' TO WS-STATUS-FIN
 000189
 000190            OPEN INPUT  MAESTRO.
 000191            IF WS-MAE-CODE IS NOT EQUAL '00'
 000192               DISPLAY '* ERROR EN OPEN FILE1   = ' WS-MAE-CODE
 000193               MOVE 9999 TO RETURN-CODE
 000194               SET  WS-FIN-LECTURA TO TRUE
 000195            END-IF.
 000196
 000197            OPEN INPUT  NOVEDAD.
 000198            IF WS-NOV-CODE IS NOT EQUAL '00'
 000199               DISPLAY '* ERROR EN OPEN MOVIMI  = ' WS-NOV-CODE
 000200               MOVE 9999 TO RETURN-CODE
 000201               SET  WS-FIN-LECTURA TO TRUE
 000202            END-IF.
 000203
 000204            OPEN I-O SALIDA.
 000205            IF WS-SAL-CODE IS NOT EQUAL '00'
 000206               DISPLAY '* ERROR EN OPEN SALIDA  = ' WS-SAL-CODE
 000207               MOVE 9999 TO RETURN-CODE
 000208               SET  WS-FIN-LECTURA TO TRUE
 000209            END-IF.
 000210
 000211             PERFORM 3000-LEER-MAESTRO THRU F-3000-LEER-MAESTRO.
 000212             PERFORM 4000-LEER-NOVEDAD  THRU F-4000-LEER-NOVEDAD.
 000213
 000214        F-1000-INICIO.   EXIT.
 000215
 000216        1500-FECHA.
 000217
 000218             MOVE SPACES     TO  LK-AREA.
 000219             MOVE 20         TO  LK-SIGLO.
 000220             MOVE WS-ANIO    TO  LK-ANIO.
 000221             MOVE WS-MES     TO  LK-MES.
 000222             MOVE WS-DIA     TO  LK-DIA.
 000223             CALL WS-PGMRUT  USING LK-AREA.
 000224             IF RETURN-CODE  EQUAL  05
 000225                SET  WS-FIN-MAE   TO TRUE
 000226                DISPLAY  'ERROR RUTINA FECHA: 05'
 000227             END-IF.
 000228
 000229        F-1500-FECHA. EXIT.
 000230       **************************************
 000231       *                                    *
 000232       *  CUERPO PRINCIPAL DE PROCESOS      *
 000233       *  LECTURA FILE INPUT CLASIFICADO    *
 000234       *  APAREO ARCHIVOS DE ENTRADA        *
 000235       *                                    *
 000236       **************************************
 000237        2000-PROCESO.
 000238
 000239            IF WS-CLAVE-MAE = WS-CLAVE2
 000240       *        DISPLAY 'IGUALES' WS-CLAVE-MAE '  '  WS-CLAVE2
 000241                PERFORM 4500-ACTUALIZAR THRU 4500-ACTUALIZAR
 000242                PERFORM 4000-LEER-NOVEDAD  THRU  F-4000-LEER-NOVEDAD
 000243
 000244            ELSE
 000245
 000246              IF WS-CLAVE-MAE  > WS-CLAVE2
 000247       *        DISPLAY 'DIFERENTES' WS-CLAVE-MAE '  '  WS-CLAVE2
 000248                PERFORM 4000-LEER-NOVEDAD  THRU  F-4000-LEER-NOVEDAD
 000249              ELSE
 000250                PERFORM 6000-GRABAR-SALIDA  THRU  F-6000-GRABAR-SALIDA
 000251                PERFORM 3000-LEER-MAESTRO  THRU  F-3000-LEER-MAESTRO
 000252       *        DISPLAY ' ERROR ' WS-CLAVE-MAE '  '  WS-CLAVE2
 000253              END-IF
 000254            END-IF.
 000255       *************************************************************
 000256       * CONTROL FIN DE ARCHIVOS DE ENTRADA, PARA FIN PROGRAMA     *
 000257       *************************************************************
 000258
 000259            IF WS-FIN-MAE AND WS-FIN-NOV
 000260               SET  WS-FIN-LECTURA TO TRUE
 000261            END-IF.
 000262
 000263        F-2000-PROCESO. EXIT.
 000264
 000265       **************************************
 000266       * LECTURA MAESTRO                    *
 000267       **************************************
 000268
 000269        3000-LEER-MAESTRO.
 000270
 000271       *    DISPLAY 'LEER MAESTRO'.
 000272            READ MAESTRO INTO WS-REG-MAESTRO.
 000273
 000274            EVALUATE WS-MAE-CODE
 000275              WHEN '00'
 000276                      ADD 1 TO WS-CONT-MAE
 000277               WHEN '10'
 000278               SET WS-FIN-MAE  TO TRUE
 000279               MOVE HIGH-VALUE   TO WS-CLAVE-MAE
 000280
 000281            WHEN OTHER
 000282               DISPLAY '* ERROR EN LECTURA MAESTRO = ' WS-MAE-CODE
 000283               MOVE 9999 TO RETURN-CODE
 000284               SET WS-FIN-MAE TO TRUE
 000285
 000286            END-EVALUATE.
 000287        F-3000-LEER-MAESTRO. EXIT.
 000288
 000289       **************************************
 000290       * LECTURA NOVEDAD                    *
 000291       **************************************
 000292        4000-LEER-NOVEDAD.
 000293
 000294       *    DISPLAY 'LEER NOVEDAD'
 000295            READ NOVEDAD INTO WS-REG-NOVEDAD.
 000296
 000297            EVALUATE WS-NOV-CODE
 000298              WHEN '00'
 000299                     ADD 1 TO WS-CONT-NOV
 000300               WHEN '10'
 000301               SET WS-FIN-NOV   TO TRUE
 000302               MOVE HIGH-VALUE   TO WS-CLAVE2
 000303
 000304            WHEN OTHER
 000305               DISPLAY '* ERROR EN LECTURA NOVEDAD = ' WS-NOV-CODE
 000306               MOVE 9999 TO RETURN-CODE
 000307               SET WS-FIN-NOV   TO TRUE
 000308
 000309            END-EVALUATE.
 000310
 000311        F-4000-LEER-NOVEDAD. EXIT.
 000312
 000313
 000314        4500-ACTUALIZAR.
 000315
 000316       *    DISPLAY ' ACTUALIZO '.
 000317            ADD  WS-NOV-SALDO TO WS-SALDO-TOTAL.
 000318
 000319        F-4500-ACTUALIZAR. EXIT.
 000320
 000321       **************************************************
 000322       *   PARRAFO PARA GRABAR LA SALIDA ACTUALIZADA    *
 000323       **************************************************
 000324
 000325        6000-GRABAR-SALIDA.
 000326
 000327       *    DISPLAY ' GRABO SALIDA ' WS-SALDO-TOTAL
 000328
 000329            MOVE WS-TIP-DOC TO  WS-TIP-DOC-SAL.
 000330            MOVE WS-NRO-DOC TO  WS-NRO-DOC-SAL.
 000331            MOVE WS-CLI-NRO TO  WS-CLI-NRO-SAL.
 000332            MOVE LK-FECHA   TO  WS-CLI-AAAAMMDD-SAL.
 000333            MOVE WS-SALDO-TOTAL TO WS-CLI-SALDO-SAL.
 000334
 000335            WRITE REG-SALIDA   FROM WS-REG-SALIDA.
 000336              ADD 1 TO WS-CONT-GRA
 000337               IF WS-SAL-CODE IS NOT EQUAL '00'
 000338                 DISPLAY '* ERROR EN WRITE   = '
 000339                                             WS-SAL-CODE
 000340                 MOVE 9999 TO RETURN-CODE
 000341                 SET WS-FIN-LECTURA TO TRUE
 000342               END-IF.
 000343            MOVE ZEROS TO WS-SALDO-TOTAL.
 000344
 000345        F-6000-GRABAR-SALIDA. EXIT.
 000346
 000347
 000348       **************************************
 000349       *                                    *
 000350       *  CUERPO FINAL CIERRE DE FILES      *
 000351       *                                    *
 000352       **************************************
 000353        9999-FINAL.
 000354
 000355            CLOSE MAESTRO.
 000356               IF WS-MAE-CODE IS NOT EQUAL '00'
 000357                 DISPLAY '* ERROR EN CLOSE MAESTRO = '
 000358                                             WS-MAE-CODE
 000359                 MOVE 9999 TO RETURN-CODE
 000360                 SET WS-FIN-LECTURA TO TRUE
 000361              END-IF.
 000362
 000363            CLOSE  NOVEDAD
 000364               IF WS-NOV-CODE IS NOT EQUAL '00'
 000365                 DISPLAY '* ERROR EN CLOSE NOVEDAD  ='
 000366                                             WS-NOV-CODE
 000367                 MOVE 9999 TO RETURN-CODE
 000368                 SET WS-FIN-LECTURA TO TRUE
 000369            END-IF.
 000370
 000371            CLOSE SALIDA
 000372               IF WS-SAL-CODE IS NOT EQUAL '00'
 000373                 DISPLAY '* ERROR EN CLOSE SALIDA  = '
 000374                                             WS-SAL-CODE
 000375                 MOVE 9999 TO RETURN-CODE
 000376                 SET WS-FIN-LECTURA TO TRUE
 000377              END-IF.
 000378
 000379       **********************************
 000380       *   MOSTRAR TOTALES DE CONTROL   *
 000381       **********************************
 000382
 000383              DISPLAY 'CANTIDAD REGISTROS MAESTRO: ' WS-CONT-MAE.
 000384              DISPLAY 'CANTIDAD REGISTROS NOVEDADES: ' WS-CONT-NOV.
 000385              DISPLAY 'CANTIDAD REGISTROS GRABADOS: ' WS-CONT-GRA.
 000386
 000387        F-9999-FINAL.
 000388            EXIT.
 ****** **************************** Bottom of Data ****************************