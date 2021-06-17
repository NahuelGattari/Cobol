 ****** ***************************** Top of Data ******************************
 000100        IDENTIFICATION DIVISION.
 000101        PROGRAM-ID. PGMPRU19.
 000110       *********************************************************
 000120       *                                                        *
 000130       *  PROGRAMA PARA SQL EMBEBIDO                            *
 000140       *                                                        *
 000150       **********************************************************
 000160       *      MANTENIMIENTO DE PROGRAMA                         *
 000170       **********************************************************
 000180       *  FECHA      *       DETALLE        *
 000190       **************************************
 000191       * 09/09/2020  *    PROGRAMA CRUD     *
 000192       *             *      DE CICS         *
 000193       **************************************
 000194        AUTHOR. NAHUEL GATTARI.
 000300        DATA DIVISION.
 000400        FILE SECTION.
 000600        WORKING-STORAGE SECTION.
 000700
 000800        01 WS-MAP                 PIC X(08)       VALUE 'MAP0119'.
 000900        01 WS-MAPSET              PIC X(08)       VALUE 'MAP0119'.
 001000        01 WS-FROM                PIC X(08)       VALUE 'MAP0119O'.
 001100        01 WS-LONG                PIC S9(04) COMP.
 001200        01 WS-DATE                PIC X(10)       VALUE SPACES.
 001300        01 WS-SEP                 PIC X           VALUE '-'.
 001400        01 WS-ABSTIME             PIC S9(16) COMP VALUE +0.
 001500        01 WS-RESP                PIC S9(4)  COMP.
 001600        01 WS-FECHA               PIC X(10).
 001700        01 WS-CURSOR              PIC S9(04) COMP VALUE ZEROES.
 001800        01 WS-TIME                PIC X(8)        VALUE SPACES.
 001900        01 WS-TIMESEP             PIC X           VALUE ':'.
 002000        01 WS-NRODOC              PIC 9(11)       VALUE ZEROS.
 002100        01 WS-SQLCODE             PIC S9(03)      VALUE ZEROS.
 002101
 002130
 002140       **************************************
 002150       *      LAYOUT MAESTRO PERSONAS       *
 002160       *                                    *
 002170       **************************************
 002180
 002190        01 WS-DATASET             PIC X(08)       VALUE 'PERSONA'.
 002200        01 WS-LEN-DATA            PIC S9(04)      VALUE 160  COMP.
 002300
 002310        01 WS-RIDFLD.
 002320           03 WS-RIDFLD-TIP       PIC X(02).
 002330           03 WS-RIDFLD-NUM       PIC 9(11).
 002340
 002400        01  WS-COMMAREA.
 002500            03  WS-COM-TIP        PIC X(02).
 002600            03  WS-COM-NRO        PIC 9(11).
 002700
 002800            03 WS-TIPO-DOC        PIC X(02).
 002900               88 WS-TIP-DOC-BOOLEANO            VALUE 'CI'
 003000                                                       'DU'
 003100                                                       'PA'
 003200                                                       'PE'.
 003300            03 WS-FLAG            PIC 9(01).
 003400                  88 WS-INICIO                   VALUE 0.
 003500                  88 WS-ERASE                    VALUE 5.
 003600                  88 WS-REGISTER                 VALUE 9.
 003700
 004600           COPY MAP0219.
 004700           COPY DFHBMSCA.
 004800           COPY DFHAID.
 004900           COPY CPPERSO.
 004910
 005000        LINKAGE SECTION.
 005100
 005200        01 DFHCOMMAREA            PIC X(16).
 005300
 005400        PROCEDURE DIVISION.
 005500
 005600        MAIN-PROGRAM.
 005700
 005800            PERFORM 1000-I-INICIO  THRU  1000-F-INICIO.
 005900
 006000            PERFORM 2000-I-PROCESO THRU  2000-F-PROCESO.
 006100
 006200            PERFORM 3999-I-LOOP    THRU  3999-F-LOOP.
 006300
 006400
 006500       **************************************
 006600       *                                    *
 006700       *  CUERPO INICIO APERTURA ARCHIVOS   *
 006800       *                                    *
 006900       **************************************
 007000
 007100        1000-I-INICIO.
 008000
 008100       *       INITIALIZE MAP0119O
 008200
 008300               MOVE LENGTH OF MAP0119O TO WS-LONG
 008400
 008500                EXEC CICS ASKTIME
 008600                   ABSTIME (WS-ABSTIME)
 008700                END-EXEC.
 008800
 008900                EXEC CICS FORMATTIME
 009000                   ABSTIME (WS-ABSTIME)
 009100                   DDMMYYYY(WS-DATE) DATESEP(WS-SEP)
 009200                   TIME (WS-TIME) TIMESEP (WS-TIMESEP)
 009300                END-EXEC.
 009400
 009500       *       MOVE WS-DATE TO FECHAO.
 009600               MOVE DFHCOMMAREA TO WS-COMMAREA.
 009700
 009800        1000-F-INICIO. EXIT.
 009900
 010000       **************************************
 010100       *                                    *
 010200       *  CUERPO PRINCIPAL DE PROCESOS      *
 010300       *                                    *
 010400       **************************************
 010500
 010600        2000-I-PROCESO.
 010700
 010800               MOVE WS-DATE TO FECHAO
 010900
 011000               EXEC CICS
 011100                    RECEIVE MAP(WS-MAP)
 011200                    MAPSET(WS-MAPSET)
 011300                    INTO(MAP0119I)
 011400                    RESP(WS-RESP)
 011500               END-EXEC
 011600
 011700
 011800             EVALUATE WS-RESP
 011900             WHEN DFHRESP(NORMAL)
 012000
 012100               PERFORM 3300-I-KEYS  THRU 3300-F-KEYS
 012200
 012300
 012400             WHEN DFHRESP(MAPFAIL)
 012500               INITIALIZE MAP0119O
 012600               MOVE LENGTH OF MAP0119O     TO WS-LONG
 012700               MOVE WS-DATE                TO FECHAO
 012800               MOVE 'DATOS INCORRECTOS'    TO MSGO
 012900               IF EIBAID = DFHPF12
 013000                   PERFORM 3800-I-ESC
 013100                     THRU  3800-F-ESC
 013200               END-IF
 013300               EXEC CICS SEND MAP (WS-MAP)
 013400                    MAPSET (WS-MAPSET)
 013500                    FROM (MAP0119O)
 013600                    LENGTH (WS-LONG)
 013700                    ERASE
 013800                    FREEKB
 013900               END-EXEC
 014000
 014100               WHEN OTHER
 014200                 CONTINUE
 014300             END-EVALUATE.
 014400
 014500        2000-F-PROCESO. EXIT.
 014600
 014700        3300-I-KEYS.
 014800            EVALUATE WS-FLAG
 014900
 015000            WHEN 0
 015100
 015200             EVALUATE EIBAID
 015300
 015400               WHEN DFHENTER
 015500                   PERFORM 3350-I-ENTER
 015600                   THRU 3350-F-ENTER
 015700
 015800
 015900               WHEN DFHPF3
 016000                   PERFORM 3500-I-CLEAN
 016100                   THRU 3500-F-CLEAN
 016200
 016300
 016400               WHEN DFHPF6
 016500                   PERFORM 3550-I-PREGUNTA
 016600                   THRU 3550-F-PREGUNTA
 016700
 016800
 016900               WHEN DFHPF7
 017000                    PERFORM 3650-I-PREGUNTO
 017100                    THRU 3650-F-PREGUNTO
 017200
 017300
 017400               WHEN DFHPF12
 017500                   PERFORM 3800-I-ESC
 017600                   THRU 3800-F-ESC
 017700
 017800               WHEN OTHER
 017900
 018000                   MOVE 'SE PULSO UNA TECLA INCORRECTA.' TO MSGO
 018100                   MOVE WS-DATE TO FECHAO
 018200
 018300                        EXEC CICS SEND MAP (WS-MAP)
 018400                          MAPSET (WS-MAPSET)
 018500                          FROM (MAP0119O)
 018600                          LENGTH (WS-LONG)
 018700                          ERASE
 018800                          FREEKB
 018900                        END-EXEC
 019000
 019100             END-EVALUATE
 019200
 019300            WHEN 5
 019400
 019500               PERFORM 3600-I-ERASE
 019600               THRU 3600-F-ERASE
 019700
 019800            WHEN 9
 019900
 020000               PERFORM 3700-I-REGISTER
 020100               THRU 3700-F-REGISTER
 020200
 020300            END-EVALUATE.
 020400
 020500        3300-F-KEYS. EXIT.
 020600
 020700        3350-I-ENTER.
 020800
 020900                MOVE TIPDOCI TO WS-TIPO-DOC.
 021000
 021100                IF WS-TIP-DOC-BOOLEANO
 021200                   IF NRODOCI IS NUMERIC
 021300
 021400                        PERFORM 3400-I-INICIAR
 021500                           THRU 3400-F-INICIAR
 021600                   ELSE
 021700                       MOVE 'ERROR AL INGRESAR NRO DE DOCUMENTO.' TO MSGO
 021800                       MOVE WS-DATE TO FECHAO
 021900                        EXEC CICS SEND MAP (WS-MAP)
 022000                          MAPSET (WS-MAPSET)
 022100                          FROM (MAP0119O)
 022200                          LENGTH (WS-LONG)
 022300                          ERASE
 022400                          FREEKB
 022500                        END-EXEC
 022600                ELSE
 022700                   MOVE 'INGRESE LOS DATOS' TO MSGO
 022800                   MOVE WS-DATE TO FECHAO
 022900                   EXEC CICS SEND MAP (WS-MAP)
 023000                          MAPSET (WS-MAPSET)
 023100                          FROM (MAP0119O)
 023200                          LENGTH (WS-LONG)
 023300                          ERASE
 023400                          FREEKB
 023500                   END-EXEC
 023600                END-IF.
 023700
 023800        3350-F-ENTER. EXIT.
 023900
 024000        3400-I-INICIAR.
 024100
 024110            MOVE TIPDOCI  TO WS-RIDFLD-TIP.
 024120            MOVE NRODOCI  TO WS-RIDFLD-NUM.
 024130
 024140            EXEC CICS
 024150                 READ DATASET (WS-DATASET)
 024160                 RIDFLD (WS-RIDFLD)
 024170                 INTO   (REG-PERSONA)
 024180                 LENGTH (WS-LEN-DATA)
 024190                 EQUAL
 024191                 RESP   (WS-RESP)
 024192            END-EXEC.
 024193
 024194            EVALUATE WS-RESP
 024195                WHEN DFHRESP(NORMAL)
 024196                     MOVE PER-CLI-NRO   TO NROCLIO
 024197                     MOVE PER-NOMAPE    TO NOMAPEO
 024198                     MOVE PER-DIRECCION TO DIRECO
 024199                     MOVE PER-EMAIL     TO EMAILO
 024200                     MOVE PER-TELEFONO  TO TELO
 024201                     MOVE 'CLIENTE ENCONTRADO'    TO MSGO
 024202                     MOVE LENGTH OF MAP0119O      TO WS-LONG
 024203                WHEN DFHRESP(NOTFND)
 024204                     INITIALIZE MAP0119O
 024205                     MOVE 'CLIENTE NO ENCONTRADO' TO MSGO
 024206                WHEN OTHER
 024207                     MOVE 'ERROR AL LEER ' TO MSGO
 024208            END-EVALUATE.
 024209
 024210                     MOVE WS-DATE       TO FECHAO.
 026000
 026300                     EXEC CICS SEND MAP (WS-MAP)
 026400                          MAPSET (WS-MAPSET)
 026500                          FROM (MAP0119O)
 026600                          LENGTH(WS-LONG)
 026700                          ERASE
 026800                          FREEKB
 026900                     END-EXEC.
 027000
 027100        3400-F-INICIAR. EXIT.
 027200
 027300
 027400        3500-I-CLEAN.
 027500
 027600               INITIALIZE MAP0119O.
 027700       *       MOVE LENGTH OF MAP0119O TO WS-LONG
 027800               MOVE WS-DATE TO FECHAO
 027900
 028000            EXEC CICS SEND MAP (WS-MAP)
 028100                  MAPSET (WS-MAPSET)
 028200                  FROM (MAP0119O)
 028300                  LENGTH (WS-LONG)
 028400                  ERASE
 028500            END-EXEC.
 028600
 028700        3500-F-CLEAN. EXIT.
 028800
 028900        3550-I-PREGUNTA.
 029000
 029100            MOVE WS-DATE TO FECHAO
 029200
 029300            MOVE 'INGRESE S PARA ELIMINAR, N PARA CANCELAR. PULSE F6'
 029400                   TO MSGO
 029500            MOVE PER-TIP-DOC   TO WS-COM-TIP
 029600            MOVE PER-NRO-DOC   TO WS-COM-NRO
 029700
 029800            MOVE '_'           TO CAMPO
 029900            MOVE -1            TO CAMPL
 030000            MOVE DFHBMUNP      TO CAMPA
 030100            MOVE DFHBMPRO      TO NRODOCA
 030200            MOVE DFHBMPRO      TO TIPDOCA
 030300
 030400            SET WS-ERASE TO TRUE
 030500
 030600            EXEC CICS SEND MAP (WS-MAP)
 030700                  MAPSET (WS-MAPSET)
 030800                  FROM (MAP0119O)
 030900                  LENGTH (WS-LONG)
 031000                  CURSOR
 031100                  ERASE
 031200            END-EXEC.
 031300
 031400        3550-F-PREGUNTA. EXIT.
 031500
 031600        3600-I-ERASE.
 031700
 031800              IF CAMPI = 'N'
 031900
 032000                MOVE 'EL REGISTRO NO FUE ELMINADO. PULSE PF3' TO MSGO
 032100
 032110               ELSE IF CAMPI = 'S'
 032120       *       WS-RIDFLD-TIP WS-RIDFLD-NUM
 032130                 MOVE    WS-COM-TIP  TO TIPDOCO
 032140                                        WS-RIDFLD-TIP
 032150
 032160                 MOVE    WS-COM-NRO  TO NRODOCO
 032170                                        WS-RIDFLD-NUM
 032180
 032190                 EXEC CICS DELETE DATASET (WS-DATASET)
 032191                   RIDFLD (WS-RIDFLD)
 032192                   RESP   (WS-RESP)
 032193                 END-EXEC
 032194
 032195                EVALUATE WS-RESP
 032196                 WHEN DFHRESP(NORMAL)
 032197                  MOVE 'EL REGISTRO SE ELIMINO EXITOSAMENTE,PULSE PF3'
 032198                          TO MSGO
 032199                 WHEN DFHRESP(NOTFND)
 032200                  MOVE 'ERROR AL BORRAR EL REGISTRO, PULSE PF3 ' TO MSGO
 032201
 032202                 WHEN OTHER
 032203                  MOVE 'ERROR EN EL ARCHIVO' TO MSGO
 032204                 END-EVALUATE
 032205               ELSE
 032206               MOVE 'ERROR, INGRESE UNICAMENTE  S / N, PULSE PF6' TO MSGO
 032207
 032208               END-IF
 032209              END-IF
 032210
 032211             SET WS-INICIO TO TRUE
 035500
 035600             EXEC CICS SEND MAP (WS-MAP)
 035700                  MAPSET (WS-MAPSET)
 035800                  FROM (MAP0119O)
 035900                  LENGTH (WS-LONG)
 036000                  ERASE
 036100                  FREEKB
 036200             END-EXEC.
 036300
 036400        3600-F-ERASE. EXIT.
 036500
 036600        3650-I-PREGUNTO.
 036700
 036800            MOVE WS-DATE TO FECHAO
 036900
 037000            MOVE
 037100            'INGRESE LOS DATOS, S PARA ALTA O N PARA CANCELAR. PULSE PF7'
 037200            TO MSGO
 037300
 037400            PERFORM 3900-I-DESPROTEGER
 037500               THRU 3900-F-DESPROTEGER
 037600
 037700            MOVE '_'           TO CAMPO
 037800            MOVE DFHBMUNP      TO CAMPA
 037900
 038000            SET WS-REGISTER TO TRUE
 038100
 038200            EXEC CICS SEND MAP (WS-MAP)
 038300                  MAPSET (WS-MAPSET)
 038400                  FROM (MAP0119O)
 038500                  LENGTH (WS-LONG)
 038600                  ERASE
 038700            END-EXEC.
 038800
 038900        3650-F-PREGUNTO. EXIT.
 039000
 039100        3700-I-REGISTER.
 039200
 039300            IF CAMPI = 'N'
 039400
 039500             MOVE 'SE CANCELO EL ALTA DEL REGISTRO. PULSE PF3' TO MSGO
 039600
 039700            ELSE IF CAMPI = 'S'
 039800
 039900            PERFORM 3750-I-VALIDACION
 040000               THRU 3750-F-VALIDACION
 040100
 040110             EXEC CICS
 040120               WRITE DATASET(WS-DATASET)
 040130               RIDFLD (WS-RIDFLD)
 040140               FROM   (REG-PERSONA)
 040150               LENGTH (WS-LEN-DATA)
 040160               RESP   (WS-RESP)
 040170             END-EXEC
 040180
 040190              EVALUATE WS-RESP
 040191
 040192               WHEN DFHRESP(NORMAL)
 040193                 MOVE 'EL REGISTRO FUE CREADO EXITOSAMENTE. PULSE PF3'
 040194                     TO MSGO
 040195               WHEN DFHRESP(DUPREC)
 040196                 MOVE 'REGISTRO DUPLICADO, PULSE PF3 ' TO MSGO
 040197               WHEN OTHER
 040198                 MOVE 'ERROR EN EL ARCHIVO, PULSE PF7' TO MSGO
 040199              END-EVALUATE
 040200            ELSE
 040210             MOVE 'ERROR, INGRESE UNICAMENTE  S / N, PULSE PF7' TO MSGO
 040220             END-IF
 040230            END-IF
 042500
 042600            SET WS-INICIO TO TRUE
 042700
 042800            EXEC CICS SEND MAP (WS-MAP)
 042900                  MAPSET (WS-MAPSET)
 043000                  FROM (MAP0119O)
 043100                  LENGTH (WS-LONG)
 043200                  ERASE
 043300            END-EXEC.
 043400
 043500        3700-F-REGISTER. EXIT.
 043600
 043700
 043800
 043900               MOVE NROCLII     TO PER-CLI-NRO
 044000               MOVE TELI        TO PER-TELEFONO
 044100               MOVE EMAILI      TO PER-EMAIL
 044200               MOVE DIRECI      TO PER-DIRECCION
 044300               MOVE NOMAPEI     TO PER-NOMAPE
 044400               MOVE TIPDOCI     TO PER-TIP-DOC
 044500               MOVE NRODOCI     TO PER-NRO-DOC
 044600               MOVE WS-AAAAMMDD TO PER-CLI-AAAAMMDD
 044700               MOVE SPACES      TO PER-LOCALIDAD
 044800               MOVE SPACES      TO PER-SEXO
 044900
 045000            IF PER-CLI-NRO NOT NUMERIC
 045100               MOVE 'EL NUMERO DE CLIENTE ES INVALIDO.' TO MSGO
 045200            END-IF
 045300
 045400            IF PER-TELEFONO NOT NUMERIC
 045500               MOVE 'EL NUMERO DE TELEFONO ES INVALIDO.' TO MSGO
 045600            END-IF
 045700
 045800            IF PER-EMAIL EQUAL SPACES
 045900               MOVE 'LA DIRECCION DE EMAIL ES INVALIDA.' TO MSGO
 046000            END-IF
 046100
 046200            IF PER-DIRECCION EQUAL SPACES
 046300               MOVE 'LA DIRECCION ES INVALIDA.' TO MSGO
 046400            END-IF
 046500
 046600            IF PER-NOMAPE  EQUAL SPACES
 046700               MOVE 'EL NOMBRE Y APELLIDO ES INVALIDO.' TO MSGO
 046800            END-IF
 046900
 047000            IF NOT WS-TIP-DOC-BOOLEANO
 047100               MOVE 'EL TIPO DE DOCUMENTO ES INVALIDO.' TO MSGO
 047200            END-IF
 047300
 047400            IF PER-NRO-DOC NOT NUMERIC
 047500               MOVE 'EL NUMERO DE DOCUMENTO ES INVALIDO.' TO MSGO
 047600            END-IF
 047700
 047800            EXEC CICS SEND MAP (WS-MAP)
 047900                  MAPSET (WS-MAPSET)
 048000                  FROM (MAP0119O)
 048100                  LENGTH (WS-LONG)
 048200                  ERASE
 048300            END-EXEC.
 048400
 048500        3750-F-VALIDACION. EXIT.
 048600
 048700        3800-I-ESC.
 048800
 048900             EXEC CICS
 049000                SEND CONTROL ERASE
 049100             END-EXEC
 049200
 049300             EXEC CICS
 049400                RETURN
 049500             END-EXEC.
 049600
 049700        3800-F-ESC. EXIT.
 049800
 049900        3900-I-DESPROTEGER.
 050000
 050100            MOVE DFHBMUNN                        TO NROCLIA.
 050200            MOVE DFHBMUNP                        TO NOMAPEA.
 050300            MOVE DFHBMUNP                        TO DIRECA.
 050400            MOVE DFHBMUNP                        TO EMAILA.
 050500            MOVE DFHBMUNN                        TO TELA.
 050600
 050610        3900-F-DESPROTEGER. EXIT.
 050620
 050695        3999-I-LOOP.
 050696            EXEC CICS
 050697                 RETURN TRANSID('T219')
 050698                 COMMAREA (WS-COMMAREA)
 050699            END-EXEC.
 050700
 050800        3999-F-LOOP. EXIT.
 050900
 ****** **************************** Bottom of Data ****************************