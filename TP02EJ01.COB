      *----------------------------------------------------------------*
      *Author:  GABRIELA RODRIGUEZ                                     *
      *         CLAUDIA SANTILLAN                                      *
      *         GABY OSELLA                                            *
      *         GUILLERMO RODRIGUEZ                                    *
      *         EDDISON MORA GOMEZ
      * Date: 28/11/2023
      * Purpose: TRABAJO PRACTICO NRO 2
      * Tectonics: cobc
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. TP02EJ01.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENTRADA
           ASSIGN TO '../CONSUMOS.SEQ'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

       SELECT ERRORES
           ASSIGN TO '../ERRORES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERRORES.

       SELECT RESUMENES
           ASSIGN TO '../RESUMENES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-RESUMENES.

       DATA DIVISION.

       FILE SECTION.

       COPY "CONSUMOS.CPY".
       COPY "ERRORES.CPY".
       COPY "RESUMENES.CPY".

       WORKING-STORAGE SECTION.

      * ESTRUCTURA PARA LA RUTINA CLVALFEC

       01 WS-VALIDA.
          COPY "VALFECIO.CPY".

      * VARIABLE AUXILIAR PARA CONVERTIR LA FECHA DEL ARCHIVO DE ENTRADA
      * E INVOCAR LA RUTINA CLVALFEC

       01 WS-FECHA-TXT                      PIC X(10).

       01 WS-FECHA-NUM                      REDEFINES WS-FECHA-TXT.
          05 WS-ANIO                        PIC 9(04).
          05 WS-SEPARADOR-1                 PIC X(01).
          05 WS-MES                         PIC 9(02).
          05 WS-SEPARADOR-2                 PIC X(01).
          05 WS-DIA                         PIC 9(02).
      *-----------------------------------------------------------------

       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                VALUE '00'.
             88 FS-ENTRADA-NFD               VALUE '35'.
             88 FS-ENTRADA-EOF               VALUE '10'.

          05 FS-ERRORES                    PIC X(2).
             88 FS-ERRORES-OK                 VALUE '00'.
             88 FS-ERRORES-EOF                VALUE '10'.
             88 FS-ERRORES-NFD                VALUE '35'.

          05 FS-RESUMENES                  PIC X(2).
             88 FS-RESUMENES-OK                 VALUE '00'.
             88 FS-RESUMENES-EOF                VALUE '10'.
             88 FS-RESUMENES-NFD                VALUE '35'.

       01 WS-ENTRADA-EOF                     PIC X(1).
          88 WS-ENTRADA-EOF-YES              VALUE 'Y'.
          88 WS-ENTRADA-EOF-NO               VALUE 'N'.

       01 WS-REG-MAESTRO.
          05 WS-MAE-CUENTA                  PIC 9(10).
          05 WS-MAE-NRO-TARJ                PIC X(19).
          05 WS-MAE-APELLIDO                PIC X(20).
          05 WS-MAE-NOMBRE                  PIC X(20).
          05 WS-MAE-DIRECCION               PIC X(40).
          05 WS-MAE-COD-POSTAL              PIC 9(4).
          05 WS-MAE-MONEDA                  PIC X(3).
          05 WS-MAE-LIMITE                  PIC 9(8)V99.

      *  ------ FECHA DEL SISTEMA ------------------
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
            10  WS-CURRENT-YEAR    PIC  9(4).
            10  WS-CURRENT-MONTH   PIC  9(2).
            10  WS-CURRENT-DAY     PIC  9(2).
            05  WS-CURRENT-TIME.
             10  WS-CURRENT-HOUR    PIC  9(2).
             10  WS-CURRENT-MINUTE  PIC  9(2).
             10  WS-CURRENT-SECOND  PIC  9(2).
             10  WS-CURRENT-MS      PIC  9(2).
            05  WS-DIFF-FROM-GMT       PIC S9(4).

      ******* DEFINICION ESTRUCTURA LISTADO DE RESUMENES ***************
       01 ENCABEZADO-1.
          05 FILLER            PIC X(9) VALUE 'Titular: '.
          05 ENC-APELLIDO      PIC X(20).
          05 FILLER            PIC X VALUE ','.
          05 ENC-NOMBRE        PIC X(20).
          05 FILLER            PIC X(20) VALUE ' Fecha de emisión : '.
          05 ENC-DIA           PIC 99.
          05 FILLER            PIC X VALUE '/'.
          05 ENC-MES           PIC 99.
          05 FILLER            PIC X VALUE '/'.
          05 ENC-ANIO          PIC 9999.

       01 ENCABEZADO-2.
          05 ENC-DIRECCION     PIC X(40).
          05 FILLER            PIC X(10) VALUE SPACES.
          05 FILLER            PIC X(20) VALUE ' Número de cuenta : '.
          05 ENC-CUENTA        PIC 9(10).

       01 ENCABEZADO-3.
          05 FILLER            PIC X(4) VALUE 'CP: '.
          05 ENC-COD-POSTAL    PIC 9(4).
          05 FILLER            PIC X(42) VALUE SPACES.
          05 FILLER            PIC X(20) VALUE ' Número de tarjeta: '.
          05 ENC-NRO-TARJ      PIC X(19).

       01 ENCABEZADO-4.
          05 FILLER            PIC X(50) VALUE SPACES.
          05 FILLER            PIC X(20) VALUE ' Límite de compra : '.
          05 ENC-LIMITE        PIC $ZZ.ZZZ.ZZ9,99.

       01 LINEA-SEPARADOR.
          05 FILLER            PIC X(90) VALUE ALL '-'.

       01 LINEA-SEPARADOR1.
          05 FILLER            PIC X(90) VALUE ALL '#'.

       01 LINEA-TITULO.
          05 FILLER            PIC X(11) VALUE 'Fecha'.
          05 FILLER            PIC X(33) VALUE 'Detalle'.
          05 FILLER            PIC X(5)  VALUE 'Cuota'.
          05 FILLER            PIC X(15) VALUE '     Pesos'.
          05 FILLER            PIC X(15) VALUE '     Dolares'.

       01 LINEA-DETALLE.
          05 DET-DIA                        PIC 99.
          05 FILLER                         PIC X VALUE "/".
          05 DET-MES                        PIC 99.
          05 FILLER                         PIC X VALUE "/".
          05 DET-ANIO                       PIC 9999.
          05 FILLER                         PIC X VALUE " ".
          05 DET-DETALLE                    PIC X(32).
          05 FILLER                         PIC X VALUE " ".
          05 DET-NRO-CUOTA                  PIC 9(2).
          05 FILLER                         PIC X VALUE "/".
          05 DET-CANT-CUOTAS                PIC 9(2).
          05 FILLER                         PIC X VALUE " ".
          05 DET-IMPORTE-PESOS              PIC ZZ.ZZZ.ZZZ,ZZ.
          05 FILLER                         PIC X VALUE " ".
          05 DET-IMPORTE-DOLARES            PIC ZZ.ZZZ.ZZZ,ZZ.

       01 LINEA-DETALLE-1.
          05 FILLER                     PIC X(11) VALUE SPACES.
          05 FILLER                     PIC X(10) VALUE 'Descuento '.
          05 DET-PORCENTAJE             PIC ZZ9,99.
          05 FILLER                     PIC X VALUE '%'.
          05 FILLER                     PIC X(21) VALUE SPACES.
          05 DET-DTO-PESOS              PIC --.---.---,--.
          05 FILLER                     PIC X.
          05 DET-DTO-DOLARES            PIC --.---.---,--.

       01 LINEA-TOTALES-1.
          05 FILLER                   PIC X(32) VALUE SPACES.
          05 FILLER                   PIC X(14) VALUE 'Saldo Actual: '.
          05 TOT-ACUM-PESOS           PIC $ZZ.ZZZ.ZZ9,99.
          05 FILLER                   PIC X(5) VALUE ' u$s '.
          05 TOT-ACUM-DOLARES         PIC $ZZ.ZZZ.ZZ9,99.

       01 LINEA-TOTALES-2.
          05 FILLER                   PIC X(32) VALUE SPACES.
          05 FILLER                   PIC X(14) VALUE 'Pago mínimo : '.
          05 TOTAL-PAGO-MIN           PIC $ZZ.ZZZ.ZZ9,99.

       01 LINEA-MENSAJE.
          05 FILLER                   PIC X(30) VALUE SPACES.
          05 FILLER                   PIC X(46) VALUE ALL '*'.

       01 LINEA-MENSAJE-1.
          05 FILLER                   PIC X(30) VALUE SPACES.
          05 FILLER                   PIC X(46) VALUE
             '*  Este mes ha superado su límite de compra  *' .
      ******************************************************************

       01 WS-CONTADORES.
          05 WS-CANT-REG-LEIDOS             PIC 9(8).
          05 WS-CANT-REG-GRABADOS           PIC 9(8).
          05 WS-CANT-REG-ERROR              PIC 9(8).

       01 WS-ACUMULADORES.
          05 WS-ACU-PESOS                   PIC 9(8)V9(02).
          05 WS-ACU-DOLAR                   PIC 9(8)V9(02).
          05 WS-ACU-TOTAL                   PIC 9(8)V9(02).

       01 WS-VARIABLES-AUXILIARES.
          05 WS-COTIZACION                  PIC 9(8)V9(02).
          05 WS-PORCENTAJE-DTO              PIC 9(3)V9(02).
          05 WS-TARJETA-ANTERIOR            PIC X(19).
          05 WS-LIMITE-ANTERIOR             PIC 9(8)V9(02).
          05 WS-IMPORTE-AUX                 PIC 9(8)V9(02).
          05 WS-IMPORTE-DTO                 PIC 9(8)V9(02).
          05 WS-PAGO-MIN                    PIC 9(8)V9(02).

       77 AUXILIAR  PIC S9(8)V99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-EXIT.

           IF FS-ENTRADA-OK

              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-ENTRADA-EOF-YES

              PERFORM 3000-CERRAR-ARCHIVOS
                 THRU 3000-CERRAR-ARCHIVOS-EXIT

             PERFORM 4000-TOTALES-ARCHIVO
                THRU 4000-TOTALES-ARCHIVO-EXIT

           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.

           INITIALIZE WS-CONTADORES
                      WS-ACUMULADORES.

           MOVE 360,55 TO WS-COTIZACION.
           MOVE 0,1 TO WS-PORCENTAJE-DTO.
           MULTIPLY WS-PORCENTAJE-DTO BY 100
                GIVING DET-PORCENTAJE.

      * CODIGO PARA ACEPTAR POR PANTALLA:
      *     DISPLAY '-----------------------------------'.
      *     DISPLAY 'INGRESE COTIZACION:'.
      *     ACCEPT WS-COTIZACION.
      *     DISPLAY '-----------------------------------'.
      *     DISPLAY 'INGRESE DESCUENTO:'.
      *     ACCEPT WS-PORCENTAJE-DTO.
      *     DISPLAY '-----------------------------------'.

      *------------APERTURA DE ARCHIVOS -------------------------------*
           OPEN INPUT ENTRADA.

           EVALUATE FS-ENTRADA
               WHEN '00'
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
               WHEN '35'
                    SET WS-ENTRADA-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    SET WS-ENTRADA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.
      *----------------------------------------------------------------*
           OPEN OUTPUT ERRORES.

           EVALUATE TRUE
               WHEN FS-ERRORES-OK
                   CONTINUE
               WHEN FS-ERRORES-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO CONSUMO'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO CONSUMO'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
           END-EVALUATE.
      *----------------------------------------------------------------*
           OPEN OUTPUT RESUMENES.

           EVALUATE TRUE
               WHEN FS-RESUMENES-OK
                    CONTINUE
               WHEN FS-RESUMENES-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO CONSUMO'
                    DISPLAY 'FILE STATUS: ' FS-RESUMENES
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO CONSUMO'
                    DISPLAY 'FILE STATUS: ' FS-RESUMENES
           END-EVALUATE.

       1000-INICIAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.

           READ ENTRADA
             AT END
                SET WS-ENTRADA-EOF-YES     TO TRUE
             NOT AT END
                SET WS-ENTRADA-EOF-NO      TO TRUE
                ADD 1 TO WS-CANT-REG-LEIDOS
           END-READ.
           EVALUATE FS-ENTRADA
               WHEN '00'
                    CONTINUE
               WHEN '10'
                    CONTINUE
               WHEN OTHER
                    SET WS-ENTRADA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL leer EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

           PERFORM 2100-VALIDAR-FECHA
              THRU 2100-VALIDAR-FECHA-FIN.

           IF WS-VALIDACION-O = "N"
              PERFORM 2150-MOVER-ERROR
                 THRU 2150-MOVER-ERROR-FIN
              PERFORM 1500-LEER-ARCHIVO
                 THRU 1500-LEER-ARCHIVO-EXIT
           ELSE
              MOVE ENT-NRO-TARJETA TO WS-MAE-NRO-TARJ
              CALL 'MAESTARJ' USING WS-REG-MAESTRO

              IF WS-MAE-NOMBRE = "ERROR"
                  MOVE ENT-ARCHIVOS TO DETALLE
                  MOVE "02" TO CODIGO-ERROR
                  MOVE WS-MAE-DIRECCION TO DESCRIPCION-ERR
                  PERFORM 2155-GRABAR-ERROR
                     THRU 2155-GRABAR-ERROR-FIN
                  PERFORM 1500-LEER-ARCHIVO
                     THRU 1500-LEER-ARCHIVO-EXIT
              ELSE
                 PERFORM 2200-PROCESAR-CORTE
                    THRU 2200-PROCESAR-CORTE-FIN
              END-IF

           END-IF.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-VALIDAR-FECHA.
           MOVE ENT-FECHA TO WS-FECHA-TXT.
           MOVE WS-DIA TO WS-DD-I.
           MOVE WS-MES TO WS-MM-I.
           MOVE WS-ANIO TO WS-AAAA-I.
           CALL 'CLVALFEC' USING WS-VALIDA.
       2100-VALIDAR-FECHA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2150-MOVER-ERROR.
           MOVE ENT-ARCHIVOS TO DETALLE.
           MOVE "01" TO CODIGO-ERROR.
           MOVE WS-DES-ERROR-O TO DESCRIPCION-ERR.
           PERFORM 2155-GRABAR-ERROR
              THRU 2155-GRABAR-ERROR-FIN.

       2150-MOVER-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2155-GRABAR-ERROR.
           WRITE REG-ERRORES.

           EVALUATE FS-ERRORES
               WHEN '00'
                    ADD 1 TO WS-CANT-REG-ERROR
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
           END-EVALUATE.

       2155-GRABAR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-CORTE.
           MOVE ENT-NRO-TARJETA TO WS-TARJETA-ANTERIOR.
           MOVE WS-MAE-LIMITE TO WS-LIMITE-ANTERIOR.
           PERFORM 2300-PROCESAR-ENCABEZADO
              THRU 2300-PROCESAR-ENCABEZADO-FIN.
           PERFORM 2250-CORTE-X-TARJETA
              THRU 2250-CORTE-X-TARJETA-FIN
              UNTIL WS-ENTRADA-EOF-YES OR
                ENT-NRO-TARJETA NOT EQUAL WS-TARJETA-ANTERIOR.
           PERFORM 2500-PROCESAR-TOTALES
              THRU 2500-PROCESAR-TOTALES-FIN.
           MOVE 0 TO WS-ACU-PESOS.
           MOVE 0 TO WS-ACU-DOLAR.

       2200-PROCESAR-CORTE-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2250-CORTE-X-TARJETA.

           IF WS-VALIDACION-O ='S'
              IF ENT-MONEDA="ARS"
                 ADD ENT-IMPORTE TO WS-ACU-PESOS
              ELSE
                 ADD ENT-IMPORTE TO WS-ACU-DOLAR
              END-IF
              PERFORM 2400-PROCESAR-DETALLE
                 THRU 2400-PROCESAR-DETALLE-FIN
           ELSE
               PERFORM 2150-MOVER-ERROR
                 THRU 2150-MOVER-ERROR-FIN
           END-IF.
           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

           PERFORM 2100-VALIDAR-FECHA
              THRU 2100-VALIDAR-FECHA-FIN.

       2250-CORTE-X-TARJETA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2300-PROCESAR-ENCABEZADO.
           MOVE WS-MAE-APELLIDO TO ENC-APELLIDO.
           MOVE WS-MAE-NOMBRE TO ENC-NOMBRE.
           MOVE WS-CURRENT-DAY TO ENC-DIA.
           MOVE WS-CURRENT-MONTH TO ENC-MES.
           MOVE WS-CURRENT-YEAR TO ENC-ANIO.
           MOVE WS-MAE-DIRECCION TO ENC-DIRECCION.
           MOVE WS-MAE-CUENTA TO ENC-CUENTA.
           MOVE WS-MAE-COD-POSTAL TO ENC-COD-POSTAL.
           MOVE WS-MAE-NRO-TARJ TO ENC-NRO-TARJ.
           MOVE WS-MAE-LIMITE TO ENC-LIMITE.
           MOVE ENCABEZADO-1 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE ENCABEZADO-2 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE ENCABEZADO-3 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE ENCABEZADO-4 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE LINEA-SEPARADOR TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE LINEA-TITULO TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE LINEA-SEPARADOR TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           ADD 7 TO WS-CANT-REG-GRABADOS.
       2300-PROCESAR-ENCABEZADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2400-PROCESAR-DETALLE.

           MOVE WS-DIA TO DET-DIA.
           MOVE WS-MES TO DET-MES.
           MOVE WS-ANIO TO DET-ANIO.
           MOVE ENT-DETALLE  TO DET-DETALLE.
           MOVE ENT-NRO-CUOTA TO DET-NRO-CUOTA.
           MOVE ENT-CANT-CUOTAS  TO DET-CANT-CUOTAS.

           IF ENT-MONEDA = "ARS"
              MOVE ENT-IMPORTE TO DET-IMPORTE-PESOS
              MOVE 0 TO DET-IMPORTE-DOLARES
           ELSE
              MOVE ENT-IMPORTE TO DET-IMPORTE-DOLARES
              MOVE 0 TO DET-IMPORTE-PESOS
           END-IF.

           MOVE LINEA-DETALLE TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           ADD 1 TO WS-CANT-REG-GRABADOS.

           IF WS-DESCUENTO-O = "S"
              MULTIPLY ENT-IMPORTE BY WS-PORCENTAJE-DTO
                GIVING WS-IMPORTE-DTO
              MULTIPLY WS-IMPORTE-DTO BY -1 GIVING AUXILIAR

              IF ENT-MONEDA = "ARS"

                 MOVE AUXILIAR TO DET-DTO-PESOS
                 MOVE 0 TO DET-DTO-DOLARES
                 SUBTRACT WS-IMPORTE-DTO FROM WS-ACU-PESOS
                 ELSE
                     MOVE AUXILIAR TO DET-DTO-DOLARES
                     MOVE 0 TO DET-DTO-PESOS
                     SUBTRACT WS-IMPORTE-DTO FROM WS-ACU-DOLAR
               END-IF

               MOVE LINEA-DETALLE-1 TO LINEA
               PERFORM 2600-GRABAR-RESUMENES
                   THRU 2600-GRABAR-RESUMENES-FIN
               ADD 1 TO WS-CANT-REG-GRABADOS
           END-IF.

       2400-PROCESAR-DETALLE-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2500-PROCESAR-TOTALES.
           MOVE 0 TO WS-IMPORTE-AUX.
           MOVE 0 TO WS-ACU-TOTAL.
           MOVE WS-ACU-PESOS TO TOT-ACUM-PESOS.
           MOVE WS-ACU-DOLAR TO TOT-ACUM-DOLARES.
           MULTIPLY WS-ACU-DOLAR BY WS-COTIZACION
                 GIVING WS-IMPORTE-AUX.
           ADD WS-ACU-PESOS TO WS-ACU-TOTAL.
           ADD WS-IMPORTE-AUX TO WS-ACU-TOTAL.
           MULTIPLY WS-ACU-TOTAL BY 0,05 GIVING WS-PAGO-MIN.
           MOVE WS-PAGO-MIN TO TOTAL-PAGO-MIN.
           MOVE LINEA-SEPARADOR TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE LINEA-TOTALES-1 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE LINEA-TOTALES-2 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           MOVE LINEA-SEPARADOR TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           ADD 4 TO WS-CANT-REG-GRABADOS.

           IF WS-ACU-TOTAL > WS-LIMITE-ANTERIOR
              MOVE LINEA-MENSAJE TO LINEA
              PERFORM 2600-GRABAR-RESUMENES
                 THRU 2600-GRABAR-RESUMENES-FIN
              MOVE LINEA-MENSAJE-1 TO LINEA
              PERFORM 2600-GRABAR-RESUMENES
                 THRU 2600-GRABAR-RESUMENES-FIN
              MOVE LINEA-MENSAJE TO LINEA
              PERFORM 2600-GRABAR-RESUMENES
                 THRU 2600-GRABAR-RESUMENES-FIN
              ADD 3 TO WS-CANT-REG-GRABADOS
           END-IF.

           MOVE LINEA-SEPARADOR1 TO LINEA.
           PERFORM 2600-GRABAR-RESUMENES
              THRU 2600-GRABAR-RESUMENES-FIN.
           ADD 1 TO WS-CANT-REG-GRABADOS.

       2500-PROCESAR-TOTALES-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2600-GRABAR-RESUMENES.
           WRITE REG-RESUMENES.

           EVALUATE FS-RESUMENES
               WHEN '00'
               CONTINUE

               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO RESUMEN'
                    DISPLAY 'FILE STATUS: ' FS-RESUMENES
           END-EVALUATE.

       2600-GRABAR-RESUMENES-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-CERRAR-ARCHIVOS.

           CLOSE ENTRADA.
           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR EN CLOSE DE ENTRADA: ' FS-ENTRADA
           END-IF.

           CLOSE ERRORES.
           IF NOT FS-ERRORES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO CONSUMO: ' FS-ERRORES
           END-IF.

           CLOSE RESUMENES.
           IF NOT FS-RESUMENES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO CONSUMO: ' FS-RESUMENES
           END-IF.

       3000-CERRAR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.

           DISPLAY '--------------------------------------'.
           DISPLAY 'CANTIDAD REG LEIDOS :  ' WS-CANT-REG-LEIDOS.
           DISPLAY 'CANTIDAD REG GRABADOS: ' WS-CANT-REG-GRABADOS.
           DISPLAY 'CANTIDAD REG CON ERROR:' WS-CANT-REG-ERROR.
           DISPLAY '--------------------------------------'.
           DISPLAY '          FIN DEL PROGRAMA            '.
           DISPLAY '--------------------------------------'.

       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM TP02EJ01.
