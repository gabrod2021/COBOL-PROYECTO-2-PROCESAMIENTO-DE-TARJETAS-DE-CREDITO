      ******************************************************************
      *Author:  GABRIELA RODRIGUEZ                                     *
      *         CLAUDIA SANTILLAN                                      *
      *         GABY OSELLA                                            *
      *         GUILLERMO RODRIGUEZ                                    *
      *         EDDISON MORA GOMEZ                                     *
      * Date: 28/11/2023
      * Purpose: LECTURA MAESTRO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAESTARJ.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-TARJETA
           ASSIGN TO '../MAESTRO-TARJETAS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-TARJETAS
           RECORD KEY IS MAE-NRO-TARJ.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       COPY "MAESTRO-TARJETAS.CPY".

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-TARJETAS                   PIC X(2).
             88 FS-TARJETAS-FILE-OK            VALUE '00'.
             88 FS-TARJETAS-FILE-EOF           VALUE '10'.
             88 FS-TARJETAS-FILE-NFD           VALUE '35'.
             88 FS-TARJETAS-CLAVE-INV          VALUE '21'.
             88 FS-TARJETAS-CLAVE-DUP          VALUE '22'.
             88 FS-TARJETAS-CLAVE-NFD          VALUE '23'.

       77 WS-NRO                    PIC X(19).
       77 WS-ERROR                  PIC X(40).

       LINKAGE SECTION.
       01 LK-REG-MAESTRO.
          05 LK-MAE-CUENTA                  PIC 9(10).
          05 LK-MAE-NRO-TARJ                PIC X(19).
          05 LK-MAE-APELLIDO                PIC X(20).
          05 LK-MAE-NOMBRE                  PIC X(20).
          05 LK-MAE-DIRECCION               PIC X(40).
          05 LK-MAE-COD-POSTAL              PIC 9(4).
          05 LK-MAE-MONEDA                  PIC X(3).
          05 LK-MAE-LIMITE                  PIC 9(8)V99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING LK-REG-MAESTRO.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-TARJETAS-FILE-OK

                 PERFORM 2000-BUSCAR-TARJETA
                 THRU 2000-BUSCAR-TARJETA-FIN

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           EXIT PROGRAM.

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-TARJETA
              THRU 1100-ABRIR-TARJETA-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-TARJETA.

           OPEN INPUT ENT-TARJETA.

           EVALUATE TRUE
               WHEN FS-TARJETAS-FILE-OK
                    CONTINUE
               WHEN FS-TARJETAS-FILE-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO MAESTRO'
                    DISPLAY 'FILE STATUS: ' FS-TARJETAS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO MAESTRO'
                    DISPLAY 'FILE STATUS: ' FS-TARJETAS
           END-EVALUATE.

       1100-ABRIR-TARJETA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-BUSCAR-TARJETA.

           MOVE LK-MAE-NRO-TARJ TO MAE-NRO-TARJ.

           PERFORM 2100-LEER-TARJETA
              THRU 2100-LEER-TARJETA-FIN.

       2000-BUSCAR-TARJETA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-LEER-TARJETA.

           READ ENT-TARJETA KEY IS MAE-NRO-TARJ.

      *    DISPLAY REG-MAESTRO.
           EVALUATE TRUE
               WHEN FS-TARJETAS-FILE-OK
                    PERFORM 2105-MOVER-DATOS
                       THRU 2105-MOVER-DATOS-FIN
               WHEN FS-TARJETAS-CLAVE-INV
                   MOVE "ID INGRESADO ES INVALIDO" TO WS-ERROR
                   PERFORM 2200-DEVOLVER-VACIO
                       THRU 2200-DEVOLVER-VACIO-FIN
               WHEN FS-TARJETAS-CLAVE-DUP
                   MOVE "ID INGRESADO SE ENCUENTRA DUPLICADO"
                     TO WS-ERROR
                   PERFORM 2200-DEVOLVER-VACIO
                       THRU 2200-DEVOLVER-VACIO-FIN
               WHEN FS-TARJETAS-CLAVE-NFD
                   MOVE "ID INGRESADO NO EXISTE" TO WS-ERROR
                   PERFORM 2200-DEVOLVER-VACIO
                       THRU 2200-DEVOLVER-VACIO-FIN
               WHEN OTHER
                    MOVE 'ERROR AL LEER EL ARCHIVO MAESTRO'
                      TO WS-ERROR
                    PERFORM 2200-DEVOLVER-VACIO
                       THRU 2200-DEVOLVER-VACIO-FIN
           END-EVALUATE.

        2100-LEER-TARJETA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2105-MOVER-DATOS.

           MOVE MAE-CUENTA     TO LK-MAE-CUENTA.
           MOVE MAE-NOMBRE     TO LK-MAE-NOMBRE.
           MOVE MAE-APELLIDO   TO LK-MAE-APELLIDO.
           MOVE MAE-DIRECCION  TO LK-MAE-DIRECCION.
           MOVE MAE-COD-POSTAL TO LK-MAE-COD-POSTAL.
           MOVE MAE-MONEDA     TO LK-MAE-MONEDA.
           MOVE MAE-LIMITE     TO LK-MAE-LIMITE.

       2105-MOVER-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-DEVOLVER-VACIO.

           MOVE 0              TO LK-MAE-CUENTA.
           MOVE "ERROR"        TO LK-MAE-NOMBRE.
           MOVE SPACES         TO LK-MAE-APELLIDO.
           MOVE WS-ERROR       TO LK-MAE-DIRECCION.
           MOVE 0              TO LK-MAE-COD-POSTAL.
           MOVE SPACES         TO LK-MAE-MONEDA.
           MOVE 0              TO LK-MAE-LIMITE.

       2200-DEVOLVER-VACIO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.



       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-TARJETA.

           IF NOT FS-TARJETAS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO: ' FS-TARJETAS
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM MAESTARJ.
