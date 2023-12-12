
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL19EJ01a.

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENT-EMPLEADO
           ASSIGN TO '../EMPLEADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-EMPLEADO.



      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       FD ENT-EMPLEADO.
       01 REG-ENT-EMPLEADO.
          05 ENT-EMP-ID                    PIC 9(08).
          05 ENT-EMP-NOMBRE                PIC X(25).
          05 ENT-EMP-APELLIDO              PIC X(25).
          05 ENT-EMP-ESTADO                PIC X(02).




       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS.
          02 FS-EMPLEADO                   PIC X(02) VALUE '00'.
             88 FS-EMPLEADOS-OK                      VALUE '00'.
             88 FS-EMPLEADOS-EOF                     VALUE '10'.
             88 FS-EMPLEADOS-NFD                     VALUE '35'.



        01 WS-CONTADORES.
           02 WS-CONT-REG-EMP            PIC 9(05) VALUE ZEROS.
           02 WS-TOTAL-EMP               PIC 9(05) VALUE ZEROS.
           02 WS-INICIO                  PIC 9(05) VALUE ZEROS.
           02 WS-FIN                     PIC 9(05) VALUE ZEROS.
           02 WS-MITAD                   PIC 9(05) VALUE ZEROS.
           02 WS-IND-ENC                 PIC 9(05) VALUE ZEROS.

            01 WS-VARIABLES.
           02 WS-BUSCAR.
              05 SW-ENCONTRO-SEC         PIC X(01) VALUE SPACE.
                 88 SW-ENCONTRO-SEC-NO   VALUE 'N'.
                 88 SW-ENCONTRO-SEC-SI   VALUE 'S'.
           05 WS-VALIDAR-ORDEN               PIC X(2).
               88 WS-ORDENADO-SI                      VALUE 'SI'.
               88 WS-ORDENADO-NO                      VALUE 'NO'.

           02 WS-LEGAJO-AUX              PIC 9(08) VALUE ZEROS.
           02 WS-ESTADO-AUX              PIC X(02) VALUE SPACES.
           02 WS-ESTADO-AUX2             PIC X(02) VALUE SPACES.
           02 WS-REG-ORDENADO            PIC X(60) VALUE SPACES.
           02 WS-TAM                     PIC 9(4)  VALUE 16.
           02 WS-J                       PIC 9(2) VALUE 0.
           02 WS-II                      PIC 9(2) VALUE 0.
        01 WS-LISTA-EMPLEADO.
           05 WS-EMPLEADO OCCURS 16 TIMES
              ASCENDING WS-EMP-LEGAJO
               INDEXED BY WS-I.
              10 WS-EMP-DATO.
                 15 WS-EMP-LEGAJO          PIC 9(08) VALUE ZEROS.
                 15 WS-EMP-NOMBRE          PIC X(25) VALUE SPACES.
                 15 WS-EMP-APELLIDO        PIC X(25) VALUE SPACES.
                 15 WS-EMP-ESTADO          PIC X(02) VALUE SPACES.


       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.

           PERFORM 2000-PROCESAR-ARCHIVO
              THRU 2000-PROCESAR-ARCHIVO-EXIT.

           PERFORM 3000-BUSCAR-LEGAJO
              THRU 3000-BUSCAR-LEGAJO-EXIT.

            PERFORM 4000-BUSCAR-ESTADO
             THRU 4000-BUSCAR-ESTADO-EXIT.



           PERFORM 8000-FINALIZAR
              THRU 8000-FINALIZAR-EXIT.



           STOP RUN.

      *----------------------------------------------------------------*
      * Proceso de iniciliazacion de programa
      *----------------------------------------------------------------*
       1000-INICIAR.

           PERFORM 1100-ABRIR-EMPELADO
              THRU 1100-ABRIR-EMPLEADO-EXIT.

       1000-INICIAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Abrir archivo empleados
      *----------------------------------------------------------------*
       1100-ABRIR-EMPELADO.

           OPEN INPUT ENT-EMPLEADO.

           EVALUATE FS-EMPLEADO
               WHEN '00'
                    SET FS-EMPLEADOS-OK       TO TRUE
                    PERFORM 1110-LEER-EMPELADO
                       THRU 1110-LEER-EMPELADO-EXIT
               WHEN '35'
                    SET FS-EMPLEADOS-NFD       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADO
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    SET FS-EMPLEADOS-EOF       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADO
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       1100-ABRIR-EMPLEADO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Leer archivo empleados
      *----------------------------------------------------------------*
       1110-LEER-EMPELADO.

           READ ENT-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    ADD 1                      TO WS-CONT-REG-EMP
               WHEN FS-EMPLEADOS-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEE EL ARCHIVO DE EMPLEADO'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADO
           END-EVALUATE.
       1110-LEER-EMPELADO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Procesar archivo de empleado: Cargar registro en tabla interna
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

      * Cargar el archivo en una tabla interna
          DISPLAY 'CANTIDAD DE REG-ENT-EMPLEADO ' WS-CONT-REG-EMP
           PERFORM  VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > WS-CONT-REG-EMP

              MOVE REG-ENT-EMPLEADO TO WS-EMP-DATO(WS-I)
              DISPLAY WS-EMP-LEGAJO(WS-I)


              PERFORM 1110-LEER-EMPELADO
                 THRU 1110-LEER-EMPELADO-EXIT
           END-PERFORM.

           MOVE WS-I    TO WS-TOTAL-EMP.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Buscar legajo
      *----------------------------------------------------------------*
       3000-BUSCAR-LEGAJO.

           DISPLAY '--- BUSCAR LEGAJO ---------------------------------'
           DISPLAY 'Ingesar numero de legajo(8 DIGITOS): '
           ACCEPT WS-LEGAJO-AUX.

           PERFORM 3100-BUSCAR-BI-LEGAJO
              THRU 3100-BUSCAR-BI-LEGAJO-EXIT.

       3000-BUSCAR-LEGAJO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Busqueda binaria
      *----------------------------------------------------------------*
       3100-BUSCAR-BI-LEGAJO.

           DISPLAY '-------- Busqueda Binaria ----------'
      *
      *    Inicializar variables para la busqueda binaria

      *   ** COMPLETAR VARIABLES INDECE DE INICIO Y FIN ************
           MOVE   1               TO WS-INICIO
           MOVE WS-TOTAL-EMP      TO WS-FIN
           SET SW-ENCONTRO-SEC-NO TO TRUE
      *
      *    Recorrer tabla interna de Empleados
           PERFORM   UNTIL WS-INICIO > WS-FIN
                        OR SW-ENCONTRO-SEC-SI

      *      Calcular la mitad del vector WS-EMPLEADO(X)
      *       Utilizando las variables  WS-INICIO y WS-FIN,
      *       Guardar el resultado en la variable WS-MITAD

      *      DIVIDE  XXXX    BY XXX        GIVING XXXXX
              ADD WS-INICIO TO WS-FIN  GIVING WS-MITAD
              DIVIDE WS-MITAD  BY 2 GIVING WS-MITAD
      ************************************************************
              DISPLAY 'WS-COMIENZO ' WS-INICIO
              DISPLAY 'WS-FIN      ' WS-FIN
              DISPLAY 'WS-MITAD    ' WS-MITAD
      *
      *      Verifica si se encontro el Legajo
              DISPLAY 'WS-EMP-LEGAJO    ' WS-EMP-LEGAJO(WS-MITAD)
              DISPLAY 'WS-LEGAJO-AUX    ' WS-LEGAJO-AUX

              IF WS-EMP-LEGAJO(WS-MITAD) EQUAL WS-LEGAJO-AUX THEN
      *           Encontro Legajo
                  SET SW-ENCONTRO-SEC-SI TO TRUE
                  MOVE WS-MITAD   TO WS-IND-ENC
      *           Verifica Si el Legajo es mayor
              ELSE IF WS-EMP-LEGAJO(WS-MITAD) > WS-LEGAJO-AUX THEN
      *               Recorro el lado menor
      *               Setear el nuvo valor de la variable fin del vector
                      ADD -1 TO WS-MITAD
                      MOVE WS-MITAD TO WS-FIN
                 ELSE
      *               Recorro el lado mayor
      *               Setear el nuvo valor de la variable inicio del vector
                      ADD 1  TO WS-MITAD
                      MOVE WS-MITAD TO WS-INICIO
                 END-IF
              END-IF

              DISPLAY 'NEW WS-COMIENZO ' WS-INICIO
              DISPLAY 'NEW WS-FIN      ' WS-FIN
              DISPLAY '--------------------------'
           END-PERFORM.

      *> *    Verifica si se encontro o no encontro el Legajo buscado
      *>       IF SW-ENCONTRO-SEC-SI   THEN
      *>         DISPLAY 'Elemento encontrado: '  WS-EMPLEADO(WS-IND-ENC)
      *>      ELSE
      *>         DISPLAY 'No se encontro el Elemento: ' WS-LEGAJO-AUX
      *>      END-IF.
       3100-BUSCAR-BI-LEGAJO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Buscar por estados
      *----------------------------------------------------------------*
       4000-BUSCAR-ESTADO.

           DISPLAY '--- BUSCAR ESTADO ---------------------------------'
           DISPLAY 'Ingesar Codigo de estado : '
           ACCEPT WS-ESTADO-AUX.



           PERFORM 4100-BUSCAR-SEC
              THRU 4100-BUSCAR-SEC-EXIT.

       4000-BUSCAR-ESTADO-EXIT.

             EXIT.

      *----------------------------------------------------------------*
      * Busqueda secuencial
      *----------------------------------------------------------------*
       4100-BUSCAR-SEC.

           DISPLAY '-------- Busqueda Secuencial  ----------'.


           SET SW-ENCONTRO-SEC-NO TO TRUE


           PERFORM  VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > WS-CONT-REG-EMP
              IF WS-EMP-ESTADO(WS-I) EQUAL WS-ESTADO-AUX THEN
                SET SW-ENCONTRO-SEC-SI TO TRUE
                   DISPLAY WS-EMPLEADO(WS-I)
              END-IF
                 MOVE WS-I   TO WS-IND-ENC
           END-PERFORM.

           IF SW-ENCONTRO-SEC-SI THEN
             DISPLAY "Estado encontrado: " WS-ESTADO-AUX
             ELSE
             DISPLAY "Estado inexistente " WS-ESTADO-AUX
             END-IF.


       4100-BUSCAR-SEC-EXIT.
           EXIT.


      *----------------------------------------------------------------*
       4050-ORDENAR-X-ESTADO.


      *---- LEO TODO EL ARCHIVO Y LO GUARDO EN UN OCCURS
           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II >
           WS-CONT-REG-EMP OR WS-ORDENADO-SI


              PERFORM 1110-LEER-EMPELADO
                 THRU 1110-LEER-EMPELADO-EXIT

           END-PERFORM.

      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA EMPLEADOS
      *----USAR VARIABLES WS-II PARA INDICE Y WS-VAR-AUX2 COMO AUXILIAR
           MOVE "NO" TO  WS-VALIDAR-ORDEN
      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA
           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II >
                           WS-CONT-REG-EMP OR WS-ORDENADO-SI
      *----PARA UNA PASADA ASUMO QUE ESTA ORDENADA
              MOVE 'SI'                        TO WS-VALIDAR-ORDEN

              PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J >
                                     (WS-CONT-REG-EMP- WS-II)
      *----SI EN UNA PASADA COMPLETA NO ENTRA EN EL IF, ESTA ORDENADA
                  IF WS-EMP-ESTADO(WS-J) >  WS-EMP-ESTADO(WS-J + 1)
      *---- AL DETECTAR UN DESORDEN SETEO EN NO, PORQUE SEGURO TENGO
      *---- QUE HACER OTRA PASADA.
                      MOVE 'NO'              TO WS-VALIDAR-ORDEN

                      MOVE WS-EMP-ESTADO(WS-J) TO WS-ESTADO-AUX2

                      MOVE WS-EMP-ESTADO(WS-J + 1)
                                            TO WS-EMP-ESTADO(WS-J)

                      MOVE WS-ESTADO-AUX2     TO
                                              WS-EMP-ESTADO(WS-J + 1)
                      DISPLAY WS-EMP-ESTADO(WS-J)

                  END-IF

              END-PERFORM
           END-PERFORM.

           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II >
                                                 WS-CONT-REG-EMP

              DISPLAY 'ESTADO ' WS-EMPLEADO (WS-II)
           END-PERFORM.

       4050-ORDENAR-X-ESTADO-EXIT.


      *----------------------------------------------------------------*
      * Proceso de finalizacion de archivo
      *----------------------------------------------------------------*
       8000-FINALIZAR.

           PERFORM 8100-CERRAR-ARCH-EMPLEADO
              THRU 8100-CERRAR-ARCH-EMPLEADO-EXIT.

       8000-FINALIZAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Cerrar archivo
      *----------------------------------------------------------------*
       8100-CERRAR-ARCH-EMPLEADO.
           CLOSE ENT-EMPLEADO.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR EN CLOSE DE ENT-EMPLEADO: ' FS-EMPLEADO
           END-IF.

       8100-CERRAR-ARCH-EMPLEADO-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM CL19EJ01a.
