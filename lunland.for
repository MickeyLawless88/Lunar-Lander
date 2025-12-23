C ****************************************************************
C *                  LUNAR LANDER SIMULATION                     *
C *              ANSI FORTRAN 77 Implementation                  *
C *          with Enhanced Character Graphics Display            *
C *                                                              *
C *             Mickey W. Lawless  Dec.21-2025                   *
C *          Compiler: Lahey Personal FORTRAN 77                 *
C *                                                              *
C ****************************************************************

      PROGRAM LUNARL
    
      IMPLICIT NONE
      INTEGER T, A, LANDST
      REAL H, V, V1, F, B, D
      
C     Clear screen and display title
    
      CALL CLRSCR
      WRITE(*,100)
100   FORMAT(/,
     >       '             LUNAR LANDING SIMULATION',/,
     >       '             ----- ------- ----------',/)
      WRITE(*,110)
110   FORMAT(' DO YOU WANT INSTRUCTIONS',/,
     >       ' (Type 0 for NO): ')
      READ(*,*) A
      
      IF (A .NE. 0) THEN
         CALL INSTRS
      END IF
      
C     Main game loop - restart point

200   CONTINUE
      
      WRITE(*,210)
210   FORMAT(//,' BEGINNING LANDING PROCEDURE.........',//,
     >       ' G O O D  L U C K ! ! !',///)
      
C     Initialize variables

      T = 0
      H = 500.0
      V = 50.0
      F = 120.0
      
C     Display header

      WRITE(*,220)
220   FORMAT(' SEC   FEET   SPEED   FUEL',5X,
     >       'PLOT OF DISTANCE',/)
      
C     Game loop - main landing sequence

300   CONTINUE
      
C     Display status with plot

      CALL DSPSTA(T, H, V, F)
      
C     Check if fuel remains

      IF (F .GT. 0.0) THEN
         WRITE(*,310) 
310      FORMAT(' ? ')
         READ(*,*) B
      ELSE
         B = 0.0
      END IF
      
C     Limit fuel burn rate

      IF (B .LT. 0.0) B = 0.0
      IF (B .GT. 30.0) B = 30.0
      IF (B .GT. F) B = F
      
C     Physics calculations

      V1 = V - B + 5.0
      F = F - B
      H = H - 0.5 * (V + V1)
      
C     Check for touchdown

      IF (H .LE. 0.0) THEN
         GOTO 400
      END IF
      
      T = T + 1
      V = V1
      
C     Check for out of fuel

      IF (F .LE. 0.0) THEN
         WRITE(*,320)
320      FORMAT(/,' ***OUT OF FUEL***',/)
         GOTO 350
      END IF
      
      GOTO 300
      
C     Out of fuel loop

350   CONTINUE
      CALL DSPSTA(T, H, V, F)
      B = 0.0
      V1 = V - B + 5.0
      F = F - B
      H = H - 0.5 * (V + V1)
      
      IF (H .LE. 0.0) THEN
         GOTO 400
      END IF
      
      T = T + 1
      V = V1
      GOTO 350
      
C     Touchdown sequence

400   CONTINUE
      
      WRITE(*,410)
410   FORMAT(/,' ****CONTACT****',/)
      
C     Calculate exact touchdown time and velocity

      H = H + 0.5 * (V + V1)
      
      IF (ABS(B - 5.0) .LT. 0.001) THEN
         D = H / V
      ELSE
         D = (-V + SQRT(V*V + H*(10.0 - 2.0*B))) / (5.0 - B)
      END IF
      
      V1 = V + (5.0 - B) * D
      
C     Display landing statistics

      WRITE(*,420) REAL(T) + D, V1, F
420   FORMAT('    TOUCHDOWN AT ',F6.2,' SECONDS.',/,
     >       '    LANDING VELOCITY = ',F6.1,' FT/SEC',/,
     >       '    ',F5.1,' UNITS OF FUEL REMAINING.',/)
      
C     Draw landing scene based on outcome

      LANDST = 0
      IF (ABS(V1) .LT. 0.01) THEN
         LANDST = 1
      ELSE IF (ABS(V1) .LT. 2.0) THEN
         LANDST = 2
      ELSE
         LANDST = 3
      END IF
      
      CALL DRAWLD(LANDST)
      
C     Display landing evaluation

      IF (LANDST .EQ. 1) THEN
         WRITE(*,430)
430      FORMAT(/,' CONGRATULATIONS!!!  A PERFECT LANDING!',/,
     >          ' YOUR LICENSE WILL BE RENEWED.........LATER.',/)
      ELSE IF (LANDST .EQ. 2) THEN
         WRITE(*,440)
440      FORMAT(/,' WELL......THAT WAS OK.  BUT NOT SPECTACULAR...',/)
      ELSE
         WRITE(*,450)
450      FORMAT(/,' ***** SORRY, BUT YOU BLEW IT KIDO!!!!!',/,
     >          ' THIS WAS OUR ONLY CHANCE TO MAKE CONTACT WITH',/,
     >          ' EXTRATERRESTRIAL BEINGS.  APPROPRIATE CONDOLENCES',/,
     >          ' MIGHT BE SENT TO YOUR NEXT OF KIN.',/)
      END IF
      
C     Ask for another mission

      WRITE(*,460)
460   FORMAT(/,' ANOTHER MISSION ANYONE',/,
     >       ' (Type 1 if YES): ')
      READ(*,*) A
      
      IF (A .EQ. 1) THEN
         CALL CLRSCR
         GOTO 200
      END IF
      
      WRITE(*,470)
470   FORMAT(/,' CONTROL OUT.',//)
      
      STOP
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE INSTRS                         *
C     *              Display game instructions                       *
C     ****************************************************************

      SUBROUTINE INSTRS

      IMPLICIT NONE
      CHARACTER*1 CH
      
      WRITE(*,100)
100   FORMAT(//,
     >       ' YOU ARE LANDING ON THE MOON AND HAVE TAKEN OVER MANUAL',
     >       /,
     >       ' CONTROL 500 FEET ABOVE A GOOD LANDING SPOT. YOU HAVE A',
     >       /,
     >       ' DOWNWARD VELOCITY OF 50 FT/SEC.  120 UNITS OF FUEL ',
     >       'REMAIN.',//,
     >       ' HERE ARE THE RULES THAT GOVERN YOUR SPACE VEHICLE:',/,
     >       ' (1) AFTER EACH SECOND, THE HEIGHT, VELOCITY, AND ',
     >       'REMAINING',/,
     >       '     FUEL WILL BE REPORTED.',/,
     >       ' (2) AFTER THE REPORT, A "?" WILL BE TYPED.  ENTER THE',
     >       /,
     >       '     NUMBER OF UNITS OF FUEL YOU WISH TO BURN DURING THE'
     >       ,/,
     >       '     NEXT SECOND.  EACH UNIT OF FUEL WILL SLOW YOUR ',
     >       'DESCENT',/,
     >       ' (3) THE MAXIMUM THRUST OF YOUR ENGINE IS 30 FT/SEC/SEC ',
     >       'OR',/,
     >       '     30 UNITS OF FUEL PER SECOND',/,
     >       ' (4) WHEN YOU CONTACT THE LUNAR SURFACE, YOUR DESCENT ',
     >       'ENGINE',/,
     >       '     WILL AUTOMATICALLY SHUT OFF AND YOU WILL BE GIVEN A'
     >       ,/,
     >       '     REPORT OF YOUR LANDING SPEED AND REMAINING FUEL',/,
     >       ' (5) IF YOU RUN OUT OF FUEL, THE "?" WILL NO LONGER ',
     >       'APPEAR',/,
     >       '     BUT YOUR SECOND BY SECOND REPORT WILL CONTINUE ',
     >       'UNTIL',/,
     >       '     YOU CONTACT LUNAR SURFACE.',//,
     >       ' HIT <RETURN> TO CONTINUE...')
      
      READ(*,'(A)') CH
      
      RETURN
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE DSPSTA                         *
C     *       Display status with distance plot indicator            *
C     ****************************************************************
      
      SUBROUTINE DSPSTA(T, H, V, F)
      
      IMPLICIT NONE
      INTEGER T, PLTPOS, I
      REAL H, V, F
      CHARACTER*80 LINE
      
C     Calculate plot position (29 to 80)
      
      PLTPOS = INT(H / 12.0) + 29
      IF (PLTPOS .GT. 80) PLTPOS = 80
      IF (PLTPOS .LT. 29) PLTPOS = 29
      
C     Build output line with plot indicator
      
      WRITE(LINE,100) T, H, V, F
100   FORMAT(I3,F8.0,F8.0,F8.0,'     I')
      
C     Add spaces and asterisk for plot
      
      DO 200 I = 29, PLTPOS-1
         LINE(I:I) = ' '
200   CONTINUE
      
      IF (PLTPOS .LE. 80) THEN
         LINE(PLTPOS:PLTPOS) = '*'
         WRITE(*,'(A)') LINE(1:PLTPOS)
      ELSE
         WRITE(*,'(A)') LINE(1:29)
      END IF
      
      RETURN
      
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE DRAWLD                         *
C     *         Draw lunar lander scene with proper alignment        *
C     ****************************************************************
      
      SUBROUTINE DRAWLD(STATUS)
      
      IMPLICIT NONE
      INTEGER STATUS, I, J, K
      CHARACTER*1 SCREEN(24,70)
      INTEGER STARX(9), STARY(9)
      CHARACTER*12 EARTH(3), MOON(3)
      INTEGER CRATX(6), CRATY(6)
      CHARACTER*1 CRATC(6)
      INTEGER ROCKX(6)
      
      DATA STARX /5, 12, 20, 28, 35, 42, 50, 58, 65/
      DATA STARY /1, 2, 1, 2, 1, 2, 1, 2, 1/
      DATA EARTH /'  .--. ', ' / o  \', ' \___/ '/
      DATA MOON  /'  ( )   ', ' / o \ ', ' \___/ '/
      DATA CRATX /10, 25, 25, 45, 45, 60/
      DATA CRATY /19, 19, 18, 19, 18, 19/
      DATA CRATC /'o', 'O', '^', 'O', '^', 'o'/
      DATA ROCKX /15, 16, 35, 36, 55, 56/
      
C     Initialize screen with spaces
      
      DO 100 I = 1, 24
         DO 110 J = 1, 70
            SCREEN(I,J) = ' '
110      CONTINUE
100   CONTINUE
      
C     Draw starfield
      
      DO 120 K = 1, 9
         SCREEN(STARY(K),STARX(K)) = '*'
120   CONTINUE
      
C     Draw Earth (starting at row 3, col 8)
      
      DO 130 I = 1, 3
         DO 140 J = 1, 7
            SCREEN(I+2,J+7) = EARTH(I)(J:J)
140      CONTINUE
130   CONTINUE
      
C     Draw Moon (starting at row 3, col 60)
      
      DO 150 I = 1, 3
         DO 160 J = 1, 7
            SCREEN(I+2,J+59) = MOON(I)(J:J)
160      CONTINUE
150   CONTINUE
      
C     Draw lunar surface
      
      DO 200 J = 1, 70
         SCREEN(20,J) = '='
         SCREEN(21,J) = '#'
         SCREEN(22,J) = '#'
         SCREEN(23,J) = 'X'
         SCREEN(24,J) = 'X'
200   CONTINUE
      
C     Add craters
      
      DO 210 K = 1, 6
         SCREEN(CRATY(K),CRATX(K)) = CRATC(K)
210   CONTINUE
      
C     Add rocks
      
      DO 220 K = 1, 6
         SCREEN(19,ROCKX(K)) = '^'
220   CONTINUE
      
      IF (STATUS .EQ. 1) THEN
C        Perfect landing - centered lander
         CALL DRWPRF(SCREEN)
      ELSE IF (STATUS .EQ. 2) THEN
C        Acceptable landing - tilted lander
         CALL DRWDMG(SCREEN)
      ELSE
C        Crash landing - explosion
         CALL DRWCSH(SCREEN)
      END IF
      
C     Display the screen
      
      DO 300 I = 1, 24
         WRITE(*,'(70A1)') (SCREEN(I,J), J=1,70)
300   CONTINUE
      
      RETURN
      END
      
      
C     ****************************************************************
C     *                 SUBROUTINE DRWPRF                            *
C     *           Draw perfect landing spacecraft                    *
C     ****************************************************************
      
      SUBROUTINE DRWPRF(SCREEN)
      
      IMPLICIT NONE
      CHARACTER*1 SCREEN(24,70)
      INTEGER R, C, I, J, K
      CHARACTER*17 BANNER
      CHARACTER*11 CRAFT(12)
      INTEGER DUSTX(6)
      
      DATA BANNER /'*** PERFECT! ***'/
      DATA CRAFT /'     |     ', '     |     ', '    /-\    ',
     >            '    | O |  ', '   /=====\ ', '   | [o] | ',
     >            '   | USA | ', '  /===H===\', '    (V)   ',
     >            '  /|     |\', ' [==]   [==]', '  .:.   .:. '/
      DATA DUSTX /28, 29, 30, 37, 38, 39/
      
      R = 10
      C = 33
      
C     Success banner
      DO 100 J = 1, 17
         SCREEN(R-2,C-8+J-1) = BANNER(J:J)
100   CONTINUE
      
C     Draw spacecraft using data statements
      DO 110 I = 1, 12
         DO 120 J = 1, 11
            IF (CRAFT(I)(J:J) .NE. ' ') THEN
               SCREEN(R-2+I,C-5+J-1) = CRAFT(I)(J:J)
            END IF
120      CONTINUE
110   CONTINUE
      
C     Add dust clouds
      DO 130 K = 1, 3
         SCREEN(R+11,DUSTX(K)) = '.'
         IF (DUSTX(K)+1 .LE. 30) SCREEN(R+11,DUSTX(K)+1) = ':'
130   CONTINUE
      DO 140 K = 4, 6
         SCREEN(R+11,DUSTX(K)) = '.'
         IF (DUSTX(K)+1 .LE. 39) SCREEN(R+11,DUSTX(K)+1) = ':'
140   CONTINUE
      
      RETURN
      END
      
      
C     ****************************************************************
C     *                 SUBROUTINE DRWDMG                            *
C     *            Draw damaged landing spacecraft                   *
C     ****************************************************************
      
      SUBROUTINE DRWDMG(SCREEN)
      
      IMPLICIT NONE
      CHARACTER*1 SCREEN(24,70)
      INTEGER R, C, I, J
      CHARACTER*10 CRAFT(11)
      INTEGER DEBX(4), DEBY(4)
      CHARACTER*1 DEBC(4)
      
      DATA CRAFT /'     .    ', '     /    ', '    /-\   ',
     >            '   / * |  ', '  /=====\ ', '  | USA | ',
     >            '  \=~==/  ', '   (/)    ', ' /|~   |\ ',
     >            '/==    ==\', '      [==]'/
      DATA DEBX /29, 40, 43, 44/
      DATA DEBY /19, 19, 20, 19/
      DATA DEBC /'-', '.', ':', 'o'/
      
      R = 11
      C = 33
      
C     Draw damaged spacecraft using data statements
      DO 100 I = 1, 11
         DO 110 J = 1, 10
            IF (CRAFT(I)(J:J) .NE. ' ') THEN
               SCREEN(R-1+I,C-4+J-1) = CRAFT(I)(J:J)
            END IF
110      CONTINUE
100   CONTINUE
      
C     Add scattered debris
      DO 120 I = 1, 4
         SCREEN(DEBY(I),DEBX(I)) = DEBC(I)
120   CONTINUE
      
      RETURN
      END
      
      
C     ****************************************************************
C     *                 SUBROUTINE DRWCSH                            *
C     *              Draw crash landing explosion                    *
C     ****************************************************************
      
      SUBROUTINE DRWCSH(SCREEN)
      
      IMPLICIT NONE
      CHARACTER*1 SCREEN(24,70)
      INTEGER R, C, I, J, K
      CHARACTER*10 EXPLO(5)
      CHARACTER*7 WRECK(6)
      CHARACTER*6 FIRE(2)
      CHARACTER*5 BOOM
      INTEGER DEBX(14), DEBY(14), CRATX(4), CRATY(4)
      CHARACTER*1 DEBC(14), CRATC(4)
      
      DATA EXPLO /' .  *  . *', '*  .  *  .', '  *  .  * ',
     >            '*  .  *  .', ' . *  . * '/
      DATA BOOM /'BOOM!'/
      DATA FIRE /' ~}{~ ', ' }~{  '/
      DATA WRECK /'\|/*\|/', '--XXX--', 'XXXXXXX', '\-X*X-/',
     >            '  /*|\ ', '       '/
      DATA DEBX /27, 28, 43, 44, 26, 27, 28, 42, 43, 44, 28, 29,
     >           41, 42/
      DATA DEBY /10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 12, 12,
     >           12, 12/
      DATA DEBC /'/', '-', '\', '-', '|', '[', 'U', 'S', ']', '|',
     >           '(', 'V', ')', '='/
      DATA CRATX /33, 34, 35, 36/
      DATA CRATY /18, 19, 19, 18/
      DATA CRATC /':', '.', '.', ':'/
      
      R = 12
      C = 35
      
C     Explosion cloud (rows R-4 to R-1)
      DO 100 I = 1, 5
         DO 110 J = 1, 10
            IF (EXPLO(I)(J:J) .NE. ' ') THEN
               SCREEN(R-5+I,C-5+J-1) = EXPLO(I)(J:J)
            END IF
110      CONTINUE
100   CONTINUE
      
C     BOOM text
      DO 120 J = 1, 5
         SCREEN(R-2,C-2+J-1) = BOOM(J:J)
120   CONTINUE
      
C     Fire (rows R to R+1)
      DO 130 I = 1, 2
         DO 140 J = 1, 6
            IF (FIRE(I)(J:J) .NE. ' ') THEN
               SCREEN(R-1+I,C-3+J-1) = FIRE(I)(J:J)
            END IF
140      CONTINUE
130   CONTINUE
      
C     Main wreckage (rows R+2 to R+5)
      DO 150 I = 1, 6
         DO 160 J = 1, 7
            IF (WRECK(I)(J:J) .NE. ' ') THEN
               SCREEN(R+1+I,C-3+J-1) = WRECK(I)(J:J)
            END IF
160      CONTINUE
150   CONTINUE
      
C     Scattered debris
      DO 170 K = 1, 14
         SCREEN(DEBY(K),DEBX(K)) = DEBC(K)
170   CONTINUE
      
C     Impact crater
      DO 180 K = 1, 4
         SCREEN(CRATY(K),CRATX(K)) = CRATC(K)
180   CONTINUE
      
C     Crater rim
      DO 190 J = 32, 37
         SCREEN(19,J) = '-'
190   CONTINUE
      
C     Ejected rocks
      SCREEN(18,27) = 'o'
      SCREEN(18,42) = 'o'
      SCREEN(19,26) = '^'
      SCREEN(19,43) = '^'
      
      RETURN
      END
      
      
C     ****************************************************************
C     *                    SUBROUTINE CLRSCR                         *
C     *                  Clear screen display                        *
C     ****************************************************************
      
      SUBROUTINE CLRSCR
      
      IMPLICIT NONE
      INTEGER I
      
C     Print blank lines to clear screen
      
      DO 100 I = 1, 6
         WRITE(*,*)
100   CONTINUE
      
      RETURN
     
      END