C
C program by Bob Brown
C 
C ENGR-4962 FORTRAN Project - Eigenmode Finder
C This program finds the natural frequencies and eigenmodes
C of a 3 degree of freedom spring-mass system with no applied force
C          ____       ____       ____
C  \| K1  |    | K2  |    | K3  |    |
C  \|-\\\-| M1 |-\\\-| M2 |-\\\-| M3 |
C  \|     |____|     |____|     |____|
C
C     variable    meaning
C     --------    -------
C     Mx          mass value for x system component
C     Kx          spring constant for x system component
C     APART       function subprogram that creates the w^6 term of
C                 the polynomial used to find the natural frequencies
C     BPART       function subprogram that creates the w^4 term of
C                 the polynomial used to find the natural frequencies
C     CPART       function subprogram that creates the w^2 term of
C                 the polynomial used to find the natural frequencies
C     DPART       function subprogram that creates the last term of
C                 the polynomial used to find the natural frequencies
C     A           value of APART in main for simplification
C     B           value of BPART in main for simplification
C     C           value of CPART in main for simplification
C     D           value of DPART in main for simplification
C     OMEGAx      function subprogram that finds the x root from the
C                 polynomial used to find the natural frequencies
C     FREQx       value of the x natural frequency
C     MODEONE     function subprogram that finds the first eigenmode
C     MODETHREE   function subprogram that finds the third eigenmode
C     MODETWOA    function subprogram that finds 1st part of the second
C                 eigenmode
C     MODETWOB    function subprogram that finds 3rd part of the second
C                 eigenmode
C     Uxy         value of the y component of the x eigenmode for
C                 output simplification
C     SQUARE      function subprogram that computes the square of a #
C     CUBE        function subprogram that computes the cube of a #
C     UROOT       function subprogram that computes a value used to
C                 find the roots of the polynomial used to find the
C                 natural frequencies
C     PERCENT1    function subprogram that computes a value used to 
C                 find the roots of the polynomial used to find the
C                 natural frequencies
C     CPLXPART    function subprogram that computes a value used to 
C                 find the roots of the polynomial used to find the
C                 natural frequencies
C     FIRSTPART   function subprogram that computes a value used to 
C                 find the roots of the polynomila used to find the 
C                 natural frequencies
C     AIMAG       complex conversion of A for calculations
C     BIMAG       complex conversion of B for calculations
C     CIMAG       complex conversion of C for calculations
C     DIMAG       complex conversion of D for calculations
C     PART1       value of 1st part of equation used in UROOT
C     PART2       value of 2nd part of equation used in UROOT
C     SUM         sum of PART1 and PART2 used in UROOT
C     ROOT3       value of SQRT(3) used for simplification
C     COMP1       value of 1st part of equation used in PERCENT1
C     COMP2       value of 2nd part of equation used in PERCENT2
C     EQ1         value of 1st part of equation used in OMEGA1
C     EQ2         value of 2nd part of equation used in OMEGA1
C     EQN1        value of 1st part of equation used in CPLXPART
C     EQN2        value of 2nd part of equation used in CPLXPART
C     FORM1       value of 1st part of equation used in FIRSTPART
C     FORM2       value of 2nd part of equation used in FIRSTPART
C     NUMx        numerator for eigenmode calculations
C     DENx        denominator for eigenmode calculations
C
C declarations of local variables
      REAL*8 M1,M2,M3,K1,K2,K3,APART,BPART,CPART,DPART,A,B,C,D
      REAL*8 OMEGA1,OMEGA2,OMEGA3,FREQ1,FREQ2,FREQ3,MODEONE,MODETHREE
      REAL*8 MODETWOA,MODETWOB,U12,U21,U23,U32
C
C begin program prompt
      PRINT *,'This program finds the natural frequencies and'
      PRINT *,'eigenmodes of a 3 degree of freedom spring-mass'
      PRINT *,'system with no applied force.'
      PRINT *,'        ____       ____       ____ '
      PRINT *,'\\| K1  |    | K2  |    | K3  |    |'
      PRINT *,'\\|-\\\\\\-| M1 |-\\\\\\-| M2 |-\\\\\\-| M3 |'
      PRINT *,'\\|     |____|     |____|     |____|'
      PRINT *, ' '
      PRINT *,'Only enter POSITIVE and REAL numbers,'
      PRINT *,'as there can never be a negative mass, and'
      PRINT *,'no spring can have a negative stiffness.'
C
C user input
      WRITE(6,901)
 901  FORMAT('Enter a value for mass 1: ',$)
      READ(5,*) M1
      WRITE(6,902)
 902  FORMAT('Enter a value for mass 2: ',$)
      READ(5,*) M2 
      WRITE(6,903)
 903  FORMAT('Enter a value for mass 3: ',$)
      READ(5,*) M3
      WRITE(6,904)
 904  FORMAT('Enter a value for the spring stiffness K1: ',$)
      READ(5,*) K1
      WRITE(6,905)
 905  FORMAT('Enter a value for the spring stiffness K2: ',$)
      READ(5,*) K2
      WRITE(6,906)
 906  FORMAT('Enter a value for the spring stiffness K3: ',$)
      READ(5,*) K3
C
C determination of equation components
      A=APART(M1,M2,M3)
      B=BPART(M1,M2,M3,K1,K2,K3)
      C=CPART(M1,M2,M3,K1,K2,K3)
      D=DPART(K1,K2,K3)
C
C frequency calculation
      FREQ1=DSQRT(OMEGA1(A,B,C,D))
      FREQ2=DSQRT(OMEGA2(A,B,C,D))
      FREQ3=DSQRT(OMEGA3(A,B,C,D))
C
C eigenmode calcuation
      U12=MODEONE(A,B,C,D,M1,K1,K2)
      U21=MODETWOA(A,B,C,D,M2,K2,K3)
      U23=MODETWOB(A,B,C,D,M2,K2,K3)
      U32=MODETHREE(A,B,C,D,M3,K3)
C
C output     
      PRINT *,' '
      PRINT *,'For the system, the natural frequencies are:'
      PRINT *,'first frequency= ',FREQ1
      PRINT *,'second frequency=',FREQ2
      PRINT *,'third frequency= ',FREQ3
      PRINT *,' '
      PRINT *,'and the eigenmodes for the system are:'
      PRINT *,'   |  1.00000000|'
      PRINT *,'u1=|',U12,'|'
      PRINT *,'   |  0.00000000|'
      PRINT *,' '
      PRINT *,'   |',U21,'|'
      PRINT *,'u2=|  1.00000000|'
      PRINT *,'   |',U23,'|'
      PRINT *,' '
      PRINT *,'   |  0.00000000|'
      PRINT *,'u3=|',U32,'|'
      PRINT *,'   |  1.00000000|'
      PRINT *,' '
      STOP
      END
C
C funciton area
      FUNCTION CUBE(X)
      REAL*8 CUBE,X
      CUBE=X**3
      RETURN
      END

      FUNCTION SQUARE(X)
      REAL*8 SQUARE,X
      SQUARE=X**2
      RETURN
      END

      FUNCTION APART(M1,M2,M3)
      REAL*8 APART,M1,M2,M3
      APART=-M1*M2*M3
      RETURN
      END
     
      FUNCTION BPART(M1,M2,M3,K1,K2,K3)
      REAL*8 BPART,M1,M2,M3,K1,K2,K3
      BPART=(M1*M2*K3)+(M1*M3*K2)+(M1*M3*K3)+(M2*M3*K1)+(M2*M3*K2)
      RETURN
      END

      FUNCTION CPART(M1,M2,M3,K1,K2,K3)
      REAL*8 CPART,CPART1,CPART2,M1,M2,M3,K1,K2,K3,SQUARE
      CPART1=(M1*SQUARE(K3))+(M2*K1*K3)+(M3*K1*K2)
      CPART2=(M3*K1*K3)+(2*M2*K2*K3)
      CPART=-(CPART1+CPART2)
      RETURN
      END

      FUNCTION DPART(K1,K2,K3)
      REAL*8 DPART,K1,K2,K3,SQUARE
      DPART=(K1*SQUARE(K3))+(K2*SQUARE(K3))-(SQUARE(K2)*K3)
      RETURN
      END

      FUNCTION UROOT(A,B,C,D)
      REAL*8 A,B,C,D,CUBE,SQUARE,PART1,PART2
      COMPLEX*16 UROOT,SUM
      PART1=(4*CUBE(C)*A)-(SQUARE(C)*SQUARE(B))-(18*C*B*A*D)
      PART2=(27*SQUARE(D)*SQUARE(A))+(4*D*CUBE(B))
      SUM=PART1+PART2
      UROOT=SQRT(SUM)
      RETURN
      END

      FUNCTION PERCENT1(A,B,C,D)
      REAL*8 A,B,C,D,ROOT3
      COMPLEX*16 AIMAG,BIMAG,CIMAG,DIMAG,PERCENT1,UROOT,COMP1,COMP2
      AIMAG=DCMPLX(A,0.D0)
      BIMAG=DCMPLX(B,0.D0)
      CIMAG=DCMPLX(C,0.D0)
      DIMAG=DCMPLX(D,0.D0)
      ROOT3=DSQRT(3.D0)
      COMP1=(36*CIMAG*BIMAG*AIMAG)-(108*DIMAG*AIMAG**2)-(8*BIMAG**3)
      COMP2=(12*ROOT3*UROOT(A,B,C,D)*AIMAG)
      PERCENT1=(COMP1+COMP2)**(1./3.)
      RETURN
      END

      FUNCTION OMEGA1(A,B,C,D)
      REAL*8 A,B,C,D,OMEGA1
      COMPLEX*16 AIMAG,BIMAG,CIMAG,DIMAG,PERCENT1,EQ1,EQ2
      AIMAG=DCMPLX(A,0.D0)
      BIMAG=DCMPLX(B,0.D0)
      CIMAG=DCMPLX(C,0.D0)
      DIMAG=DCMPLX(D,0.D0)
      EQ1=((1./6.)*(PERCENT1(A,B,C,D)/AIMAG))
      EQ2=(((3*CIMAG*AIMAG)-(BIMAG**2))/(AIMAG*PERCENT1(A,B,C,D)))
      OMEGA1=DREAL(ABS(EQ1-((2./3.)*EQ2)-((1./3.)*(BIMAG/AIMAG))))
      RETURN
      END
      
      FUNCTION CPLXPART(A,B,C,D)
      REAL*8 A,B,C,D,CPLXPART
      COMPLEX*16 AIMAG,BIMAG,CIMAG,DIMAG,PERCENT1,EQN1,EQN2
      AIMAG=DCMPLX(A,0.D0)
      BIMAG=DCMPLX(B,0.D0)
      CIMAG=DCMPLX(C,0.D0)
      DIMAG=DCMPLX(D,0.D0)
      EQN1=((1./6.)*(PERCENT1(A,B,C,D)/AIMAG))
      EQN2=((3*CIMAG*AIMAG)-(BIMAG**2))/(AIMAG*PERCENT1(A,B,C,D))
      CPLXPART=(1./2.)*DSQRT(3.D0)*(EQN1+((2./3.)*EQN2))*(0.D0,1.D0)
      RETURN
      END

      FUNCTION FIRSTPART(A,B,C,D)
      REAL*8 A,B,C,D,FIRSTPART
      COMPLEX*16 AIMAG,BIMAG,CIMAG,DIMAG,PERCENT1,FORM1,FORM2
      AIMAG=DCMPLX(A,0.D0)
      BIMAG=DCMPLX(B,0.D0)
      CIMAG=DCMPLX(C,0.D0)
      DIMAG=DCMPLX(D,0.D0)
      FORM1=(-(1./12.)*(PERCENT1(A,B,C,D)/AIMAG))
      FORM2=(((3*CIMAG*AIMAG)-(BIMAG**2))/(AIMAG*PERCENT1(A,B,C,D)))
      FIRSTPART=DREAL(FORM1+((1./3.)*FORM2)-((1./3.)*(BIMAG/AIMAG)))
      RETURN
      END
      
      FUNCTION OMEGA2(A,B,C,D)
      REAL*8 A,B,C,D,OMEGA2,FIRSTPART,CPLXPART
      OMEGA2=FIRSTPART(A,B,C,D)+CPLXPART(A,B,C,D)
      RETURN
      END

      FUNCTION OMEGA3(A,B,C,D)
      REAL*8 A,B,C,D,OMEGA3,FIRSTPART,CPLXPART
      OMEGA3=FIRSTPART(A,B,C,D)-CPLXPART(A,B,C,D)
      RETURN
      END

      FUNCTION MODEONE(A,B,C,D,M1,K1,K2)
      REAL*8 A,B,C,D,M1,K1,K2,OMEGA1,NUM1,DEN1,MODEONE
      NUM1=(-(OMEGA1(A,B,C,D))*M1)+K1+K2
      DEN1=K2
      MODEONE=NUM1/DEN1
      RETURN
      END

      FUNCTION MODETHREE(A,B,C,D,M3,K3)
      REAL*8 A,B,C,D,M3,K3,OMEGA3,NUM3,DEN3,MODETHREE
      NUM3=(-(OMEGA3(A,B,C,D))*M3)+K3
      DEN3=K3
      MODETHREE=NUM3/DEN3
      RETURN
      END

      FUNCTION MODETWOA(A,B,C,D,M2,K2,K3)
      REAL*8 A,B,C,D,M2,K2,K3,OMEGA2,NUM2,DEN2A,MODETWOA
      NUM2=(-(OMEGA2(A,B,C,D))*M2)+K2+K3
      DEN2A=K2
      MODETWOA=NUM2/DEN2A
      RETURN
      END

      FUNCTION MODETWOB(A,B,C,D,M2,K2,K3)
      REAL*8 A,B,C,D,M2,K2,K3,OMEGA2,NUM2,DEN2B,MODETWOB
      NUM2=(-(OMEGA2(A,B,C,D))*M2)+K2+K3
      DEN2B=K3
      MODETWOB=NUM2/DEN2B
      RETURN
      END

