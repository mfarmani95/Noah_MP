

MODULE module_Noahlsm_utility 
 
        REAL, PARAMETER      :: CP = 1004.5, RD = 287.04, SIGMA = 5.67E-8,    &
                                CPH2O = 4.218E+3,CPICE = 2.106E+3,            &
                                LSUBF = 3.335E+5

	CONTAINS

	SUBROUTINE CALTMP(T1,SFCTMP, SFCPRS, ZLVL, Q2,                             & !I
                          TH2, T1V, TH2V, RHO ) 

        IMPLICIT NONE

        REAL, INTENT(IN)       :: Q2, T1, SFCTMP, SFCPRS, ZLVL
        REAL, INTENT(OUT)      :: TH2, T1V, TH2V, RHO 
        REAL                   :: T2V

        TH2 = SFCTMP + ( 0.0098 * ZLVL)
        T1V= T1 * (1.0+ 0.61 * Q2) 
        TH2V = TH2 * (1.0+ 0.61 * Q2)
        T2V = SFCTMP * ( 1.0 + 0.61 * Q2 )
        RHO = SFCPRS/(RD * T2V)

        END SUBROUTINE CALTMP

        SUBROUTINE CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE
  
        REAL, INTENT(IN)       :: SFCTMP, SFCPRS
        REAL, INTENT(OUT)      :: Q2SAT, DQSDT2
        REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                                  A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                                  EPSILON=0.622 
        REAL                   :: ES, SFCPRSX

!  Q2SAT: saturated mixing ratio 
        ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./SFCTMP) )
! convert SFCPRS from Pa to KPa 
        SFCPRSX = SFCPRS*1.E-3
        Q2SAT = EPSILON * ES / (SFCPRSX-ES)
! convert from  g/g to g/kg
        Q2SAT = Q2SAT * 1.E3
! Q2SAT is currently a 'mixing ratio'

! DQSDT2 is calculated assuming Q2SAT is a specific humidity
        DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2 

!DG Q2SAT needs to be in g/g when returned for SFLX
        Q2SAT = Q2SAT / 1.E3

        END SUBROUTINE CALHUM
 

END MODULE module_Noahlsm_utility

