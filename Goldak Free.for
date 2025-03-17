C      ######################################################################
C      #################      CAE Assistant Company          ################
C      ##############         www CAEassistant com              #############
C      ###########   Copy right by CAE Assistant Company    ###############
C      ######################################################################
C      ONLY the BUYER  of this package has permission to use its codes.
C	   Any distribution of this subroutine is illegal and will be prosecuted 
C      ######################################################################
C      ######################################################################
C      CAE Assisitant Services: 
C      Toturial Packages,Consultancy,Articles,Q&A,Video Gallery,Online Course
C      ######################################################################
C      Need help with your project? 
C      You can get initial free consultation from (Support CAEassistant com)
C      ######################################################################
      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,
     1 TIME,DTIME,CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,
     2 KSPT,KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,MATLAYO,LACCFLA)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3  FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),
     1 T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)

	  double precision T_tr !The parameter used to store and represent the critical temperature at which an element activates.
	  
	  !GETVRM: An Abaqus function that provides nodal values. The parameter 'TEMP' represents the nodal temperature, and 'ARRAY' represents the array that stores the nodal variables.
      CALL GETVRM('TEMP',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,MATLAYO,
     1 LACCFLA)
	  
	  !Storing the temperature value from the array in the parameter 'TEMP 1'
	  TEMP=ARRAY(1)
	  
	  ! Set the Critical Temperature
	  T_tr=1296.0
	  
	  !Field 1 determines whether the element is active or not. It is updated by USDFLD and called by Abaqus.
	  FIELD(1)=0.0
	  
	  !Now, we must check if the temperature is high enough to activate the element by setting the field variable and the user-defined state variable to 1.
	  if(TEMP .GE. T_tr)then
			
		FIELD(1)=1.0
		
		statev(1)=1.0
		
	  endif
		
	  FIELD(1)=STATEV(1)
	  
	  statev(1)=statev(1)
	  
	  
	  !Now, we must check if any error exists. To do so, we check the JRCD parameter.
      !It is a return code (0 – no error, 1 – output request error or all components of the output request are zero).
	  !If an error is detected, we must print the error along with the element number.
	  if(JRCD.NE.0)then
        WRITE(6,*)'REQUEST ERROR IN USDFLD FOR ELEMENT NUMBER',NOEL, 
     1  'INTEGRATION POINT BUMBER ' ,NPT
	  
	  endif


      RETURN
      END
	  