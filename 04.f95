PROGRAM seno
IMPLICIT NONE

REAL*8:: s, f, c, ap, acum, aa
REAL:: n, tol, ci, erp, era
INTEGER:: t, i, fa, j


        WRITE(*,*)"Ingresar valor de x:"
        WRITE(*,*)"sug: 0.3*PI=0.942477"
        READ(*,*)n

    	    s=sin(n)

        WRITE(*,*)" "
        WRITE(*,*)" Valor de la funci�n evaluada en x=",n
        WRITE(*,*)" sen(", n, ")=", s
      	WRITE(*,*)" "
 

        WRITE(*,*)"Ingresar cu�ntas cifras significativas desea utilizar:"
        READ(*,*)ci
        WRITE(*,*)" "
        WRITE(*,*)"Usando ",ci," cifras significativas:"
        WRITE(*,*)" "
 
		        tol=2.4 * (10**(2-ci))

        WRITE(*,*)"Tolerancia: ",tol,"%"
        WRITE(*,*)" "
        WRITE(*,*)"Aproximaci�n."
        WRITE(*,*)" "
        WRITE(*,*)"Ingresar n�mero de t�rminos de la serie:"
        READ(*,*)t

					f = 0.d0
					c = 1.d0
        			aa = 0.d0
			        t=1

       DO
         
		WRITE(*,*)"T�RMINOS ITERATIVOS."
		WRITE(*,*)"	"
		WRITE(*,*)"T�rmino: ",t
		WRITE(*,*)"	"

		        IF(t == 1)THEN
     
	                ap = 1.d0
                erp = ((s - ap)/s)*100.
                era = 0.

        WRITE(*,*)"Error relativo aproximado ~~NO APLICA~~."
        WRITE(*,*)" "

   	      ELSEIF (t > 1) THEN

    	           DO i = 0,t-1
               
   	                 fa = 1
                    
    	                     IF (i == 0) THEN

     	                        fa = 1

      	                   ELSE IF (i > 0) THEN

       	                      DO j = i,1,-1

       	                           fa = fa * j
	
        	                  END DO
	
           	              END IF

           	        f = ((-1)**i)*((n)**i/fa)
 
      	              acum = acum + f


    	                IF (t == 2) aa = 1.


     	        		IF (i == t-2) THEN

      	                       aa = acum + c

                  		END IF

	 		     WRITE(*,*)" "
 			     WRITE(*,*)"i=",i
 			     WRITE(*,*)"factorial =",fa
 			     WRITE(*,*)"Aproximacion= ",f
 			     WRITE(*,*)"Aproximacion anterior= ",aa
			     WRITE(*,*)"acum =",acum
			     WRITE(*,*)" "

               END DO

       		       	 ap = acum + c
         	    	 erp = ((s - ap)/s)*100.
        	       	 era = ((ap - aa)/ap)*100.

		WRITE(*,*)" "
		WRITE(*,*)"Aproximaci�n: ",ap
		WRITE(*,*)" "

       END IF

	WRITE(*,*)"Error relativo porcentual = ",erp,"%"
        WRITE(*,*)"Error relativo aproximado = ",era,"%"
        WRITE(*,*)"Valor de la funcion=",s
        WRITE(*,*)"Valor de la aproximaci�n=",ap
	WRITE(*,*)" "
        WRITE(*,*)"Tolerancia ",tol,"%"
        WRITE(*,*)" "
 

      		  IF(era /= 0 .and. era < tol .and. t > 1)THEN

         	       WRITE(*,*)"Aproximaci�n V�LIDA."

      		          EXIT

     		 ELSE

                WRITE(*,*)"Aproximaci�n INV�LIDA:"
                WRITE(*,*)"agregue terminos para ajustar."

        END IF

        WRITE(*,*)" "
  
                t = t + 1
                acum = 0


        END DO


END PROGRAM seno