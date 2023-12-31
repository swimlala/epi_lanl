CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:37Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        ,  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  A(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  GT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  O   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  V�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  ^|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  d�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  l`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ,  t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  zD   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    zt   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    }t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181005191737  20181005191737  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���c��z1   @�����@5��"��`�d�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Ca�fCd  Cf  Cg�fCj�Cl  Cm�fCp  Cq�fCt  Cv�Cx�Cz�C|�C~  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C�  C�  C��3C��C�  C�  C��C��C�  C��3C�  C��C��C�  C�  C�  C��3C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C��C�  C��C�  C��C�  C�  C��C��3C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��fC��3C�  C�  C�  C��C��C��C�  C�  C��3C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3D y�DfD�fDfD��DfD�fDfD� D  D� DfD�fD  D� D  D�fD	fD	�fD
  D
� DfD�fDfD�fD��D� D��D� D  D� DfD�fDfD� D  D�fD  Dy�D  D� DfD�fDfD� D  D� D��D� D  Dy�D�3Dy�D  D� DfD�fDfD�fD  D� D  D� D fD �fD!  D!� D"fD"� D#  D#� D#��D$y�D$��D%� D&  D&� D'  D'�fD(  D(y�D)  D)� D*  D*� D*��D+y�D+��D,y�D,��D-� D.fD.� D/  D/� D0  D0�fD1fD1�fD2  D2� D3fD3� D4  D4� D5  D5�fD6  D6y�D7  D7� D8  D8�fD9  D9y�D9��D:y�D;  D;� D<fD<� D=  D=y�D=��D>y�D?  D?� D@  D@� DA  DA�fDB  DB� DC  DCy�DD  DD�fDEfDy�fD�Eq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@\AG�A!G�AAG�AaG�A���A���A���A���A��
AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B���B���B�(�B�(�B�(�B�(�B�\)B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C��C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CA��CD{CF{CH{CJ.CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^.C`{Ca��Cd{Cf{Cg��Cj.Cl{Cm��Cp{Cq��Ct{Cv.Cx.Cz.C|.C~{C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
C��pC��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC��pC�
=C��pC��pC�
=C�
=C��pC�
C�
=C�
=C�
C�
C�
=C��pC�
=C�
C�
C�
=C�
=C�
=C��pC�
C�
=C��pC�
=C�
=C�
=C�
=C�
=C��pC�
=C��pC�
=C�
=C�
=C�
=C�
=C�
C�
=C�
C�
=C�
C�
=C�
=C�
C��pC�
=C��pC��pC��pC�
=C�
C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C��C��pC�
=C�
=C�
=C�
C�
C�
C�
=C�
=C��pC�
C�
=C�
=C��pC��pC�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C��pC��pC��pC�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C��pC��pD ~�D�D��D�D��D�D��D�D�DD�D�D��DD�DD��D	�D	��D
D
�D�D��D�D��D��D�D��D�DD�D�D��D�D�DD��DD~�DD�D�D��D�D�DD�D��D�DD~�D�RD~�DD�D�D��D�D��DD�DD�D �D ��D!D!�D"�D"�D#D#�D#��D$~�D$��D%�D&D&�D'D'��D(D(~�D)D)�D*D*�D*��D+~�D+��D,~�D,��D-�D.�D.�D/D/�D0D0��D1�D1��D2D2�D3�D3�D4D4�D5D5��D6D6~�D7D7�D8D8��D9D9~�D9��D:~�D;D;�D<�D<�D=D=~�D=��D>~�D?D?�D@D@�DADA��DBDB�DCDC~�DDDD��DE�Dy��D�H 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A�  A�A�  A�  A���A�t�A�K�A� �A��HAĕ�AăA�ZA�G�A�;dA�7LA�+A�VA�A���Aá�A�jA�5?A� �A�{A��A7A�A�p�A�M�A��A��A��jA�`BA���A��!A�+A���A���A�|�A���A��/A�l�A�5?A�v�A���A��A��/A�ĜA��A�1A�\)A��`A�O�A��^A��9A��jA��A��+A��#A� �A��RA�t�A�/A�  A���A�A�A���A�A���A��A�n�A�`BA�?}A��
A�1A�-A��9A��jA�%A�7LA��/A��HA�&�A���A���A��RA��A�r�A� �A�XA�\)A��\A��A�x�A��DA��A�dZA��wA���A�hA~JA{C�Ax�\Av�AuO�At�As��ArffAp9XAo\)AmAlffAk�Ak7LAjz�Ah��Ag/Afr�Ae�-Ad�`Ac��Ab��Aa�wA_VA^1'A]�A]��A]XA\A�AZ��AYƨAXĜAW�AV^5AU�AT��AS|�AR�AQ�7APz�AO��AN��AM��AJ�AI�FAHn�AGx�AE�hAD(�AC33AC
=AB��AA�A?l�A=�;A<�9A:jA:-A9�wA8��A6�A4��A3�A2��A2A�A1�TA1�A0  A.�HA.M�A-ƨA,�A(^5A&�9A&ffA& �A%��A%S�A#;dA ��A��AC�A|�A�A1'A��Al�A;dA�jA�AS�A�A�HA~�A  A?}AQ�A�uA�mA|�A1'A��AA-A�A1'A�FA  A
=qA	�mA	�wA	C�A�+Ax�A1A;dAjA��AE�A�A �+A �@�;d@�5?@��T@��@��@���@�@�?}@��@��@� �@���@��;@��@�D@���@�ƨ@��@���@�{@��@��m@�+@�p�@�|�@�=q@�j@��y@��T@�V@� �@��y@�&�@�"�@݉7@۝�@��y@ى7@���@ץ�@ՙ�@�&�@Ӯ@ҏ\@Ѳ-@Л�@���@�/@˝�@ɑh@�`B@�&�@���@�Ĝ@�b@���@�S�@Ł@�A�@�;d@�@�V@�=q@�-@�$�@�{@�@���@��@�r�@�Q�@��@���@��
@�K�@���@��\@���@�7L@�z�@�I�@�t�@�+@�@���@��+@�{@�@��/@�bN@�@���@�@���@�{@�V@��H@��@��y@�x�@�?}@��/@�(�@�t�@��H@��R@��\@��@��@��\@�$�@���@�%@��@���@��@�M�@�J@��@���@�/@���@��/@���@���@���@�Ĝ@��9@�A�@��@���@�~�@�V@�@���@�G�@�V@���@�bN@�1@��
@��y@��!@���@���@�n�@�5?@��#@�?}@�bN@�(�@��@��m@��@���@�;d@��@��@�O�@�Ĝ@��u@�z�@�  @���@�K�@��@�ȴ@�ȴ@q��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A�  A�A�  A�  A���A�t�A�K�A� �A��HAĕ�AăA�ZA�G�A�;dA�7LA�+A�VA�A���Aá�A�jA�5?A� �A�{A��A7A�A�p�A�M�A��A��A��jA�`BA���A��!A�+A���A���A�|�A���A��/A�l�A�5?A�v�A���A��A��/A�ĜA��A�1A�\)A��`A�O�A��^A��9A��jA��A��+A��#A� �A��RA�t�A�/A�  A���A�A�A���A�A���A��A�n�A�`BA�?}A��
A�1A�-A��9A��jA�%A�7LA��/A��HA�&�A���A���A��RA��A�r�A� �A�XA�\)A��\A��A�x�A��DA��A�dZA��wA���A�hA~JA{C�Ax�\Av�AuO�At�As��ArffAp9XAo\)AmAlffAk�Ak7LAjz�Ah��Ag/Afr�Ae�-Ad�`Ac��Ab��Aa�wA_VA^1'A]�A]��A]XA\A�AZ��AYƨAXĜAW�AV^5AU�AT��AS|�AR�AQ�7APz�AO��AN��AM��AJ�AI�FAHn�AGx�AE�hAD(�AC33AC
=AB��AA�A?l�A=�;A<�9A:jA:-A9�wA8��A6�A4��A3�A2��A2A�A1�TA1�A0  A.�HA.M�A-ƨA,�A(^5A&�9A&ffA& �A%��A%S�A#;dA ��A��AC�A|�A�A1'A��Al�A;dA�jA�AS�A�A�HA~�A  A?}AQ�A�uA�mA|�A1'A��AA-A�A1'A�FA  A
=qA	�mA	�wA	C�A�+Ax�A1A;dAjA��AE�A�A �+A �@�;d@�5?@��T@��@��@���@�@�?}@��@��@� �@���@��;@��@�D@���@�ƨ@��@���@�{@��@��m@�+@�p�@�|�@�=q@�j@��y@��T@�V@� �@��y@�&�@�"�@݉7@۝�@��y@ى7@���@ץ�@ՙ�@�&�@Ӯ@ҏ\@Ѳ-@Л�@���@�/@˝�@ɑh@�`B@�&�@���@�Ĝ@�b@���@�S�@Ł@�A�@�;d@�@�V@�=q@�-@�$�@�{@�@���@��@�r�@�Q�@��@���@��
@�K�@���@��\@���@�7L@�z�@�I�@�t�@�+@�@���@��+@�{@�@��/@�bN@�@���@�@���@�{@�V@��H@��@��y@�x�@�?}@��/@�(�@�t�@��H@��R@��\@��@��@��\@�$�@���@�%@��@���@��@�M�@�J@��@���@�/@���@��/@���@���@���@�Ĝ@��9@�A�@��@���@�~�@�V@�@���@�G�@�V@���@�bN@�1@��
@��y@��!@���@���@�n�@�5?@��#@�?}@�bN@�(�@��@��m@��@���@�;d@��@��@�O�@�Ĝ@��u@�z�@�  @���@�K�@��@�ȴ@�ȴ@q��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbNBbNBbNBbNBbNBbNBbNBbNBjB}�B�7B�hB��B�9B�LB�jB�jB�jB�qB��BĜBŢBŢBŢBÖBÖBÖBÖBĜBÖBŢBƨBŢBŢBB�}B�jB�}B��BÖBɺBÖB��B�jB�jB�qB��B��BƨB�wB�dB�XB�FB�-B�B��B��B�hB�=B�B~�Bv�Bo�BffBaHB]/BYBS�BE�B'�B �B �B�B�B�B�B�BoB	7B��B�NB��BƨB�^B��B�\B�Bz�Bn�BhsB^5BR�BM�BA�B2-B�B�B�B	7B
��B
�B
��B
�{B
�B
v�B
bNB
K�B
A�B
7LB
1'B
)�B
!�B
uB
VB
%B	��B	��B	�B	�B	�TB	�B	��B	��B	ƨB	�qB	�FB	�B	��B	��B	��B	��B	�{B	�PB	�B	}�B	v�B	q�B	hsB	cTB	^5B	VB	L�B	I�B	C�B	>wB	9XB	33B	$�B	�B	�B	{B	PB		7B	%B	B	B��B�B�B�B�`B�ZB�NB�5B�
B��B��B��BɺBŢBÖB��B�}B�qB�^B�9B�B�B��B��B��B��B��B�{B�oB�\B�PB�=B�=B�1B�+B�%B�B�B� B}�Bz�By�Bx�Bv�Bs�Bq�Bo�Bm�Bk�BiyBhsBffBiyBr�Bn�BgmBaHB_;B^5B\)BYBVBS�BR�BQ�BP�BN�BM�BM�BL�BL�BL�BL�BK�BK�BK�BK�BK�BK�BJ�BJ�BK�BI�BK�BP�BQ�BR�BR�BS�BS�BR�BQ�BO�BN�BO�BO�BR�BT�BVBVBVBVBW
BYBZB[#BYB[#B\)B]/BaHBbNBe`BgmBhsBjBq�Br�Bw�B}�B}�B~�B~�B� B�B�B�B�7B�PB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�'B�FB�XB�^B�wBĜBŢBȴB��B��B�
B�B�B�B�5B�TB�`B�NB�B�/B�`B�B�B��B��B	B	+B	1B	+B	%B	B	DB	VB	hB	bB	{B	#�B	0!B	2-B	1'B	.B	.B	.B	/B	2-B	33B	6FB	:^B	?}B	F�B	K�B	M�B	Q�B	T�B	W
B	YB	aHB	aHB	bNB	bNB	dZB	ffB	hsB	hsB	iyB	m�B	p�B	q�B	x�B	z�B	z�B	{�B	|�B	}�B	� B	�B	�+B	�7B	�=B	�DB	�PB	�PB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B
a22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BbNBbNBbNBbNBbNBbNBbNBbNBjB}�B�7B�hB��B�9B�LB�jB�jB�jB�qB��BĜBŢBŢBŢBÖBÖBÖBÖBĜBÖBŢBƨBŢBŢBB�}B�jB�}B��BÖBɺBÖB��B�jB�jB�qB��B��BƨB�wB�dB�XB�FB�-B�B��B��B�hB�=B�B~�Bv�Bo�BffBaHB]/BYBS�BE�B'�B �B �B�B�B�B�B�BoB	7B��B�NB��BƨB�^B��B�\B�Bz�Bn�BhsB^5BR�BM�BA�B2-B�B�B�B	7B
��B
�B
��B
�{B
�B
v�B
bNB
K�B
A�B
7LB
1'B
)�B
!�B
uB
VB
%B	��B	��B	�B	�B	�TB	�B	��B	��B	ƨB	�qB	�FB	�B	��B	��B	��B	��B	�{B	�PB	�B	}�B	v�B	q�B	hsB	cTB	^5B	VB	L�B	I�B	C�B	>wB	9XB	33B	$�B	�B	�B	{B	PB		7B	%B	B	B��B�B�B�B�`B�ZB�NB�5B�
B��B��B��BɺBŢBÖB��B�}B�qB�^B�9B�B�B��B��B��B��B��B�{B�oB�\B�PB�=B�=B�1B�+B�%B�B�B� B}�Bz�By�Bx�Bv�Bs�Bq�Bo�Bm�Bk�BiyBhsBffBiyBr�Bn�BgmBaHB_;B^5B\)BYBVBS�BR�BQ�BP�BN�BM�BM�BL�BL�BL�BL�BK�BK�BK�BK�BK�BK�BJ�BJ�BK�BI�BK�BP�BQ�BR�BR�BS�BS�BR�BQ�BO�BN�BO�BO�BR�BT�BVBVBVBVBW
BYBZB[#BYB[#B\)B]/BaHBbNBe`BgmBhsBjBq�Br�Bw�B}�B}�B~�B~�B� B�B�B�B�7B�PB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�'B�FB�XB�^B�wBĜBŢBȴB��B��B�
B�B�B�B�5B�TB�`B�NB�B�/B�`B�B�B��B��B	B	+B	1B	+B	%B	B	DB	VB	hB	bB	{B	#�B	0!B	2-B	1'B	.B	.B	.B	/B	2-B	33B	6FB	:^B	?}B	F�B	K�B	M�B	Q�B	T�B	W
B	YB	aHB	aHB	bNB	bNB	dZB	ffB	hsB	hsB	iyB	m�B	p�B	q�B	x�B	z�B	z�B	{�B	|�B	}�B	� B	�B	�+B	�7B	�=B	�DB	�PB	�PB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B
a22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191737                              AO  ARCAADJP                                                                    20181005191737    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191737  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191737  QCF$                G�O�G�O�G�O�8000            