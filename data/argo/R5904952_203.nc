CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:51Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       =P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       A0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       DH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  G`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       H(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  K@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       O    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  R8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       S    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  V   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       V�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  Y�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Z(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ](   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    `(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  c(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    cT   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    cX   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    c\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    c`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  cd   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    c�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    c�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    c�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         c�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         c�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        c�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    c�Argo profile    3.1 1.2 19500101000000  20181005190551  20181005190551  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)βU21   @��*`� V@1�j~��#�c���O�;1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B/��B8  B@  BG��BP  BX  B_��Bh  Bp  Bx  B�33B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4�C6  C8�C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCk�fCn  Cp  Cr  Cs�fCu�fCx  Cz�C|�C~�C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��Dy�RD�MD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A  A$  AE��Ad  A�  A�  A�  A�  A�  A�  A�  A�  B  B	  B  B��B!  B)  B0��B9  BA  BH��BQ  BY  B`��Bi  Bq  By  B��3B��3B�� B�� B��3B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ B�L�B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C&fC&fC
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C Y�C"@ C$@ C&@ C(&fC*@ C,@ C.@ C0@ C2@ C4Y�C6@ C8Y�C:@ C<@ C>@ C@@ CB@ CD@ CF&fCH@ CJ@ CL@ CN@ CP@ CRY�CT@ CV@ CX@ CZY�C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj&fCl&fCn@ Cp@ Cr@ Ct&fCv&fCx@ CzY�C|Y�C~Y�C�  C�  C�  C�3C�  C�,�C�  C�  C�  C�  C�  C�,�C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�3C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�,�Dy�RD�UD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�dZA�ffA�dZA�dZA�dZA�n�A�p�A�r�A�|�A�|�AǁAǁAǁAǁAǃAǉ7AǃAǁAǉ7Aǲ-A�ĜA���A��
A��/A��HA��mA��mA��mA��`A��#A��#A��
A��A��A���A��A��mA��yA��HA��#A��#A��A��
A��
A��
A��
A���AǍPAƉ7A� �A���A�$�A��#AőhA�$�A�XA�+A���AÑhADA��A��;A��A�A��A��A���A���A��A��;A�&�A��A�^5A�+A���A���A�p�A�S�A�1A�x�A��A��yA�VA��9A��A�1'A��TA��A��A�33A�ȴA�XA��7A��wA��A��hA��/A�A���A���A�|�A��wA��+A���A�v�A�`BA�;dA��7A�5?A��/A�O�A���A�Q�A�ĜA�XA�XAy|�Ay/Ay;dAxJAu��Au�Au
=As33Aq33ApA�An(�AlA�Ai�-Af��Aep�AcVA_XA[��AV�\AR�jAP��AL�/AL1'AKC�AI�TAH�/AGC�AE�;AC�wAAG�A?XA<��A:VA933A8bNA6��A5G�A4�9A1%A.��A-dZA,�A,(�A+p�A(�yA'?}A%/A#\)A!�mA �9A A�A�
A7LA�\A��A�HAM�A(�AC�AA�A��A��AVA��A^5A�AoA�jAv�A�A+A�DA��AVA��A(�AG�@�}�@�PH@oZ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bNA�dZA�ffA�dZA�dZA�dZA�n�A�p�A�r�A�|�A�|�AǁAǁAǁAǁAǃAǉ7AǃAǁAǉ7Aǲ-A�ĜA���A��
A��/A��HA��mA��mA��mA��`A��#A��#A��
A��A��A���A��A��mA��yA��HA��#A��#A��A��
A��
A��
A��
A���AǍPAƉ7A� �A���A�$�A��#AőhA�$�A�XA�+A���AÑhADA��A��;A��A�A��A��A���A���A��A��;A�&�A��A�^5A�+A���A���A�p�A�S�A�1A�x�A��A��yA�VA��9A��A�1'A��TA��A��A�33A�ȴA�XA��7A��wA��A��hA��/A�A���A���A�|�A��wA��+A���A�v�A�`BA�;dA��7A�5?A��/A�O�A���A�Q�A�ĜA�XA�XAy|�Ay/Ay;dAxJAu��Au�Au
=As33Aq33ApA�An(�AlA�Ai�-Af��Aep�AcVA_XA[��AV�\AR�jAP��AL�/AL1'AKC�AI�TAH�/AGC�AE�;AC�wAAG�A?XA<��A:VA933A8bNA6��A5G�A4�9A1%A.��A-dZA,�A,(�A+p�A(�yA'?}A%/A#\)A!�mA �9A A�A�
A7LA�\A��A�HAM�A(�AC�AA�A��A��AVA��A^5A�AoA�jAv�A�A+A�DA��AVA��A(�AG�@�}�@�PH@oZ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�oB
�oB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�oB
�uB
�{B
�{B
�uB
��B
B
�B
�/B
�TB
�fB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B%B6FBO�BZBcTBjBq�B�B��B��B��B�!BB��B�fB�B�B��BuB(�B)�B0!B<jB?}BA�BB�B>wB<jB=qB>wB>wB?}BD�BS�BM�BE�B5?B,B&�B�B�B�BbBDBB��B�sB�5B��B��B�%Bl�BaHBXBJ�B>wB6FB5?B6FB49B)�B�BB
�#B
�VB
M�B
�B	�B	��B	�3B	ĜB	��B	ɺB	ƨB	ǮB	ÖB	�RB	�B	��B	��B	�1B	|�B	o�B	ffB	XB	?}B	/B	�B	+B��B�B�yB�B�fB�)B�B��BƨB�qB�-B�B��B��B��B��B�{B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B�B�!B�'B�3B�?B�FB�?B�?B�FB�FB�?B�FB�FB�?B�9B�LB�RB�RB�LB�FB�LB�LB�FB�FB�9B�3B�LB
�B
=B
(�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�oB
�oB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�oB
�uB
�{B
�{B
�uB
��B
B
�B
�/B
�TB
�fB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B%B6FBO�BZBcTBjBq�B�B��B��B��B�!BB��B�fB�B�B��BuB(�B)�B0!B<jB?}BA�BB�B>wB<jB=qB>wB>wB?}BD�BS�BM�BE�B5?B,B&�B�B�B�BbBDBB��B�sB�5B��B��B�%Bl�BaHBXBJ�B>wB6FB5?B6FB49B)�B�BB
�#B
�VB
M�B
�B	�B	��B	�3B	ĜB	��B	ɺB	ƨB	ǮB	ÖB	�RB	�B	��B	��B	�1B	|�B	o�B	ffB	XB	?}B	/B	�B	+B��B�B�yB�B�fB�)B�B��BƨB�qB�-B�B��B��B��B��B�{B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B�B�!B�'B�3B�?B�FB�?B�?B�FB�FB�?B�FB�FB�?B�9B�LB�RB�RB�LB�FB�LB�LB�FB�FB�9B�3B�LB
�B
=B
(�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190551                              AO  ARCAADJP                                                                    20181005190551    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190551  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190551  QCF$                G�O�G�O�G�O�8000            