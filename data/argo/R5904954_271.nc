CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:50Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  A�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  FX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  O�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  V�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  W�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  `(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    `X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    cX   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    fX   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  iX   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        j    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    jArgo profile    3.1 1.2 19500101000000  20181005191750  20181005191750  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�޲:1   @��%[�@5KI�^�d{�O�;d1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C�C  C  C  C  C  C  C  C  C�C �C"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCY�fC\  C^  C`  Cb�Cd  Cf  Ch  Cj�Cl�Cn�Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�fC��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C��C��C��C��C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C��3C��3C��3C��3C��3C�  C��C��C��C�  C�  C��C��C�  C��3C��3C�  C��3C�  C�  C�  C��3C��3C��C��C�  C�  C�  C�  C��C��C��C�  C��3C��3C��Dy��D�9HD�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@~�R@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HCǮC�HC	�HC��C�HC�HC�HC�HC�HC�HC�HC�HC��C��C!�HC#�HC%��C'��C)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCWǮCYǮC[�HC]�HC_�HCa��Cc�HCe�HCg�HCi��Ck��Cm��Co�HCq�HCs�HCu�HCwǮCy�HC{�HC}�HCǮC���C��C��C���C��C��C���C���C��C��C��C��qC��C��C��C��C��qC��C��C��qC��qC��qC��qC��qC��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��qC��qC��qC��qC��C��C��C��C��qC��qC��qC��qC��qC��C��C���C���C��C��C��C��C��C��C��C��C��qC��qC��qC��qC��C���C���C���C���C���C��C��qC��qC��qC��C��C��qC��qC��C���C���C��C���C��C��C��C���C���C��qC��qC��C��C��C��C��qC��qC��qC��C���C���C��qDy�
D�5qD�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��`A��;A��/A��mA��A��A��A���A���A���A���A���A���A���A���A���A�  A���A���A���A���A�  A�%A�1A�1A�A�A�  A�
=A�oA��A�A�A�C�A�33A��A�1A�A���A��A�VA��wA��PA�^5A�7LA�(�A��A��`A�A�t�A��A�hsA�JA���A�z�A�C�A�  A�(�A�1'A��\A���A��A�A�l�A�&�A��A�~�A��A���A�^5A��`A�O�A��A��9A�S�A�hsA�r�A�ffA�C�A�JA�A���A��A�K�A��
A�M�A���A��+A�VA��!A�~�A�9XA��;A��FA��A�9XA�hsA���A��A�jA�  A�7LA�dZA�;dA��;A���A��A��A��9A��A���A�  A�M�A��A}��AyXAxv�Aw�-Av5?Au�FAtI�Aq�ApjAnn�Al�+AjĜAi�TAi`BAfA�Ab��A_��A]p�A\��A[�7AXbAX �AW�^AW&�AT��AQ�
AP�uAO`BAL�AK%AIS�AI/AI
=AH-AG;dAEAES�AE"�AD�HAD��ADM�AD�AChsAA�7A>��A<�yA;�A;|�A:��A7�PA5S�A4��A3��A3�A2�DA1G�A/hsA-ƨA,jA*ffA)XA(�!A'7LA&�DA%ƨA$�!A$JA#|�A"�A"1A!p�A �yA�A��AE�A
=Az�AVAA�A=qA�TAK�A~�A�A�PA��A{A��A��A��A��A�TAI�A|�A{AO�A�`AƨA/A
�`A
��A
1A	`BA�wA=qA��A
=A1'A�7A�A�jAr�A��A%A �\@��@��^@��;@��\@�p�@��a@|��@i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��;A��/A��mA��A��A��A���A���A���A���A���A���A���A���A���A�  A���A���A���A���A�  A�%A�1A�1A�A�A�  A�
=A�oA��A�A�A�C�A�33A��A�1A�A���A��A�VA��wA��PA�^5A�7LA�(�A��A��`A�A�t�A��A�hsA�JA���A�z�A�C�A�  A�(�A�1'A��\A���A��A�A�l�A�&�A��A�~�A��A���A�^5A��`A�O�A��A��9A�S�A�hsA�r�A�ffA�C�A�JA�A���A��A�K�A��
A�M�A���A��+A�VA��!A�~�A�9XA��;A��FA��A�9XA�hsA���A��A�jA�  A�7LA�dZA�;dA��;A���A��A��A��9A��A���A�  A�M�A��A}��AyXAxv�Aw�-Av5?Au�FAtI�Aq�ApjAnn�Al�+AjĜAi�TAi`BAfA�Ab��A_��A]p�A\��A[�7AXbAX �AW�^AW&�AT��AQ�
AP�uAO`BAL�AK%AIS�AI/AI
=AH-AG;dAEAES�AE"�AD�HAD��ADM�AD�AChsAA�7A>��A<�yA;�A;|�A:��A7�PA5S�A4��A3��A3�A2�DA1G�A/hsA-ƨA,jA*ffA)XA(�!A'7LA&�DA%ƨA$�!A$JA#|�A"�A"1A!p�A �yA�A��AE�A
=Az�AVAA�A=qA�TAK�A~�A�A�PA��A{A��A��A��A��A�TAI�A|�A{AO�A�`AƨA/A
�`A
��A
1A	`BA�wA=qA��A
=A1'A�7A�A�jAr�A��A%A �\@��@��^@��;@��\@�p�@��a@|��@i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1B1B1B1B+B+B+B+B+B+B+B1B+B+B+B+B+B+B+B+B+B1B
=B	7B1B1B1B1BDB\B�B(�B1'B6FB:^B=qB?}BA�BL�Br�B}�B�B�JB�oB�{B��B��B�!B�XB��BɺB��B��B�B�B�B��BŢB��B�}B��B�XB��B�bB�B� Bw�BcTBYBXBQ�BI�BA�B49B �B�B�B{BhB\B	7BB  B��B�sB�)B��B��B�dB�B��B�bB~�Bq�Bl�BbNBN�B@�B33B�BB
��B
�B
�B
�fB
�5B
��B
�dB
��B
��B
�=B
�B
v�B
jB
W
B
O�B
I�B
G�B
I�B
A�B
0!B
!�B
PB	��B	�yB	�HB	�HB	ȴB	��B	�JB	�B	�B	{�B	hsB	t�B	v�B	p�B	\)B	H�B	?}B	8RB	"�B	�B	\B	oB	�B	�B	�B	�B	{B	{B	{B	uB	hB	bB	DB	B��B�B�B�yB�`B�B��B��BȴBŢB��B�dB�-B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�\B�PB�PB�JB�JB�JB�DB�DB�7B�1B�+B�%B�B�B�B�B~�B|�By�Bu�Br�Br�Bq�Bo�Bo�Bo�Bn�Bm�Bl�BjBiyBiyBjBhsBffBe`BdZBcTBdZBffBe`BdZBcTBdZBcTBcTBdZB
 �B
�B
/O22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B1B1B1B1B+B+B+B+B+B+B+B1B+B+B+B+B+B+B+B+B+B1B
=B	7B1B1B1B1BDB\B�B(�B1'B6FB:^B=qB?}BA�BL�Br�B}�B�B�JB�oB�{B��B��B�!B�XB��BɺB��B��B�B�B�B��BŢB��B�}B��B�XB��B�bB�B� Bw�BcTBYBXBQ�BI�BA�B49B �B�B�B{BhB\B	7BB  B��B�sB�)B��B��B�dB�B��B�bB~�Bq�Bl�BbNBN�B@�B33B�BB
��B
�B
�B
�fB
�5B
��B
�dB
��B
��B
�=B
�B
v�B
jB
W
B
O�B
I�B
G�B
I�B
A�B
0!B
!�B
PB	��B	�yB	�HB	�HB	ȴB	��B	�JB	�B	�B	{�B	hsB	t�B	v�B	p�B	\)B	H�B	?}B	8RB	"�B	�B	\B	oB	�B	�B	�B	�B	{B	{B	{B	uB	hB	bB	DB	B��B�B�B�yB�`B�B��B��BȴBŢB��B�dB�-B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�\B�PB�PB�JB�JB�JB�DB�DB�7B�1B�+B�%B�B�B�B�B~�B|�By�Bu�Br�Br�Bq�Bo�Bo�Bo�Bn�Bm�Bl�BjBiyBiyBjBhsBffBe`BdZBcTBdZBffBe`BdZBcTBdZBcTBcTBdZB
 �B
�B
/O22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191750                              AO  ARCAADJP                                                                    20181005191750    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191750  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191750  QCF$                G�O�G�O�G�O�8000            