CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  u   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:32Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  ?D   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  M�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  S�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  Z�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  bH   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  oh   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  v�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    v�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    y�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    |�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140832  20181024140832  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�Ԥ�J��1   @�ԥ""4v@5H�9Xb�c���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @9��@�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B ffB(  B0  B8ffB@  BI��BNffBX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C�C  C  C  C�fC  C   C"  C$  C&  C(  C)�fC+�fC.  C0  C1�fC4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D��D� D  D� D  D� DfD� D  D� D	fD	� D
  D
�fD  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D�fDfD� D��D� D  D�fD  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:  D:� D:��D;y�D<  D<� D<��D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC�fDD  DD� DE  Dy|�D�B=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @I��@�33@�33A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A���B  B	  B  B  B!ffB)  B1  B9ffBA  BJ��BOffBY  Ba  Bi  Bp��By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ Bг3BԀ B؀ B܀ B�� B� B�L�B�L�B�� B� B�� B�� C @ C&fC@ C@ C@ C
@ C@ C@ C@ C@ CY�C@ C@ C@ C&fC@ C @ C"@ C$@ C&@ C(@ C*&fC,&fC.@ C0@ C2&fC4@ C6&fC8&fC:@ C<@ C>@ C@@ CB@ CD@ CFY�CH@ CJ@ CL@ CN&fCP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^Y�C`Y�CbY�CdY�Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|Y�C~@ C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�3C�  C�,�C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�,�D  D � D D� D D� D D� D	�D� D D� D D� DfD� D D� D	fD	� D
 D
�fD D� D	�D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D��D D� D D� D D�fDfD� D	�D� D D�fD D� DfD� D D� D  D � D! D!� D" D"� D# D#� D/� D0 D0��D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8	�D8��D9 D9� D: D:� D;	�D;��D< D<� D=	�D=� D>fD>� D? D?� D@ D@� DA DA� DB DB��DC DC�fDD DD� DE Dy��D�J=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aׇ+Aח�Aי�Aי�Aכ�Aכ�Aם�Aם�Aכ�Aכ�Aכ�Aם�Aס�Aס�Aי�Aי�Aי�Aם�Aס�Aף�Aץ�Aץ�A׋DA�jA�=qA��A��A�E�AɁAǛ�A�=qA��hA�1'A��A���A�C�A��#A��\A�=qA�M�A��!A�Q�A��A�v�A�Q�A��A�C�A�9XA��
A�XA�VA���A���A���A��A���A�%A�t�A�ĜA��A�\)A�hsA��A�9XA�G�A���A��-A��^A�hsA�hsA���A���A���A�^5A�
=A��jA��uA��FA��A�A��!A�M�A�v�A��uA�r�A��A��A��A��A}��A|�`A{�^AxȴAw��Aw�Aw�7Av�yAuS�Ar��Ap  Ao�Am\)AkG�Ajr�Ah�AfffAc�hAa�Aa�A`��A]�TAY�7AVbAU��AU��AS��AO33ALĜAJ��AH5?AGdZAF�9AD��AA�A@�`A@r�A?�A?A=G�A:JA8JA7�A7p�A6��A6$�A6{A5�7A4�!A4M�A3��A2�DA1�-A1&�A0��A0I�A/&�A-�#A,�+A*�A*-A)��A)�A(-A&�/A&JA%l�A"�!A!�A ��A�A�AXAQ�A`BA�A$�A/AZAA��At�A�RA1'A�/A{AȴAVAbA�TA�AG�A7LA�A  AXA�A��AVA�^AG�A&�A��A=qAt�A	/AO�A�/A �A�AC�A�DA�
A�A�jA��A��A�-Ahs@��T@�~�@�Z@�K�@���@��@�1@�&�@�j@�33@�~�@�^@�?}@�V@�A�@�|�@�
=@�ȴ@柾@��T@�@�@��@�-@�7L@���@�A�@�dZ@���@�~�@ݩ�@ܬ@�o@��@ى7@�p�@�X@�?}@��@�V@���@�j@ץ�@�v�@�5?@���@�x�@��@ԓu@�bN@�S�@ҟ�@�5?@�$�@�@љ�@��@ЋD@ϕ�@·+@�X@̼j@� �@˕�@�l�@�;d@�@�~�@ɡ�@���@�r�@�1'@�(�@ǍP@Ƈ+@�@���@Ų-@���@�Q�@�Q�@���@ēu@�Q�@�(�@�b@�l�@�/@���@�+@�ȴ@��@�33@���@�Ĝ@�;d@��-@��h@�/@�A�@��@��@���@�hs@��-@��-@��^@�@�@���@��h@��-@��#@��@��T@��#@��#@��T@��@���@��R@��@�`B@��@�Z@�r�@�Ĝ@�`B@�&�@��j@���@��u@�Q�@�S�@�o@��@��@���@�V@�-@��@��h@�p�@���@���@��@�Z@�  @�ƨ@��F@�C�@�
=@��@��@��R@���@�=q@��-@�`B@��@��9@�r�@�A�@��@xj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aׇ+Aח�Aי�Aי�Aכ�Aכ�Aם�Aם�Aכ�Aכ�Aכ�Aם�Aס�Aס�Aי�Aי�Aי�Aם�Aס�Aף�Aץ�Aץ�A׋DA�jA�=qA��A��A�E�AɁAǛ�A�=qA��hA�1'A��A���A�C�A��#A��\A�=qA�M�A��!A�Q�A��A�v�A�Q�A��A�C�A�9XA��
A�XA�VA���A���A���A��A���A�%A�t�A�ĜA��A�\)A�hsA��A�9XA�G�A���A��-A��^A�hsA�hsA���A���A���A�^5A�
=A��jA��uA��FA��A�A��!A�M�A�v�A��uA�r�A��A��A��A��A}��A|�`A{�^AxȴAw��Aw�Aw�7Av�yAuS�Ar��Ap  Ao�Am\)AkG�Ajr�Ah�AfffAc�hAa�Aa�A`��A]�TAY�7AVbAU��AU��AS��AO33ALĜAJ��AH5?AGdZAF�9AD��AA�A@�`A@r�A?�A?A=G�A:JA8JA7�A7p�A6��A6$�A6{A5�7A4�!A4M�A3��A2�DA1�-A1&�A0��A0I�A/&�A-�#A,�+A*�A*-A)��A)�A(-A&�/A&JA%l�A"�!A!�A ��A�A�AXAQ�A`BA�A$�A/AZAA��At�A�RA1'A�/A{AȴAVAbA�TA�AG�A7LA�A  AXA�A��AVA�^AG�A&�A��A=qAt�A	/AO�A�/A �A�AC�A�DA�
A�A�jA��A��A�-Ahs@��T@�~�@�Z@�K�@���@��@�1@�&�@�j@�33@�~�@�^@�?}@�V@�A�@�|�@�
=@�ȴ@柾@��T@�@�@��@�-@�7L@���@�A�@�dZ@���@�~�@ݩ�@ܬ@�o@��@ى7@�p�@�X@�?}@��@�V@���@�j@ץ�@�v�@�5?@���@�x�@��@ԓu@�bN@�S�@ҟ�@�5?@�$�@�@љ�@��@ЋD@ϕ�@·+@�X@̼j@� �@˕�@�l�@�;d@�@�~�@ɡ�@���@�r�@�1'@�(�@ǍP@Ƈ+@�@���@Ų-@���@�Q�@�Q�@���@ēu@�Q�@�(�@�b@�l�@�/@���@�+@�ȴ@��@�33@���@�Ĝ@�;d@��-@��h@�/@�A�@��@��@���@�hs@��-@��-@��^@�@�@���@��h@��-@��#@��@��T@��#@��#@��T@��@���@��R@��@�`B@��@�Z@�r�@�Ĝ@�`B@�&�@��j@���@��u@�Q�@�S�@�o@��@��@���@�V@�-@��@��h@�p�@���@���@��@�Z@�  @�ƨ@��F@�C�@�
=@��@��@��R@���@�=q@��-@�`B@��@��9@�r�@�A�@��@xj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�
B�
B��B�BBB+B#�B/B2-B49B6FB7LB=qB?}BE�BI�BJ�BI�BP�BP�BN�BK�BL�BM�BK�BI�BD�B=qB33B%�B�B{B+B��B�TB��B��B��B�oB�=B|�BgmBN�BA�B49B)�B�B\B
��B
�B
�
B
�qB
�B
��B
��B
�VB
{�B
jB
]/B
F�B
9XB
2-B
,B
$�B
�B
DB
%B	��B	�B	�B	�yB	�sB	�ZB	�)B	��B	�qB	�XB	�-B	��B	��B	��B	�B	q�B	aHB	[#B	VB	G�B	2-B	 �B	�B	�B	uB	
=B	uB	DB��B��B�B�B�B�sB�mB�ZB�BB�B��BɺBɺBȴBǮBƨBŢBĜBÖBB��B�wB�qB�dB�^B�RB�FB�-B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�3B�9B�^B�^B�wB�wB�wB�wB�wB�qB�jB�dB�^B�^B�XB�LB�?B�3B�'B�!B�B��B��B��B�\B�PB�=B�7B�1B�%B�B�B�B�+B�1B�=B�PB�bB�oB�uB�uB�oB�hB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�?B�FB�FB�FB�FB�FB�FB�LB�LB�RB�dB�dB�dB�jB�jB�jB�jB�wB��BBBBÖBĜBȴB��B��B��B��B��B��B�B�B�
B�B�)B�5B�BB�BB�BB�BB�NB�TB�TB�TB�ZB�fB�B�B��B��B��B�B�B�ZB��B��B��B��BɺB��B��B��B��B�B�
B�B�HB�yB�B�B��B��B��B��B	1B	
=B	PB	uB	�B	�B	�B	�B	�B	 �A�bNB	\)B	bNB	hsB	m�B	r�B	v�B	y�B	{�B	�B	�B	�B	�%B	�%B	�%B	�1B	�=B	�JB	�VB	�VB	�\B	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B
^B
)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�
B�
B��B�BBB+B#�B/B2-B49B6FB7LB=qB?}BE�BI�BJ�BI�BP�BP�BN�BK�BL�BM�BK�BI�BD�B=qB33B%�B�B{B+B��B�TB��B��B��B�oB�=B|�BgmBN�BA�B49B)�B�B\B
��B
�B
�
B
�qB
�B
��B
��B
�VB
{�B
jB
]/B
F�B
9XB
2-B
,B
$�B
�B
DB
%B	��B	�B	�B	�yB	�sB	�ZB	�)B	��B	�qB	�XB	�-B	��B	��B	��B	�B	q�B	aHB	[#B	VB	G�B	2-B	 �B	�B	�B	uB	
=B	uB	DB��B��B�B�B�B�sB�mB�ZB�BB�B��BɺBɺBȴBǮBƨBŢBĜBÖBB��B�wB�qB�dB�^B�RB�FB�-B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�3B�9B�^B�^B�wB�wB�wB�wB�wB�qB�jB�dB�^B�^B�XB�LB�?B�3B�'B�!B�B��B��B��B�\B�PB�=B�7B�1B�%B�B�B�B�+B�1B�=B�PB�bB�oB�uB�uB�oB�hB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�?B�FB�FB�FB�FB�FB�FB�LB�LB�RB�dB�dB�dB�jB�jB�jB�jB�wB��BBBBÖBĜBȴB��B��B��B��B��B��B�B�B�
B�B�)B�5B�BB�BB�BB�BB�NB�TB�TB�TB�ZB�fB�B�B��B��B��B�B�B�ZB��B��B��B��BɺB��B��B��B��B�B�
B�B�HB�yB�B�B��B��B��B��B	1B	
=B	PB	uB	�B	�B	�B	�B	�B	 �A�bNB	\)B	bNB	hsB	m�B	r�B	v�B	y�B	{�B	�B	�B	�B	�%B	�%B	�%B	�1B	�=B	�JB	�VB	�VB	�\B	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B
^B
)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140832                              AO  ARCAADJP                                                                    20181024140832    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140832  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140832  QCF$                G�O�G�O�G�O�0               