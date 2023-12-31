CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:25Z creation      
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181024141425  20181024141425  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               {A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @����5}1   @���ww�0@3�A�7K��c�Z�11   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      {A   A   A   @333@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D��Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� Dy|�D�R�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @6ff@���@���A ��A ��A@��AbfgA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B�L�B��B��gB��B��B��B��gB��gB��B��B��B��B��B�L�B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C-�3C0�C2�C4�C6�C8�C:�C<�C>&gC@&gCB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D��D�3D3D�3D3D�3D3D�3D3D�3D��D|�D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3Dy� D�T)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A۴9A۲-A۴9A۸RA۾wA۾wA۾wA���A�A�A�A�ĜA�A�A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A۩�A�7LAٍPA�C�A�%Aײ-A���A�E�A���A���A�;dA�jA���A͇+Ḁ�A�hsA���Aʲ-Aɉ7A�+A�?}A�7LA�%A�?}A��A®A7A�VA��A��A��jA�ĜA�$�A���A�1A��A�bNA��hA�{A�  A�A��A��A���A�n�A���A��A�
=A��mA�ffA�  A�VA��FA��-A��A��A�G�A��FA���A���A���A�A��A��FA��+A�%A�A�A�&�A�G�A��uA���A���A�ȴA��A�~�A���A�A���A���A�(�AA}�mAy%AvQ�ArM�Ao�hAm�mAi��Af�`AfffAd��AcVA_;dA\I�A[�wAZQ�AY�AX�RAW`BAS�AR�uAO�AMl�AK��AJ(�AIVAH��AGt�AEƨACVAA�-A@I�A>��A=�A<=qA:M�A9�A7�;A5"�A3�
A2�A1��A0��A/K�A.ffA-�A,1'A+�A*�A)�A(^5A%/A$Q�A#p�A#A"��A"bNA!O�A`BA�9AAZA�hA��AI�A�A�DAȴA�wA�9AZA(�A��A��AA��A�9A�TA`BA�AJA��AK�A
JA	dZA��A�HA�FA��AbAA��A��A ��@��@�;d@���@��T@���@���@��@��@��@��j@��w@��@�Q�@�7@��@��@�v�@��T@�z�@�ƨ@�K�@��@�^5@�^@��@�Ĝ@�@�ƨ@�+@��@���@�-@�ƨ@އ+@ݡ�@���@���@�\)@��H@ڧ�@�ff@���@ؓu@� �@��;@�|�@�M�@պ^@�X@ԣ�@�z�@�(�@��
@�33@��@��T@Ь@�Q�@��@϶F@ϝ�@�;d@��@��@́@�hs@�O�@���@�Q�@�dZ@��y@�E�@��#@���@ɩ�@ə�@�hs@���@ȋD@�1@ǅ@��y@�n�@�=q@�$�@��@�{@�@��@��@�Z@�dZ@�M�@���@�G�@�7L@��`@�j@�ƨ@�33@��!@�V@�{@�M�@�M�@�5?@��^@�O�@��9@��u@��D@��@�A�@�1@�ƨ@�|�@�;d@�@�M�@��@�7L@�%@�Q�@�9X@�(�@�  @�C�@�V@�@�x�@�z�@��
@�\)@��H@�^5@���@��@�Q�@�|�@���@���@�"�@���@��!@���@�~�@�ff@�-@���@��h@�p�@�?}@���@���@��@�r�@�r�@�bN@�b@���@��@��@���@��w@��@�|�@�\)@��@�^5@�=q@�5?@�$�@�$�@���@�hs@�&�@��@���@�j@�  @���@��y@���@��U@�YK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A۴9A۲-A۴9A۸RA۾wA۾wA۾wA���A�A�A�A�ĜA�A�A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A۩�A�7LAٍPA�C�A�%Aײ-A���A�E�A���A���A�;dA�jA���A͇+Ḁ�A�hsA���Aʲ-Aɉ7A�+A�?}A�7LA�%A�?}A��A®A7A�VA��A��A��jA�ĜA�$�A���A�1A��A�bNA��hA�{A�  A�A��A��A���A�n�A���A��A�
=A��mA�ffA�  A�VA��FA��-A��A��A�G�A��FA���A���A���A�A��A��FA��+A�%A�A�A�&�A�G�A��uA���A���A�ȴA��A�~�A���A�A���A���A�(�AA}�mAy%AvQ�ArM�Ao�hAm�mAi��Af�`AfffAd��AcVA_;dA\I�A[�wAZQ�AY�AX�RAW`BAS�AR�uAO�AMl�AK��AJ(�AIVAH��AGt�AEƨACVAA�-A@I�A>��A=�A<=qA:M�A9�A7�;A5"�A3�
A2�A1��A0��A/K�A.ffA-�A,1'A+�A*�A)�A(^5A%/A$Q�A#p�A#A"��A"bNA!O�A`BA�9AAZA�hA��AI�A�A�DAȴA�wA�9AZA(�A��A��AA��A�9A�TA`BA�AJA��AK�A
JA	dZA��A�HA�FA��AbAA��A��A ��@��@�;d@���@��T@���@���@��@��@��@��j@��w@��@�Q�@�7@��@��@�v�@��T@�z�@�ƨ@�K�@��@�^5@�^@��@�Ĝ@�@�ƨ@�+@��@���@�-@�ƨ@އ+@ݡ�@���@���@�\)@��H@ڧ�@�ff@���@ؓu@� �@��;@�|�@�M�@պ^@�X@ԣ�@�z�@�(�@��
@�33@��@��T@Ь@�Q�@��@϶F@ϝ�@�;d@��@��@́@�hs@�O�@���@�Q�@�dZ@��y@�E�@��#@���@ɩ�@ə�@�hs@���@ȋD@�1@ǅ@��y@�n�@�=q@�$�@��@�{@�@��@��@�Z@�dZ@�M�@���@�G�@�7L@��`@�j@�ƨ@�33@��!@�V@�{@�M�@�M�@�5?@��^@�O�@��9@��u@��D@��@�A�@�1@�ƨ@�|�@�;d@�@�M�@��@�7L@�%@�Q�@�9X@�(�@�  @�C�@�V@�@�x�@�z�@��
@�\)@��H@�^5@���@��@�Q�@�|�@���@���@�"�@���@��!@���@�~�@�ff@�-@���@��h@�p�@�?}@���@���@��@�r�@�r�@�bN@�b@���@��@��@���@��w@��@�|�@�\)@��@�^5@�=q@�5?@�$�@�$�@���@�hs@�&�@��@���@�j@�  @���@��y@���@��U@�YK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��BBBBB+B
=BPB{B�B�B#�B9XBG�BVB^5Bv�B�%B�oB��B��BĜB�5B�BB�B�B�
B��B�HB�yB�B��BPBVB�B33B2-B;dBW
Be`Bl�Bs�Br�Bp�Bp�Bz�B��B�B�RBBǮBȴB��B�wB�dB�9B�!B��B�{B}�Br�Bk�BbNB]/BL�B�B�BɺB�VBI�B�B
��B
��B
ɺB
B
�?B
�9B
�!B
��B
�bB
u�B
[#B
O�B
A�B
2-B
#�B
PB	�B	�B	�wB	�3B	��B	�\B	�=B	�B	w�B	jB	T�B	Q�B	M�B	C�B	@�B	;dB	/B	)�B	 �B	uB	JB	B	  B��B��B�B�yB�NB�/B�B�B��B��BɺB��B�9B�B��B�B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�VB�VB�1B�B�B|�B{�By�Bx�Bx�Bw�Bx�B� B�B�%B��B�B�!B��B��B��B��B��B��B��B��B�JB�1B�1B�%B�1B�1B�%B�DB�oB��B��B�'B�B�B�B�B�B�B�?B�?B�XB�^B�wBÖB��B��B��B��B��B��B�B�5B�TB�mB�mB�yB�B�B�B�yB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	%B	
=B	JB	VB	\B	bB	\B	uB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	&�B	'�B	(�B	+B	.B	1'B	33B	6FB	8RB	:^B	:^B	:^B	:^B	<jB	=qB	=qB	@�B	C�B	F�B	F�B	G�B	G�B	H�B	J�B	L�B	M�B	P�B	VB	[#B	bNB	cTB	cTB	ffB	hsB	l�B	l�B	l�B	l�B	m�B	l�B	m�B	m�B	m�B	n�B	p�B	q�B	v�B	y�B	{�B	{�B	|�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�LB	�XB	�XB	�^B	�jB	�jB	�jB	�qB	�}B	��B	��B	��B	��B	��B	��B	ÖB	ĜB	ǮB	ǮB
1B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
��BBBBB+B
=BPB{B�B�B#�B9XBG�BVB^5Bv�B�%B�oB��B��BĜB�5B�BB�B�B�
B��B�HB�yB�B��BPBVB�B33B2-B;dBW
Be`Bl�Bs�Br�Bp�Bp�Bz�B��B�B�RBBǮBȴB��B�wB�dB�9B�!B��B�{B}�Br�Bk�BbNB]/BL�B�B�BɺB�VBI�B�B
��B
��B
ɺB
B
�?B
�9B
�!B
��B
�bB
u�B
[#B
O�B
A�B
2-B
#�B
PB	�B	�B	�wB	�3B	��B	�\B	�=B	�B	w�B	jB	T�B	Q�B	M�B	C�B	@�B	;dB	/B	)�B	 �B	uB	JB	B	  B��B��B�B�yB�NB�/B�B�B��B��BɺB��B�9B�B��B�B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�VB�VB�1B�B�B|�B{�By�Bx�Bx�Bw�Bx�B� B�B�%B��B�B�!B��B��B��B��B��B��B��B��B�JB�1B�1B�%B�1B�1B�%B�DB�oB��B��B�'B�B�B�B�B�B�B�?B�?B�XB�^B�wBÖB��B��B��B��B��B��B�B�5B�TB�mB�mB�yB�B�B�B�yB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	%B	
=B	JB	VB	\B	bB	\B	uB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	&�B	'�B	(�B	+B	.B	1'B	33B	6FB	8RB	:^B	:^B	:^B	:^B	<jB	=qB	=qB	@�B	C�B	F�B	F�B	G�B	G�B	H�B	J�B	L�B	M�B	P�B	VB	[#B	bNB	cTB	cTB	ffB	hsB	l�B	l�B	l�B	l�B	m�B	l�B	m�B	m�B	m�B	n�B	p�B	q�B	v�B	y�B	{�B	{�B	|�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�LB	�XB	�XB	�^B	�jB	�jB	�jB	�qB	�}B	��B	��B	��B	��B	��B	��B	ÖB	ĜB	ǮB	ǮB
1B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141425                              AO  ARCAADJP                                                                    20181024141425    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141425  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141425  QCF$                G�O�G�O�G�O�0               