CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:56Z creation      
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181005191656  20181005191656  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL                A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׷d�+Ni1   @׷e33E�@5fffff�c�t�j~�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                       A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffBffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\�C^�C`  Ca�fCc�fCe�fCh  Cj  Cl�Cn  Cp  Cq�fCs��Cu�fCx  Cy�fC|  C~  C�fC��3C�  C��C�  C�  C�  C�  C�  C��3C��C��C��C��C��3C��3C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��3C��3C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��3C�  C��3C�  C�  C�  C�  C�  C��3C��C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C��C��3C��3C�  C�  C�  C��C��C��C�  C��3C��C��C�  C�  C��C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C��C�  C�  C��3C�  C��C�  C�  C�  C��C��C��3C�  C��3C��3C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C��C��D fD � D ��Dy�D  D�fD��Ds3D  D�fD�D�fDfD�fDfD�fD  D� D	  D	�fD
  D
y�D  D�fD��Dy�D  D�fDfDy�D��Dy�D  D�fD  D� D��Dy�D  D� D  D� D  D� D  Dy�D  D� D  D� D��Dy�D��D� D  Dy�D  D�fD  D� DfD�fDfD� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(y�D(��D)�fD*  D*y�D+fD+�fD,fD,�fD-  D-�fD.�D.�fD/fD/�fD0fD0�fD1  D1�fD2fD2�fD2��D3� D4  D4y�D5fD5� D5��D6y�D7fD7��D8fD8y�D9  D9�fD:fD:y�D;  D;��D<fD<� D=fD=� D>fD>�fD?  D?y�D?��D@� DA  DA� DB  DBy�DB��DCy�DD  DD� DE  Dy��D�G
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�B�B�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBoQ�Bw�RB�RB��)B�\B��)B��)B��)B��)B�\B�\B��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�zCI�CK�CN�CP�CQ�CS�CU�CW�CY�C\�C^�C_�Ca�zCc�zCe�zCg�Ci�Cl�Cm�Co�Cq�zCs��Cu�zCw�Cy�zC{�C}�C�zC��=C��
C��C��
C��
C��
C��
C��
C��=C��C��C��C��C��=C��=C��
C��
C��
C��
C��=C��=C��
C��C��C��
C��=C��=C��
C��=C��=C��
C��
C��
C��
C��
C��C��
C��=C��=C��
C��=C��
C��=C��
C��
C��
C��
C��
C��=C��C��
C��
C��C��
C��=C��
C��
C��C��C��
C��
C��
C��
C��
C��C��=C��=C��
C��
C��
C��C��C��C��
C��=C��C��C��
C��
C��C��
C��
C��
C��
C��=C��
C��=C��
C��
C��
C��C��C��C��
C��=C��=C��
C��
C��C��
C��
C��=C��
C��C��
C��
C��
C��C��C��=C��
C��=C��=C��
C��
C��=C��=C��
C��C��
C��=C��
C��
C��
C��
C��C��D �D {�D �DuD��D��D�Dn�D��D��DRD��D�D��D�D��D��D{�D��D	��D	��D
uD
��D��D�DuD��D��D�DuD�DuD��D��D��D{�D�DuD��D{�D��D{�D��D{�D��DuD��D{�D��D{�D�DuD�D{�D��DuD��D��D��D{�D�D��D�D{�D��D uD ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'uD'��D(uD(�D)��D)��D*uD+�D+��D,�D,��D,��D-��D.RD.��D/�D/��D0�D0��D0��D1��D2�D2��D2�D3{�D3��D4uD5�D5{�D5�D6uD7�D7�RD8�D8uD8��D9��D:�D:uD:��D;�RD<�D<{�D=�D={�D>�D>��D>��D?uD?�D@{�D@��DA{�DA��DBuDB�DCuDC��DD{�DD��Dy�RD�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�33A�7LA�5?A�5?A�9XA�;dA�=qA�7LA�=qA�=qA�=qA�9XA�$�AҁA�A�+A��;A��
A�hsA�"�A�=qA̙�A�33A���A�&�A�I�Aƛ�A�jA��A�1A�hsA�"�A��/A��A���A���A�t�A��jA�ffA���A���A�?}A���A��A��-A�/A�
=A��mA���A���A�ffA� �A���A�S�A��!A��A�bA���A���A��
A��
A���A��A��TA���A��A�ȴA�1'A��-A��TA�$�A�=qA�l�A���A�VA���A�dZA���A���A��A��HA�bA�ffA���A�O�A��jA��A���A�bNA��mA��A���A�O�A�XA��;A��A��+A��TA���A���A�A�$�A���A�VA�?}A}��A{��Ax�HAu`BAr�/Ap�uAo?}Al�yAl-Ak�hAi�Agl�Af�HAfz�Ac��AbA�Aa�A`�/A_��A^�RA^z�A[�;AY��AV=qAUG�AT�+AS��ASS�AR��AQ�mAPȴAO�mAO"�AN=qAK�AG+AFM�AD��ACG�AB��A@��A<�A<�A;hsA9��A65?A5&�A41A2�A2A0A,��A++A*(�A)"�A(��A&��A$�A"ȴA!�wA!��A!l�A!7LA ��A E�A1'A|�AQ�A��A��AffA(�A�mA1'A�PA|�AS�AbNAbNAC�AbNA��A��AG�AA��A"�A^5A�A��A|�A
��A
{A��AƨA�`A1'A��A�Az�A�A �@�M�@��`@��+@�bN@��H@�V@�w@�R@�@�;d@�~�@�@�%@�\)@�G�@�ƨ@�=q@�V@�z�@��@�|�@��y@�&�@�j@�9X@�b@߾w@�C�@ݲ-@ۍP@�
=@�^5@��`@ם�@��@�@��@և+@�E�@��@�/@���@���@�?}@ЋD@ϥ�@��@Ώ\@�=q@���@ͩ�@͙�@�O�@ˍP@�~�@�V@�E�@�$�@Ǿw@�K�@�~�@�{@��#@�x�@�?}@�z�@î@�+@��@���@�%@�(�@���@�"�@���@��y@��y@��H@�n�@��^@�O�@�z�@���@��P@�33@�{@���@�O�@�%@���@���@���@��`@�V@�?}@�p�@���@��@��@�j@�V@��/@�p�@�G�@�j@�S�@���@���@��7@��#@���@��P@�  @��9@��D@��@��m@��@��;@�"�@��@�p�@� �@�J@���@�Ĝ@���@���@���@�Q�@���@�"�@��@�O�@�7L@���@�Q�@� �@�K�@�^5@��@��-@�X@�X@�?}@�V@��`@���@��D@���@��@�bN@�b@��w@���@�\)@�
=@��R@��\@�~�@�^5@��h@�1'@�1@���@���@��y@��+@�^5@��@��T@���@���@�p�@��@��@���@��j@�Ĝ@�Z@��@�ȴ@���@�E�@���@�!�@~��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�33A�7LA�5?A�5?A�9XA�;dA�=qA�7LA�=qA�=qA�=qA�9XA�$�AҁA�A�+A��;A��
A�hsA�"�A�=qA̙�A�33A���A�&�A�I�Aƛ�A�jA��A�1A�hsA�"�A��/A��A���A���A�t�A��jA�ffA���A���A�?}A���A��A��-A�/A�
=A��mA���A���A�ffA� �A���A�S�A��!A��A�bA���A���A��
A��
A���A��A��TA���A��A�ȴA�1'A��-A��TA�$�A�=qA�l�A���A�VA���A�dZA���A���A��A��HA�bA�ffA���A�O�A��jA��A���A�bNA��mA��A���A�O�A�XA��;A��A��+A��TA���A���A�A�$�A���A�VA�?}A}��A{��Ax�HAu`BAr�/Ap�uAo?}Al�yAl-Ak�hAi�Agl�Af�HAfz�Ac��AbA�Aa�A`�/A_��A^�RA^z�A[�;AY��AV=qAUG�AT�+AS��ASS�AR��AQ�mAPȴAO�mAO"�AN=qAK�AG+AFM�AD��ACG�AB��A@��A<�A<�A;hsA9��A65?A5&�A41A2�A2A0A,��A++A*(�A)"�A(��A&��A$�A"ȴA!�wA!��A!l�A!7LA ��A E�A1'A|�AQ�A��A��AffA(�A�mA1'A�PA|�AS�AbNAbNAC�AbNA��A��AG�AA��A"�A^5A�A��A|�A
��A
{A��AƨA�`A1'A��A�Az�A�A �@�M�@��`@��+@�bN@��H@�V@�w@�R@�@�;d@�~�@�@�%@�\)@�G�@�ƨ@�=q@�V@�z�@��@�|�@��y@�&�@�j@�9X@�b@߾w@�C�@ݲ-@ۍP@�
=@�^5@��`@ם�@��@�@��@և+@�E�@��@�/@���@���@�?}@ЋD@ϥ�@��@Ώ\@�=q@���@ͩ�@͙�@�O�@ˍP@�~�@�V@�E�@�$�@Ǿw@�K�@�~�@�{@��#@�x�@�?}@�z�@î@�+@��@���@�%@�(�@���@�"�@���@��y@��y@��H@�n�@��^@�O�@�z�@���@��P@�33@�{@���@�O�@�%@���@���@���@��`@�V@�?}@�p�@���@��@��@�j@�V@��/@�p�@�G�@�j@�S�@���@���@��7@��#@���@��P@�  @��9@��D@��@��m@��@��;@�"�@��@�p�@� �@�J@���@�Ĝ@���@���@���@�Q�@���@�"�@��@�O�@�7L@���@�Q�@� �@�K�@�^5@��@��-@�X@�X@�?}@�V@��`@���@��D@���@��@�bN@�b@��w@���@�\)@�
=@��R@��\@�~�@�^5@��h@�1'@�1@���@���@��y@��+@�^5@��@��T@���@���@�p�@��@��@���@��j@�Ĝ@�Z@��@�ȴ@���@�E�@���@�!�@~��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B'�B6FBT�B�B�uB��B��B��B��B�B�qB�B�TB�B�B��B%BDB'�BK�BZBiyBw�B�PB��B��B��B��B��B��B��B�B�B�B�B�B��B��B�\Bw�Br�B��B�RBƨBǮBǮBǮBǮBƨBŢB��B�B��B�bB�DB�+Bu�BW
B"�B��B��Bv�BZBS�BS�B[#BgmB�%B�bBw�BhsBR�B(�B�B�B7LBL�BT�BO�BH�B:^B(�B�B\B
��B
��B
�FB
��B
��B
��B
�DB
|�B
dZB
P�B
9XB
�B
DB	��B	�B	�NB	�#B	�B	��B	�jB	�LB	�3B	��B	��B	�hB	�DB	�+B	�B	{�B	k�B	ZB	C�B	<jB	6FB	2-B	.B	)�B	&�B	#�B	�B	�B	�B	DB��B�B�B�TB�;B�B��BɺBĜB�wB�FB�-B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�hB�bB�\B�VB�JB�PB�DB�VB�VB�VB�PB�bB�bB�\B�VB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�-B�3B�9B�?B�?B�?B�RB�jB�jB�jB�qB�}B��B��B��B��BBBÖBɺB��B��B��B��B��B��B��B��B��B��B��B�#B�5B�5B�/B�/B�NB�TB�ZB�fB�sB�yB�B�B�B�B��B��B��B	  B	B	B	B	B	B	B	B	1B		7B	PB	\B	bB	hB	�B	�B	�B	 �B	$�B	$�B	%�B	)�B	+B	-B	.B	1'B	6FB	9XB	:^B	?}B	A�B	D�B	D�B	B�B	B�B	B�B	D�B	E�B	J�B	W
B	YB	^5B	cTB	e`B	e`B	e`B	hsB	k�B	l�B	jB	hsB	dZB	`BB	bNB	bNB	cTB	cTB	bNB	bNB	bNB	cTB	iyB	o�B	s�B	u�B	u�B	v�B	z�B	}�B	~�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�9B	�3B	�FB	�LB	�XB	�dB
�B
S22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B'�B6FBT�B�B�uB��B��B��B��B�B�qB�B�TB�B�B��B%BDB'�BK�BZBiyBw�B�PB��B��B��B��B��B��B��B�B�B�B�B�B��B��B�\Bw�Br�B��B�RBƨBǮBǮBǮBǮBƨBŢB��B�B��B�bB�DB�+Bu�BW
B"�B��B��Bv�BZBS�BS�B[#BgmB�%B�bBw�BhsBR�B(�B�B�B7LBL�BT�BO�BH�B:^B(�B�B\B
��B
��B
�FB
��B
��B
��B
�DB
|�B
dZB
P�B
9XB
�B
DB	��B	�B	�NB	�#B	�B	��B	�jB	�LB	�3B	��B	��B	�hB	�DB	�+B	�B	{�B	k�B	ZB	C�B	<jB	6FB	2-B	.B	)�B	&�B	#�B	�B	�B	�B	DB��B�B�B�TB�;B�B��BɺBĜB�wB�FB�-B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�hB�bB�\B�VB�JB�PB�DB�VB�VB�VB�PB�bB�bB�\B�VB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�-B�3B�9B�?B�?B�?B�RB�jB�jB�jB�qB�}B��B��B��B��BBBÖBɺB��B��B��B��B��B��B��B��B��B��B��B�#B�5B�5B�/B�/B�NB�TB�ZB�fB�sB�yB�B�B�B�B��B��B��B	  B	B	B	B	B	B	B	B	1B		7B	PB	\B	bB	hB	�B	�B	�B	 �B	$�B	$�B	%�B	)�B	+B	-B	.B	1'B	6FB	9XB	:^B	?}B	A�B	D�B	D�B	B�B	B�B	B�B	D�B	E�B	J�B	W
B	YB	^5B	cTB	e`B	e`B	e`B	hsB	k�B	l�B	jB	hsB	dZB	`BB	bNB	bNB	cTB	cTB	bNB	bNB	bNB	cTB	iyB	o�B	s�B	u�B	u�B	v�B	z�B	}�B	~�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�9B	�3B	�FB	�LB	�XB	�dB
�B
S22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191656                              AO  ARCAADJP                                                                    20181005191656    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191656  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191656  QCF$                G�O�G�O�G�O�8000            