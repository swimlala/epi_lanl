CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-12-05T18:52:25Z creation;2009-03-18T07:19:32Z update;2015-06-09T19:36:39Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900649 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  20051205185225  20150621172514  A5_23579_045                    2C  D   APEX                            1556                            013004                          846 @���?Vx1   @���"�E@6��vȴ9�c3S���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Ah  A�33A�33A���B	33B  B133BD  BZ  Bk��B�  B�33B���B���B�ffB�  B���Bƙ�BЙ�B�ffB�ffB�33B���C��CffC��C� C33CL�C� C$L�C)�C.  C3�C8ffC=  CB  CG33CQL�C[  Ce33CoL�CyffC�s3C�s3C���C��3C��fC��3C�� C���C��3C��fC���C���C���C¦fC�s3C̳3CѦfC�� Cۙ�C���C� C� C�3C��3C�� D�fD� D� D�fD� D��D��D$�3D)�fD.� D3ٚD8� D=� DB�3DG� DLٚDQ� DV� D[�fD`ٚDe� Dj� Do��Dt��Dy�3D�,�D�i�D���D�� D�#3D�l�D���D���D�#3D�ffD��fD��D�  D�i�Dڬ�D��3D�&fD�i�D�fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA��Ad��A���A���A�33BffB33B0ffBC33BY33Bj��B33B���B�33B�ffB�  B���B�ffB�33B�33B�  B�  B���B�ffCffC33CffCL�C  C�CL�C$�C(�fC-��C2�fC833C<��CA��CG  CQ�CZ��Ce  Co�Cy33C�Y�C�Y�C�� C���C���C���C��fC�s3C���C���C�� C�� C�� C�C�Y�C̙�Cь�C֦fCۀ C�� C�ffC�ffCC���C��fD��D�3D�3D��D�3D��D� D$�fD)ٚD.�3D3��D8�3D=�3DB�fDG�3DL��DQ�3DV�3D[ٚD`��De�3Dj�3Do��Dt��Dy�fD�&fD�c3D��fD�ٚD��D�ffD��fD��fD��D�` D�� D��3D��D�c3DڦfD���D�  D�c3D� D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aģ�A�JA�A�A�VA�"�A�G�A� �A�(�AŶFAŸRA���A��HAžwA���AŰ!Aŗ�A�hsA��A��A��`A�9XA��`A�1A�O�A���A�%A�E�A��`A�"�A�ZA���A�ZA��A��`A���A���A���A�A�JA�~�A��`A�XA��#A�{A|-Ao��Ae�AV�AQ&�AHM�ABr�A@�!A6�`A1��A,�/A)�hA$��A!�A��Av�A��A��A�A�TA
$�A�7A�mA��@��/@��`@�@���@Ɂ@���@��-@��T@�@��!@�V@��@��@��@��/@��!@�Ĝ@�1@�M�@�x�@�b@���@�G�@���@��j@�33@�p�@���@w��@n��@f�@_�w@X1'@QX@K"�@D��@>ff@97L@3�m@-�@(�u@#C�@�@�@��@bN@�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aģ�A�JA�A�A�VA�"�A�G�A� �A�(�AŶFAŸRA���A��HAžwA���AŰ!Aŗ�A�hsA��A��A��`A�9XA��`A�1A�O�A���A�%A�E�A��`A�"�A�ZA���A�ZA��A��`A���A���A���A�A�JA�~�A��`A�XA��#A�{A|-Ao��Ae�AV�AQ&�AHM�ABr�A@�!A6�`A1��A,�/A)�hA$��A!�A��Av�A��A��A�A�TA
$�A�7A�mA��@��/@��`@�@���@Ɂ@���@��-@��T@�@��!@�V@��@��@��@��/@��!@�Ĝ@�1@�M�@�x�@�b@���@�G�@���@��j@�33@�p�@���@w��@n��@f�@_�w@X1'@QX@K"�@D��@>ff@97L@3�m@-�@(�u@#C�@�@�@��@bN@�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTB�1B�-B�
B�B�BuB�BM�BW
Bv�B�jB��B�RB��B�/B�`B�TBB�B�/B�9B^5Bq�B��B��B��B��B�jB�qB��B�^B��B|�BE�B:^B�B�yB�7BA�BH�B1'B
�DB
�uB	�B	�uB	?}B�B�B�-B�%B�Bk�Bz�Bu�Bo�Bp�Bu�Bo�Bn�BgmBaHB[#B\)BN�BG�B^5Bs�BR�B1'B.BR�B�B�JB��B�?BÖB�fB	B��B	bB	!�B	$�B	33B	P�B	n�B	�B	��B	��B	�3B	��B	��B	�B	�BB	�fB	��B	��B
	7B
hB
�B
"�B
+B
1'B
8RB
?}B
B�B
I�B
N�B
W
B
[#B
^5B
cTB
hsB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BcTB�1B�-B�
B�B�BuB�BM�BW
Bv�B�jB��B�RB��B�/B�`B�TBB�B�/B�9B^5Bq�B��B��B��B��B�jB�qB��B�^B��B|�BE�B:^B�B�yB�7BA�BH�B1'B
�DB
�uB	�B	�uB	?}B�B�B�-B�%B�Bk�Bz�Bu�Bo�Bp�Bu�Bo�Bn�BgmBaHB[#B\)BN�BG�B^5Bs�BR�B1'B.BR�B�B�JB��B�?BÖB�fB	B��B	bB	!�B	$�B	33B	P�B	n�B	�B	��B	��B	�3B	��B	��B	�B	�BB	�fB	��B	��B
	7B
hB
�B
"�B
+B
1'B
8RB
?}B
B�B
I�B
N�B
W
B
[#B
^5B
cTB
hsB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200512190000002005121900000020051219000000200604190000002006041900000020060419000000JA  ARFMfmtp2.2                                                                 20051205185225  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051205185226  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051205190429                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20051209105052  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051209105053  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051209105900                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20051219000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20051219000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013416  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013643                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120559  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071810  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318071932                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609193634                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621172514                      G�O�G�O�G�O�                