CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-03-01T02:50:47Z creation;2014-07-22T22:21:19Z update;2015-06-09T19:56:35Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050301025047  20150614052517  A5_23632_017                    2C  D   APEX                            1557                            013004                          846 @ӭ З�+1   @ӭ<�u�@4�"��`B�b��n��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ah  A�  A�33A�33B��B  B0��BC33BY33Bm33B���B���B���B�ffB���B���B�33B���B�  B���B㙚B�ffB���CffC� C33C�C��CffC�C$� C)33C.� C3L�C8ffC=�CBffCGL�CQffC[��CeffCoL�CyffC��fC�� C�s3C��3C���C��3C�� C���C��fC���C��fC���C�� C�C�� C̙�Cљ�C���C�� C���C噚C�fC�3C��fC���D�3D� D� D�fD��DٚD�fD$��D)� D.�3D3�fD8�fD=�fDB��DG� DL��DQ�3DV�3D[��D`�fDe� Dj��Do�3Dt��Dy�fD�&fD�ffD���D��fD�)�D�l�D���D��D��D�i�D���D�� D�#3D�i�Dک�D�ٚD�&fD�ffD� D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA  Ad��A�ffA���A陚B��B33B0  BBffBXffBlffB�33B�ffB�ffB�  B�33B�33B���B�ffBЙ�B�ffB�33B�  B�33C33CL�C  C�fCffC33C�fC$L�C)  C.L�C3�C833C<�fCB33CG�CQ33C[ffCe33Co�Cy33C���C�ffC�Y�C���C��3C���C��fC�� C���C�� C���C�� C�ffC�s3CǦfC̀ Cр Cֳ3CۦfC�� C� C��CC��C�s3D�fD�3D�3D��D� D��D��D$� D)�3D.�fD3��D8��D=��DB� DG�3DL��DQ�fDV�fD[� D`��De�3Dj� Do�fDt� Dy��D�  D�` D��3D�� D�#3D�ffD��fD��3D�fD�c3D��3D�ٚD��D�c3Dڣ3D��3D�  D�` D�D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA���A���A�z�A�ffA�\)A�\)A�XA�VA�\)A�\)A�K�A�G�A��A��
A��A�;dA��9A�ĜA�%A���A��yA���A�ȴA��;A�bA���A��HA��TA���A�jA���A�hsA��PA��A� �A��A�VA���A��9A�9XAv��AuC�AahsAZ��AN$�AB�DA;�^A3S�A,A)��A%7LA ffA�#Av�A�A9XA�`AJA"�@�33@���@�;d@䛦@���@��@�{@�"�@���@�@��@�
=@�M�@���@�b@��@��@�r�@� �@��@�x�@���@��@���@�C�@��@�ȴ@�j@��!@�Q�@�v�@�Z@���@��@�ff@��@y��@p�u@h�u@b-@Y��@Q�#@I�@B�\@:J@3��@,�@&{@ r�@1@�@33@��@t�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA���A���A�z�A�ffA�\)A�\)A�XA�VA�\)A�\)A�K�A�G�A��A��
A��A�;dA��9A�ĜA�%A���A��yA���A�ȴA��;A�bA���A��HA��TA���A�jA���A�hsA��PA��A� �A��A�VA���A��9A�9XAv��AuC�AahsAZ��AN$�AB�DA;�^A3S�A,A)��A%7LA ffA�#Av�A�A9XA�`AJA"�@�33@���@�;d@䛦@���@��@�{@�"�@���@�@��@�
=@�M�@���@�b@��@��@�r�@� �@��@�x�@���@��@���@�C�@��@�ȴ@�j@��!@�Q�@�v�@�Z@���@��@�ff@��@y��@p�u@h�u@b-@Y��@Q�#@I�@B�\@:J@3��@,�@&{@ r�@1@�@33@��@t�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1'B1'B33B33B49B6FB9XB:^B;dB<jB=qB@�B@�BH�BVBcTBr�B�BÖB�9B�B��B��BB��B�B�NBɺB��B�+BXB
=B��B�B�hBt�B$�B
�B
�B
{�B
�B	��B	�\B��B�)B��B_;B?}B1'B/B8RBL�BJ�BH�BG�BG�BD�B?}B;dB8RB8RB:^B5?B.B9XBI�BK�B]/Bx�B�+B�uB�BɺB�;B�B	'�B	K�B	ZB	gmB	x�B	�VB	��B	��B	�?B	�wB	ǮB	��B	�B	�;B	�mB	�B	�B	�B	��B	��B
%B
bB
�B
 �B
&�B
.B
5?B
=qB
C�B
I�B
N�B
T�B
[#B
`BB
e`B
hsB
m�B
r�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B1'B1'B33B33B49B6FB9XB:^B;dB<jB=qB@�B@�BH�BVBcTBr�B�BÖB�9B�B��B��BB��B�B�NBɺB��B�+BXB
=B��B�B�hBt�B$�B
�B
�B
{�B
�B	��B	�\B��B�)B��B_;B?}B1'B/B8RBL�BJ�BH�BG�BG�BD�B?}B;dB8RB8RB:^B5?B.B9XBI�BK�B]/Bx�B�+B�uB�BɺB�;B�B	'�B	K�B	ZB	gmB	x�B	�VB	��B	��B	�?B	�wB	ǮB	��B	�B	�;B	�mB	�B	�B	�B	��B	��B
%B
bB
�B
 �B
&�B
.B
5?B
=qB
C�B
I�B
N�B
T�B
[#B
`BB
e`B
hsB
m�B
r�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                               The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200503120000002005031200000020050312000000200506090000002005060900000020050609000000JA  ARFMfmtp2.1                                                                 20050301025047  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050301025049  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050301025752                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050305004857  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050305004857  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20050305004857  CV  TIME            G�O�G�O�F�h                JA  ARGQrelo2.1                                                                 20050305004857  CV  LAT$            G�O�G�O�A���                JA  ARGQrelo2.1                                                                 20050305004857  CV  LON$            G�O�G�O���                JA  ARUP                                                                        20050305005429                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050312000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050312000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050609000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060310063556  IP                  G�O�G�O�G�O�                JA  RFMTcnv22.0                                                                 20060414072359  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060418030305                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120629  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072458  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073016                      G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20140715031140  CF  POSITION_QC     G�O�G�O�@�                  JA  RFMTcnvd2.1                                                                 20140722221959  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20140722222119                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609195629                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052517                      G�O�G�O�G�O�                