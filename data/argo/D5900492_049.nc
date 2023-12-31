CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-06-11T00:48:27Z creation;2011-06-06T00:20:04Z update;2015-06-09T09:28:57Z conversion to V3.1;     
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
_FillValue                  p  ;\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Ap   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  A�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  G�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ll   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Ul   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  gl   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    hL   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    hP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         hd   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        hh   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  5900492 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               1A   JA  20050611004827  20150621114513  A5_21055_049                    2C  D   APEX                            1091                            061703                          846 @��r333@1   @��s����@&�(�\�dF��n�1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  A`  A���A�ffA�ffB	��B33B0��BE33BY33BnffB���B�33B�  B���B���B�  B�33B�ffB���B�33B䙚B�33B�33C��CffC  C33C�3C��CffC$��C)�3C.��C3L�C8  C=33CBffCG33CQL�CZ�fCe33Co�3CyffC�� C�� C��3C�s3C�� C���C��3C��fC���C��3C��3C�� C��3C�Cǌ�C̦fCь�C�ffC�� C�3C���C�fC���C��C���D�3D�fD��D��DٚD  D"S3D(�fD.��D5  D;L�DA��DG�3DN,�DT` DZ�fD`ٚDg�DmY�Ds�fDy��D�,�D�ffD�� D��3D�,�D�ffD���D��fD�,�D�l�D��3D�ٚD�#3D�c3Dڜ�D���D��D�c3D�fD�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A  A`  A���A�ffA�ffB	��B33B0��BE33BY33BnffB���B�33B�  B���B���B�  B�33B�ffB���B�33B䙚B�33B�33C��CffC  C33C�3C��CffC$��C)�3C.��C3L�C8  C=33CBffCG33CQL�CZ�fCe33Co�3CyffC�� C�� C��3C�s3C�� C���C��3C��fC���C��3C��3C�� C��3C�Cǌ�C̦fCь�C�ffC�� C�3C���C�fC���C��C���D�3D�fD��D��DٚD  D"S3D(�fD.��D5  D;L�DA��DG�3DN,�DT` DZ�fD`ٚDg�DmY�Ds�fDy��D�,�D�ffD�� D��3D�,�D�ffD���D��fD�,�D�l�D��3D�ٚD�#3D�c3Dڜ�D���D��D�c3D�fD�ff333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA���A���AּjA�A�ĜA���A���A���A���A��
A��#A���A���A�p�A�^5AɋDA��/A�dZA�`BA�
=A���A��#A�  A�A�A��A�t�A��;A�A�ZA��Ag�^Ad�A[�APbNAJ��AH�+AF9XAC�FAB�A:�HA6n�A3�;A0JA,��A+O�A(�RA'33A$�HA#A"��A �A VA�jA`BA��AVA�#A�A�AC�A�A=qA�A�
Ar�A|�A�A�;A�A �A�\A z�@�J@@�A�@�@�p�@�j@��@�33@�^5@�j@���@�K�@�"�@�p�@��@��F@��@�7L@���@���@���@x��@qhs@hQ�@bM�@XA�@PA�@DZ@:�!@3�
@,��@(�u@%�@�P@dZ@ff@�#@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA���A���AּjA�A�ĜA���A���A���A���A��
A��#A���A���A�p�A�^5AɋDA��/A�dZA�`BA�
=A���A��#A�  A�A�A��A�t�A��;A�A�ZA��Ag�^Ad�A[�APbNAJ��AH�+AF9XAC�FAB�A:�HA6n�A3�;A0JA,��A+O�A(�RA'33A$�HA#A"��A �A VA�jA`BA��AVA�#A�A�AC�A�A=qA�A�
Ar�A|�A�A�;A�A �A�\A z�@�J@@�A�@�@�p�@�j@��@�33@�^5@�j@���@�K�@�"�@�p�@��@��F@��@�7L@���@���@���@x��@qhs@hQ�@bM�@XA�@PA�@DZ@:�!@3�
@,��@(�u@%�@�P@dZ@ff@�#@��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
B
B
B
B
B
B
B
%B
%B
%B
B
B	�B	�B
"�B
n�B
��B
�B|�B�-B��B�!B��BiyB�B
��B
��B
� B
YB
�7B
{B	��B	��B	��B	��B	�yB	��B
�B
-B
8RB
x�B
�B
�PB
�7B
�!B
�wB
�FB
�?B
�B
�B
�B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�\B
�7B
�B
|�B
x�B
s�B
l�B
aHB
[#B
T�B
H�B
<jB
0!B
'�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
$�B
&�B
+B
-B
/B
1'B
7LB
;dB
@�B
H�B
L�B
P�B
S�B
ZB
]/B
dZB
iyB
m�B
t�B
w�B
y�B
|�B
�B
�B
�7B
�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
B
B
B
B
B
B
B
%B
%B
%B
B
B	�B	�#B
&�B
q�B
��B
��B�B�RBÖB�-B��Bn�B#�BB
��B
�B
_;B
�uB
"�B	��B	��B	��B	ÖB	�B	��B
�B
.B
:^B
y�B
�B
�VB
�=B
�!B
�}B
�FB
�FB
�B
�B
�B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�\B
�7B
�B
|�B
x�B
t�B
m�B
aHB
[#B
VB
H�B
=qB
0!B
(�B
 �B
�B
�B
�B
�B
�B
 �B
"�B
"�B
$�B
&�B
+B
-B
/B
1'B
7LB
;dB
@�B
H�B
L�B
P�B
S�B
ZB
]/B
dZB
iyB
m�B
t�B
w�B
y�B
|�B
�B
�B
�7B
�V333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
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
<e`B<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200506242320152005062423201520050624232015200701262004012007012620040120070126200401200709280000002007092800000020070928000000  JA  ARFMfmtp2.2                                                                 20050611004827  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050611004827  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050611005818                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050614204915  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050614204916  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20050614204916  CV  TIME            G�O�G�O�F�3�                JA  ARGQrelo2.1                                                                 20050614204916  CV  LAT$            G�O�G�O�A6��                JA  ARGQrelo2.1                                                                 20050614204916  CV  LON$            G�O�G�O��"7�                JA  ARUP                                                                        20050614205917                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20050624232015  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20050624232015  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126200401  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070928000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071017052410  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071017074004                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110527053357  CV  JULD            G�O�G�O�F�3�                JM  AREQREJM1.0                                                                 20110527053357  CF  PRES_ADJUSTED_QC@���D�ffG�O�                JM  AREQREJM1.0                                                                 20110527053357  CF  TEMP_ADJUSTED_QC@���D�ffG�O�                JM  AREQREJM1.0                                                                 20110527053357  CF  PSAL_ADJUSTED_QC@���D�ffG�O�                JA  RFMTcnvd2.1                                                                 20110606001739  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110606002004                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609092853                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621114513                      G�O�G�O�G�O�                