CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-06-01T10:54:21Z creation;2011-06-06T00:20:04Z update;2015-06-09T09:28:50Z conversion to V3.1;     
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
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  5900492 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  20050601105421  20150621114513  A5_21055_048                    2C  D   APEX                            1091                            061703                          846 @���(�1   @����0��@&��hr��d:E����1   ARGOS   F   F   F   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���AffA`  A�33A���A�ffB��B��B0  BD��BV��Bk��B��B���B���B���B�  B���B���B�33B�  Bۙ�B���B�ffB���CL�CL�CL�C� CffCL�C33C$ffC)��C.ffC3ffC8  C=33CB��CG�3CQ33C[33CeL�CoffCyL�C�� C��3C�� C�� C��3C�s3C�� C�s3C�� C�� C��3C�s3C�� C�� Cǀ C̦fCљ�C�� C�� C�3C噚CꙚC�3C�s3C���D��D�fD�3D��D� D3D"L�D(�fD.��D4�3D;,�DA�fDG��DN  DTS3DZ�fD`�fDf�3DmS3Ds��Dy��D�)�D�l�D��fD�� D�&fD�p D��3D���D��D�i�D��3D��3D�&fD�c3Dڬ�D�� D��D�VfD� D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���AffA`  A�33A���A�ffB��B��B0  BD��BV��Bk��B��B���B���B���B�  B���B���B�33B�  Bۙ�B���B�ffB���CL�CL�CL�C� CffCL�C33C$ffC)��C.ffC3ffC8  C=33CB��CG�3CQ33C[33CeL�CoffCyL�C�� C��3C�� C�� C��3C�s3C�� C�s3C�� C�� C��3C�s3C�� C�� Cǀ C̦fCљ�C�� C�� C�3C噚CꙚC�3C�s3C���D��D�fD�3D��D� D3D"L�D(�fD.��D4�3D;,�DA�fDG��DN  DTS3DZ�fD`�fDf�3DmS3Ds��Dy��D�)�D�l�D��fD�� D�&fD�p D��3D���D��D�i�D��3D��3D�&fD�c3Dڬ�D�� D��D�VfD� D�I�333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aى7A�n�A�`BA�`BA�dZA�dZA�dZA�\)A�^5A�\)A�ZA�XA�M�A��A�-A־wAָRA֑hA֏\A�r�A��Aө�A�bNA�1'A�5?A���A�dZA�G�A���A� �Au�Ag��A_��A\�9AW�FAM�AIp�AHz�AB�A<1A4��A3�;A3S�A0ȴA.��A-G�A+��A)��A({A&JA%"�A#C�A!ƨAƨAI�A;dA5?A�7A�TAA�TA5?A�FA��A��AK�A�AO�A
v�A�A�Ab@�t�@�t�@�9X@��/@۶F@�{@�ƨ@��@�Q�@�C�@��@��@��P@�t�@���@���@��^@�v�@���@�;d@���@�1@���@y��@l�/@a��@Z~�@QG�@JJ@Cƨ@:�H@3��@-`B@%��@��@-@��@�u@v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aى7A�n�A�`BA�`BA�dZA�dZA�dZA�\)A�^5A�\)A�ZA�XA�M�A��A�-A־wAָRA֑hA֏\A�r�A��Aө�A�bNA�1'A�5?A���A�dZA�G�A���A� �Au�Ag��A_��A\�9AW�FAM�AIp�AHz�AB�A<1A4��A3�;A3S�A0ȴA.��A-G�A+��A)��A({A&JA%"�A#C�A!ƨAƨAI�A;dA5?A�7A�TAA�TA5?A�FA��A��AK�A�AO�A
v�A�A�Ab@�t�@�t�@�9X@��/@۶F@�{@�ƨ@��@�Q�@�C�@��@��@��P@�t�@���@���@��^@�v�@���@�;d@���@�1@���@y��@l�/@a��@Z~�@QG�@JJ@Cƨ@:�H@3��@-`B@%��@��@-@��@�u@v�333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
~�B
p�B
jB
p�B
n�B
o�B
q�B
r�B
x�B
�bB
�hB
�FB
��B
x�B
XB	�BB	�B	��B	�oB	��B	��B	�oB	�B	�B	��B
T�B
��B
��B
�XB
ŢB
��B
��B
��B
��B
ȴB
ÖB
��B
�}B
�RB
�LB
�-B
�-B
�B
��B
��B
�{B
�oB
�VB
�1B
�B
|�B
w�B
r�B
m�B
k�B
gmB
]/B
W
B
J�B
;dB
.B
&�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
$�B
%�B
'�B
)�B
(�B
0!B
49B
8RB
A�B
F�B
M�B
S�B
W
B
\)B
bNB
e`B
jB
l�B
r�B
w�B
|�B
�B
�B
�7B
�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
� B
p�B
jB
p�B
n�B
o�B
q�B
u�B
�1B
��B
��B
�jB
��B
{�B
_;B	�sB	�LB	��B	��B	��B	��B	��B	�'B	�B	��B
XB
��B
��B
�XB
ƨB
��B
��B
��B
��B
ȴB
ĜB
��B
�}B
�RB
�RB
�-B
�-B
�B
��B
��B
�{B
�oB
�VB
�7B
�B
}�B
w�B
r�B
m�B
k�B
hsB
]/B
XB
K�B
<jB
/B
&�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
$�B
%�B
'�B
)�B
(�B
0!B
49B
8RB
A�B
F�B
M�B
S�B
W
B
\)B
bNB
e`B
jB
l�B
r�B
w�B
|�B
�B
�B
�7B
�D333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<�9X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200506141115552005061411155520050614111555200701262003552007012620035520070126200355200709280000002007092800000020070928000000  JA  ARFMfmtp2.2                                                                 20050601105421  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050601105422  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050601110502                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050604224811  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050604224811  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20050604224811  CV  TIME            G�O�G�O�F��                JA  ARGQrelo2.1                                                                 20050604224811  CV  LAT$            G�O�G�O�A5ƨ                JA  ARGQrelo2.1                                                                 20050604224811  CV  LON$            G�O�G�O��!�{                JA  ARUP                                                                        20050604225338                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20050614111555  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20050614111555  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126200355  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070928000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071017052409  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071017074002                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110527053355  CV  JULD            G�O�G�O�F��                JM  AREQREJM1.0                                                                 20110527053355  CF  PRES_ADJUSTED_QC@���D�I�G�O�                JM  AREQREJM1.0                                                                 20110527053355  CF  TEMP_ADJUSTED_QC@���D�I�G�O�                JM  AREQREJM1.0                                                                 20110527053355  CF  PSAL_ADJUSTED_QC@���D�I�G�O�                JA  RFMTcnvd2.1                                                                 20110606001751  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110606002004                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609092839                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621114513                      G�O�G�O�G�O�                