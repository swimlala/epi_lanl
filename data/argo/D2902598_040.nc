CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   e   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CSIO   source        
Argo float     history       92015-08-10T12:13:35Z creation;2015-12-22T01:44:38Z update      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7    PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8$   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8D   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8H   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8P   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8T   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8\   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8d   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8l   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8p   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8x   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8|   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  ;   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  =   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  @�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  B�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  F4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  h  H0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  J,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    J\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    M\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    P\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  S\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    S�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    S�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    S�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    S�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  S�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    S�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    S�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    S�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         S�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         T    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    TArgo profile    3.1 1.2 19500101000000  20150810121335  20200707075746  2902598 PROVOR                          CHINA ARGO EQUIVALENT                                           ZENGHONG LIU                                                    PRES            TEMP            PSAL               (A   HZ  0277_135043_040                 2C  D   OIN-13CH-S31-46                 N/A                             841 @�f}A
��1   @�f}A
��@4N��O�;�d%G�z�1   ARGOS   A   A   A   Primary sampling: averaged []                                                                                                                                                                                                                                      ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C;  CD  CU  Cm  C�  C�  C�� C�  C�  C�  C΀ C�  C� C�  D @ D� D� D@ D@ D� D%� D+� D2@ D8� D>� DE  DK@ DR  DW� D^  Dd@ Dj@ Dp� Dw  D}@ D�� D�� D�@ D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C;  CD  CU  Cm  C�  C�  C�� C�  C�  C�  C΀ C�  C� C�  D @ D� D� D@ D@ D� D%� D+� D2@ D8� D>� DE  DK@ DR  DW� D^  Dd@ Dj@ Dp� Dw  D}@ D�� D�� D�@ D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�bA��A��A�A���A��A���A���A�  A���A��A�v�A��TA�JA���AѲ-A�A�p�A�ȴA�7LA��A�&�A�p�A���A���A�{A��A�n�A��At^5AX5?A=�FA-hsA��A(�Al�A�7@��T@���@ް!@��H@�\)@�@���@��\@��@�C�@��w@�O�@��H@��w@�%@���@��@��H@��D@���@�33@��9@��@��@~5?@xĜ@t(�@pr�@lZ@hĜ@d�@a�@^V@Z�@Up�@PA�@J�!@F$�@B��@?l�@<j@9�@7|�@4�@0r�@-�h@*�!@(1'@%p�@#33@ �`@`B@�!@7L@�R@��@=q@A�@E�@�/@dZ@
~�@	�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�bA��A��A�A���A��A���A���A�  A���A��A�v�A��TA�JA���AѲ-A�A�p�A�ȴA�7LA��A�&�A�p�A���A���A�{A��A�n�A��At^5AX5?A=�FA-hsA��A(�Al�A�7@��T@���@ް!@��H@�\)@�@���@��\@��@�C�@��w@�O�@��H@��w@�%@���@��@��H@��D@���@�33@��9@��@��@~5?@xĜ@t(�@pr�@lZ@hĜ@d�@a�@^V@Z�@Up�@PA�@J�!@F$�@B��@?l�@<j@9�@7|�@4�@0r�@-�h@*�!@(1'@%p�@#33@ �`@`B@�!@7L@�R@��@=q@A�@E�@�/@dZ@
~�@	�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
}�B
v�B
� B
�B
�B
�B
�B
�B
� B
~�B
� B
��B�?B�BC�B�7B�B�5B%�B/B�NBe`B+BȴB�7BPB
�}B
ZB	�?B	$�BɺBĜB�PBy�Bk�Bm�Bs�Bn�Bx�B��B�B��B��B	�B	7LB	YB	q�B	�=B	��B	�3B	ƨB	��B	�BB	�fB	�B	�B	��B	��B
  B
B
%B
DB
VB
oB
{B
�B
�B
�B
�B
#�B
'�B
,B
2-B
6FB
:^B
>wB
A�B
C�B
F�B
H�B
L�B
N�B
P�B
R�B
VB
XB
ZB
[#B
_;B
aHB
cTB
ffB
hsB
jB
l�B
n�B
o�B
p�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
}�B
w�B
�dB
�B
��B
��B
��B
�QB
�&B
 B
�B
�VB�B�BG�B��B�WB�\B*gB4�B��BlB'B�?B��B�B
��B
^�B	�3B	*_B�UB��B�hB{�BmXBo�Bu?BpDBz=B�zB�	B��B�RB	KB	7�B	Y�B	q�B	��B	�(B	��B	�B	�WB	�B	�B	��B	��B	�B	�?B
 �B
aB
|B
�B
�B
�B
�B
�B
�B
�B
 B
$#B
(DB
,bB
2zB
6}B
:�B
>�B
A�B
C�B
F�B
H�B
L�B
O
B
QB
SB
V)B
X6B
ZWB
[OB
_SB
aqB
ctB
f�B
h�B
j�B
l�B
n�B
o�B
p�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES = PRES - [PRES_SurfaceOffsetCorrectedNotResetNegative_1dbarResolution_dbar]; PRES_ADJUSTED = PRES                                                                                                                                                          no change                                                                                                                                                                                                                                                       PSAL_ADJUSTED = sal(CNDC,TEMP,PRES_ADJUSTED); PSAL_ADJ corrects conductivity cell thermal mass (CTM), Johnson et al, 2007, JAOT                                                                                                                                 PRES_SurfaceOffsetCorrectedNotResetNegative_1dbarResolution_dbar in TECH file for N-1 profile                                                                                                                                                                   no change                                                                                                                                                                                                                                                       same as for PRES_ADJUSTED; CTL: alpha=0.1410, tau=6.68;                                                                                                                                                                                                         Pressures adjusted using PRES_SurfaceOffsetCorrectedNotResetNegative_1dbarResolution_dbar; Pressure drift corrected onboard the float; Manufacturers sensor accuracy                                                                                            No significant temperature drift detected; SBE sensor accuracy                                                                                                                                                                                                  No significant salinity drift detected; Manufacturers sensor accuracy;                                                                                                                                                                                          202007070757462020070707574620200707075746  HZ  ARGQ                                                                        20150810121335  QCP$                G�O�G�O�G�O�D7BFE           HZ  ARGQ                                                                        20150810121335  QCF$                G�O�G�O�G�O�0               HZ  ARGQ                                                                        20150810121335                      G�O�G�O�G�O�                HZ  ARGQ                                                                        20150810121335                      G�O�G�O�G�O�                HZ  ARSQCTL v1.0                                                                20200707075216  QC  PSAL            ?�  D�  G�O�                HZ  ARSQSIQCV2.0WOD2001 & Argo                                                  20200707075737  IP                  ?�  D�  G�O�                