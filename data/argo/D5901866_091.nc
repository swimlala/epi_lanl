CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   F   N_CALIB       	N_HISTORY             	   history       d2010-12-29T15:08:32Z creation; 2019-09-23T16:26:23Z updated; 2019-10-08T17:26:38Z converted from 2.2   title         Argo float vertical profile    institution       AOML   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   axis      T      
resolution        >�E�vQ�        8   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        8   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8$   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8,   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    80   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    88   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8<   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8@   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          8D   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       9�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ;   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       <   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  =4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       =|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       >�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ?�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  A   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       AT   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  Bl   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       B�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  C�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    C�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    F�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    I�   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  L�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    N�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    N�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    N�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         O    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         O   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        O   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    O   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    M(   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        N(   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     N,   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     NL   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     NlArgo profile    3.1 1.2 19500101000000  5901866 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               [A   AO  20101229150832  20191008173744  2530_37205_091                  2C  D   846 @��g���1   @���y` @(_;dZ��c�G�z�1   ARGOS   A   A   A   @�ffA!��A���A���A�  B  B3��BQ��Bn  B�ffB�33B���B�ffBǙ�B�  B�33B���C� C  CL�C��C"��C+  C3��C=��CH  CQL�C\�3Cg33Cs� C�3C��3C���C��3C�&fC��C�s3C��3C�� C�L�C�s3C�@ C��3C��fD9�D��D  DL�D3D&S3D.��D7�fDA  DK3DU��D`�3Dl�Dx3D�i�D�#3D�#3D��3D�C3D��fD�fD�  D�D�|�D��fDٶf1111111111111111111111111111111111111111111111111111111111111111111111  @�ffA!��A���A���A�  B  B3��BQ��Bn  B�ffB�33B���B�ffBǙ�B�  B�33B���C� C  CL�C��C"��C+  C3��C=��CH  CQL�C\�3Cg33Cs� C�3C��3C���C��3C�&fC��C�s3C��3C�� C�L�C�s3C�@ C��3C��fD9�D��D  DL�D3D&S3D.��D7�fDA  DK3DU��D`�3Dl�Dx3D�i�D�#3D�#3D��3D�C3D��fD�fD�  D�D�|�D��fDٶf2222222222222222222222222222222222222222222222222222222222222222222222  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�VA��A��A�I�A�v�AӋDAҲ-A�  A���A���A�
=A���A�I�A�&�Az��A_�AW
=AF��ADn�A:�9A6�A6jA5�A3\)A/�
A,ZA)\)A&A�A$��AhsAG�A=qA�uA�`AZAĜAA�AbA�A	XA33A�@��@�33@�O�@�dZ@�-@޸R@֏\@�Q�@�C�@�33@��@���@�  @��;@�p�@��w@��R@�+@� �@sS�@h�u@]�@P��@D�/@=��@2��@( �@%�1111111111111111111111111111111111111111111111111111111111111111111111  A�VA��A��A�I�A�v�AӋDAҲ-A�  A���A���A�
=A���A�I�A�&�Az��A_�AW
=AF��ADn�A:�9A6�A6jA5�A3\)A/�
A,ZA)\)A&A�A$��AhsAG�A=qA�uA�`AZAĜAA�AbA�A	XA33A�@��@�33@�O�@�dZ@�-@޸R@֏\@�Q�@�C�@�33@��@���@�  @��;@�p�@��w@��R@�+@� �@sS�@h�u@]�@P��@D�/@=��@2��@( �@%�2222222222222222222222222222222222222222222222222222222222222222222222  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�+B��BB�NB	^5B	ffB	��B	�B
hB
�B
DB	��B	k�B	�B	)�B	�B	�B	�+B	�XB
B
C�B
S�B
[#B
dZB
\)B
]/B
m�B
O�B
D�B
N�B
`BB
l�B
w�B
w�B
s�B
l�B
ffB
aHB
[#B
Q�B
J�B
E�B
C�B
9XB
/B
+B
'�B
$�B
"�B
�B
�B
�B
"�B
(�B
.B
1'B
5?B
<jB
B�B
J�B
P�B
W
B
aHB
ffB
jB
r�B
y�B
|�1111111111111111111111111111111111111111111111111111111111111111111111  B}�B}�B� B��B�dB�)B	XB	e`B	��B	�B
�B
�B

=B	��B	m�B	�B	'�B	�B	� B	�B	�-B	��B
=qB
M�B
T�B
^5B
VB
W
B
gmB
I�B
>wB
G�B
YB
e`B
p�B
p�B
l�B
e`B
_;B
ZB
T�B
K�B
C�B
>wB
<jB
33B
'�B
$�B
 �B
�B
�B
�B
uB
�B
�B
!�B
&�B
)�B
.B
5?B
;dB
C�B
I�B
O�B
ZB
_;B
cTB
k�B
r�B
u�2222222222222222222222222222222222222222222222222222222222222222222222  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0 dbar                                                                                                                                                                                                                          none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0), vertically averaged dS = -0.007 (+/-0)                                                                                                                            Pressures adjusted using reported surface pressure when warranted.  TNPD: APEX float that truncated negative pressure drift.                                                                                                                                    none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      201909231626232019092316262320190923162623  Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                APEX                            3582                            052405                          AO  ARGQ                                                                        20101229150832  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20101229150832  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20110407103848  QC  PRES            @�ffDٶf                    PM  ARSQCTM V1.1                                                                20110407103848  QC  PSAL            @�ffDٶf                    PM  ARSQOWGUV1.0                                                                20191008173744  IP                  G�O�G�O�G�O�                