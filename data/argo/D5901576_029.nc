CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-03-15T06:58:25Z creation;2009-09-01T09:17:47Z update;2015-06-11T11:27:13Z conversion to V3.1;     
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
_FillValue                  t  ;p   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >(   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B<   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  FP   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H    PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  H�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Jd   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M8   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V8   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _8   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h8   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i,   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i0   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i4   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20090315065825  20150614170515  A9_76264_029                    2C  D   APEX                            3512                            070407                          846 @��k�61   @������@+C��$��d�ě��T1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA33AfffA�ffA�ffA�33B
  B  B1��BE33BY33Bl��B�  B�ffB�ffB���B�  B�ffB���B�ffB���Bڙ�B�  B�ffB�33C�C  CL�CffC��CffC� C$� C)ffC.� C3� C8� C=� CBffCG33CQ��C[L�Ce  Cn�fCyL�C�� C�� C���C���C���C�s3C��3C���C��fC��fC���C���C���C¦fCǌ�C̳3Cљ�Cֳ3C۳3C���C��C��C�s3C��C�� D��D� D�fD�fDٚD�3DٚD$� D)ٚD.ٚD3��D8� D=�3DB��DG�fDLٚDQ�fDV�3D[��D`� DeٚDj�fDoٚDt�fDy�3D�  D�` D��3D��3D�#3D�` D��3D���D��D�Y�D���D���D�0 D�\�DږfD���D�#3D�\�D�fD�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���AffAa��A�  A�  A���B��B��B0ffBD  BX  Bk��B�ffB���B���B�  B�ffB���B�  B���B�33B�  B�ffB���B���C ��C�3C  C�CL�C�C33C$33C)�C.33C333C833C=33CB�CF�fCQL�C[  Cd�3Cn��Cy  C���C���C�s3C�ffC�ffC�L�C���C�s3C�� C�� C��fC�ffC�ffC C�ffČ�C�s3C֌�Cی�C�ffC�ffC�ffC�L�C�ffC���D��D��D�3D�3D�fD� D�fD$��D)�fD.�fD3��D8��D=� DB��DG�3DL�fDQ�3DV� D[��D`��De�fDj�3Do�fDt�3Dy� D�fD�VfD���D�ٚD��D�VfD���D��3D� D�P D�� D��3D�&fD�S3Dڌ�D��3D��D�S3D��D��fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA˃A�S�A�?}A�-A� �A�VA���A�7LA���A���A�XA�1'AƉ7A�`BAüjA�A�r�A��A��A�;dA���A��DA���A�S�A��FA���A���A��jA��A���A�&�A���A�hsAx�RAn  Af�DA^VA[VAUl�AM"�ABJA;�A2��A*�A%�FA JA��A�A�A�#A�#AjAdZA�
A��A"�A|�A
A�A�`AA�+A��A �+@���@�G�@�E�@��
@��@���@�Z@��@�E�@���@�7L@ʸR@�@���@�ȴ@��@�-@�@���@�p�@���@���@�
=@���@�Z@�z�@�5?@���@��9@��@���@���@���@|j@s��@j�@a�7@T��@J~�@A&�@:��@4z�@-�T@)�7@#t�@O�@�@�@��@`B@
�!@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA˃A�S�A�?}A�-A� �A�VA���A�7LA���A���A�XA�1'AƉ7A�`BAüjA�A�r�A��A��A�;dA���A��DA���A�S�A��FA���A���A��jA��A���A�&�A���A�hsAx�RAn  Af�DA^VA[VAUl�AM"�ABJA;�A2��A*�A%�FA JA��A�A�A�#A�#AjAdZA�
A��A"�A|�A
A�A�`AA�+A��A �+@���@�G�@�E�@��
@��@���@�Z@��@�E�@���@�7L@ʸR@�@���@�ȴ@��@�-@�@���@�p�@���@���@�
=@���@�Z@�z�@�5?@���@��9@��@���@���@���@|j@s��@j�@a�7@T��@J~�@A&�@:��@4z�@-�T@)�7@#t�@O�@�@�@��@`B@
�!@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	��B
{B
49B
;dB
O�B
��BN�B�BuB&�B5?B2-B-B>wBH�BC�B=qB1'B�B�wB�1B$�B
�ZB
��B
,B
B	�FB	k�B	<jB	�B	B�B�mB�TB�/B	B�B�B	$�B	-B	P�B	m�B	�9B	�dB	��B	�
B	�BB	��B
B
%B
	7B
1B
1B
DB

=B
+B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
1B
	7B
PB
bB
{B
�B
�B
�B
�B
�B
 �B
'�B
-B
5?B
:^B
@�B
D�B
L�B
Q�B
XB
]/B
aHB
e`B
hsB
l�B
q�B
v�B
y�B
|�B
� B
�%B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
49B
;dB
P�B
��BO�B�B{B+B6FB49B33B@�BJ�BE�B@�B33B%�B��B�VB(�B
�yB
�B
0!B
1B	�dB	o�B	@�B	�B	+B�B�B�`B�;B	B�B�B	%�B	.B	Q�B	m�B	�9B	�jB	��B	�
B	�BB	��B
B
%B
	7B
1B
1B
JB
DB
1B
B
  B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
1B
	7B
PB
bB
{B
�B
�B
�B
�B
�B
 �B
'�B
-B
5?B
:^B
@�B
D�B
L�B
Q�B
XB
]/B
aHB
e`B
hsB
l�B
q�B
v�B
y�B
|�B
� B
�%B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.3(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200903281339312009032813393120090328133931200903281337062009032813370620090328133706200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20090315065819  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090315065825  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090315065827  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090315065828  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090315065830  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090315065830  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090315065830  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090315065830  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090315065831  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090315070550                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090319035655  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090319035756  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090319035757  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090319035757  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090319035758  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090319035758  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090319035758  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090319035758  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090319035758  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090319040252                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090318232223  CV  DAT$            G�O�G�O�F��M                JM  ARCAJMQC1.0                                                                 20090328133931  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090328133931  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090328133706  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091711  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091747                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112703                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170515                      G�O�G�O�G�O�                