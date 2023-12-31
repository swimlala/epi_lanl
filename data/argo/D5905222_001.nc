CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-06-08T06:53:08Z creation;2018-06-11T21:53:06Z conversion to V3.1;2019-09-10T08:08:33Z update;2022-11-10T04:23:19Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Et   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  GD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  KX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    iD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  iH   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20180608065308  20221117224508  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131533_001                   2C  Dd�ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�h�d��1   @�hėS @-V�+J�d�E���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�ffA��Ay��A�  A�33A�ffB��B$  B6  BI33B\��Bo��B�ffB�ffB���B�  B�  B�33B�33Bș�Bљ�B�ffB�33B�ffB�ffCffC� C��CL�C33C�3C   C%ffC)��C/� C3�fC:  C>��CCL�CH� CQ�fC[��Cf  CpffCy�fC�Y�C��3C�ffC���C�33C���C�33C��3C��C���C���C��C��3C�  C�L�C���C�&fC��C�L�C�ffC��C�ٚC��fC�  C�&fD,�D  DٚD&fDٚDfD��D%  D*33D/  D4�D8�fD>&fDB�3DHfDLٚDQ��DW�D\  D`�3Df33Dk�Dp  Du,�Dy�3D�L�D�� D�� D�	�D�C3D�� D��3D�3D�,�D�ffD��fD��3D�<�Dԙ�Dڹ�D�	�D�@ D� D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�ffA��Ay��A�  A�33A�ffB��B$  B6  BI33B\��Bo��B�ffB�ffB���B�  B�  B�33B�33Bș�Bљ�B�ffB�33B�ffB�ffCffC� C��CL�C33C�3C   C%ffC)��C/� C3�fC:  C>��CCL�CH� CQ�fC[��Cf  CpffCy�fC�Y�C��3C�ffC���C�33C���C�33C��3C��C���C���C��C��3C�  C�L�C���C�&fC��C�L�C�ffC��C�ٚC��fC�  C�&fD,�D  DٚD&fDٚDfD��D%  D*33D/  D4�D8�fD>&fDB�3DHfDLٚDQ��DW�D\  D`�3Df33Dk�Dp  Du,�Dy�3D�L�D�� D�� D�	�D�C3D�� D��3D�3D�,�D�ffD��fD��3D�<�Dԙ�Dڹ�D�	�D�@ D� D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A٬AًDAٛ�A�v�A�dZA�bNA�\)A�bNA�bNA�bNA�l�A�bNA�7LA� �A���A�|�AǸRA�K�A�~�A�dZA��jA�?}A�A�v�A�dZA���A�1A�;dA�oA��mA��A���A�v�A�I�A���Au��Anv�Aa�hAY�
AQ�hAN��A>�9A2$�A3;dA-XA(  A#/A$��A$~�A"bNA�#A�A�A�+A��A�RA"�A�RA�\A�AA
�jA	x�A/A
=AȴA ��@�?}@� �@�{@�33@�E�@�ƨ@�/@�Z@��@��@�V@�C�@���@��-@�ff@��@���@�@���@� �@���@�
=@��w@��@��!@�+@��F@��T@�|�@�&�@��@}��@sƨ@k�
@a�#@YG�@P  @I&�@Co@<��@5�@/�@)��@#��@��@�P@@�@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A٬AًDAٛ�A�v�A�dZA�bNA�\)A�bNA�bNA�bNA�l�A�bNA�7LA� �A���A�|�AǸRA�K�A�~�A�dZA��jA�?}A�A�v�A�dZA���A�1A�;dA�oA��mA��A���A�v�A�I�A���Au��Anv�Aa�hAY�
AQ�hAN��A>�9A2$�A3;dA-XA(  A#/A$��A$~�A"bNA�#A�A�A�+A��A�RA"�A�RA�\A�AA
�jA	x�A/A
=AȴA ��@�?}@� �@�{@�33@�E�@�ƨ@�/@�Z@��@��@�V@�C�@���@��-@�ff@��@���@�@���@� �@���@�
=@��w@��@��!@�+@��F@��T@�|�@�&�@��@}��@sƨ@k�
@a�#@YG�@P  @I&�@Co@<��@5�@/�@)��@#��@��@�P@@�@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	B	
=B	
=B	DB	PB	PB	VB	\B	\B	bB	{B	�B	J�B	v�B
DB
VB
��BJB>wB{�B��B��B�B�;B�B�wB�B��B�jB�+BQ�B
ȴB
m�B
�B	�wB	hsB	E�B	�B�B��B	�B�B��B	� B	��B	ĜB	�jB
\B
uB
 �B
"�B
(�B
(�B
.B
49B
9XB
:^B
1'B
.B
.B
0!B
0!B
,B
+B
&�B
�B
�B
oB
PB
PB
	7B
1B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
DB
DB
\B
bB
uB
{B
�B
�B
�B
$�B
-B
33B
9XB
>wB
F�B
K�B
P�B
T�B
YB
\)B
bNB
e`B
iyB
m�B
r�B
w�B
{�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	SB	
=B	
rB	^B	jB	jB	pB	vB	vB	}B	�B	�B	NB	{�B
6B
Y�B
ϫB�BA�B}VBϫB��B��B�B�B�OB�?B�sB��B��BW�B
οB
r|B
~B	�YB	l�B	J�B	�B�BңB	�B��B��B	�;B	�NB	żB	�PB
BB
,B
!bB
#TB
)yB
)�B
.�B
4�B
9�B
:�B
1�B
.}B
.cB
0oB
0oB
,�B
+�B
'�B
OB
KB
&B
�B
�B
	�B
�B
�B	�]B	�VB	�^B	�B	�PB	�JB	�B	�6B	�0B	�VB	�$B	�0B	�HB
MB
xB
�B
�B
}B
�B
�B
�B
�B
�B
%B
-]B
3hB
9XB
>�B
F�B
K�B
Q B
UB
YB
\CB
bhB
ezB
i�B
m�B
r�B
w�B
|B
.B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.1(dbar); PO2=-0.1(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806220015592018062200155920180622001559202210251311032022102513110320221025131103202210251803142022102518031420221025180314  JA  ARFMdecpV4_b                                                                20180608065307  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180608065308  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180608065308  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180608065309  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180608065309  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180608065309  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20180608065951                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20180611215140  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180611215305  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180611215305  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180611215306  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180611215306  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180611215306  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180611215306                      G�O�G�O�G�O�                JA  ARUP                                                                        20180611215530                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180612000000  CF  PSAL_ADJUSTED_QC?��?��G�O�                JM  ARCAJMQC2.0                                                                 20180621151559  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180621151559  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180622151339  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920071515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041103  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117224508                      G�O�G�O�G�O�                