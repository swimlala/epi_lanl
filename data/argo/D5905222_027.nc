CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-02-23T06:51:51Z creation;2019-02-26T21:53:19Z conversion to V3.1;2019-09-10T08:04:55Z update;2022-11-10T04:22:12Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190223065151  20221117231505  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131533_027                   2C  Dd(ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @ة��� 1   @ةİ*z @.������d(�\1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�ff@�  A&ffAnffA�33A���A�B��B#33B4  BJ  BZ  Br  B���B�33B�  B���B�ffB�33B�33B�ffBҙ�B�  B���B���B���C�C��C33C��CL�C� C L�C%�C+33C/33C3�fC933C>33CC�fCH�CR� C\��CfffCo�fCz�C�@ C�33C�L�C�ffC��fC�&fC��C�Y�C��fC���C�&fC��3C���C�C�L�C�  C��3C�&fC�  C��3C��3C���C�Y�C�&fC��fD9�D� D� D� D�3D,�D fD%fD*�D/�D3�fD9  D=�3DC,�DH3DM  DR�DW  D\@ Da  De� Dk33Dp,�Du&fDz�D�Y�D��3D�ɚD��fD�C3D���D��3D��D�<�D�i�D�ɚD���D�VfD�y�D��3D��D�&fD�vfD��fD�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A$��Al��A�ffA�  A���BffB"��B3��BI��BY��Bq��B���B�  B���B���B�33B�  B�  B�33B�ffB���B噚BB�ffC  C�3C�C�3C33CffC 33C%  C+�C/�C3��C9�C>�CC��CH  CRffC\� CfL�Co��Cz  C�33C�&fC�@ C�Y�C�ٚC��C��C�L�C�ٚC�� C��C��fC�� C�C�@ C��3C��fC��C��3C��fC��fC�� C�L�C��C�ٚD33DٚDٚDٚD��D&fD   D%  D*3D/fD3� D8��D=��DC&fDH�DL��DR3DW�D\9�Da�DeٚDk,�Dp&fDu  DzfD�VfD�� D��fD��3D�@ D���D�� D�fD�9�D�ffD��fD��fD�S3D�vfD�� D�fD�#3D�s3D��3D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A̸RA̩�A̧�A̩�A̛�A̗�ȂhA�`BA��HA�ĜAˣ�AˑhA��AɁA���Aɕ�A�r�A��mA��PA�{A�E�A�G�A��A��A���A��A�5?A��
A�?}A�-A��-A�+A�^5A���A�A���A~~�A}+AuAb��A^ȴAQ��AGl�A;��A3�-A.�A%��A&�!A%A�!AE�AVAS�A	AƨA"�A�#A�A%@�&�@��@�-@�V@�C�@���@�hs@�Z@�!@�X@�J@�A�@�G�@� �@�M�@�J@�@�J@�x�@��#@�~�@��m@�z�@�ƨ@��@�@�S�@���@���@��@�Q�@�5?@���@�@�  @��@���@�X@��@w��@nȴ@h  @`bN@T9X@LZ@D9X@=��@3�
@-�h@&E�@!hs@"�@��@S�@K�@��@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A̸RA̩�A̧�A̩�A̛�A̗�ȂhA�`BA��HA�ĜAˣ�AˑhA��AɁA���Aɕ�A�r�A��mA��PA�{A�E�A�G�A��A��A���A��A�5?A��
A�?}A�-A��-A�+A�^5A���A�A���A~~�A}+AuAb��A^ȴAQ��AGl�A;��A3�-A.�A%��A&�!A%A�!AE�AVAS�A	AƨA"�A�#A�A%@�&�@��@�-@�V@�C�@���@�hs@�Z@�!@�X@�J@�A�@�G�@� �@�M�@�J@�@�J@�x�@��#@�~�@��m@�z�@�ƨ@��@�@�S�@���@���@��@�Q�@�5?@���@�@�  @��@���@�X@��@w��@nȴ@h  @`bN@T9X@LZ@D9X@=��@3�
@-�h@&E�@!hs@"�@��@S�@K�@��@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B�B��BhBVB&�B0!B	B	,B	K�B
.B
�3B�oB�JBƨB�BB�B�PB�1Bz�B��B�;B�B�B�\B�B
��B
�wB
`BB	�)B	��B	�B	�-B	F�B	.B	1B	$�B	)�B	(�B	$�B	�B	k�B	�B	��B	L�B	I�B	1'B	2-B	1'B	T�B	bNB	gmB	�bB	t�B	�B	�B	�7B	�uB	��B	�'B	�qB	ɺB	��B	��B	��B	ƨB	ŢB	�B	�HB	�mB	�yB	�mB	�B	�B	��B	�B	��B
B	��B	��B
B
1B
+B
hB
�B
�B
�B
�B
�B
�B
#�B
/B
5?B
:^B
C�B
J�B
O�B
R�B
YB
]/B
bNB
ffB
l�B
q�B
v�B
z�B
|�B
�B
�B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B�WB�hB�B�B'B1�B	AB	+�B	LdB
.�B
�DB��B�BB�0B��BAB]B�4B��B|�B�FB�BB��B�!B��B!bB
��B
�AB
g�B	�vB	��B	�[B	�^B	I�B	1vB	B	($B	,=B	*eB	'8B	�B	lB	��B	�B	NpB	K^B	1�B	3hB	1�B	UgB	b�B	gRB	��B	uZB	��B	��B	��B	��B	��B	�vB	��B	��B	�oB	��B	�pB	�B	��B	�_B	�B	��B	��B	��B	��B	��B	�+B	��B	�HB
uB	�.B	�BB
AB
�B
_B
�B
�B
�B
�B
B
B
 B
$B
/OB
5tB
:�B
C�B
J�B
PB
SB
YKB
]dB
b�B
f�B
l�B
q�B
v�B
{B
}"B
�;B
�SB
�E31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.1(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903090015352019030900153520190309001535202210251311322022102513113220221025131132202210251804572022102518045720221025180457  JA  ARFMdecpV4_b                                                                20190223065150  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190223065151  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190223065152  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190223065152  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190223065152  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190223065153  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190223065911                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190226215159  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190226215317  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190226215317  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190226215318  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190226215318  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190226215318  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190226215319                      G�O�G�O�G�O�                JA  ARUP                                                                        20190226215554                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190227000000  CF  PSAL_ADJUSTED_QC?�ff?�ffG�O�                JM  ARCAJMQC2.0                                                                 20190308151535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190308151535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190309151421  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920071516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041132  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231505                      G�O�G�O�G�O�                