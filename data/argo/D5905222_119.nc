CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-09-01T06:51:52Z creation;2021-09-04T21:52:04Z conversion to V3.1;2022-11-10T04:18:11Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  EX   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Il   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    il   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20210901065152  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  V4_131533_119                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @ُ�#� 1   @ِ
��X�@0�l�C���c����F1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A��A{33A�  A�33A�ffB33B ��B/��BDffBX��Bn  B�ffB�  B�ffB�33B���B�ffB���B�ffB�  B���B���B�ffB�  C�C�fC��C��C  C�C �3C%33C*ffC0  C4�3C8�3C>� CB�fCG� CR� C\� Cf�3Co��Cz��C�  C�@ C�� C�@ C�ffC�@ C�33C�33C�ٚC�ffC�� C��3C�Y�C�  C��C�  C�s3Cֳ3C��fC��C�&fC�&fC�&fC�33C�Y�D��D�3D�3D  DٚD� D� D%3D)ٚD/3D3�3D9�D=�3DB��DH�DM�DR  DWfD\  Da  De�fDj� Do�3DufDy��D�S3D��fD�ٚD��D�9�D���D��3D���D�S3D�s3D��fD�fD�@ D�s3D��fD�  D�` D�y�D�fD� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A��A{33A�  A�33A�ffB33B ��B/��BDffBX��Bn  B�ffB�  B�ffB�33B���B�ffB���B�ffB�  B���B���B�ffB�  C�C�fC��C��C  C�C �3C%33C*ffC0  C4�3C8�3C>� CB�fCG� CR� C\� Cf�3Co��Cz��C�  C�@ C�� C�@ C�ffC�@ C�33C�33C�ٚC�ffC�� C��3C�Y�C�  C��C�  C�s3Cֳ3C��fC��C�&fC�&fC�&fC�33C�Y�D��D�3D�3D  DٚD� D� D%3D)ٚD/3D3�3D9�D=�3DB��DH�DM�DR  DWfD\  Da  De�fDj� Do�3DufDy��D�S3D��fD�ٚD��D�9�D���D��3D���D�S3D�s3D��fD�fD�@ D�s3D��fD�  D�` D�y�D�fD� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��
A�XA���A���A�ĜAٺ^A٬Aٛ�A�G�A�7LA��A�ĜA���A�ffA��;A�hsA�;dA��`A�n�A���A�bAɇ+A��A�r�A���A���A�M�A�"�A���A��wA��+A��A�A��HA��A�ffA�?}A��A���A���A�I�A���A�ffA��A�x�Apn�AW��AJȴAE�mA@ �A9�A4�A/oA)"�A Q�AVA"�A
=A=qA�hAffA��An�AĜA�mA �9@��@���@�J@�S�@�?}@�o@��m@�+@�Ĝ@��@�M�@�O�@��@��@��-@���@�v�@�t�@���@���@���@��@�{@��!@�%@��\@�j@��@��@�E�@���@���@y%@p�`@i�@^5?@XĜ@M�T@Fv�@?
=@65?@/�@)��@#��@V@�7@Z@��@�@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��
A�XA���A���A�ĜAٺ^A٬Aٛ�A�G�A�7LA��A�ĜA���A�ffA��;A�hsA�;dA��`A�n�A���A�bAɇ+A��A�r�A���A���A�M�A�"�A���A��wA��+A��A�A��HA��A�ffA�?}A��A���A���A�I�A���A�ffA��A�x�Apn�AW��AJȴAE�mA@ �A9�A4�A/oA)"�A Q�AVA"�A
=A=qA�hAffA��An�AĜA�mA �9@��@���@�J@�S�@�?}@�o@��m@�+@�Ĝ@��@�M�@�O�@��@��@��-@���@�v�@�t�@���@���@���@��@�{@��!@�%@��\@�j@��@��@�E�@���@���@y%@p�`@i�@^5?@XĜ@M�T@Fv�@?
=@65?@/�@)��@#��@V@�7@Z@��@�@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�XB
�XB
�RB
�RB
�XB
�^B
�qB
ɺB
��B
��B
��B
��B
�jB
ȴB
��B
��B
�B
Q�B
r�B
�VB
��B
��BN�B�B��B�fB�B��B��B��B�NB�`B�5B��B��B�BdZBK�BA�B�B
��B
��B
x�B
J�B	�}B	L�B	&�B	�B	�B	B��B	(�B	?}B	=qB	`BB	y�B	q�B	y�B	�7B	�3B	��B	��B	�VB	��B	��B	��B	��B	��B	��B	��B	�!B	�uB	��B	��B	��B	�!B	ĜB	ŢB	ɺB	��B	�fB	�B	�B	��B
B
1B
VB
uB
�B
�B
�B
!�B
!�B
 �B
!�B
,B
33B
:^B
E�B
M�B
R�B
W
B
YB
]/B
bNB
jB
o�B
t�B
y�B
~�B
�B
�=B
�\B
�uB
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
�tB
�nB
�TB
�tB
�zB
��B
��B
��B
��B
�B
��B
��B
�B
�"B
�B
��B
@ B
_;B
z�B
��B
�xB;0Bo�B��B�FBňB�oB�<B�}B��B�{B�B�5B�+Br�BQhB7�B/B	�B
�QB
�[B
e�B
9�B	��B	;B	�B	�B	�B� B��B	B	-�B	)�B	L�B	fLB	]�B	fB	t�B	�BB	�bB	�-B	zB	��B	�oB	��B	��B	��B	��B	��B	�]B	cB	��B	�B	��B	�B	�UB	�vB	��B	��B	�B	�9B	�VB	�B	��B	�B	�B	�.B
B
'B
?B
jB
jB
JB
PB
�B
�B
%�B
1'B
9rB
>]B
B�B
D�B
H�B
M�B
VB
[#B
`BB
ezB
jB
o�B
u�B
z�B
~�B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.020(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202109150015562021091500155620210915001556202210251313002022102513130020221025131300202210251810502022102518105020221025181050  JA  ARFMdecpV4_b                                                                20210901065151  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210901065152  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210901065152  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210901065153  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210901065153  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210901065153  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20210901065232                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20210904215144  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210904215203  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210904215203  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210904215204  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210904215204  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210904215204  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210904215204                      G�O�G�O�G�O�                JA  ARUP                                                                        20210904215223                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20210904151556  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20210904151556  QCF$                G�O�G�O�G�O�300000          JM  ARGQJMQC2.0                                                                 20210904151552  CV  JULD_LOCATION   G�O�G�O�F̀T                JM  ARGQJMQC2.0                                                                 20210904151552  CV  LATITUDE        G�O�G�O�A�A�                JM  ARGQJMQC2.0                                                                 20210904151552  CV  LONGITUDE       G�O�G�O��g+                JM  ARCAJMQC2.0                                                                 20210914151556  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210914151556  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041300  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091050  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                