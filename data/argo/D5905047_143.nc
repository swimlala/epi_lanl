CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-04T06:52:35Z creation;2020-04-07T21:53:32Z conversion to V3.1;2022-07-26T02:44:12Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200404065235  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_143                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�;�# 1   @�H�x9�@45�$�/�ch���S�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�  A)��AnffA�ffA�  A���B��B"ffB4��BG��B`  Bs33B���B���B���B���B�  B���B���B�  B�33Bޙ�B���B�ffB�ffC��C�fC  C��CL�C�fCffC%ffC)�fC/�3C3�fC9��C>�CB��CGffCQ�C[� Cg  Cp�Cz��C�s3C�L�C��3C��fC�&fC�L�C��fC�Y�C��C�ٚC��fC��C�Y�C�L�C��3C�33C�ffC��3C��C�ffC�33C��CC�@ C�� D  D�DfD@ D@ D�fD�fD%  D*�D/&fD4  D9&fD>9�DCfDG�fDL��DR,�DV�3D\&fDa3Df,�Dk  Dp  DufDz3D�I�D�vfD��3D�  D�<�D��fD��3D��D�FfD��3D��fD���D�<�Dԙ�D�� D���D�I�D�fD�ٚD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>���@�ffA,��Aq��A�  A͙�A�ffB��B#33B5��BHffB`��Bt  B�  B�33B�33B�  B�ffB�  B�  B�ffBՙ�B�  B�33B���B���C��C�C33C  C� C�C��C%��C*�C/�fC4�C9��C>L�CC  CG��CQL�C[�3Cg33CpL�Cz��C���C�ffC���C�  C�@ C�ffC�  C�s3C�&fC��3C�� C�33C�s3C�ffC��C�L�CҀ C��C�&fC� C�L�C�33C�3C�Y�C���D�D&fD3DL�DL�D�3D�3D%,�D*&fD/33D4�D933D>FfDC3DG�3DMfDR9�DW  D\33Da  Df9�Dk�Dp,�Du3Dz  D�P D�|�D�ٚD�&fD�C3D���D�ɚD�3D�L�D���D���D�  D�C3DԠ D��fD�3D�P D��D�� D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�C�A��A�  A���A��PA�n�A��A���A��hA��+A��TA� �A��9A���A�~�A�?}A�"�A�?}A���A��HA�  A�-A���A�$�A�~�A� �A�t�A��jA�{A��7A�ƨA�Q�A��#A��A���A��FA���A���A�AzQ�As��Ad9XAVA�AO�mAG��ABȴA<�A9�wA6~�A1��A,bA&1'A�9A�^A;dA�jA
�9Av�A�A+A1'@�+@�Ĝ@��@�ȴ@��D@�  @��y@�  @�ff@�X@�v�@��@�z�@��R@���@�b@�r�@���@���@���@���@�x�@�~�@��;@�z�@���@��H@�1@�5?@�  @��@���@�-@��7@w�w@j��@d�@Yhs@T(�@Qx�@J��@E�@?;d@3��@-/@)��@#@ȴ@�P@Z@X@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�C�A��A�  A���A��PA�n�A��A���A��hA��+A��TA� �A��9A���A�~�A�?}A�"�A�?}A���A��HA�  A�-A���A�$�A�~�A� �A�t�A��jA�{A��7A�ƨA�Q�A��#A��A���A��FA���A���A�AzQ�As��Ad9XAVA�AO�mAG��ABȴA<�A9�wA6~�A1��A,bA&1'A�9A�^A;dA�jA
�9Av�A�A+A1'@�+@�Ĝ@��@�ȴ@��D@�  @��y@�  @�ff@�X@�v�@��@�z�@��R@���@�b@�r�@���@���@���@���@�x�@�~�@��;@�z�@���@��H@�1@�5?@�  @��@���@�-@��7@w�w@j��@d�@Yhs@T(�@Qx�@J��@E�@?;d@3��@-/@)��@#@ȴ@�P@Z@X@�@	hs31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	;dB	M�B	M�B	L�B	L�B	J�B	E�B	E�B	�^B	�;B	��B
�B
I�B
�VB
��B
�'B
�wB
��B(�B33B@�BdZB��B��B��B�BuBoBVB��B�NBB��B?}B%�B
=B
��B
��B
]/B
P�B
C�B	��B	�B	S�B	�B	�B�B�)B��BŢB�wB�3B��B��B�hB�PBw�BgmBt�Bx�B��B	B�/B��B��B�B	33B	B	�B	!�B��B�sB�B	JB	ffB	��B	��B	�?B	��B	ɺB	�#B	�yB	�TB	�B	�`B	�ZB	�fB	�B	�B	��B
B
	7B

=B
hB
�B
�B
"�B
(�B
)�B
6FB
@�B
I�B
R�B
S�B
[#B
aHB
iyB
k�B
n�B
t�B
x�B
~�B
�B
�B
�7B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	(
B	:^B	9�B	8�B	8�B	6�B	1�B	2|B	��B	�B	��B
B
5%B
zB
�YB
��B
�B
�|BBVB,�BQ�B��B�^B��BEB B��B�B�B�bB�AB�	B,�B[B
�rB
�UB
�\B
JrB
=qB
2GB	�TB	ޞB	C�B	KB	�B�B�	B��B��B��B��B�bB��B~wBz�Be�BUMBbBe`B�iB�BʦB��B�BB��B	 �B�[B	^B	�B��B�MB�WB�B	R�B	�rB	�aB	�bB	��B	�+B	�EB	��B	�\B	ּB	ѝB	�bB	ңB	ּB	ݲB	��B	�!B	�ZB	�`B	�qB
�B
�B
�B
B
B
"hB
,�B
5�B
>�B
@ B
G+B
MjB
U�B
W�B
Z�B
`�B
d�B
kB
nB
qAB
u?B
x833111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.1(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.020(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202004180015312020041800153120200418001531202207232057292022072320572920220723205729202207261128042022072611280420220726112804  JA  ARFMdecpV4_b                                                                20200404065234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200404065235  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200404065235  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200404065237  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200404065237  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200404065238  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200404065345                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200407215227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200407215330  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200407215330  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200407215331  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200407215331  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200407215331  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200407215332                      G�O�G�O�G�O�                JA  ARUP                                                                        20200407215416                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200408000000  CF  PSAL_ADJUSTED_QC>L��@�  G�O�                JM  ARSQJMQC2.0                                                                 20200408000000  CF  TEMP_ADJUSTED_QC>L��>L��G�O�                JM  ARCAJMQC2.0                                                                 20200417151531  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200417151531  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115729  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022804  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                