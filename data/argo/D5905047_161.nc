CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-10-01T06:52:02Z creation;2020-10-04T21:53:15Z conversion to V3.1;2022-07-26T02:43:29Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20201001065202  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_161                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�<;��[ 1   @�<H�t�@3��n���cN��`A�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�ffA#33Ah  A���A�ffA�33B
  B!��B8��BN  B`ffBo33B���B�33B�ffB���B���B���B���B�ffBҙ�B�ffB�33B�33B���C�3C�fC��CL�C33C33C�fC$ffC)��C.�fC3�3C:33C>��CCffCHffCQ�fC[�3Ce� Cp��CzffC���C�33C�  C�  C�ٚC��C�&fC���C�ٚC�&fC��C�&fC�&fC�33C�&fC�  C�L�C��fC�33C��3C�ٚC�Y�C�3C��3C�@ DٚD�D��D&fD�3D�D �D%  D)�3D/&fD4�D9  D=� DC3DH9�DL�3DQ�3DW  D\,�DafDf  Dj�3Do��Dt��DzfD�9�D�y�D���D� D�C3D�s3D��3D�fD�S3D�� D���D�fD�@ DԌ�D�ٚD���D�I�D�fD�ɚD�ٚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A$��Ai��A���A�33A�  B
ffB"  B933BNffB`��Bo��B�  B�ffB���B���B���B���B���Bə�B���Bܙ�B�ffB�ffB���C��C  C�3CffCL�CL�C  C$� C)�fC/  C3��C:L�C>�3CC� CH� CR  C[��Ce��Cp�3Cz� C���C�@ C��C��C��fC�&fC�33C�ٚC��fC�33C�&fC�33C�33C�@ C�33C��C�Y�C��3C�@ C�  C��fC�ffC�� C�  C�L�D� D  D�3D,�DٚD  D 3D%fD)��D/,�D4  D9&fD=�fDC�DH@ DL��DQ��DW&fD\33Da�Df&fDj��Do�3Du  Dz�D�<�D�|�D�� D�3D�FfD�vfD��fD��D�VfD��3D�� D��D�C3DԐ D���D�  D�L�D홚D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA���AֶFA֥�A�dZA��A���A��;A�ĜAՋDA���A҅A�  AΉ7A�S�A��Aɰ!A�%A���A���A�
=A��FA�1A�\)A� �A���A�&�A��`A�?}A���A�/A��`A�/A��9A���A�n�A���A�n�A�ZA���A��RA��TA���A�p�Au�AeC�AM�^AB��A=�A8ZA0(�A&��A"��AK�A��A\)A~�A�HAdZA
�9Av�A�RAn�A  A�7@�hs@��@�o@�ƨ@�x�@��@�ȴ@��@�O�@��/@ΰ!@�Z@ċD@�ff@���@��j@�{@��-@�J@�J@���@��T@�"�@���@���@�"�@��H@��@��@��F@�M�@�-@�ȴ@��/@u�@n@b�\@Y�@R�\@I7L@A�@8�`@.V@(�u@%�@��@�#@��@��@ȴ@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA���AֶFA֥�A�dZA��A���A��;A�ĜAՋDA���A҅A�  AΉ7A�S�A��Aɰ!A�%A���A���A�
=A��FA�1A�\)A� �A���A�&�A��`A�?}A���A�/A��`A�/A��9A���A�n�A���A�n�A�ZA���A��RA��TA���A�p�Au�AeC�AM�^AB��A=�A8ZA0(�A&��A"��AK�A��A\)A~�A�HAdZA
�9Av�A�RAn�A  A�7@�hs@��@�o@�ƨ@�x�@��@�ȴ@��@�O�@��/@ΰ!@�Z@ċD@�ff@���@��j@�{@��-@�J@�J@���@��T@�"�@���@���@�"�@��H@��@��@��F@�M�@�-@�ȴ@��/@u�@n@b�\@Y�@R�\@I7L@A�@8�`@.V@(�u@%�@��@�#@��@��@ȴ@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
(�B
G�B
K�B
O�B
J�B
F�B
F�B
E�B
G�B
I�B
`BB
��BB1BB
�TB
�HB
�ZB%�B%�BM�BQ�B�B��B�B�B��B�B	7BVB�B�B�BB�mB��B�qB��BiyB)�B
�B
��B
�uB
S�B
  B	�B��B�;B��B�XB�9B�B�'B�?B�LB�LB�dB�RB�jB�wB��BÖB��B�B��B	B	JB	PB	PB	.B	@�B	\)B	w�B	�uB	��B	��B	�B	��B	��B	��B	�LB	��B	��B	��B	�/B	�fB	�B	�B	��B	��B	��B
%B
1B

=B
PB
\B
oB
�B
"�B
/B
49B
;dB
B�B
L�B
T�B
[#B
bNB
ffB
m�B
n�B
s�B
v�B
z�B
� B
�+B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
2B
2�B
6�B
;0B
6+B
1�B
1�B
0�B
2�B
5�B
M�B
�oB
� B
�B
��B
�HB
�TB
��B�BFB:*B>�Bq�B��B�B�B�B߾B��B��B�B�B
=B�BԕB��B��B�BXEB�B
�OB
��B
��B
A�B	�OB	tB�B�0B��B�8B�4B��B�5B��B��B��B�mB��B��B��B��B��B�.B�7B�2B�OB��B�XB�$B	�B	,WB	H1B	c�B	.B	��B	��B	��B	��B	�\B	��B	�:B	�)B	�fB	��B	��B	�:B	�7B	�;B	�HB	�mB	�_B	�B	�B	��B	��B	��B	��B
EB
VB
�B
�B
&�B
.B
8RB
@�B
F�B
M�B
Q�B
YB
ZB
_!B
bNB
fLB
k�B
r�B
w�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0001), deepest deltaS=-0.020(+-0.004)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202010150015562020101500155620201015001556202207232057452022072320574520220723205745202207261129122022072611291220220726112912  JA  ARFMdecpV4_b                                                                20201001065201  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201001065202  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201001065202  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201001065203  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201001065203  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201001065204  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20201001065308                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20201004215230  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201004215313  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201004215313  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201004215314  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201004215314  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201004215315  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201004215315                      G�O�G�O�G�O�                JA  ARUP                                                                        20201004215403                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20201005000000  CF  PSAL_ADJUSTED_QC?�  @�ffG�O�                JM  ARCAJMQC2.0                                                                 20201014151556  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201014151556  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115745  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022912  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                