CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-05-19T07:00:24Z creation;2022-05-22T21:56:13Z conversion to V3.1;2022-11-10T04:17:03Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20220519070024  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_145                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @������1   @��	^З�@0�E����d����S�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A!��Ai��A���A���A�33BffB��B6  BL  B^��Bs33B�33B�ffB�33B���B�ffB�  B���BǙ�B���B�33B�  B�33B���C� C33C33C33C�C��C 33C%ffC*��C.�fC4� C8�3C>��CC33CG�fCQ33C[�Ce��Cp��CzL�C�s3C�@ C��C���C�� C���C�&fC��C�� C��3C�@ C��fC�&fC�L�C�ffC�s3C�Y�C�33C�&fC��C� C�s3C�L�C�Y�C���D,�D�D�fD��D33D&fD �D%,�D)��D.��D43D8�3D=�fDB��DGٚDM33DR3DW,�D\fDa,�Df�Dk  Dp,�Du33Dz33D�P D���D���D�3D�I�D���D��fD�fD�L�D�� D���D�3D�C3DԐ D�ٚD� D�FfD� D��fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�ff@�  A#33Ak33A�ffA͙�A�  B��B   B6ffBLffB_33Bs��B�ffB���B�ffB���B���B�33B���B���B�  B�ffB�33B�ffB���C��CL�CL�CL�C33C�fC L�C%� C*�fC/  C4��C8��C>�3CCL�CH  CQL�C[33Ce�fCp�3CzffC�� C�L�C�&fC�ٚC���C�ٚC�33C��C���C�  C�L�C��3C�33C�Y�C�s3C̀ C�ffC�@ C�33C��C��C� C�Y�C�ffC�ٚD33D3D��D�3D9�D,�D   D%33D*  D/  D4�D8��D=��DB�3DG� DM9�DR�DW33D\�Da33Df  DkfDp33Du9�Dz9�D�S3D�� D�� D�fD�L�D���D�ٚD��D�P D��3D�� D�fD�FfDԓ3D���D�3D�I�D�3D�ɚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA�7LA��A���AԾwA�?}A���A��HA���AӇ+A�`BA�I�A�S�A�ĜA�p�A�?}A�JA�\)A�+A��A��A�"�AÛ�A�n�A�x�A��A���A�1'A���A�Q�A�`BA�A�K�A��mA�E�A�VA�ȴA�%A��A�I�A�S�A�\)A�v�A��9A|�Ah�jAT �AE�A>=qA1�7A*�yA&��A ffA�wA �A+AS�A�A�`A33A	�AXA��AS�A��@��;@�$�@�V@��`@���@�@띲@�P@�1@��/@�t�@�O�@��`@�/@�=q@�~�@��m@��@�5?@���@�9X@�7L@���@�(�@�Ĝ@�=q@�A�@�;d@�/@���@��#@�/@�  @~��@u��@h��@_�P@R��@Fȴ@;33@3ƨ@+C�@&$�@#��@�@-@��@r�@t�@	7L@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA�7LA��A���AԾwA�?}A���A��HA���AӇ+A�`BA�I�A�S�A�ĜA�p�A�?}A�JA�\)A�+A��A��A�"�AÛ�A�n�A�x�A��A���A�1'A���A�Q�A�`BA�A�K�A��mA�E�A�VA�ȴA�%A��A�I�A�S�A�\)A�v�A��9A|�Ah�jAT �AE�A>=qA1�7A*�yA&��A ffA�wA �A+AS�A�A�`A33A	�AXA��AS�A��@��;@�$�@�V@��`@���@�@띲@�P@�1@��/@�t�@�O�@��`@�/@�=q@�~�@��m@��@�5?@���@�9X@�7L@���@�(�@�Ĝ@�=q@�A�@�;d@�/@���@��#@�/@�  @~��@u��@h��@_�P@R��@Fȴ@;33@3ƨ@+C�@&$�@#��@�@-@��@r�@t�@	7L@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BƨBǮBǮBȴBɺB��B��B��B��B�yB��B�B5?BI�BK�BhsBhsB_;B33B33BA�B?}B>wBB�BD�BA�B(�B/B?}B=qB+B�BbB��B�fB��B�'B��B�JBn�B �B
��B
jB
6FB	ɺB	[#B	�B��B�B�sB�fB�mB�fB�B	$�B	D�B	u�B	|�B	�hB	�uB	�{B	��B	��B	�9B	�FB	�FB	�?B	�LB	�FB	�-B	�?B	ÖB	��B	��B	�wB	��B	�
B	�BB	�sB	�B	��B	��B	��B
B
%B
1B
JB
hB
{B
�B
�B
�B
"�B
&�B
'�B
.B
;dB
A�B
E�B
M�B
S�B
[#B
bNB
iyB
o�B
v�B
y�B
{�B
�B
�B
�=B
�\B
�hB
�{B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B�B��B��B��B�B��B�[B�B9B�B1�B4BP�BQhBH�BIBB*0B)*B'�B,WB.IB,�B2B7B)_B(�BgBB�B�B�NB��B�kB�=Bv�BY�B�B
��B
TFB
#�B	�2B	G�B	
�B�6B�?B�:B��B� B� BөB	�B	-CB	^�B	f�B	z�B	|�B	}�B	�B	�B	�dB	�VB	��B	�B	�\B	�pB	��B	�OB	��B	��B	��B	��B	�B	�iB	�B	�hB	׍B	޸B	��B	��B	��B	�B	�B	�?B	�xB	�VB
oB
mB
�B
�B
�B
�B
�B
$&B
*KB
.}B
6�B
<�B
C�B
K)B
R:B
XEB
_�B
b�B
d�B
i�B
l�B
r�B
xB
zB
}"B
~(31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.3(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.023(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202206020015082022060200150820220602001508202210251313242022102513132420221025131324202210251812342022102518123420221025181234  JA  ARFMdecpV4_b                                                                20220519070023  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220519070024  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220519070024  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220519070025  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220519070025  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220519070025  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20220519070057                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20220519155543  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220522215611  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220522215611  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220522215612  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220522215612  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220522215613  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220522215613                      G�O�G�O�G�O�                JA  ARUP                                                                        20220522215632                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220522151506  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220522151506  QCF$                G�O�G�O�G�O�200000          JM  ARGQJMQC2.0                                                                 20220522151506  CV  JULD_LOCATION   G�O�G�O�FΈJ                JM  ARCAJMQC2.0                                                                 20220601151508  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20220601151508  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041324  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091234  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                