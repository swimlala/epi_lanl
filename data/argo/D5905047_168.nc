CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-10T06:51:45Z creation;2020-12-13T21:51:33Z conversion to V3.1;2022-07-26T02:43:12Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20201210065145  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_168                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�M��c]�1   @�Mȴ���@39XbN�c,���S�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@ə�A)��A{33A�33A�ffA���B��B#33B333BI33BZ��Br  B���B�  B�33B�  B���B���B�33B�33B�ffB���B�ffB�33B���C  C  C� C�C�3C� C ffC%33C*  C0�C4��C9� C>�fCC�CG��CRffC[��Cf  Cn��Cy� C�ٚC��3C��C�s3C��C�33C��3C��fC��3C��C�33C�@ C�@ C��fC�  C�  C��C�L�C�s3C�s3C�ٚC�ffC��3C��3C�Y�D�3D9�D@ D33D,�D��D� D%&fD*  D/�D4  D8��D=�3DCfDHfDL�3DQ�3DV�3D\3D`��Df@ Dk  Do��Dt��Dy�3D�L�D��fD��3D��D�@ D�� D��fD�3D�P D�� D�ɚD��D�VfDԌ�D�� D�fD�FfD��D�� D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @�ffA(  Ay��A�ffAř�A�  B33B"��B2��BH��BZffBq��B���B���B�  B���B���B�ffB�  B�  B�33Bܙ�B�33B�  B���C�fC�fCffC  C��CffC L�C%�C)�fC0  C4�3C9ffC>��CC  CG�3CRL�C[� Ce�fCn�3CyffC���C��fC�  C�ffC��C�&fC��fC�ٚC��fC��C�&fC�33C�33C�ٚC��3C��3C�  C�@ C�ffC�ffC���C�Y�C��fC��fC�L�D��D33D9�D,�D&fD�fD��D%  D*�D/3D3��D8�fD=��DC  DH  DL��DQ��DV��D\�D`�3Df9�Dk�Do�fDt�fDy��D�I�D��3D�� D�	�D�<�D���D��3D�  D�L�D�|�D��fD�	�D�S3Dԉ�Dڼ�D�3D�C3D퉚D���D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aΰ!AΓuA�jA�bNA�`BA�^5A�\)A�`BA�bNA�dZA�hsA�hsA�jA�l�A�n�A�n�A�r�A�r�A�(�A��A�$�A�dZA��!A��7A��7A�9XA��A�ĜA��wA��A�n�A�ĜA�ƨA�^5A��RA�^5A���A���A�jA��RA�"�A��A���A���A�JAy�Aj��A];dAM�#ABE�A5�
A4��A-p�A+�hA&�A��A33A��A��A�A�
A��@��@�M�@�p�@�E�@���@ݩ�@�X@ׅ@Ցh@Η�@�33@��@�Ĝ@�$�@��@��@�b@���@�/@��@�  @���@��@�o@�I�@���@�r�@�-@�1'@�M�@�I�@��@�\)@���@�;d@}p�@u/@i��@`�@Z^5@SS�@Kt�@C"�@;�m@3dZ@-��@*��@$�j@\)@�\@{@&�@�@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aΰ!AΓuA�jA�bNA�`BA�^5A�\)A�`BA�bNA�dZA�hsA�hsA�jA�l�A�n�A�n�A�r�A�r�A�(�A��A�$�A�dZA��!A��7A��7A�9XA��A�ĜA��wA��A�n�A�ĜA�ƨA�^5A��RA�^5A���A���A�jA��RA�"�A��A���A���A�JAy�Aj��A];dAM�#ABE�A5�
A4��A-p�A+�hA&�A��A33A��A��A�A�
A��@��@�M�@�p�@�E�@���@ݩ�@�X@ׅ@Ցh@Η�@�33@��@�Ĝ@�$�@��@��@�b@���@�/@��@�  @���@��@�o@�I�@���@�r�@�-@�1'@�M�@�I�@��@�\)@���@�;d@}p�@u/@i��@`�@Z^5@SS�@Kt�@C"�@;�m@3dZ@-��@*��@$�j@\)@�\@{@&�@�@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
s�B
y�B
w�B
x�B
w�B
x�B
x�B
y�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
x�B
u�B
\)B
m�B
�oB
��B
��B
��B
��B�BE�Bk�B��B��B�B��B�TB�BB�qB��B�hBgmBI�B2-BDB
�B
��B
v�B
�B	��B	�1B	(�B�yB�B��B��B�B	{B	%B�ZB��B�B�1Bz�B|�B�%B�bB��B�%B�B�+B�JB�JB��B��B��B�LB�B	B	�B	.B	I�B	_;B	z�B	�hB	��B	�'B	�qB	ȴB	��B	�#B	�NB	�mB	�B	��B	��B
  B
B
%B
\B
�B
!�B
'�B
/B
;dB
B�B
I�B
P�B
YB
^5B
ffB
k�B
n�B
s�B
x�B
{�B
�B
�%B
�=B
�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
jKB
poB
ncB
oOB
nIB
oOB
oOB
pUB
oiB
oOB
oOB
pUB
pUB
pUB
pUB
pUB
o�B
ncB
]IB
g�B
�XB
��B
�zB
�dB
�BhB=<Bc�B�'B�SB� B�PB�)B��B��B�B�xB`BBBAB,WBGB
�FB
�RB
p�B
�B	�B	�AB	#�B� B�HB��B��B��B	dB�B��B�@Bz^B� Br�BtTB}�B��B��B}�Bx�B~]B��B�aB��B� B�HB�cB�NB�PB	vB	%B	@�B	VB	q�B	�KB	��B	�$B	�TB	��B	��B	�B	�1B	�jB	�B	��B	��B	��B	�B	�B
YB
�B
�B
�B
%�B
2GB
9XB
@�B
G�B
O�B
T�B
]IB
bhB
e`B
jB
o�B
r�B
y�B
|�B
�B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9998(+-0.0000), deepest deltaS=-0.009(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202012240015312020122400153120201224001531202207232057522022072320575220220723205752202207261129392022072611293920220726112939  JA  ARFMdecpV4_b                                                                20201210065144  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201210065145  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201210065145  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201210065145  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201210065145  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201210065146  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20201210065208                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20201213215118  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201213215132  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201213215132  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201213215132  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201213215133  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201213215133  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201213215133                      G�O�G�O�G�O�                JA  ARUP                                                                        20201213215151                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20201214000000  CF  PSAL_ADJUSTED_QC?��?��G�O�                JM  ARCAJMQC2.0                                                                 20201223151531  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201223151531  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115752  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022939  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                