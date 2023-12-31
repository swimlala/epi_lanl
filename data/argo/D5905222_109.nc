CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-05-24T06:53:11Z creation;2021-05-27T21:52:07Z conversion to V3.1;2022-11-10T04:18:36Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20210524065311  20221117231507  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               mA   JA  V4_131533_109                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�v�|�G 1   @�w	�qf @0"��`A��c�I�^5?1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�  A��AvffA�ffA�33A�ffB��B��B0��BD��BY33Bm33B���B���B�  B�33B���B�ffB�33B�33Bҙ�B�ffB癚B���B�  C�fC�fC  C��CL�C33C�fC$� C)L�C/ffC4ffC:�C>�fCC��CI  CR�3C[33CfffCo�fCzffC�33C�L�C��3C��3C��C�&fC�  C��3C��C���C���C�33C�&fC��fCǙ�C��3C�  C�ٚC�� C�� C��3C��C��C�� C�� D&fD�D33D&fD�D�D fD$ٚD)� D.��D4,�D9&fD>9�DB�3DH�DMfDR&fDWfD\fDa@ Df9�Dk&fDpfDu�Dz�D�C3D��fD��3D��D�c3D���D��3D��D�` D��fD��3D���D�P Dԃ3DڶfD��fD�P D퉚D�ٚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @���A��Aw33A���AÙ�A���B	  B  B1  BE  BYffBmffB��fB��fB��B�L�B��3B�� B�L�B�L�Bҳ3B݀ B�3B��fB��C�3C�3C�CٚCY�C@ C�3C$��C)Y�C/s3C4s3C:&fC>�3CCٚCI�CR� C[@ Cfs3Co�3Czs3C�9�C�S3C���C���C�3C�,�C�fC���C�  C��3C�� C�9�C�,�C���CǠ C���C�fC�� C��fC��fC���C�3C�3C��fC��fD)�D�D6fD)�D D�D 	�D$��D)�3D.��D40 D9)�D><�DB�fDH DM	�DR)�DW	�D\	�DaC3Df<�Dk)�Dp	�Du�Dz D�D�D�� D���D�fD�d�D��fD���D�fD�a�D�� D���D��3D�Q�DԄ�Dڸ D�� D�Q�D�3D��3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�~�A�E�A�"�Aʲ-Aʡ�Aʗ�AʑhAʍPAʃA�r�A�M�A�(�A� �A��A��A�
=A�%A��Aɧ�A�v�A�/A��yA�v�A��mAčPA���A�33A�9XA��TA�Q�A��A�K�A��A���A�ffA�|�A���A�ȴA���A�1A��PA���A���A��hA�XAx�AW\)ADbA8ȴA3XA.  A(�9A"v�A�A��At�Ax�A`BA%A7LA��Al�A�#A��A	�A�Al�A �@�v�@�C�@��@�`B@�/@�^5@ёh@�\)@�ƨ@�33@��w@�S�@�  @�1'@�j@�1'@��+@��^@�33@��@�5?@��H@��@���@�o@���@�$�@�`B@��
@y�@p �@dz�@\I�@S�m@K��@DZ@:�@1��@+t�@%�@\)@^5@��@j@ �@�
@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�~�A�E�A�"�Aʲ-Aʡ�Aʗ�AʑhAʍPAʃA�r�A�M�A�(�A� �A��A��A�
=A�%A��Aɧ�A�v�A�/A��yA�v�A��mAčPA���A�33A�9XA��TA�Q�A��A�K�A��A���A�ffA�|�A���A�ȴA���A�1A��PA���A���A��hA�XAx�AW\)ADbA8ȴA3XA.  A(�9A"v�A�A��At�Ax�A`BA%A7LA��Al�A�#A��A	�A�Al�A �@�v�@�C�@��@�`B@�/@�^5@ёh@�\)@�ƨ@�33@��w@�S�@�  @�1'@�j@�1'@��+@��^@�33@��@�5?@��H@��@���@�o@���@�$�@�`B@��
@y�@p �@dz�@\I�@S�m@K��@DZ@:�@1��@+t�@%�@\)@^5@��@j@ �@�
@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
A�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
P�B
S�B
W
B
XB
W
B
W
B
W
B
VB
VB
T�B
N�B
Q�B
[#B
{�B
��B
��B
�B+B;dBL�B[#B}�B�\B�\B��B�^B��B�B�B�fB�
B�BdZBA�B
��B
N�B	�HB	)�B	B	PB	0!B	D�B	aHB	`BB	�JB	��B	��B	��B	��B	�!B	�dB	ŢB	�B	�
B	�`B	�TB	�#B	��B	�)B	�9B	��B	��B	��B	��B	ÖB	�}B	�wB	��B	��B	�
B	�5B	�TB	�yB	�B	�B	�B	�B	��B
B
%B
\B
uB
�B
�B
�B
"�B
$�B
+B
5?B
=qB
D�B
K�B
Q�B
YB
^5B
ffB
o�B
s�B
y�B
}�B
�B
�%B
�7B
�JB
�uB
��B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
./B
9$B
9XB
9$B
9$B
9	B
9	B
:*B
;0B
=<B
@OB
CGB
DMB
CGB
CaB
CGB
B[B
B�B
B'B
<6B
>wB
G�B
h�B
��B
��B
ƨB�B(�B:BJ�Bk�B|B}�B��B�_B�UB�_B��BּB�%B��BRTB1'B
�<B
>�B	��B	�B��B�B	B	2�B	O�B	N"B	y	B	�gB	��B	��B	��B	�/B	�>B	��B	��B	�B	�oB	��B	ȴB	��B	��B	��B	��B	��B	�tB	��B	�oB	�qB	�QB	�}B	��B	��B	�B	�.B	�SB	�xB	�B	چB	�dB	�B	��B	��B	�B
 B
GB
EB

XB
vB
�B
�B
!�B
*0B
1AB
8lB
>�B
E�B
J�B
SB
\)B
`BB
fLB
jB
o�B
r�B
u�B
x�B
� B
�B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.019(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202106160016552021061600165520210616001655202210251312502022102513125020221025131250202210251810112022102518101120221025181011  JA  ARFMdecpV4_b                                                                20210524065311  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210524065311  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210524065311  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210524065312  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210524065312  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210524065312  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20210524065341                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20210527215149  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210527215206  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210527215206  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210527215206  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210527215206  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210527215207  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210527215207                      G�O�G�O�G�O�                JA  ARUP                                                                        20210527215225                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20210610152106  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20210610152106  QCF$                G�O�G�O�G�O�300000          JM  ARCAJMQC2.0                                                                 20210615151655  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210615151655  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041250  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091011  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231507                      G�O�G�O�G�O�                