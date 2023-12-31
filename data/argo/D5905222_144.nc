CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-05-09T07:02:54Z creation;2022-05-12T15:56:44Z conversion to V3.1;2022-11-10T04:17:06Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20220509070254  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_144                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��{R}( 1   @��{h|e�@1�S����d�ě��T1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�ffA)��Ap  A�33A�33A�ffB  B33B5��BF  B]33Bo��B�  B�33B�ffB���B�  B���B���B�33B�ffB���B癚B�ffB���C�C�fC�C�3C�3C��CL�C%  C(�fC.33C4� C9�fC?�CCL�CH�CQffC[ffCfL�Cp�Cz� C��3C�  C�Y�C�Y�C�&fC��C��3C�@ C�@ C��C�ٚC��3C�L�C�� C��C�@ C�33C�  C�ffC�L�C�L�C�3C��3C�@ C���DٚD��D  DfD��D  D�fD$� D)��D/�D43D8� D>  DB�fDG��DM3DQ�3DW�D\�D`��Df�DkfDo�fDu  DyٚD�,�D���D��3D�fD�` D�|�D��3D�3D�6fD���D�ٚD�3D�@ DԌ�D�ٚD�fD�FfD�fD��fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�33@�33A(  AnffA�ffA�ffA홚B��B��B533BE��B\��Bo33B���B�  B�33B�ffB���B���B���B�  B�33Bܙ�B�ffB�33B�ffC  C��C  C��C��C� C33C$�fC(��C.�C4ffC9��C?  CC33CH  CQL�C[L�Cf33Cp  CzffC��fC��3C�L�C�L�C��C�  C��fC�33C�33C��C���C��fC�@ C³3C�  C�33C�&fC��3C�Y�C�@ C�@ C�fC��fC�33C�� D�3D�fD��D  D�3D�D� D$ٚD)�fD/3D4�D8��D>�DB� DG�fDM�DQ��DWfD\3D`�3Df3Dk  Do� Du�Dy�3D�)�D��fD�� D�3D�\�D�y�D�� D� D�33D��fD��fD� D�<�Dԉ�D��fD�3D�C3D�3D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA�ĜA���A���AҸRAҬAҙ�Aҙ�Aҙ�Aҝ�Aң�A�~�A�t�A�n�A�ZA�Q�A�A�Aѩ�A�ZA���A�jA��#A�
=AÓuA+A���A�`BA��9A��
A�oA�v�A�z�A�|�A���A�1'A�E�A�G�A���A��#A���A���A�jA�%AS�Ai��AX(�AI��A8M�A41A/�TA'oA ^5AA�AbNAr�A��A�^A  AĜAVA
�uA�A`BAA Z@���@��
@�$�@�@�b@�dZ@�t�@���@�Q�@мj@ȼj@��@���@���@���@��P@�z�@���@���@�=q@��@��@��\@�@�33@�j@�V@��
@���@� �@�S�@��
@�ȴ@yhs@l(�@`�u@Vȴ@O|�@H1'@A��@<�j@8 �@3�m@,(�@$��@�+@J@�R@��@l�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA�ĜA���A���AҸRAҬAҙ�Aҙ�Aҙ�Aҝ�Aң�A�~�A�t�A�n�A�ZA�Q�A�A�Aѩ�A�ZA���A�jA��#A�
=AÓuA+A���A�`BA��9A��
A�oA�v�A�z�A�|�A���A�1'A�E�A�G�A���A��#A���A���A�jA�%AS�Ai��AX(�AI��A8M�A41A/�TA'oA ^5AA�AbNAr�A��A�^A  AĜAVA
�uA�A`BAA Z@���@��
@�$�@�@�b@�dZ@�t�@���@�Q�@мj@ȼj@��@���@���@���@��P@�z�@���@���@�=q@��@��@��\@�@�33@�j@�V@��
@���@� �@�S�@��
@�ȴ@yhs@l(�@`�u@Vȴ@O|�@H1'@A��@<�j@8 �@3�m@,(�@$��@�+@J@�R@��@l�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BO�BVBVBVBVBW
BXBYBZBdZBt�B�JB�VB�bB�PB�JB�PB�B��B-B�DB�%BI�BE�BF�BXBS�BJ�B8RB33B/B�BB�NB�B�dB�DBn�BYB33B+B
��B
��B
S�B	�B	q�B	'�B��B�B�mB�5B�B	JB	&�B	33B	@�B	G�B	aHB	gmB	q�B	��B	��B	��B	��B	��B	��B	��B	�RB	�'B	�RB	ŢB	�qB	�qB	ĜB	ȴB	��B	��B	�)B	�5B	�`B	�B	�B	��B	��B
B
	7B
VB
bB
�B
�B
 �B
#�B
'�B
(�B
,B
2-B
6FB
<jB
D�B
L�B
R�B
YB
]/B
bNB
gmB
l�B
o�B
t�B
x�B
~�B
�B
�1B
�DB
�\B
�oB
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B8lB>BB>BB>BB>BB?cB@iBAoBBuBL�B]Bt�Bv�Bx�Bu�Bt�Bv+B��B�!BSBt�Bs3B49B/5B3�BC�B?�B6�B#TB�B�BB�IB�xB��B�RBu%BX�BD�B�B
�B
��B
��B
BB	�EB	^jB	�B��B��B��B�7BּB��B	}B	B	)�B	1'B	KB	QB	[#B	�PB	��B	�B	��B	�B	�[B	�.B	��B	�B	��B	�5B	�B	��B	�B	�GB	��B	�6B	�9B	ǔB	ΊB	��B	��B	��B	�B	�=B	�GB	�fB	��B	��B
�B
	�B
�B
�B
 B
gB
	B
VB
%`B
-�B
5�B
;�B
A�B
FB
KDB
PHB
U�B
XyB
]�B
a�B
g�B
lB
qB
tB
xRB
{JB
c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.3(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.023(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202205230015062022052300150620220523001506202210251313232022102513132320221025131323202210251812302022102518123020221025181230  JA  ARFMdecpV4_b                                                                20220509070253  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220509070254  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220509070254  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220509070254  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220509070254  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220509070254  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20220509070332                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20220509125705  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220512155640  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220512155640  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220512155641  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220512155641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220512155644  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220512155644                      G�O�G�O�G�O�                JA  ARUP                                                                        20220512155704                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220512151508  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220512151508  QCF$                G�O�G�O�G�O�200000          JM  ARGQJMQC2.0                                                                 20220512151508  CV  JULD_LOCATION   G�O�G�O�F�s�                JM  ARCAJMQC2.0                                                                 20220522151506  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20220522151506  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041323  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091230  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                