CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   q   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-03T06:53:08Z creation;2020-02-06T21:53:29Z conversion to V3.1;2022-07-26T02:44:27Z update;     
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
resolution        =���     �  <<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Bp   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  E   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  GT   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i0Argo profile    3.1 1.2 19500101000000  20200203065308  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_137                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @����� 1   @� �S�@4������cp���F1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�33A)��Ax  A�  A�33A�33B  B��B533BF��B^ffBo��B���B�  B���B���B�  B���B�ffB�ffB�ffB�  B���B�33B���C��C� CffC��C�C33C33C&�C*�3C/L�C4�C9��C=��CCffCG��CQ��C\�Cf  CpffCz�3C�� C�@ C�L�C�  C�� C��C��3C���C�� C���C��3C�33C�s3C��3C��3C�  C�33C��3C�Y�C��C�@ C�L�C�L�C�� C�&fD�3DٚD  D&fD  D��D &fD%33D*9�D.��D3� D8��D>�DC  DG��DMfDR�DW&fD[�3Da  Df&fDk33Do��Du�Dz&fD�,�D�|�D�ɚD�3D�9�D�c3D�ɚD�fD�L�D���D��3D��D�S3D�s3DڶfD�3D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ?fff@�  A(  AvffA�33A�ffA�ffB��BffB4��BFffB^  Bo33B���B���B�ffB�ffB���B�ffB�33B�33B�33B���B噚B�  B���C� CffCL�C�3C  C�C�C&  C*��C/33C4  C9� C=�3CCL�CG�3CQ�3C\  Ce�fCpL�Cz��C��3C�33C�@ C��3C�s3C��C��fC�� C��3C�� C��fC�&fC�ffC��fC��fC��3C�&fC��fC�L�C�  C�33C�@ C�@ C�s3C��D��D�3D��D  D��D�fD   D%,�D*33D.�3D3ٚD8�fD>3DB��DG�3DM  DR3DW  D[��D`��Df  Dk,�Do�fDufDz  D�)�D�y�D��fD�  D�6fD�` D��fD�3D�I�D��fD�� D�	�D�P D�p Dڳ3D�  D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA�XA�`BA�Q�A�S�A�K�A�I�A�M�A�M�A�M�A�O�A�Q�A�S�A�XA�XA�\)A�O�A�5?A©�A���A� �A�VA��A���A�A�JA��DA��9A��;A���A�I�A�;dA�C�A�r�A�z�A���A�XA��A���A�1'A��jA�Aw7LAi��A_�AUx�AK|�AC�mA=�A733A0�uA,-A)A$�RA!�AbA��A5?A��A�A��A �@���@�{@띲@�{@���@�^5@�j@�b@��@å�@��F@��@���@�V@�o@�r�@��@�A�@� �@��@��\@��@��
@�K�@���@��
@�/@�j@��R@��9@���@��!@���@� �@+@xbN@pbN@l��@e`B@]@U��@Q�^@G�@B��@9�^@/�P@(r�@!��@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA�XA�`BA�Q�A�S�A�K�A�I�A�M�A�M�A�M�A�O�A�Q�A�S�A�XA�XA�\)A�O�A�5?A©�A���A� �A�VA��A���A�A�JA��DA��9A��;A���A�I�A�;dA�C�A�r�A�z�A���A�XA��A���A�1'A��jA�Aw7LAi��A_�AUx�AK|�AC�mA=�A733A0�uA,-A)A$�RA!�AbA��A5?A��A�A��A �@���@�{@띲@�{@���@�^5@�j@�b@��@å�@��F@��@���@�V@�o@�r�@��@�A�@� �@��@��\@��@��
@�K�@���@��
@�/@�j@��R@��9@���@��!@���@� �@+@xbN@pbN@l��@e`B@]@U��@Q�^@G�@B��@9�^@/�P@(r�@!��@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
M�B
VB
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
ZB
ZB
YB
[#B
_;B
n�B
q�B
ŢB
�TBDB@�BK�Bt�B8RB&�B��B�oB1B�B �BuBÖB]/B
�B
�'B
{�B
�5B
�B
�;B
ƨB
@�B	ŢB	�VB	u�B	;dB	1B�B��B�RB��B��B�{B�1B~�Bw�Bo�B[#BO�BG�BC�B>wB;dB?}B@�BD�BE�BN�B^5BbNBk�B{�B��BȴB�sB�B	DB	'�B	;dB	[#B	m�B	{�B	�JB	�hB	��B	�'B	�jB	ȴB	��B	��B	�#B	�NB	�B	�B	��B
  B
1B
�B
�B
!�B
2-B
;dB
B�B
F�B
O�B
T�B
]/B
e`B
iyB
p�B
u�B
x�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
:�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
EB
D�B
D�B
F�B
F�B
E�B
G�B
LJB
[�B
`�B
��B
�uB
�HB/�B;�BeB&�BB� B�B�ZB�BpB�B�9BN�B
��B
�B
hsB
�xB
چB
��B
�LB
1�B	�+B	~B	ezB	+kB�LBچB��B�B��B��B�Bv`Bm]BffB^OBJXB>�B5�B2-B-B)�B-�B.�B2�B4B=<BLJBP}BY�Bi�B��B�FB��B�5B��B	gB	(�B	H�B	[	B	i_B	y�B	~�B	�FB	��B	��B	�+B	��B	�UB	ȚB	ϑB	��B	�-B	�B	�]B	�tB
�B
	�B
B
�B
(�B
/�B
4B
=B
B[B
JrB
R�B
V�B
]�B
b�B
f2B
p;31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.018(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202002170015292020021700152920200217001529202207232057242022072320572420220723205724202207261127412022072611274120220726112741  JA  ARFMdecpV4_b                                                                20200203065307  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200203065308  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200203065308  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200203065309  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200203065309  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200203065309  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200203065459                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200206215222  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200206215327  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200206215328  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200206215328  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200206215329  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200206215329  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200206215329                      G�O�G�O�G�O�                JA  ARUP                                                                        20200206215414                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200207000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20200216151529  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200216151529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115724  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022741  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                