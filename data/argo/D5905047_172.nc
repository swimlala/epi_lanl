CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-19T06:51:31Z creation;2021-01-22T21:51:39Z conversion to V3.1;2022-07-26T02:43:03Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20210119065131  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_172                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�W���. 1   @�W� 0��@2� ě��c>V�u1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�33A��AnffA�  A�ffA�  B��B!33B1��BJ  B\ffBr  B�  B���B�  B�33B���B�33B���B�ffB�33B�  B�  B�33B�  CL�C�C��C� CffC� C �3C$��C*33C/� C3��C833C<�fCB  CG33CQ�fC[��CgL�Cp  Cy�3C��3C��fC���C�  C�&fC�&fC�s3C�� C��C�&fC�L�C��3C�L�C�ٚC�@ C��fCљ�C�@ C��C��3C��3C��3C�  C�&fC�33D  D3D33D  D3D� D &fD$�3D)�3D.�fD3�3D9,�D=� DB�3DG�3DL��DQ��DV��D\,�D`ٚDe��Dk  Dp�Du�Dz  D�S3D�|�D��3D�fD�I�D���D��3D�fD�<�D��3D��fD��D�\�D�p D�� D���D�\�D�fD�ٚD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�33A��AnffA�  A�ffA�  B��B!33B1��BJ  B\ffBr  B�  B���B�  B�33B���B�33B���B�ffB�33B�  B�  B�33B�  CL�C�C��C� CffC� C �3C$��C*33C/� C3��C833C<�fCB  CG33CQ�fC[��CgL�Cp  Cy�3C��3C��fC���C�  C�&fC�&fC�s3C�� C��C�&fC�L�C��3C�L�C�ٚC�@ C��fCљ�C�@ C��C��3C��3C��3C�  C�&fC�33D  D3D33D  D3D� D &fD$�3D)�3D.�fD3�3D9,�D=� DB�3DG�3DL��DQ��DV��D\,�D`ٚDe��Dk  Dp�Du�Dz  D�S3D�|�D��3D�fD�I�D���D��3D�fD�<�D��3D��fD��D�\�D�p D�� D���D�\�D�fD�ٚD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aǡ�A�bNA��A��A���A���A���A���A�  A�(�A��HA���Aˡ�A�\)A�{A�bAʺ^AʑhA�"�A�n�A�ZA�1A���A��jA��DA�t�A�=qA�ZA���A�v�A��A��TA��hA�VA�A�1A�ZA��A��yA���A���A� �A���A���Ax(�AjffA^1ARz�AK�ADQ�A8E�A/�7A'/A$z�AE�A1'A%A(�A{A�A5?A1'@�hs@�P@�`B@�;d@�V@�E�@�Z@�-@�$�@��@�1@���@�$�@�~�@�A�@���@�`B@���@�;d@�I�@��u@���@�=q@�5?@�@�Q�@�-@�p�@�1@�z�@�5?@�  @��@�ƨ@���@x��@p��@g�;@_��@V��@P�`@HĜ@Ax�@<�j@6��@/;d@(Q�@$9X@K�@"�@��@�@/@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aǡ�A�bNA��A��A���A���A���A���A�  A�(�A��HA���Aˡ�A�\)A�{A�bAʺ^AʑhA�"�A�n�A�ZA�1A���A��jA��DA�t�A�=qA�ZA���A�v�A��A��TA��hA�VA�A�1A�ZA��A��yA���A���A� �A���A���Ax(�AjffA^1ARz�AK�ADQ�A8E�A/�7A'/A$z�AE�A1'A%A(�A{A�A5?A1'@�hs@�P@�`B@�;d@�V@�E�@�Z@�-@�$�@��@�1@���@�$�@�~�@�A�@���@�`B@���@�;d@�I�@��u@���@�=q@�5?@�@�Q�@�-@�p�@�1@�z�@�5?@�  @��@�ƨ@���@x��@p��@g�;@_��@V��@P�`@HĜ@Ax�@<�j@6��@/;d@(Q�@$9X@K�@"�@��@�@/@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB��B��B��B��B��B��B��B�sB	ffB
r�B
z�B
}�B
�B
�B
��B
��B
��B
��B
��B
��B
�B
��B<jBN�B\)B_;BhsBv�B��BȴB�`B��BBȴB��BȴB�?B��B�7B�!B:^B
��B
u�B	�#B	{�B	8RB	B�B��B�B��B��B��B�oB�VB�B�B�B|�Bz�B�Bx�Bu�B|�B�JB�VB�VB��B��B��B�jB�5B��B	 �B	@�B	e`B	l�B	r�B	�bB	��B	�B	�jB	��B	��B	��B	�mB	�B	��B
B
%B
B
B
DB
bB
hB
�B
&�B
33B
9XB
D�B
N�B
T�B
YB
[#B
gmB
k�B
m�B
q�B
w�B
|�B
�B
�B
�=B
�VB
�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B� B�7B�B��B��B��B��B��B��B	`�B
oB
xB
{0B
~]B
�4B
��B
��B
��B
��B
��B
�KB
��B
��B;�BL�BYeB]/Bf�BuB��B��B�@B�^B�BȀB��B�B�?B��B�KB�B:�B
̘B
y$B	��B	|6B	8lB	 B�QB� B��B�$B��B��B��B��B�B��BcB{By$B.Bw2Bs�B{B�	B��B�JB��B�&B�/B��B��B�^B	B	=�B	b�B	i�B	o�B	��B	��B	�kB	��B	�)B	�<B	� B	�B	��B	��B
AB
{B
 OB
UB
�B
�B
�B
�B
$B
0UB
6`B
A�B
K�B
R:B
V9B
XEB
dtB
h�B
j�B
n�B
t�B
zB
.B
�AB
�_B
�xB
�p31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9999(+-0.0001), deepest deltaS=-0.003(+-0.003)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202102020015302021020200153020210202001530202207232057552022072320575520220723205755202207261129542022072611295420220726112954  JA  ARFMdecpV4_b                                                                20210119065130  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210119065131  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210119065131  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210119065131  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210119065131  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210119065131  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20210119065158                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20210122215122  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210122215138  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210122215138  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210122215138  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210122215138  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210122215139  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210122215139                      G�O�G�O�G�O�                JA  ARUP                                                                        20210122215157                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20210123000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20210201151530  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210201151530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115755  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022954  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                