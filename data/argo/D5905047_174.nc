CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-02-08T06:51:55Z creation;2021-02-11T21:52:03Z conversion to V3.1;2022-07-26T02:42:58Z update;     
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
_FillValue                  `  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Ml   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Sl   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Yl   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  _l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    `   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    `    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    `$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         `4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         `8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        `<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    `@Argo profile    3.1 1.2 19500101000000  20210208065155  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_174                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�\�¿�1   @�\Ȼ�G�@2j��n��cWn��P1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?333@�  A   Aq��A�33A�ffA�33B  B��B533BK33B\  Bs33B�ffB�33B���B�  B�ffB�33B�ffBə�Bҙ�B���B噚BB���CL�CffC�C��CffCffC ��C%�3C*ffC/  C5  C8�fC=�fCD  CG��CQ��C[L�Cf��CpffCz�fC�&fC��3C��3C��C�ٚC���C�L�C��C��C�Y�C��C�Y�C�Y�C�33C��C��C��C�@ C��C��fC��fC�  C�&fC�L�C�@ D  D�D&fD�fD  DfD��D%�D*�D/�D3�3D9  D>�DC,�DH�DM&fDR  DW33D[��Da33De��Dk&fDp33Du  Dz  D�C3D�� D��fD� D�\�D�vfD��fD�  D�` D���D���D��D�I�Dԃ3D��3D�3D�VfD�fD�ɚD�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?333@�  A   Aq��A�33A�ffA�33B  B��B533BK33B\  Bs33B�ffB�33B���B�  B�ffB�33B�ffBə�Bҙ�B���B噚BB���CL�CffC�C��CffCffC ��C%�3C*ffC/  C5  C8�fC=�fCD  CG��CQ��C[L�Cf��CpffCz�fC�&fC��3C��3C��C�ٚC���C�L�C��C��C�Y�C��C�Y�C�Y�C�33C��C��C��C�@ C��C��fC��fC�  C�&fC�L�C�@ D  D�D&fD�fD  DfD��D%�D*�D/�D3�3D9  D>�DC,�DH�DM&fDR  DW33D[��Da33De��Dk&fDp33Du  Dz  D�C3D�� D��fD� D�\�D�vfD��fD�  D�` D���D���D��D�I�Dԃ3D��3D�3D�VfD�fD�ɚD�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A� �A��yA���A���A���A���A���A���A���A���A���A���A��
A��
Aŧ�A�5?Aİ!A���A�(�A��Aŗ�AŰ!A��mA��A�=qA��A��`A�7LA�n�A�?}A� �A�7LA�l�A��`A��A�S�A��FA��9A���A��PA�"�A�C�A{x�Ah��Aa��AQ�mAJ�9A:�+A0^5A,=qA(��A$ffAVA7LAdZA�A��A
�`A$�A�A$�@�7L@�x�@�|�@�-@��@�p�@�=q@Ұ!@϶F@�  @�=q@ă@��/@�t�@��`@��D@��h@��`@��@��/@��@��
@��!@�&�@��R@�S�@���@��!@�p�@��w@�^5@�x�@� �@���@�K�@x��@s33@m��@dI�@[S�@U��@M�@E�@?�@6��@1�@+�m@&v�@ ��@��@�@\)@
�H@E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A� �A��yA���A���A���A���A���A���A���A���A���A���A��
A��
Aŧ�A�5?Aİ!A���A�(�A��Aŗ�AŰ!A��mA��A�=qA��A��`A�7LA�n�A�?}A� �A�7LA�l�A��`A��A�S�A��FA��9A���A��PA�"�A�C�A{x�Ah��Aa��AQ�mAJ�9A:�+A0^5A,=qA(��A$ffAVA7LAdZA�A��A
�`A$�A�A$�@�7L@�x�@�|�@�-@��@�p�@�=q@Ұ!@϶F@�  @�=q@ă@��/@�t�@��`@��D@��h@��`@��@��/@��@��
@��!@�&�@��R@�S�@���@��!@�p�@��w@�^5@�x�@� �@���@�K�@x��@s33@m��@dI�@[S�@U��@M�@E�@?�@6��@1�@+�m@&v�@ ��@��@�@\)@
�H@E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
5?B
>wB
?}B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
E�B
E�B
F�B
H�B
J�B
O�B
P�B
_;B
x�B
��B
�'B
�fBPB �B:^B#�B/B~�B�qB�TBbB�B2-B"�B{B��B��B�dB��B�JBE�B
�HB
�DB
�B	�oB	^5B	B�BBB�B�?B��B��B��B��B��B��B�PB�%B�1B�1B�7B�=B�bB�{B�VB��B��B�FBĜB��B�B�B	hB	'�B	8RB	ZB	v�B	�\B	�B	�FB	�jB	�qB	B	�NB	�B	�B	�B	��B
B
+B
VB
bB
�B
�B
�B
$�B
$�B
/B
?}B
B�B
C�B
H�B
R�B
[#B
`BB
hsB
l�B
p�B
u�B
z�B
� B
�%B
�7B
�\B
�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
6zB
>�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
E�B
E�B
F�B
IB
KDB
P�B
Q�B
_�B
x�B
�-B
��B
�BVB!HB<6B(�B3MB�oB��B�B�B=B4B%�BsB�B�B��B��B��BI�B
�&B
��B
�B	�{B	b�B	zB�BňB��B��B�kB��B�-B��B��B�jB��B�_B��B�RB��B��B�:B�9B�vB�sB��B�2B�mB�^B��B�9B	B	(�B	8�B	Z�B	wLB	��B	��B	��B	�"B	�B	��B	�B	�B	�MB	�'B	�FB
oB
�B
�B
�B
B
 BB
 BB
%`B
%FB
/�B
@ B
CB
C�B
IB
S[B
[�B
`�B
h�B
l�B
qB
v+B
{JB
�iB
��B
��B
��B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES, where PRES is already adjusted with PRESSURE OFFSET; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                   TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & OW error , 0.01(PSS-78))                                                                                                                                 PO1=0.3(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=1.0000(+-0.0001), deepest deltaS=0.000(+-0.004)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                         Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202207260242582022072602425820220726024258202207261130012022072611300120220726113001JA  ARFMdecpV4_b                                                                20210208065155  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210208065155  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210208065155  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210208065156  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210208065156  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210208065156  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20210208065221                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20210211215142  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210211215202  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210211215202  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210211215203  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210211215203  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210211215203  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210211215203                      G�O�G�O�G�O�                JA  ARUP                                                                        20210211215223                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20210212000000  CF  PSAL_ADJUSTED_QC?333?333G�O�                JM  ARCAJMTM1.0                                                                 20220723115757  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726023001  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                