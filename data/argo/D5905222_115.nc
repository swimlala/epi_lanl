CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-07-23T06:51:33Z creation;2021-07-26T21:51:52Z conversion to V3.1;2022-11-10T04:18:21Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20210723065133  20221117231507  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA  V4_131533_115                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @م�Q�n 1   @ن
=p�@1��t�j�c�fffff1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@ə�A!��Aq��A�33A�ffA�  BffB ��B333BF  BX��BrffB�  B���B�  B�  B�  B���B���B���B�  B�  B晚B�  B�  C�C33C� C��C�3C� C ffC%L�C*�C/�C433C:33C>� CC�CG��CQffC[�3Ce��Co� Cy��C��C�ٚC�ٚC��3C�@ C��3C�Y�C��3C�Y�C���C�L�C�� C�33C�Y�C��fC�  C��C�L�C�ٚC�Y�C�L�C�ffC��fC��C�Y�D�D�D��D  D� D��D   D$��D)�3D/�D4  D99�D>,�DB�3DG��DM&fDQ��DV�3D[�3D`ٚDf  Dk  Do�3DufDy�3D�,�D�p D��fD���D�P D�� D�� D���D�L�D�|�D���D�fD�S3DԐ D�� D�fD�P D�fD��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?L��@�ffA   Ap  A�ffAř�A�33B  B ffB2��BE��BXffBr  B���B�ffB���B���B���B�ffB�ffBǙ�B���B���B�ffB���B���C  C�CffC�3C��CffC L�C%33C*  C/  C4�C:�C>ffCC  CG�3CQL�C[��Ce� CoffCy�3C��C���C���C��fC�33C��fC�L�C��fC�L�C�� C�@ C�s3C�&fC�L�C�ٚC��3C�  C�@ C���C�L�C�@ C�Y�C�ٚC�  C�L�D3D3D�3D��DٚD�3D �D$�fD)��D/fD3��D933D>&fDB��DG�3DM  DQ�fDV��D[��D`�3De��Dk�Do��Du  Dy��D�)�D�l�D��3D���D�L�D���D���D��fD�I�D�y�D�ɚD�3D�P DԌ�Dڼ�D�3D�L�D�3D�� D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�S�A�9XA�C�A�-A�oA���A���A���A���A���A���A���A���A��A��A��AЛ�A��A�$�Aɗ�A��mAǗ�A�n�A�jA��A�l�A�`BA�n�A�\)A��-A�^5A���A��hA��DA���A�=qA���A�(�A��A�C�A��uA�1A�=qA�JA��#A}�AuK�A^��AR�/AJVA<=qA, �A%A�
A�A�A��AA�AS�A"�A33A�;AjA �A/Ap�A ��@�/@�G�@�-@�+@�F@ّh@��@�1'@�5?@�ȴ@�X@��`@�$�@�@��h@�%@�{@��@�p�@��`@��#@�ȴ@��@���@���@���@��y@�A�@�\)@��@y&�@pbN@gK�@\��@Q��@Kt�@F�@BM�@;dZ@3�m@-��@'�w@#��@
=@33@K�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�S�A�9XA�C�A�-A�oA���A���A���A���A���A���A���A���A��A��A��AЛ�A��A�$�Aɗ�A��mAǗ�A�n�A�jA��A�l�A�`BA�n�A�\)A��-A�^5A���A��hA��DA���A�=qA���A�(�A��A�C�A��uA�1A�=qA�JA��#A}�AuK�A^��AR�/AJVA<=qA, �A%A�
A�A�A��AA�AS�A"�A33A�;AjA �A/Ap�A ��@�/@�G�@�-@�+@�F@ّh@��@�1'@�5?@�ȴ@�X@��`@�$�@�@��h@�%@�{@��@�p�@��`@��#@�ȴ@��@���@���@���@��y@�A�@�\)@��@y&�@pbN@gK�@\��@Q��@Kt�@F�@BM�@;dZ@3�m@-��@'�w@#��@
=@33@K�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
v�B
|�B
}�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
|�B
}�B
|�B
|�B
z�B
m�B
�B
�B
�B
�B
!�B
8RB
^5B
�1B
�9B
�jB
��B$�B;dB�B��B�9BƨB�
B�B�B�B�B�B�;B�3BD�B
�^B
r�B
�B	�HB	_;B	-B	PB	BĜB�-B�9B	%B	+B�/B��B{�B|�B�bB��B�B��B	
=B	&�B	49B	7LB	:^B	D�B	^5B	ZB	J�B	\)B	r�B	�1B	��B	�^B	�B	�/B	�fB	�B	�B	��B	��B
B	��B	��B
B

=B
VB
hB
oB
�B
�B
'�B
-B
6FB
=qB
@�B
I�B
Q�B
W
B
\)B
`BB
e`B
l�B
s�B
w�B
{�B
� B
�B
�7B
�JB
�b31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
bhB
h�B
iyB
h�B
h�B
h�B
h�B
hsB
h�B
iyB
h�B
iyB
hsB
iyB
hsB
h�B
f�B
]�B
dB
�B
�B
B
�B
$�B
J�B
t9B
��B
��B
�B4B)yBo�B��B��B��B�+B��B�yB�+B�~B�dB�B�TB5?B
�sB
aHB
xB	҉B	NpB	#B�VB��B��B��B��B��B�B��B��Bh�Bi*B|jB��B�]B�B��B	&B	 �B	#�B	&�B	1'B	J�B	F�B	7fB	H�B	_;B	t�B	��B	�fB	�AB	ɆB	ҽB	��B	��B	�,B	�B	�)B	�,B	�B	�IB	�`B	�^B	�qB	�wB
�B
�B
B
1B
"4B
)yB
,�B
5�B
=�B
B�B
H1B
L0B
QhB
XyB
_�B
c�B
g�B
k�B
pB
u?B
x8B
|P31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.3(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.020(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202108070016242021080700162420210807001624202210251312562022102513125620221025131256202210251810342022102518103420221025181034  JA  ARFMdecpV4_b                                                                20210723065133  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210723065133  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210723065134  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210723065134  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210723065134  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210723065135  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20210723065203                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20210726215134  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20210726215151  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20210726215151  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20210726215151  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20210726215151  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20210726215152  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210726215152                      G�O�G�O�G�O�                JA  ARUP                                                                        20210726215211                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20210802151813  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20210802151813  QCF$                G�O�G�O�G�O�300000          JM  ARCAJMQC2.0                                                                 20210806151624  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210806151624  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041256  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091034  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231507                      G�O�G�O�G�O�                