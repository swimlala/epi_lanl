CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-29T06:52:47Z creation;2020-02-01T21:53:19Z conversion to V3.1;2022-11-10T04:20:44Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200129065247  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA  V4_131533_061                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @����L��1   @���β@�@0�j~��#�d���R1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�ffA33Ap  A�33A���A陚BffB!��B3��BFffBZffBn��B���B�33B�ffB���B���B���B���Bș�Bԙ�B�  B晚B�B�33C33CL�C�C�C��C�3C �C%��C)��C/  C4�3C8�fC>� CC  CG��CQ�fC[  Cf  Cp� C{�C�ffC��3C��fC���C�@ C��3C�  C�s3C��C��3C�@ C��C�@ C CǙ�C�� CѦfC��C�ffC�ٚC��C�L�C��C�33C��D�D��D  D  D,�D&fD ,�D%fD*,�D.��D4,�D933D>,�DC  DH&fDL�3DR33DV��D\3Da,�Df,�Dj��Dp33Du�Dz,�D�@ D�� D�ٚD���D�33D�|�D�� D���D�` D���D��3D� D�33D�|�Dڳ3D�3D�FfD�3D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�ffA33Ap  A�33A���A陚BffB!��B3��BFffBZffBn��B���B�33B�ffB���B���B���B���Bș�Bԙ�B�  B晚B�B�33C33CL�C�C�C��C�3C �C%��C)��C/  C4�3C8�fC>� CC  CG��CQ�fC[  Cf  Cp� C{�C�ffC��3C��fC���C�@ C��3C�  C�s3C��C��3C�@ C��C�@ C CǙ�C�� CѦfC��C�ffC�ٚC��C�L�C��C�33C��D�D��D  D  D,�D&fD ,�D%fD*,�D.��D4,�D933D>,�DC  DH&fDL�3DR33DV��D\3Da,�Df,�Dj��Dp33Du�Dz,�D�@ D�� D�ٚD���D�33D�|�D�� D���D�` D���D��3D� D�33D�|�Dڳ3D�3D�FfD�3D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��;A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��#A��/A��/A��;AоwA���A�|�A�=qA���A��;A��A��yA�p�A�=qA��A�"�A�x�A��A��A�%A�ƨA�~�A���A���A�I�A|�HAq+Af��A^ĜAN�HAA�7A:9XA0�/A)l�A$�!A�A�PAQ�A=qAĜAbA&�A\)A�A{A�PA�A   @�;d@�hs@�P@�`B@�5?@��@땁@�V@��@�1'@�O�@Ӿw@�+@�=q@�`B@��/@�7L@�bN@���@�`B@���@�o@�%@���@�9X@�-@��@�l�@�?}@��y@�A�@��^@��@�$�@���@�w@x�`@q�7@hQ�@^�R@VV@K�@@Ĝ@;@4j@-��@(��@#C�@��@��@1@�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��;A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��#A��/A��/A��;AоwA���A�|�A�=qA���A��;A��A��yA�p�A�=qA��A�"�A�x�A��A��A�%A�ƨA�~�A���A���A�I�A|�HAq+Af��A^ĜAN�HAA�7A:9XA0�/A)l�A$�!A�A�PAQ�A=qAĜAbA&�A\)A�A{A�PA�A   @�;d@�hs@�P@�`B@�5?@��@땁@�V@��@�1'@�O�@Ӿw@�+@�=q@�`B@��/@�7L@�bN@���@�`B@���@�o@�%@���@�9X@�-@��@�l�@�?}@��y@�A�@��^@��@�$�@���@�w@x�`@q�7@hQ�@^�R@VV@K�@@Ĝ@;@4j@-��@(��@#C�@��@��@1@�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
+B
.B
-B
,B
,B
,B
+B
+B
+B
,B
+B
+B
+B
,B
,B
,B
,B
-B
.B
-B
+B
��Bw�B��B	7B@�BXBW
B~�B[#BbNB_;B^5B8RB�By�B
�HB
ĜB
t�B
N�B
7LB	ɺB	�uB	jB	E�B	oB	{B��B	{B	\B	%B�B��B		7B	.B	@�B	H�B	P�B	\)B	]/B	A�B	F�B	[#B	\)B	�B	�bB	�\B	��B	��B	�!B	�B	�B	�}B	��B	��B	�B	�5B	�NB	�`B	�yB	�B	�B	��B	��B	��B
  B
B
B
+B

=B
JB
PB
\B
oB
�B
�B
�B
$�B
.B
33B
9XB
<jB
B�B
H�B
O�B
VB
]/B
bNB
dZB
hsB
m�B
o�B
t�B
x�B
}�B
~�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
+B
.B
-B
,B
,B
,"B
+B
+B
+B
,B
+B
+B
+B
,B
,B
,B
,B
-B
.B
-CB
/5B�By>B��BBB�BY�BX�B�?B\�Bc�Ba|B_�B<PB�B�;B
�B
�=B
w�B
P�B
<PB	�B	��B	lqB	I�B	B	�B�rB	�B	�B	B�]B��B		�B	.}B	A;B	I�B	QhB	\�B	^�B	B[B	F�B	\)B	\]B	��B	�4B	��B	�B	�$B	�UB	��B	��B	��B	��B	�BB	�kB	ޞB	�B	�B	��B	��B	�B	�B	�$B	�(B
 4B
GB
SB
_B

XB
dB
�B
�B
�B
�B
�B
�B
%,B
.cB
3MB
9rB
<�B
B�B
H�B
PB
V9B
]/B
bhB
dtB
h�B
m�B
o�B
t�B
x�B
~B
~�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002120015582020021200155820200212001558202210251312042022102513120420221025131204202210251807052022102518070520221025180705  JA  ARFMdecpV4_b                                                                20200129065246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200129065247  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200129065248  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200129065249  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200129065249  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200129065249  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200129065457                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200201215251  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200201215318  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200201215318  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200201215319  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200201215319  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200201215319  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200201215319                      G�O�G�O�G�O�                JA  ARUP                                                                        20200201215404                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200201151547  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20200201151547  QCF$                G�O�G�O�G�O�300000          JM  ARCAJMQC2.0                                                                 20200211151558  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200211151558  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041204  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090705  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                