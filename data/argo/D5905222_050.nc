CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-11T06:52:50Z creation;2019-10-14T21:53:35Z conversion to V3.1;2022-11-10T04:21:12Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20191011065250  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA  V4_131533_050                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��:��� 1   @��F�io @.O�;dZ�d#��v�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�ffA��Ai��A���A�ffA�33B��B��B4  BHffB_��Bs��B�ffB�33B���B�33B�ffB���B���B�ffB�ffB�ffB�33B�B���CffC� C��C33C��C� C 33C%�C*  C/�C4�C9�C>ffCB� CH33CR��C[��Cf�3Cp33Cz�C�ffC��C�Y�C�33C��C�ffC��C�Y�C��C�s3C�@ C�@ C�L�C�� C��C��3C�  C�&fCی�C�ٚC�33C�3C��C�ffC��fD�fDٚD�fDfD�DٚD   D%�D*,�D/@ D3��D93D=��DCfDG��DL��DQٚDV�fD\�Da  De��Dk  Dp�Dt�3DzfD�0 D��fD�� D�  D�L�D���D�� D�	�D�C3D�� D�ٚD��fD�<�DԀ Dڹ�D� D�,�D�i�D�D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�33A33Ah  A���Ař�A�ffB33B33B3��BH  B_33Bs33B�33B�  B���B�  B�33B�ffB���B�33B�33B�33B�  B�ffB�ffCL�CffC� C�C�3CffC �C%  C)�fC/  C4  C9  C>L�CBffCH�CR� C[�3Cf��Cp�Cz  C�Y�C��C�L�C�&fC�  C�Y�C��C�L�C�  C�ffC�33C�33C�@ C³3C��C��fC��3C��Cۀ C���C�&fC�fC�  C�Y�C���D� D�3D� D  DfD�3D �D%fD*&fD/9�D3�fD9�D=�3DC  DG�fDL�3DQ�3DV� D\fDa�De�fDk�Dp3Dt��Dz  D�,�D��3D���D���D�I�D��fD���D�fD�@ D�|�D��fD��3D�9�D�|�DڶfD��D�)�D�ffD�fD�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A敁A�33A�
=A��#A��
A��A��HA��A��A��HA��HA��HA�FA�v�Aו�Aˣ�A��TA��A�l�A��A���A�z�A�x�A�XA���A���A��AxQ�An��Af�Ac��AY`BAU��AN5?AK&�AB��A>��A=�A9�A7dZA6bA.1'A)��A#`BA��A
=AE�A�A�AoA��A	��A	hsA�jAVA��A �D@���@��@�K�@��/@�P@��@�F@�G�@��@�J@�+@܃@�
=@���@Ձ@�`B@��@�|�@Ý�@��H@���@��@��h@��y@�Q�@��7@�~�@�^5@��m@��7@�(�@�@���@��@��@��u@�5?@��9@��^@�1'@|�j@s��@j��@_;d@V$�@Nff@D��@=�@4z�@-�-@'�;@"�@v�@7L@��@�7@��@`B@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A敁A�33A�
=A��#A��
A��A��HA��A��A��HA��HA��HA�FA�v�Aו�Aˣ�A��TA��A�l�A��A���A�z�A�x�A�XA���A���A��AxQ�An��Af�Ac��AY`BAU��AN5?AK&�AB��A>��A=�A9�A7dZA6bA.1'A)��A#`BA��A
=AE�A�A�AoA��A	��A	hsA�jAVA��A �D@���@��@�K�@��/@�P@��@�F@�G�@��@�J@�+@܃@�
=@���@Ձ@�`B@��@�|�@Ý�@��H@���@��@��h@��y@�Q�@��7@�~�@�^5@��m@��7@�(�@�@���@��@��@��u@�5?@��9@��^@�1'@|�j@s��@j��@_;d@V$�@Nff@D��@=�@4z�@-�-@'�;@"�@v�@7L@��@�7@��@`B@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�!B�B�!B�'B�?B�XBǮB��B�B�#B�;B	5?B

=B	�=B	`BB	�RB
-B
R�B
P�B
]/B
k�B
y�B
o�B
u�B
:^B
B	�B	u�B	P�B	:^B	oB	B�B�B�
B�
B�mB�`B�#B�B��BȴB��B	oB	�B	#�B	"�B	1'B	>wB	:^B	K�B	o�B	�B	�B	�B	�JB	�DB	�PB	�oB	��B	��B	��B	��B	��B	�'B	�-B	�LB	B	ÖB	��B	�
B	�B	�HB	�ZB	�mB	�B	�B	�B	��B	��B
  B
B
+B
JB
VB
oB
uB
�B
�B
�B
�B
�B
 �B
!�B
&�B
.B
5?B
;dB
A�B
G�B
M�B
R�B
YB
^5B
bNB
hsB
l�B
q�B
u�B
y�B
|�B
�B
�B
�B
�131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�nB�UB�OB�;B�AB�ZB�rB��B�B�+B�=B��B	9�B
hB	�mB	l=B	�B
2|B
[�B
S�B
`�B
o5B
� B
q�B
z^B
?.B
	�B	�'B	y�B	SB	>�B	�B	�B�B��B��B�_B�yB�B��B�)B�oB�rB�qB	uB	 B	%,B	$B	1�B	?HB	;JB	K�B	o�B	��B	��B	�B	��B	��B	��B	��B	��B	�XB	��B	�:B	�@B	�AB	��B	��B	��B	��B	�@B	�YB	ڠB	�B	�B	��B	�B	� B	�B	�*B	�"B
 4B
aB
zB
�B
�B
�B
�B
�B
�B
�B
�B
B
 �B
"B
'8B
.cB
5tB
;�B
A�B
G�B
NB
S&B
YKB
^OB
bhB
h�B
l�B
q�B
u�B
zB
}"B
�B
�B
�9B
�131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<AV�<?�[<[��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.1(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910250016092019102500160920191025001609202210251311542022102513115420221025131154202210251806232022102518062320221025180623  JA  ARFMdecpV4_b                                                                20191011065249  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191011065250  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191011065250  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191011065251  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191011065251  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191011065251  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20191011065517                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20191014215229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191014215333  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191014215334  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191014215334  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191014215334  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191014215334  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191014215335                      G�O�G�O�G�O�                JA  ARUP                                                                        20191014215419                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191016151601  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20191016151601  QCF$                G�O�G�O�G�O�200000          JM  ARCAJMQC2.0                                                                 20191024151609  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191024151609  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041154  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090623  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                