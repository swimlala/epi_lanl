CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-08T06:52:15Z creation;2020-06-11T21:53:07Z conversion to V3.1;2022-11-10T04:20:11Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200608065215  20221117231507  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               JA   JA  V4_131533_074                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�z�Ā1   @���1� @.E�Q��d�ě��T1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?ٙ�@���A.ffAy��A�33A͙�A�33B��B!33B6��BG33B\ffBs��B�33B�ffB�33B���B���B���B�33B�33B�  Bܙ�B晚B�  B���C  CL�C�3C��C  C  C��C%33C)�3C.L�C4� C9�C=��CBL�CHL�CS�C\�3CfL�Cp�3Cz� C�� C��C��C�s3C�  C�  C�33C��3C��C�ffC��C�Y�C�&fCÀ C���C��C���C֦fC�L�C�s3C�ffC�s3C�ffC�ٚC��D�fD�3D��D�3D&fDfD �D%fD)� D/&fD4  D8��D>&fDB�3DHfDL��DR  DV� D[�3Da  De�3Dj��Dp  Du�Dy��D�33D�� D��fD���D�,�D��3D���D��D�C3D���D�ɚD��D�I�D�y�Dڼ�D��D�0 D퉚D�� D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?ٙ�@���A.ffAy��A�33A͙�A�33B��B!33B6��BG33B\ffBs��B�33B�ffB�33B���B���B���B�33B�33B�  Bܙ�B晚B�  B���C  CL�C�3C��C  C  C��C%33C)�3C.L�C4� C9�C=��CBL�CHL�CS�C\�3CfL�Cp�3Cz� C�� C��C��C�s3C�  C�  C�33C��3C��C�ffC��C�Y�C�&fCÀ C���C��C���C֦fC�L�C�s3C�ffC�s3C�ffC�ٚC��D�fD�3D��D�3D&fDfD �D%fD)� D/&fD4  D8��D>&fDB�3DHfDL��DR  DV� D[�3Da  De�3Dj��Dp  Du�Dy��D�33D�� D��fD���D�,�D��3D���D��D�C3D���D�ɚD��D�I�D�y�Dڼ�D��D�0 D퉚D�� D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aׇ+A�z�A�z�A׃A�z�A�~�A�n�A�p�A�p�A�t�A�t�A�v�A�x�A�v�A�I�Aԧ�A�/A��A�p�AɬA�z�A�oA�S�A�?}A���Aç�A�1A�{A�1'A���A�l�A��yA�K�A��#A���A�JA��/A��A��7A�%Az��Ah=qA[`BAQ��AE��A9�
A5"�A.��A$�A$ �A!��A�uA��A�mA`BA�A �A`BA
bNA��Ax�A@��T@�X@���@���@���@��@�=q@�w@���@�!@�z�@�33@ӝ�@Ͼw@ͺ^@��@�1@�r�@���@��7@�J@���@��@�5?@�@��j@��!@�o@��
@��!@��@��@�
=@�/@�M�@���@�@v{@l1@e�@]�@T�D@JJ@?�@8�9@0Q�@*��@%�@!G�@�j@�y@�@�P@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aׇ+A�z�A�z�A׃A�z�A�~�A�n�A�p�A�p�A�t�A�t�A�v�A�x�A�v�A�I�Aԧ�A�/A��A�p�AɬA�z�A�oA�S�A�?}A���Aç�A�1A�{A�1'A���A�l�A��yA�K�A��#A���A�JA��/A��A��7A�%Az��Ah=qA[`BAQ��AE��A9�
A5"�A.��A$�A$ �A!��A�uA��A�mA`BA�A �A`BA
bNA��Ax�A@��T@�X@���@���@���@��@�=q@�w@���@�!@�z�@�33@ӝ�@Ͼw@ͺ^@��@�1@�r�@���@��7@�J@���@��@�5?@�@��j@��!@�o@��
@��!@��@��@�
=@�/@�M�@���@�@v{@l1@e�@]�@T�D@JJ@?�@8�9@0Q�@*��@%�@!G�@�j@�y@�@�P@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
  B	��B	��B	�B
B
�B
5?B
B�B
iyB
�=B
�B
��B�B^5BD�B�BgmBt�Bv�BM�B
��B
�#B
��B
~�B
\)B
!�B	��B	ŢB	jB	B�B	(�B	
=B��B��B	  B	DB	\B	$�B	%�B	$�B	(�B	/B	B�B	O�B	r�B	r�B	>wB	l�B	� B	�B	��B	��B	��B	�3B	�^B	�qB	ĜB	�^B	ĜB	��B	��B	��B	�#B	�NB	�ZB	�mB	�B	�B	��B	��B	��B
B
B
	7B
JB
PB
PB
PB
hB
bB
�B
�B
!�B
(�B
/B
6FB
;dB
>wB
C�B
J�B
O�B
R�B
\)B
`BB
ffB
jB
n�B
s�B
v�B
{�B
}�B
�B
�+31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	��B	��B
 B
B
 B
B
B
B
B
B
B
 B
 4B	�BB	�B	��B
3B
9B
5tB
B�B
jB
�DB
�}B
�B�Bb�BI�B�MBm�Bx�BzBS@B
��B
ߤB
�]B
�-B
a�B
&�B	��B	��B	nB	E9B	+�B	�B�B��B	�B	xB	B	&LB	&�B	%�B	)�B	0B	C-B	P�B	r�B	t�B	>�B	mB	��B	�'B	�B	�4B	�_B	��B	��B	��B	�SB	��B	�B	�)B	�.B	�FB	�WB	�B	��B	�B	�B	�B	�B	�8B	�"B
UB
mB
	lB
~B
�B
�B
jB
�B
}B
�B
�B
"B
)*B
/OB
6zB
;�B
>wB
C�B
J�B
PB
S&B
\CB
`vB
f�B
jB
n�B
s�B
v�B
|B
~B
�B
�+31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006220016022020062200160220200622001602202210251312172022102513121720221025131217202210251807562022102518075620221025180756  JA  ARFMdecpV4_b                                                                20200608065214  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200608065215  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200608065216  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200608065217  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200608065217  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200608065217  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200608065356                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200611215225  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200611215305  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200611215305  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200611215306  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200611215306  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200611215306  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200611215307                      G�O�G�O�G�O�                JA  ARUP                                                                        20200611215358                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200611151600  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20200611151600  QCF$                G�O�G�O�G�O�200000          JM  ARCAJMQC2.0                                                                 20200621151602  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200621151602  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041217  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090756  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231507                      G�O�G�O�G�O�                