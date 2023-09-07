CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-07T06:52:05Z creation;2019-08-10T15:52:43Z conversion to V3.1;2022-07-26T02:45:10Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190807065205  20220818051506  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  V4_131545_119                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @���5�I�1   @���� @4ݲ-V�c����F1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @���A,��AvffA�33A�ffA�B��B ffB8ffBJ  B^ffBo33B�ffB���B�  B���B�33B���B�33BǙ�Bљ�Bٙ�B䙚B���B�  C� C  CL�C��CffCffC L�C%ffC)�3C.L�C3�C9� C=�fCB��CH�3CQ��C\33Cf� Cp��Cz�fC��fC��fC�@ C��3C��3C��C�  C�ٚC�ٚC�&fC�� C�33C�  C���C��C�&fC�L�C��3C�s3C�ffC�s3C�ffC�@ C�  C�ffD3D  D  D  D�D,�D   D%,�D)�fD/&fD3��D93D>@ DC,�DH  DM3DQ��DW  D\  Da9�Df@ Dj�3Dp�Du&fDz3D�L�D�� D��3D�  D�VfD��fD��fD�  D�S3D��3D��fD���D�S3D�|�D���D�fD�L�D홚D��3D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�  A.ffAx  A�  A�33A�ffB33B ��B8��BJffB^��Bo��B���B�  B�33B�  B�ffB�  B�ffB���B���B���B���B�  B�33C��C�CffC�fC� C� C ffC%� C)��C.ffC333C9��C>  CB�3CH��CQ�fC\L�Cf��Cp�3C{  C��3C��3C�L�C�� C�  C�&fC��C��fC��fC�33C���C�@ C��C�ٚC�&fC�33C�Y�C�  C܀ C�s3C� C�s3C�L�C��C�s3D�D&fD&fD&fD  D33D fD%33D)��D/,�D4  D9�D>FfDC33DH&fDM�DR  DW&fD\&fDa@ DfFfDj��Dp3Du,�Dz�D�P D��3D��fD�3D�Y�D���D�ٚD�3D�VfD��fD�ɚD���D�VfDԀ D�� D�	�D�P D��D��fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�t�A�hA�G�A��`A��A�z�A�oA�1'A�+A�jA���A��`A��`A�?}A�dZA�Q�A��A�VA���A���A�%A�M�A�9XA��^A���A��A��uA��A��;A���A�K�A�hsA��TA���A�9XA��A�l�A�bAv^5Ar�Ae�A^�AY�AK�FABn�A=C�A9�#A4 �A.��A$��A E�AdZA�A�A�^A�;A�HA`BA �`@���@��`@�b@�V@��@��@�I�@�Ĝ@Χ�@ʇ+@�@��h@��!@�"�@��@��@���@�K�@�S�@��\@��@��@��@�Z@�O�@�;d@�@�t�@�7L@�33@��@��@�@�ƨ@��`@�~�@�~�@}�T@tz�@ko@`��@W��@N��@I��@B~�@<I�@4�j@/��@+�
@&ȴ@#33@�P@S�@��@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�t�A�hA�G�A��`A��A�z�A�oA�1'A�+A�jA���A��`A��`A�?}A�dZA�Q�A��A�VA���A���A�%A�M�A�9XA��^A���A��A��uA��A��;A���A�K�A�hsA��TA���A�9XA��A�l�A�bAv^5Ar�Ae�A^�AY�AK�FABn�A=C�A9�#A4 �A.��A$��A E�AdZA�A�A�^A�;A�HA`BA �`@���@��`@�b@�V@��@��@�I�@�Ĝ@Χ�@ʇ+@�@��h@��!@�"�@��@��@���@�K�@�S�@��\@��@��@��@�Z@�O�@�;d@�@�t�@�7L@�33@��@��@�@�ƨ@��`@�~�@�~�@}�T@tz�@ko@`��@W��@N��@I��@B~�@<I�@4�j@/��@+�
@&ȴ@#33@�P@S�@��@��@�-31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B(�B!�B�BhBJB$�B�B��B�jB�B�;B�B�B%B�B!�B'�B/B49B?}BN�B`BBaHBA�B;dB)�BuB��B�^B~�B�B@�B%�BB
��B
�VB
>wB
�B	��B	�-B	p�B	Q�B	8RB		7B�B�5B�B�}B��B� By�Bq�Bn�BjBl�BhsBo�BcTBffBe`BffBw�Bz�B�B�%B�DB��B�9B��B�B��B	 �B	.B	?}B	XB	o�B	�B	��B	�B	�?B	�}B	ŢB	�#B	�HB	�`B	�B	�B	��B
B
+B
	7B
PB
\B
bB
\B
{B
�B
&�B
/B
9XB
?}B
F�B
M�B
Q�B
W
B
^5B
bNB
ffB
k�B
n�B
r�B
v�B
{�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�QB7B�B�B�B
�VB�BIB�B��B�1B��B�B�zB��B	B&B�B�B%B2�BA�BQ BSB2aB,=B]B�B�B��Bt�BBB3hBsB
��B
�B
��B
0oB
�B	��B	��B	b�B	CaB	,"B�BߊB�vB��B�[B��Bq�Bk�Bc�B_VB\]B]~BZkBaBU2BX_BW$BWsBiBk�Bs3Bw2B|PB��B�`B��B��B�B	NB	�B	0;B	H�B	`BB	t�B	��B	�~B	��B	��B	�B	ˬB	ѷB	��B	�B	�&B	�qB	�|B	�fB	��B	��B	��B
 �B	��B
�B
B
YB
�B
)�B
/�B
6�B
>(B
B'B
GEB
NpB
R�B
V�B
[�B
^�B
b�B
gB
l=B
oOB
uZ31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<z�u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.015(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201908210015302019082100153020190821001530202207232057082022072320570820220723205708202207261126322022072611263220220726112632  JA  ARFMdecpV4_b                                                                20190807065204  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190807065205  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190807065205  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190807065206  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190807065206  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190807065206  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190807065626                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190810155209  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190810155242  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190810155242  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190810155243  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190810155243  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190810155243  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190810155243                      G�O�G�O�G�O�                JA  ARUP                                                                        20190810155523                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190811000000  CF  PSAL_ADJUSTED_QC?   ?   G�O�                JM  ARSQJMQC2.0                                                                 20190811000000  CF  TEMP_ADJUSTED_QC?   ?   G�O�                JM  ARCAJMQC2.0                                                                 20190820151530  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190820151530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115708  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022632  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051506                      G�O�G�O�G�O�                