CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-29T06:53:51Z creation;2020-05-02T15:53:56Z conversion to V3.1;2022-11-10T04:20:21Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200429065351  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA  V4_131533_070                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�{= 1   @��VH @.����m�dĬ1&�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       =���@���A��Ax  A���A�33A�  B	��B��B0��BJffB^  Bp��B�ffB�ffB�  B���B�ffB�ffB�  B���B�ffB���B�ffB�  B���C��CffC�3C  C  C� C ffC%� C*�3C/��C3��C9� C=�fCBL�CHffCR�3C[��Ce�fCp  Cz  C���C��C�ٚC�ٚC�@ C�33C�Y�C�ٚC�ffC��fC��3C�@ C���C�Y�C��C�  C��3C�ٚC�L�C��C��fC��3C��3C��C�ffD��DfD��D�fD  D  D�3D$�3D)��D/�D4  D9,�D=��DC  DH,�DM,�DR  DV��D\  Da  Df@ Dk,�Dp�DtٚDy��D�FfD�s3D���D�3D�L�D���D��3D�3D�Y�D�� D���D�3D�VfDԙ�D�ٚD�fD�33D��D�ٚD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>L��@�  A33Ay��A���A�  A���B
  B  B133BJ��B^ffBq33B���B���B�33B�  B���B���B�33B�  Bә�B�  B晚B�33B���C�3C� C��C�C�C��C � C%��C*��C/�fC3�fC9��C>  CBffCH� CR��C[�fCf  Cp�Cz�C���C��C��fC��fC�L�C�@ C�ffC��fC�s3C��3C�  C�L�C�ٚC�ffC�&fC��C�  C��fC�Y�C��C��3C�  C�  C�&fC�s3D�3D�D  D��D&fD&fDٚD$��D*  D/3D4&fD933D>  DCfDH33DM33DR&fDW  D\&fDafDfFfDk33Dp  Dt� Dy�3D�I�D�vfD�� D�fD�P D�� D��fD�fD�\�D��3D�� D�fD�Y�DԜ�D���D��D�6fD� D���D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aթ�A�x�A�r�A�~�A�C�A�-A�1'A�/A�/A�-A�(�A�&�A��`A��A�+Aщ7A�x�A��A͟�A�5?A�XA��yA���A���A�JA���A�%A�VA�l�A�|�A��A�|�A�VA���A�E�A�VA��`A���A���Ayx�Am�mAY7LAI��ACO�A=�7A:�A2jA"��A~�A1'A��AC�A�mA�^A{A�;Av�A	|�AhsAt�A�A�^A �`@�-@���@�@�j@�&�@�ȴ@���@�1@Ӆ@��T@У�@��y@��@°!@�J@�+@��u@��@�o@��/@��D@�-@�@�A�@�5?@�l�@�l�@�\)@��@�V@�1'@��R@�&�@�Z@�-@v{@kC�@a�@YX@Q��@HbN@=�-@6��@0bN@*M�@#dZ@(�@�u@�h@�\@\)@1@�`31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aթ�A�x�A�r�A�~�A�C�A�-A�1'A�/A�/A�-A�(�A�&�A��`A��A�+Aщ7A�x�A��A͟�A�5?A�XA��yA���A���A�JA���A�%A�VA�l�A�|�A��A�|�A�VA���A�E�A�VA��`A���A���Ayx�Am�mAY7LAI��ACO�A=�7A:�A2jA"��A~�A1'A��AC�A�mA�^A{A�;Av�A	|�AhsAt�A�A�^A �`@�-@���@�@�j@�&�@�ȴ@���@�1@Ӆ@��T@У�@��y@��@°!@�J@�+@��u@��@�o@��/@��D@�-@�@�A�@�5?@�l�@�l�@�\)@��@�V@�1'@��R@�&�@�Z@�-@v{@kC�@a�@YX@Q��@HbN@=�-@6��@0bN@*M�@#dZ@(�@�u@�h@�\@\)@1@�`31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111By�B�B�B�+B�B�B�%B�B�B�B�%B�B�1B�{B�RBɺB�B	JB	#�B	w�B	�LB
r�B
�uB
��B
�/B
��B
�mB
�/B
�TB�B
��B
�BB
��B
ĜB
�PB
o�B
;dB
B	�
B	�FB	l�B	�B�`B��B��BŢB��B��B�)B	B	33B	~�B	��B	�^B	�wB	�?B	�qB	�RB	��B	��B	�B	��B	�-B	�B	�{B	u�B	k�B	w�B	�%B	��B	�B	��B	�qB	B	��B	��B	��B	��B	�B	�)B	�;B	�yB	��B
B
B
+B
PB
VB
bB
�B
�B
�B
�B
 �B
"�B
$�B
,B
49B
9XB
>wB
B�B
F�B
I�B
Q�B
YB
_;B
ffB
jB
o�B
u�B
y�B
|�B
� B
�B
�%B
�=33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BzB�B�B�_B�9B�B�%B�B�B�B�%B�mB�7B��B��B��B�B	�B	'8B	y�B	��B
r�B
��B
�B
�B iB
��B
�\B
�B�B
��B
�`B
҉B
�#B
�B
t�B
@�B

�B	�qB	��B	q�B	B�B�aBбBǔB�B�B��B	[B	3�B	HB	��B	�JB	�.B	��B	�(B	��B	��B	��B	�qB	�B	��B	�/B	�9B	v�B	lB	xB	�YB	��B	��B	�eB	��B	��B	��B	��B	�NB	�B	�EB	�]B	�VB	�B	�6B
'B
-B
_B
jB
�B
�B
�B
�B
�B
�B
 �B
"�B
%B
,=B
4nB
9�B
>�B
B�B
F�B
I�B
R B
Y1B
_;B
ffB
j�B
o�B
u�B
y�B
|�B
� B
�'B
�%B
�=33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.3(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005130016062020051300160620200513001606202210251312132022102513121320221025131213202210251807402022102518074020221025180740  JA  ARFMdecpV4_b                                                                20200429065350  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200429065351  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200429065351  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200429065352  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200429065352  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200429065352  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200429065454                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200502155239  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200502155354  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200502155354  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200502155355  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200502155355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200502155356  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200502155356                      G�O�G�O�G�O�                JA  ARUP                                                                        20200502155445                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200502151611  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20200502151611  QCF$                G�O�G�O�G�O�600000          JM  ARCAJMQC2.0                                                                 20200512151606  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200512151606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041213  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090740  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                