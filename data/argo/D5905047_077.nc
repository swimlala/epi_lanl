CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-06-16T18:53:13Z creation;2018-06-16T18:53:15Z conversion to V3.1;2019-09-10T08:54:58Z update;2022-07-26T02:46:53Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Et   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  GD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  KX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    iD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  iH   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20180616185313  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               MA   JA  V4_131545_077                   2C  Ddi�ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�i��ƻ�1   @�jU�� @3T��E��di�^5?}1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?333@�ffA&ffAy��A�33A�  A�ffB33B   B2��BE��B\��Bs��B�33B�33B���B�ffB���B���B�ffB���B�  B�  B�33B���B�  C  CffC��C�3CL�C��C L�C$� C*� C/�C3��C9��C>L�CCL�CHffCRL�C\33Cf33Cp�3Cz33C�ffC��fC�@ C���C��fC��C�33C�s3C��3C��C�L�C�s3C��fC��C�s3C�&fC�33C��C�33C�ٚC噚C�@ C��C��3C�� D  D  D�D�D�3D&fD� D$�3D)��D/&fD3��D933D>�DB��DH�DL�fDQ��DW  D\  D`� De� DkfDp&fDu  Dz  D�FfD�� D���D�3D�VfD�|�D��fD���D�0 D�|�D���D��3D�\�DԐ D���D�fD�33D�y�D�� D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�33A$��Ax  A�ffA�33A���B��B��B2ffBE33B\ffBs33B�  B�  B�ffB�33B���B���B�33Bə�B���B���B�  B�B���C�fCL�C�3C��C33C� C 33C$ffC*ffC/  C3�3C9� C>33CC33CHL�CR33C\�Cf�Cp��Cz�C�Y�C�ٚC�33C�� C�ٚC�  C�&fC�ffC��fC��C�@ C�ffC�ٚC�  C�ffC��C�&fC��C�&fC���C��C�33C��C��fC��3D��D��DfDfD��D  DٚD$��D)�3D/  D3�3D9,�D>3DB�3DH3DL� DQ�fDW�D\�D`��De��Dk  Dp  Dt��Dz�D�C3D�|�D�ɚD�  D�S3D�y�D��3D��fD�,�D�y�D�ɚD�� D�Y�DԌ�D�ɚD�3D�0 D�vfD���D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�9XA�p�A��A�JA���A�^5A�1'A���A�n�A�ZA�A�A��yA�/A�l�A���A̧�A��A�?}A�S�A���A�{A��A�ZA��HA��;A�I�A��DA��uA��#A��A�?}A��hA��A� �A��jA��A�~�A��A���A�ƨA�G�A�r�A�5?A���A�$�A~��An�jA^��AL^5AA��A<bA6��A2�/A.  A*�/A%��A �jA�A��A��A-A
~�A�A�A j@�
=@���@�!@���@�+@�V@�I�@Ǿw@��@��j@�$�@��j@�t�@���@� �@�\)@�V@�t�@�?}@���@�|�@���@��@�O�@�ƨ@�~�@� �@�G�@��7@�ȴ@�(�@��P@}��@tI�@k"�@b~�@Y&�@O��@I�#@B�@<�@5p�@/�;@*�@%�-@ b@dZ@E�@��@��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�9XA�p�A��A�JA���A�^5A�1'A���A�n�A�ZA�A�A��yA�/A�l�A���A̧�A��A�?}A�S�A���A�{A��A�ZA��HA��;A�I�A��DA��uA��#A��A�?}A��hA��A� �A��jA��A�~�A��A���A�ƨA�G�A�r�A�5?A���A�$�A~��An�jA^��AL^5AA��A<bA6��A2�/A.  A*�/A%��A �jA�A��A��A-A
~�A�A�A j@�
=@���@�!@���@�+@�V@�I�@Ǿw@��@��j@�$�@��j@�t�@���@� �@�\)@�V@�t�@�?}@���@�|�@���@��@�O�@�ƨ@�~�@� �@�G�@��7@�ȴ@�(�@��P@}��@tI�@k"�@b~�@Y&�@O��@I�#@B�@<�@5p�@/�;@*�@%�-@ b@dZ@E�@��@��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ǮB	�#B	�#B	�#B	�)B	�TB	�HB	�B	��B	��B	��B	�B	�B	�B
"�B
`BB
�qBhB(�BXB��B��B�XB�qB�RB��B+BB��B�B��B��B	7B�B�/B�^B��Bq�BL�B7LB!�B
�B
�RB
:^B
!�B
B	�oB	.B�TBB�dB�}B�wBB�FB��B��B��B��B��B��B�%B�PB��B��B��B��B�LB�XB�qB�RB�B�XBɺB��B	!�B	�B	#�B	=qB	aHB	�1B	��B	��B	�!B	ÖB	��B	�B	�BB	�B	��B	��B
B
%B
	7B
hB
�B
 �B
&�B
,B
1'B
8RB
>wB
F�B
I�B
O�B
S�B
ZB
_;B
cTB
iyB
n�B
r�B
v�B
{�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ȴB	یB	�=B	�=B	��B	�nB	�B	چB	�2B	�B	�MB	ںB	�6B	��B
$�B
c B
��BoB+B[#B��B�
B��B�4B��B��B�B�B��B��B��B��BDB��B��B�B�Bt�BN�B8�B#�B
�/B
��B
;�B
"�B
�B	��B	2�B�B�MB��B��B��BÖB��B�eB�8B�8B��B��B��B��B�"B�B�!B��B�6B�B��B��B��B�B�*B��B��B	"�B	B	$&B	=�B	abB	�KB	��B	�8B	��B	��B	�B	�kB	��B	��B	��B	�B
AB
tB
	lB
�B
�B
 �B
'B
,WB
1[B
8�B
>�B
F�B
I�B
O�B
TB
Z7B
_pB
c�B
i�B
n�B
r�B
v�B
|B
�B
�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.3(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806270015452018062700154520180627001545202207232056302022072320563020220723205630202207261123442022072611234420220726112344  JA  ARFMdecpV4_b                                                                20180616185149  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180616185313  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180616185314  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180616185314  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180616185315  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180616185315  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180616185315                      G�O�G�O�G�O�                JA  ARUP                                                                        20180616185540                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180617000000  CF  PSAL_ADJUSTED_QC?333?333G�O�                JM  ARCAJMQC2.0                                                                 20180626151545  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180626151545  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180627151458  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115630  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                