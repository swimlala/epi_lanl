CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-06-21T03:53:41Z creation;2017-06-24T19:02:12Z conversion to V3.1;2019-09-10T08:27:03Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Dh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20170621035341  20190919231515  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA  V4_131538_060                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @��l��1   @���F� @;�V��cl�`A�71   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�ffA0  Al��A���Aə�A�ffB
��B��B6��BLffB`  Bs��B�33B���B�  B���B���B�33B�  B�  B�  B�ffB�  B���B���C��C33C�3C�3C  CffC ��C%33C)��C.L�C4ffC9�3C>  CC33CH��CR� C\33Ce��Co�fCz� C��C�� C��3C���C��3C��fC��C�s3C�@ C��C�L�C�&fC�33C�&fC�33C��C�s3C��C�s3C�&fC�  C��fC��C���C�@ D�fD�D��D�D@ D,�D��D$�fD*  D/  D4  D9&fD>,�DC&fDG�fDM  DR  DW  D[��D`�fDf  Dk  Dp  Du�Dz&fD�P D�s3D��3D� D�\�D�� D��3D�fD�C3D���D���D��D�C3DԀ D�� D�� D�FfD��D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�ffA0  Al��A���Aə�A�ffB
��B��B6��BLffB`  Bs��B�33B���B�  B���B���B�33B�  B�  B�  B�ffB�  B���B���C��C33C�3C�3C  CffC ��C%33C)��C.L�C4ffC9�3C>  CC33CH��CR� C\33Ce��Co�fCz� C��C�� C��3C���C��3C��fC��C�s3C�@ C��C�L�C�&fC�33C�&fC�33C��C�s3C��C�s3C�&fC�  C��fC��C���C�@ D�fD�D��D�D@ D,�D��D$�fD*  D/  D4  D9&fD>,�DC&fDG�fDM  DR  DW  D[��D`�fDf  Dk  Dp  Du�Dz&fD�P D�s3D��3D� D�\�D�� D��3D�fD�C3D���D���D��D�C3DԀ D�� D�� D�FfD��D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�O�A��A�VAƓuAþwA�oA�XA��A���A�Q�A��FA���A�  A��A�7LA��A�(�A�G�A���A��;A�|�A��A��RA��FA�A�A�%A�A�A���A�{A��yA��A�A|��Ay�AwdZAu��Ap��Al��AjZAgK�Aa�A\�9AX9XAR1AL��AG"�AC��A?��A=%A9VA3�A/�7A-K�A)��A%�A"��A��A%A~�A�hA�RA��A
bA^5AVA ��@�V@��@�h@��@��D@�V@Ǖ�@�1@��@�  @�`B@��`@�5?@��h@��@��
@��@��@�K�@��9@�=q@�;d@�`B@�V@|(�@yx�@y%@u?}@sdZ@pĜ@b�H@^��@YX@Q�@LI�@F�@Bn�@<�/@7�@2��@,��@&�+@!�#@��@�`@�/@G�@@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�O�A��A�VAƓuAþwA�oA�XA��A���A�Q�A��FA���A�  A��A�7LA��A�(�A�G�A���A��;A�|�A��A��RA��FA�A�A�%A�A�A���A�{A��yA��A�A|��Ay�AwdZAu��Ap��Al��AjZAgK�Aa�A\�9AX9XAR1AL��AG"�AC��A?��A=%A9VA3�A/�7A-K�A)��A%�A"��A��A%A~�A�hA�RA��A
bA^5AVA ��@�V@��@�h@��@��D@�V@Ǖ�@�1@��@�  @�`B@��`@�5?@��h@��@��
@��@��@�K�@��9@�=q@�;d@�`B@�V@|(�@yx�@y%@u?}@sdZ@pĜ@b�H@^��@YX@Q�@LI�@F�@Bn�@<�/@7�@2��@,��@&�+@!�#@��@�`@�/@G�@@
M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B2-B5?B5?B)�B%�B�BVB�B	7B�B��B�7B�DBp�BK�B5?B�BB��B�5BƨB�3B�+B|�BZB8RB�B
��B
��B
��B
s�B
[#B
;dB
+B
�B	��B	�;B	��B	B	��B	s�B	aHB	?}B	�B�B�/B��BɺB�dB��B��B��B��B�VB�Bq�B\)BVBM�BE�B<jB0!B)�B%�B�BhBDBBB��BBoB�B/B<jBA�BH�B\)Bx�B�B��B��B�LB�
B�yB��B	oB	/B	B�B	T�B	bNB	w�B	�B	�\B	��B	��B	��B	�B
B
JB
uB
�B
'�B
0!B
8RB
<jB
B�B
K�B
R�B
W
B
]/B
bNB
gmB
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B2-B5?B5?B)�B%�B�BVB�B	7B�B��B�7B�DBp�BK�B5?B�BB��B�5BƨB�3B�+B|�BZB8RB�B
��B
��B
��B
s�B
[#B
;dB
+B
�B	��B	�;B	��B	B	��B	s�B	aHB	?}B	�B�B�/B��BɺB�dB��B��B��B��B�VB�Bq�B\)BVBM�BE�B<jB0!B)�B%�B�BhBDBB �B��BBoB�B/B<jBA�BH�B\)Bx�B�B��B��B�LB�
B�yB��B	�B	/B	B�B	T�B	bNB	w�B	�B	�\B	�B	��B	��B	�B
B
JB
uB
�B
(
B
0!B
8RB
<jB
B�B
K�B
R�B
W
B
]IB
bNB
gmB
k�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707050015372017070500153720170705001537201804031232492018040312324920180403123249JA  ARFMdecpV4_b                                                                20170621035340  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170621035341  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170621035341  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170621035342  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170621035342  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170621035342  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20170621040912                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20170624185216  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170624190210  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170624190211  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170624190212  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170624190212  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170624190212  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170624190212                      G�O�G�O�G�O�                JA  ARUP                                                                        20170624191300                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170625000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20170704151537  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170704151537  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033249  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919231515                      G�O�G�O�G�O�                