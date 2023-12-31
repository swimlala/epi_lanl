CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-06-11T03:52:18Z creation;2019-06-14T18:53:43Z conversion to V3.1;2019-09-10T08:17:08Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20190611035218  20190920001517  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_132                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�ļI2q 1   @�����| @;t9XbN�cpě��T1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�  A,��A���A�ffA���A�  B��B$  B733BI��B`ffBpffB���B�ffB�  B�33B���B�33B���B�ffB���B���B�ffBB���C�C�fC�C  C�C�fC L�C%ffC*33C/��C4L�C9��C=��CCffCG��CQ��C[��CfffCoL�Cz� C��3C��C��C�ٚC�  C��C��fC��C�ٚC��3C�s3C�s3C��C�Y�C��C�  Cѳ3C��C���C�L�C�33C��fC�L�C�&fC�L�D&fD  D  D&fD9�D�3D 33D%  D)�3D.��D4  D9&fD>  DC,�DG�3DL�3DR,�DV�3D\fDa3Df  Dk@ Dp�Dt��Dz33D�C3D��3D���D��D�L�D���D��3D���D�<�D�y�D��3D�fD�C3DԆfD�� D��3D�,�D� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�ff@�33A.ffA�ffA�33Aə�A���B  B$ffB7��BJ  B`��Bp��B�  B���B�33B�ffB���B�ffB�  Bș�B�  B�  B晚B���B�  C33C  C33C�C33C  C ffC%� C*L�C/�3C4ffC9�fC=�fCC� CG�3CQ�fC[�3Cf� CoffCz��C�� C�&fC��C��fC��C�&fC��3C��C��fC�  C�� C�� C�&fC�ffC�&fC��C�� C��C�ٚC�Y�C�@ C��3C�Y�C�33C�Y�D,�D&fDfD,�D@ D��D 9�D%fD)��D.�3D4&fD9,�D>&fDC33DG��DL��DR33DV��D\�Da�Df&fDkFfDp3Du  Dz9�D�FfD��fD�� D� D�P D���D��fD���D�@ D�|�D��fD�	�D�FfDԉ�D��3D��fD�0 D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʣ�A�\)A�JA�A���A��!A�I�A�{A��A�E�A��;A��PA��9A��;A�A��PA��#A���A�E�A�ffA�1'A��A�dZA���A��
A��uA�1A��wA�dZA���A�l�A��A��
A�M�A�K�A�p�A~Awp�AtM�Aq�Ao�mAhJA^�/AZ�AQ��AOt�ALn�AH5?AC�hA?/A:��A7ƨA3�^A0��A,��A)�A$��A"�A �`A$�AI�A��A�9A`BA�HA/A
�+A�#A|�@��H@�7L@�@���@Ϯ@ă@���@���@�{@�r�@��F@�33@�ff@�1@��9@�=q@�9X@��h@�v�@�t�@���@}�T@y�^@v{@sdZ@pbN@m�h@g��@aG�@Y�^@S"�@Lj@F�@AX@;@65?@1�7@,�@&��@ Ĝ@/@A�@"�@�@o@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʣ�A�\)A�JA�A���A��!A�I�A�{A��A�E�A��;A��PA��9A��;A�A��PA��#A���A�E�A�ffA�1'A��A�dZA���A��
A��uA�1A��wA�dZA���A�l�A��A��
A�M�A�K�A�p�A~Awp�AtM�Aq�Ao�mAhJA^�/AZ�AQ��AOt�ALn�AH5?AC�hA?/A:��A7ƨA3�^A0��A,��A)�A$��A"�A �`A$�AI�A��A�9A`BA�HA/A
�+A�#A|�@��H@�7L@�@���@Ϯ@ă@���@���@�{@�r�@��F@�33@�ff@�1@��9@�=q@�9X@��h@�v�@�t�@���@}�T@y�^@v{@sdZ@pbN@m�h@g��@aG�@Y�^@S"�@Lj@F�@AX@;@65?@1�7@,�@&��@ Ĝ@/@A�@"�@�@o@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BC�BB�B6FB��B�B�`B�)B�B��BǮBB�XB��B��B�7B|�Bu�Bo�BgmB[#BE�B9XB-B!�BVBB�TBĜB��B�DBF�B\B
��B
��B
��B
q�B
;dB
'�B
\B
  B	��B	�hB	s�B	>wB	1'B	$�B	�B	VB	  B�B�TB��B�XB��B�uB�hB�JB�B{�Bt�BcTB_;BYBL�BH�BA�B7LB$�B�B�BDBBB
=B�B)�B;dBN�BcTBu�B��B��B�wB��B�/B�B	
=B	�B	2-B	D�B	aHB	t�B	� B	�JB	��B	�9B	��B	�mB	��B

=B
�B
"�B
-B
49B
:^B
B�B
I�B
Q�B
VB
[#B
bNB
hsB
m�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�eBC�BB�B6+B��B�B�`B�)B��B��BǮB�uB�>B��B��B�7B|�Bu�Bo�BgmB[#BE�B9>B-B!�B<BB�TBāB��B�)BF�BBB
��B
��B
�sB
q�B
;dB
'�B
BB
  B	�oB	�NB	s�B	>wB	1'B	$�B	�B	VB��B�B�:B��B�>B��B�[B�NB�JB�B{�Bt�BcTB_;BYBL�BH�BAoB72B$�B�B�BDBB�B
#B�B)�B;JBN�Bc:Bu�B��B��B�]B��B�B�B	
#B	�B	2B	D�B	a-B	t�B	�B	�0B	�B	�9B	��B	�RB	��B

=B
sB
"�B
,�B
49B
:^B
B�B
I�B
Q�B
VB
[#B
bNB
hsB
m�B
q�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906250015502019062500155020190625001550201906260012532019062600125320190626001253JA  ARFMdecpV4_b                                                                20190611035217  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190611035218  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190611035218  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190611035219  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190611035219  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190611035220  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190611035623                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190614185219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190614185340  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190614185341  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190614185342  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190614185342  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190614185342  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190614185343                      G�O�G�O�G�O�                JA  ARUP                                                                        20190614185733                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190615000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20190624151550  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190624151550  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190625151253  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001517                      G�O�G�O�G�O�                