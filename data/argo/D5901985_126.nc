CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2014-11-15T00:54:21Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:38:37Z update;     
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
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    O�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    R�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  U�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    V    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    V   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    V   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    V   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  V   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    VP   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    V`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Vd   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Vt   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         Vx   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        V|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    V�Argo profile    3.1 1.2 19500101000000  20141115005421  20161129234515  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA  P7_97922_126                    2C  D   PROVOR                          09027                           5815A03                         841 @�#<C���1   @�#<�)� @5�dZ��dP�/��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�ffA#33Aq��A���Aə�A�  B
��BffB6  BH��Bb  BrffB�33B���B���B�ffB�  B���B�33B�  B�  Bܙ�B晚B���B�  CffC��C��C�3C��C�C ��C%33C*33C/ffC3ffC8��C>33CBffCG��CQ�fC\��Cf�fCpffCz��C�L�C�Y�C�  C�&fC��C�@ C�  C��C��3C���C���C�L�C�ffC�ffCȀ C�33C�L�C�@ C��3C��C�L�C�@ C��3C�@ C��3D�D  D��D�D3D3D�fD%3D)�fD/fD43D99�D>33DC9�DH�DM,�DRfDW�D\�DafDf3Dk9�Dp@ Du�DzfD�@ D���D�� D�fD�\�D��fD��fD�  D�P D��3D��3D� D�@ Dԙ�D�ɚD�fD�L�D�3D��3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�ffA#33Aq��A���Aə�A�  B
��BffB6  BH��Bb  BrffB�33B���B���B�ffB�  B���B�33B�  B�  Bܙ�B晚B���B�  CffC��C��C�3C��C�C ��C%33C*33C/ffC3ffC8��C>33CBffCG��CQ�fC\��Cf�fCpffCz��C�L�C�Y�C�  C�&fC��C�@ C�  C��C��3C���C���C�L�C�ffC�ffCȀ C�33C�L�C�@ C��3C��C�L�C�@ C��3C�@ C��3D�D  D��D�D3D3D�fD%3D)�fD/fD43D99�D>33DC9�DH�DM,�DRfDW�D\�DafDf3Dk9�Dp@ Du�DzfD�@ D���D�� D�fD�\�D��fD��fD�  D�P D��3D��3D� D�@ Dԙ�D�ɚD�fD�L�D�3D��3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��TA���A�AڶFAڥ�Aڟ�AڋDA�r�A�K�Aש�A�=qA�33A���AÇ+A��A��!A�K�A�C�A���A��A�;dA��PA��uA��wA�A�A��+A��A�/A���A�n�A���A���A���A���A��A��hA�G�A��A��hA��+Ay�#As�7AkoAb��AZ(�AR1'AJZA>�9A5�FA0�9A*Q�A$AK�A�mA�9A��A
��A��A�@�|�@�z�@�`B@��-@���@�@�ƨ@��@�V@թ�@�X@��@��y@þw@���@�S�@��\@��@�"�@��R@�@�`B@���@�K�@���@�
=@�-@��@�ff@���@��@�O�@���@���@�9X@�E�@|��@u��@n��@c��@X��@P �@G�@A�#@;"�@4�@-�T@( �@#o@j@�w@��@�;@�D@"�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��TA���A�AڶFAڥ�Aڟ�AڋDA�r�A�K�Aש�A�=qA�33A���AÇ+A��A��!A�K�A�C�A���A��A�;dA��PA��uA��wA�A�A��+A��A�/A���A�n�A���A���A���A���A��A��hA�G�A��A��hA��+Ay�#As�7AkoAb��AZ(�AR1'AJZA>�9A5�FA0�9A*Q�A$AK�A�mA�9A��A
��A��A�@�|�@�z�@�`B@��-@���@�@�ƨ@��@�V@թ�@�X@��@��y@þw@���@�S�@��\@��@�"�@��R@�@�`B@���@�K�@���@�
=@�-@��@�ff@���@��@�O�@���@���@�9X@�E�@|��@u��@n��@c��@X��@P �@G�@A�#@;"�@4�@-�T@( �@#o@j@�w@��@�;@�D@"�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BjB|�B|�B|�Bz�By�Bx�Bw�Bw�Bx�Bx�B�%BN�B��BÖB|�B<jB;dBt�BA�BN�BS�BS�BO�BC�B0!B�B�BɺB�3B��B�PBl�B9XB�ZB�{Bw�BL�BC�B(�B
�TB
�B
;dB
JB	�jB	�=B	`BB	.B	#�B�;BB�FB��B��B�{B�Bx�Bq�Bp�Bp�Bn�Bk�BiyBdZBffBl�Bq�B�+B��B��B�9B�)B�B��B	JB	bB	�B	7LB	A�B	Q�B	iyB	{�B	�{B	��B	�'B	�XB	��B	��B	ȴB	�}B	ƨB	�B	�B	�B	�B	��B
PB
�B
�B
$�B
.B
5?B
6FB
:^B
C�B
K�B
R�B
VB
\)B
aHB
ffB
iyB
jB
n�B
s�B
u�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B|�B|�B|�Bz�By�Bx�Bw�Bw�Bx�Bx�B�%BN�B��BÖB|�B<jB;dBt�BA�BN�BS�BS�BO�BC�B0!B�B�BɺB�3B��B�PBl�B9XB�ZB�{Bw�BL�BC�B(�B
�TB
�B
;dB
JB	�jB	�=B	`BB	.B	#�B�;BB�FB��B��B�{B�Bx�Bq�Bp�Bp�Bn�Bk�BiyBdZBffBl�Bq�B�+B��B��B�9B�)B�B��B	JB	bB	�B	7LB	A�B	Q�B	iyB	{�B	�{B	��B	�'B	�XB	��B	��B	ȴB	�}B	ƨB	�B	�B	�B	�B	��B
PB
�B
�B
$�B
.B
5?B
6FB
:^B
C�B
K�B
R�B
VB
\)B
aHB
ffB
iyB
jB
n�B
s�B
u�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJ=PRES; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ=PSAL; PSAL_ADJ_ERR=0.01(PSS-78)                                                                                                                                                                                                                        None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608161411232016081614112320160816141123  JA  ARFMdecpP7_h                                                                20141115005400  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20141115005421  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20141115005422  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20141115005427  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20141115005427  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20141115005427  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20141115005428  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20141115005428  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20141115012954                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20141117185336  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20141117190358  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20141117190400  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20141117190404  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20141117190404  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20141117190405  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20141117190405  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20141117190405  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20141117193345                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004514                      G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816051123  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234515                      G�O�G�O�G�O�                