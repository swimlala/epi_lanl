CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-01-22T06:53:06Z creation;2017-01-25T18:59:19Z conversion to V3.1;2019-09-10T08:29:09Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20170122065306  20190919231517  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  V4_131538_045                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @��;3�a�1   @��=�i�@:�(�\�c@j~��#1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @�33A$��Ap  A���A�33A�  BffB��B5��BI��B_��Bn��B�  B�  B�  B���B���B���B�  B�33Bљ�B�33B���B�ffB�  C�3C��C33C�3C  C� C ��C%�fC)�fC/�3C4��C9  C>  CC��CG��CQ�fC[�3Cf� Cp��Cz� C�&fC�&fC��fC��fC�� C�33C�ffC���C�ffC��C��3C�&fC�Y�C�@ C��fC�s3C�  C��C�&fC��C�L�C�  C��C�@ C�L�D��D��D3DfD�D  D 3D%�D)�3D.�3D4&fD8��D>&fDC  DH,�DM  DR  DW  D\3D`�fDf�Dk  Do�3Du3Dz&fD�\�D�|�D��fD�fD�9�D�|�D��fD��3D�\�D��3D�� D�fD�C3DԐ D�ɚD��D�I�D�|�D�ٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@�  A#33AnffA���A�ffA�33B  B33B533BI33B_33BnffB���B���B���B���B�ffB�ffB���B�  B�ffB�  B晚B�33B���C��C�3C�C��C�fCffC �3C%��C)��C/��C4�3C8�fC=�fCC� CG� CQ��C[��CfffCp�3CzffC��C��C���C�ٚC��3C�&fC�Y�C�� C�Y�C��C��fC��C�L�C�33C�ٚC�ffC��3C��C��C��C�@ C��3C� C�33C�@ D�3D�fD�D  DfD�D �D%3D)��D.��D4  D8�3D>  DB��DH&fDM�DR�DW�D\�D`� Df3Dj��Do��Du�Dz  D�Y�D�y�D��3D�3D�6fD�y�D��3D�� D�Y�D�� D���D�3D�@ DԌ�D��fD�	�D�FfD�y�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�$�A�"�A�&�A�&�A�(�A�&�A�$�A�VA�{A�bA�A���A���A���A�A��wA�|�A�ffA�"�A��A��/A��!A�l�A�~�A�VA�dZA���A�Q�A��+A��A�$�A��A��yA���A�ȴA��A�{A�/A�bA�bNA���A��A��FA��A���A}��Atr�Ak�;AfQ�A`�uA[��AX~�AP�HALn�AH��AD�/AAS�A;hsA6M�A1ƨA.�`A,r�A*E�A �A��A��A�;AXAp�AffA	��AK�@�G�@�@�{@�hs@�`B@��F@�1'@�A�@��`@�V@�C�@���@�S�@���@��P@�V@��7@��m@��@���@�o@�?}@�@���@u�@n��@c�F@\9X@Up�@N5?@Gl�@@�9@:�@5��@0b@*~�@$j@�@��@�+@�@�@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�$�A�"�A�&�A�&�A�(�A�&�A�$�A�VA�{A�bA�A���A���A���A�A��wA�|�A�ffA�"�A��A��/A��!A�l�A�~�A�VA�dZA���A�Q�A��+A��A�$�A��A��yA���A�ȴA��A�{A�/A�bA�bNA���A��A��FA��A���A}��Atr�Ak�;AfQ�A`�uA[��AX~�AP�HALn�AH��AD�/AAS�A;hsA6M�A1ƨA.�`A,r�A*E�A �A��A��A�;AXAp�AffA	��AK�@�G�@�@�{@�hs@�`B@��F@�1'@�A�@��`@�V@�C�@���@�S�@���@��P@�V@��7@��m@��@���@�o@�?}@�@���@u�@n��@c�F@\9X@Up�@N5?@Gl�@@�9@:�@5��@0b@*~�@$j@�@��@�+@�@�@S�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��BBB+BPB\B\BDBB
=B�B�B��B�)B�JBgmBL�B9XB$�B�B��B�ZB��B�^B��B�7BM�B  B
�TB
�B
ZB
PB	��B	��B	�B	gmB	O�B	 �B	PB	  B�B�)B�XB��B�VB�bB�hB�%BiyB\)BQ�BH�BA�B>wB6FB/B'�B�B�B�B"�B5?B@�BL�BaHBo�Bz�B�+B��B��B�5B�B��B	hB	 �B	9XB	<jB	G�B	VB	y�B	�DB	�-B	ƨB	�HB	�B	��B
%B
bB
�B
'�B
49B
;dB
C�B
H�B
Q�B
VB
_;B
bNB
gmB
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B��B�B�B�B��B��B��B��B��BBB+BjBvB\BDB B
=B�B�B��B�CB�JBg�BL�B9XB$�B�B��B�tB��B�^B��B�7BM�B  B
�nB
�"B
Z7B
jB	��B	��B	�B	g�B	O�B	 �B	jB	  B�B�)B�XB��B�VB�bB��B�?Bi�B\)BRBH�BA�B>�B6FB/5B(
B�B�B�B"�B5?B@�BL�BaHBo�Bz�B�+B��B��B�5B�B�B	�B	 �B	9rB	<jB	G�B	VB	y�B	�^B	�-B	��B	�bB	�B	��B
%B
bB
�B
(
B
49B
;B
C�B
H�B
Q�B
VB
_;B
bhB
gmB
m�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=-0.1(dbar)                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702050016392017020500163920170205001639201804031232042018040312320420180403123204JA  ARFMdecpV4_b                                                                20170122065305  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170122065306  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170122065306  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170122065307  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170122065307  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170122065307  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20170122070106                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20170125185239  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170125185917  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170125185918  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170125185919  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170125185919  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170125185919  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170125185919                      G�O�G�O�G�O�                JA  ARUP                                                                        20170125190704                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170126000000  CF  PSAL_ADJUSTED_QC?   ?   G�O�                JM  ARSQJMQC2.0                                                                 20170126000000  CF  TEMP_ADJUSTED_QC?   ?   G�O�                JM  ARCAJMQC2.0                                                                 20170204151639  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170204151639  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033204  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919231517                      G�O�G�O�G�O�                