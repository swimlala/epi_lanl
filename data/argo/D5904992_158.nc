CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-26T06:52:42Z creation;2020-02-29T21:53:07Z conversion to V3.1;2020-12-25T04:15:31Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20200226065242  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_158                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @��}X^ 1   @�Ɯ� @;��7Kƨ�c�5?|�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@���A$��A{33A���A͙�A�B  B  B6  BD  B^ffBq��B���B�ffB���B���B�33B���B�  Bș�B���B�  B晚B�B�33C�fC  C  C��CL�C��C �3C%33C*  C.��C3��C9��C>ffCC33CHL�CQ�3C[�3CfL�Cp��Cz�C�@ C�ٚC�Y�C��C�Y�C�Y�C��C��C�L�C��C�ٚC���C�@ C��C�Y�C��C�&fC�&fC��C��3C�&fC��C��fC��C�ٚD  DfD�D��D��D&fD 9�D%@ D*&fD/&fD4&fD9  D>fDB��DH  DL��DR33DW,�D[�fDa33De�3Dj�3Dp&fDu�Dz�D�FfD���D��3D�	�D�I�D���D��fD�3D�C3D��3D�ɚD��3D�L�D�|�D���D���D�FfD��D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@�ffA#33Ay��A���A���A���B��B��B5��BC��B^  Bq33B�ffB�33B���B���B�  B���B���B�ffBҙ�B���B�ffB�ffB�  C��C�fC�fC�3C33C� C ��C%�C)�fC.�3C3�3C9� C>L�CC�CH33CQ��C[��Cf33Cp� Cz  C�33C���C�L�C�  C�L�C�L�C��C��C�@ C��C���C�� C�33C�  C�L�C�  C��C��C�  C��fC��C��C�ٚC� C���D��D  DfD�fD�3D  D 33D%9�D*  D/  D4  D9�D>  DB�3DH�DL�3DR,�DW&fD[� Da,�De��Dj��Dp  Du3DzfD�C3D��fD�� D�fD�FfD��fD��3D� D�@ D�� D��fD�� D�I�D�y�D�ɚD���D�C3D퉚D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��`A���A���A�x�A�S�A�9XA�$�A�A���A�1'A���A��yA���A�`BA�=qA���A���A���A�%A��/A��FA��FA��;A�K�A�-A�p�A�33A���A��A�7LA���A��A��Az=qAxr�Au��ArVAm�7Ai&�Ae33A_��AX�AQ��AJ��AI+ABA�A?�mA9�;A7�A3�A0�A,�A*��A&��A%
=A!t�AhsA%A�A��AXA�\A�!AS�A�9@���@��j@�M�@�%@���@�M�@Ϯ@�1@�7L@��^@�`B@�9X@��w@��P@���@�`B@�?}@���@��#@�
=@�r�@�G�@�Q�@|9X@xĜ@u�@r-@nV@mV@j~�@d1@]@U?}@N5?@HbN@C��@<��@4�@0r�@+@%�@ �9@�@�@�D@b@O�@	�#@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��`A���A���A�x�A�S�A�9XA�$�A�A���A�1'A���A��yA���A�`BA�=qA���A���A���A�%A��/A��FA��FA��;A�K�A�-A�p�A�33A���A��A�7LA���A��A��Az=qAxr�Au��ArVAm�7Ai&�Ae33A_��AX�AQ��AJ��AI+ABA�A?�mA9�;A7�A3�A0�A,�A*��A&��A%
=A!t�AhsA%A�A��AXA�\A�!AS�A�9@���@��j@�M�@�%@���@�M�@Ϯ@�1@�7L@��^@�`B@�9X@��w@��P@���@�`B@�?}@���@��#@�
=@�r�@�G�@�Q�@|9X@xĜ@u�@r-@nV@mV@j~�@d1@]@U?}@N5?@HbN@C��@<��@4�@0r�@+@%�@ �9@�@�@�D@b@O�@	�#@��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�=B�bB�bB�bB�hB�bB�hB�hB�hB�uB�uB��B��B��B��B�{B�oB��B��B�PB�B��BÖB��Bv�BXBC�B�B
��B
��B
�^B
��B
y�B
ffB
@�B
33B
�B
B	�HB	ǮB	��B	�+B	_;B	7LB	�B	PB�B�TB��BŢB�LB��B��B��B�DB�Bv�Bk�BffBffB^5BW
BL�B@�B5?B)�B �B�B\BDB1B+B	7BJB�B0!BD�BT�Bk�B~�B�bB��B�dB��B�B	B	�B	49B	<jB	Q�B	`BB	p�B	}�B	�PB	�oB	��B	�RB	��B	�B	��B
DB
{B
"�B
1'B
9XB
B�B
I�B
Q�B
W
B
_;B
cTB
jB
n�B
s�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�=B�bB�bB�bB�hB�bB��B��B��B��B�uB��B��B��B��B�{B�oB��B��B�jB�B��BðB��Bv�BXBC�B�B
��B
�B
�xB
��B
y�B
ffB
@�B
33B
�B
B	�HB	��B	��B	�+B	_;B	7LB	�B	jB��B�nB��BŢB�fB�B��B��B�^B�Bv�Bk�BffBffB^OBW
BL�B@�B5?B*B �B�BvB^B1BEB	7BJB�B0!BD�BUBk�B~�B�}B��B�dB��B�B	 B	�B	4TB	<jB	RB	`BB	p�B	~B	�PB	�oB	��B	�RB	��B	�B	�B
^B
{B
"�B
1'B
9XB
B�B
I�B
RB
W$B
_VB
cTB
j�B
n�B
s�B
w�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003120015272020031200152720200312001527202003130012182020031300121820200313001218JA  ARFMdecpV4_b                                                                20200226065241  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200226065242  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200226065242  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200226065243  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200226065243  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200226065243  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200226065436                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200229215229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200229215305  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200229215305  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200229215306  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200229215306  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200229215306  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200229215307                      G�O�G�O�G�O�                JA  ARUP                                                                        20200229215354                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200301000000  CF  PSAL_ADJUSTED_QC>���>���G�O�                JM  ARSQJMQC2.0                                                                 20200301000000  CF  TEMP_ADJUSTED_QC>���>���G�O�                JM  ARCAJMQC2.0                                                                 20200311151527  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200311151527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200312151218  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                