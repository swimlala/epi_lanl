CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2011-09-02T16:10:26Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:52:40Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20110902161026  20161129232527  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA  P7_97922_009                    2C  D   PROVOR                          09027                           5815A03                         841 @���A���1   @����
=�@1�l�C���c�1&�x�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�33AffAnffA�ffA�  A�33BffBffB0��BC��B[33Bm33B33B���B���B�ffB���B�ffB���B���B�33B���B�ffB�  B�  C�fC��CffCffC� C��C L�C$ffC)�fC.33C3�3C9L�C>�fCC�3CI�CR��C\33Cf��Cp�3Cz��C�Y�C��C��C�33C��C�33C�� C��fC�ٚC�&fC�ffC�33C�Y�C�33C�33C�@ C�33C�@ C�ffC�@ C�Y�C�ٚC�  C�� C��3D�3D  D�3D�fD�D,�D ,�D%&fD*  D.��D4&fD93D>fDB��DH  DL��DR�DWfD\�Da&fDf9�Dk  Do��Du�DzfD�C3D�� D�ɚD�fD�C3D�y�D��3D���D�33D�s3D��3D�fD�@ Dԙ�D�� D���D�S3D�3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >L��@�  A��Al��A���A�33A�ffB  B  B0ffBC33BZ��Bl��B~��B�ffB�ffB�33B�ffB�33B�ffBș�B�  Bݙ�B�33B���B���C��C� CL�CL�CffC�3C 33C$L�C)��C.�C3��C933C>��CC��CI  CR� C\�Cf�3Cp��Cz� C�L�C�  C�  C�&fC��C�&fC��3C�ٚC���C��C�Y�C�&fC�L�C�&fC�&fC�33C�&fC�33C�Y�C�33C�L�C���C��3C��3C��fD��D��D��D� DfD&fD &fD%  D*�D.�fD4  D9�D>  DB�3DH�DL�fDRfDW  D\fDa  Df33Dk�Do�3Du3Dz  D�@ D���D��fD�3D�@ D�vfD�� D��fD�0 D�p D�� D�3D�<�DԖfD���D���D�P D� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AҸRAҕ�Aқ�Aҗ�A�dZA�^5A�ZA�{Aк^AЉ7A�-Aϕ�AάA͍PA���A���A�;dA���A�I�AǺ^A�|�A��A��TA�A��-A��A�t�A�VA�
=A�S�A��A�;dA��hA��A�dZA���A�^5A���A���A�XA��;A�O�A�9XA��/A�ƨA��hAs�Ahr�AX��AP�!AD9XA:~�A5��A0jA+"�A&�A�A��Ar�AA�A
�jA5?@��@���@�33@�\)@��@䛦@��@�ȴ@�@�K�@�A�@��@��`@�33@��R@��@���@�(�@�ƨ@�-@��@��
@���@��@���@��@��@�ƨ@��@�M�@���@�O�@�9X@��+@��@|��@p �@g|�@]��@T�D@M��@E�@>ff@7;d@0��@)��@%O�@ �9@O�@�u@�j@b2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AҸRAҕ�Aқ�Aҗ�A�dZA�^5A�ZA�{Aк^AЉ7A�-Aϕ�AάA͍PA���A���A�;dA���A�I�AǺ^A�|�A��A��TA�A��-A��A�t�A�VA�
=A�S�A��A�;dA��hA��A�dZA���A�^5A���A���A�XA��;A�O�A�9XA��/A�ƨA��hAs�Ahr�AX��AP�!AD9XA:~�A5��A0jA+"�A&�A�A��Ar�AA�A
�jA5?@��@���@�33@�\)@��@䛦@��@�ȴ@�@�K�@�A�@��@��`@�33@��R@��@���@�(�@�ƨ@�-@��@��
@���@��@���@��@��@�ƨ@��@�M�@���@�O�@�9X@��+@��@|��@p �@g|�@]��@T�D@M��@E�@>ff@7;d@0��@)��@%O�@ �9@O�@�u@�j@b2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�B�B��B��B��B��B�bB�VB�DB�%B�+B�PB�bB�7B�uB�uB��B��B��B��B�)BB�BQ�Bw�B��B��B�RBǮB��B��B�XB�FB��B�uB�BcTBW
B9XB�;B�1BJ�B
��B
6FB	�XB	`BB	"�B	B�B��B�B��B	  B	"�B�B��B�-B��B��B��B�B��B�hB��B�XBƨB�;B�B��B	uB	<jB	ZB	&�B	C�B	v�B	�B	��B	��B	��B	�?B	�qB	ǮB	��B	�)B	�;B	�`B	�B	��B	��B
B
B
%B

=B
hB
�B
 �B
&�B
-B
5?B
<jB
=qB
B�B
H�B
N�B
S�B
YB
`BB
dZB
hsB
k�B
o�B
s�B
x�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�B�B�B�B��B�B��B��B�bB�VB�DB�%B�+B�PB�bB�7B�uB�uB��B�B��B��B�)BB�BQ�Bw�B��B��B�RBǮB��B��B�XB�`B��B�uB�-BcTBW
B9rB�;B�KBJ�B
��B
6FB	�rB	`BB	"�B	3B��B�.B��B��B	 B	"�B�B�B�aB��B��B�,B�IB��B��B�B�rB��B�VB��B��B	�B	<�B	Z7B	'B	C�B	v�B	� B	��B	��B	�B	�ZB	��B	ǮB	� B	�)B	�;B	�zB	�B	��B	�B
3B
-B
?B

XB
�B
�B
 �B
&�B
-)B
5ZB
<�B
=�B
B�B
H�B
N�B
TB
Y1B
`BB
dtB
h�B
k�B
o�B
s�B
x�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281650252013072816502520130728165025201608161348502016081613485020160816134850JA  ARFMdecpP7_d                                                                20110902161021  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20110902161026  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20110902161028  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20110902161032  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20110902161032  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20110902161033  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20110902161033  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20110902161033  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20110902162434                      G�O�G�O�G�O�                JA  ARFMdecpP7_d                                                                20110905010754  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20110905011506  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20110905011507  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20110905011512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20110905011512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20110905011512  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20110905011512  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20110905011512  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20110905012243                      G�O�G�O�G�O�                JA  ARFMdecpP7_e                                                                20111021064658  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111021064918  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111021064919  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111021064923  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111021064923  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111021064924  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111021064924  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20111021064924  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20111021065126                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075025  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816044850  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232527                      G�O�G�O�G�O�                