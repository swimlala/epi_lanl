CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-09-10T21:59:55Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:43:46Z update;     
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
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    S   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  _   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130910215955  20161129234511  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               SA   JA  P7_97922_083                    2C  D   PROVOR                          09027                           5815A03                         841 @ַ��z�1   @ַ�*z @6b�\(���c��\)1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?L��@�ffA.ffAnffA���A���A�B��B��B4ffBI33B\ffBn  B���B���B���B���B�  B���B���B�  B�33B���B�33B���B���C��C�3C��C�C��C�C L�C%��C*�fC/� C4ffC9�3C>�CC��CH  CR�C\�Cf33Co�fCyffC��3C��fC��3C�� C���C��C�ٚC�� C�@ C��C��C�  C�ٚC�@ C��C�� C��C�33C��fC���C�ٚC�33C�ffC�ffC�@ D�D@ D3D&fD  D��D��D%fD)�fD.ٚD433D9&fD>&fDB��DH3DM3DR�DV�3D\  D`�3De��Dj��Do�fDt�3Dz&fD�Y�D�� D�� D�  D�I�D��3D�ٚD�3D�9�D�� D��fD���D�C3DԠ D���D�fD�S3D�|�D�fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?L��@�ffA.ffAnffA���A���A�B��B��B4ffBI33B\ffBn  B���B���B���B���B�  B���B���B�  B�33B���B�33B���B���C��C�3C��C�C��C�C L�C%��C*�fC/� C4ffC9�3C>�CC��CH  CR�C\�Cf33Co�fCyffC��3C��fC��3C�� C���C��C�ٚC�� C�@ C��C��C�  C�ٚC�@ C��C�� C��C�33C��fC���C�ٚC�33C�ffC�ffC�@ D�D@ D3D&fD  D��D��D%fD)�fD.ٚD433D9&fD>&fDB��DH3DM3DR�DV�3D\  D`�3De��Dj��Do�fDt�3Dz&fD�Y�D�� D�� D�  D�I�D��3D�ٚD�3D�9�D�� D��fD���D�C3DԠ D���D�fD�S3D�|�D�fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��AոRAպ^A�~�A�x�A�jA�bNA�A�Aѡ�A��A���A���A���A��A�G�A�~�A�dZA�JA�|�A�r�A�jA�n�A�/A�`BA�ZA�M�A���A�?}A�O�A���A�=qA��jA�;dA�ffA� �A�~�A�A�oA�-A���A���A��uA�p�A�ƨA�^5AzI�Ar�Ak`BAb�!A`jAW��AMO�AB��A9hsA5l�A0VA)/A!A��A��A%A�AG�A�mA��@��@�+@畁@��@�
=@�Z@��@�=q@��T@�Q�@��u@�"�@���@�\)@�5?@�j@�J@�@��@��+@�dZ@�33@�Q�@��P@�Ĝ@��R@�33@�1'@�n�@��@|1@s�
@j��@d��@^ff@V{@N�R@D9X@=�T@8bN@4�@.{@)&�@#C�@��@�
@�H@�@j@
~�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��AոRAպ^A�~�A�x�A�jA�bNA�A�Aѡ�A��A���A���A���A��A�G�A�~�A�dZA�JA�|�A�r�A�jA�n�A�/A�`BA�ZA�M�A���A�?}A�O�A���A�=qA��jA�;dA�ffA� �A�~�A�A�oA�-A���A���A��uA�p�A�ƨA�^5AzI�Ar�Ak`BAb�!A`jAW��AMO�AB��A9hsA5l�A0VA)/A!A��A��A%A�AG�A�mA��@��@�+@畁@��@�
=@�Z@��@�=q@��T@�Q�@��u@�"�@���@�\)@�5?@�j@�J@�@��@��+@�dZ@�33@�Q�@��P@�Ĝ@��R@�33@�1'@�n�@��@|1@s�
@j��@d��@^ff@V{@N�R@D9X@=�T@8bN@4�@.{@)&�@#C�@��@�
@�H@�@j@
~�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�;B�fB�mB�fB�`B�ZB�ZB�HB��B��B��B�PBz�Bu�Bt�Br�Bx�Bw�Bv�Bs�Bt�Br�Bq�Bx�BbNBbNBaHB]/BR�BP�BJ�BE�B=qBbB��BĜB{�BVB,B�B�`B��BdZB
�5B
�\B
_;B
 �B	�ZB	�XB	�7B	w�B	D�B	�B�`BǮB�wB�B��B�{B�DB�7B�B�%B~�B�B�B�B�hB�PB�7B�+B��B�PB~�BgmB�+B��BŢB�B	8RB	C�B	cTB	v�B	�B	��B	��B	�B	�^B	ǮB	��B	�B	�/B	�TB	�B	�B	��B
B
1B
{B
�B
 �B
(�B
0!B
;dB
B�B
I�B
L�B
S�B
XB
]/B
_;B
_;B
k�B
r�B
w�B
z�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B�fB�mB�fB�`B�ZB�ZB�HB��B��B��B�PBz�Bu�Bt�Br�Bx�Bw�Bv�Bs�Bt�Br�Bq�Bx�BbNBbNBaHB]/BR�BP�BJ�BE�B=qBbB��BĜB{�BVB,B�B�`B��BdZB
�5B
�\B
_;B
 �B	�ZB	�XB	�7B	w�B	D�B	�B�`BǮB�wB�B��B�{B�DB�7B�B�%B~�B�B�B�B�hB�PB�7B�+B��B�PB~�BgmB�+B��BŢB�B	8RB	C�B	cTB	v�B	�B	��B	��B	�B	�^B	ǮB	��B	�B	�/B	�TB	�B	�B	��B
B
1B
{B
�B
 �B
(�B
0;B
;dB
B�B
I�B
L�B
S�B
XB
]/B
_;B
_;B
kkB
r�B
w�B
z�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201309240703562013092407035620130924070356201608161402482016081614024820160816140248JA  ARFMdecpP7_h                                                                20130910215936  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130910215955  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130910215956  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130910220001  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130910220001  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130910220002  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130910220002  CF  PSAL            ?L��?L��?�                  JA  ARGQpump1.0                                                                 20130910220002  CF  TEMP            ?L��?L��?�                  JA  ARUP                                                                        20130910221626                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20130913185758  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130913190645  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130913190646  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130913190651  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130913190651  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130913190651  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130913190651  CF  PSAL            ?L��?L��?�                  JA  ARGQpump1.0                                                                 20130913190651  CF  TEMP            ?L��?L��?�                  JA  ARUP                                                                        20130913192040                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004508                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130923220356  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130923220356  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050248  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234511                      G�O�G�O�G�O�                