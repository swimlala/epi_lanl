CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2014-01-09T00:58:18Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:42:21Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20140109005818  20161129234510  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA  P7_97922_095                    2C  D   PROVOR                          09027                           5815A03                         841 @�ջ��?�1   @�ռB�s�@6���l�D�c���v�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�ff@���A+33A~ffA�33A͙�A�33BffB!33B4  BJffB[��Bp��B�33B�33B�33B���B�  B���B���B�  B���B�  B癚B���B���C�fC  C33C�3CL�C�C �fC&  C*L�C/��C433C9�C>L�CCffCH��CRffC\�3Cf�Cq�Cz��C�s3C�@ C�ffC�s3C��C�&fC�@ C�33C�ٚC��C��3C�� C�33C��3C�  C��fCҌ�C�L�C�33C�Y�C��3C�@ C��3C��C�&fD�D�D�fD3D,�D3D�3D%&fD)��D.� D4  D9&fD>&fDC�DG��DM&fDR,�DW@ D\3Da33Df,�Dk  Dp�Du  Dz�D�L�D��3D�ɚD�fD�L�D��fD��3D�  D�L�D�� D�� D��D�C3DԀ D��fD�&fD�VfD��D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�ffA)��A|��A�ffA���A�ffB  B ��B3��BJ  B[33BpffB�  B�  B�  B�ffB���B���B�ffB���Bҙ�B���B�ffB�B���C��C�fC�C��C33C  C ��C%�fC*33C/� C4�C9  C>33CCL�CH� CRL�C\��Cf  Cq  Cz�3C�ffC�33C�Y�C�ffC��C��C�33C�&fC���C�  C��fC��3C�&fC��fC��3C�ٚCҀ C�@ C�&fC�L�C��fC�33C��fC��C��DfD3D� D�D&fD�D��D%  D)�fD.ٚD3��D9  D>  DCfDG�fDM  DR&fDW9�D\�Da,�Df&fDk�DpfDt��DzfD�I�D�� D��fD�3D�I�D��3D�� D���D�I�D���D���D��D�@ D�|�D��3D�#3D�S3D퉚D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AÇ+A�z�A�~�A�t�A�t�A�r�A�p�A�r�A�t�A�t�A�v�A�z�A�x�A�|�A�|�A�z�A�z�A�^5A��TA��yA��A�{A��A���A��A�l�A���A�
=A���A��A�Q�A�;dA��A�A��A�^5A���A�M�A� �A���A��DA��A� �A~1An��Ae��A^v�AXbARQ�AIt�AC��A?�-A:�9A7��A4��A1+A-��A*�/A&��A"��A 5?A��A+A\)A&�AȴA b@�`B@���@��H@�7L@�Q�@�33@�(�@�-@��@�b@��@�l�@�J@���@�K�@��u@�Z@��H@��`@�ƨ@��-@��@��@��-@�&�@�
=@�hs@��R@���@|(�@t�@hĜ@_�@Y��@S�F@L9X@G�P@@�u@:M�@01'@*��@#S�@|�@�@�
@  @(�@�`@�+21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AÇ+A�z�A�~�A�t�A�t�A�r�A�p�A�r�A�t�A�t�A�v�A�z�A�x�A�|�A�|�A�z�A�z�A�^5A��TA��yA��A�{A��A���A��A�l�A���A�
=A���A��A�Q�A�;dA��A�A��A�^5A���A�M�A� �A���A��DA��A� �A~1An��Ae��A^v�AXbARQ�AIt�AC��A?�-A:�9A7��A4��A1+A-��A*�/A&��A"��A 5?A��A+A\)A&�AȴA b@�`B@���@��H@�7L@�Q�@�33@�(�@�-@��@�b@��@�l�@�J@���@�K�@��u@�Z@��H@��`@�ƨ@��-@��@��@��-@�&�@�
=@�hs@��R@���@|(�@t�@hĜ@_�@Y��@S�F@L9X@G�P@@�u@:M�@01'@*��@#S�@|�@�@�
@  @(�@�`@�+21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BĜBǮBǮBƨBǮBƨBǮBƨBǮBǮBǮBǮBǮBǮBǮBƨBƨB��B��BbNB?}B&�B�B+B��B�BB��B��B�PBcTBVBR�B,B1BBǮB��B�uB�%B�BR�B
��B
�oB
:^B	�5B	��B	z�B	YB	7LB	hB�B�B�B��BǮB�wB�FB��B��B��B�JBu�BjB`BBZBdZBm�Bq�Bp�Br�Bk�BdZBF�BL�B^5Bx�B��B�`B��B	B	!�B	  B	33B	ZB	T�B	~�B	�hB	��B	��B	�9B	�dB	��B	��B	�B	�BB	�yB	��B
	7B
PB
uB
!�B
(�B
33B
:^B
?}B
F�B
F�B
M�B
S�B
YB
^5B
bNB
gmB
k�B
p�B
t�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B��BǮBƨBǮBƨBǮBƨBǮBǮB��BǮB��BǮB��BƨB��B��B��BbNB?}B'B�BEB�B�BB��B��B�PBcnBVBR�B,B1BBǮB��B�uB�%B�BR�B
��B
�oB
:^B	�5B	��B	z�B	YB	7fB	hB��B�B�B��B��B�wB�`B�B��B��B�dBu�Bj�B`BBZ7BdZBm�Bq�Bp�Br�Bk�BdtBF�BL�B^5Bx�B��B�zB��B	3B	!�B	 B	33B	ZB	T�B	~�B	��B	��B	��B	�9B	�dB	��B	��B	�B	�BB	�B	��B
	RB
PB
uB
!�B
)B
33B
:xB
?}B
F�B
F�B
M�B
S�B
YB
^5B
bhB
g�B
k�B
p�B
t�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201401220704222014012207042220140122070422201608161405042016081614050420160816140504JA  ARFMdecpP7_h                                                                20140109005758  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140109005818  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140109005819  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140109005824  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140109005824  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140109005825  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20140109005825  CF  PSAL            ?�ff?�ff?�                  JA  ARGQpump1.0                                                                 20140109005825  CF  TEMP            ?�ff?�ff?�                  JA  ARUP                                                                        20140109012003                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20140111185932  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140111194112  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140111194114  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140111194119  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140111194119  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140111194120  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20140111194120  CF  PSAL            ?�ff?�ff?�                  JA  ARGQpump1.0                                                                 20140111194120  CF  TEMP            ?�ff?�ff?�                  JA  ARUP                                                                        20140111200614                      G�O�G�O�G�O�                JA  ARUP                                                                        20140111221527                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004508                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140121220422  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140121220422  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050504  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234510                      G�O�G�O�G�O�                