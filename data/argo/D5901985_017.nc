CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2011-11-21T10:09:28Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:51:43Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20111121100928  20161129232523  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_017                    2C  D   PROVOR                          09027                           5815A03                         841 @���8!�1   @���7�@3���
=q�dr��"��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�ffA$��Al��A���A�  A�ffBffB   B2ffBI33B[��BnffB�  B�  B�33B�  B���B�33B���B���B�  B�ffB�  B�  B���CL�C��C��C33C�3C��C �3C%33C*  C.��C4��C9ffC>ffCCL�CH33CQ� C\33Cf�Cp� C{33C�s3C�L�C��C�33C��C��C��C��C��3C��3C�  C��C�� C¦fC�33C̳3C�Y�C�L�C�L�C�Y�C��C�Y�C��C�ffC�33D�D��D  D�fD�D��D� D$ٚD*�D/@ D433D9�D>,�DB��DH&fDM&fDQ��DV� D\  D`�3De��Dk&fDp@ Du,�Dz&fD�9�D���D�� D�fD�Y�D�� D��3D� D�\�D�� D��3D��D�I�DԦfD�� D�	�D�C3D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?   @���A&ffAnffA���A���A�33B��B ffB2��BI��B\  Bn��B�33B�33B�ffB�33B���B�ffB�  B�  B�33Bۙ�B�33B�33B���CffC�fC�fCL�C��C�3C ��C%L�C*�C.�fC4�3C9� C>� CCffCHL�CQ��C\L�Cf33Cp��C{L�C�� C�Y�C��C�@ C��C�&fC��C��C�  C�  C��C�&fC���C³3C�@ C�� C�ffC�Y�C�Y�C�ffC�&fC�ffC�&fC�s3C�@ D3D  DfD��D  D  D�fD$� D*3D/FfD49�D9  D>33DC  DH,�DM,�DR  DV�fD\fD`��Df  Dk,�DpFfDu33Dz,�D�<�D���D��3D��D�\�D��3D��fD�3D�` D��3D��fD��D�L�Dԩ�D��3D��D�FfD��fD�ٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A֍PAցAցA�l�A�hsA�jA�p�A�p�A�n�A�v�A�v�AցA�|�A֏\A֑hA֗�A֝�A֣�A֣�A֩�A֩�A���A�/A��
AʁA�^5Aŉ7A�dZA�l�A� �A��A�ZA��yA���A��hA��^A���A�S�A��A��RA��A�(�A���A�p�A��A��AzbAnn�Ag�A`$�AV5?AIVA=`BA5S�A/VA'��A$�A ~�A�yA+A  A%A
1'A-@�-@�@�Ĝ@�@���@���@�/@�-@�
=@�t�@��@ź^@�l�@�t�@� �@��@��@���@���@��m@���@��P@���@�33@��@��y@�ƨ@��^@�S�@��@���@�j@���@�/@xr�@m/@c�
@[dZ@T��@L1@D��@>E�@8Ĝ@3�m@/
=@'�@$�/@ Q�@�@Q�@��2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A֍PAցAցA�l�A�hsA�jA�p�A�p�A�n�A�v�A�v�AցA�|�A֏\A֑hA֗�A֝�A֣�A֣�A֩�A֩�A���A�/A��
AʁA�^5Aŉ7A�dZA�l�A� �A��A�ZA��yA���A��hA��^A���A�S�A��A��RA��A�(�A���A�p�A��A��AzbAnn�Ag�A`$�AV5?AIVA=`BA5S�A/VA'��A$�A ~�A�yA+A  A%A
1'A-@�-@�@�Ĝ@�@���@���@�/@�-@�
=@�t�@��@ź^@�l�@�t�@� �@��@��@���@���@��m@���@��P@���@�33@��@��y@�ƨ@��^@�S�@��@���@�j@���@�/@xr�@m/@c�
@[dZ@T��@L1@D��@>E�@8Ĝ@3�m@/
=@'�@$�/@ Q�@�@Q�@��2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BuB�B�B�B�B�B�B�B�B�B�B�B�B �B�B �B#�B$�B#�B%�B+B9XB$�BXB�+B��B��B�B��B��B��B��B��B�B�B�
BɺB�FB�bBiyBH�B�B��B��B)�B
�B
7LB	�B	��B	�{B	ZB	�B�;B��B�}B�}B��B��B�)B�B��B�B��B�?B�RB�}BɺB�#B�
B�ZB	  B	 �B	>wB	H�B	B�B	Q�B	aHB	~�B	�oB	��B	��B	�jB	��B	ǮB	��B	�B	�5B	�`B	�B	�B	��B	��B	��B
B
+B

=B
uB
�B
�B
(�B
0!B
8RB
;dB
A�B
G�B
M�B
Q�B
W
B
ZB
`BB
cTB
hsB
k�B
n�B
q�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�B�BgB�B�B�B�BsB�B�BB�B�B �B�B �B#�B$�B#�B%�B+B9XB$�BXB�+B��B��B�B��B��B��B��B��B�B�B��BɺB�FB�bBiyBH�B�B��B��B)�B
�B
7LB	�B	��B	�{B	ZB	�B�;B��B�}B�}B��B��B�)B�B��B�B��B�ZB�lB�}BɺB�#B�
B�ZB	  B	 �B	>wB	H�B	B�B	Q�B	aHB	~�B	�oB	��B	��B	�jB	��B	ǮB	��B	�B	�5B	�`B	�kB	�B	��B	��B	��B
 �B
B

#B
uB
�B
�B
(�B
0!B
88B
;dB
AoB
G�B
M�B
Q�B
V�B
ZB
`'B
cTB
hXB
k�B
n�B
q�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281650582013072816505820130728165058201608161350352016081613503520160816135035JA  ARFMdecpP7_e                                                                20111121100925  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111121100928  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111121100929  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111121100934  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111121100934  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111121100934  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111121100934  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20111121100934  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20111121102206                      G�O�G�O�G�O�                JA  ARFMdecpP7_e                                                                20111123040631  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111123041317  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111123041318  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111123041322  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111123041323  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111123041323  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111123041323  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20111123041323  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20111123042211                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075058  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075058  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045035  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232523                      G�O�G�O�G�O�                