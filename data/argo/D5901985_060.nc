CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-01-23T21:59:50Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:46:36Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130123215950  20161129234510  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA  P7_97922_060                    2C  D   PROVOR                          09027                           5815A03                         841 @�~9s��1   @�~:*�6 @3��Q��dj~��#1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�33A��Ac33A�ffA�ffA�ffB  B��B4  BFffBX  BjffB���B���B���B�  B���B�ffB�  Bř�B���Bڙ�B�33B�33B���C L�C�C
L�C�fC��C��C�3C#33C(�fC-L�C3  C733C;��CA��CG33CQ33C[��Ce� Co� Cy� C��3C�  C�L�C�&fC�L�C�33C�@ C�@ C���C�ffC�ffC�33C�ٚC�@ C�@ C�L�C��3C��C��3C�ٚC��fC��C��3C��fC�ٚD@ D�DfD  D3D��D�fD%�D)� D.�fD4�D8��D>fDCFfDH&fDMfDQ��DW,�D[��D`�3Df  Dj�fDo�3Dt�fDz@ D�C3D���D��3D�fD�P D��fD���D��D�I�D�� D�� D�	�D�Y�DԀ D��3D�  D�9�D�3D�� D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>L��@�  A  Aa��A���A���A陚B��BffB3��BF  BW��Bj  B���B�ffB���B���B�ffB�33B���B�ffBΙ�B�ffB�  B�  B���C 33C  C
33C��C�3C� C��C#�C(��C-33C2�fC7�C;�3CA�3CG�CQ�C[�3CeffCoffCyffC��fC��3C�@ C��C�@ C�&fC�33C�33C�� C�Y�C�Y�C�&fC���C�33C�33C�@ C��fC�  C��fC���C�ٚC��C��fC�ٚC���D9�DfD  D��D�D�3D� D%fD)ٚD.� D4fD8�fD>  DC@ DH  DM  DQ�3DW&fD[�3D`��Df�Dj� Do��Dt� Dz9�D�@ D��fD�� D�3D�L�D��3D�ɚD��D�FfD���D���D�fD�VfD�|�D�� D���D�6fD� D���D�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�O�A�A�A�C�A�;dA�9XA�9XA�9XA�=qA�=qA�?}A�;dA�7LA�33A���A��;A�I�A�jA�(�A���A�hsA�+A�A�A�JA�v�A��FA�C�A�M�A�S�A�l�A�jA�ȴA���A���A� �A��!A��/A��-A��yA�5?A�G�A��wAx��Arr�Al��Ab��AXZAR�AK�7AH^5ADĜA=��A6n�A1�7A+��A&^5A"�!AhsA�AA�Al�A��AoA��A	�Ab@�r�@�@�@�o@���@ԋD@�bN@ǅ@�$�@���@�hs@�|�@�O�@�X@��+@���@��w@��D@�x�@�V@�Z@�@�+@��P@���@�\)@�?}@�o@��@���@�^5@|1@u�-@mp�@h1'@_+@X�@P �@G��@?�;@9�7@2�@,9X@$��@ ��@z�@�u@�@\)@�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�O�A�A�A�C�A�;dA�9XA�9XA�9XA�=qA�=qA�?}A�;dA�7LA�33A���A��;A�I�A�jA�(�A���A�hsA�+A�A�A�JA�v�A��FA�C�A�M�A�S�A�l�A�jA�ȴA���A���A� �A��!A��/A��-A��yA�5?A�G�A��wAx��Arr�Al��Ab��AXZAR�AK�7AH^5ADĜA=��A6n�A1�7A+��A&^5A"�!AhsA�AA�Al�A��AoA��A	�Ab@�r�@�@�@�o@���@ԋD@�bN@ǅ@�$�@���@�hs@�|�@�O�@�X@��+@���@��w@��D@�x�@�V@�Z@�@�+@��P@���@�\)@�?}@�o@��@���@�^5@|1@u�-@mp�@h1'@_+@X�@P �@G��@?�;@9�7@2�@,9X@$��@ ��@z�@�u@�@\)@�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bn�Br�Br�Br�Br�Br�Bs�Bs�Br�Bs�Bq�Br�Br�Bo�BcTB�B1B%B��B��B�B�B�B�B�B�B�B�)B��B�dB��B�PB�B`BBA�B6FB�B�BZB�B
��B
VB
�B	��B	ƨB	�hB	\)B	G�B	(�B	�B	\B�B�B��B�?B��B��B�PB�DB�DB�bB��B��B�'B�9B�\B�oB��B��B��B��B�BǮB��B�B�yB��B	�B	0!B	=qB	S�B	r�B	�B	��B	�!B	�}B	ɺB	��B	�5B	�yB	�B	��B	��B
B
B
%B
PB
�B
�B
"�B
(�B
.B
33B
8RB
?}B
F�B
K�B
R�B
ZB
`BB
dZB
hsB
m�B
q�B
w�B
y�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�Br�Br�Br�Br�Bs�Bs�Br�Bs�Bq�Br�Br�Bo�BcnB�B1B%B��B��B�B�B�B�B�B��B�B�CB��B�dB��B�jB�9B`\BA�B6FB�B�BZB�B
��B
VB
�B	��B	��B	��B	\CB	G�B	)B	�B	vB��B�B��B�ZB��B��B��B�^B�xB�}B��B��B�[B�TB��B��B��B��B��B��B�"B��B�B�B�B��B	�B	0;B	=�B	TB	r�B	�9B	��B	�;B	��B	��B	�B	�OB	�B	�B	��B	��B
B
3B
?B
PB
�B
�B
"�B
(�B
./B
33B
8RB
?�B
F�B
K�B
SB
Z7B
`\B
dZB
hsB
m�B
q�B
w�B
y�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.2(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281652592013072816525920130728165259201608161358162016081613581620160816135816JA  ARFMdecpP7_g                                                                20130123215947  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130123215950  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130123215951  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130123215956  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130123215956  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130123215957  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130123215957  CF  PSAL            >���@�33?�                  JA  ARGQpump1.0                                                                 20130123215957  CF  TEMP            >���@�33?�                  JA  ARUP                                                                        20130123221257                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20130126155959  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130126160709  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130126160710  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130126160715  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130126160715  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130126160715  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130126160716  CF  PSAL            >���@�33?�                  JA  ARGQpump1.0                                                                 20130126160716  CF  TEMP            >���@�33?�                  JA  ARUP                                                                        20130126161838                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002511                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075259  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075259  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045816  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234510                      G�O�G�O�G�O�                