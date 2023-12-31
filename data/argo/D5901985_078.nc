CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-07-22T21:59:19Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:44:28Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20130722215919  20161129234514  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               NA   JA  P7_97922_078                    2C  D   PROVOR                          09027                           5815A03                         841 @֫:>2� 1   @֫=�� @7{"��`B�dM���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @���A$��AnffA�  Aə�A�  B
��B"  B4��BK33B^ffBpffB���B�33B�  B�  B�33B���B�ffB�33B�ffB���B�ffB�  B���CL�CL�C�C  C�3CffC   C$�3C*ffC/  C3�fC8��C=�fCB�fCH  CQ��C\��Ce33Co��Cy  C��3C��3C�  C��fC�33C�ٚC�ffC�Y�C��3C�33C��3C�&fC���C���C��fC��fC��fC��3C��3C��C�&fC�Y�C�&fC�&fC�Y�D�DfD�D&fD�fD� D��D%9�D*�D/  D4,�D99�D>,�DC  DH  DL�3DQ�fDW�D\@ Da�Df�Dk&fDp3Du  Dz�D�C3D��3D��3D�3D�L�D�� D��fD�  D�9�D���D��3D���D�VfDԆfD�ɚD��D�C3D퉚D��D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A&ffAp  A���A�ffA���B33B"ffB533BK��B^��Bp��B���B�ffB�33B�33B�ffB�  B���B�ffBҙ�B�  B噚B�33B�  CffCffC33C�C��C� C �C$��C*� C/�C4  C8�fC>  CC  CH�CQ�3C\�3CeL�Co�3Cy�C�  C�  C��C��3C�@ C��fC�s3C�ffC�  C�@ C�� C�33C��fC�ٚC��3C��3C��3C�  C�  C��C�33C�ffC�33C�33C�ffD  D�D3D,�D��D�fD�3D%@ D*  D/fD433D9@ D>33DCfDHfDL��DQ��DW3D\FfDa  Df  Dk,�Dp�Du&fDz3D�FfD��fD��fD�fD�P D��3D���D�3D�<�D�� D��fD�  D�Y�Dԉ�D���D� D�FfD��D�� D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AХ�AЗ�A�A�A�oA��mA���A���A�ƨAϲ-Aϣ�AρA�S�A��A�JA�{A�oA���A�oA�-A��mA���A��wA�jA���A�
=A���A��A��^A�bA�C�A���A�bA�oA��RA�=qA�A��+A�hsA��^A��;A��#A�C�A���A�ZA�ĜA��A��DA�=qA���A|VAs�#Ak��A]AR�AG��AChsA;�PA65?A2 �A-l�A)�A!�A1A(�Av�A�AQ�A�y@�ƨ@�x�@�r�@�A�@ݲ-@ѩ�@�b@��`@�j@��-@��T@�p�@�O�@��y@�;d@��;@��`@��!@�~�@�I�@�n�@�1@�1@��@���@��H@�v�@�G�@�Q�@|�/@r-@j�H@^�@WK�@Q�7@J~�@Bn�@<z�@7��@3ƨ@/�P@,(�@&@��@"�@�T@�!@`B22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AХ�AЗ�A�A�A�oA��mA���A���A�ƨAϲ-Aϣ�AρA�S�A��A�JA�{A�oA���A�oA�-A��mA���A��wA�jA���A�
=A���A��A��^A�bA�C�A���A�bA�oA��RA�=qA�A��+A�hsA��^A��;A��#A�C�A���A�ZA�ĜA��A��DA�=qA���A|VAs�#Ak��A]AR�AG��AChsA;�PA65?A2 �A-l�A)�A!�A1A(�Av�A�AQ�A�y@�ƨ@�x�@�r�@�A�@ݲ-@ѩ�@�b@��`@�j@��-@��T@�p�@�O�@��y@�;d@��;@��`@��!@�~�@�I�@�n�@�1@�1@��@���@��H@�v�@�G�@�Q�@|�/@r-@j�H@^�@WK�@Q�7@J~�@Bn�@<z�@7��@3ƨ@/�P@,(�@&@��@"�@�T@�!@`B22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B7LB;dB;dB;dB;dB;dB;dB;dB;dB9XB5?B%�BBm�BW
BhsBx�B�B�JB�oB��B��B��B��B��B�Bw�BaHBL�B|�Bw�BdZBZBN�B7LB'�B�BhB��B��B�B��B-B�B�\B?}B
�B
��B
u�B
8RB
B	B	|�B	:^B	1B��B�#B�}B�-B�B��B�DB{�Bk�BgmB_;B\)B_;BT�BS�BT�BgmBp�B�B�PB��B�?B�;B��B	�B	:^B	ZB	v�B	�B	��B	��B	��B	�-B	�dB	B	ŢB	��B	��B	�B	�BB	�ZB	�B
B
	7B
�B
oB
�B
"�B
,B
33B
:^B
>wB
D�B
E�B
K�B
M�B
W
B
]/B
e`B
iyB
k�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�B;JB;dB;dB;dB;dB;dB;dB9>B5?B%�BBm�BW
BhsBx�B�B�JB�oB��B��B��B��B�gB�Bw�BaHBL�B|�Bw�Bd@BZBN�B7LB'�BBhB��B��B�wB��B-B�B�\B?}B
�B
�B
u�B
88B
B	�uB	|�B	:DB	B��B�	B�}B�-B�B��B�)B{�Bk�BgmB_;B\B_!BT�BS�BT�BgmBp�B�B�6B��B�%B�!B��B	�B	:^B	ZB	v�B	��B	��B	��B	��B	�-B	�JB	�uB	ŢB	̳B	��B	�B	�'B	�@B	�B
 �B
	B
�B
TB
�B
"�B
,B
33B
:DB
>]B
D�B
E�B
K�B
M�B
W
B
]/B
eFB
i_B
kk44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201308050704162013080507041620130805070416201608161401402016081614014020160816140140JA  ARFMdecpP7_h                                                                20130722215859  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130722215919  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130722215920  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130722215924  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130722215925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130722215925  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130722215925  CF  PSAL            ?�  @���?�                  JA  ARGQpump1.0                                                                 20130722215925  CF  TEMP            ?�  @���?�                  JA  ARUP                                                                        20130722221211                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20130725155810  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20130725160820  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20130725160822  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20130725160826  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20130725160826  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20130725160827  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20130725160827  CF  PSAL            ?�  @���?�                  JA  ARGQpump1.0                                                                 20130725160827  CF  TEMP            ?�  @���?�                  JA  ARUP                                                                        20130725162434                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130804220416  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130804220416  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050140  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234514                      G�O�G�O�G�O�                