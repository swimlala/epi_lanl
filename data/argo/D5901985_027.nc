CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-02-29T10:07:28Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:50:32Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20120229100728  20161129232526  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_027                    2C  D   PROVOR                          09027                           5815A03                         841 @�+�Sp 1   @�+�x�� @2��Q��db��vȴ1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�  AffAi��A�33Ař�A�ffB
  B��B8  BK33B^ffBrffB�ffB�  B�  B���B���B�  B�  B�  Bҙ�B�33B�  B�ffB�33CffC��C�CL�C��C�C33C$��C*33C.��C4  C9��C>�fCC��CH�3CR�3C\��Ce�fCp  CzffC�@ C�  C��3C���C���C��fC��fC���C�� C�ٚC��3C��C���C�ffC�  C��C�@ C��C�L�C�33C�  C�33C��C�Y�C��fD��D��D3D33D3D�D �D%9�D*,�D/fD4�D8�3D>�DC  DG��DL�3DQٚDV��D[�3Da9�Df9�Dk33Dp9�DufDz�D�<�D�l�D��fD���D�C3D��3D�� D�3D�P D��3D�ٚD���D�Y�DԆfDڶfD��3D�I�D��D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?333@�33A  Ak33A�  A�ffA�33B
ffB33B8ffBK��B^��Br��B���B�33B�33B���B�  B�33B�33B�33B���B�ffB�33B�B�ffC� C�3C33CffC�fC33CL�C$�3C*L�C.�3C4�C9�fC?  CC�fCH��CR��C\�3Cf  Cp�Cz� C�L�C��C�  C��fC���C��3C��3C���C���C��fC�  C��C��fC�s3C��C��C�L�C�&fC�Y�C�@ C��C�@ C�&fC�ffC��3D�3D�3D�D9�D�D3D   D%@ D*33D/�D43D8��D>3DC&fDH  DL��DQ� DV�3D[��Da@ Df@ Dk9�Dp@ Du�Dz3D�@ D�p D�ɚD�  D�FfD��fD��3D�fD�S3D��fD���D�  D�\�Dԉ�Dڹ�D��fD�L�D� D��fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$�A��A��A��A��A��A�1A��A�33A�=qA�33AǾwA�bNA�&�A��A�A�AȋDA�~�A�VA��`Aǰ!A�I�A�  AƑhA�9XA��A���A��`A��A���A�dZA��DA��-A�+A�p�A���A�oA�M�A���A�x�A��A�x�A���A���A���A���Aw33Ag�A`{AXAR��AH�+A?�A9`BA2�+A,�`A&��A!7LA�;A~�A�!A;dA
�A�A�A ��@�S�@�!@�\@���@��@ם�@��T@ʗ�@�5?@�-@��@��!@�M�@��+@�\)@���@�r�@���@� �@���@�z�@�$�@��9@���@�-@�  @�p�@��@���@�{@���@w��@o�P@g�w@_|�@VV@N��@I�@D1@<�D@4��@.ff@)��@%p�@ �9@�j@��@V@�9@K�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$�A��A��A��A��A��A�1A��A�33A�=qA�33AǾwA�bNA�&�A��A�A�AȋDA�~�A�VA��`Aǰ!A�I�A�  AƑhA�9XA��A���A��`A��A���A�dZA��DA��-A�+A�p�A���A�oA�M�A���A�x�A��A�x�A���A���A���A���Aw33Ag�A`{AXAR��AH�+A?�A9`BA2�+A,�`A&��A!7LA�;A~�A�!A;dA
�A�A�A ��@�S�@�!@�\@���@��@ם�@��T@ʗ�@�5?@�-@��@��!@�M�@��+@�\)@���@�r�@���@� �@���@�z�@�$�@��9@���@�-@�  @�p�@��@���@�{@���@w��@o�P@g�w@_|�@VV@N��@I�@D1@<�D@4��@.ff@)��@%p�@ �9@�j@��@V@�9@K�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bs�B� B� B�+B�7B�JB��B��B�B�dB�wBÖBĜBǮB��B^5B�DB��B��B��B�B�}B�}B�jB�FB�^B��B�5B��B��BɺB�wB�XB��B�oB�Bw�B\)B49B2-B.BuB��BP�B
�B
�\B
'�B	��B	�B	�1B	ffB	B�B	\B�`B��B��B�B��B��B�B�1B�B�B�B|�B}�B�B|�B}�B�=B�bB�B��B�B	B	oB	$�B	>wB	VB	o�B	�%B	��B	�XB	ƨB	��B	�#B	�fB	�B	�B	��B	��B
B
B
1B
PB
VB
�B
�B
"�B
)�B
2-B
8RB
>wB
B�B
G�B
N�B
VB
\)B
_;B
cTB
hsB
k�B
o�B
r�B
v�B
x�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�B�B�+B�7B�JB��B��B��B�dB�wBÖBĜBǮBοB^5B�DB��B��B��B�B�}B�}B�PB�FB�^B��B�5B��B��BɺB�wB�XB��B�oB�Bw�B\)B49B2-B-�B[B��BP�B
�B
�\B
'�B	��B	�B	�1B	ffB	B�B	vB�`B��B��B�"B��B��B� B�1B�B�3B�'B}B~B�'B|�B~B�XB�bB�B��B�B	B	oB	$�B	>wB	VB	o�B	�%B	��B	�XB	ƨB	��B	�	B	�fB	�B	�B	��B	��B
B
B
1B
6B
VB
gB
�B
"�B
)�B
2-B
8RB
>wB
B�B
G�B
N�B
VB
\)B
_!B
cTB
hsB
k�B
o�B
r�B
v�B
x�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281651302013072816513020130728165130201608161352262016081613522620160816135226JA  ARFMdecpP7_g                                                                20120229100724  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120229100728  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120229100729  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120229100733  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120229100734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120229100734  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120229100734  CF  PSAL            ?��@�  ?�                  JA  ARGQpump1.0                                                                 20120229100734  CF  TEMP            ?��@�  ?�                  JA  ARUP                                                                        20120229102101                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120302160450  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120302161156  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120302161158  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120302161202  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120302161202  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120302161203  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120302161203  CF  PSAL            ?��@�  ?�                  JA  ARGQpump1.0                                                                 20120302161203  CF  TEMP            ?��@�  ?�                  JA  ARUP                                                                        20120302162108                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002513                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075130  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075130  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045226  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232526                      G�O�G�O�G�O�                