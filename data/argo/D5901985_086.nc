CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-10-10T21:59:10Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:43:25Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20131010215910  20161129234512  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               VA   JA  P7_97922_086                    2C  D   PROVOR                          09027                           5815A03                         841 @ֿ:Z�% 1   @ֿ;���@6e`A�7L�c�|�hs1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @�ffA#33Ai��A���A�  A�33BffB!33B5��BJffBX��Bt��B�33B�33B�  B�  B���B�ffB�ffB���B�33B�ffB�ffB���B�33CL�C  CL�C�C� C�fC�C$�3C*33C.33C3�3C9� C?�CD�CHL�CR��C\�fCf��Cp��Cz�3C�&fC�@ C�33C�  C��fC��fC�� C��3C��fC��fC�� C�@ C��C�s3C�&fC�L�C�&fC�  C�ٚC�L�C���C�fC�fC��3C���D� D�fD�3D�D�fD��D �D$ٚD)�fD/  D3�fD9fD=��DBٚDH3DM3DR3DV�3D[�fDa  De� Dj� DpfDt�3Dz  D�S3D���D��fD���D�,�D��fD��fD�  D�C3D���D�ٚD��D�Y�Dԓ3D��3D� D�L�D�3D���D�Y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @�ffA#33Ai��A���A�  A�33BffB!33B5��BJffBX��Bt��B�33B�33B�  B�  B���B�ffB�ffB���B�33B�ffB�ffB���B�33CL�C  CL�C�C� C�fC�C$�3C*33C.33C3�3C9� C?�CD�CHL�CR��C\�fCf��Cp��Cz�3C�&fC�@ C�33C�  C��fC��fC�� C��3C��fC��fC�� C�@ C��C�s3C�&fC�L�C�&fC�  C�ٚC�L�C���C�fC�fC��3C���D� D�fD�3D�D�fD��D �D$ٚD)�fD/  D3�fD9fD=��DBٚDH3DM3DR3DV�3D[�fDa  De� Dj� DpfDt�3Dz  D�S3D���D��fD���D�,�D��fD��fD�  D�C3D���D�ٚD��D�Y�Dԓ3D��3D� D�L�D�3D���D�Y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA��`A��
A���AռjA՗�A��A�p�A�dZA�S�A�A�A��A�-A��TA�oA�p�A��
A���A��RA���A���A�ĜA��jA��A��A�-A��uA��TA�+A���A�M�A�+A�1A�1A��A��;A�l�A��
A��A�XA�p�A��A��yA�t�A�A�A{��Aq�#Ah��A`v�AWl�AP��AL5?AH�ABffA7��A/��A*�9A%��A�-A��A�A&�A~�A��AG�A	��A�@�"�@�hs@�r�@�\@ޗ�@щ7@˶F@��m@���@� �@�|�@��@��9@�"�@�x�@�~�@��@�X@���@�t�@�C�@��@�7L@�V@���@�-@���@��-@��9@~��@t��@m�@eO�@Y��@R�H@N@D��@>�y@9�#@3@.E�@*n�@"�@{@�@I�@��@(�@
^521111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA��`A��
A���AռjA՗�A��A�p�A�dZA�S�A�A�A��A�-A��TA�oA�p�A��
A���A��RA���A���A�ĜA��jA��A��A�-A��uA��TA�+A���A�M�A�+A�1A�1A��A��;A�l�A��
A��A�XA�p�A��A��yA�t�A�A�A{��Aq�#Ah��A`v�AWl�AP��AL5?AH�ABffA7��A/��A*�9A%��A�-A��A�A&�A~�A��AG�A	��A�@�"�@�hs@�r�@�\@ޗ�@щ7@˶F@��m@���@� �@�|�@��@��9@�"�@�x�@�~�@��@�X@���@�t�@�C�@��@�7L@�V@���@�-@���@��-@��9@~��@t��@m�@eO�@Y��@R�H@N@D��@>�y@9�#@3@.E�@*n�@"�@{@�@I�@��@(�@
^521111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�fB�fB�mB�yB�yB�B��BVB\B\BPB%B�/B��B�uBt�BXB8RBL�Bk�BiyBl�Be`B`BBJ�B8RB)�B�B�B	7B�BbB'�B�B�B�ZB�yB�`B�BĜB�1B�B��B
��B
l�B
+B	�B	�3B	� B	O�B	2-B	49B	1'B	�B�BB�LB��B��B�DB�B{�Bw�By�Bt�Bt�Bw�B�B�\B��B��B��B��B��B��B�!B�ZB	$�B	B�B	T�B	^5B	cTB	iyB	x�B	�%B	�hB	��B	��B	�RB	�qB	�dB	�jB	ÖB	��B	��B	�5B	�HB	��B
	7B
oB
�B
%�B
.B
33B
=qB
D�B
H�B
M�B
R�B
W
B
]/B
bNB
gmB
l�B
r�B
s�B
w�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B�fB�mB�yB�yB�B��BVB\B\BPB%B�/B��B�uBt�BXB8RBL�Bk�BiyBl�Be`B`BBJ�B8RB)�B�B�B	7B�BbB'�B�B�B�ZB�yB�`B�BĜB�1B�B��B
��B
l�B
+B	�B	�3B	� B	O�B	2-B	49B	1'B	�B�BB�LB��B��B�DB�B{�Bw�By�Bt�Bt�Bw�B�B�\B��B��B��B��B��B��B�!B�ZB	$�B	B�B	T�B	^5B	cTB	iyB	x�B	�%B	�hB	��B	��B	�RB	�qB	�dB	�jB	ÖB	��B	��B	�5B	�-B	��B
	7B
oB
�B
%�B
.B
3B
=�B
D�B
H�B
M�B
R�B
W
B
]/B
bNB
gmB
l�B
r�B
s�B
w�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201310240703432013102407034320131024070343201608161403182016081614031820160816140318JA  ARFMdecpP7_h                                                                20131010215850  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20131010215910  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20131010215911  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20131010215916  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20131010215916  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20131010215917  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20131010215917  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20131010215917  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20131010221722                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20131013155649  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20131013160900  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20131013160902  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20131013160906  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20131013160906  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20131013160907  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20131013160907  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20131013160907  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20131013162753                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20131023220343  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20131023220343  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050318  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234512                      G�O�G�O�G�O�                