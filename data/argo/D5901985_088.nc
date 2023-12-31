CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2013-10-31T00:59:41Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:43:10Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20131031005941  20161129234516  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               XA   JA  P7_97922_088                    2C  D   PROVOR                          09027                           5815A03                         841 @��:��À1   @��>}X^ @6}/��w�d��l�D1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�ffA��Ai��A�  A�ffA�33B  B33B2  BD��B^��Bs��B���B�ffB���B�  B���B���B���B���Bә�B�ffB癚B�B���CL�C� CffCL�CffC�C   C%�fC*�3C/ffC433C9L�C>��CC��CH�3CR33C[�3Ce� CpffCz  C��3C��C�33C�  C��3C��fC�� C�L�C�ffC��C�ffC�L�C�  C�  C��C�@ C�33C�@ C�ٚC�@ C��C��3C��fC���C�  D��D�D�D33D�D,�D   D%  D)��D/3D4  D9,�D>@ DC3DH�DM  DR&fDW&fD[�3DafDe��Dj�3Do��Dt��Dz9�D�P D���D�ɚD�fD�S3D���D���D��D�FfD��fD���D���D�P Dԉ�D���D�	�D�C3D퉚D�� D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @�33A33Ah  A�33A���A�ffB��B��B1��BDffB^ffBs33B���B�33B���B���B���B�ffB�ffBǙ�B�ffB�33B�ffB�ffB���C33CffCL�C33CL�C  C�fC%��C*��C/L�C4�C933C>� CC�3CH��CR�C[��CeffCpL�Cy�fC��fC�  C�&fC��3C��fC�ٚC�s3C�@ C�Y�C��C�Y�C�@ C��3C��3C��C�33C�&fC�33C���C�33C�  C��fC�ٚC�� C��3D�3DfD3D,�DfD&fD��D%�D)�3D/�D4�D9&fD>9�DC�DH3DL��DR  DW  D[��Da  De�fDj��Do�3Dt�fDz33D�L�D���D��fD�3D�P D��fD���D�	�D�C3D��3D�ɚD���D�L�DԆfD�ٚD�fD�@ D�fD��D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A֍PA�(�A��A��`A��HA��A�{AլA�9XA��AնFA�M�A��TA�ȴA���A�hsA��9A�=qA�dZA��A��A��A��A��A���A�;dA�|�A�A�A��A���A�?}A��-A�dZA���A���A���A�VA��7A��\A�S�A�l�A�`BA}p�Azv�At(�Ai�AcAW�
AL�9AD��AA�A>ffA7��A1��A*I�A%�A!�TA�AffAƨA�AhsA
bA�!A$�AV@�@���@���@�^5@�h@�x�@أ�@�7L@���@�J@�9X@��/@�G�@��@��@��R@��H@��@�9X@�`B@��@��!@��@�K�@���@���@��/@��y@�K�@��@��@t(�@kS�@e/@[ƨ@S�@NE�@H��@D�D@<�@5�@-�h@(1'@#�F@�P@J@�@%@v�@�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A֍PA�(�A��A��`A��HA��A�{AլA�9XA��AնFA�M�A��TA�ȴA���A�hsA��9A�=qA�dZA��A��A��A��A��A���A�;dA�|�A�A�A��A���A�?}A��-A�dZA���A���A���A�VA��7A��\A�S�A�l�A�`BA}p�Azv�At(�Ai�AcAW�
AL�9AD��AA�A>ffA7��A1��A*I�A%�A!�TA�AffAƨA�AhsA
bA�!A$�AV@�@���@���@�^5@�h@�x�@أ�@�7L@���@�J@�9X@��/@�G�@��@��@��R@��H@��@�9X@�`B@��@��!@��@�K�@���@���@��/@��y@�K�@��@��@t(�@kS�@e/@[ƨ@S�@NE�@H��@D�D@<�@5�@-�h@(1'@#�F@�P@J@�@%@v�@�21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�dBƨBǮB��B��B��B!�BA�B9XBn�Bu�Br�BA�BBt�BbNBVB@�B5?B.B%�B�BVB%B�B�#B��B��B�\BaHB6FB�B��B�B��B�Bz�B+B"�BVB
�#B
�B
L�B
49B
B	ŢB	��B	_;B	"�B	B��B�B�/BB�'B��B�{B�%B� Bz�Bt�Bt�Bq�Bn�BjBffB]/BZB�JB�B��B��B�LBB�;B��B	bB	�B	 �B	Q�B	iyB	v�B	�DB	�\B	��B	��B	��B	�FB	�wB	ŢB	��B	�B	�;B	�BB	�5B	�yB	��B
B
uB
�B
#�B
)�B
0!B
:^B
>wB
E�B
L�B
S�B
YB
]/B
`BB
e`B
hsB
l�B
u�B
x�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�BƨB��B��B�B��B!�BA�B9XBn�Bu�Br�BA�BªBt�BbhBVB@�B5?B.B%�B�BVB%B�B�#B��B�B�\BaHB6FB�B��B�B� B�Bz�B+B"�BVB
�#B
�B
L�B
4TB
B	ŢB	��B	_;B	"�B	B��B�B�/BªB�'B��B��B�?B�Bz�Bt�Bt�Bq�Bn�Bj�BffB]IBZ7B�dB�B��B��B�LBB�VB��B	bB	�B	 �B	RB	iyB	v�B	�^B	�vB	��B	��B	�
B	�`B	�wB	żB	��B	�7B	�VB	�BB	�OB	�yB	��B
B
�B
�B
#�B
)�B
0!B
:xB
>wB
E�B
L�B
TB
YB
]IB
`BB
ezB
h�B
l�B
u�B
x�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.2(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201311140708512013111407085120131114070851201608161403462016081614034620160816140346JA  ARFMdecpP7_h                                                                20131031005921  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20131031005941  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20131031005942  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20131031005947  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20131031005947  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20131031005948  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20131031005948  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20131031005948  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20131031012116                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20131102185922  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20131102191539  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20131102191541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20131102191545  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20131102191545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20131102191546  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20131102191546  CF  PSAL            ?��?��?�                  JA  ARGQpump1.0                                                                 20131102191546  CF  TEMP            ?��?��?�                  JA  ARUP                                                                        20131102193720                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004514                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20131113220851  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20131113220851  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050346  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234516                      G�O�G�O�G�O�                