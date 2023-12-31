CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-07-01T03:52:24Z creation;2017-07-04T19:02:06Z conversion to V3.1;2019-09-10T08:26:55Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20170701035224  20190919231515  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA  V4_131538_061                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�;`$h�1   @�A:Ӏ@;���n��czM���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A.ffA�  A�33A�ffA���B��B"��B333BH��B[��BtffB���B���B���B���B���B���B�ffB���Bϙ�B�33B�  BB���CL�CffC�3C�3C�fC�C L�C$33C)��C.��C3��C8� C=��CB��CH�CQ�C\�Cf� Cp�3CzL�C�L�C��fC��C��3C�  C�s3C��3C�s3C���C�  C�@ C��C�Y�C��fC�Y�C�&fC��C�ffC�L�C�ٚC��C��C��3C��fC�ٚD3D��D�D  D,�D&fD 33D$��D)�fD.��D3�3D9&fD>�DC  DG��DM�DR�DW�D\  Da�DffDj� Dp33Du  Dy�3D�L�D���D���D��D�VfD�|�D���D�	�D�<�D�� D��fD�	�D�Y�Dԃ3D�� D��D�6fD� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?���@���A.ffA�  A�33A�ffA���B��B"��B333BH��B[��BtffB���B���B���B���B���B���B�ffB���Bϙ�B�33B�  BB���CL�CffC�3C�3C�fC�C L�C$33C)��C.��C3��C8� C=��CB��CH�CQ�C\�Cf� Cp�3CzL�C�L�C��fC��C��3C�  C�s3C��3C�s3C���C�  C�@ C��C�Y�C��fC�Y�C�&fC��C�ffC�L�C�ٚC��C��C��3C��fC�ٚD3D��D�D  D,�D&fD 33D$��D)�fD.��D3�3D9&fD>�DC  DG��DM�DR�DW�D\  Da�DffDj� Dp33Du  Dy�3D�L�D���D���D��D�VfD�|�D���D�	�D�<�D�� D��fD�	�D�Y�Dԃ3D�� D��D�6fD� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A̲-A̝�A�VA��A�7LA�(�A��\A�ZA� �A���A��mA�S�A��RA�$�A��FA��TA�I�A�I�A���A�v�A�bA��A�"�A�bA��DA���A��A��A�jA��A��A�1'A��FA~M�A|JAx~�Au33Am��Ag��AfjA]�AYƨAV�ANAHz�A@�A<�\A8�DA6=qA1/A.�uA*��A(ȴA%�A!�Az�A%AAv�AVA�mA
ȴA��Az�A =q@��/@�"�@�`B@�C�@���@�(�@�bN@�-@��@�ȴ@��@�X@���@�?}@��/@��\@��m@��R@�  @��`@��!@��w@�G�@}�@|(�@y�@w�;@u@rJ@n��@c��@bJ@[C�@S"�@J��@HbN@D9X@=/@6�R@1�^@-/@';d@"^5@��@��@�/@A�@��@	7L1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A̲-A̝�A�VA��A�7LA�(�A��\A�ZA� �A���A��mA�S�A��RA�$�A��FA��TA�I�A�I�A���A�v�A�bA��A�"�A�bA��DA���A��A��A�jA��A��A�1'A��FA~M�A|JAx~�Au33Am��Ag��AfjA]�AYƨAV�ANAHz�A@�A<�\A8�DA6=qA1/A.�uA*��A(ȴA%�A!�Az�A%AAv�AVA�mA
ȴA��Az�A =q@��/@�"�@�`B@�C�@���@�(�@�bN@�-@��@�ȴ@��@�X@���@�?}@��/@��\@��m@��R@�  @��`@��!@��w@�G�@}�@|(�@y�@w�;@u@rJ@n��@c��@bJ@[C�@S"�@J��@HbN@D9X@=/@6�R@1�^@-/@';d@"^5@��@��@�/@A�@��@	7L1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B1'B>wB<jB;dB6FB
=BBhB�BDB��B�9B�uBl�B]/BF�B0!B�B�`B�B�dB��B��B�B�7B|�BM�B�BB
�B
�!B
�qB
��B
�B
bNB
J�B
)�B
DB	��B	��B	�{B	N�B	dZB	H�B	PB�B�3B��B�RB�!B��B��B�PB�DB{�Bl�BhsBe`BdZB_;BP�B?}B9XB,B �B�B�BuBJBB��B��B��BBhB�B1'BB�BS�BffBy�B�B��BÖB��B�sB��B	VB	"�B	<jB	N�B	bNB	t�B	�%B	��B	��B	�B	�B	�B	��B
B
uB
�B
&�B
33B
9XB
@�B
F�B
L�B
R�B
YB
^5B
cTB
hsB
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B1'B>wB<jB;dB6FB
=BBhB�BDB��B�9B�uBl�B]/BF�B0!B�B�`B�B�dB��B��B�B�7B|�BM�B�BB
�B
�!B
�qB
��B
�B
bNB
J�B
)�B
DB	��B	��B	�{B	N�B	dZB	H�B	PB�B�3B��B�RB�!B��B��B�PB�DB{�Bl�BhsBe`BdZB_;BP�B?�B9XB,B �B�B�BuBJBB��B��B��BBNB�B1'BB�BS�BffBy�B�B��BÖB��B�sB��B	VB	"�B	<jB	N�B	bNB	t�B	�%B	��B	��B	�/B	�B	�B	��B
B
uB
�B
&�B
33B
9XB
@�B
F�B
L�B
R�B
YB
^B
cTB
hsB
m�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707150016022017071500160220170715001602201804031232522018040312325220180403123252JA  ARFMdecpV4_b                                                                20170701035223  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170701035224  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170701035224  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170701035225  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170701035225  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170701035225  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20170701040908                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20170704185224  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170704190204  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170704190204  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170704190205  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170704190205  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170704190205  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170704190206                      G�O�G�O�G�O�                JA  ARUP                                                                        20170704191307                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170705000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20170714151602  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170714151602  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033252  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919231515                      G�O�G�O�G�O�                