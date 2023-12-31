CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2015-12-18T06:53:45Z creation;2015-12-21T19:03:47Z conversion to V3.1;2019-09-10T08:34:39Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20151218065345  20190919221515  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131538_005                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @׆��F� 1   @׆�4%� @:m�hr�!�cw�
=p�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @�ffA!��Ap  A�  A�33A���B33B!��B6��BI��B[33Bq33B���B�33B�33B�  B���B�ffB�  B�ffB�33B�ffB���B�  B���CL�C� C�fC�3C�3CffC   C$�fC*�3C/ffC4��C933C>�3CC33CG��CR�C\�3CfffCo��CzL�C�� C�  C�� C�  C�33C�L�C�@ C�&fC�@ C��3C�  C��fC��C�  C��C�ٚC�@ C�  C܌�C�  C��C�ٚC��3C�33C�ffD��DfD33D3D&fD�D �D$� D)�3D.�fD3�3D9  D>�DC�DH@ DM  DR�DW�D\�Da�Df  Dk  Dp  Du33Dz33D�\�D���D��fD�fD�L�D��fD���D��3D�9�D���D���D�3D�I�DԌ�D�ɚD�	�D�VfD��D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@�33A   AnffA�33A�ffA�  B��B!33B6ffBI33BZ��Bp��B���B�  B�  B���B���B�33B���B�33B�  B�33B噚B���B���C33CffC��C��C��CL�C�fC$��C*��C/L�C4�3C9�C>��CC�CG� CR  C\��CfL�Co�3Cz33C�s3C��3C�s3C��3C�&fC�@ C�33C��C�33C��fC��3C�ٚC�  C��3C��C���C�33C��3C܀ C��3C�  C���C��fC�&fC�Y�D�fD  D,�D�D  DfD fD$ٚD)��D.� D3��D9�D>3DCfDH9�DL��DRfDWfD\fDafDe��Dk�Do��Du,�Dz,�D�Y�D��fD��3D�3D�I�D��3D���D�� D�6fD���D���D�  D�FfDԉ�D��fD�fD�S3D퉚D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�bNA�`BA�S�A�E�A�/A� �A�7LA��A��TA�33A��A�ȴA���A��RA��-A���A�oA���A��yA�v�A�bNA���A�=qA�~�A��A���A�S�A�O�A�|�A�ZA�K�A��uA�%A���A��/A�A���A��^A�ZA��\A|M�Ar��Ao�PAfA`bNAZ�AQ�#AO�mANffAI
=A@��A=dZA7��A5��A1
=A,ĜA(�\A&��A"��A JAS�A33A�A|�A1'AA�Ax�A�P@�x�@�o@⟾@�I�@��T@��@��@�J@���@�hs@��/@� �@�hs@�`B@��@��@���@��^@�ƨ@��#@�l�@�$�@��@|�/@zJ@vv�@o��@g�@^�y@W�w@P�`@J�H@EV@>E�@8�u@3o@-�-@)G�@%?}@ �@Z@bN@�F@K�@
�2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�bNA�`BA�S�A�E�A�/A� �A�7LA��A��TA�33A��A�ȴA���A��RA��-A���A�oA���A��yA�v�A�bNA���A�=qA�~�A��A���A�S�A�O�A�|�A�ZA�K�A��uA�%A���A��/A�A���A��^A�ZA��\A|M�Ar��Ao�PAfA`bNAZ�AQ�#AO�mANffAI
=A@��A=dZA7��A5��A1
=A,ĜA(�\A&��A"��A JAS�A33A�A|�A1'AA�Ax�A�P@�x�@�o@⟾@�I�@��T@��@��@�J@���@�hs@��/@� �@�hs@�`B@��@��@���@��^@�ƨ@��#@�l�@�$�@��@|�/@zJ@vv�@o��@g�@^�y@W�w@P�`@J�H@EV@>E�@8�u@3o@-�-@)G�@%?}@ �@Z@bN@�F@K�@
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�3BɺB�BB�B��BB
=B%�B6FB;dBA�BB�B>wB%�B�B�B�fB�3B��B�PBp�BP�B1'B$�B�B�B�`B�bBt�Bo�B[#BC�BJB
�B
�B
�bB
�1B
r�B
&�B
1B	�wB	�uB	cTB	2-B	=qB	<jB	�B�)B�B�#B�B�jB�'B��B��B�7B�Bx�Bk�BbNBVBM�BA�B:^B1'B)�B�B�B
=B1BbB)�B6FBC�BW
Bk�B|�B�VB�LBǮB�/B�B	hB	#�B	/B	?}B	VB	jB	~�B	�=B	�{B	��B	�qB	�B	�B
  B
JB
uB
�B
&�B
-B
6FB
=qB
C�B
I�B
N�B
R�B
XB
^5B
cTB
hs4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�MBɺB�\B�B��BB
=B%�B6FB;dBA�BB�B>�B%�B�B�B�fB�3B��B�PBp�BP�B1'B$�B�B�B�`B�bBt�Bo�B[#BC�BJB
�B
�B
�}B
�KB
r�B
&�B
1B	�wB	��B	cTB	2GB	=�B	<jB	�B�)B�B�=B�7B�jB�AB��B��B�RB�Bx�Bk�BbNBVBM�BA�B:^B1AB*B�B�B
XB1B}B*B6`BC�BW
Bk�B|�B�VB�fBǮB�/B�B	hB	#�B	/B	?}B	VB	jB	B	�XB	�{B	��B	�qB	�B	�B
 B
JB
uB
�B
'B
-)B
6FB
=qB
C�B
I�B
N�B
SB
XB
^OB
cnB
hs3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=-0.1(dbar)                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201601010022342016010100223420160101002234201804031230072018040312300720180403123007JA  ARFMdecpV4_b                                                                20151218065345  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151218065345  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151218065345  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151218065346  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151218065346  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20151218065346  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20151218065347  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20151218065347  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20151218070443                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20151221185246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151221190345  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151221190346  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151221190347  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151221190347  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20151221190347  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20151221190347  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20151221190347  CF  TEMP            ?   ?   ?�                  JA      jafc1.0                                                                 20151221190347                      G�O�G�O�G�O�                JA  ARUP                                                                        20151221190810                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20151222000000  CF  PSAL_ADJUSTED_QC?   ?   G�O�                JM  ARSQJMQC2.0                                                                 20151222000000  CF  TEMP_ADJUSTED_QC?   ?   G�O�                JM  ARCAJMQC2.0                                                                 20151231152234  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20151231152234  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033007  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919221515                      G�O�G�O�G�O�                