CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   q   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-04-11T04:12:43Z creation;2016-04-11T04:12:45Z conversion to V3.1;2019-09-10T08:33:20Z update;     
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
_FillValue                  t  ;`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Fx   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  F�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Jt   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ^�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ^�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ^�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ^�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ^�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _lArgo profile    3.1 1.2 19500101000000  20160411041243  20190919231516  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131538_015                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @נ:RL��1   @נ=�33 @;8���F�cr� ě�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�ffA(  A|��A���Ař�A�ffB  B��B0��BI��B\ffBn��B�33B�  B���B�  B�33B�ffB�ffB�ffB�ffBۙ�B䙚B�  B�  C�3CffC  C� C��C��C ��C%�fC+  C/��C4� C9�3C>�3CD  CH�fCR�3C\33Cf�3Cp��Cz33C�33C�ffC�  C�  C��3C���C�� C��fC��fC�� C�� C�  C��C¦fC�� C̀ C��fC��3Cۙ�C��3C�� C�s3C�  C��C�  D3D3D��D33D3D�D 33D%  D)��D/�D4  D93D=�fDC�DH�DM3DRfDW,�D[�3D`�3Df3Dj�3Dp&fDu&fDz  D�0 D�|�D��3D� D�VfD�� D�� D�fD�FfD�y�D���D��D�FfD�|�Dڼ�D�  D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ?���@�ffA(  A|��A���Ař�A�ffB  B��B0��BI��B\ffBn��B�33B�  B���B�  B�33B�ffB�ffB�ffB�ffBۙ�B䙚B�  B�  C�3CffC  C� C��C��C ��C%�fC+  C/��C4� C9�3C>�3CD  CH�fCR�3C\33Cf�3Cp��Cz33C�33C�ffC�  C�  C��3C���C�� C��fC��fC�� C�� C�  C��C¦fC�� C̀ C��fC��3Cۙ�C��3C�� C�s3C�  C��C�  D3D3D��D33D3D�D 33D%  D)��D/�D4  D93D=�fDC�DH�DM3DRfDW,�D[�3D`�3Df3Dj�3Dp&fDu&fDz  D�0 D�|�D��3D� D�VfD�� D�� D�fD�FfD�y�D���D��D�FfD�|�Dڼ�D�  D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�9XA��A��A�C�A���A���A�I�A���A��A�=qA�A�A�ĜA��A�?}A�-A���A�~�A��wA��A�&�A���A�XA� �A�1'A�XA���A���A���A�bA���A�A�A�hsA�x�A7LAy��Av�Ap�AmoAjr�Ag&�A`ffA_t�A[�AVz�AQ�ANQ�AL-AF$�AC�PA?VA<Q�A8�\A5\)A1G�A/+A-S�A'�PA$�A!�-A��A�A�A��AA�A	�TA�A��A Z@��T@�ȴ@���@��#@���@��^@�^5@�\)@��u@�$�@�j@�-@��R@�33@���@���@�v�@�9X@�@��m@�{@�V@���@��@
=@{�@wK�@m��@e�T@_;d@X�9@PA�@I�#@DZ@=V@7l�@1X@,��@&E�@  �@dZ@l�@Z@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�9XA��A��A�C�A���A���A�I�A���A��A�=qA�A�A�ĜA��A�?}A�-A���A�~�A��wA��A�&�A���A�XA� �A�1'A�XA���A���A���A�bA���A�A�A�hsA�x�A7LAy��Av�Ap�AmoAjr�Ag&�A`ffA_t�A[�AVz�AQ�ANQ�AL-AF$�AC�PA?VA<Q�A8�\A5\)A1G�A/+A-S�A'�PA$�A!�-A��A�A�A��AA�A	�TA�A��A Z@��T@�ȴ@���@��#@���@��^@�^5@�\)@��u@�$�@�j@�-@��R@�33@���@���@�v�@�9X@�@��m@�{@�V@���@��@
=@{�@wK�@m��@e�T@_;d@X�9@PA�@I�#@DZ@=V@7l�@1X@,��@&E�@  �@dZ@l�@Z@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�PB��B��B��B�BjBp�Bp�Bo�Bm�B_;BN�B@�B�B�TB�yB�fB�B��B��B�1Br�BgmBr�B�B%B��B��BZB.B
��B
�XB
�B
��B
�JB
^5B
C�B
�B	��B	�B	��B	��B	��B	�B	dZB	O�B	?}B	1'B	�B	�B	  B�B�BB��BĜB�jB�3B��B�bB�Bp�BhsBM�BE�B5?B/B.B%�B�B�B{B
=B%BDBhB(�B8RBM�BffB{�B��B�'BǮB�)B�mB�B	B	�B	-B	<jB	E�B	[#B	n�B	}�B	�VB	��B	��B	�)B	�B	��B
DB
�B
�B
(�B
1'B
8RB
=qB
F�B
M�B
T�B
ZB
^5B
bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�PB��B��B��B�BjBp�Bp�Bo�Bm�B_;BN�B@�B�B�TB�yB�fB�B��B��B�1Br�BgmBr�B�B%B��B��BZB.B
��B
�XB
�B
��B
�JB
^5B
C�B
�B	��B	�B	��B	��B	��B	�B	dZB	O�B	?}B	1'B	�B	�B	  B�B�BB��BĜB�jB�3B��B�bB�Bp�BhsBM�BE�B5?B/B.B%�B�B�B{B
=B%BDBhB(�B8RBM�BffB{�B��B�'BǮB�)B�mB��B	B	�B	-B	<jB	E�B	[#B	n�B	}�B	�VB	��B	��B	�B	�B	��B
DB
�B
�B
(�B
1'B
8RB
=�B
F�B
M�B
T�B
ZB
^5B
bN33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201604110017422016041100174220160411001742201804031230372018040312303720180403123037JA  ARFMdecpV4_b                                                                20160411011407  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20160411041243  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20160411041243  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20160411041244  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20160411041245  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20160411041245  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160411041245                      G�O�G�O�G�O�                JA  ARUP                                                                        20160411041848                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20160401000000  CF  PSAL_ADJUSTED_QC?���@�ffG�O�                JM  ARCAJMQC2.0                                                                 20160410151742  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160410151742  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033037  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919231516                      G�O�G�O�G�O�                