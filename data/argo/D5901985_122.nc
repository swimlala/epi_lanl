CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2014-10-06T00:54:30Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:39:05Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20141006005430  20161129234514  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA  P7_97922_122                    2C  D   PROVOR                          09027                           5815A03                         841 @�<4� 1   @�>�Sp @5����o�d^n��O�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?L��@���A)��At��A�33A���A�  B��B$ffB8��BJffB^��Br��B�33B���B���B�  B�ffB�  B�33Bʙ�B�  B�  B�ffB�B���C� CL�C  C�3C� CffC ��C$��C*33C/� C3�fC9ffC>  CB�fCH�CQL�C[� Cf�3Cp�3Cz��C�  C��C�&fC�&fC��C��fC�33C�33C�L�C��fC���C�L�C�L�C�Y�C�ffC�@ C�Y�C�  C�33C�&fC�33C�L�C���C�  C�� D  D�D��D,�D  DfD �D%&fD*�D/3D4@ D933D>&fDC&fDH3DM  DRfDW3D\�Da  Df&fDk�Dp  Du9�Dz,�D�` D��3D���D��D�FfD��3D���D�3D�P D���D�ɚD�fD�I�DԌ�D�� D�3D�L�D홚D��3D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?fff@�  A+33AvffA�  A͙�A���B33B$��B933BJ��B_33Bs33B�ffB���B���B�33B���B�33B�ffB���B�33B�33B晚B���B�  C��CffC�C��C��C� C �3C$�fC*L�C/��C4  C9� C>�CC  CH33CQffC[��Cf��Cp��Cz�fC��C��C�33C�33C��C��3C�@ C�@ C�Y�C��3C���C�Y�C�Y�C�ffC�s3C�L�C�ffC��C�@ C�33C�@ C�Y�C�ٚC��C���D&fD  D�3D33DfD�D   D%,�D*  D/�D4FfD99�D>,�DC,�DH�DM&fDR�DW�D\  DafDf,�Dk3Dp&fDu@ Dz33D�c3D��fD�� D�  D�I�D��fD�� D�fD�S3D���D���D��D�L�DԐ D��3D�fD�P D��D��fD�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A旍A�l�A�ZA��A�Q�A�5?A���A�z�A�^5Aݝ�A��A�XAҸRA�ƨA�  A�^5A�v�A�S�A���A�G�A��yA�Q�A��#A�I�A�5?A�+A�~�A��9A�33A��jA�33A���A��+A��hA�=qA���A��A��\A���A�bAG�Aw�^Aj��AZ�APM�AKx�AE��AAx�A:�A4ȴA1�
A-�wA(I�A!�-A��AXA
=A~�At�A
~�A(�A�A+A {@��@�~�@�E�@�S�@��`@�E�@��@���@�r�@��m@��@�C�@���@�%@�(�@���@��@��@�(�@���@�33@�1@��@��T@�z�@�l�@��@��/@�
=@�j@�{@���@{��@q��@g��@]p�@T�@OK�@I��@D�@>{@6�y@1G�@+��@(bN@%�@��@�^@�@�#@O�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A旍A�l�A�ZA��A�Q�A�5?A���A�z�A�^5Aݝ�A��A�XAҸRA�ƨA�  A�^5A�v�A�S�A���A�G�A��yA�Q�A��#A�I�A�5?A�+A�~�A��9A�33A��jA�33A���A��+A��hA�=qA���A��A��\A���A�bAG�Aw�^Aj��AZ�APM�AKx�AE��AAx�A:�A4ȴA1�
A-�wA(I�A!�-A��AXA
=A~�At�A
~�A(�A�A+A {@��@�~�@�E�@�S�@��`@�E�@��@���@�r�@��m@��@�C�@���@�%@�(�@���@��@��@�(�@���@�33@�1@��@��T@�z�@�l�@��@��/@�
=@�j@�{@���@{��@q��@g��@]p�@T�@OK�@I��@D�@>{@6�y@1G�@+��@(bN@%�@��@�^@�@�#@O�22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�HB�HB�BB��B�qB�}B��B�B�;B�;B�B��B�B��B��B	7B5?BR�B]/B:^B33B33B/BbB�ZB��B��B��B�B[#BD�B;dB%B�LBx�BiyBD�B2-B%B
�XB
\)B
�B	ǮB	YB	%�B	JB��B�B��BB�dB�B��B�oB�bB�=B�By�Bw�Bz�B}�B|�B�B�B�+B�=B�%B��B�B�!B�}B��B��B�B	hB	)�B	9XB	N�B	]/B	�B	�=B	��B	��B	�B	��B	�!B	�LB	�}B	ɺB	�BB	�B	��B
B
1B
DB
oB
�B
!�B
)�B
33B
:^B
@�B
G�B
J�B
P�B
VB
ZB
^5B
_;B
bNB
ffB
k�B
o�B
s�B
x�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�B�HB�BB��B�VB�cB��B�eB�;B�;B�B��B�B��B��B	7B5?BR�B]/B:^B3B33B/BHB�ZB��B��B��B��B[	BD�B;JBB�LBx�Bi_BD�B2-BB
�>B
\)B
�B	ǮB	YB	%�B	0B��B�B��B�uB�dB� B��B�oB�bB�=B�By�Bw�Bz�B}�B|�B��B��B�+B�=B�%B��B�B�!B�cBοB��B�qB	hB	)�B	9>B	N�B	]B	��B	�=B	��B	��B	��B	��B	�B	�LB	�}B	ɺB	�BB	�B	��B
B
1B
)B
TB
B
!�B
)�B
3B
:^B
@�B
G�B
J�B
P�B
U�B
ZB
^B
_;B
b4B
ffB
k�B
o�B
s�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201410190018082014101900180820141019001808201608161410362016081614103620160816141036JA  ARFMdecpP7_h                                                                20141006005410  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20141006005430  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20141006005431  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20141006005436  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20141006005436  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20141006005437  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20141006005437  CF  PSAL            ?L��@���?�                  JA  ARGQpump1.0                                                                 20141006005437  CF  TEMP            ?L��@���?�                  JA  ARUP                                                                        20141006012317                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20141008185310  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20141008190210  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20141008190211  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20141008190216  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20141008190216  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20141008190216  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20141008190216  CF  PSAL            ?L��@���?�                  JA  ARGQpump1.0                                                                 20141008190216  CF  TEMP            ?L��@���?�                  JA  ARUP                                                                        20141008192459                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20141018151808  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20141018151808  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816051036  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234514                      G�O�G�O�G�O�                