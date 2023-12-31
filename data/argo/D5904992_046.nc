CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-02-01T06:52:47Z creation;2017-02-04T19:04:35Z conversion to V3.1;2019-09-10T08:29:01Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20170201065247  20190919231517  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               .A   JA  V4_131538_046                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @���� 1   @���+l� @:s33333�cF� ě�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�33A33AvffA���A͙�A�  B��B ��B6��BFffB^ffBpffB���B�ffB�ffB�33B�ffB�33B�33B���B�33B�  B晚B�ffB�  C�3CffC�C33C�fC��C�3C$�3C)ffC.� C4�fC9�fC>� CC��CH33CR  C\L�Ce�fCp  Cz�3C�33C��fC�@ C�&fC��3C��C�L�C�  C�� C�&fC���C�@ C��fC�  C���C��C�Y�C�33C��C��C�  C�&fC�  C�ٚC�  D��D,�DfD�DٚD  D �D%3D*  D/&fD4�D9&fD>33DC3DH,�DM�DRfDW9�D\&fDa�Df�Dk  Dp3Du@ Dz�D�L�D���D�� D�3D�I�D��3D��3D��fD�I�D�p D�� D�fD�P D�y�Dڼ�D���D�FfD�3D�fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�ff@�ffA��Ax  A���A�ffA���B33B!33B733BF��B^��Bp��B�  B���B���B�ffB���B�ffB�ffB�  B�ffB�33B���B�B�33C��C� C33CL�C  C�fC��C$��C)� C.��C5  C:  C>��CC�3CHL�CR�C\ffCf  Cp�Cz��C�@ C��3C�L�C�33C�� C��C�Y�C��C���C�33C��fC�L�C��3C��C�ٚC��C�ffC�@ C��C��C��C�33C��C��fC��D�3D33D�D  D� D&fD 3D%�D*&fD/,�D4  D9,�D>9�DC�DH33DM  DR�DW@ D\,�Da  Df  Dk&fDp�DuFfDz3D�P D���D��3D�fD�L�D��fD��fD���D�L�D�s3D��3D�	�D�S3D�|�D�� D�  D�I�D�fD�D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�=qA�?}A�=qA�A�A�A�A�E�A�E�A�G�A�G�A�I�A�I�A�K�A�O�A�I�A�E�A�9XA�1'A�5?A�7LA�ȴA��TA��7A��7A�;dA��A��^A��7A�&�A�7LA�ƨA�
=A�=qA�bA�7LA���A�|�A�O�A���A�%A��PA��DA�JA}�^Aut�Aq�;Ai7LAc�A^JAV��AQAK�AG�FAE�A=�TA6�A4�A0=qA,^5A(��A$�A!+Av�AoAƨA �A;dA	K�AM�A�@��u@���@��@�Z@�j@��@���@��m@�5?@��@��^@�G�@���@�/@���@�j@�dZ@�Ĝ@���@��@��F@��\@��@�A�@|�D@z��@p�@j��@b~�@[o@R��@K��@F�R@@�@9G�@4��@/�@(1'@#S�@E�@x�@��@hs@��@
J@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�=qA�?}A�=qA�A�A�A�A�E�A�E�A�G�A�G�A�I�A�I�A�K�A�O�A�I�A�E�A�9XA�1'A�5?A�7LA�ȴA��TA��7A��7A�;dA��A��^A��7A�&�A�7LA�ƨA�
=A�=qA�bA�7LA���A�|�A�O�A���A�%A��PA��DA�JA}�^Aut�Aq�;Ai7LAc�A^JAV��AQAK�AG�FAE�A=�TA6�A4�A0=qA,^5A(��A$�A!+Av�AoAƨA �A;dA	K�AM�A�@��u@���@��@�Z@�j@��@���@��m@�5?@��@��^@�G�@���@�/@���@�j@�dZ@�Ĝ@���@��@��F@��\@��@�A�@|�D@z��@p�@j��@b~�@[o@R��@K��@F�R@@�@9G�@4��@/�@(1'@#S�@E�@x�@��@hs@��@
J@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB1B1B1B+B+B+B1B1B1B1B1B1B1B1B1B+B	7B1B+B%B��B��B��B�sB�TB�B�JB��B�bB�B6FB �B��B�/BǮB�%BffB=qB&�BB
�B
�DB
H�B
"�B
B	ƨB	��B	s�B	F�B	$�B	
=B��B�B�3B��B�B��B�bB�DBx�Bl�B\)BQ�BF�B=qB5?B%�B�B�BbBVBPB\B�B:^BD�BN�BZBm�B�B��B�3B��B�sB�B	�B	8RB	:^B	P�B	aHB	jB	w�B	�VB	��B	��B	ƨB	�B	�B	��B
+B
uB
�B
#�B
,B
1'B
8RB
C�B
J�B
Q�B
XB
\)B
bNB
hsB
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB1B1BBB+B+B1B1B1BBBB1B1BB+B	7BBB%B��B��B��B�sB�TB�B�JB��B�bB�B6FB �B��B�/BǮB�%BffB=VB&�BB
��B
�)B
H�B
"�B
B	ƨB	��B	s�B	F�B	$�B	
=B��B�B�B��B�B��B�HB�DBx�BlqB\)BQ�BF�B=qB5?B%�B�B�BHB<BPBBB�B:^BD�BN�BZBm�B�B��B�3B��B�sB�B	�B	88B	:^B	P�B	a-B	jB	w�B	�VB	��B	��B	ƎB	�B	�eB	��B
+B
[B
�B
#�B
,B
1'B
8RB
C{B
J�B
Q�B
W�B
\)B
bNB
hsB
l�B
p�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=-0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702150015482017021500154820170215001548201804031232072018040312320720180403123207JA  ARFMdecpV4_b                                                                20170201065246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170201065247  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170201065247  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170201065248  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170201065248  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170201065248  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20170201070209                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20170204185217  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170204190433  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170204190433  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170204190434  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170204190434  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170204190434  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170204190435                      G�O�G�O�G�O�                JA  ARUP                                                                        20170204191408                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170205000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20170214151548  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170214151548  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033207  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919231517                      G�O�G�O�G�O�                