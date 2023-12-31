CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2011-11-11T10:08:49Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:51:50Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20111111100849  20161129232523  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_016                    2C  D   PROVOR                          09027                           5815A03                         841 @�8��� 1   @�:U$� @3qhr� ��dq\(�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A333A�  A�  A�33A���B  B"  B4  BHffB]��BrffB�  B���B�33B�33B���B�33B�  B���B�33B���B�  B�33B���C� C�C��C�3C�C��C ��C%33C*�3C/� C433C9�3C>ffCC33CH�CR�C\�3Ce��CpffCzL�C��fC�� C�  C�s3C��C�@ C�@ C�� C�Y�C�  C��fC�Y�C�&fC�Y�C�Y�C�33C�ٚC��fCۙ�C��fC�3C� C��3C���C�� D9�D33D�D��D� DٚD��D%fD*fD.�fD3��D9fD>  DC  DG� DM9�DR�DW�D\�Da&fDe�3Dj�3Dp  Du�Dz  D�6fD�� D��3D�fD�<�D�p D���D� D�L�D��fD��3D�	�D�)�DԆfDڹ�D��D�S3D��D� D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@ə�A1��A~ffA�33A�ffA�  B��B!��B3��BH  B]33Br  B���B�ffB�  B�  B�ffB�  B���Bș�B�  Bۙ�B���B�  B�ffCffC  C�3C��C  C�3C � C%�C*��C/ffC4�C9��C>L�CC�CH  CR  C\��Ce�3CpL�Cz33C�ٚC��3C��3C�ffC�  C�33C�33C�s3C�L�C��3C�ٚC�L�C��C�L�C�L�C�&fC���C�ٚCی�C�ٚC�fC�s3C��fC��C��3D33D,�D3D�fDٚD�3D�3D%  D*  D.� D3�fD9  D=��DB��DGٚDM33DRfDW3D\3Da  De��Dj��Do��Du3Dy��D�33D���D�� D�3D�9�D�l�D���D��D�I�D��3D�� D�fD�&fDԃ3DڶfD��D�P D퉚D��D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��TA��HA��/A��A��/A��#A��/A��/A��;A��HA��;A��;A��#A���A���A־wAԝ�Aӗ�A�ffA��A�5?A��`A��A�  Aȕ�A�VA�1A�ffA���A�A��;A�
=A�|�A�bNA�ƨA�I�A�\)A�XA��HA�dZA�9XA��yA���A��A��AzbAoC�A`�jAX�RAQ?}AEA=��A41'A-�
A*A�A'%A�TA�
Az�A�DAE�Ar�A	`BA�AĜA J@�G�@�X@�+@��H@���@���@ԣ�@͑h@���@��@���@�C�@�O�@�j@��-@��@���@��@�x�@�C�@��@���@�v�@�dZ@�j@�J@�l�@���@�ȴ@��m@�$�@{o@r��@kC�@d��@]V@Tz�@J�H@E�T@?�P@7��@0��@*�\@#��@@hs@@7L@
=21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��TA��HA��/A��A��/A��#A��/A��/A��;A��HA��;A��;A��#A���A���A־wAԝ�Aӗ�A�ffA��A�5?A��`A��A�  Aȕ�A�VA�1A�ffA���A�A��;A�
=A�|�A�bNA�ƨA�I�A�\)A�XA��HA�dZA�9XA��yA���A��A��AzbAoC�A`�jAX�RAQ?}AEA=��A41'A-�
A*A�A'%A�TA�
Az�A�DAE�Ar�A	`BA�AĜA J@�G�@�X@�+@��H@���@���@ԣ�@͑h@���@��@���@�C�@�O�@�j@��-@��@���@��@�x�@�C�@��@���@�v�@�dZ@�j@�J@�l�@���@�ȴ@��m@�$�@{o@r��@kC�@d��@]V@Tz�@J�H@E�T@?�P@7��@0��@*�\@#��@@hs@@7L@
=21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB�;B�)B�BbB@�B�LB�)B�TB�B��B��B��B\B�B�
BĜB�B@�B#�Bk�B^5BL�B-B��B�=BiyB
�B
�B
bB	�XB	bNB	6FB	hB�B�B�HB�fB�B�HB�-B��BƨB�?BŢB��B��B�}B�3B�B�B�3B�qBȴB��B	  B�B	B	�B	+B	33B	N�B	bNB	w�B	�%B	��B	�B	�LB	B	ǮB	��B	�5B	�mB	�B	�B	��B	��B
B
+B
hB
�B
 �B
%�B
)�B
/B
5?B
=qB
C�B
F�B
L�B
S�B
YB
_;B
dZB
iyB
l�B
p�B
t�B
w�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B��B��B�B� B� B��B� B� B��B��B��B��B��B�B��B� B�\B�;B�CB�BbB@�B�LB�CB�TB��B�B��B��B\B�B�
BĶB�B@�B#�Bk�B^5BL�B-B��B�=BiyB
�B
�B
}B	�rB	bNB	6`B	�B�B�B�bB�B�+B�bB�aB�B��B�ZBżB��B��B��B�MB�5B�5B�hB��B��B�B	 B��B	AB	�B	+B	3MB	N�B	bhB	w�B	�?B	��B	�B	�fB	ªB	��B	� B	�OB	�B	��B	��B	��B	�B
9B
EB
�B
�B
 �B
%�B
*B
/B
5?B
=�B
C�B
F�B
L�B
TB
Y1B
_VB
dZB
i�B
l�B
p�B
t�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281650542013072816505420130728165054201608161350222016081613502220160816135022JA  ARFMdecpP7_e                                                                20111111100845  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111111100849  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111111100850  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111111100854  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111111100854  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111111100855  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111111100855  CF  PSAL            ?���?���?�                  JA  ARGQpump1.0                                                                 20111111100855  CF  TEMP            ?���?���?�                  JA  ARUP                                                                        20111111102057                      G�O�G�O�G�O�                JA  ARFMdecpP7_e                                                                20111113220729  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111113221356  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111113221357  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111113221401  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111113221402  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111113221402  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111113221402  CF  PSAL            ?���?���?�                  JA  ARGQpump1.0                                                                 20111113221402  CF  TEMP            ?���?���?�                  JA  ARUP                                                                        20111113222139                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075054  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075054  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045022  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232523                      G�O�G�O�G�O�                