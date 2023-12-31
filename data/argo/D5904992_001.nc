CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2015-11-11T16:02:49Z creation;2015-11-11T16:02:50Z conversion to V3.1;2019-09-10T08:35:12Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20151111160249  20190919221515  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131538_001                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�|��vT 1   @�|���
 @;%�����c�
=p��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@�ffA1��A{33A���A͙�A�33B  BffB5��BFffB]��Bs33B�33B�ffB���B�ffB�ffB�  B���B�  B�ffB�33B���B�B���CffCL�C  C��C33C� C�fC%��C)�fC/L�C433C9�3C>  CB�3CG��CR33C[�3Ce��CoffCy33C�� C�ٚC�L�C��fC�s3C�&fC�ٚC�L�C�Y�C�  C�ٚC�&fC�@ C�L�C�L�C�&fC��C�33C�ffC��C噚C�L�C�L�C�Y�C�  DfD  D3D�D  D��D �D%,�D*3D/fD3��D9  D>3DC  DH3DMfDQ�3DV��D\fDa33De�3Dk  Dp&fDu,�Dz�D�@ D�p D�� D���D�<�D�� D��fD��3D�S3D���D���D��D�0 D�y�D�� D���D�I�D��D��D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?fff@�ffA1��A{33A���A͙�A�33B  BffB5��BFffB]��Bs33B�33B�ffB���B�ffB�ffB�  B���B�  B�ffB�33B���B�B���CffCL�C  C��C33C� C�fC%��C)�fC/L�C433C9�3C>  CB�3CG��CR33C[�3Ce��CoffCy33C�� C�ٚC�L�C��fC�s3C�&fC�ٚC�L�C�Y�C�  C�ٚC�&fC�@ C�L�C�L�C�&fC��C�33C�ffC��C噚C�L�C�L�C�Y�C�  DfD  D3D�D  D��D �D%,�D*3D/fD3��D9  D>3DC  DH3DMfDQ�3DV��D\fDa33De�3Dk  Dp&fDu,�Dz�D�@ D�p D�� D���D�<�D�� D��fD��3D�S3D���D���D��D�0 D�y�D�� D���D�I�D��D��D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�A��AǓuA�G�A�O�A�K�A�K�A�I�A�=qA�/A��A��AÓuA�ƨA��DA��HA��#A�$�A�A�ĜA�Q�A���A�hsA���A�M�A��A��\A��9A�JA�1A�x�A�|�A�VA�A���A��
A~~�A}��Ax��Av�9Ao�Ah5?AdAa
=A]G�AXAW��AO33AL(�AFbAD�A?%A9�#A7l�A4-A0E�A+G�A%33A"��A&�AA�A��A%A��A	�A��AE�@�{@��@�@�?}@��@�t�@���@���@��@��`@�M�@��j@��@���@�Ĝ@�&�@��@�o@��j@��R@�I�@��@��@�$�@�(�@}��@z=q@w�;@r��@mp�@f{@\�@U��@N��@GK�@?�w@9��@2~�@.@'|�@#��@�-@1'@?}@x�@�@	��@{21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�A��AǓuA�G�A�O�A�K�A�K�A�I�A�=qA�/A��A��AÓuA�ƨA��DA��HA��#A�$�A�A�ĜA�Q�A���A�hsA���A�M�A��A��\A��9A�JA�1A�x�A�|�A�VA�A���A��
A~~�A}��Ax��Av�9Ao�Ah5?AdAa
=A]G�AXAW��AO33AL(�AFbAD�A?%A9�#A7l�A4-A0E�A+G�A%33A"��A&�AA�A��A%A��A	�A��AE�@�{@��@�@�?}@��@�t�@���@���@��@��`@�M�@��j@��@���@�Ĝ@�&�@��@�o@��j@��R@�I�@��@��@�$�@�(�@}��@z=q@w�;@r��@mp�@f{@\�@U��@N��@GK�@?�w@9��@2~�@.@'|�@#��@�-@1'@?}@x�@�@	��@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B^5BgmBgmBjB��B�3B�3B�9B�?B�?B�3B�3B�FB�B��B�=B�B�yB�jB��B�B�\Bu�BT�B/BhB	7B�TB�#B�XB��B�Be`BE�B�B
��B
�B
�B
�B
[#B
F�B
1B	ɺB	�RB	��B	� B	bNB	w�B	.B	2-B	%B	hB�sB��B�/B�B��B�-B��B�bB�BjB\)BS�BB�B9XB49B+B#�B�B�B�B�B�B2-B;dBF�B]/Bq�B�%B��B�BB�#B�B	B	uB	#�B	2-B	I�B	VB	jB	{�B	�B	��B	��B	�9B	ɺB	�;B	�B	��B

=B
�B
!�B
,B
5?B
;dB
D�B
H�B
P�B
VB
ZB
_;B
dZB
iyB
m�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B^5BgmBgmBjB��B�3B�3B�9B�?B�?B�3B�3B�FB�B��B�=B�B�yB�jB��B�B�\Bu�BT�B/BhB	7B�TB�#B�XB��B�Be`BE�B�B
��B
�B
�B
�B
[#B
F�B
1B	ɺB	�RB	��B	� B	bNB	w�B	.B	2-B	%B	hB�sB��B�/B�B��B�-B��B�bB�BjB\)BS�BB�B9XB49B+B#�B�B�B�B�B�B2GB;dBF�B]/Bq�B�%B��B�BB�#B�B	 B	uB	#�B	2-B	I�B	VB	jeB	{�B	�B	�mB	��B	�9B	ɺB	�;B	�B	��B

=B
gB
!�B
,B
5?B
;B
D�B
H�B
P�B
VB
Z7B
_;B
dZB
i_B
m�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201511220022072015112200220720151122002207201804031229552018040312295520180403122955JA  ARFMdecpV4_b                                                                20151111155308  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151111160249  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151111160249  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151111160250  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151111160250  QCP$                G�O�G�O�G�O�            FB40JA  ARGQpump1.0                                                                 20151111160250  CF  PSAL            ?fff?fff?�                  JA  ARGQpump1.0                                                                 20151111160250  CF  TEMP            ?fff?fff?�                  JA      jafc1.0                                                                 20151111160250                      G�O�G�O�G�O�                JA  ARUP                                                                        20151111160632                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20151112000000  CF  PSAL_ADJUSTED_QC?fff?fffG�O�                JM  ARSQJMQC2.0                                                                 20151112000000  CF  TEMP_ADJUSTED_QC?fff?fffG�O�                JM  ARCAJMQC2.0                                                                 20151121152207  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20151121152207  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403032955  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919221515                      G�O�G�O�G�O�                