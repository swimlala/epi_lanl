CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2015-11-28T03:53:10Z creation;2015-12-01T16:04:42Z conversion to V3.1;2019-09-10T08:34:55Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20151128035310  20190919221515  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131538_003                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @ׁ�Ib��1   @ׁ��� @:���R�c�^5?|�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >L��@�  A   Ax  A�33A���A�  B33B��B2ffBFffB[33BpffB�33B���B�ffB���B�ffB�ffB�ffB�33B�ffB���B�  B�  B�ffC�fCffC��C��C�C  C!  C%� C*��C/� C4��C8��C>� CC� CI  CRffC\  Ce�fCp� Cz��C�&fC�L�C�@ C��fC�L�C�� C��3C�@ C�ٚC�� C�@ C�� C�ffC�ٚC�33C��fC�Y�C�s3C��C��C�s3C�33C��C��3C�  D  D�D�D  D��D3D�3D%�D)��D/fD43D9  D>&fDB��DH�DM�DR�DW  D\�D`��De�3Dk&fDo��Du33Dz3D�Y�D�ffD���D�  D�I�D��3D���D�fD�FfD�� D��3D��fD�9�DԌ�D���D���D�<�D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >L��@�  A   Ax  A�33A���A�  B33B��B2ffBFffB[33BpffB�33B���B�ffB���B�ffB�ffB�ffB�33B�ffB���B�  B�  B�ffC�fCffC��C��C�C  C!  C%� C*��C/� C4��C8��C>� CC� CI  CRffC\  Ce�fCp� Cz��C�&fC�L�C�@ C��fC�L�C�� C��3C�@ C�ٚC�� C�@ C�� C�ffC�ٚC�33C��fC�Y�C�s3C��C��C�s3C�33C��C��3C�  D  D�D�D  D��D3D�3D%�D)��D/fD43D9  D>&fDB��DH�DM�DR�DW  D\�D`��De�3Dk&fDo��Du33Dz3D�Y�D�ffD���D�  D�I�D��3D���D�fD�FfD�� D��3D��fD�9�DԌ�D���D���D�<�D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��FA���A���A���A��hA��A�v�A�`BA�I�A��A���A���A��#A���A���A���A� �A�ffA�$�A�ȴA���A�|�A�t�A��^A���A���A�jA���A�K�A�$�A�A�t�A�9XA���A��A�VA�S�A��-A}+Ay�AvJAo�-Ah�+A`��A\��AW��AS%APffAN�DAI��AF�ADA?33A8��A5\)A1�#A/�A+�A$ĜA ȴA��A��At�A�A��A$�A;dA��A v�@�C�@�Q�@�C�@��m@ҧ�@�9X@�-@�S�@�;d@�j@� �@��!@��#@��@���@�A�@���@�E�@�(�@�~�@�9X@���@�1@|j@y��@w�@u�@mV@g;d@`��@Z-@RM�@L9X@E�-@?�P@8�u@3��@-`B@(�u@$(�@l�@��@��@Ĝ@(�@	x�2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��FA���A���A���A��hA��A�v�A�`BA�I�A��A���A���A��#A���A���A���A� �A�ffA�$�A�ȴA���A�|�A�t�A��^A���A���A�jA���A�K�A�$�A�A�t�A�9XA���A��A�VA�S�A��-A}+Ay�AvJAo�-Ah�+A`��A\��AW��AS%APffAN�DAI��AF�ADA?33A8��A5\)A1�#A/�A+�A$ĜA ȴA��A��At�A�A��A$�A;dA��A v�@�C�@�Q�@�C�@��m@ҧ�@�9X@�-@�S�@�;d@�j@� �@��!@��#@��@���@�A�@���@�E�@�(�@�~�@�9X@���@�1@|j@y��@w�@u�@mV@g;d@`��@Z-@RM�@L9X@E�-@?�P@8�u@3��@-`B@(�u@$(�@l�@��@��@Ĝ@(�@	x�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bs�Bw�Bw�Bx�By�Bz�B{�B|�B~�B�B�B�B�B~�B}�B{�Br�B[#BB�mB�qB�B��B��Bl�BI�B?}B��B�)B�}B�B{�B9XB�B
��B
�ZB
�FB
��B
t�B
_;B
B�B
DB	��B	��B	�B	aHB	F�B	D�B	=qB	�B	�B	
=B��B�NB��B��B�FB�B��B�By�BjBaHBR�BG�B?}B49B,B&�B"�B�B{B�B�B%�B8RB:^BM�BiyB�=B��B�XBȴB�`B�B	uB	�B	0!B	@�B	XB	gmB	w�B	�B	�oB	��B	�B	ÖB	�)B	�B	��B
1B
oB
�B
%�B
0!B
5?B
=qB
C�B
H�B
M�B
S�B
\)B
aHB
gmB
k�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bs�Bw�Bw�Bx�By�Bz�B{�B|�B~�B�B�B�B�B~�B}�B{�Br�B[#BB�mB�qB�B��B��Bl�BI�B?}B��B�)B�}B�B{�B9XB�B
��B
�ZB
�FB
��B
t�B
_;B
B�B
DB	��B	��B	�B	aHB	F�B	D�B	=qB	�B	�B	
=B��B�NB��B��B�FB�B��B�By�BjBaHBR�BG�B?}B49B,B&�B"�B�B{ByB�B%�B8RB:DBM�BiyB�XB��B�rBȴB�`B��B	uB	�B	0;B	@�B	XB	gmB	w�B	�B	�oB	��B	�"B	ÖB	�B	�B	��B
1B
oB
�B
%�B
0!B
5?B
=qB
C�B
H�B
M�B
S�B
\)B
aHB
gmB
k�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201512120022012015121200220120151212002201201804031230012018040312300120180403123001JA  ARFMdecpV4_b                                                                20151128035309  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151128035310  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151128035310  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151128035311  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151128035311  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20151128035311  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20151128035311  CF  PSAL            >L��>L��?�                  JA  ARGQpump1.0                                                                 20151128035311  CF  TEMP            >L��>L��?�                  JA  ARUP                                                                        20151128040816                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20151201155308  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151201160441  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151201160441  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151201160442  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151201160442  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20151201160442  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20151201160442  CF  PSAL            >L��>L��?�                  JA  ARGQpump1.0                                                                 20151201160442  CF  TEMP            >L��>L��?�                  JA      jafc1.0                                                                 20151201160442                      G�O�G�O�G�O�                JA  ARUP                                                                        20151201160900                      G�O�G�O�G�O�                JA  ARUP                                                                        20151216100502                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20151202000000  CF  PSAL_ADJUSTED_QC>L��>L��G�O�                JM  ARSQJMQC2.0                                                                 20151202000000  CF  TEMP_ADJUSTED_QC>L��>L��G�O�                JM  ARCAJMQC2.0                                                                 20151211152201  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20151211152201  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033001  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919221515                      G�O�G�O�G�O�                