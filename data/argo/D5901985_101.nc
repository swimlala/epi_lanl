CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2014-03-10T00:58:32Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:41:38Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20140310005832  20161129234511  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA  P7_97922_101                    2C  D   PROVOR                          09027                           5815A03                         841 @��^З�1   @��#� @4����m�c�$�/�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�ffA#33AvffA���Aə�A�ffB��B33B2  BF��B[��Bp��B���B�ffB�  B�33B�ffB���B�  B���B�  B���B晚B�33B���C��CffC  C33C��CL�C L�C$33C)� C.�fC4L�C8L�C=�fCB33CGffCS�C]33Cf��Cp33CzffC��3C��fC�&fC��fC���C�� C�L�C�L�C�Y�C�L�C�33C�&fC�&fC��C�ffC��C�ffC�ffC�@ C�L�C��C�ffC�@ C�&fC�&fD�DfDFfD33D  D��D��D%33D*,�D/&fD4  D9  D>�DC&fDH9�DM�DR  DW3D\  D`��Df�Dk33Dp3Du&fDz  D�c3D���D��3D��3D�\�D��3D���D�fD�FfD��fD�� D��D�I�D�y�D���D��D�I�D��D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@�ffA#33AvffA���Aə�A�ffB��B33B2  BF��B[��Bp��B���B�ffB�  B�33B�ffB���B�  B���B�  B���B晚B�33B���C��CffC  C33C��CL�C L�C$33C)� C.�fC4L�C8L�C=�fCB33CGffCS�C]33Cf��Cp33CzffC��3C��fC�&fC��fC���C�� C�L�C�L�C�Y�C�L�C�33C�&fC�&fC��C�ffC��C�ffC�ffC�@ C�L�C��C�ffC�@ C�&fC�&fD�DfDFfD33D  D��D��D%33D*,�D/&fD4  D9  D>�DC&fDH9�DM�DR  DW3D\  D`��Df�Dk33Dp3Du&fDz  D�c3D���D��3D��3D�\�D��3D���D�fD�FfD��fD�� D��D�I�D�y�D���D��D�I�D��D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��wA�5?A���A���A���A���A��\A�jA�A��yA���A�E�A�(�A��`A�l�A��A���A�t�A��mA��+A�
=A��A�
=A���A��#A�hsA��A�ĜA��A�
=A�VA��A�n�A��A���A��A��A���A�p�A��9A��jA�z�Au/Aj  Ac��AXz�AQ��AH��AD�DA7�#A1G�A+�A(�\A#�TA"~�A��A�+A�A�yA
z�A�mA ff@�r�@�@��@�{@�n�@�bN@�j@�C�@�
=@��m@�
=@�@�?}@��9@���@��-@��@�V@�+@�\)@��D@�A�@��m@��@�"�@�V@�S�@��P@��/@��@�33@���@~$�@s�
@jn�@cdZ@[t�@R��@L��@H �@A7L@;�m@5?}@17L@+��@(A�@!%@��@�^@�@��@12111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��wA�5?A���A���A���A���A��\A�jA�A��yA���A�E�A�(�A��`A�l�A��A���A�t�A��mA��+A�
=A��A�
=A���A��#A�hsA��A�ĜA��A�
=A�VA��A�n�A��A���A��A��A���A�p�A��9A��jA�z�Au/Aj  Ac��AXz�AQ��AH��AD�DA7�#A1G�A+�A(�\A#�TA"~�A��A�+A�A�yA
z�A�mA ff@�r�@�@��@�{@�n�@�bN@�j@�C�@�
=@��m@�
=@�@�?}@��9@���@��-@��@�V@�+@�\)@��D@�A�@��m@��@�"�@�V@�S�@��P@��/@��@�33@���@~$�@s�
@jn�@cdZ@[t�@R��@L��@H �@A7L@;�m@5?}@17L@+��@(A�@!%@��@�^@�@��@12111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B49BL�B^5Bv�B�'B�?B�XB�^B�jB�}B�wB�jB�-B�'B�!B�B�3B�3B�9B�-B�B�-B�LB�B��Bp�BW
B#�BhBB�ZB��BB�RB�B?}B\B�ZB��B�Br�BB
�1B	�ZB	�JB	\)B	�B�sBĜB�?B��B��B��B��B��B�hB�+B|�Bw�Bk�Bq�B� BW
BdZBn�Bm�Bt�B�JB��B�B�!B��B�bB��B	\B	)�B	(�B	/B	H�B	M�B	q�B	r�B	u�B	�B	��B	�-B	�^B	�wB	ȴB	�B	�fB	�B	�B	��B	��B
  B
1B
bB
�B
!�B
-B
5?B
:^B
>wB
D�B
I�B
M�B
R�B
W
B
_;B
hsB
m�B
q�B
v�B
z�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�BL�B^5Bv�B�'B�?B�XB�^B�jB�}B�wB�jB�-B�'B�!B�B�3B�3B�9B�-B�B�-B�LB�B��Bp�BW
B#�BhBB�ZB��BB�RB�B?}B\B�ZB��B�Br�BB
�1B	�ZB	�JB	\)B	�B�sBĜB�?B��B��B��B��B��B�hB�+B|�Bw�Bk�Bq�B� BW
BdZBn}BmwBt�B�JB��B�B�!B��B�HB��B	\B	)�B	(�B	/B	H�B	M�B	q�B	r�B	u�B	� B	��B	�-B	�^B	�wB	ȴB	��B	�fB	�B	�B	��B	��B
  B
1B
bB
�B
!�B
,�B
5?B
:^B
>wB
D�B
I�B
M�B
R�B
W
B
_;B
hsB
m�B
q�B
v�B
z�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201403230017272014032300172720140323001727201608161406172016081614061720160816140617JA  ARFMdecpP7_h                                                                20140310005812  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140310005832  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140310005834  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140310005838  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140310005838  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140310005839  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20140310005839  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20140310005839  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20140310012718                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20140312185543  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140312190454  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140312190455  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140312190500  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140312190500  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140312190500  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20140312190501  CF  PSAL            >���>���?�                  JA  ARGQpump1.0                                                                 20140312190501  CF  TEMP            >���>���?�                  JA  ARUP                                                                        20140312192345                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004509                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140322151727  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140322151727  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050617  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234511                      G�O�G�O�G�O�                