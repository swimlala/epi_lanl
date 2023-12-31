CDF   ,   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2012-05-09T10:04:11Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:49:42Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20120509100411  20161129232528  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               "A   JA  P7_97922_034                    2C  D   PROVOR                          09027                           5815A03                         841 @�=:i�� 1   @�=<���@3x�t�j�d#���+1   ARGOS   B   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                           @�  A33AnffA�ffAř�A���B33B"��B8��BJffB_��BrffB�ffB�ffB�  B���B���B�ffB�  B���B�  B�  B�33B�  B���C�C�3CffC33C33C� C   C%�C*L�C.L�C3�fC833C=�fCBL�CF�fCR  C[��Cg33Cp�Cz�C�L�C�ٚC�Y�C��3C�&fC�  C��3C���C���C�ffC�33C�33C�  C�33C�@ C�33C�ffC�ffC�@ C�s3C�ffC��C�  C�L�C�Y�DfD3DFfD,�D�D&fD �D%fD*�D/&fD4&fD9,�D>,�DC3DH�DM&fDQ��DW�D\3D`�3De��DjٚDo��Du&fDz@ D�` D���D���D�fD�P D��fD�� D�3D�FfD�vfD�ɚD��D�I�DԜ�D��3D��D�S3D� D��fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�@�  A33AnffA�ffAř�A���B33B"��B8��BJffB_��BrffB�ffB�ffB�  B���B���B�ffB�  B���B�  B�  B�33B�  B���C�C�3CffC33C33C� C   C%�C*L�C.L�C3�fC833C=�fCBL�CF�fCR  C[��Cg33Cp�Cz�C�L�C�ٚC�Y�C��3C�&fC�  C��3C���C���C�ffC�33C�33C�  C�33C�@ C�33C�ffC�ffC�@ C�s3C�ffC��C�  C�L�C�Y�DfD3DFfD,�D�D&fD �D%fD*�D/&fD4&fD9,�D>,�DC3DH�DM&fDQ��DW�D\3D`�3De��DjٚDo��Du&fDz@ D�` D���D���D�fD�P D��fD�� D�3D�FfD�vfD�ɚD��D�I�DԜ�D��3D��D�S3D� D��fD��341111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A��A�A�|�A�G�A�$�A��A�VAź^A�I�A�;dA��AîAËDA�v�AÁA�x�A�$�A��A��yA²-A�Q�A�A�A�  A��\A�dZA��A�r�A�ƨA�bA�|�A���A�z�A�9XA���A��hA�"�A��!A�9XA�1'A��wA�oA�dZA���A���A��Ap��Ah�A`��AUXAMO�AD$�A;+A3��A/&�A+��A&r�A!�TA�#AbAK�A5?At�A	/A�yA ��@���@�@��H@�G�@׶F@�A�@ΰ!@�hs@���@�"�@�&�@��;@�;d@�Q�@�x�@��@��@���@��@�V@�^5@�r�@��@�"�@��9@���@�j@��\@�r�@�J@��@|�j@q%@hĜ@_�w@W+@P�u@H�`@B�@<Z@5�@2�@-?}@)hs@$��@  �@�!@�j@7L@{21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�A��A�A�|�A�G�A�$�A��A�VAź^A�I�A�;dA��AîAËDA�v�AÁA�x�A�$�A��A��yA²-A�Q�A�A�A�  A��\A�dZA��A�r�A�ƨA�bA�|�A���A�z�A�9XA���A��hA�"�A��!A�9XA�1'A��wA�oA�dZA���A���A��Ap��Ah�A`��AUXAMO�AD$�A;+A3��A/&�A+��A&r�A!�TA�#AbAK�A5?At�A	/A�yA ��@���@�@��H@�G�@׶F@�A�@ΰ!@�hs@���@�"�@�&�@��;@�;d@�Q�@�x�@��@��@���@��@�V@�^5@�r�@��@�"�@��9@���@�j@��\@�r�@�J@��@|�j@q%@hĜ@_�w@W+@P�u@H�`@B�@<Z@5�@2�@-?}@)hs@$��@  �@�!@�j@7L@{41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�FB
�dB
�^B
�XB
�XB
�RB
�XB
�XB
�3B
�BDB�JB��B��B�B�}B�}BÖB��B�NBB
=BhB\B33By�Bs�BgmB�1B��B}�Bs�Bp�BhsBS�B2-B\BB�TB�;B�fBB�BoB
t�B	�)B	�%B	O�B	-B	B	1B��B��B�B��B�`B�wB�RBŢB��B�B�BB�ZB�XB�\B�PB�1B�+B�B�B�B�3B��B�B	B	�B	+B	49B	VB	m�B	�B	�hB	��B	�?B	�qB	ǮB	��B	�BB	�fB	�B	��B	��B	��B
B
%B
	7B
uB
�B
#�B
(�B
0!B
7LB
<jB
C�B
I�B
L�B
R�B
W
B
\)B
_;B
dZB
hsB
l�B
r�B
u�B
x�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�B
�dB
�^B
�XB
�XB
�RB
�XB
�XB
�3B
�BDB�JB��B��B�B�}B�}BÖB��B�NBB
=BhB\B33By�Bs�BgmB�1B��B}�Bs�Bp�BhsBS�B2-B\BB�TB�;B�fBB�BoB
t�B	�CB	�%B	O�B	-B	3B	KB��B��B�B��B�zB��B�lBŢB��B�+B�\B�tB�rB�vB�jB�KB�EB�-B�B�9B�MB��B��B	-B	�B	+B	49B	VB	m�B	�B	��B	��B	�?B	�qB	ǮB	��B	�BB	�fB	��B	��B	��B	��B
B
%B
	7B
uB
�B
#�B
(�B
0!B
7LB
<�B
C�B
I�B
L�B
R�B
W
B
\CB
_;B
dZB
hsB
l�B
r�B
u�B
x�41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281651492013072816514920130728165149201608161353302016081613533020160816135330JA  ARFMdecpP7_g                                                                20120509100407  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120509100411  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120509100412  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120509100417  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcpt19b                                                                20120509100417  QCF$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120509100417  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8d                                                                20120509100417  QCF$                G�O�G�O�G�O�             100JA  ARGQrqcpt16b                                                                20120509100417  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20120509101731                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120511190223  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120511190656  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120511190657  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20120511190702  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcpt19b                                                                20120511190702  QCF$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120511190702  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8d                                                                20120511190702  QCF$                G�O�G�O�G�O�             100JA  ARGQrqcpt16b                                                                20120511190702  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20120511191427                      G�O�G�O�G�O�                JA  ARFMdecpP7_g                                                                20120914041748  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20120914041900  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20120914041901  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20120914041906  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20120914041906  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20120914041907  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20120914041907  CF  PSAL                    ?�                  JA  ARGQpump1.0                                                                 20120914041907  CF  TEMP                    ?�                  JA  ARUP                                                                        20120914043523                      G�O�G�O�G�O�                JM  ARFMjmbcP7_g                                                                20121002060817  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20121002060906  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20121002060909  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20121002060914  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20121002060914  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20121002060914  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20121002060915  CF  PSAL                    ?�                  JA  ARGQpump1.0                                                                 20121002060915  CF  TEMP                    ?�                  JA  ARUP                                                                        20121002072119                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002515                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20120907000000  CF  TEMP_ADJUSTED_QC       G�O�                JM  ARSQJMQC2.0                                                                 20120912000000  CF  PRES_ADJUSTED_QC       G�O�                JM  ARCAJMQC2.0                                                                 20130728075149  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728075149  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816045330  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232528                      G�O�G�O�G�O�                