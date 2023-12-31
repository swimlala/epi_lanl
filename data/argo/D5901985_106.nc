CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2014-04-29T00:56:17Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:41:02Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20140429005617  20161129234513  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA  P7_97922_106                    2C  D   PROVOR                          09027                           5815A03                         841 @��;-��1   @��<n�� @5WKƧ��c�7KƧ�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�  A,��Ay��A���A���A�ffBffB ffB4  BF��BZffBq��B�  B�  B���B�33B�33B���B�33B�33B�33B���B�  B���B���C� C��C  C�3C�3C�fC L�C%��C)�fC/�C4L�C9� C>��CC�CG��CR33C\�Cf33Cp�Cy�3C�@ C���C�s3C��C��3C�&fC�L�C��C��C��3C�@ C��fC�33C�&fC��C̦fC�L�C�ffC܀ C�s3C�33C�&fC�33C�ٚC�@ D�D�D�D� D�D�3D   D%FfD*,�D/  D3��D93D>  DC  DG��DM  DR  DW33D\,�Da3Df�Dj��Dp3Du,�DzfD�I�D�� D��fD�  D�L�D�� D��fD�	�D�6fD�� D��3D�fD�I�DԖfDڶfD�3D�L�D�3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�  A,��Ay��A���A���A�ffBffB ffB4  BF��BZffBq��B�  B�  B���B�33B�33B���B�33B�33B�33B���B�  B���B���C� C��C  C�3C�3C�fC L�C%��C)�fC/�C4L�C9� C>��CC�CG��CR33C\�Cf33Cp�Cy�3C�@ C���C�s3C��C��3C�&fC�L�C��C��C��3C�@ C��fC�33C�&fC��C̦fC�L�C�ffC܀ C�s3C�33C�&fC�33C�ٚC�@ D�D�D�D� D�D�3D   D%FfD*,�D/  D3��D93D>  DC  DG��DM  DR  DW33D\,�Da3Df�Dj��Dp3Du,�DzfD�I�D�� D��fD�  D�L�D�� D��fD�	�D�6fD�� D��3D�fD�I�DԖfDڶfD�3D�L�D�3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʲ-AʁA�l�A�XA�Q�A�O�A�7LA�-A���A���AļjA�l�A���A���A��A�z�A�7LA��-A�^5A��A�p�A�VA���A��7A�;dA�=qA�jA���A��PA�n�A�p�A�
=A��wA�r�A��DA��A��/A���A��RA�bA�A�oA}Av��Aq/Ait�AY�AN$�A>�A3�FA/
=A*�DA&-A+A��A�A�A�DA
A�AA�A`BA ��@��@�^5@���@���@�G�@��m@�\@�@�"�@��m@�5?@�-@�5?@�E�@��D@��@��@��/@��@���@�~�@�9X@��T@��D@��-@�l�@�j@���@�=q@���@�=q@���@��@��@yx�@m`B@c�@]?}@VV@Nff@H  @A�^@8��@0�u@)�^@$��@ ��@(�@b@@�9@/2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʲ-AʁA�l�A�XA�Q�A�O�A�7LA�-A���A���AļjA�l�A���A���A��A�z�A�7LA��-A�^5A��A�p�A�VA���A��7A�;dA�=qA�jA���A��PA�n�A�p�A�
=A��wA�r�A��DA��A��/A���A��RA�bA�A�oA}Av��Aq/Ait�AY�AN$�A>�A3�FA/
=A*�DA&-A+A��A�A�A�DA
A�AA�A`BA ��@��@�^5@���@���@�G�@��m@�\@�@�"�@��m@�5?@�-@�5?@�E�@��D@��@��@��/@��@���@�~�@�9X@��T@��D@��-@�l�@�j@���@�=q@���@�=q@���@��@��@yx�@m`B@c�@]?}@VV@Nff@H  @A�^@8��@0�u@)�^@$��@ ��@(�@b@@�9@/2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBPBDBPBVB"�B-B/BA�B��B<jB`BB`BB�Bz�B��B��B��B��B�JBz�BcTBXBI�B#�B"�B�ZB�B�mB�B�qB��Bl�B(�B�B�B�?Be`B33BVB
��B
�bB
33B
+B	�BB	�B	I�B	�B�B�}B�'B��B��B�hB�DB�7B�B�B�JB�+B�-BB��B��B�/B�ZB�B�B��B��B	B�B	\B	 �B	33B	33B	6FB	9XB	C�B	P�B	aHB	n�B	t�B	�7B	�%B	�hB	��B	�B	�XB	ĜB	ɺB	��B	�B	�;B	�B	�B	��B
bB
�B
 �B
!�B
)�B
2-B
9XB
C�B
L�B
T�B
[#B
`BB
e`B
k�B
o�B
q�B
v�B
z�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�BPBDBPBVB"�B-B/BA�B��B<jB`BB`BB�Bz�B��B��B��B��B�JBz�BcTBXBI�B#�B"�B�ZB�B�mB�B�qB��Bl�B(�B�B�B�?Be`B33BVB
��B
�bB
33B
+B	�BB	�B	I�B	�B�B�}B�'B��B��B�hB�DB�7B�B�B�JB�+B�GBB��B��B�/B�tB�B�B��B��B	B�B	\B	 �B	33B	33B	6FB	9XB	C�B	P�B	aHB	n�B	t�B	�7B	�%B	�hB	��B	�B	�XB	ĜB	ɺB	��B	�B	�;B	�B	�B	��B
HB
�B
 �B
!�B
)�B
2-B
9XB
C�B
L�B
T�B
[#B
`BB
e`B
k�B
o�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201405120017382014051200173820140512001738201608161407212016081614072120160816140721JA  ARFMdecpP7_h                                                                20140429005557  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140429005617  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140429005619  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140429005623  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140429005623  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140429005624  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20140429005624  CF  PSAL            ?�  ?�  ?�                  JA  ARGQpump1.0                                                                 20140429005624  CF  TEMP            ?�  ?�  ?�                  JA  ARUP                                                                        20140429011957                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20140501185519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140501190432  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140501190433  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140501190437  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140501190438  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140501190438  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20140501190438  CF  PSAL            ?�  ?�  ?�                  JA  ARGQpump1.0                                                                 20140501190438  CF  TEMP            ?�  ?�  ?�                  JA  ARUP                                                                        20140501191910                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004510                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140511151738  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140511151738  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050721  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234513                      G�O�G�O�G�O�                