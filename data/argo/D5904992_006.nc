CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2015-12-28T06:53:52Z creation;2015-12-31T19:05:59Z conversion to V3.1;2019-09-10T08:34:29Z update;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20151228065352  20190919221514  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131538_006                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @׉zgY  1   @׉~��% @:pbM���ct�����1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�33A(  A{33A�  A�ffA�33B	33B"  B533BH��B\  Bo33B���B���B���B�33B�33B���B�  B�33B���B�  B�33B�  B���C��C��CffC�3C33C��C 33C%��C*��C/� C4�3C9�fC>��CC�fCG��CRL�C[�fCf� CpL�Cz��C�  C�  C�L�C�&fC�Y�C�Y�C�ٚC�  C�Y�C��C�&fC�  C�  C�ٚC��fC��fCѦfC�ٚC�L�C���C��C���C��C�� C��fD�D@ D  D�3D3D�3D�3D%,�D*  D/�D4,�D9  D>  DC33DH  DM  DR3DVٚD\  Da&fDf3DkfDp  Du,�Dz�D�S3D��fD��3D�	�D�@ D��3D���D��D�I�D�� D���D���D�P DԠ D��3D�fD�33D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?���@�ffA)��A|��A���A�33A�  B	��B"ffB5��BI33B\ffBo��B�  B�  B���B�ffB�ffB�  B�33B�ffB�  B�33B�ffB�33B�  C�3C�fC� C��CL�C�3C L�C%�fC*�3C/��C4��C:  C>�3CD  CG�fCRffC\  Cf��CpffCz�fC��C��C�Y�C�33C�ffC�ffC��fC��C�ffC�&fC�33C��C��C��fC��3C��3Cѳ3C��fC�Y�C�ٚC�&fC�ٚC�&fC���C��3D3DFfDfD��D�DٚD��D%33D*fD/  D433D9&fD>&fDC9�DH&fDM&fDR�DV� D\fDa,�Df�Dk�DpfDu33Dz  D�VfD���D��fD��D�C3D��fD���D� D�L�D��3D�� D�  D�S3Dԣ3D��fD�	�D�6fD�3D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A��DA��7A��DA��\A��PA��uA��hA��hA��uA���A��hA��A���A���A�  A�+A�dZA���A�dZA��A��!A�"�A���A��A�+A�9XA���A�VA�r�A��A��9A��TA��A�M�A��TA~v�A|{Av$�AqK�Aj�uAeK�A_�AY�wATjAO�7AI��AF5?A?�;A;�;A6��A2�A/C�A+��A(5?A%�A"z�AM�A�yA��A�+A�-AVAI�A�AA (�@��@�5?@�1@�r�@ʰ!@���@��
@��-@�+@���@��/@��@�33@���@��@�Q�@�/@���@�/@�
=@�G�@��F@��@�z�@}��@z~�@r��@k�m@d��@^{@T�@Kƨ@D�/@>�y@7�w@3t�@.@)%@#"�@��@1'@��@��@@2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A��DA��7A��DA��\A��PA��uA��hA��hA��uA���A��hA��A���A���A�  A�+A�dZA���A�dZA��A��!A�"�A���A��A�+A�9XA���A�VA�r�A��A��9A��TA��A�M�A��TA~v�A|{Av$�AqK�Aj�uAeK�A_�AY�wATjAO�7AI��AF5?A?�;A;�;A6��A2�A/C�A+��A(5?A%�A"z�AM�A�yA��A�+A�-AVAI�A�AA (�@��@�5?@�1@�r�@ʰ!@���@��
@��-@�+@���@��/@��@�33@���@��@�Q�@�/@���@�/@�
=@�G�@��F@��@�z�@}��@z~�@r��@k�m@d��@^{@T�@Kƨ@D�/@>�y@7�w@3t�@.@)%@#"�@��@1'@��@��@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B7LB9XB9XB8RB9XB9XB9XB9XB:^B9XB:^B:^B:^B:^B;dB<jBC�BI�BS�B6FBPB�B�fB��B�B��Bs�BQ�B\B�NBĜB��B}�BXB7LB
��B
�ZB
ÖB
��B
�B
p�B
@�B
�B	�HB	�^B	�VB	p�B	I�B	0!B	\B	B�B�TB��BÖB�jB�B��B��B�7By�Bq�B`BBR�BL�B>wB7LB1'B,B&�B�B�B�B�B!�B-B7LBH�BYBk�B|�B��B��B�jB�B�B	B	�B	&�B	8RB	H�B	XB	iyB	t�B	�+B	�oB	�-B	��B	�NB	�B
B
uB
�B
'�B
1'B
7LB
>wB
D�B
J�B
R�B
XB
]/B
aHB
dZB
hs4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B72B9XB9XB88B9XB9>B9XB9XB:DB9>B:DB:^B:^B:^B;dB<jBC{BI�BS�B6FBPB�B�fB̳B��B��Bs�BQ�BBB�NBĜB�yB}�BW�B72B
��B
�ZB
ÖB
��B
��B
p�B
@�B
�B	�-B	�^B	�<B	p�B	I�B	0!B	\B	�B�wB�TB��B�{B�jB��B��B�mB�By�Bq�B`'BR�BL�B>wB7LB1B,B&�B�BgB�BmB!�B,�B72BH�BYBk�B|�B��B��B�jB��B�B	B	�B	&�B	88B	H�B	XB	i_B	t�B	�B	�TB	�-B	ˬB	�4B	�B
�B
[B
�B
'�B
1B
7LB
>]B
D�B
J�B
R�B
XB
]B
aHB
d@B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=-0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201601110020342016011100203420160111002034201804031230102018040312301020180403123010JA  ARFMdecpV4_b                                                                20151228065352  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151228065352  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151228065353  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151228065353  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151228065353  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20151228065354  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20151228065354  CF  PSAL            ?�  ?�  ?�                  JA  ARGQpump1.0                                                                 20151228065354  CF  TEMP            ?�  ?�  ?�                  JA  ARUP                                                                        20151228070849                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20151231185258  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20151231190557  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20151231190557  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20151231190558  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20151231190559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20151231190559  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20151231190559  CF  PSAL            ?�  ?�  ?�                  JA  ARGQpump1.0                                                                 20151231190559  CF  TEMP            ?�  ?�  ?�                  JA      jafc1.0                                                                 20151231190559                      G�O�G�O�G�O�                JA  ARUP                                                                        20151231191137                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20160101000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARSQJMQC2.0                                                                 20160101000000  CF  TEMP_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20160110152034  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160110152034  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403033010  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190919221514                      G�O�G�O�G�O�                