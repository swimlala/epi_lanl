CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-26T08:52:43Z creation; 2015-10-19T16:05:17Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    8   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    8   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8$   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  8,   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  8l   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     9   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     90   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     9P   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9p   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        ?q   comment_on_resolution         �JULD resolution is 6 minutes, except when JULD = JULD_LOCATION or when JULD = JULD_FIRST_MESSAGE (TRAJ file variable); in that case, JULD resolution is 1 second        9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9|   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >��	4E�        9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        L  :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        L  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ?�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  @t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  E   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  E�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  G�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  J�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  M   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  M�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  O�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  P�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  R�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    S   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  \   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    \4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    \8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    \<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    \@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  \D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    \�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    \�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    \�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         \�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         \�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        \�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    \�Argo profile    3.1 1.2 19500101000000  20090326085243  20200417102504  6900680 BIOArgo                                                         Antoine POTEAU                                                  PRES            TEMP            PSAL               
A   IF  10680034                        2C  D   PROVOR_II                       n/a                             n/a                             841 @����-��1   @����-��@7*�0��c�Fs��1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   B   B   A$��B33B33B)��B5��BA��BLffBY��BfffBs33B|  B���B�33B�ffB���B���B�ffB���B���B�  B�  B���Bƙ�B�ffB�ffB�ffB�33B�ffBꙚBB�33B���C�3CL�C�fC	  CL�C� CffCL�C33C�3C��C"� C%L�C(�C+��C.��C1� C4� C7��C:��C=ffC@  CB  CF  CI� CM  CO�fCR33CU��CW�fC[� C^�fCa�3Cd� ChffCm33CpffCr��CvffCy��C{��C� C�Y�C��fC�@ C���C�L�C���C�L�C�ٚC�&fC���C�Y�C���C�Y�C�� C�&fC��fC��C���C�33C��fC��C�@ C�ffC��fC�33C���C�L�C��3C�33C���C�&fC�� C�&fC���C�@ C��fC�L�C��fC��3C���C�33C���C�33C���C�&fCĦfC�&fCǳ3C�ffC��C�Y�C��C�33D L�D��D�3D�D@ Ds3D%��D,&fD2�fD8s3D>� DE�DK` DQ�fDWٚD^  Dd�fDj��Dp��DvL�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A$��B33B33B)��B5��BA��BLffBY��BfffBs33B|  B���B�33B�ffB���B���B�ffB���B���B�  B�  B���Bƙ�B�ffB�ffB�ffB�33B�ffBꙚBB�33B���C�3CL�C�fC	  CL�C� CffCL�C33C�3C��C"� C%L�C(�C+��C.��C1� C4� C7��C:��C=ffC@  CB  CF  CI� CM  CO�fCR33CU��CW�fC[� C^�fCa�3Cd� ChffCm33CpffCr��CvffCy��C{��C� C�Y�C��fC�@ C���C�L�C���C�L�C�ٚC�&fC���C�Y�C���C�Y�C�� C�&fC��fC��C���C�33C��fC��C�@ C�ffC��fC�33C���C�L�C��3C�33C���C�&fC�� C�&fC���C�@ C��fC�L�C��fC��3C���C�33C���C�33C���C�&fCĦfC�&fCǳ3C�ffC��C�Y�C��C�33D L�D��D�3D�D@ Ds3D%��D,&fD2�fD8s3D>� DE�DK` DQ�fDWٚD^  Dd�fDj��Dp��DvL�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%B��B��B��B��B��B��B�JBz�BZBK�B<jB@�BI�B\)B^5BaHBt�B{�B�B�PB�\B�bB�hB��B��B��B��B��B��B�B�B�'B�9B�?B�RB�^B�dB�}BB��BBÖBĜBĜB�}B�^B�'B��B��B��B�bB� Bq�BgmBbNBK�B9XB49B2-B,BuB��B�B�BB��B��B�^B�hBx�BJ�B(�B
�
B
��B
�B
�B
R�B
1'B
+B
)�B
$�B
�B
	7B	�B	ƨB	��B	m�B	`BB	N�B	<jB	0!B	 �B	+B��B�B�B�B��B	  B��B�B�`B�/B�B��B��B��BÖBB�}B�^B�B�B��B��B��B��B��B��B��B��B��B|�BhsBaHA�1B_;BaHBe`Bp�B��B�!B��B�;B�B	%B	2-B	F�B	K�B	Q�B	cTB	~�B	��B	�FB	�qB	��B	�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 G�O�B��B��B��B��B��B��B�JBz�BZBK�B<jB@�BI�B\)B^5BaHBt�B{�B�B�PB�\B�bB�hB��B��B��B��B��B��B�B�B�'B�9B�?B�RB�^B�dB�}BB��BBÖBĜBĜB�}B�^B�'B��B��B��B�bB� Bq�BgmBbNBK�B9XB49B2-B,BuB��B�B�BB��B��B�^B�hBx�BJ�B(�B
�
B
��B
�B
�B
R�B
1'B
+B
)�B
$�B
�B
	7B	�B	ƨB	��B	m�B	`BB	N�B	<jB	0!B	 �B	+B��B�B�B�B��B	  B��B�B�`B�/B�B��B��B��BÖBB�}B�^B�B�B��B��B��B��B��B��B��B��B��B|�BhsBaHG�O�B_;BaHBe`Bp�B��B�!B��B�;B�B	%B	2-B	F�B	K�B	Q�B	cTB	~�B	��B	�FB	�qB	��B	�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
B[�A�r�A�bNA�bNA�VA�I�A�;dA�A���A�^5A��A�M�A��A���A�z�A��yA��A��yA� �A�~�A���A��A��A��#A�dZA�1A�1A��
A��A�Q�A��A�dZA�1A��-A�v�A�XA�VA��jA�E�A���A��7A�A�Q�A��A��+A�A�A� �A��A��A�bNA��hA�+A�ffA�VA���A�S�A�S�A��9A�bA���A�(�A���A��A���A�Q�A�x�A�"�A���A��A�|�A���A��+A���A��A��DA�-A��-A�A~VA~ �A}��A|�jAz�Aw��Ar=qAl��Ad��Ac|�Aa�wA^v�A]oA[33AU��AR��AN�AK��AK
=AJ�uAJ  AF�DACl�AA`BA?x�A>n�A="�A<~�A;��A9�^A9hsA8�yA8 �A3`BA0��A/��A/�A.  A,��A+hsA*��A(�`A(  A&��AhsA
�9AO�A�;d@��@�+@�ƨ@�E�@�G�@�@��@�1@�+@��@��/@��y@�~�@�bN@��`@�~�@�33@�`B@���@�ȴ@���411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 G�O�A�r�A�bNA�bNA�VA�I�A�;dA�A���A�^5A��A�M�A��A���A�z�A��yA��A��yA� �A�~�A���A��A��A��#A�dZA�1A�1A��
A��A�Q�A��A�dZA�1A��-A�v�A�XA�VA��jA�E�A���A��7A�A�Q�A��A��+A�A�A� �A��A��A�bNA��hA�+A�ffA�VA���A�S�A�S�A��9A�bA���A�(�A���A��A���A�Q�A�x�A�"�A���A��A�|�A���A��+A���A��A��DA�-A��-A�A~VA~ �A}��A|�jAz�Aw��Ar=qAl��Ad��Ac|�Aa�wA^v�A]oA[33AU��AR��AN�AK��AK
=AJ�uAJ  AF�DACl�AA`BA?x�A>n�A="�A<~�A;��A9�^A9hsA8�yA8 �A3`BA0��A/��A/�A.  A,��A+hsA*��A(�`A(  A&��AhsA
�9AO�G�O�@��@�+@�ƨ@�E�@�G�@�@��@�1@�+@��@��/@��y@�~�@�bN@��`@�~�@�33@�`B@���@�ȴ@���411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    202004171025042020041710250420200417102504  IF  ARGQSCOO1.2                                                                 20091001150611  CF  TEMP            B33B33@�                  IF  ARGQSCOO1.2                                                                 20091001150611  CF  TEMP            C�Y�C�Y�@�                  IF  ARGQSCOO1.2                                                                 20091001150611  CF  TEMP            C�33C�33@�                  IF  ARGQSCOO1.2                                                                 20091001150611  CF  PSAL            A$��A$��?�                  IF  ARGQSCOO1.2                                                                 20091001150611  CF  PSAL            B33B33@�                  IF  ARGQSCOO1.2                                                                 20091001150611  CF  PSAL            C�Y�C�Y�@�                  IF  ARGQSCOO1.2                                                                 20091001150611  CF  PSAL            C�33C�33@�                  IF  ARGQCOAR1.0                                                                 20111010073514  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073514  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109162450  QC                  G�O�G�O�G�O�                        CORA                                                                    20090815220747  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20141028090015  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103518  QCF$TEMP            G�O�G�O�G�O�4               IF  CODMCOOA6.2 DMQCGL01                                                        20140818104840  QCP$PSAL            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818104546  QCF$PSAL            G�O�G�O�G�O�4               IF      COFC2.7                                                                 20151019160517                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417102504  IP  PSAL            A$��DvL�G�O�                