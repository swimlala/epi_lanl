CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      history       X2009-03-25T14:33:23Z creation; 2015-10-19T16:06:20Z last update (coriolis COFC software)   comment_dmqc_operator         DPRIMARY | https://orcid.org/0000-0002-3512-2070 | Saout-Grit, Glazeo      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        =���   axis      Z        X  :�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ?�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ED   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  E�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  H4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  H�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  K$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  M|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  N   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Pl   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  Q   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  S\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    S�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Y�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  \�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    \�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    \�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    \�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    \�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  \�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ]   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ]   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ]   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ],   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ]0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ]4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ]8Argo profile    3.1 1.2 19500101000000  20090325143323  20200417103158  6900681 BIOArgo                                                         Antoine POTEAU                                                  PSAL            PRES            TEMP               	A   IF  10680447                        2C  D   PROVOR_II                       n/a                             n/a                             841 @���[1   @���[@6��A���c�qu�!�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   B   B   A$��B��BffBffB*ffB3��BD��BO33BZ  Bd��Bq��Bv  B~ffB�33B�  B���B���B���B���B���B�ffB�33B�ffB���Bƙ�B�ffB�ffBי�Bޙ�B���B�  B�ffB���B���C  CffC��C
��CL�CL�CffCL�C�CffC33C"33C&33C(L�C+�C.� C2�3C4� C7  C:33C=ffC@� CC�CF��CI33CLL�COffCQ�fCV  CXffC[ffC^��Ca� CeL�Cg� CjL�CmffCp��CsL�Cv�CyffC|ffC~��C���C��3C�Y�C���C�&fC�� C���C��3C�33C��fC��C�33C�L�C�� C�@ C���C�33C���C�ffC���C�� C�  C�  C���C�s3C���C�@ C���C�L�C��fC�&fC�� C�@ C�� C�Y�C���C�@ C�� C�33C��3C�&fC��fC��C�ٚC�@ CĦfC��Cǌ�C�Y�C���C癚C�  D @ D�fD��D` D��DFfD�3D%�fD,ffD2Y�D8��D>� DEffDKFfDQ�fDW��D^3DdS3Dj�3Dp�3Dw33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A$��B��BffBffB*ffB3��BD��BO33BZ  Bd��Bq��Bv  B~ffB�33B�  B���B���B���B���B���B�ffB�33B�ffB���Bƙ�B�ffB�ffBי�Bޙ�B���B�  B�ffB���B���C  CffC��C
��CL�CL�CffCL�C�CffC33C"33C&33C(L�C+�C.� C2�3C4� C7  C:33C=ffC@� CC�CF��CI33CLL�COffCQ�fCV  CXffC[ffC^��Ca� CeL�Cg� CjL�CmffCp��CsL�Cv�CyffC|ffC~��C���C��3C�Y�C���C�&fC�� C���C��3C�33C��fC��C�33C�L�C�� C�@ C���C�33C���C�ffC���C�� C�  C�  C���C�s3C���C�@ C���C�L�C��fC�&fC�� C�@ C�� C�Y�C���C�@ C�� C�33C��3C�&fC��fC��C�ٚC�@ CĦfC��Cǌ�C�Y�C���C癚C�  D @ D�fD��D` D��DFfD�3D%�fD,ffD2Y�D8��D>� DEffDKFfDQ�fDW��D^3DdS3Dj�3Dp�3Dw33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��B8R�B�+A�v�B�B~�B~�B~�Bw�Bk�B[#B>wB?|�B<jB?}BO�BS�BYBo�B�7B�bB��B��B��B��B��B�B�B�B�B�B�B�B�B�B��B��B��B��B�B�'B�dB�wBBɺB��B�
B�B�
B��BB�XB�LB�RB�LB�B��B��B�PB{�Bt�BdZBS�B=qB(�BhB+B��B�mB�`B�HB��B�B��B�BgmB[#BM�B�B
�sB
��B
�jB
�B
��B
v�B
]/B
M�B
@�B
%�B
�B	��B	�mB	�NB	�/B	ƨB	�9B	�DB	x�B	aHB	K�B	:^B	+B�B�)B��BÖBŢBƨBǮBƨB�jB��B�uB�hB�{B�{B�hB�bB�\B�PB�1B�DB�=B�B� B{�B� Bz�Bw�Bl�BffBp�B�P@9XB��B��B��B	uB	,B	9XB	XB	gmB	y�B	�PB	��B	��B	�B	�?B	��B	ŢB	��414111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  G�O�B�+G�O�B�B~�B~�B~�Bw�Bk�B[#B>wG�O�B<jB?}BO�BS�BYBo�B�7B�bB��B��B��B��B��B�B�B�B�B�B�B�B�B�B��B��B��B��B�B�'B�dB�wBBɺB��B�
B�B�
B��BB�XB�LB�RB�LB�B��B��B�PB{�Bt�BdZBS�B=qB(�BhB+B��B�mB�`B�HB��B�B��B�BgmB[#BM�B�B
�sB
��B
�jB
�B
��B
v�B
]/B
M�B
@�B
%�B
�B	��B	�mB	�NB	�/B	ƨB	�9B	�DB	x�B	aHB	K�B	:^B	+B�B�)B��BÖBŢBƨBǮBƨB�jB��B�uB�hB�{B�{B�hB�bB�\B�PB�1B�DB�=B�B� B{�B� Bz�Bw�Bl�BffBp�B�PG�O�B��B��B��B	uB	,B	9XB	XB	gmB	y�B	�PB	��B	��B	�B	�?B	��B	ŢB	��414111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  G�O�<#�
G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
A���AӅA��wA�A��/A��
A���A�v�Aщ7A�^5A�n�BwT�A�z�A��A�l�A�/A��PA��A���A�(�A��PA��A�M�A�\)A��A���A��!A���A�C�A��!A�K�A��`A��DA��mA���A�9XA���A��-A�&�A��jA�n�A�;dA�  A��A�K�A���A�K�A��uA�hsA�ƨA�/A�oA��`A��\A���A���A�Q�A�
=A��#A�VA��A���A��7A���A�VA���A��#A�&�A��`A���A��7A�(�A�S�A�hsA��uA���A���A��`A��jA�oA���A�1A�1'A��jA�"�A��A�x�A}
=A{�AwC�At�/At�As�FAqO�Am�;Agl�AeS�Ab  A_XA\��AT~�AM�PAIADM�AA�A?�;A?�A> �A<��A:M�A5/A0�A.  A-"�A+;dA*��A*$�A)�A'7LA%p�A$��A#�
A"v�A E�A|�AK�A
=AS�@���@��@��/@�=qB�s@Ɨ�@��P@��R@���@��@��u@��@�ƨ@���@���@�7L@�r�@���@���@�
=@���@�J414111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  G�O�AӅG�O�A�A��/A��
A���A�v�Aщ7A�^5A�n�G�O�A�z�A��A�l�A�/A��PA��A���A�(�A��PA��A�M�A�\)A��A���A��!A���A�C�A��!A�K�A��`A��DA��mA���A�9XA���A��-A�&�A��jA�n�A�;dA�  A��A�K�A���A�K�A��uA�hsA�ƨA�/A�oA��`A��\A���A���A�Q�A�
=A��#A�VA��A���A��7A���A�VA���A��#A�&�A��`A���A��7A�(�A�S�A�hsA��uA���A���A��`A��jA�oA���A�1A�1'A��jA�"�A��A�x�A}
=A{�AwC�At�/At�As�FAqO�Am�;Agl�AeS�Ab  A_XA\��AT~�AM�PAIADM�AA�A?�;A?�A> �A<��A:M�A5/A0�A.  A-"�A+;dA*��A*$�A)�A'7LA%p�A$��A#�
A"v�A E�A|�AK�A
=AS�@���@��@��/@�=qG�O�@Ɨ�@��P@��R@���@��@��u@��@�ƨ@���@���@�7L@�r�@���@���@�
=@���@�J414111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  G�O�;oG�O�;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPSAL            PRES            TEMP            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustement was necessary. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 1.1,  -CTD2018V01 & ARGO2018V01 -                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              202004171031582020041710315820200417103158  IF  ARGQSCOO1.2                                                                 20091001150929  CF  TEMP            A$��B��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  TEMP            Bq��Bq��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  TEMP            B~ffB~ff@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  TEMP            D��D��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  TEMP            D��D��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  PSAL            B��B��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  PSAL            BffBff@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  PSAL            Bq��Bq��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  PSAL            B~ffB~ff@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  PSAL            D��D��@�                  IF  ARGQSCOO1.2                                                                 20091001150929  CF  PSAL            D��D��@�                  IF  ARGQSCOO1.2                                                                 20091001151219  CF  TEMP            A$��A$��?�                  IF  ARGQCOAR1.0                                                                 20111010073751  QCP$                G�O�G�O�G�O�00840           IF  ARGQCOAR1.0                                                                 20111010073751  QCF$                G�O�G�O�G�O�00000           IF      SCOO1.4                                                                 20130109163605  QC                  G�O�G�O�G�O�                        CORA                                                                    20090815080848  SVP                 G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103651  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818103518  QCP$TEMP            G�O�G�O�G�O�                IF  CODMCOOA6.2 DMQCGL01                                                        20140818104546  QCP$PSAL            G�O�G�O�G�O�                IF      COFC2.7                                                                 20151019160620                      G�O�G�O�G�O�                IF  ARSQOW  1.1 CTD2018V01 & ARGO2018V01                                        20200417103158  IP  PSAL            A$��Dw33G�O�                