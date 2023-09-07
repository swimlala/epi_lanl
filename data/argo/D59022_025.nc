CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_CALIB       N_LEVELS   6   	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       p2005-06-27T13:42:15Z Decoding & Creation; 2010-04-22T09:34:37Z DMQC & Calibration; 2013-09-20T20:48:14Z Timing;    
references        (http://www.argodatamgt.org/Documentation   comment       	free text      comment_on_resolution         �The profile TEMP and PSAL data resolution can be different than nominal. The data packing algorithm requires lower resolution be used to accommodate high vertical gradients. Values of 0.002, 0.004, & 0.008 are typical.     user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         F   	DATA_TYPE                  conventions       Argo reference table 1     	long_name         	Data type      
_FillValue                    =�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    >    REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    >   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    >   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    >$   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    >4   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  ><   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  >|   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  >�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        >�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?    DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     ?   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    ?(   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    ?,   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     ?0   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     ?P   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     ?p   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    ?�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   axis      T      
_FillValue        A.�~            ?�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    ?�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            ?�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            ?�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            ?�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    ?�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    ?�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ?�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ?�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ?�   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    ?�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ?�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        @�   PRES               
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z      
_FillValue        G�O�      �  @�   PRES_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  A�   PRES_ADJUSTED                  
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z      
_FillValue        G�O�      �  A�   PRES_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  B�   PRES_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     
_FillValue        G�O�      �  B�   TEMP               	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  C�   TEMP_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  D�   TEMP_ADJUSTED                  	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  D�   TEMP_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  E�   TEMP_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�      �  E�   PSAL               	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  F�   PSAL_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  G�   PSAL_ADJUSTED                  	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   
_FillValue        G�O�      �  G�   PSAL_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  H�   PSAL_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�      �  H�   CNDC               	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      units         mhos/m     	valid_min                	valid_max         A     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        8ѷ   
_FillValue        G�O�      �  I�   CNDC_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  J�   CNDC_ADJUSTED                  	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      units         mhos/m     	valid_min                	valid_max         A     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        8ѷ   
_FillValue        G�O�      �  J�   CNDC_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  K�   CNDC_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         mhos/m     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        8ѷ   
_FillValue        G�O�      �  K�   	PARAMETER            
   	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  L�   SCIENTIFIC_CALIB_EQUATION            
   	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT         
   	            	long_name         *Calibration coefficients for this equation     
_FillValue                    P�   SCIENTIFIC_CALIB_COMMENT         
   	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    T�   SCIENTIFIC_CALIB_DATE            
   	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  X�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    Y0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    Y4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    Y8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    Y<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Y@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Y�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    Y�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Y�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        Y�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        Y�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Y�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    Y�Argo profile    3.1 1.2 19500101000000  20050627134215  20140718165306  59022   ARGO EQUIVALENT: CONSORTIUM ON THE OCEANS ROLE IN CLIMATE (CORC)RUSS DAVIS                                                      PRES            TEMP            PSAL            CNDC               A   AO  0041_00745_025                  2C  D   SOLO                            1159                            V0.1; FSI513 22May00            852 @�\�N���8   @�\��l�.@(��S���dkS���1   ARGOS   A   A   F   F   Primary sampling: averaged [data averaged with equal weights into irregular pressure bins, sampled at 0.5 Hz from a SBE41CP]                                                                                                                                       @�  AnffA�ffB
��B2ffBZffB�  B���B���B�ffB�ffB�33B�  C�fC�fC��C#�3C-��C7� CA� CKffCUL�C_33Ci�Cs�C��3C��fC���C�� C��fC���C�� C�ffC�Y�C�@ C�33C��C�  D ��D��D
�fDٚD�3D�fD��D'l�D3�3D@9�DL� DYfDel�Dq�3D~9�D�P 111111111111111111111111111111111111111111111111111111  @@  A&ffA�ffA�B ffBHffBp  B���B���B�ffB�ffB�33B�  CffCffCL�C33C)�C3  C=  CF�fCP��CZ�3Cd��Cn��C}ffC��fC���C�� C�ffC�Y�C�@ C�&fC��C�  C��3C�ٚC�� C��4D��D	�fD��D�3D�fD��D&L�D2�3D?�DK� DW�fDdL�Dp�3D}�D�� 111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�  A���A՟�Aղ-A�ĜA��
Aե�A�t�A�oA�"�A���A��A��Av�jAg��AUAN��ACƨA<��A7�A5+A4��A2��A/C�A*�`A'x�A%XA#p�A v�A�!Av�A�/A��A�A%A�A	�wA5?A|�@�l�@�?}@�o@�b@���@��@�{@���@��T@��j@��@�E�@�@�
=111111111111111111111111111111111111111111111111111111  A��A�  A���A՟�Aղ-A�ĜA��
Aե�A�t�A�oA�"�A���A��A��Av�jAg��AUAN��ACƨA<��A7�A5+A4��A2��A/C�A*�`A'x�A%XA#p�A v�A�!Av�A�/A��A�A%A�A	�wA5?A|�@�l�@�?}@�o@�b@���@��@�{@���@��T@��j@��@�E�@�@�
=111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�B�JB�PB�bB�VB�hB�oB��B	7B��BĜB��B��BG�B
�#B
q�B
ȴB
�B+B{B'�BP�Br�B{�B\)B^5BaHBiyB]/BYBZBT�BQ�B@�B>wB.B"�B{B1BB
��B
�B
�B
�mB
�;B
��B
��B
�
B
�
B
�B
�5B
�mB
�m444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�E�@�M�@�D�@�5?@�C�@�L/@�Z�@�K^@�B[@�@�Y@��@�e�@�I�@�:*@���@���@�M@�O@}�C@{�f@{�@{,�@z��@ys�@w�@@vh
@u��@u�@s�V@sH�@r�@@q�(@p�U@ob�@nE�@l�@k�{@j)�@i#�@g�@f��@ec@d<�@b�c@`��@^-@\�@Z͞@YIR@X@V��@U��@T��444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            CNDC            PRES_ADJUSTED = PRES - measured surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Float pressure sensor corrected for mild pressure drift in DMQC by zeroing the pressure sensor while on the surface.                                                                                   PRES_ADJ_ERR: Manufacturer sensor accuracy               No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy                        PSAL data obtained by FSI sensor biased salty in thermal gradient depths and float fails altimetry comparison when salinity is used (Guinehut, S; 2009: On the Use of Satellite Altimeter Data in Argo Quality Control. JAOT 26: 395 DOI 10.1175/2008JTECHO648.1CNDC comments same as PSAL comments;                                                                                                                                                                                                                            20100422093427201004220934272010042209342720100422093427AO  ARFM                                                                        20050627134215  IP                  G�O�G�O�G�O�                AO  ARGQ                                                                        20050627134215  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20050627134215  QCF$                G�O�G�O�G�O�20000           SI  ARSQSIQCV2.1                                                                20100422093317  CF  PSAL            @@  D��?�  Sensor Failure  SI  ARSQSIQCV2.1                                                                20100422093317  CF  CNDC            @@  D�� @@  Sensor Failure  SI  ARSQOW  V1.0CTD_for_DMQC_2010V01                                            20100422093432  IP                  G�O�G�O�G�O�                SI  ARSQSIQCV2.1                                                                20100422093432  IP                  G�O�G�O�G�O�                SI  ARCAOW                                                                      20100422093437  IP                  G�O�G�O�G�O�                SI  ARDU                                                                        20140718165306  IP                  G�O�G�O�G�O�                SI  ARSQ                                                                        20140718165306  IP                  G�O�G�O�G�O�Timing          SI  ARDU                                                                        20140718165306  IP                  G�O�G�O�G�O�                