CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:33Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140833  20181024140833  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�W��1   @��e'ҏ�@5V�+J�c��hr�!1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@ffBG��BO33BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC'�fC)�fC,  C.  C/�fC2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  Dy�D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D*��D+y�D+��D,y�D-  D-� D.  D.�fD/fD/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6fD6�fD7  D7� D8  D8� D9fD9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DC��DD� DE  DE�fDFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DKy�DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DTfDT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw` Dy�RD�H�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J=q@�Q�@�Q�A(�A$(�AB�\Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)p�B1
=B9
=BAp�BH��BP=pBX��Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B�Q�B��B��B��BąB�Q�B̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C (�CB�CB�CB�C\)C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&(�C((�C*(�C,B�C.B�C0(�C2B�C4B�C6B�C8\)C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\(�C^B�C`B�CbB�CdB�CfB�ChB�CjB�Cl(�Cn(�CpB�CrB�CtB�CvB�CxB�CzB�C|\)C~\)C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�{C�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�{C�!HC�.C�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D
>D��D�D�>D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�>D�D��D�D��D
D��D�D��D�D��D�D��D�D��D�D��D �D ��D!
>D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'�>D(�D(��D)�D)��D*�D*��D+
>D+�>D,
>D,�>D-�D-��D.�D.�
D/
D/�
D0�D0��D1�D1��D2�D2��D3�D3��D4�D4�
D5�D5��D6
D6�
D7�D7��D8�D8��D9
D9�
D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�>DD
>DD��DE�DE�
DF
DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK
>DK�>DL�DL��DM�DM�
DN�DN��DO�DO��DP�DP��DQ�DQ�
DR�DR��DS�DS��DT
DT��DU�DU��DV�DV�>DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^
D^��D_�D_��D`�D`��Da�Da��Db
Db��Dc�Dc�>Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di
Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dwp�Dy��D�P�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��HA��;A��/A��#A��HA��;A��`A��`A��yA��yA��mA��yA��A��A��A��A��A��A��A��yA��TA��HA��/AԮAа!A��A�`BA�I�A��A� �A��A���A��FA���A�A��mA�x�A�7LA�~�A���A�l�A�  A��TA�hsA�oA� �A��-A�?}A���A��+A�?}A�ȴA�+A�(�A�ffA���A���A��uA�JA��hA�r�A�I�A���A���A���A�K�A���A�$�A���A���A��-A�S�A�
=A���A�ZA��A���A�A�A�M�A~ȴA~{A|v�Azr�Ayx�AxE�AwoAu7LAq��Ao
=Am�AlAj��Ah��Ac��Aa��A`JA_dZA^�/A]�7A[��A[�7A[XA[VAZȴAY�-AX��AX��AV��AU&�ATM�AS�hAQ�;AP-AM�AK��AJVAH�AG/AE/A@�A=`BA;S�A:v�A9�A9�PA9G�A8��A8A6�HA5�hA4(�A2�`A0�RA//A-/A,A�A+��A*�A)�;A(�!A'K�A%�hA$ȴA#��A"�+A!�A!�A ĜA jA?}A��A�A
=A�hA�RA  AK�A�HA��AbA��A��AoA�hA��A=qA��Ax�AoAĜAE�A�^Ap�A�A�;A��Al�AA��AI�A�AJA��A�7AG�AoA
��A
�A	�PA�jA�;AC�A�\AI�AE�A�A��AoA��A�DAZA�R@�S�@�{@�&�@��P@���@�(�@���@���@��@�ƨ@�-@�hs@��/@�z�@���@�~�@�9@��;@�V@�V@�9X@�33@�-@�^@�/@�Q�@�|�@���@�!@�$�@��@�r�@�+@޸R@�{@۾w@ڗ�@ى7@�1@��@�/@�  @�K�@�ȴ@��@Ѳ-@�7L@мj@��@θR@���@��@̓u@�  @˥�@�o@���@�E�@���@ț�@�dZ@���@�=q@�&�@�I�@�t�@�@¸R@�=q@��@��@�1'@��m@��F@�dZ@��+@�@�O�@���@�Ĝ@�z�@�9X@��;@�"�@���@��@�G�@��@�1'@�n�@��@�j@�?}@���@�33@��P@��w@�\)@��@��@���@���@���@���@��@�x�@�hs@�O�@�?}@�7L@�&�@�V@��@�j@�"�@�^5@��-@��7@�p�@�O�@��@��9@�(�@��
@���@��@���@�5?@�p�@�Ĝ@�z�@�Q�@� �@�  @��@��@��@�@��y@�ȴ@���@�v�@�M�@�$�@�@���@��@��@���@���@�1'@�  @�C�@��T@�x�@��@��`@�Q�@���@��P@�dZ@���@�v�@���@�Ĝ@�r�@�Z@�  @��@�l�@�o@���@�n�@�M�@��@��h@��@�p�@�G�@��`@�I�@�9X@� �@��@��F@�\)@���@�M�@�=q@��^@��h@��#@��#@��#@���@���@��@�b@��F@�  @�(�@��@��;@�|�@�t�@��F@��@�l�@�\)@�;d@��H@��@��H@��!@�~�@�~�@��+@�5?@�$�@�@��@���@��h@�`B@���@��@�j@�Z@�I�@�A�@�9X@��@��m@���@�ƨ@�ƨ@��w@��w@��@��@�l�@�dZ@�"�@���@���@�~�@�=q@��@���@�p�@�7L@��@���@��@���@���@�|�@�S�@�o@���@���@�ff@�=q@�{@��@���@���@��h@��/@�Ĝ@��u@��D@�z�@�j@�Q�@�9X@��@�1@��@��
@���@�;d@�@��H@��\@�~�@�~�@�n�@�-@�{@��@���@�;d@vq�@`y>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��HA��;A��/A��#A��HA��;A��`A��`A��yA��yA��mA��yA��A��A��A��A��A��A��A��yA��TA��HA��/AԮAа!A��A�`BA�I�A��A� �A��A���A��FA���A�A��mA�x�A�7LA�~�A���A�l�A�  A��TA�hsA�oA� �A��-A�?}A���A��+A�?}A�ȴA�+A�(�A�ffA���A���A��uA�JA��hA�r�A�I�A���A���A���A�K�A���A�$�A���A���A��-A�S�A�
=A���A�ZA��A���A�A�A�M�A~ȴA~{A|v�Azr�Ayx�AxE�AwoAu7LAq��Ao
=Am�AlAj��Ah��Ac��Aa��A`JA_dZA^�/A]�7A[��A[�7A[XA[VAZȴAY�-AX��AX��AV��AU&�ATM�AS�hAQ�;AP-AM�AK��AJVAH�AG/AE/A@�A=`BA;S�A:v�A9�A9�PA9G�A8��A8A6�HA5�hA4(�A2�`A0�RA//A-/A,A�A+��A*�A)�;A(�!A'K�A%�hA$ȴA#��A"�+A!�A!�A ĜA jA?}A��A�A
=A�hA�RA  AK�A�HA��AbA��A��AoA�hA��A=qA��Ax�AoAĜAE�A�^Ap�A�A�;A��Al�AA��AI�A�AJA��A�7AG�AoA
��A
�A	�PA�jA�;AC�A�\AI�AE�A�A��AoA��A�DAZA�R@�S�@�{@�&�@��P@���@�(�@���@���@��@�ƨ@�-@�hs@��/@�z�@���@�~�@�9@��;@�V@�V@�9X@�33@�-@�^@�/@�Q�@�|�@���@�!@�$�@��@�r�@�+@޸R@�{@۾w@ڗ�@ى7@�1@��@�/@�  @�K�@�ȴ@��@Ѳ-@�7L@мj@��@θR@���@��@̓u@�  @˥�@�o@���@�E�@���@ț�@�dZ@���@�=q@�&�@�I�@�t�@�@¸R@�=q@��@��@�1'@��m@��F@�dZ@��+@�@�O�@���@�Ĝ@�z�@�9X@��;@�"�@���@��@�G�@��@�1'@�n�@��@�j@�?}@���@�33@��P@��w@�\)@��@��@���@���@���@���@��@�x�@�hs@�O�@�?}@�7L@�&�@�V@��@�j@�"�@�^5@��-@��7@�p�@�O�@��@��9@�(�@��
@���@��@���@�5?@�p�@�Ĝ@�z�@�Q�@� �@�  @��@��@��@�@��y@�ȴ@���@�v�@�M�@�$�@�@���@��@��@���@���@�1'@�  @�C�@��T@�x�@��@��`@�Q�@���@��P@�dZ@���@�v�@���@�Ĝ@�r�@�Z@�  @��@�l�@�o@���@�n�@�M�@��@��h@��@�p�@�G�@��`@�I�@�9X@� �@��@��F@�\)@���@�M�@�=q@��^@��h@��#@��#@��#@���@���@��@�b@��F@�  @�(�@��@��;@�|�@�t�@��F@��@�l�@�\)@�;d@��H@��@��H@��!@�~�@�~�@��+@�5?@�$�@�@��@���@��h@�`B@���@��@�j@�Z@�I�@�A�@�9X@��@��m@���@�ƨ@�ƨ@��w@��w@��@��@�l�@�dZ@�"�@���@���@�~�@�=q@��@���@�p�@�7L@��@���@��@���@���@�|�@�S�@�o@���@���@�ff@�=q@�{@��@���@���@��h@��/@�Ĝ@��u@��D@�z�@�j@�Q�@�9X@��@�1@��@��
@���@�;d@�@��H@��\@�~�@�~�@�n�@�-@�{@��@���@�;d@vq�@`y>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB"�B-B2-B6FB8RB:^B>wB@�BA�BD�BE�BD�BF�BG�BF�BB�B=qB7LB0!B'�B#�B�B�B+B��B�B�TB��BĜB�wB�jB�dB�9B��B��B~�Bl�BT�BI�BA�B6FB"�BB
��B
ŢB
��B
�jB
�9B
��B
v�B
G�B
33B
�B
uB
VB
B	��B	�B	�B	�NB	��B	��B	�'B	��B	��B	��B	�PB	t�B	iyB	dZB	`BB	]/B	YB	S�B	R�B	Q�B	O�B	M�B	H�B	D�B	@�B	8RB	2-B	.B	+B	"�B	�B	PB	+B	  B��B�B�ZB�B��B��B��B��B��B��B��B��BƨBB�}B�jB�XB�?B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�\B�VB�VB�\B�\B�\B�bB�hB�hB�bB�bB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�LB�XB�jB�wB�}B��BBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B�B�;B�BB�NB�`B�yB�B�B�B��B��B��B��B��B��B	  B	B	+B		7B	
=B	DB	DB	JB	PB	uB	�B	�B	�B	uB	
=B	B	B	B	PB	�B	"�B	&�B	'�B	/B	0!B	5?B	8RB	9XB	9XB	9XB	9XB	9XB	:^B	:^B	;dB	;dB	;dB	;dB	<jB	>wB	E�B	H�B	J�B	K�B	K�B	K�B	L�B	L�B	N�B	N�B	N�B	N�B	O�B	P�B	T�B	YB	[#B	\)B	]/B	]/B	^5B	^5B	`BB	dZB	dZB	e`B	gmB	hsB	jB	m�B	o�B	q�B	s�B	w�B	y�B	|�B	~�B	� B	�B	�B	�%B	�1B	�=B	�PB	�uB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�!B	�!B	�-B	�-B	�9B	�FB	�LB	�LB	�XB	�dB	�}B	��B	��B	ÖB	ĜB	ŢB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�TB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
�B
"hB
.�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB"�B-B2-B6FB8RB:^B>wB@�BA�BD�BE�BD�BF�BG�BF�BB�B=qB7LB0!B'�B#�B�B�B+B��B�B�TB��BĜB�wB�jB�dB�9B��B��B~�Bl�BT�BI�BA�B6FB"�BB
��B
ŢB
��B
�jB
�9B
��B
v�B
G�B
33B
�B
uB
VB
B	��B	�B	�B	�NB	��B	��B	�'B	��B	��B	��B	�PB	t�B	iyB	dZB	`BB	]/B	YB	S�B	R�B	Q�B	O�B	M�B	H�B	D�B	@�B	8RB	2-B	.B	+B	"�B	�B	PB	+B	  B��B�B�ZB�B��B��B��B��B��B��B��B��BƨBB�}B�jB�XB�?B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�\B�VB�VB�\B�\B�\B�bB�hB�hB�bB�bB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�LB�XB�jB�wB�}B��BBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B�B�;B�BB�NB�`B�yB�B�B�B��B��B��B��B��B��B	  B	B	+B		7B	
=B	DB	DB	JB	PB	uB	�B	�B	�B	uB	
=B	B	B	B	PB	�B	"�B	&�B	'�B	/B	0!B	5?B	8RB	9XB	9XB	9XB	9XB	9XB	:^B	:^B	;dB	;dB	;dB	;dB	<jB	>wB	E�B	H�B	J�B	K�B	K�B	K�B	L�B	L�B	N�B	N�B	N�B	N�B	O�B	P�B	T�B	YB	[#B	\)B	]/B	]/B	^5B	^5B	`BB	dZB	dZB	e`B	gmB	hsB	jB	m�B	o�B	q�B	s�B	w�B	y�B	|�B	~�B	� B	�B	�B	�%B	�1B	�=B	�PB	�uB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�!B	�!B	�-B	�-B	�9B	�FB	�LB	�LB	�XB	�dB	�}B	��B	��B	ÖB	ĜB	ŢB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�TB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
�B
"hB
.�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140833                              AO  ARCAADJP                                                                    20181024140833    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140833  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140833  QCF$                G�O�G�O�G�O�0               