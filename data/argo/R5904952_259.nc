CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:04Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190604  20181005190604  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)���]1   @��*l�f@1�I�^5?�c�$�/�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�33@�  A   A   AA��A`  A~ffA�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��C��C�  C�  C��C�  C��3C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C��C��C��C��C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D  D� D  D� D  D� D  D� D��D� D��Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� DfD�fD  Dy�D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fD  Dy�D��D� D  D�fD  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D#��D$� D%fD%� D&  D&�fD'  D'� D(  D(y�D(��D)y�D*  D*�fD+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@fD@� DA  DA�fDB  DB� DC  DC�fDDfDD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DPy�DP��DQ� DRfDR� DS  DSy�DT  DTy�DT��DU� DV  DV�fDWfDW�fDXfDX� DX��DYy�DY��DZ� DZ��D[y�D[��D\y�D]  D]� D^  D^�fD_fD_�fD`  D`y�Da  Da�fDbfDb� Dc  Dc� DdfDd�fDe  Dey�Df  Df�fDg  Dg� Dh  Dhy�Di  Di� Dj  Dj�fDkfDk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv�fDw  Dw� Dw�fDy��D�2=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ə�A��A$��AFfgAd��A���A�ffA�ffA�ffA�ffAљ�A�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bp��By33B���B���B���B���B���B���B���B���B���B���B���B�fgB�34B���B���B���B���Bę�B���B̙�B�fgBԙ�Bؙ�Bܙ�B�fgB䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0fgC2fgC4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`33CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C��C�&fC�&fC�33C�33C�&fC�&fC�33C�&fC��C�&fC�&fC�&fC�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC��C�&fC�&fC�&fC��C�&fC�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��D 3D �3D3D�3D3D�3D3D�3D3D�3D�D�3D�D��D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D�D�3D�D��D3D��D3D�3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D3D��D3D��D�D�3D3D��D3D��D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#��D$�D$�3D%�D%�3D&3D&��D'3D'�3D(3D(��D)�D)��D*3D*��D+3D+�3D,3D,�3D-�D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=��D>3D>�3D?3D?�3D@�D@�3DA3DA��DB3DB�3DC3DC��DD�DD�3DE3DE�3DF3DF��DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM��DN3DN�3DO3DO�3DP3DP��DQ�DQ�3DR�DR�3DS3DS��DT3DT��DU�DU�3DV3DV��DW�DW��DX�DX�3DY�DY��DZ�DZ�3D[�D[��D\�D\��D]3D]�3D^3D^��D_�D_��D`3D`��Da3Da��Db�Db�3Dc3Dc�3Dd�Dd��De3De��Df3Df��Dg3Dg�3Dh3Dh��Di3Di�3Dj3Dj��Dk�Dk�3Dl3Dl��Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt�Dt�3Du3Du�3Dv3Dv��Dw3Dw�3Dw��Dy�D�;�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A��A��yA��mA��A���A���A�1A�1A�oA�1A�%A�
=A�JA�bA�oA�oA�bA�oA��A��A�{A��A�A��A��A��TA��A��`Aʹ9A� �A��ȂhA�1'A˸RAʛ�A�~�A�?}Aɛ�AȓuAǾwA�M�A�^5Aŗ�AĶFAÙ�A�\)A�7LA�\)A��A�A�A���A��-A���A��9A�G�A�33A��7A��A��RA���A�-A�bNA��A�p�A�A���A�9XA��RA�VA�bA��A��7A���A�bNA���A�7LA��9A�ffA��wA��FA���A�x�A���A�Q�A��A�;dA��A�t�A�^A{`BAt��Ar�yAo�
Al��Ai��Ag;dAeS�Ad�Ac�;Ac+A_��A]+A\�uAZ�9AX��AV5?AUVAT�ARI�API�AOO�ANn�AJ�uAI�AIXAHz�AE��ABM�A>VA=��A<�A;�mA;7LA9�A8�uA61'A3�A2~�A1�A0v�A/7LA.�HA.bA,�9A+�A)�wA(��A'x�A&�`A&�RA&�uA&n�A&5?A&{A%��A$ȴA#�A!��A!�A �HA�
A��AE�A;dA��AK�A��A��AhsAdZA��AG�A^5AQ�AE�AAz�A=qA;dAl�Ap�AXA�A��A��A�mAbAƨAA�AJA
�\A
bA	��A	%A�jA�^A�`An�A�TA?}A/A�A=qA33AƨAx�A�7A��A j@�ff@�E�@���@�Ĝ@��9@�
=@�ff@��^@��@�|�@�M�@�E�@�5?@�+@�=q@�hs@� �@�P@�R@�@��-@�r�@�"�@�$�@�?}@�Ĝ@�Z@�j@�bN@��@ꟾ@���@��T@��-@�@���@���@��@�~�@���@��@���@�9@�D@��@�K�@�-@�@���@�  @�ƨ@�v�@���@�hs@�&�@� �@���@�dZ@��@��@ݺ^@�p�@���@�ƨ@���@�E�@��@�@٩�@���@�I�@��@�ƨ@�dZ@�^5@ղ-@Չ7@�hs@�z�@�j@�r�@ԃ@�bN@��@ӶF@ӍP@�dZ@�C�@�+@��@���@��y@�ȴ@�M�@�@ёh@��@мj@�1'@Ͼw@�;d@�ff@��@͡�@�&�@̛�@�t�@ʇ+@�@�@ɡ�@�p�@���@�b@���@�C�@��@�ff@�E�@��/@î@�K�@î@�"�@��@¸R@�@�ȴ@�hs@���@��@�dZ@�K�@�o@��!@�o@���@�^5@��^@�&�@��@�Ĝ@���@�bN@�(�@���@��@��+@��^@�X@��/@�z�@���@���@�9X@�  @�1'@�z�@��P@���@�J@�(�@���@�dZ@���@��@�C�@�|�@��@��H@�?}@���@�p�@�-@�=q@��-@�j@���@��m@�l�@���@�V@���@���@�@���@�Ĝ@��@��@� �@��m@��P@�t�@�C�@�
=@��H@�n�@��T@�X@�X@�?}@��@��j@� �@���@�;d@��\@�M�@��@��^@�`B@�V@�Ĝ@�r�@�9X@�ƨ@�S�@�@�ȴ@�ȴ@�n�@�V@��T@���@�`B@�/@��@���@�j@�A�@�1@���@�S�@�C�@�;d@�o@��+@�-@���@�`B@�%@��/@�Ĝ@�j@���@�|�@�C�@�@��@��@��!@�V@�$�@��h@���@��u@��@��@��@�K�@�n�@���@�?}@��@���@���@�Q�@�1@���@�|�@�;d@�"�@��@��R@��\@�V@�J@���@�X@�O�@�7L@��@��/@���@��@�Z@�9X@�1@���@��F@�"�@��@���@�h
@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A��A��yA��mA��A���A���A�1A�1A�oA�1A�%A�
=A�JA�bA�oA�oA�bA�oA��A��A�{A��A�A��A��A��TA��A��`Aʹ9A� �A��ȂhA�1'A˸RAʛ�A�~�A�?}Aɛ�AȓuAǾwA�M�A�^5Aŗ�AĶFAÙ�A�\)A�7LA�\)A��A�A�A���A��-A���A��9A�G�A�33A��7A��A��RA���A�-A�bNA��A�p�A�A���A�9XA��RA�VA�bA��A��7A���A�bNA���A�7LA��9A�ffA��wA��FA���A�x�A���A�Q�A��A�;dA��A�t�A�^A{`BAt��Ar�yAo�
Al��Ai��Ag;dAeS�Ad�Ac�;Ac+A_��A]+A\�uAZ�9AX��AV5?AUVAT�ARI�API�AOO�ANn�AJ�uAI�AIXAHz�AE��ABM�A>VA=��A<�A;�mA;7LA9�A8�uA61'A3�A2~�A1�A0v�A/7LA.�HA.bA,�9A+�A)�wA(��A'x�A&�`A&�RA&�uA&n�A&5?A&{A%��A$ȴA#�A!��A!�A �HA�
A��AE�A;dA��AK�A��A��AhsAdZA��AG�A^5AQ�AE�AAz�A=qA;dAl�Ap�AXA�A��A��A�mAbAƨAA�AJA
�\A
bA	��A	%A�jA�^A�`An�A�TA?}A/A�A=qA33AƨAx�A�7A��A j@�ff@�E�@���@�Ĝ@��9@�
=@�ff@��^@��@�|�@�M�@�E�@�5?@�+@�=q@�hs@� �@�P@�R@�@��-@�r�@�"�@�$�@�?}@�Ĝ@�Z@�j@�bN@��@ꟾ@���@��T@��-@�@���@���@��@�~�@���@��@���@�9@�D@��@�K�@�-@�@���@�  @�ƨ@�v�@���@�hs@�&�@� �@���@�dZ@��@��@ݺ^@�p�@���@�ƨ@���@�E�@��@�@٩�@���@�I�@��@�ƨ@�dZ@�^5@ղ-@Չ7@�hs@�z�@�j@�r�@ԃ@�bN@��@ӶF@ӍP@�dZ@�C�@�+@��@���@��y@�ȴ@�M�@�@ёh@��@мj@�1'@Ͼw@�;d@�ff@��@͡�@�&�@̛�@�t�@ʇ+@�@�@ɡ�@�p�@���@�b@���@�C�@��@�ff@�E�@��/@î@�K�@î@�"�@��@¸R@�@�ȴ@�hs@���@��@�dZ@�K�@�o@��!@�o@���@�^5@��^@�&�@��@�Ĝ@���@�bN@�(�@���@��@��+@��^@�X@��/@�z�@���@���@�9X@�  @�1'@�z�@��P@���@�J@�(�@���@�dZ@���@��@�C�@�|�@��@��H@�?}@���@�p�@�-@�=q@��-@�j@���@��m@�l�@���@�V@���@���@�@���@�Ĝ@��@��@� �@��m@��P@�t�@�C�@�
=@��H@�n�@��T@�X@�X@�?}@��@��j@� �@���@�;d@��\@�M�@��@��^@�`B@�V@�Ĝ@�r�@�9X@�ƨ@�S�@�@�ȴ@�ȴ@�n�@�V@��T@���@�`B@�/@��@���@�j@�A�@�1@���@�S�@�C�@�;d@�o@��+@�-@���@�`B@�%@��/@�Ĝ@�j@���@�|�@�C�@�@��@��@��!@�V@�$�@��h@���@��u@��@��@��@�K�@�n�@���@�?}@��@���@���@�Q�@�1@���@�|�@�;d@�"�@��@��R@��\@�V@�J@���@�X@�O�@�7L@��@��/@���@��@�Z@�9X@�1@���@��F@�"�@��@���@�h
@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B0!B0!B0!B/B/B/B/B/B/B0!B0!B1'B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B-B(�B&�B'�B(�B&�B�B�BuBhBuB�B!�B!�B+BO�B�dB	uB	G�B	��B	ǮB	�B
@�B
k�B
x�B
�RB
�fB
�B8RB�VB��B�B�3B�?B�LBB��B��B��B��B��B�}B�XB�XB�RB�9B�B��B�PBl�B<jBuB
��B
�5B
�B
��B
ÖB
�B
{�B
=qB
;dB
B�B
 �B	��B	�)B	ĜB	�^B	��B	�VB	�B	o�B	]/B	K�B	>wB	49B	0!B	+B	%�B	�B	JB	+B��B��B�B�B�B��B��B��B��B	B��B��B	%B	VB	JB��B��B�B�B�fB�/B��BŢB�^B�LB�9B�LB�dB�^B�dB�^B�LB�XB�^B�jB�jB�jB�dB�dB�^B�^B�LB�3B�3B�9B�-B�'B�B�B�B�B�'B�9B�qB�wB��B��B�B�TB�sB�B��B��B	�B	�B	!�B	+B	0!B	49B	%�B	�B	B��B	  B��B�B�B�yB�sB�yB�B�B�B�B�B�B�B��B	bB	{B	(�B	5?B	33B	'�B	�B	  B��B��B��B��B	  B	  B	B	B��B��B��B��B	+B	B	B	B	%B	1B	1B		7B	JB	uB	�B	�B	"�B	(�B	33B	8RB	?}B	F�B	[#B	t�B	�+B	�7B	�7B	�7B	�7B	�=B	�1B	�1B	�=B	�DB	�DB	�DB	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�'B	�'B	�!B	�!B	�?B	�RB	�^B	�wB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�/B	�/B	�)B	�)B	�/B	�5B	�5B	�5B	�BB	�;B	�;B	�)B	�B	�B	�;B	�BB	�;B	�;B	�;B	�ZB	�ZB	�NB	�TB	�NB	�NB	�NB	�NB	�fB	�sB	�mB	�ZB	�TB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�HB	�5B	�/B	�)B	�/B	�HB	�TB	�NB	�NB	�TB	�`B	�`B	�TB	�TB	�#B	�B	�B	�/B	�5B	�BB	�HB	�HB	�5B	�B	�/B	�NB	�mB	�sB	�mB	�ZB	�`B	�B	�yB	�mB	�yB	�B	�B	�B	�mB	�mB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
%B
%B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
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

=B

=B
DB
DB
DB
JB
JB
PB
PB
VB
jB
]22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B0!B0!B0!B/B/B/B/B/B/B0!B0!B1'B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B0!B-B(�B&�B'�B(�B&�B�B�BuBhBuB�B!�B!�B+BO�B�dB	uB	G�B	��B	ǮB	�B
@�B
k�B
x�B
�RB
�fB
�B8RB�VB��B�B�3B�?B�LBB��B��B��B��B��B�}B�XB�XB�RB�9B�B��B�PBl�B<jBuB
��B
�5B
�B
��B
ÖB
�B
{�B
=qB
;dB
B�B
 �B	��B	�)B	ĜB	�^B	��B	�VB	�B	o�B	]/B	K�B	>wB	49B	0!B	+B	%�B	�B	JB	+B��B��B�B�B�B��B��B��B��B	B��B��B	%B	VB	JB��B��B�B�B�fB�/B��BŢB�^B�LB�9B�LB�dB�^B�dB�^B�LB�XB�^B�jB�jB�jB�dB�dB�^B�^B�LB�3B�3B�9B�-B�'B�B�B�B�B�'B�9B�qB�wB��B��B�B�TB�sB�B��B��B	�B	�B	!�B	+B	0!B	49B	%�B	�B	B��B	  B��B�B�B�yB�sB�yB�B�B�B�B�B�B�B��B	bB	{B	(�B	5?B	33B	'�B	�B	  B��B��B��B��B	  B	  B	B	B��B��B��B��B	+B	B	B	B	%B	1B	1B		7B	JB	uB	�B	�B	"�B	(�B	33B	8RB	?}B	F�B	[#B	t�B	�+B	�7B	�7B	�7B	�7B	�=B	�1B	�1B	�=B	�DB	�DB	�DB	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�'B	�'B	�!B	�!B	�?B	�RB	�^B	�wB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�/B	�/B	�)B	�)B	�/B	�5B	�5B	�5B	�BB	�;B	�;B	�)B	�B	�B	�;B	�BB	�;B	�;B	�;B	�ZB	�ZB	�NB	�TB	�NB	�NB	�NB	�NB	�fB	�sB	�mB	�ZB	�TB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�HB	�5B	�/B	�)B	�/B	�HB	�TB	�NB	�NB	�TB	�`B	�`B	�TB	�TB	�#B	�B	�B	�/B	�5B	�BB	�HB	�HB	�5B	�B	�/B	�NB	�mB	�sB	�mB	�ZB	�`B	�B	�yB	�mB	�yB	�B	�B	�B	�mB	�mB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
%B
%B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
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

=B

=B
DB
DB
DB
JB
JB
PB
PB
VB
jB
]22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190604                              AO  ARCAADJP                                                                    20181005190604    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190604  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190604  QCF$                G�O�G�O�G�O�8000            