CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:56Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190556  20181005190556  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��/<1   @��jDDY0@0�r� Ĝ�c�333331   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0�C2�C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  D   D y�D  D� D  D� D  D� D  D�fDfD� D��D� D  D� D  D� D	  D	� D
fD
�fD  Dy�D  D� DfD� D��D� D��D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D�fD  D�fD  Dy�D  D� DfD�fDfD� D  D� D  Dy�DfD� D  D�fD   D � D!  D!� D"  D"� D#fD#� D#��D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)fD)� D*  D*�fD+  D+� D,  D,�fD-fD-� D.  D.� D/  D/� D0  D0� D1  D1y�D1��D2� D3  D3� D4  D4�fD5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D>��D?� D@  D@�fDA  DA� DB  DB� DC  DC�fDDfDD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM�fDNfDN� DOfDO�fDPfDP� DP��DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D]��D^� D_  D_� D_��D`� DafDa� Db  Db�fDcfDc�fDd  Dd� De  De�fDf  Dfy�Dg  Dg� Dg��Dh� Di  Diy�Di��Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds�fDs��Dty�Du  Du� Dv  Dv� Dw  Dw� Dw��Dy�fD�0�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @333@���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�33A�33A�ffB33B	33B33B33B ��B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�C33CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*fgC,L�C.L�C0fgC2fgC4L�C6L�C8L�C:33C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\fgC^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|fgC~fgC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC��C��C��C��C�&fC�&fC�&fC�&fC�33C�33C�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�33C�&fC�&fC�&fD 3D ��D3D�3D3D�3D3D�3D3D��D�D�3D�D�3D3D�3D3D�3D	3D	�3D
�D
��D3D��D3D�3D�D�3D�D�3D�D�3D3D�3D3D�3D3D�3D3D�3D�D��D3D�3D3D��D3D��D3D��D3D�3D�D��D�D�3D3D�3D3D��D�D�3D3D��D 3D �3D!3D!�3D"3D"�3D#�D#�3D$�D$�3D%3D%�3D&3D&�3D'3D'��D(3D(�3D)�D)�3D*3D*��D+3D+�3D,3D,��D-�D-�3D.3D.�3D/3D/�3D03D0�3D13D1��D2�D2�3D33D3�3D43D4��D53D5�3D63D6��D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=�D=�3D>3D>�3D?�D?�3D@3D@��DA3DA�3DB3DB�3DC3DC��DD�DD��DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM�DM��DN�DN�3DO�DO��DP�DP�3DQ�DQ��DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\��D]3D]�3D^�D^�3D_3D_�3D`�D`�3Da�Da�3Db3Db��Dc�Dc��Dd3Dd�3De3De��Df3Df��Dg3Dg�3Dh�Dh�3Di3Di��Dj�Dj�3Dk3Dk�3Dl3Dl��Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr��Ds3Ds��Dt�Dt��Du3Du�3Dv3Dv�3Dw3Dw�3Dx�DyəD�:�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�bA͡�A�bNAˮA�p�A��A��Aʇ+A�?}AɼjA�^5AȼjA�JA��#AǼjAǝ�AǍPAǅA�ffA�I�A�+A��A��`A���A�ƨAƧ�AƟ�Aƛ�AƇ+A�XA�Q�A�S�A�ZA�~�AƉ7AƉ7AƃA��`AƃA��A�(�A���A�1A7A�-A��`A��!A�9XA��A��A�C�A��uA�(�A��
A�+A�ƨA�t�A�$�A�bA�dZA�{A�=qA�5?A�VA�K�A�(�A���A�-A��A�33A��A�C�A�"�A�7LA���A��yA�E�A�JA��/A�?}A��PA��A�r�A��TA�A��PA���A�^5A�
=A�1'A�%A�ffA�5?A�+A�ffA�M�A��A�Q�A�K�A���A~��A|��A{?}Az  Aw�TAs33AoAl�AhM�AdffA`��A_��A]?}A[��A[�hAY�AU�AT=qAR�!AR �AP=qAK�AJ�AH5?AE��AE�AD��AC�#AB�HAA��AAoA@�A>��A<��A;?}A9oA7�7A5�A4��A4n�A4A�A3��A1�#A0�9A/XA.r�A-��A-S�A+��A)�wA(�A'�-A&r�A#�A#;dA"��A"��A"v�A!��A!�A ��AƨAjAhsA��A�A �A�TA\)A�#A�PAXAv�AK�AS�A%A�/A
=At�A�A�wA��A/A	+AO�Av�A��AO�AVA�A�HA�uAQ�A�A =q@�Q�@�V@���@�Z@�K�@��+@�M�@�Z@�@�33@���@���@�@�l�@�{@�^@���@�j@�1'@��H@��`@��@��@��m@�l�@��@�=q@�7L@�1'@��H@�/@�K�@ڸR@�-@���@ّh@�/@���@�A�@�ƨ@�v�@�&�@���@ԓu@��@�|�@�C�@�@�{@�;d@�J@�V@˝�@���@�n�@�E�@�=q@�=q@�E�@��@��/@�bN@��@ǅ@�"�@�ff@��@ź^@�?}@��@ċD@��@��m@�r�@�$�@�@Ĵ9@��@ċD@� �@�l�@°!@�E�@�M�@��@��@��@��@��T@�@��7@���@�@���@�1'@�l�@���@�/@��/@��9@�j@�1@�%@���@��9@�dZ@�~�@�5?@��#@��@�j@�9X@�A�@�A�@�bN@��F@�~�@�?}@���@�Q�@�dZ@��@�o@��@��@�ff@�V@���@���@���@�~�@�=q@�=q@�V@�V@���@���@� �@�dZ@�5?@�@���@�bN@�1@� �@��m@���@���@��@��H@�M�@��@��#@��@�{@��-@�1'@���@��@�o@��R@��+@�^5@��+@�~�@�n�@�v�@�^5@�^5@�$�@�@��^@���@�Ĝ@�I�@��
@�t�@�5?@�@���@�M�@�V@�$�@�?}@���@�r�@�9X@���@�(�@��P@���@�
=@�5?@�/@��/@�%@�9X@��
@���@��@�|�@��P@�\)@�;d@�"�@��@�ȴ@��@���@�@�hs@�7L@�/@��@��j@��D@�Q�@� �@��;@���@�l�@�33@��R@�v�@�E�@��@�@�hs@�?}@��@��9@��D@�bN@��F@�K�@�
=@���@���@�=q@��^@��@�hs@�/@��`@���@���@�b@�ƨ@���@��@�t�@�S�@�33@��H@��R@���@�ff@�M�@�E�@�$�@�{@��@�p�@��@�%@���@��9@���@��@�I�@�1@�  @��
@��@���@�t�@�+@��+@�n�@�ff@�E�@�J@���@�O�@�/@���@�z�@�1@��F@�|�@�+@��y@�ȴ@���@��\@�v�@�-@���@���@��@��U@~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�bA͡�A�bNAˮA�p�A��A��Aʇ+A�?}AɼjA�^5AȼjA�JA��#AǼjAǝ�AǍPAǅA�ffA�I�A�+A��A��`A���A�ƨAƧ�AƟ�Aƛ�AƇ+A�XA�Q�A�S�A�ZA�~�AƉ7AƉ7AƃA��`AƃA��A�(�A���A�1A7A�-A��`A��!A�9XA��A��A�C�A��uA�(�A��
A�+A�ƨA�t�A�$�A�bA�dZA�{A�=qA�5?A�VA�K�A�(�A���A�-A��A�33A��A�C�A�"�A�7LA���A��yA�E�A�JA��/A�?}A��PA��A�r�A��TA�A��PA���A�^5A�
=A�1'A�%A�ffA�5?A�+A�ffA�M�A��A�Q�A�K�A���A~��A|��A{?}Az  Aw�TAs33AoAl�AhM�AdffA`��A_��A]?}A[��A[�hAY�AU�AT=qAR�!AR �AP=qAK�AJ�AH5?AE��AE�AD��AC�#AB�HAA��AAoA@�A>��A<��A;?}A9oA7�7A5�A4��A4n�A4A�A3��A1�#A0�9A/XA.r�A-��A-S�A+��A)�wA(�A'�-A&r�A#�A#;dA"��A"��A"v�A!��A!�A ��AƨAjAhsA��A�A �A�TA\)A�#A�PAXAv�AK�AS�A%A�/A
=At�A�A�wA��A/A	+AO�Av�A��AO�AVA�A�HA�uAQ�A�A =q@�Q�@�V@���@�Z@�K�@��+@�M�@�Z@�@�33@���@���@�@�l�@�{@�^@���@�j@�1'@��H@��`@��@��@��m@�l�@��@�=q@�7L@�1'@��H@�/@�K�@ڸR@�-@���@ّh@�/@���@�A�@�ƨ@�v�@�&�@���@ԓu@��@�|�@�C�@�@�{@�;d@�J@�V@˝�@���@�n�@�E�@�=q@�=q@�E�@��@��/@�bN@��@ǅ@�"�@�ff@��@ź^@�?}@��@ċD@��@��m@�r�@�$�@�@Ĵ9@��@ċD@� �@�l�@°!@�E�@�M�@��@��@��@��@��T@�@��7@���@�@���@�1'@�l�@���@�/@��/@��9@�j@�1@�%@���@��9@�dZ@�~�@�5?@��#@��@�j@�9X@�A�@�A�@�bN@��F@�~�@�?}@���@�Q�@�dZ@��@�o@��@��@�ff@�V@���@���@���@�~�@�=q@�=q@�V@�V@���@���@� �@�dZ@�5?@�@���@�bN@�1@� �@��m@���@���@��@��H@�M�@��@��#@��@�{@��-@�1'@���@��@�o@��R@��+@�^5@��+@�~�@�n�@�v�@�^5@�^5@�$�@�@��^@���@�Ĝ@�I�@��
@�t�@�5?@�@���@�M�@�V@�$�@�?}@���@�r�@�9X@���@�(�@��P@���@�
=@�5?@�/@��/@�%@�9X@��
@���@��@�|�@��P@�\)@�;d@�"�@��@�ȴ@��@���@�@�hs@�7L@�/@��@��j@��D@�Q�@� �@��;@���@�l�@�33@��R@�v�@�E�@��@�@�hs@�?}@��@��9@��D@�bN@��F@�K�@�
=@���@���@�=q@��^@��@�hs@�/@��`@���@���@�b@�ƨ@���@��@�t�@�S�@�33@��H@��R@���@�ff@�M�@�E�@�$�@�{@��@�p�@��@�%@���@��9@���@��@�I�@�1@�  @��
@��@���@�t�@�+@��+@�n�@�ff@�E�@�J@���@�O�@�/@���@�z�@�1@��F@�|�@�+@��y@�ȴ@���@��\@�v�@�-@���@���@��@��U@~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�9B�
B	33B	<jB	?}B	E�B	Q�B	XB	S�B	T�B	e`B	N�B	:^B	6FB	F�B	_;B	cTB	e`B	iyB	t�B	�B	��B	��B	�B	�3B	�qB	�}B	��B	ÖB	ÖB	ÖB	ÖB	ɺB
+B
(�B
L�B
�1B
��B"�B1'B9XBYB�7B�3B�jBB�B�B  BJB�B#�B,B(�B"�B"�B�B&�B.B0!B6FBB�BE�BK�BM�BN�BO�BN�BK�BG�BG�BH�BD�B;dB5?B)�B�B�BVB�B�TB��B��B�Bn�BS�B=qB#�BB
�TB
�RB
��B
�DB
y�B
^5B
?}B
49B
,B
�B	��B	�B	��B	�}B	�?B	��B	�B	o�B	\)B	A�B	)�B	�B	bB	B	DB	�B	�B	hB		7B	B��B�B�)B��B��BĜBB��B�qB�dB�dB�XB�FB�'B�!B�B��B��B��B��B��B��B�{B�+B�B�B�B�B�B�B�DB�\B�uB�{B��B��B��B��B�B�RB�jB��BƨBŢB��B�LB�?B�FB�wBɺB��B��BȴBŢBǮB��B��B��B�/B��B��B	  B��B�B�;B�B�#B�
B�B��B��B��B��B��B��BĜB��B��B��B��B�}B��B�}B�wB�jB�XB�LB�LB�RB�XBÖBǮBȴB��B��B��B�
B�)B�)B�)B�;B�HB�TB�fB�B�B�B��B��B��B��B��B��B��B	  B	  B	B	
=B	PB	\B	hB	uB	{B	{B	�B	�B	�B	�B	�B	"�B	$�B	&�B	&�B	'�B	(�B	/B	5?B	6FB	7LB	8RB	:^B	>wB	B�B	C�B	F�B	H�B	K�B	N�B	P�B	VB	dZB	dZB	bNB	aHB	gmB	l�B	k�B	iyB	iyB	jB	l�B	m�B	m�B	p�B	t�B	v�B	w�B	y�B	{�B	}�B	}�B	}�B	y�B	w�B	w�B	y�B	{�B	}�B	�%B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�B	�LB	�qB	�}B	�wB	�}B	��B	��B	��B	�}B	�qB	�qB	�wB	�jB	�dB	�^B	�dB	�jB	�}B	��B	��B	B	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ŢB	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�#B	�5B	�/B	�)B	�;B	�NB	�ZB	�fB	�fB	�`B	�fB	�fB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
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
DB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
uB
uB
uB
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
jB
.I222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�9B�
B	33B	<jB	?}B	E�B	Q�B	XB	S�B	T�B	e`B	N�B	:^B	6FB	F�B	_;B	cTB	e`B	iyB	t�B	�B	��B	��B	�B	�3B	�qB	�}B	��B	ÖB	ÖB	ÖB	ÖB	ɺB
+B
(�B
L�B
�1B
��B"�B1'B9XBYB�7B�3B�jBB�B�B  BJB�B#�B,B(�B"�B"�B�B&�B.B0!B6FBB�BE�BK�BM�BN�BO�BN�BK�BG�BG�BH�BD�B;dB5?B)�B�B�BVB�B�TB��B��B�Bn�BS�B=qB#�BB
�TB
�RB
��B
�DB
y�B
^5B
?}B
49B
,B
�B	��B	�B	��B	�}B	�?B	��B	�B	o�B	\)B	A�B	)�B	�B	bB	B	DB	�B	�B	hB		7B	B��B�B�)B��B��BĜBB��B�qB�dB�dB�XB�FB�'B�!B�B��B��B��B��B��B��B�{B�+B�B�B�B�B�B�B�DB�\B�uB�{B��B��B��B��B�B�RB�jB��BƨBŢB��B�LB�?B�FB�wBɺB��B��BȴBŢBǮB��B��B��B�/B��B��B	  B��B�B�;B�B�#B�
B�B��B��B��B��B��B��BĜB��B��B��B��B�}B��B�}B�wB�jB�XB�LB�LB�RB�XBÖBǮBȴB��B��B��B�
B�)B�)B�)B�;B�HB�TB�fB�B�B�B��B��B��B��B��B��B��B	  B	  B	B	
=B	PB	\B	hB	uB	{B	{B	�B	�B	�B	�B	�B	"�B	$�B	&�B	&�B	'�B	(�B	/B	5?B	6FB	7LB	8RB	:^B	>wB	B�B	C�B	F�B	H�B	K�B	N�B	P�B	VB	dZB	dZB	bNB	aHB	gmB	l�B	k�B	iyB	iyB	jB	l�B	m�B	m�B	p�B	t�B	v�B	w�B	y�B	{�B	}�B	}�B	}�B	y�B	w�B	w�B	y�B	{�B	}�B	�%B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�B	�LB	�qB	�}B	�wB	�}B	��B	��B	��B	�}B	�qB	�qB	�wB	�jB	�dB	�^B	�dB	�jB	�}B	��B	��B	B	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ŢB	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�#B	�5B	�/B	�)B	�;B	�NB	�ZB	�fB	�fB	�`B	�fB	�fB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
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
DB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
uB
uB
uB
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
jB
.I222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190556                              AO  ARCAADJP                                                                    20181005190556    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190556  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190556  QCF$                G�O�G�O�G�O�8000            