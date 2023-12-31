CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:57Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191757  20181005191757  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              )A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�������1   @����n�@5@ě��T�d���`A�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     )A   A   A   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC+�fC-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CY�fC[�fC^  C`  Cb  Cd  Cf  Cg�fCi�fCk�fCm�fCp  Cr  Cs�fCv  Cx  Cz�C|  C~  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C��C��C��3C��3C�  C��3C�  C��C��C��C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��C�  C��3C��3C��3C��3C��3C��3C�  C��C��C�  C��3C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C��C��C�  C�  C��C�  C�  C�  C��C��C��C�  C��3C��3C�  C��C��C�  C��3D � D ��Dy�D  D�fD  Dy�D��D� D  D� D  D� D  D� DfD�fD��D	y�D	��D
� D  D� D  Dy�D  D� D  D�fD  D� D��D�fD  D�fDfD� D��D� D��Dy�D  Ds3DfD�fD��D�fD�D�fD��Dy�D  D��DfD� D  Dy�D  D� D  Dy�D  D� D fD � D ��D!�fD"fD"�fD#  D#� D$  D$� D%  D%� D&  D&�fD&��D'� D(fD(� D)  D)� D)��D*� D+fD+y�D,fD,y�D,�3D-y�D-��D.�fD/  D/y�D0fD0y�D1fD1�fD2fD2�fD3  D3�fD4  D4� D5fD5y�D5��D6� D7  D7� D8  D8y�D8��D9y�D9��D:� D;fD;y�D;��D<� D<��D=� D>  D>� D?fD?� D?��D@y�DA  DA� DB  DB�fDCfDCy�DC��DD�fDD��DEy�DF  DF�fDGfDGy�DG��DHy�DI  DIy�DJ  DJ�fDJ��DK� DLfDLy�DMfDM� DM��DN� DN��DO� DP  DPy�DP��DQy�DQ��DRy�DS  DS� DTfDT�fDU  DUy�DV  DV� DV�3DWy�DW�3DXy�DYfDY� DZ  DZs3DZ��D[s3D\  D\�fD]  D]� D^  D^� D^��D_y�D_��D`� Da  Day�Db  Db�fDcfDc� Dc��Dd� De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� DjfDj� Dj��Dky�Dl  Dl� Dm  Dmy�Dn  Dn� DofDo�fDp  Dp�fDqfDq�fDq��Drs3Ds  Ds�fDs��Dt� Du  Duy�DvfDv��Dw  Dwy�Dw� Dy��D�5D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B��B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B�fgB���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B���B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*33C,33C.33C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CN33CP33CRL�CTL�CVL�CXL�CZ33C\33C^L�C`L�CbL�CdL�CfL�Ch33Cj33Cl33Cn33CpL�CrL�Ct33CvL�CxL�CzfgC|L�C~L�C�&fC��C��C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�33C�33C�33C�&fC��C�&fC��C��C�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�33C�33C��C��C�&fC��C�&fC�@ C�33C�33C��C��C��C��C��C��C��C�&fC�&fC��C��C�33C�&fC��C��C��C��C��C��C�&fC�33C�33C�&fC��C�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C��C�&fC�&fC�33C�33C�&fC�&fC�33C�&fC�&fC�&fC�33C�33C�33C�&fC��C��C�&fC�33C�33C�&fD �D �3D�D��D3D��D3D��D�D�3D3D�3D3D�3D3D�3D�D��D	�D	��D
�D
�3D3D�3D3D��D3D�3D3D��D3D�3D�D��D3D��D�D�3D�D�3D�D��D3D�fD�D��D�D��D  D��D�D��D3D� D�D�3D3D��D3D�3D3D��D3D�3D �D �3D!�D!��D"�D"��D#3D#�3D$3D$�3D%3D%�3D&3D&��D'�D'�3D(�D(�3D)3D)�3D*�D*�3D+�D+��D,�D,��D-fD-��D.�D.��D/3D/��D0�D0��D1�D1��D2�D2��D33D3��D43D4�3D5�D5��D6�D6�3D73D7�3D83D8��D9�D9��D:�D:�3D;�D;��D<�D<�3D=�D=�3D>3D>�3D?�D?�3D@�D@��DA3DA�3DB3DB��DC�DC��DD�DD��DE�DE��DF3DF��DG�DG��DH�DH��DI3DI��DJ3DJ��DK�DK�3DL�DL��DM�DM�3DN�DN�3DO�DO�3DP3DP��DQ�DQ��DR�DR��DS3DS�3DT�DT��DU3DU��DV3DV�3DWfDW��DXfDX��DY�DY�3DZ3DZ�fD[�D[�fD\3D\��D]3D]�3D^3D^�3D_�D_��D`�D`�3Da3Da��Db3Db��Dc�Dc�3Dd�Dd�3De3De�3Df3Df�3Dg3Dg��Dh3Dh�3Di3Di�3Dj�Dj�3Dk�Dk��Dl3Dl�3Dm3Dm��Dn3Dn�3Do�Do��Dp3Dp��Dq�Dq��Dr�Dr�fDs3Ds��Dt�Dt�3Du3Du��Dv�Dv� Dw3Dw��Dw�3Dy��D�>�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aĉ7AċDAċDAđhAė�Aė�Aė�Aė�Aė�Aę�Aĝ�Ağ�Aę�A�bNA���A���A�A�A�r�A�\)A�ĜA�M�A��A��A�VA�&�A��`A��DA�=qA��HA�?}A��HA�ƨA��9A��hA�|�A�\)A�9XA�$�A� �A�VA�A�A���A��;A���A�ƨA�ĜA��hA�|�A�r�A�ffA�ZA�=qA�$�A�A���A��wA��A��uA�l�A�C�A�1A�z�A�/A��^A�M�A�VA�S�A���A�{A�ffA��A��A��A��7A��HA��\A��A��DA��A�oA�z�A���A��!A���A��;A�jA��A�t�A��uA���A�JA��A���A�ffA���A��A��DA��A�=qA�Q�A��/A���A�dZA���A�+A���A��#A��hA�?}A�n�A���A���A�\)A�bA��RA�A��A}��A}oA|��A|I�A{�
A{|�Azz�Aw�AtbNAr�+ApAnffAl�Aj�uAh�9Af��Af  Ad�jAb��A`M�A^�\A\^5A[�AZ1AX1'AWK�AU��AT$�AS��AS/AQ�AP�AO`BAN�!AM�mAL�`AL�+AL~�ALbNAK�AH��AG;dAFE�AFJAE��AE�PAEVAD��AD�AC%AA&�A=�A;��A:�HA:~�A9oA7K�A5�;A3��A2z�A0�/A/�A-�A-S�A+�PA*JA(v�A(A&��A&A%VA$Q�A#��A#��A#�A"�A!�A A�AdZAO�A�wA(�AbNAƨA��AZA�!A�FA��A~�AI�A{A��A`BA�`Av�A�
A`BA+A�A�A��A�jAv�A��A��A��A{A
�9A
ffA
=qA
{A��A;dAA�A�^A��Ax�A Q�@���@�O�@���@�dZ@��y@��!@�5?@��@���@��T@�%@�x�@�Ĝ@�~�@�x�@�@�A�@�Z@�bN@��@�;d@@�ff@�V@�@��@��@�D@�hs@�|�@�n�@�b@١�@��`@׶F@�;d@��@�@Ցh@�G�@�(�@�=q@�
=@͑h@���@�j@�|�@�
=@�v�@���@�O�@���@ț�@�z�@�Q�@�1'@ǍP@�33@��@��@Ɨ�@�V@ũ�@���@�9X@�;d@��7@���@�(�@��F@�o@�=q@�`B@��u@��P@��y@��@�r�@�+@�$�@��@��/@�z�@��@�\)@�|�@�+@�^5@��@�I�@���@�+@���@�E�@�{@��T@�`B@�&�@��`@���@�dZ@�o@�v�@�X@�%@��/@��@�bN@�9X@� �@��@�b@��m@��w@�ƨ@�|�@�|�@��y@�v�@�5?@��@���@���@�A�@�9X@�A�@�9X@��
@��w@��@�\)@���@��\@�n�@�-@�@�@��7@�hs@�O�@�O�@�7L@��@��@��@�&�@���@�j@��@�1'@�Q�@�9X@�  @��@�|�@�5?@���@�@�@���@�@��@�Ĝ@��j@���@��@�j@�b@��@���@�t�@�t�@��y@�E�@���@��^@���@��-@��@���@�K�@�
=@���@���@��!@���@���@���@�V@�hs@��@��`@�%@��/@��@���@�Q�@�I�@���@���@�@��R@��y@�o@�"�@��H@���@���@�$�@��7@��j@�(�@���@���@��;@��@�K�@�C�@�o@��@��\@�E�@�-@�@���@��@���@��-@��@���@�z�@�j@�Z@� �@��
@��F@��@�dZ@�;d@�@���@���@���@�=q@��@�$�@�-@�5?@�-@�@���@�x�@�?}@�V@��`@��u@�z�@�Q�@�b@�=�@z�<@k�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aĉ7AċDAċDAđhAė�Aė�Aė�Aė�Aė�Aę�Aĝ�Ağ�Aę�A�bNA���A���A�A�A�r�A�\)A�ĜA�M�A��A��A�VA�&�A��`A��DA�=qA��HA�?}A��HA�ƨA��9A��hA�|�A�\)A�9XA�$�A� �A�VA�A�A���A��;A���A�ƨA�ĜA��hA�|�A�r�A�ffA�ZA�=qA�$�A�A���A��wA��A��uA�l�A�C�A�1A�z�A�/A��^A�M�A�VA�S�A���A�{A�ffA��A��A��A��7A��HA��\A��A��DA��A�oA�z�A���A��!A���A��;A�jA��A�t�A��uA���A�JA��A���A�ffA���A��A��DA��A�=qA�Q�A��/A���A�dZA���A�+A���A��#A��hA�?}A�n�A���A���A�\)A�bA��RA�A��A}��A}oA|��A|I�A{�
A{|�Azz�Aw�AtbNAr�+ApAnffAl�Aj�uAh�9Af��Af  Ad�jAb��A`M�A^�\A\^5A[�AZ1AX1'AWK�AU��AT$�AS��AS/AQ�AP�AO`BAN�!AM�mAL�`AL�+AL~�ALbNAK�AH��AG;dAFE�AFJAE��AE�PAEVAD��AD�AC%AA&�A=�A;��A:�HA:~�A9oA7K�A5�;A3��A2z�A0�/A/�A-�A-S�A+�PA*JA(v�A(A&��A&A%VA$Q�A#��A#��A#�A"�A!�A A�AdZAO�A�wA(�AbNAƨA��AZA�!A�FA��A~�AI�A{A��A`BA�`Av�A�
A`BA+A�A�A��A�jAv�A��A��A��A{A
�9A
ffA
=qA
{A��A;dAA�A�^A��Ax�A Q�@���@�O�@���@�dZ@��y@��!@�5?@��@���@��T@�%@�x�@�Ĝ@�~�@�x�@�@�A�@�Z@�bN@��@�;d@@�ff@�V@�@��@��@�D@�hs@�|�@�n�@�b@١�@��`@׶F@�;d@��@�@Ցh@�G�@�(�@�=q@�
=@͑h@���@�j@�|�@�
=@�v�@���@�O�@���@ț�@�z�@�Q�@�1'@ǍP@�33@��@��@Ɨ�@�V@ũ�@���@�9X@�;d@��7@���@�(�@��F@�o@�=q@�`B@��u@��P@��y@��@�r�@�+@�$�@��@��/@�z�@��@�\)@�|�@�+@�^5@��@�I�@���@�+@���@�E�@�{@��T@�`B@�&�@��`@���@�dZ@�o@�v�@�X@�%@��/@��@�bN@�9X@� �@��@�b@��m@��w@�ƨ@�|�@�|�@��y@�v�@�5?@��@���@���@�A�@�9X@�A�@�9X@��
@��w@��@�\)@���@��\@�n�@�-@�@�@��7@�hs@�O�@�O�@�7L@��@��@��@�&�@���@�j@��@�1'@�Q�@�9X@�  @��@�|�@�5?@���@�@�@���@�@��@�Ĝ@��j@���@��@�j@�b@��@���@�t�@�t�@��y@�E�@���@��^@���@��-@��@���@�K�@�
=@���@���@��!@���@���@���@�V@�hs@��@��`@�%@��/@��@���@�Q�@�I�@���@���@�@��R@��y@�o@�"�@��H@���@���@�$�@��7@��j@�(�@���@���@��;@��@�K�@�C�@�o@��@��\@�E�@�-@�@���@��@���@��-@��@���@�z�@�j@�Z@� �@��
@��F@��@�dZ@�;d@�@���@���@���@�=q@��@�$�@�-@�5?@�-@�@���@�x�@�?}@�V@��`@��u@�z�@�Q�@�b@�=�@z�<@k�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B)�B+B.B6FB6FB:^B=qBC�BA�BC�BF�BP�BYB[#B]/B^5B`BB]/B\)BdZBk�Bn�Br�Bu�Bt�Bt�Bs�Bv�By�Bw�Bx�Bz�B|�B{�B�B�DB�hB�\B�VB�PB�VB�\B�\B�VB�7B�PB��B��B��B��B�B�B�9B�}BĜB�?B�RB��B��B��B��B�uB�+Bp�BYBH�B=qB49B.B'�B�BB��B�B�`B�#B�)B�fB�yB�/BŢB��B�PBy�BjBW
BE�B:^B6FB49B0!B)�B�B
��B
�ZB
��B
�qB
��B
��B
�DB
�JB
�=B
�DB
�1B
�%B
�B
y�B
hsB
M�B
>wB
)�B
�B
VB	��B	�B	�5B	��B	ȴB	�^B	��B	��B	��B	��B	�VB	�B	{�B	t�B	iyB	ffB	_;B	K�B	A�B	;dB	5?B	/B	(�B	(�B	(�B	&�B	!�B	VB	B	B	  B��B��B��B	B	DB	B��B�;B�B�
B��B��BɺB��B�FB�B��B��B��B��B��B�bB�PB�=B�7B�B�B�B� B~�B|�Bx�Bv�Bs�Bo�Bk�BhsBffBffBdZBbNB`BB[#BZBW
BVBVBT�BVBVBVBVBW
BW
BW
BW
BW
BXBXB[#B^5B_;B_;B[#BW
BVBT�BR�BN�BK�BI�BI�BH�BF�BC�B@�B>wB>wB@�B@�B@�BB�BE�BL�BO�BR�BZBbNBjBiyBl�Bo�Bp�Bq�Bp�Bp�Bq�Br�Bq�Bp�Bp�Bm�BiyBe`BcTBcTBdZBgmBiyBk�BjBjBk�Bm�Bl�Bk�Bk�Bn�Bp�Bs�Bv�By�B}�B�B�1B�DB�PB�VB�\B�oB�uB��B��B��B��B�B�B�'B�-B�3B�FB�jB�qB�qB�wB�}B��BÖBÖBŢBȴBɺBȴB��B��B��B�B�
B�#B�fB�B�B�B�B	B	1B	VB	oB	{B	�B	�B	�B	�B	�B	#�B	'�B	,B	1'B	6FB	9XB	;dB	>wB	A�B	B�B	C�B	B�B	B�B	D�B	G�B	G�B	F�B	E�B	F�B	I�B	J�B	K�B	Q�B	W
B	ZB	ZB	ZB	[#B	[#B	[#B	ZB	YB	YB	ZB	[#B	[#B	[#B	^5B	bNB	ffB	iyB	jB	k�B	l�B	n�B	p�B	s�B	v�B	{�B	~�B	� B	�B	�B	�B	�+B	�1B	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�FB	�LB	�LB	�XB	�dB	�wB	�}B	B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�;B	�BB	�HB	�NB	�HB	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�mB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
oB
�B
(�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B)�B+B.B6FB6FB:^B=qBC�BA�BC�BF�BP�BYB[#B]/B^5B`BB]/B\)BdZBk�Bn�Br�Bu�Bt�Bt�Bs�Bv�By�Bw�Bx�Bz�B|�B{�B�B�DB�hB�\B�VB�PB�VB�\B�\B�VB�7B�PB��B��B��B��B�B�B�9B�}BĜB�?B�RB��B��B��B��B�uB�+Bp�BYBH�B=qB49B.B'�B�BB��B�B�`B�#B�)B�fB�yB�/BŢB��B�PBy�BjBW
BE�B:^B6FB49B0!B)�B�B
��B
�ZB
��B
�qB
��B
��B
�DB
�JB
�=B
�DB
�1B
�%B
�B
y�B
hsB
M�B
>wB
)�B
�B
VB	��B	�B	�5B	��B	ȴB	�^B	��B	��B	��B	��B	�VB	�B	{�B	t�B	iyB	ffB	_;B	K�B	A�B	;dB	5?B	/B	(�B	(�B	(�B	&�B	!�B	VB	B	B	  B��B��B��B	B	DB	B��B�;B�B�
B��B��BɺB��B�FB�B��B��B��B��B��B�bB�PB�=B�7B�B�B�B� B~�B|�Bx�Bv�Bs�Bo�Bk�BhsBffBffBdZBbNB`BB[#BZBW
BVBVBT�BVBVBVBVBW
BW
BW
BW
BW
BXBXB[#B^5B_;B_;B[#BW
BVBT�BR�BN�BK�BI�BI�BH�BF�BC�B@�B>wB>wB@�B@�B@�BB�BE�BL�BO�BR�BZBbNBjBiyBl�Bo�Bp�Bq�Bp�Bp�Bq�Br�Bq�Bp�Bp�Bm�BiyBe`BcTBcTBdZBgmBiyBk�BjBjBk�Bm�Bl�Bk�Bk�Bn�Bp�Bs�Bv�By�B}�B�B�1B�DB�PB�VB�\B�oB�uB��B��B��B��B�B�B�'B�-B�3B�FB�jB�qB�qB�wB�}B��BÖBÖBŢBȴBɺBȴB��B��B��B�B�
B�#B�fB�B�B�B�B	B	1B	VB	oB	{B	�B	�B	�B	�B	�B	#�B	'�B	,B	1'B	6FB	9XB	;dB	>wB	A�B	B�B	C�B	B�B	B�B	D�B	G�B	G�B	F�B	E�B	F�B	I�B	J�B	K�B	Q�B	W
B	ZB	ZB	ZB	[#B	[#B	[#B	ZB	YB	YB	ZB	[#B	[#B	[#B	^5B	bNB	ffB	iyB	jB	k�B	l�B	n�B	p�B	s�B	v�B	{�B	~�B	� B	�B	�B	�B	�+B	�1B	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�FB	�LB	�LB	�XB	�dB	�wB	�}B	B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�;B	�BB	�HB	�NB	�HB	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�mB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
oB
�B
(�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191757                              AO  ARCAADJP                                                                    20181005191757    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191757  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191757  QCF$                G�O�G�O�G�O�8000            