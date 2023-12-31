CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:08Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190608  20181005190608  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����� h1   @���-��@2%�����c��1&�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@ffBH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� DfD� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D��Dy�D��D� D  D� D  D� D��Dy�D��Dy�D��D� D��Dy�D  Dy�D��Dy�D��Dy�D��D� DfD� D  D� D  D� D��D� D fD �fD!  D!� D"fD"� D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D(��D)� D)��D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6�fD7  D7� D8  D8y�D9  D9� D:  D:� D;  D;�fD<  D<y�D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DQ��DR� DSfDS� DS��DT� DU  DU� DU��DVy�DW  DW�fDXfDX�fDYfDY�fDZ  DZy�DZ��D[y�D\  D\y�D\��D]� D^fD^�fD_fD_�fD`  D`y�Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dg��Dhy�Dh��Diy�Di��Dj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dpy�Dp��Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv�fDw  Dwl�DyY�D�;3D�}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�34@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B��B ��B)33B133B933BA��BI33BQ33BX��Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B�fgB�fgBę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�fgB���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�C33C33CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CH33CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`fgCbfgCdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�Cv33CxL�CzL�C|L�C~L�C�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�33C�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�33C�33C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D�D�3D3D�3D3D�3D�D�3D3D�3D3D��D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D��D3D�3D�D��D�D�3D3D�3D3D�3D�D��D�D��D�D�3D�D��D3D��D�D��D�D��D�D�3D�D�3D3D�3D3D�3D�D�3D �D ��D!3D!�3D"�D"�3D#3D#�3D$3D$��D%�D%�3D&3D&�3D'3D'�3D(3D(�3D)�D)�3D*�D*��D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D6�D6��D73D7�3D83D8��D93D9�3D:3D:�3D;3D;��D<3D<��D=3D=�3D>3D>��D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF��DG3DG��DH3DH�3DI3DI�3DJ3DJ�3DK3DK��DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP�DP�3DQ3DQ�3DR�DR�3DS�DS�3DT�DT�3DU3DU�3DV�DV��DW3DW��DX�DX��DY�DY��DZ3DZ��D[�D[��D\3D\��D]�D]�3D^�D^��D_�D_��D`3D`��Da3Da�3Db3Db�3Dc�Dc�3Dd3Dd�3De3De�3Df3Df��Dg3Dg�3Dh�Dh��Di�Di��Dj�Dj�3Dk�Dk��Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp�Dp��Dq�Dq�3Dr3Dr�3Ds�Ds�3Dt3Dt�3Du3Du�3Dv3Dv��Dw3Dw� Dyl�D�D�D��]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�ƨA�ƨA�ƨA���A���A���A���A���A���A�ȴA�ȴA���A���A���A��;A��;A��/A���A�ĜAȣ�A�XA�-A�{A�A���Aǝ�A�33A�
=A�\)A��AŴ9A�(�Aħ�A�1'A�A�K�A�33A�VA��;A�A�ffA��A�jA��A��mA��FA���A�  A��A��uA�^5A�M�A�$�A��A��9A���A�v�A�Q�A�5?A� �A��RA�x�A�S�A��A�bA��#A�%A�(�A��#A���A�VA�XA��FA��DA��DA��TA���A�\)A��A�E�A�JA��
A���A��A��A��!A�;dA��A���A��/A���A��^A��HA��`A��^A��DA�Q�A�O�A���A��`A��A�\)A�jA�7LA�/A�~�A�ƨA�O�A�-A�33A�n�A���A��A��DA�oA}%Av��Aq7LAn��AhbAfQ�Ac��A\��AY�
AV�ATȴAS%AR�+AQK�AM�PAIt�AHz�AGG�AFA�ADI�AB=qA?��A?33A>ZA<�jA:�HA9�A6��A4{A2�!A0�uA/�A.Q�A,bA*�\A*�A)�A(�A(�DA(�A'`BA& �A#XA"�DA �/A��A�Ax�A$�A"�AJAC�A��A�mAVA�#A&�AoAȴAv�A�A�A��A�!Ar�A�A�TA�-Al�A��A�;A�TA�^A�#A��A�A�A=qAVA
��A	�#A	&�A��An�A�TAx�A��A�AjA�AXAE�Az�AffAbNA �A �A{AA �u@���@��h@�(�@�t�@�V@���@�G�@���@�j@�l�@�V@�=q@�$�@���@��@�V@�{@��@�@�@�@��y@�
=@@�x�@�/@�9@��@�o@��@�\@�~�@�^5@�-@�7L@���@�Q�@� �@��
@睲@��y@���@�n�@�bN@�p�@�;d@ۅ@�bN@܃@�z�@܃@ܛ�@�Q�@�ƨ@ۥ�@�C�@��@� �@��@۶F@�K�@�J@�v�@���@�~�@�?}@���@�A�@�t�@�
=@֧�@�=q@�x�@���@�z�@Ӯ@���@҇+@�5?@��T@�O�@�9X@�S�@�l�@�+@�"�@�33@���@�~�@���@�r�@� �@�1@��@�ff@��T@ɩ�@�?}@ț�@�A�@�(�@ǝ�@ǍP@���@�b@� �@� �@ǅ@�-@š�@��@�r�@�A�@��m@Å@�
=@���@�"�@�|�@�;d@�@���@���@�hs@���@�1'@��m@�  @��P@�+@��H@�ff@��@��@�r�@�1'@���@�"�@�ȴ@��#@��@��`@�z�@�Q�@�I�@�1'@��m@��P@��@�|�@�|�@�dZ@�S�@�K�@�"�@��+@�$�@��-@�Q�@��m@��@���@��
@��w@��F@��F@��@�
=@�=q@��@��#@���@���@��h@�O�@��@��@�j@�A�@�1'@�1'@�(�@�(�@��@��@�"�@��R@�~�@�-@�%@�A�@���@�C�@���@��@�p�@���@�&�@��@��@�l�@�t�@�dZ@�+@���@�~�@�{@���@���@�Z@�I�@�1@��P@�dZ@�;d@�
=@���@���@���@�v�@��@��@�X@��@�7L@�%@��@��@�bN@�A�@��@�l�@��@��H@�-@���@��7@�?}@���@��@��`@��/@���@�Ĝ@���@���@�+@�ȴ@��+@�V@�5?@�J@��@��7@���@�@���@�~�@�V@�5?@�@���@��7@��@�Z@��;@���@��@��@��\@�{@���@���@�&�@���@���@�r�@�Z@��@�c�@y�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ĜA�ƨA�ƨA�ƨA���A���A���A���A���A���A�ȴA�ȴA���A���A���A��;A��;A��/A���A�ĜAȣ�A�XA�-A�{A�A���Aǝ�A�33A�
=A�\)A��AŴ9A�(�Aħ�A�1'A�A�K�A�33A�VA��;A�A�ffA��A�jA��A��mA��FA���A�  A��A��uA�^5A�M�A�$�A��A��9A���A�v�A�Q�A�5?A� �A��RA�x�A�S�A��A�bA��#A�%A�(�A��#A���A�VA�XA��FA��DA��DA��TA���A�\)A��A�E�A�JA��
A���A��A��A��!A�;dA��A���A��/A���A��^A��HA��`A��^A��DA�Q�A�O�A���A��`A��A�\)A�jA�7LA�/A�~�A�ƨA�O�A�-A�33A�n�A���A��A��DA�oA}%Av��Aq7LAn��AhbAfQ�Ac��A\��AY�
AV�ATȴAS%AR�+AQK�AM�PAIt�AHz�AGG�AFA�ADI�AB=qA?��A?33A>ZA<�jA:�HA9�A6��A4{A2�!A0�uA/�A.Q�A,bA*�\A*�A)�A(�A(�DA(�A'`BA& �A#XA"�DA �/A��A�Ax�A$�A"�AJAC�A��A�mAVA�#A&�AoAȴAv�A�A�A��A�!Ar�A�A�TA�-Al�A��A�;A�TA�^A�#A��A�A�A=qAVA
��A	�#A	&�A��An�A�TAx�A��A�AjA�AXAE�Az�AffAbNA �A �A{AA �u@���@��h@�(�@�t�@�V@���@�G�@���@�j@�l�@�V@�=q@�$�@���@��@�V@�{@��@�@�@�@��y@�
=@@�x�@�/@�9@��@�o@��@�\@�~�@�^5@�-@�7L@���@�Q�@� �@��
@睲@��y@���@�n�@�bN@�p�@�;d@ۅ@�bN@܃@�z�@܃@ܛ�@�Q�@�ƨ@ۥ�@�C�@��@� �@��@۶F@�K�@�J@�v�@���@�~�@�?}@���@�A�@�t�@�
=@֧�@�=q@�x�@���@�z�@Ӯ@���@҇+@�5?@��T@�O�@�9X@�S�@�l�@�+@�"�@�33@���@�~�@���@�r�@� �@�1@��@�ff@��T@ɩ�@�?}@ț�@�A�@�(�@ǝ�@ǍP@���@�b@� �@� �@ǅ@�-@š�@��@�r�@�A�@��m@Å@�
=@���@�"�@�|�@�;d@�@���@���@�hs@���@�1'@��m@�  @��P@�+@��H@�ff@��@��@�r�@�1'@���@�"�@�ȴ@��#@��@��`@�z�@�Q�@�I�@�1'@��m@��P@��@�|�@�|�@�dZ@�S�@�K�@�"�@��+@�$�@��-@�Q�@��m@��@���@��
@��w@��F@��F@��@�
=@�=q@��@��#@���@���@��h@�O�@��@��@�j@�A�@�1'@�1'@�(�@�(�@��@��@�"�@��R@�~�@�-@�%@�A�@���@�C�@���@��@�p�@���@�&�@��@��@�l�@�t�@�dZ@�+@���@�~�@�{@���@���@�Z@�I�@�1@��P@�dZ@�;d@�
=@���@���@���@�v�@��@��@�X@��@�7L@�%@��@��@�bN@�A�@��@�l�@��@��H@�-@���@��7@�?}@���@��@��`@��/@���@�Ĝ@���@���@�+@�ȴ@��+@�V@�5?@�J@��@��7@���@�@���@�~�@�V@�5?@�@���@��7@��@�Z@��;@���@��@��@��\@�{@���@���@�&�@���@���@�r�@�Z@��@�c�@y�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�BB�5B�HB�fB��B	�B	G�B	w�B	�LB
+B
P�B
^5B
cTB
w�B
�VB
�uB
�oB
��B
��B
��B
�jB
��B
�HB
�B
�B
�B
��B
��BBPB�B-B6FBA�BC�B@�B;dB>wB<jB<jB=qBC�BH�BG�BI�BQ�BXBZBZBq�B�B�oB�B�^B��B��B1B�B#�B%�B&�B(�B,B-B/B(�B(�B �B�B�BVBDBPBJB\BBBbB�BoBB�B�HB��B��B�B��B�7Bm�BL�B0!B�B
��B
�sB
�5B
��B
��B
�LB
��B
~�B
S�B
!�B	��B	�TB	�?B	v�B	G�B	.B	uB	PB��B�B�TB�5B�)B�#B�B�B��B��B��BȴBƨBŢBÖBŢBŢBŢBȴBɺB��B�
B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��BǮBǮBȴBɺBĜBƨBɺBŢB��B�wB�XB�}BÖB��B��B�)B�BB�ZB�yB�B��B	  B��B��B��B		7B	�B	)�B	1'B	I�B	]/B	[#B	VB	O�B	G�B	@�B	=qB	=qB	<jB	:^B	@�B	K�B	G�B	J�B	G�B	H�B	G�B	D�B	D�B	D�B	H�B	O�B	P�B	N�B	I�B	C�B	=qB	;dB	;dB	;dB	<jB	=qB	?}B	?}B	C�B	K�B	M�B	O�B	R�B	VB	W
B	YB	[#B	`BB	aHB	dZB	e`B	hsB	o�B	p�B	w�B	y�B	{�B	� B	�B	�B	�B	�B	�B	�=B	�DB	�DB	�DB	�=B	�=B	�1B	� B	v�B	n�B	dZB	_;B	e`B	q�B	w�B	{�B	|�B	�B	�1B	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�'B	�'B	�-B	�9B	�?B	�FB	�FB	�RB	�XB	�^B	�jB	�wB	�}B	�}B	��B	�}B	��B	��B	B	ĜB	ȴB	��B	��B	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�;B	�BB	�HB	�ZB	�ZB	�ZB	�`B	�ZB	�TB	�HB	�BB	�BB	�NB	�`B	�fB	�fB	�`B	�`B	�fB	�ZB	�NB	�ZB	�fB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
1B
JB
PB
PB
PB
PB
PB
JB
JB
JB
JB
PB
VB
VB
\B
\B
\B
\B
\B
\B
VB
VB
PB
JB
JB
JB
JB
PB
PB
PB
VB
PB

=B

=B
	7B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
JB
DB
PB
�B
�B
%F22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�/B�BB�5B�HB�fB��B	�B	G�B	w�B	�LB
+B
P�B
^5B
cTB
w�B
�VB
�uB
�oB
��B
��B
��B
�jB
��B
�HB
�B
�B
�B
��B
��BBPB�B-B6FBA�BC�B@�B;dB>wB<jB<jB=qBC�BH�BG�BI�BQ�BXBZBZBq�B�B�oB�B�^B��B��B1B�B#�B%�B&�B(�B,B-B/B(�B(�B �B�B�BVBDBPBJB\BBBbB�BoBB�B�HB��B��B�B��B�7Bm�BL�B0!B�B
��B
�sB
�5B
��B
��B
�LB
��B
~�B
S�B
!�B	��B	�TB	�?B	v�B	G�B	.B	uB	PB��B�B�TB�5B�)B�#B�B�B��B��B��BȴBƨBŢBÖBŢBŢBŢBȴBɺB��B�
B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��BǮBǮBȴBɺBĜBƨBɺBŢB��B�wB�XB�}BÖB��B��B�)B�BB�ZB�yB�B��B	  B��B��B��B		7B	�B	)�B	1'B	I�B	]/B	[#B	VB	O�B	G�B	@�B	=qB	=qB	<jB	:^B	@�B	K�B	G�B	J�B	G�B	H�B	G�B	D�B	D�B	D�B	H�B	O�B	P�B	N�B	I�B	C�B	=qB	;dB	;dB	;dB	<jB	=qB	?}B	?}B	C�B	K�B	M�B	O�B	R�B	VB	W
B	YB	[#B	`BB	aHB	dZB	e`B	hsB	o�B	p�B	w�B	y�B	{�B	� B	�B	�B	�B	�B	�B	�=B	�DB	�DB	�DB	�=B	�=B	�1B	� B	v�B	n�B	dZB	_;B	e`B	q�B	w�B	{�B	|�B	�B	�1B	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�'B	�'B	�-B	�9B	�?B	�FB	�FB	�RB	�XB	�^B	�jB	�wB	�}B	�}B	��B	�}B	��B	��B	B	ĜB	ȴB	��B	��B	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�;B	�BB	�HB	�ZB	�ZB	�ZB	�`B	�ZB	�TB	�HB	�BB	�BB	�NB	�`B	�fB	�fB	�`B	�`B	�fB	�ZB	�NB	�ZB	�fB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
1B
JB
PB
PB
PB
PB
PB
JB
JB
JB
JB
PB
VB
VB
\B
\B
\B
\B
\B
\B
VB
VB
PB
JB
JB
JB
JB
PB
PB
PB
VB
PB

=B

=B
	7B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B

=B
DB
JB
DB
PB
�B
�B
%F22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190608                              AO  ARCAADJP                                                                    20181005190608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190608  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190608  QCF$                G�O�G�O�G�O�8000            