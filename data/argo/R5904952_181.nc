CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:46Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190546  20181005190546  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�橻�\1   @��F)��@1����S��c�z�G�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C7�fC9�fC<  C>  C@  CB  CC�fCF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C��3C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  D   D � D  D� D  Dy�D��D� D  D� D  D� DfD� D  D�fD  D� D	  D	� D
  D
�fD  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D��D� D  Dy�D��Dy�D  D�fD  D� D  D� D  D� D  D� D  Dy�D��Dy�D��D � D!  D!� D"  D"� D#  D#�fD$fD$� D%fD%� D&  D&� D'  D'� D'��D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.y�D.��D/y�D/��D0� D1fD1�fD2  D2� D3  D3�fD4fD4�fD5fD5�fD6fD6�fD7fD7� D7��D8y�D9  D9� D:  D:� D;  D;� D;��D<y�D<��D=� D>  D>� D>��D?y�D@  D@� DA  DA� DB  DB� DB��DCy�DD  DD�fDE  DEy�DF  DF�fDGfDG� DH  DH�fDIfDI�fDJfDJ� DK  DK�fDLfDL�fDM  DMy�DN  DN� DO  DO� DPfDP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DWfDW� DX  DX�fDYfDY� DZ  DZ� D[  D[� D\fD\�fD]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Dby�Dc  Dc� Dd  Dd�fDe  Dey�De��Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Du  Du�fDvfDv�fDwfDw� DwٚDy�D�<)D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @L��@���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�33A�ffB��B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B�fgB���B���B���B���B�fgB���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C033C2L�C4L�C6L�C833C:33C<L�C>L�C@L�CBL�CD33CFL�CHfgCJfgCLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�33C�33C�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�33C�33C�33C�&fC��C�&fC�&fC�&fC�&fC�33C�33C�33C�&fC�&fC�&fC��C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�33C�33C�&fC��C�&fC�&fC��C��C��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�33C�&fC�&fD 3D �3D3D�3D3D��D�D�3D3D�3D3D�3D�D�3D3D��D3D�3D	3D	�3D
3D
��D3D�3D3D�3D3D��D�D�3D3D�3D3D�3D3D�3D3D�3D3D��D�D�3D3D�3D�D�3D3D��D�D��D3D��D3D�3D3D�3D3D�3D3D�3D3D��D�D��D �D �3D!3D!�3D"3D"�3D#3D#��D$�D$�3D%�D%�3D&3D&�3D'3D'�3D(�D(�3D)�D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.�D.��D/�D/��D0�D0�3D1�D1��D23D2�3D33D3��D4�D4��D5�D5��D6�D6��D7�D7�3D8�D8��D93D9�3D:3D:�3D;3D;�3D<�D<��D=�D=�3D>3D>�3D?�D?��D@3D@�3DA3DA�3DB3DB�3DC�DC��DD3DD��DE3DE��DF3DF��DG�DG�3DH3DH��DI�DI��DJ�DJ�3DK3DK��DL�DL��DM3DM��DN3DN�3DO3DO�3DP�DP�3DQ3DQ��DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV�DV�3DW�DW�3DX3DX��DY�DY�3DZ3DZ�3D[3D[�3D\�D\��D]3D]�3D^3D^�3D_3D_�3D`3D`��Da3Da�3Db3Db��Dc3Dc�3Dd3Dd��De3De��Df�Df�3Dg3Dg�3Dh3Dh��Di3Di�3Dj3Dj��Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn��Do3Do�3Dp3Dp��Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt�Dt��Du3Du��Dv�Dv��Dw�Dw�3Dw��Dy�RD�E�D��g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA�ƨA�AɼjA�ȴA��#A��HA��HA��HA��;A��/A��TA��A���A���A���A���A���A�A�{A��A��A�$�A�(�A�(�A�(�A�-A�1'A�=qA�A�A�A�A�M�A�S�A�VA�XA�O�A�E�A�-A��A�oAʺ^A�?}A�5?A�&�A���A�hsAɴ9A�ȴA�ƨA��/A�1Aȟ�A�bNA���A�  A�?}AǁAƬA�$�A�1'A���A�C�A���A�%A��-A���A�`BA�"�A���A���A�"�A�|�A�A�A��jA��yA��A���A�+A���A��A�{A�?}A��A��uA� �A�l�A�;dA���A�VA�1A��A�9XA��7A��A�/A���A���A�A�A���A�^5A�{A��wA��TA�O�A�~�A���A��A��^A�l�A��AyƨAw\)As
=Ap�AnbNAi��Af��Ad-Abv�A_S�A\=qAY��AUO�AS;dARn�AQ�AN��AL�AJ1AH��AGS�AE\)AB-A?`BA9�;A7hsA6^5A5+A1�A0��A/A.  A-A,5?A+��A)�-A(bA&(�A%�A$A�A$JA#%A!�-A!�A E�AhsAA�A�AĜA��AXA��A~�AƨAI�AXA�yAQ�A1A�FAK�AȴA�#A^5AdZA�DA�AG�A�uAJA��AA	hsA	+A	VAƨAO�AE�A��A`BAA�A��A�^AS�A�A �\@�@��@��9@���@���@�A�@�;d@�ȴ@�-@�&�@��;@�M�@��@���@��m@�J@�V@��@�|�@@�n�@홚@�ƨ@�{@�O�@��@旍@���@��@���@�u@��@�n�@�7@���@�Ĝ@��H@���@݉7@��/@�r�@�t�@�ff@�p�@��/@أ�@׾w@�K�@��H@�-@���@�G�@��@�|�@�@��H@�n�@Ѻ^@��@��@϶F@�K�@��@θR@�v�@ͺ^@�1'@˅@�33@�o@ʇ+@��@ɑh@�/@�Ĝ@�(�@�S�@���@Ƈ+@�-@Ų-@�x�@�`B@�?}@Ĵ9@�1'@�K�@\@�V@�5?@��-@�7L@���@�l�@��@��\@��/@�j@���@�l�@�Z@�Q�@�ƨ@���@�?}@�/@�G�@�&�@�V@���@���@�Q�@��@�ff@�V@�J@��#@�x�@��`@��@�dZ@�v�@�J@�p�@���@��@��j@�r�@�(�@�1@���@�C�@��@�{@�`B@��7@��h@��@�/@��@�9X@�b@��@���@�t�@�dZ@�S�@�;d@�+@��@��H@��#@�O�@�&�@���@�j@�1'@�(�@���@���@��@���@��@�dZ@��P@�;d@�ȴ@���@�^5@�$�@�$�@�5?@�$�@�J@��#@���@�x�@��@���@��D@�I�@���@�v�@��\@��\@�^5@��T@�hs@��@�Ĝ@�9X@�;d@���@��@���@��+@�^5@�-@�J@��#@���@�hs@�O�@��@��@�9X@��m@��F@��@�\)@�;d@��@�@���@���@��@��+@�V@�{@��T@���@���@�`B@��@�%@��`@��/@��D@�Z@�1'@���@��@�l�@�+@��@��R@���@�v�@�n�@�n�@�5?@���@��@�7L@��/@��9@�j@�I�@��@��@�ƨ@�+@��@�ȴ@��R@���@���@�v�@��@��T@�x�@��/@�r�@�Z@�A�@� �@��m@��w@��P@�S�@�33@���@���@���@�v�@�E�@�@�7L@��@��/@���@�Ĝ@��j@��9@��u@�9X@��
@�ƨ@��F@�S�@��@�o@�@��!@�n�@�^5@�E�@�@���@�7L@��@�g@kJ#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ƨA�AɼjA�ȴA��#A��HA��HA��HA��;A��/A��TA��A���A���A���A���A���A�A�{A��A��A�$�A�(�A�(�A�(�A�-A�1'A�=qA�A�A�A�A�M�A�S�A�VA�XA�O�A�E�A�-A��A�oAʺ^A�?}A�5?A�&�A���A�hsAɴ9A�ȴA�ƨA��/A�1Aȟ�A�bNA���A�  A�?}AǁAƬA�$�A�1'A���A�C�A���A�%A��-A���A�`BA�"�A���A���A�"�A�|�A�A�A��jA��yA��A���A�+A���A��A�{A�?}A��A��uA� �A�l�A�;dA���A�VA�1A��A�9XA��7A��A�/A���A���A�A�A���A�^5A�{A��wA��TA�O�A�~�A���A��A��^A�l�A��AyƨAw\)As
=Ap�AnbNAi��Af��Ad-Abv�A_S�A\=qAY��AUO�AS;dARn�AQ�AN��AL�AJ1AH��AGS�AE\)AB-A?`BA9�;A7hsA6^5A5+A1�A0��A/A.  A-A,5?A+��A)�-A(bA&(�A%�A$A�A$JA#%A!�-A!�A E�AhsAA�A�AĜA��AXA��A~�AƨAI�AXA�yAQ�A1A�FAK�AȴA�#A^5AdZA�DA�AG�A�uAJA��AA	hsA	+A	VAƨAO�AE�A��A`BAA�A��A�^AS�A�A �\@�@��@��9@���@���@�A�@�;d@�ȴ@�-@�&�@��;@�M�@��@���@��m@�J@�V@��@�|�@@�n�@홚@�ƨ@�{@�O�@��@旍@���@��@���@�u@��@�n�@�7@���@�Ĝ@��H@���@݉7@��/@�r�@�t�@�ff@�p�@��/@أ�@׾w@�K�@��H@�-@���@�G�@��@�|�@�@��H@�n�@Ѻ^@��@��@϶F@�K�@��@θR@�v�@ͺ^@�1'@˅@�33@�o@ʇ+@��@ɑh@�/@�Ĝ@�(�@�S�@���@Ƈ+@�-@Ų-@�x�@�`B@�?}@Ĵ9@�1'@�K�@\@�V@�5?@��-@�7L@���@�l�@��@��\@��/@�j@���@�l�@�Z@�Q�@�ƨ@���@�?}@�/@�G�@�&�@�V@���@���@�Q�@��@�ff@�V@�J@��#@�x�@��`@��@�dZ@�v�@�J@�p�@���@��@��j@�r�@�(�@�1@���@�C�@��@�{@�`B@��7@��h@��@�/@��@�9X@�b@��@���@�t�@�dZ@�S�@�;d@�+@��@��H@��#@�O�@�&�@���@�j@�1'@�(�@���@���@��@���@��@�dZ@��P@�;d@�ȴ@���@�^5@�$�@�$�@�5?@�$�@�J@��#@���@�x�@��@���@��D@�I�@���@�v�@��\@��\@�^5@��T@�hs@��@�Ĝ@�9X@�;d@���@��@���@��+@�^5@�-@�J@��#@���@�hs@�O�@��@��@�9X@��m@��F@��@�\)@�;d@��@�@���@���@��@��+@�V@�{@��T@���@���@�`B@��@�%@��`@��/@��D@�Z@�1'@���@��@�l�@�+@��@��R@���@�v�@�n�@�n�@�5?@���@��@�7L@��/@��9@�j@�I�@��@��@�ƨ@�+@��@�ȴ@��R@���@���@�v�@��@��T@�x�@��/@�r�@�Z@�A�@� �@��m@��w@��P@�S�@�33@���@���@���@�v�@�E�@�@�7L@��@��/@���@�Ĝ@��j@��9@��u@�9X@��
@�ƨ@��F@�S�@��@�o@�@��!@�n�@�^5@�E�@�@���@�7L@��@�g@kJ#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	5?B	5?B	49B	49B	49B	8RB	:^B	:^B	:^B	9XB	8RB	:^B	=qB	>wB	?}B	>wB	@�B	@�B	A�B	H�B	K�B	M�B	O�B	YB	ZB	\)B	`BB	dZB	l�B	p�B	u�B	}�B	�B	�B	�DB	�\B	��B	�B
%B
C�B
�RB
�mB
��B+BVBuBB
�B
�B
��B\B	7BBB�BL�BgmB�+B�3B��B��B�)B�TB�`B�BBbB7LBD�BP�BS�BM�BJ�BB�B<jB33B0!B<jBK�BH�BC�B:^B%�B�BVBB�B�)B�FB� BW
B>wB%�B\BB
�HB
��B
�?B
��B
��B
� B
hsB
K�B
'�B
�B
bB	��B	�BB	��B	�XB	��B	�uB	�B	v�B	q�B	W
B	A�B	+B	�B	{B	%B��B�HB��B��BǮBŢBȴBĜBĜB��B�^B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�FB�LB�RB�jB�qB�}B��B��B��B�wB�jB�dB�LB�FB�jB�qB�wBƨB��B��B��BB�}B��BƨB��B�B�B�
B��B��B��B��B��B��B��B��B��B��B�B�/B�/B�5B�/B�/B�/B�;B�TB�ZB�`B�mB�sB�B�B�B�B�B�B�B�B�B�B��B��B	B	B	B	DB	PB	PB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	'�B	'�B	(�B	+B	-B	1'B	33B	6FB	9XB	>wB	A�B	C�B	H�B	K�B	M�B	N�B	O�B	S�B	VB	ZB	\)B	\)B	^5B	bNB	iyB	k�B	l�B	n�B	o�B	p�B	u�B	x�B	y�B	�B	�B	�B	{�B	{�B	|�B	�B	�+B	�7B	�JB	�JB	�PB	�PB	�=B	�PB	�bB	��B	��B	��B	��B	��B	��B	�{B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�?B	�FB	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	��B	��B	��B	ÖB	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B

=B

=B

=B

=B

=B
JB
�B
$�B
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	5?B	5?B	49B	49B	49B	8RB	:^B	:^B	:^B	9XB	8RB	:^B	=qB	>wB	?}B	>wB	@�B	@�B	A�B	H�B	K�B	M�B	O�B	YB	ZB	\)B	`BB	dZB	l�B	p�B	u�B	}�B	�B	�B	�DB	�\B	��B	�B
%B
C�B
�RB
�mB
��B+BVBuBB
�B
�B
��B\B	7BBB�BL�BgmB�+B�3B��B��B�)B�TB�`B�BBbB7LBD�BP�BS�BM�BJ�BB�B<jB33B0!B<jBK�BH�BC�B:^B%�B�BVBB�B�)B�FB� BW
B>wB%�B\BB
�HB
��B
�?B
��B
��B
� B
hsB
K�B
'�B
�B
bB	��B	�BB	��B	�XB	��B	�uB	�B	v�B	q�B	W
B	A�B	+B	�B	{B	%B��B�HB��B��BǮBŢBȴBĜBĜB��B�^B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�FB�LB�RB�jB�qB�}B��B��B��B�wB�jB�dB�LB�FB�jB�qB�wBƨB��B��B��BB�}B��BƨB��B�B�B�
B��B��B��B��B��B��B��B��B��B��B�B�/B�/B�5B�/B�/B�/B�;B�TB�ZB�`B�mB�sB�B�B�B�B�B�B�B�B�B�B��B��B	B	B	B	DB	PB	PB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	'�B	'�B	(�B	+B	-B	1'B	33B	6FB	9XB	>wB	A�B	C�B	H�B	K�B	M�B	N�B	O�B	S�B	VB	ZB	\)B	\)B	^5B	bNB	iyB	k�B	l�B	n�B	o�B	p�B	u�B	x�B	y�B	�B	�B	�B	{�B	{�B	|�B	�B	�+B	�7B	�JB	�JB	�PB	�PB	�=B	�PB	�bB	��B	��B	��B	��B	��B	��B	�{B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�?B	�FB	�FB	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	��B	��B	��B	ÖB	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B

=B

=B

=B

=B

=B
JB
�B
$�B
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190546                              AO  ARCAADJP                                                                    20181005190546    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190546  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190546  QCF$                G�O�G�O�G�O�8000            