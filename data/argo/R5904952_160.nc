CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:41Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190541  20181005190541  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��1   @��j[,@0��$�/�cŉ7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�33B�33B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� DfD�fDfD�fD  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD�fD  Dy�D��D� D  D�fD  Dy�D��D� DfD� D  D� D  Dy�D  D�fDfD� D��D� DfD� D  D�fD  D� D��Dy�D��Dy�D  D�fD   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&y�D'  D'�fD(fD(�fD)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D-��D.y�D/  D/� D0  D0� D1  D1� D2fD2y�D2��D3� D3��D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D<��D=y�D>  D>� D?  D?�fD@  D@y�DA  DA� DB  DB�fDC  DC� DD  DD�fDE  DEy�DF  DF� DF��DGy�DHfDH� DH��DI� DI��DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DY� DZfDZ� D[  D[� D\  D\� D]fD]� D^  D^y�D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� Dd��Dey�Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� DkfDk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do�fDp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy�HD�,{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ə�A��A#33AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�33B33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bp��By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bș�B̙�BЙ�Bԙ�B�fgBܙ�B���B䙚B���B���B���B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�33C�33C�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC��C�&fC�&fC�33C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�33C�33C�&fC�&fC�&fC��C��C��C��C�&fC�&fC�33C�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D��D3D�3D3D�3D�D��D�D��D3D��D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D�D��D3D��D�D�3D3D��D3D��D�D�3D�D�3D3D�3D3D��D3D��D�D�3D�D�3D�D�3D3D��D3D�3D�D��D�D��D3D��D 3D ��D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&�D&��D'3D'��D(�D(��D)3D)�3D*�D*�3D+3D+�3D,3D,�3D-3D-�3D.�D.��D/3D/�3D03D0�3D13D1�3D2�D2��D3�D3�3D4�D4�3D53D5�3D6�D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<�D<��D=�D=��D>3D>�3D?3D?��D@3D@��DA3DA�3DB3DB��DC3DC�3DD3DD��DE3DE��DF3DF�3DG�DG��DH�DH�3DI�DI�3DJ�DJ�3DK3DK�3DL3DL��DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR��DS3DS��DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX��DY�DY�3DZ�DZ�3D[3D[�3D\3D\�3D]�D]�3D^3D^��D_�D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc��Dd�Dd�3De�De��Df3Df�3Dg3Dg�3Dh3Dh�3Di�Di�3Dj3Dj�3Dk�Dk�3Dl�Dl��Dm3Dm�3Dn3Dn�3Do3Do��Dp3Dp��Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt��Du3Du�3Dv3Dv�3Dw3Dw�3DxfDy�{D�6D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aҕ�Aҕ�Aҙ�Aҡ�AҰ!AҴ9Aҧ�Aҩ�AҶFAҺ^AҾwAҾwAҾwAҺ^AҬAқ�A�l�A�\)A�ZA�A�A��A�bA�
=A�1A�1A�1A�
=A�
=A�VA��A�1A��HAѴ9A�G�A���Aк^AЧ�AЕ�A�%A�-A��A�A�/A�%AɼjA�AȃAǶFA�t�A��Aţ�AìA�?}A��A��A�dZA�~�A��A��-A��yA�=qA��A��A�^5A�"�A���A�=qA��HA�ZA��A�;dA�+A�bNA�ȴA�G�A�9XA�z�A�1A��9A���A�ȴA��A���A���A�p�A���A��hA���A�^5A��TA��A��9A�ĜA�  A���A�XA��A��A�jA�ȴA���A��TA���A���A�ZA��A}33Ay/As��AqVAn��Ah��Ac�wAb�AbQ�A`JA[�
AW�7AS�#AR�/AP  AK�FAJ��AJ(�AIAGC�AD��AC?}ABjAA��A?oA=�mA<�`A:�+A8 �A733A5S�A4�A1�#A0jA/t�A-�A+�#A)dZA(��A'ƨA&�A$�A#%A!�
A �+AA�!AĜAG�A�!A�A��An�A��At�A��AbAC�A5?A�A\)A�FAA��A{A
=A  A�A�A
��A	��A~�A��AK�A��A��AAt�A?}A�AȴAI�A�A1A{AbAl�A-A �A {@���@��@���@�E�@�r�@�@��`@���@��j@�1'@�R@�&�@�%@�;d@�ff@�r�@�33@�?}@�ȴ@�\@旍@�h@�@��@�R@��@��/@�\)@ޏ\@�$�@��#@�%@�l�@ڏ\@�M�@ش9@׶F@�K�@֟�@�J@ա�@Ցh@ԣ�@�l�@�n�@�=q@��@љ�@У�@Ͼw@ϝ�@�^5@�G�@̴9@˥�@�+@�o@��@ʏ\@���@�J@Ɂ@�Q�@Ǯ@��@��@��@ƸR@Ɵ�@�M�@�E�@��#@�p�@��/@ģ�@�Z@�  @öF@��@�^5@�V@�V@�G�@���@��j@���@�j@�1'@��F@�"�@�E�@��^@�&�@� �@���@��@��@���@��+@�V@�J@��-@�p�@���@��@��D@�1'@�33@��@�ȴ@�E�@��7@�V@�Ĝ@�r�@�1'@���@�+@���@���@�~�@�$�@�X@��@�(�@���@�;d@�@�V@�@��#@��h@�G�@�V@��/@���@�j@�I�@���@�ƨ@���@�E�@��@���@���@���@�r�@�I�@��@��
@��F@���@�l�@�\)@�K�@�@��R@�v�@�@�@�p�@�O�@�O�@�&�@���@��9@�A�@�t�@���@�V@�-@��@���@��#@�p�@�V@�%@���@��`@���@��j@��@�r�@�bN@�Z@�1'@��@��@���@�t�@�C�@�"�@�o@���@���@�-@���@�x�@��@��@�I�@�(�@�1@��w@�l�@�K�@��@��H@���@���@�{@���@�p�@�/@��@���@�z�@�bN@�A�@�1@��F@�dZ@�o@��y@�ȴ@�v�@�-@��#@�p�@�7L@���@���@�Z@�b@���@�S�@�"�@�ȴ@�-@�$�@���@���@��h@�hs@�`B@�?}@��@��`@��j@���@�j@�9X@��@��@��;@��
@���@��@�K�@���@��!@�V@�J@��T@���@�?}@��u@�b@��@��;@���@�dZ@�S�@�33@�+@�ȴ@���@�V@�$�@���@��^@��h@�`B@���@�Q�@�b@�ƨ@��P@�\)@�K�@�;d@��@��R@��+@�~�@�ff@�n�@�5?@��7@���@w��@f�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aҕ�Aҕ�Aҙ�Aҡ�AҰ!AҴ9Aҧ�Aҩ�AҶFAҺ^AҾwAҾwAҾwAҺ^AҬAқ�A�l�A�\)A�ZA�A�A��A�bA�
=A�1A�1A�1A�
=A�
=A�VA��A�1A��HAѴ9A�G�A���Aк^AЧ�AЕ�A�%A�-A��A�A�/A�%AɼjA�AȃAǶFA�t�A��Aţ�AìA�?}A��A��A�dZA�~�A��A��-A��yA�=qA��A��A�^5A�"�A���A�=qA��HA�ZA��A�;dA�+A�bNA�ȴA�G�A�9XA�z�A�1A��9A���A�ȴA��A���A���A�p�A���A��hA���A�^5A��TA��A��9A�ĜA�  A���A�XA��A��A�jA�ȴA���A��TA���A���A�ZA��A}33Ay/As��AqVAn��Ah��Ac�wAb�AbQ�A`JA[�
AW�7AS�#AR�/AP  AK�FAJ��AJ(�AIAGC�AD��AC?}ABjAA��A?oA=�mA<�`A:�+A8 �A733A5S�A4�A1�#A0jA/t�A-�A+�#A)dZA(��A'ƨA&�A$�A#%A!�
A �+AA�!AĜAG�A�!A�A��An�A��At�A��AbAC�A5?A�A\)A�FAA��A{A
=A  A�A�A
��A	��A~�A��AK�A��A��AAt�A?}A�AȴAI�A�A1A{AbAl�A-A �A {@���@��@���@�E�@�r�@�@��`@���@��j@�1'@�R@�&�@�%@�;d@�ff@�r�@�33@�?}@�ȴ@�\@旍@�h@�@��@�R@��@��/@�\)@ޏ\@�$�@��#@�%@�l�@ڏ\@�M�@ش9@׶F@�K�@֟�@�J@ա�@Ցh@ԣ�@�l�@�n�@�=q@��@љ�@У�@Ͼw@ϝ�@�^5@�G�@̴9@˥�@�+@�o@��@ʏ\@���@�J@Ɂ@�Q�@Ǯ@��@��@��@ƸR@Ɵ�@�M�@�E�@��#@�p�@��/@ģ�@�Z@�  @öF@��@�^5@�V@�V@�G�@���@��j@���@�j@�1'@��F@�"�@�E�@��^@�&�@� �@���@��@��@���@��+@�V@�J@��-@�p�@���@��@��D@�1'@�33@��@�ȴ@�E�@��7@�V@�Ĝ@�r�@�1'@���@�+@���@���@�~�@�$�@�X@��@�(�@���@�;d@�@�V@�@��#@��h@�G�@�V@��/@���@�j@�I�@���@�ƨ@���@�E�@��@���@���@���@�r�@�I�@��@��
@��F@���@�l�@�\)@�K�@�@��R@�v�@�@�@�p�@�O�@�O�@�&�@���@��9@�A�@�t�@���@�V@�-@��@���@��#@�p�@�V@�%@���@��`@���@��j@��@�r�@�bN@�Z@�1'@��@��@���@�t�@�C�@�"�@�o@���@���@�-@���@�x�@��@��@�I�@�(�@�1@��w@�l�@�K�@��@��H@���@���@�{@���@�p�@�/@��@���@�z�@�bN@�A�@�1@��F@�dZ@�o@��y@�ȴ@�v�@�-@��#@�p�@�7L@���@���@�Z@�b@���@�S�@�"�@�ȴ@�-@�$�@���@���@��h@�hs@�`B@�?}@��@��`@��j@���@�j@�9X@��@��@��;@��
@���@��@�K�@���@��!@�V@�J@��T@���@�?}@��u@�b@��@��;@���@�dZ@�S�@�33@�+@�ȴ@���@�V@�$�@���@��^@��h@�`B@���@�Q�@�b@�ƨ@��P@�\)@�K�@�;d@��@��R@��+@�~�@�ff@�n�@�5?@��7@���@w��@f�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B-B-B-B-B/B/B-B.B0!B2-B33B5?B7LB6FB6FB6FB49B33B33B2-B1'B2-B2-B2-B2-B2-B33B33B5?B7LB@�BH�BXBu�B|�B|�B|�B{�Bw�B|�B�B�+B��B�-B�-B�9B�dB��B�B��B1B"�B�BA�BbNBgmBk�Bp�Bo�Bq�Bn�Bl�Bn�Bn�Bl�Bt�B�B� B}�Bw�Bp�BgmB[#B>wB!�BB�B�yB�NB�;B��B�qB��B�%Bu�Bn�B]/BE�B(�B�B
=B
��B
�/B
ȴB
�^B
��B
}�B
gmB
iyB
YB
,B
hB	�B	��B	��B	ƨB	�3B	��B	�B	�B	}�B	aHB	H�B	C�B	>wB	2-B	 �B	VB	  B��B�B�NB�5B�)B�B�B��B��B��BƨBB�}B�^B�?B�!B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�XB��B��B��B��B�}B�}B�}B�wB�}BBŢBǮB��B��B�B�BB�fB�fB�`B�ZB�B�B�B�B�fB�HB�NB�mB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	%B		7B	PB	VB	bB	uB	{B	{B	�B	�B	�B	�B	�B	�B	!�B	(�B	.B	/B	1'B	49B	6FB	9XB	;dB	;dB	>wB	A�B	H�B	J�B	K�B	O�B	Q�B	R�B	S�B	S�B	VB	W
B	XB	YB	ZB	[#B	\)B	[#B	_;B	dZB	ffB	hsB	hsB	iyB	jB	l�B	n�B	p�B	q�B	q�B	t�B	x�B	y�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	}�B	}�B	}�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�DB	�DB	�JB	�VB	�bB	�oB	�{B	��B	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�FB	�FB	�LB	�XB	�dB	�jB	�jB	�jB	�jB	�qB	�wB	��B	��B	��B	��B	B	ÖB	ĜB	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�BB	�HB	�NB	�NB	�NB	�HB	�NB	�TB	�NB	�NB	�NB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
\B
bB
bB
hB
hB
oB
oB
uB
�B
'�B
4n222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B-B-B-B-B/B/B-B.B0!B2-B33B5?B7LB6FB6FB6FB49B33B33B2-B1'B2-B2-B2-B2-B2-B33B33B5?B7LB@�BH�BXBu�B|�B|�B|�B{�Bw�B|�B�B�+B��B�-B�-B�9B�dB��B�B��B1B"�B�BA�BbNBgmBk�Bp�Bo�Bq�Bn�Bl�Bn�Bn�Bl�Bt�B�B� B}�Bw�Bp�BgmB[#B>wB!�BB�B�yB�NB�;B��B�qB��B�%Bu�Bn�B]/BE�B(�B�B
=B
��B
�/B
ȴB
�^B
��B
}�B
gmB
iyB
YB
,B
hB	�B	��B	��B	ƨB	�3B	��B	�B	�B	}�B	aHB	H�B	C�B	>wB	2-B	 �B	VB	  B��B�B�NB�5B�)B�B�B��B��B��BƨBB�}B�^B�?B�!B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�XB��B��B��B��B�}B�}B�}B�wB�}BBŢBǮB��B��B�B�BB�fB�fB�`B�ZB�B�B�B�B�fB�HB�NB�mB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	%B		7B	PB	VB	bB	uB	{B	{B	�B	�B	�B	�B	�B	�B	!�B	(�B	.B	/B	1'B	49B	6FB	9XB	;dB	;dB	>wB	A�B	H�B	J�B	K�B	O�B	Q�B	R�B	S�B	S�B	VB	W
B	XB	YB	ZB	[#B	\)B	[#B	_;B	dZB	ffB	hsB	hsB	iyB	jB	l�B	n�B	p�B	q�B	q�B	t�B	x�B	y�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	}�B	}�B	}�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�DB	�DB	�JB	�VB	�bB	�oB	�{B	��B	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�FB	�FB	�LB	�XB	�dB	�jB	�jB	�jB	�jB	�qB	�wB	��B	��B	��B	��B	B	ÖB	ĜB	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�BB	�HB	�NB	�NB	�NB	�HB	�NB	�TB	�NB	�NB	�NB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
\B
bB
bB
hB
hB
oB
oB
uB
�B
'�B
4n222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190541                              AO  ARCAADJP                                                                    20181005190541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190541  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190541  QCF$                G�O�G�O�G�O�8000            