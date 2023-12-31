CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:09Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191709  20181005191709  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               \A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��dz�Y�1   @��e�mP@4�^5?}�d=�+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      \A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR�CT  CV  CX  CY�fC\  C^  C`  Cb�Cd�Cf  Ch  Ci�fCl  Cn�Cp  Cq�fCt  Cv  Cx  Cy�fC|  C~�C��C��C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C��3C��3C��3C��C��3C��3C�  C�  C��3C��C�  C��C�  C�  C��3C�  C��C��C��C��C�  C��fC��3C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C��C�  C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��3C�  C��C�  C��C��C�  C�  C��C��3C�  C��C��C�  C�  C�  C��3C�  C��C�  C��C��C�  C��3C��3C��3C�  C��C�  C�  C��C��3C��3C�  C��3C��C�  C�  C��C��C��C��3C�  D fD y�D  D�fDfDy�D  D�fD  Dy�D��Dy�D  D� DfD�fD��D� D	  D	y�D
fD
�fD  D� D�D�fDfD� D  D�fDfD� D��Dy�D��Dy�D��Dy�D��Ds3D�3Dy�DfD�fD  D� D  Dy�D��D� D  D� D��D� D  D� D  Dy�D  D� D  D� D  Dy�D   D �fD!  D!� D"  D"�fD#�D#�fD$  D$�fD%fD%�fD&  D&� D'  D'� D(fD(�fD(��D)� D*fD*� D+  D+�fD,fD,�fD-  D-� D.  D.s3D.��D/� D0  D0� D1  D1y�D1��D2y�D2��D3y�D4  D4� D5  D5�fD6  D6� D7fD7�fD8  D8y�D9  D9� D:  D:� D;  D;�fD<fD<� D=  D=y�D=��D>� D?  D?� D@fD@� D@��DA� DBfDB� DC  DC�fDD  DDy�DE  DEy�DE��DFy�DG  DGy�DG��DH� DIfDI� DJ  DJ�fDK  DK� DK��DL� DMfDM�fDN  DN� DOfDO� DO��DPy�DQ  DQ�fDRfDR� DS  DS�fDT  DT� DT��DUy�DU��DVy�DV�3DWy�DX  DX�fDY  DYy�DY��DZ� D[  D[� D\fD\� D\��D]�fD^  D^� D_fD_�fD_��D`s3D`��Day�DbfDb� Dc  Dc� Dd  Dd� Dd��De� DffDf�fDgfDgy�Dh  Dh� Di  Dis3Dj  Dj�fDk  Dky�Dl  Dl� Dm  Dm�fDm��Dny�Dn��Doy�Do��Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dt� Du  Du� Du��Dv�fDw  Dw� Dw�fDy��D�<{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB ��B33B33B33B��B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B�L�B��B��B��gB��gB��B��B��B��B��B��B�L�B�L�B�L�B��gB��gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�3C�C�C�C
�C&gC�C�C�C�C�C�C�C�C�C �C"�C$�C&&gC(�C*�C,�C.�C0�C2�C4�C5�3C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL&gCN�CP�CR&gCT�CV�CX�CY�3C\�C^�C`�Cb&gCd&gCf�Ch�Ci�3Cl�Cn&gCp�Cq�3Ct�Cv�Cx�Cy�3C|�C~&gC�3C�3C�3C�3C�fC���C�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�3C�fC�fC���C���C���C�3C���C���C�fC�fC���C�3C�fC�3C�fC�fC���C�fC�3C�3C�3C�3C�fC���C���C���C���C���C���C�fC�3C�fC���C�fC�fC�fC���C���C�fC�fC���C�fC�3C�3C�fC�fC���C���C���C�fC�3C�fC���C�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�3C�3C���C�fC�3C�fC�3C�3C�fC�fC�3C���C�fC�3C�3C�fC�fC�fC���C�fC�3C�fC�3C�3C�fC���C���C���C�fC�3C�fC�fC�3C���C���C�fC���C�3C�fC�fC�3C�3C�3C���C�fD 	�D |�D3D��D	�D|�D3D��D3D|�D��D|�D3D�3D	�D��D��D�3D	3D	|�D
	�D
��D3D�3D D��D	�D�3D3D��D	�D�3D��D|�D��D|�D��D|�D��DvfD�fD|�D	�D��D3D�3D3D|�D��D�3D3D�3D��D�3D3D�3D3D|�D3D�3D3D�3D3D|�D 3D ��D!3D!�3D"3D"��D# D#��D$3D$��D%	�D%��D&3D&�3D'3D'�3D(	�D(��D(��D)�3D*	�D*�3D+3D+��D,	�D,��D-3D-�3D.3D.vfD.��D/�3D03D0�3D13D1|�D1��D2|�D2��D3|�D43D4�3D53D5��D63D6�3D7	�D7��D83D8|�D93D9�3D:3D:�3D;3D;��D<	�D<�3D=3D=|�D=��D>�3D?3D?�3D@	�D@�3D@��DA�3DB	�DB�3DC3DC��DD3DD|�DE3DE|�DE��DF|�DG3DG|�DG��DH�3DI	�DI�3DJ3DJ��DK3DK�3DK��DL�3DM	�DM��DN3DN�3DO	�DO�3DO��DP|�DQ3DQ��DR	�DR�3DS3DS��DT3DT�3DT��DU|�DU��DV|�DV�fDW|�DX3DX��DY3DY|�DY��DZ�3D[3D[�3D\	�D\�3D\��D]��D^3D^�3D_	�D_��D_��D`vfD`��Da|�Db	�Db�3Dc3Dc�3Dd3Dd�3Dd��De�3Df	�Df��Dg	�Dg|�Dh3Dh�3Di3DivfDj3Dj��Dk3Dk|�Dl3Dl�3Dm3Dm��Dm��Dn|�Dn��Do|�Do��Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds|�Dt3Dt�3Du3Du�3Du��Dv��Dw3Dw�3DwəDy�D�>D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�+A�(�A�$�A�$�A�&�A��A߸RA��`Aݰ!A�p�A�I�A�(�A�
=A��`AܾwA�-A���A��yA�ffA���AΣ�A��/A�ZA��
A�E�A�hsA�bAɉ7A��;Aȕ�A��HA�AþwA�JA��wA��+A���A�?}A���A�p�A��`A�bNA��A���A�E�A���A��A�"�A�O�A��yA���A��!A�33A���A���A��^A��A�r�A���A�33A��9A��A��mA��A��RA�hsA�5?A�9XA��`A�(�A�O�A��A���A�G�A�1A�A��A�C�A�A��A��\A�^5A�A��A�\)A��yA��A��A���A��9Al�A~ffA}�7Az��Ay�TAy/Ax  Au�Ap��Ao��AkdZAfM�Ab-A`�A_K�A\�/A[AZ��AX�AWoATȴAS7LAQoANJAKdZAH�AFjA@�HA>�+A=t�A<z�A;&�A9hsA8��A8(�A7VA6E�A4��A4v�A4  A2r�A0��A/��A.E�A-�A,��A+7LA*JA(��A't�A&VA%hsA%oA$5?A"��A!A!A M�A��A�HA��A�A�PA�Ax�A��AQ�A��Ar�AZAQ�AM�AQ�A=qA�A��A�RA1A�#A�wA�AM�AAG�A��A�A�AG�A�A=qA
=A	�A	7LA�9Av�AE�A��AbNA��A`BA�jAZA$�AhsA��A�
AS�A ��A �!A 1@��@���@�1@�S�@�@�&�@��@�  @�@�~�@��@�`B@�C�@���@���@�1@���@�~�@�=q@���@�X@� �@���@ݺ^@���@�C�@���@׶F@և+@��`@�b@�"�@�$�@�X@� �@�1'@϶F@�@���@��@�%@�33@ʸR@ʗ�@�n�@�v�@�~�@ʏ\@ʏ\@�5?@�`B@�A�@�z�@ț�@��m@�+@Ɵ�@Ɨ�@�~�@�E�@�J@�-@�+@�E�@��@�x�@�hs@�I�@�C�@��@�/@��@�K�@�`B@�?}@��@��!@��m@���@��y@�@�hs@�%@�b@�5?@�j@���@�x�@�n�@�o@�j@��T@�
=@���@��7@�O�@��D@��9@��@�1'@��F@���@��\@��#@��@��#@��@�@�$�@�Ĝ@�o@��j@���@�ȴ@���@��\@�v�@���@�O�@�@���@�G�@�@�@���@�/@���@�Z@�9X@�A�@�
=@��@�M�@���@�b@�j@�9X@��;@�|�@�n�@��j@��u@�j@�1'@�A�@�1@�  @�  @��m@��@�t�@�C�@�@��+@�5?@��@��#@��-@��7@�$�@�V@���@��@�+@�"�@�o@��@�$�@��@�`B@�X@�G�@���@���@�j@��@���@��@�"�@�ȴ@��R@��+@�n�@�ff@�=q@�{@���@���@��@�X@�/@���@���@�A�@���@�ƨ@��@�@���@�ff@��@��@��^@��h@��7@��7@��h@���@���@���@��^@�@�@�@�&�@�1'@��m@�ƨ@���@�K�@�33@��@��H@��\@�^5@�J@�?}@��`@�Q�@�t�@���@�=q@�{@���@��T@���@��-@��@�p�@�X@�?}@��@�Ĝ@��j@��@�Q�@���@��P@�dZ@�\)@�\)@�S�@�33@���@��\@��-@��7@��@�X@�/@�%@��`@���@���@�S�@�C�@�;d@�33@�o@�ȴ@�v�@�{@��@��@��`@���@�V@�%@��@�z�@�1'@��@���@�;d@��@��R@���@�V@�$�@��@���@�@��^@���@�t�@t��@^�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�"�A�+A�(�A�$�A�$�A�&�A��A߸RA��`Aݰ!A�p�A�I�A�(�A�
=A��`AܾwA�-A���A��yA�ffA���AΣ�A��/A�ZA��
A�E�A�hsA�bAɉ7A��;Aȕ�A��HA�AþwA�JA��wA��+A���A�?}A���A�p�A��`A�bNA��A���A�E�A���A��A�"�A�O�A��yA���A��!A�33A���A���A��^A��A�r�A���A�33A��9A��A��mA��A��RA�hsA�5?A�9XA��`A�(�A�O�A��A���A�G�A�1A�A��A�C�A�A��A��\A�^5A�A��A�\)A��yA��A��A���A��9Al�A~ffA}�7Az��Ay�TAy/Ax  Au�Ap��Ao��AkdZAfM�Ab-A`�A_K�A\�/A[AZ��AX�AWoATȴAS7LAQoANJAKdZAH�AFjA@�HA>�+A=t�A<z�A;&�A9hsA8��A8(�A7VA6E�A4��A4v�A4  A2r�A0��A/��A.E�A-�A,��A+7LA*JA(��A't�A&VA%hsA%oA$5?A"��A!A!A M�A��A�HA��A�A�PA�Ax�A��AQ�A��Ar�AZAQ�AM�AQ�A=qA�A��A�RA1A�#A�wA�AM�AAG�A��A�A�AG�A�A=qA
=A	�A	7LA�9Av�AE�A��AbNA��A`BA�jAZA$�AhsA��A�
AS�A ��A �!A 1@��@���@�1@�S�@�@�&�@��@�  @�@�~�@��@�`B@�C�@���@���@�1@���@�~�@�=q@���@�X@� �@���@ݺ^@���@�C�@���@׶F@և+@��`@�b@�"�@�$�@�X@� �@�1'@϶F@�@���@��@�%@�33@ʸR@ʗ�@�n�@�v�@�~�@ʏ\@ʏ\@�5?@�`B@�A�@�z�@ț�@��m@�+@Ɵ�@Ɨ�@�~�@�E�@�J@�-@�+@�E�@��@�x�@�hs@�I�@�C�@��@�/@��@�K�@�`B@�?}@��@��!@��m@���@��y@�@�hs@�%@�b@�5?@�j@���@�x�@�n�@�o@�j@��T@�
=@���@��7@�O�@��D@��9@��@�1'@��F@���@��\@��#@��@��#@��@�@�$�@�Ĝ@�o@��j@���@�ȴ@���@��\@�v�@���@�O�@�@���@�G�@�@�@���@�/@���@�Z@�9X@�A�@�
=@��@�M�@���@�b@�j@�9X@��;@�|�@�n�@��j@��u@�j@�1'@�A�@�1@�  @�  @��m@��@�t�@�C�@�@��+@�5?@��@��#@��-@��7@�$�@�V@���@��@�+@�"�@�o@��@�$�@��@�`B@�X@�G�@���@���@�j@��@���@��@�"�@�ȴ@��R@��+@�n�@�ff@�=q@�{@���@���@��@�X@�/@���@���@�A�@���@�ƨ@��@�@���@�ff@��@��@��^@��h@��7@��7@��h@���@���@���@��^@�@�@�@�&�@�1'@��m@�ƨ@���@�K�@�33@��@��H@��\@�^5@�J@�?}@��`@�Q�@�t�@���@�=q@�{@���@��T@���@��-@��@�p�@�X@�?}@��@�Ĝ@��j@��@�Q�@���@��P@�dZ@�\)@�\)@�S�@�33@���@��\@��-@��7@��@�X@�/@�%@��`@���@���@�S�@�C�@�;d@�33@�o@�ȴ@�v�@�{@��@��@��`@���@�V@�%@��@�z�@�1'@��@���@�;d@��@��R@���@�V@�$�@��@���@�@��^@���@�t�@t��@^�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BE�BE�BE�BE�BE�BE�BC�B=qB49B(�B$�B"�B �B�B�B�BuBBB%B%B%B	7B
=BPB�B�B�B%�B,B.B33BC�B[#BbNBn�B�B�=B�JB�\B�\B�bB��B��B��B��B��B��B��B��B�uB�PB�Br�BbNBL�BB�B49B(�B'�B=qBD�BE�B?}B5?B#�B\B  B�B�NBȴB�XB��B�\BdZBQ�BJ�B?}B49B�B
��B
��B
�B
�ZB
�
B
�jB
�3B
�B
��B
�=B
k�B
L�B
D�B
<jB
)�B
!�B
�B
bB	��B	�#B	��B	�9B	�hB	z�B	n�B	e`B	YB	Q�B	H�B	@�B	6FB	)�B	 �B	�B	1B��B�B�HB��B��BɺBŢB�}B�dB�qB�qB�dB�XB�FB�?B�3B�!B�B��B��B��B��B��B��B��B�bB�VB�VB�JB�DB�1B�+B�B�B�B�B�=B�JB�VB�\B�\B�\B�hB�bB�bB�hB�hB�hB�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�{B��B��B�uB�uB�uB�oB�uB�oB�oB�hB�{B��B��B��B�{B�oB�oB�oB�hB�bB�bB�\B�\B�PB�=B�7B�1B�1B�%B�%B�B�B�%B�1B�1B�+B�B�7B�\B�oB�oB�oB�uB�{B��B��B��B��B��B��B��B�B�B�'B�'B�-B�-B�?B�RB�jBŢBƨBBǮB��B��B��B��B��B��B��B�/B�/B�BƨB�jB�^B�dB�qB�jB�jB�jB�RB�FB�-B�9BBɺB�BB�B��B	�B	�B	�B	�B	�B	%�B	!�B	�B	 �B	�B	�B	 �B	%�B	!�B	�B	�B	�B	�B	hB	hB	uB	�B	�B	�B	 �B	$�B	.B	G�B	O�B	W
B	YB	YB	YB	XB	W
B	XB	ZB	]/B	]/B	hsB	t�B	x�B	{�B	}�B	~�B	|�B	z�B	|�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�+B	�+B	�1B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�9B	�XB	�dB	�dB	�qB	�qB	�wB	�}B	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�NB	�`B	�fB	�fB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B	��B	��B	��B	��B	��B
  B
B
	7B
	7B
	7B
	7B

=B
	�B
 �B
'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BE�BE�BE�BE�BE�BE�BC�B=qB49B(�B$�B"�B �B�B�B�BuBBB%B%B%B	7B
=BPB�B�B�B%�B,B.B33BC�B[#BbNBn�B�B�=B�JB�\B�\B�bB��B��B��B��B��B��B��B��B�uB�PB�Br�BbNBL�BB�B49B(�B'�B=qBD�BE�B?}B5?B#�B\B  B�B�NBȴB�XB��B�\BdZBQ�BJ�B?}B49B�B
��B
��B
�B
�ZB
�
B
�jB
�3B
�B
��B
�=B
k�B
L�B
D�B
<jB
)�B
!�B
�B
bB	��B	�#B	��B	�9B	�hB	z�B	n�B	e`B	YB	Q�B	H�B	@�B	6FB	)�B	 �B	�B	1B��B�B�HB��B��BɺBŢB�}B�dB�qB�qB�dB�XB�FB�?B�3B�!B�B��B��B��B��B��B��B��B�bB�VB�VB�JB�DB�1B�+B�B�B�B�B�=B�JB�VB�\B�\B�\B�hB�bB�bB�hB�hB�hB�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�{B��B��B�uB�uB�uB�oB�uB�oB�oB�hB�{B��B��B��B�{B�oB�oB�oB�hB�bB�bB�\B�\B�PB�=B�7B�1B�1B�%B�%B�B�B�%B�1B�1B�+B�B�7B�\B�oB�oB�oB�uB�{B��B��B��B��B��B��B��B�B�B�'B�'B�-B�-B�?B�RB�jBŢBƨBBǮB��B��B��B��B��B��B��B�/B�/B�BƨB�jB�^B�dB�qB�jB�jB�jB�RB�FB�-B�9BBɺB�BB�B��B	�B	�B	�B	�B	�B	%�B	!�B	�B	 �B	�B	�B	 �B	%�B	!�B	�B	�B	�B	�B	hB	hB	uB	�B	�B	�B	 �B	$�B	.B	G�B	O�B	W
B	YB	YB	YB	XB	W
B	XB	ZB	]/B	]/B	hsB	t�B	x�B	{�B	}�B	~�B	|�B	z�B	|�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�+B	�+B	�1B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�9B	�XB	�dB	�dB	�qB	�qB	�wB	�}B	�}B	��B	B	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�NB	�`B	�fB	�fB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B	��B	��B	��B	��B	��B
  B
B
	7B
	7B
	7B
	7B

=B
	�B
 �B
'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191709                              AO  ARCAADJP                                                                    20181005191709    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191709  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191709  QCF$                G�O�G�O�G�O�8000            