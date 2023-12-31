CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:48Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191648  20181005191648  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @מm���l1   @מn��@2�S����c�XbM�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC�fC�fC�fC�fC  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2�C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Cg�fCi�fCk�fCm�fCp  Cr  Ct  Cv  Cx�Cz�C|  C}�fC�fC�  C��C��C��C�  C�  C�  C��3C�  C�  C��C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C��C��C�  C�  C��3C�  C��C�  C��3C��3C��3C��3C��3C�  C��C��C��C��C��C��C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C��3C��3C��C�  C��3C�  C�  C��3C��3C��3C�  C�  C��C��3C�  C��C�  C��3C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C��C��C�  C��3C��3C��3C��3C�  C��C�  C��3C��fC��3C��3C��C�  C��3C�  C��C��C��C�  C��3C��3C��C�  C��3C��C�  C��3C�  C�  C��3C�  C��C��3C�  C�  C�  C��3D y�DfDy�D  D��DfDy�D��D� D��D�fD  Dy�DfD� D��D� D��D	�fD
  D
� D  D� D  Dy�D  D� D  D� D  D�fD  D� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0y�D1  D1� D2  D2y�D3  D3� D4  D4�fD5  D5� D6  D6�fD7  D7� D8  D8y�D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?�fD?��D@� DA  DA� DA��DB� DB��DC� DDfDDy�DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI�fDJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN�fDO  DOy�DPfDP� DQfDQy�DRfDRy�DSfDSy�DT  DT�fDT��DU�fDVfDVs3DW  DW�fDX  DXy�DX��DY�fDY��DZ� D[�D[� D[��D\�fD\��D]� D^  D^y�D_fD_�fD`�D`��Da�Da�fDb  Dby�Db��Dcs3Dc��Ddy�Dd��Dey�De��Dfy�Dg  Dgy�Dg��Dhy�Dh�3Dis3Di��Djy�Dj��Dks3Dl  Dl� DmfDm� Dn  Dn� DofDo�fDp  Dp� Dq  Dq� Dr  Dr�fDs  Dsy�Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dw� Dy�3D�XRD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��gB��gB��B��B��B��B��C �C�C�C�C�C
�C�3C�3C�3C�3C�3C�C�C�C�C�C �C"�C$�C&&gC(�C*�C,�C.�C0�C2&gC4�C6&gC8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CM�3CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cc�3Cf�Cg�3Ci�3Ck�3Cm�3Cp�Cr�Ct�Cv�Cx&gCz&gC|�C}�3C�3C�fC�3C�3C�3C�fC�fC�fC���C�fC�fC�3C�fC�fC�3C�fC���C�fC�fC���C�fC�fC���C���C���C�fC�fC�3C�3C�fC�fC���C�fC�3C�fC���C���C���C���C���C�fC�3C�3C�3C�3C�3C�3C�fC�3C�fC�fC�3C�fC�fC�3C�fC�fC�3C�fC���C���C���C�3C�fC���C�fC�fC���C���C���C�fC�fC�3C���C�fC�3C�fC���C�fC�fC�fC�3C�3C�3C�fC���C���C�fC�fC�  C�3C�fC���C���C���C���C�fC�  C�fC���C���C���C���C�3C�fC���C�fC�3C�  C�3C�fC���C���C�3C�fC���C�3C�fC���C�fC�fC���C�fC�3C���C�fC�fC�fC���D |�D	�D|�D3D� D	�D|�D��D�3D��D��D3D|�D	�D�3D��D�3D��D	��D
3D
�3D3D�3D3D|�D3D�3D3D�3D3D��D3D�3D3D�3D	�D�3D3D�3D3D|�D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D|�D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%|�D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.	�D.�3D/3D/�3D03D0|�D13D1�3D23D2|�D33D3�3D43D4��D53D5�3D63D6��D73D7�3D83D8|�D9	�D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D>��D?��D?��D@�3DA3DA�3DA��DB�3DB��DC�3DD	�DD|�DE3DE�3DF3DF�3DG3DG|�DH3DH�3DI3DI��DJ3DJ�3DK3DK�3DL	�DL�3DM3DM�3DN3DN��DO3DO|�DP	�DP�3DQ	�DQ|�DR	�DR|�DS	�DS|�DT3DT��DT��DU��DV	�DVvfDW3DW��DX3DX|�DX��DY��DY��DZ�3D[ D[�3D[��D\��D\��D]�3D^3D^|�D_	�D_��D` D`� Da Da��Db3Db|�Db��DcvfDc��Dd|�Dd��De|�De��Df|�Dg3Dg|�Dg��Dh|�Dh�fDivfDi��Dj|�Dj��DkvfDl3Dl�3Dm	�Dm�3Dn3Dn�3Do	�Do��Dp3Dp�3Dq3Dq�3Dr3Dr��Ds3Ds|�Dt3Dt��Du3Du�3Dv3Dv�3Dw3Dw�3Dw�3Dy�fD�Y�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�^5A�hsA�l�A�jA�l�A�l�A�n�A�p�A�p�A�r�A�r�A�t�A�t�A�v�A�|�A�z�A�z�A�|�A�|�AǏ\Aǝ�Aǟ�Aǟ�Aǡ�Aǣ�AǮAǴ9A�G�A��Aƺ^A��AŋDA�E�A�  Aĩ�A�9XA��
A��RA�C�A��DA���A�v�A�JA���A���A�$�A�VA��uA���A��-A��\A�ȴA�oA��A�"�A��A���A�1'A��#A���A��RA��FA�7LA� �A��`A�%A���A�C�A���A���A���A�  A���A���A�/A�hsA���A�n�A��A�A�A��uA�K�A�S�A���A��A�;dA�l�A�ƨA�1A�5?A��!A�VA��!A���A���A�`BA��A���A���A��A�JA���A��^A���Az�RAy7LAwp�At�RAqƨApAoVAjffAfjAc��A_��A[\)AX�AW%AU��AS�#AQoAO�hAN  AL=qAJ�/AI�AG
=ADQ�AB�DA>��A<�A:{A9%A7��A6n�A5`BA4��A4bA3+A0A.jA-��A-l�A-/A,ȴA+��A+�A(��A&�A$�9A#�mA#/A!&�A �+A I�A�uA��A�FAbA33AVA-A{AJA��AhsA�A��A`BA��AȴAr�A�A�-A�^A	;dA�/AĜA��AA�A��AA�A�Al�AK�A+A��AVAQ�AA�PA33A �A �@�E�@�&�@�Q�@�\)@���@���@�1'@�+@��@�ff@��7@��m@��@��@�9X@���@���@��@�`B@��/@㝲@�^5@���@��@�Q�@�@���@ݩ�@�x�@݁@ݩ�@�9X@ڇ+@��@�r�@ץ�@�dZ@�;d@���@�-@��#@Ԭ@��;@Ұ!@��T@�?}@�t�@�-@�V@˅@�+@ʧ�@��@�z�@�l�@�{@ř�@�x�@�`B@�G�@�7L@��@ÍP@��@\@��@��j@���@��P@�C�@�K�@�ȴ@��@�{@�{@��^@�p�@�G�@��@��j@�I�@�"�@�$�@��#@���@�O�@��`@��@��@��P@���@�/@�C�@�
=@��y@���@���@��+@���@���@�V@��D@�I�@��@��@�ff@�$�@�@���@�X@�%@�Ĝ@���@��D@��@��P@�+@��y@�ȴ@��!@���@���@�V@�5?@�-@��#@�{@�%@��@��@�A�@�C�@�~�@�J@��-@���@�7L@���@��u@��@�r�@�Q�@�9X@��@�b@��m@��
@��F@���@�K�@��@��y@�-@���@���@��h@��@�p�@�hs@�?}@���@���@��@�z�@�Z@�9X@���@��;@��@���@�l�@��\@�-@�@���@���@��@�G�@�?}@��@��`@��u@��F@�t�@��y@��!@���@���@�
=@�S�@�K�@�"�@���@�$�@���@��h@�x�@�`B@��@�9X@���@��P@�t�@��@��\@�E�@�=q@�=q@�5?@��@��-@�O�@��@��D@��;@��F@��P@�S�@�+@�+@�@�n�@�{@�@���@�x�@�X@���@���@���@��j@���@�z�@�Q�@��@�33@�33@��@��y@���@��@���@�ff@�E�@�M�@�=q@��#@��@�bN@�  @��m@��@��@�t�@�l�@���@�E�@�{@��T@���@�hs@�&�@�bN@�|�@��@��@���@��!@���@�~�@�ff@�=q@�$�@��#@���@���@���@��-@��^@��^@��-@��-@���@���@���@�V@���@�1@�b@��@��@��m@��F@���@�|�@��H@��y@�dZ@�\)@�Z@q��@[�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�^5A�hsA�l�A�jA�l�A�l�A�n�A�p�A�p�A�r�A�r�A�t�A�t�A�v�A�|�A�z�A�z�A�|�A�|�AǏ\Aǝ�Aǟ�Aǟ�Aǡ�Aǣ�AǮAǴ9A�G�A��Aƺ^A��AŋDA�E�A�  Aĩ�A�9XA��
A��RA�C�A��DA���A�v�A�JA���A���A�$�A�VA��uA���A��-A��\A�ȴA�oA��A�"�A��A���A�1'A��#A���A��RA��FA�7LA� �A��`A�%A���A�C�A���A���A���A�  A���A���A�/A�hsA���A�n�A��A�A�A��uA�K�A�S�A���A��A�;dA�l�A�ƨA�1A�5?A��!A�VA��!A���A���A�`BA��A���A���A��A�JA���A��^A���Az�RAy7LAwp�At�RAqƨApAoVAjffAfjAc��A_��A[\)AX�AW%AU��AS�#AQoAO�hAN  AL=qAJ�/AI�AG
=ADQ�AB�DA>��A<�A:{A9%A7��A6n�A5`BA4��A4bA3+A0A.jA-��A-l�A-/A,ȴA+��A+�A(��A&�A$�9A#�mA#/A!&�A �+A I�A�uA��A�FAbA33AVA-A{AJA��AhsA�A��A`BA��AȴAr�A�A�-A�^A	;dA�/AĜA��AA�A��AA�A�Al�AK�A+A��AVAQ�AA�PA33A �A �@�E�@�&�@�Q�@�\)@���@���@�1'@�+@��@�ff@��7@��m@��@��@�9X@���@���@��@�`B@��/@㝲@�^5@���@��@�Q�@�@���@ݩ�@�x�@݁@ݩ�@�9X@ڇ+@��@�r�@ץ�@�dZ@�;d@���@�-@��#@Ԭ@��;@Ұ!@��T@�?}@�t�@�-@�V@˅@�+@ʧ�@��@�z�@�l�@�{@ř�@�x�@�`B@�G�@�7L@��@ÍP@��@\@��@��j@���@��P@�C�@�K�@�ȴ@��@�{@�{@��^@�p�@�G�@��@��j@�I�@�"�@�$�@��#@���@�O�@��`@��@��@��P@���@�/@�C�@�
=@��y@���@���@��+@���@���@�V@��D@�I�@��@��@�ff@�$�@�@���@�X@�%@�Ĝ@���@��D@��@��P@�+@��y@�ȴ@��!@���@���@�V@�5?@�-@��#@�{@�%@��@��@�A�@�C�@�~�@�J@��-@���@�7L@���@��u@��@�r�@�Q�@�9X@��@�b@��m@��
@��F@���@�K�@��@��y@�-@���@���@��h@��@�p�@�hs@�?}@���@���@��@�z�@�Z@�9X@���@��;@��@���@�l�@��\@�-@�@���@���@��@�G�@�?}@��@��`@��u@��F@�t�@��y@��!@���@���@�
=@�S�@�K�@�"�@���@�$�@���@��h@�x�@�`B@��@�9X@���@��P@�t�@��@��\@�E�@�=q@�=q@�5?@��@��-@�O�@��@��D@��;@��F@��P@�S�@�+@�+@�@�n�@�{@�@���@�x�@�X@���@���@���@��j@���@�z�@�Q�@��@�33@�33@��@��y@���@��@���@�ff@�E�@�M�@�=q@��#@��@�bN@�  @��m@��@��@�t�@�l�@���@�E�@�{@��T@���@�hs@�&�@�bN@�|�@��@��@���@��!@���@�~�@�ff@�=q@�$�@��#@���@���@���@��-@��^@��^@��-@��-@���@���@���@�V@���@�1@�b@��@��@��m@��F@���@�|�@��H@��y@�dZ@�\)@�Z@q��@[�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1'B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B33B33B33B5?B49B49B49B49B;dBB�BB�BC�BD�BF�BJ�BW
BaHBm�B�DB�B��B��B�B�HB�BB�B.B33B6FB>wBB�BC�BB�BA�BVB[#BZBW
BXBYBZB[#B_;Be`BffBt�Bz�Bx�Bx�B�B�bB��B��B�B��B��B�\B|�BhsB]/BZBW
BP�BE�B;dB49B(�BoBB�B��B�^B��B��B�=B\)BA�B7LB.B$�B �B�B�B1B
��B
�HB
�FB
��B
�\B
|�B
aHB
7LB
B	�B	�HB	��B	�XB	�B	��B	�=B	n�B	\)B	C�B	)�B	�B	hB	PB	1B	B��B��B�B�B�TB�BƨB�}B�FB�B��B��B��B��B��B��B��B�bB�DB�DB�JB�DB�DB�=B�7B�+B�%B�B�B�B�B� B~�B}�B}�B{�B|�B}�B~�B� B�B�B�B�B� B~�B}�B|�Bz�Bt�BgmBdZBaHB_;B[#B[#BZBZBZB^5BZB]/BbNBe`BgmBhsBn�B�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B�B�B�B�B�B�B�B�B�!B�!B�!B�RB�jB�qB��BĜBĜBƨB��B��B��B��B�B�B�
B�
B�
B�
B�BB�ZB�TB�`B�mB�yB�yB�B�B�B��B��B��B��B��B��B��B��B	B	
=B	\B	bB	bB	hB	oB	{B	�B	�B	�B	$�B	/B	0!B	0!B	1'B	1'B	2-B	5?B	7LB	:^B	=qB	?}B	C�B	F�B	H�B	J�B	O�B	P�B	R�B	T�B	W
B	XB	YB	\)B	`BB	cTB	e`B	e`B	ffB	ffB	ffB	hsB	iyB	jB	l�B	t�B	x�B	}�B	� B	~�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�7B	�=B	�=B	�JB	�PB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�RB	�jB	�}B	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�fB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
VB
�B
"�B
5?222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B1'B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B33B33B33B5?B49B49B49B49B;dBB�BB�BC�BD�BF�BJ�BW
BaHBm�B�DB�B��B��B�B�HB�BB�B.B33B6FB>wBB�BC�BB�BA�BVB[#BZBW
BXBYBZB[#B_;Be`BffBt�Bz�Bx�Bx�B�B�bB��B��B�B��B��B�\B|�BhsB]/BZBW
BP�BE�B;dB49B(�BoBB�B��B�^B��B��B�=B\)BA�B7LB.B$�B �B�B�B1B
��B
�HB
�FB
��B
�\B
|�B
aHB
7LB
B	�B	�HB	��B	�XB	�B	��B	�=B	n�B	\)B	C�B	)�B	�B	hB	PB	1B	B��B��B�B�B�TB�BƨB�}B�FB�B��B��B��B��B��B��B��B�bB�DB�DB�JB�DB�DB�=B�7B�+B�%B�B�B�B�B� B~�B}�B}�B{�B|�B}�B~�B� B�B�B�B�B� B~�B}�B|�Bz�Bt�BgmBdZBaHB_;B[#B[#BZBZBZB^5BZB]/BbNBe`BgmBhsBn�B�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B�B�B�B�B�B�B�B�B�!B�!B�!B�RB�jB�qB��BĜBĜBƨB��B��B��B��B�B�B�
B�
B�
B�
B�BB�ZB�TB�`B�mB�yB�yB�B�B�B��B��B��B��B��B��B��B��B	B	
=B	\B	bB	bB	hB	oB	{B	�B	�B	�B	$�B	/B	0!B	0!B	1'B	1'B	2-B	5?B	7LB	:^B	=qB	?}B	C�B	F�B	H�B	J�B	O�B	P�B	R�B	T�B	W
B	XB	YB	\)B	`BB	cTB	e`B	e`B	ffB	ffB	ffB	hsB	iyB	jB	l�B	t�B	x�B	}�B	� B	~�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�7B	�=B	�=B	�JB	�PB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�RB	�jB	�}B	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�fB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
VB
�B
"�B
5?222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191648                              AO  ARCAADJP                                                                    20181005191648    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191648  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191648  QCF$                G�O�G�O�G�O�8000            