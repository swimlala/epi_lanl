CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:09Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190509  20181005190509  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׫����1   @׫�/h^2@2��-V�c�&�x��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  BffB  B   B(  B0  B7��B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C�C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C��C��C��C��C��C�  C�  C��C�  C��3D y�D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D	��D
� D  D� D  Dy�D  D�fD  D� D  D� D  D�fD  D� D  D�fD  D� DfD�fD  D� D  D� D  D�fD  D� DfD� D  D� D  D� D  D� D  D� D  Dy�D  D� D��D � D!  D!� D"  D"� D#fD#� D$  D$�fD%fD%�fD&  D&� D'  D'�fD(fD(� D)  D)� D)��D*� D+  D+� D,  D,y�D,��D-� D.fD.� D/  D/� D0  D0�fD1  D1y�D1��D2y�D2��D3� D4  D4�fD5  D5y�D5��D6� D7  D7�fD8  D8� D9  D9� D:  D:�fD;fD;� D<  D<� D=  D=y�D>  D>�fD?  D?y�D@  D@� DA  DAy�DB  DB� DC  DC� DC��DD� DE  DE� DF  DFy�DG  DG�fDHfDH� DH��DI� DJ  DJ� DK  DK�fDLfDL� DL��DMy�DN  DN� DOfDO� DP  DPy�DP��DQy�DR  DR� DS  DS�fDT  DTy�DU  DUy�DV  DV� DV��DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`fD`�fDafDa�fDb  Db� Dc  Dc� Dc��Dd� De  Dey�De��Df� DgfDg� Dg��Dh� Di  Di� DjfDj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp�fDqfDq� Dq��Dr� DsfDs�fDt  Dt� Du  Du� DvfDv� Dw  Dw� Dw� Dy�HD�1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�33A�ffA�ffA�ffA�ffB 33B33B��B33B 33B(33B033B7��B@33BG��BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B�L�B�L�B�L�B��B��B��B��B��B�L�B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C&gC�C&gC�C�3C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CS�3CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf&gCh�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz&gC|&gC~&gC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC���C�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC���C���C���C�fC�fC���C�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C�3C�3C�fC���C�fC�fC�fC���C���C�fC�fC�fC�fC���C���C�fC�fC�3C�3C�3C�3C�3C�fC�fC�3C�fC���D |�D3D�3D��D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D|�D	3D	�3D	��D
�3D3D�3D3D|�D3D��D3D�3D3D�3D3D��D3D�3D3D��D3D�3D	�D��D3D�3D3D�3D3D��D3D�3D	�D�3D3D�3D3D�3D3D�3D3D�3D3D|�D3D�3D��D �3D!3D!�3D"3D"�3D#	�D#�3D$3D$��D%	�D%��D&3D&�3D'3D'��D(	�D(�3D)3D)�3D)��D*�3D+3D+�3D,3D,|�D,��D-�3D.	�D.�3D/3D/�3D03D0��D13D1|�D1��D2|�D2��D3�3D43D4��D53D5|�D5��D6�3D73D7��D83D8�3D93D9�3D:3D:��D;	�D;�3D<3D<�3D=3D=|�D>3D>��D?3D?|�D@3D@�3DA3DA|�DB3DB�3DC3DC�3DC��DD�3DE3DE�3DF3DF|�DG3DG��DH	�DH�3DH��DI�3DJ3DJ�3DK3DK��DL	�DL�3DL��DM|�DN3DN�3DO	�DO�3DP3DP|�DP��DQ|�DR3DR�3DS3DS��DT3DT|�DU3DU|�DV3DV�3DV��DW�3DX3DX�3DY3DY�3DY��DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_��D`	�D`��Da	�Da��Db3Db�3Dc3Dc�3Dc��Dd�3De3De|�De��Df�3Dg	�Dg�3Dg��Dh�3Di3Di�3Dj	�Dj�3Dk3Dk|�Dl3Dl�3Dm3Dm�3Dn3Dn�3Do	�Do�3Dp3Dp��Dq	�Dq�3Dq��Dr�3Ds	�Ds��Dt3Dt�3Du3Du�3Dv	�Dv�3Dw3Dw�3Dw�3Dy�{D�3411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��/AͶFA�&�A��A��;A���A�A̼jA̸RA̴9A̲-A̮A̩�A̧�Ḁ�Ḁ�Ạ�A̡�A̟�A̝�A̛�A̓uA�t�A��A���A���A˾wA�A˶FA�p�A�dZAˉ7A˺^A˾wA�ƨẢ7A�+A�I�A�I�A�+A�&�A��`A˸RA�E�A�  A��mAȇ+A�M�A���A�t�A�dZA�A�K�A�n�AǁA�ZAőhA�
=A���A�ĜAĩ�A��A�%AA�~�A�bNA��A�"�A���A���A��A���A� �A�`BA�+A��A�A��TA���A��yA�O�A��hA���A��^A��yA�bNA�^5A��9A��yA��A�5?A��;A�9XA��A���A�n�A��uA��A�1'A��\A�C�A�9XA�-A�(�A���A�dZA��A��mA��mA��DA�K�A��A��;A���A�1A{p�Ax�ArJAn��Al$�Akt�AjjAhv�AdffAaƨAa�A_�PA\jAV��AS��AO�PAJM�AE33AB�RAA�mA@��A>�9A=�A<=qA:~�A9p�A8I�A7C�A6��A5/A3&�A29XA1S�A/ƨA/7LA.-A,~�A+S�A)��A(bA'`BA&A�A%+A#��A#33A#�A"JA VA�A�wA�A��A 5?A jA �uA �\A Q�A��A�Ar�A�AI�A�-AO�Av�A��A�uAl�A�AZA  A�A�^Ap�A��A�A��A��A�^A��AoA
 �A	hsAQ�A�#AbNA`BA��A�uAK�A�wA �/@�l�@��@���@���@���@���@��!@�M�@�@�G�@�"�@�z�@��@�o@�+@���@�x�@�|�@�Z@�"�@�@�Ĝ@��@�O�@ߕ�@�n�@�7L@�  @�@��@�"�@�V@Ձ@��@�A�@�ȴ@�~�@��#@�z�@���@���@�E�@�@�X@̣�@�(�@��@˶F@�"�@�V@���@��@�z�@�b@ǅ@��H@�@ă@���@��@+@�J@��^@�hs@���@�Q�@��@�=q@��h@�%@���@�o@�n�@���@�Z@�Q�@�Q�@�I�@�1'@���@��!@��y@�
=@��H@��\@���@���@�z�@�Q�@�  @��@�C�@�+@�
=@��\@��@���@��#@�{@��-@���@�Q�@�r�@��9@���@���@���@���@�1@��w@�dZ@��R@��@��@�O�@�/@�%@���@��D@�j@��
@���@�S�@���@�E�@�J@�@���@���@���@��@�O�@���@��@���@��u@��m@��@�+@�ȴ@�^5@��@���@�?}@�r�@�ƨ@�  @���@���@�J@�@�`B@�V@��@��m@�@�\)@��H@�ff@��T@��#@���@��^@���@���@���@��^@���@���@���@��u@�j@��@���@��@�@��@��!@��\@�V@��@�O�@��@���@���@��@��@�(�@��;@��@�;d@�o@���@��@��y@��R@��+@�n�@�V@��@��#@���@�hs@�X@�O�@�7L@��@���@��j@��@�A�@���@�33@���@��!@�v�@�E�@�5?@��@�{@��@���@�hs@�O�@�?}@��^@�^5@��@�l�@���@�~�@�~�@�=q@�/@��D@�j@�1@���@�K�@���@��@�$�@�J@���@�&�@��@���@��@�9X@��P@�dZ@�\)@�33@�S�@�ƨ@�dZ@�dZ@�;d@��@�o@��y@���@��@�x�@�p�@�G�@�7L@�V@��/@�Ĝ@���@�bN@��m@���@��@���@��@�l�@���@��R@���@��@�J@�@��T@��-@��@��h@qs�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��/AͶFA�&�A��A��;A���A�A̼jA̸RA̴9A̲-A̮A̩�A̧�Ḁ�Ḁ�Ạ�A̡�A̟�A̝�A̛�A̓uA�t�A��A���A���A˾wA�A˶FA�p�A�dZAˉ7A˺^A˾wA�ƨẢ7A�+A�I�A�I�A�+A�&�A��`A˸RA�E�A�  A��mAȇ+A�M�A���A�t�A�dZA�A�K�A�n�AǁA�ZAőhA�
=A���A�ĜAĩ�A��A�%AA�~�A�bNA��A�"�A���A���A��A���A� �A�`BA�+A��A�A��TA���A��yA�O�A��hA���A��^A��yA�bNA�^5A��9A��yA��A�5?A��;A�9XA��A���A�n�A��uA��A�1'A��\A�C�A�9XA�-A�(�A���A�dZA��A��mA��mA��DA�K�A��A��;A���A�1A{p�Ax�ArJAn��Al$�Akt�AjjAhv�AdffAaƨAa�A_�PA\jAV��AS��AO�PAJM�AE33AB�RAA�mA@��A>�9A=�A<=qA:~�A9p�A8I�A7C�A6��A5/A3&�A29XA1S�A/ƨA/7LA.-A,~�A+S�A)��A(bA'`BA&A�A%+A#��A#33A#�A"JA VA�A�wA�A��A 5?A jA �uA �\A Q�A��A�Ar�A�AI�A�-AO�Av�A��A�uAl�A�AZA  A�A�^Ap�A��A�A��A��A�^A��AoA
 �A	hsAQ�A�#AbNA`BA��A�uAK�A�wA �/@�l�@��@���@���@���@���@��!@�M�@�@�G�@�"�@�z�@��@�o@�+@���@�x�@�|�@�Z@�"�@�@�Ĝ@��@�O�@ߕ�@�n�@�7L@�  @�@��@�"�@�V@Ձ@��@�A�@�ȴ@�~�@��#@�z�@���@���@�E�@�@�X@̣�@�(�@��@˶F@�"�@�V@���@��@�z�@�b@ǅ@��H@�@ă@���@��@+@�J@��^@�hs@���@�Q�@��@�=q@��h@�%@���@�o@�n�@���@�Z@�Q�@�Q�@�I�@�1'@���@��!@��y@�
=@��H@��\@���@���@�z�@�Q�@�  @��@�C�@�+@�
=@��\@��@���@��#@�{@��-@���@�Q�@�r�@��9@���@���@���@���@�1@��w@�dZ@��R@��@��@�O�@�/@�%@���@��D@�j@��
@���@�S�@���@�E�@�J@�@���@���@���@��@�O�@���@��@���@��u@��m@��@�+@�ȴ@�^5@��@���@�?}@�r�@�ƨ@�  @���@���@�J@�@�`B@�V@��@��m@�@�\)@��H@�ff@��T@��#@���@��^@���@���@���@��^@���@���@���@��u@�j@��@���@��@�@��@��!@��\@�V@��@�O�@��@���@���@��@��@�(�@��;@��@�;d@�o@���@��@��y@��R@��+@�n�@�V@��@��#@���@�hs@�X@�O�@�7L@��@���@��j@��@�A�@���@�33@���@��!@�v�@�E�@�5?@��@�{@��@���@�hs@�O�@�?}@��^@�^5@��@�l�@���@�~�@�~�@�=q@�/@��D@�j@�1@���@�K�@���@��@�$�@�J@���@�&�@��@���@��@�9X@��P@�dZ@�\)@�33@�S�@�ƨ@�dZ@�dZ@�;d@��@�o@��y@���@��@�x�@�p�@�G�@�7L@�V@��/@�Ĝ@���@�bN@��m@���@��@���@��@�l�@���@��R@���@��@�J@�@��T@��-@��@��h@qs�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�{B	�oB	�\B	�\B	�\B	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�7B	z�B	o�B	o�B	u�B	y�B	|�B	x�B	{�B	��B	�wB	��B	�/B
�B
R�B
dZB
k�B
n�B
r�B
x�B
�B
��B
��B
�B
��B
v�B
�%B
��B
��BJB �B,BF�B�7B��B�XBȴB��B�/B�B�B33B8RB;dB7LBC�BJ�B=qB+B"�B�BVB��B�BB�ZB�mBǮB�XB�B��B��B�-B��Bp�B`BBR�B?}B5?B.B'�B#�B>wBS�Bl�Bx�B~�BcTB\)BW
BJ�B?}B"�BB
�B
�TB
��B
�LB
�{B
�%B
r�B
e`B
7LB
oB	�B	�B	�LB	��B	��B	�oB	�JB	�B	o�B	bNB	^5B	T�B	E�B	-B	�B	DB��B�`B�HB�BB�5B�/B�B�B�B�/B�`B�fB�fB�sB�B�B�B�B�B�B�sB�TB�B��B��B��B��BǮBB��B�jB�RB�LB�LB�qBɺB��B��B�)B�NB�ZB�ZB�mB�sB�yB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�fB�HB�BB�BB�)B�
B�B��B��B��B��B��B��B��B��B��B��BɺBŢB�}B�jB�dB�^B�^B�RB�9B�-B�'B�!B�!B�B�B�B�!B�!B�B�B�'B�-B�3B�9B�?B�XB�XB�dB��BĜBǮBɺB��B��B��B��B��B��B��B�B�#B�)B�#B�)B�HB�`B�B�B�B��B��B��B	B	B	%B	+B	1B	1B	+B	+B	
=B	JB	\B	hB	�B	�B	�B	�B	�B	 �B	)�B	33B	7LB	7LB	7LB	9XB	9XB	=qB	?}B	?}B	?}B	@�B	@�B	A�B	C�B	G�B	H�B	J�B	M�B	O�B	O�B	S�B	T�B	W
B	W
B	ZB	]/B	^5B	dZB	ffB	gmB	iyB	k�B	m�B	n�B	o�B	q�B	u�B	w�B	y�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�=B	�VB	�\B	�\B	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�FB	�^B	�qB	�qB	�wB	�wB	��B	ÖB	B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B	��B	��B	��B	��B
  B
B
B
B
%B
%B
%B
%B
%B
1B
+B
1B
DB
JB
PB
VB
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
2B
!|22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	��B	�{B	�oB	�\B	�\B	�\B	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�7B	z�B	o�B	o�B	u�B	y�B	|�B	x�B	{�B	��B	�wB	��B	�/B
�B
R�B
dZB
k�B
n�B
r�B
x�B
�B
��B
��B
�B
��B
v�B
�%B
��B
��BJB �B,BF�B�7B��B�XBȴB��B�/B�B�B33B8RB;dB7LBC�BJ�B=qB+B"�B�BVB��B�BB�ZB�mBǮB�XB�B��B��B�-B��Bp�B`BBR�B?}B5?B.B'�B#�B>wBS�Bl�Bx�B~�BcTB\)BW
BJ�B?}B"�BB
�B
�TB
��B
�LB
�{B
�%B
r�B
e`B
7LB
oB	�B	�B	�LB	��B	��B	�oB	�JB	�B	o�B	bNB	^5B	T�B	E�B	-B	�B	DB��B�`B�HB�BB�5B�/B�B�B�B�/B�`B�fB�fB�sB�B�B�B�B�B�B�sB�TB�B��B��B��B��BǮBB��B�jB�RB�LB�LB�qBɺB��B��B�)B�NB�ZB�ZB�mB�sB�yB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�fB�HB�BB�BB�)B�
B�B��B��B��B��B��B��B��B��B��B��BɺBŢB�}B�jB�dB�^B�^B�RB�9B�-B�'B�!B�!B�B�B�B�!B�!B�B�B�'B�-B�3B�9B�?B�XB�XB�dB��BĜBǮBɺB��B��B��B��B��B��B��B�B�#B�)B�#B�)B�HB�`B�B�B�B��B��B��B	B	B	%B	+B	1B	1B	+B	+B	
=B	JB	\B	hB	�B	�B	�B	�B	�B	 �B	)�B	33B	7LB	7LB	7LB	9XB	9XB	=qB	?}B	?}B	?}B	@�B	@�B	A�B	C�B	G�B	H�B	J�B	M�B	O�B	O�B	S�B	T�B	W
B	W
B	ZB	]/B	^5B	dZB	ffB	gmB	iyB	k�B	m�B	n�B	o�B	q�B	u�B	w�B	y�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�=B	�VB	�\B	�\B	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�FB	�^B	�qB	�qB	�wB	�wB	��B	ÖB	B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B	��B	��B	��B	��B
  B
B
B
B
%B
%B
%B
%B
%B
1B
+B
1B
DB
JB
PB
VB
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
2B
!|22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190509                              AO  ARCAADJP                                                                    20181005190509    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190509  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190509  QCF$                G�O�G�O�G�O�8000            