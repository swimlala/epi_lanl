CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:10Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191710  20181005191710  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               cA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$����1   @��%1M��@4�\(��dC�l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      cA   A   A   @333@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(��B0ffB7��B?��BH  BP��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B���B�  B���B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC/�fC2  C4  C6  C8  C:�C<  C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX�CZ  C\  C^�C`  Ca�fCd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCu�fCw�fCy�fC{�fC}�fC�  C�  C�  C�  C��3C��C�  C��3C�  C��C��3C��3C�  C��3C�  C�  C�  C��C��C��C��C��3C��C��C��C��C��C��C��C��C�  C�  C�  C�  C��3C��C��C�  C��C��C��3C�  C�  C��C��3C��3C�  C��C��C��C�  C��C��C�  C��C��C�  C��C��3C��3C��3C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C��fC�  C��C��C�  C�  C��3C�  C�  C��fC��fC�  C�  C��C�  C��3C�  C��C��C�  C��C��3C��3C�  C��3C��C��C�  C�  C��3C�  C��3C��3C��3C��3C�  C�  C��3C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��C�  C��3C�  C��C��C��D fD � DfD� D��D�fD  Dy�DfD�fDfD�fDfD� D  D�fD  D� D	  D	� D
  D
y�D  D�fD  D� D��D� DfD� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D  D�fD  D� DfD� D  D� D��Dy�D��D � D!  D!y�D!��D"y�D#  D#�fD$fD$� D%  D%�fD&fD&� D'  D'� D(  D(y�D(��D)� D*  D*� D+  D+y�D+��D,� D-fD-� D-��D.y�D/  D/� D0  D0�fD1  D1y�D2  D2y�D3  D3� D4  D4� D5  D5�fD6fD6�fD7  D7�fD8  D8� D9  D9� D:  D:�fD;fD;� D<  D<�fD=fD=� D>  D>� D?  D?� D@fD@� D@��DA� DB  DB� DC  DC� DDfDD� DD��DE� DFfDF�fDGfDG�fDHfDHy�DH��DI� DJfDJ�fDK  DK� DLfDL�fDM  DM� DNfDN�fDO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV�fDW  DW� DW��DX� DY  DYy�DZ  DZ� DZ��D[� D\fD\� D\��D]� D^  D^� D_fD_� D_��D`y�Da  Da� Db  Db� DcfDc� Dc��Dd� DefDe� De��Df� Dg  Dg� DhfDh�fDifDi� DjfDj�fDk  Dk� Dl  Dl�fDm  Dm� DnfDn� Do  Do�fDp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dty�Dt��Du� Dv  Dvy�DwfDw� Dw� Dy�D�8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @7�@�=q@�
>A�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B){B0�B7�HB?�HBHG�BQ{BW�HB`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B��B�#�B�#�B�#�B�#�B��B�#�B�#�B��B�#�B��B�#�B�#�C �C�C�C�RC�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C-�RC/�RC2�C4�C6�C8�C:+�C<�C=�RC?�RCB�CD�CF�CH�CJ�CL�CN�CP�CR+�CT�CV�CX+�CZ�C\�C^+�C`�Ca�RCd�Cf+�Ch�Cj�Cl�Cn�Cp�Cr�Cs�RCu�RCw�RCy�RC{�RC}�RC��C��C��C��C��)C��C��C��)C��C��C��)C��)C��C��)C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C�"�C��C��C��C��)C��C��C��C��)C��)C��C��C��C��C��C��C��C��C��C��C��C��C��)C��)C��)C��)C��C��C��C��)C��)C��)C��)C��C��C��\C��C��C��C��C��C��)C��C��C��\C��\C��C��C��C��C��)C��C��C��C��C��C��)C��)C��C��)C��C��C��C��C��)C��C��)C��)C��)C��)C��C��C��)C��C��C��C��C��C��)C��)C��C��C��C��C��C��C��C��)C��C��C��C��D 
�D �{D
�D�{D�D��D{D~D
�D��D
�D��D
�D�{D{D��D{D�{D	{D	�{D
{D
~D{D��D{D�{D�D�{D
�D�{D
�D��D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D~D�D~D{D��D{D�{D
�D�{D{D�{D�D~D�D �{D!{D!~D!�D"~D#{D#��D$
�D$�{D%{D%��D&
�D&�{D'{D'�{D({D(~D(�D)�{D*{D*�{D+{D+~D+�D,�{D-
�D-�{D-�D.~D/{D/�{D0{D0��D1{D1~D2{D2~D3{D3�{D4{D4�{D5{D5��D6
�D6��D7{D7��D8{D8�{D9{D9�{D:{D:��D;
�D;�{D<{D<��D=
�D=�{D>{D>�{D?{D?�{D@
�D@�{D@�DA�{DB{DB�{DC{DC�{DD
�DD�{DD�DE�{DF
�DF��DG
�DG��DH
�DH~DH�DI�{DJ
�DJ��DK{DK�{DL
�DL��DM{DM�{DN
�DN��DO{DO�{DP{DP��DQ{DQ�{DR{DR�{DS{DS�{DT
�DT�{DU{DU�{DV{DV��DW{DW�{DW�DX�{DY{DY~DZ{DZ�{DZ�D[�{D\
�D\�{D\�D]�{D^{D^�{D_
�D_�{D_�D`~Da{Da�{Db{Db�{Dc
�Dc�{Dc�Dd�{De
�De�{De�Df�{Dg{Dg�{Dh
�Dh��Di
�Di�{Dj
�Dj��Dk{Dk�{Dl{Dl��Dm{Dm�{Dn
�Dn�{Do{Do��Dp{Dp�{Dq{Dq��Dr{Dr�{Ds{Ds�{Dt{Dt~Dt�Du�{Dv{Dv~Dw
�Dw�{Dw�{Dy��D�:�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A��A��A� �A� �A��A��A� �A� �A�"�A�"�A�$�A�-A�-A�/A�$�AۋDA�K�Aͥ�A�`BA�hsAɋDA��/A�z�A���A�ƨA�ȴA�33A�;dA���A���A�A���A��wA���A�A���A�A�A��RA��A��A�I�A�|�A�bNA��A�9XA���A��A�G�A�oA��+A��DA� �A�{A�S�A�
=A�5?A�bNA���A���A�A���A���A�jA�;dA���A�v�A��A�=qA��A�ffA�A���A��DA��A�
=A�t�A��A�A�-A�JA���A��jA���A�\)A��#A�XA�I�A�  A}��A|�HA|��A{�wAz�DAy��Ay�Aw��AuO�As�Ar�`Ar1'AqAm��AjjAg\)Ad�yAd �Ac7LAbZA`��A]�AZ^5AV�AT�`AS�;AR�!AO��AL��AKoAJ��AJZAH��AE�AB �A>��A<VA;�A:I�A7��A5�A5G�A2�A/��A.(�A-�A-��A-�^A-��A-oA+��A(�uA't�A&ȴA&r�A%��A$��A#��A!��A��A�DAx�A&�A�AjA��A;dA�9A�
A�yA�;AoA$�A�/A�#A&�A�uAM�A��A�AjA\)A��Az�A�hA
v�A	t�A��A�FA�A�RA�jA��A�RA�RA5?A�A�wA��A`BA ��A �A �@���@��@�J@�X@���@�t�@�ff@�J@��#@�@��9@���@�"�@���@���@�/@�r�@�ff@�h@�?}@� �@�K�@�ff@�J@��#@���@�K�@�M�@���@��
@��@�~�@���@��@��@� �@�S�@�$�@�j@�l�@��@�bN@��H@�ff@թ�@�/@Ԭ@� �@җ�@��@ёh@��@ЋD@�~�@͉7@�?}@�v�@�Q�@ȴ9@ə�@��@���@�O�@ȴ9@�bN@�\)@�ȴ@�^5@�J@ċD@��@�ff@�bN@��@�^5@��#@��#@��^@�`B@��@��@��T@���@�K�@���@�ȴ@��@�  @�
=@��+@�V@�5?@�v�@�=q@��j@��@���@�K�@�G�@��9@�{@��@��@��@�{@�J@��@��@���@�o@�"�@�33@���@�n�@���@��`@��D@�1@�dZ@�o@�dZ@�%@��@���@�;d@���@���@���@���@�K�@���@��h@�@�v�@��+@�@��^@�x�@��u@��9@�%@�?}@�`B@�p�@��9@��u@��@�Z@�1'@��u@�x�@���@�=q@�`B@�K�@�"�@�=q@�X@�M�@�ff@�ff@�$�@��-@�7L@�7L@�/@�/@�7L@��/@�j@�A�@��@��F@��F@�t�@�;d@�K�@�33@�o@���@�ȴ@���@��+@�ff@�=q@�$�@���@�&�@��/@���@�Ĝ@��u@�I�@�1@�1@�  @��
@�K�@�+@�o@��y@���@�~�@�=q@�J@��@�@���@�p�@�O�@�V@�Ĝ@���@��D@�j@�Z@�bN@��@�S�@�
=@��!@���@�E�@��#@�@�x�@�/@��`@�Q�@��@�t�@�o@��@��\@�{@��`@�b@��;@���@�|�@�\)@���@���@���@���@���@���@�~�@�$�@�J@��^@�@��^@��-@��h@�?}@��@�7L@�?}@�1'@�ƨ@��w@��@�"�@���@���@�$�@��#@�&�@�/@�%@��j@���@�Z@�1@�1@� �@�9X@�b@��@�{@��T@���@�hs@�O�@��@��@�9X@��@~ff@~@}�-@}�@|��@|1@{��@{��@{t�@yhs@e�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A��A��A��A� �A� �A��A��A� �A� �A�"�A�"�A�$�A�-A�-A�/A�$�AۋDA�K�Aͥ�A�`BA�hsAɋDA��/A�z�A���A�ƨA�ȴA�33A�;dA���A���A�A���A��wA���A�A���A�A�A��RA��A��A�I�A�|�A�bNA��A�9XA���A��A�G�A�oA��+A��DA� �A�{A�S�A�
=A�5?A�bNA���A���A�A���A���A�jA�;dA���A�v�A��A�=qA��A�ffA�A���A��DA��A�
=A�t�A��A�A�-A�JA���A��jA���A�\)A��#A�XA�I�A�  A}��A|�HA|��A{�wAz�DAy��Ay�Aw��AuO�As�Ar�`Ar1'AqAm��AjjAg\)Ad�yAd �Ac7LAbZA`��A]�AZ^5AV�AT�`AS�;AR�!AO��AL��AKoAJ��AJZAH��AE�AB �A>��A<VA;�A:I�A7��A5�A5G�A2�A/��A.(�A-�A-��A-�^A-��A-oA+��A(�uA't�A&ȴA&r�A%��A$��A#��A!��A��A�DAx�A&�A�AjA��A;dA�9A�
A�yA�;AoA$�A�/A�#A&�A�uAM�A��A�AjA\)A��Az�A�hA
v�A	t�A��A�FA�A�RA�jA��A�RA�RA5?A�A�wA��A`BA ��A �A �@���@��@�J@�X@���@�t�@�ff@�J@��#@�@��9@���@�"�@���@���@�/@�r�@�ff@�h@�?}@� �@�K�@�ff@�J@��#@���@�K�@�M�@���@��
@��@�~�@���@��@��@� �@�S�@�$�@�j@�l�@��@�bN@��H@�ff@թ�@�/@Ԭ@� �@җ�@��@ёh@��@ЋD@�~�@͉7@�?}@�v�@�Q�@ȴ9@ə�@��@���@�O�@ȴ9@�bN@�\)@�ȴ@�^5@�J@ċD@��@�ff@�bN@��@�^5@��#@��#@��^@�`B@��@��@��T@���@�K�@���@�ȴ@��@�  @�
=@��+@�V@�5?@�v�@�=q@��j@��@���@�K�@�G�@��9@�{@��@��@��@�{@�J@��@��@���@�o@�"�@�33@���@�n�@���@��`@��D@�1@�dZ@�o@�dZ@�%@��@���@�;d@���@���@���@���@�K�@���@��h@�@�v�@��+@�@��^@�x�@��u@��9@�%@�?}@�`B@�p�@��9@��u@��@�Z@�1'@��u@�x�@���@�=q@�`B@�K�@�"�@�=q@�X@�M�@�ff@�ff@�$�@��-@�7L@�7L@�/@�/@�7L@��/@�j@�A�@��@��F@��F@�t�@�;d@�K�@�33@�o@���@�ȴ@���@��+@�ff@�=q@�$�@���@�&�@��/@���@�Ĝ@��u@�I�@�1@�1@�  @��
@�K�@�+@�o@��y@���@�~�@�=q@�J@��@�@���@�p�@�O�@�V@�Ĝ@���@��D@�j@�Z@�bN@��@�S�@�
=@��!@���@�E�@��#@�@�x�@�/@��`@�Q�@��@�t�@�o@��@��\@�{@��`@�b@��;@���@�|�@�\)@���@���@���@���@���@���@�~�@�$�@�J@��^@�@��^@��-@��h@�?}@��@�7L@�?}@�1'@�ƨ@��w@��@�"�@���@���@�$�@��#@�&�@�/@�%@��j@���@�Z@�1@�1@� �@�9X@�b@��@�{@��T@���@�hs@�O�@��@��@�9X@��@~ff@~@}�-@}�@|��@|1@{��@{��@{t�@yhs@e�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BN�BN�BN�BN�BN�BN�BN�BN�BO�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BO�BO�BN�BC�B	7B��B
=B�B"�B&�B49BB�BL�BbNBr�B{�B~�B�+B�DB��B��B��B��B��B��B�B��B��B��B��B�hB�B�B�%By�Bn�BhsB_;BL�BI�BH�BA�B2-B)�B!�B�BoBPBB��B��B�B�B�B��B�B��B�-B��B�bB}�Bk�BYB�B
��B
�dB
�!B
��B
�B
m�B
iyB
e`B
^5B
YB
XB
YB
O�B
H�B
E�B
>wB
49B
-B
$�B
�B
+B	��B	�B	�B	�BB	��B	�-B	��B	�PB	�+B	�B	z�B	n�B	^5B	K�B	8RB	.B	&�B	�B	oB	%B��B��B��B�B�BB��BƨB�wB�^B�LB�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�JB�1B�B�B�B�+B�+B�1B�1B�PB�PB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�PB�VB�oB�oB�hB�bB�oB�oB�oB�oB�\B�\B�\B�\B�VB�bB�hB�hB�oB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�9B�FB�LB�RB�^B�qB�qB�jB�qB�jB�}B��B�}B�jB�?B�XB��BȴBȴBɺB��B��B��B��B��B��B��B��BɺBÖB�qB�}B��B��B��B��B��B�qBŢBȴB�B��B��BǮBŢBĜBǮB��B��B��B��B�sB��B��B	B	B	+B	�B	!�B	!�B	&�B	0!B	33B	49B	0!B	2-B	1'B	&�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	.B	1'B	8RB	:^B	?}B	@�B	C�B	E�B	E�B	D�B	B�B	J�B	P�B	S�B	R�B	Q�B	P�B	R�B	W
B	YB	ZB	[#B	`BB	_;B	^5B	`BB	cTB	e`B	hsB	r�B	y�B	}�B	}�B	{�B	� B	� B	~�B	�%B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�FB	�FB	�LB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	��B	B	B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�HB	�HB	�TB	�NB	�HB	�HB	�HB	�HB	�TB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�yB	�yB	�yB	�yB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
B
�B
?222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BN�BN�BN�BN�BN�BN�BN�BN�BO�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BO�BO�BN�BC�B	7B��B
=B�B"�B&�B49BB�BL�BbNBr�B{�B~�B�+B�DB��B��B��B��B��B��B�B��B��B��B��B�hB�B�B�%By�Bn�BhsB_;BL�BI�BH�BA�B2-B)�B!�B�BoBPBB��B��B�B�B�B��B�B��B�-B��B�bB}�Bk�BYB�B
��B
�dB
�!B
��B
�B
m�B
iyB
e`B
^5B
YB
XB
YB
O�B
H�B
E�B
>wB
49B
-B
$�B
�B
+B	��B	�B	�B	�BB	��B	�-B	��B	�PB	�+B	�B	z�B	n�B	^5B	K�B	8RB	.B	&�B	�B	oB	%B��B��B��B�B�BB��BƨB�wB�^B�LB�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�JB�1B�B�B�B�+B�+B�1B�1B�PB�PB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�PB�VB�oB�oB�hB�bB�oB�oB�oB�oB�\B�\B�\B�\B�VB�bB�hB�hB�oB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�9B�FB�LB�RB�^B�qB�qB�jB�qB�jB�}B��B�}B�jB�?B�XB��BȴBȴBɺB��B��B��B��B��B��B��B��BɺBÖB�qB�}B��B��B��B��B��B�qBŢBȴB�B��B��BǮBŢBĜBǮB��B��B��B��B�sB��B��B	B	B	+B	�B	!�B	!�B	&�B	0!B	33B	49B	0!B	2-B	1'B	&�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	.B	1'B	8RB	:^B	?}B	@�B	C�B	E�B	E�B	D�B	B�B	J�B	P�B	S�B	R�B	Q�B	P�B	R�B	W
B	YB	ZB	[#B	`BB	_;B	^5B	`BB	cTB	e`B	hsB	r�B	y�B	}�B	}�B	{�B	� B	� B	~�B	�%B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�FB	�FB	�LB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	��B	B	B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�HB	�HB	�TB	�NB	�HB	�HB	�HB	�HB	�TB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�yB	�yB	�yB	�yB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
B
�B
?222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191710                              AO  ARCAADJP                                                                    20181005191710    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191710  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191710  QCF$                G�O�G�O�G�O�8000            