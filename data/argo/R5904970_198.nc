CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:39Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024141539  20181024141539  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��~�f1   @���mP@7�/��w�c�"��`B1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5�fD6fD6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dw� Dy�)D�4�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Dz�@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C+�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^+�C`�Ca�RCd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv+�Cx+�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D��D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2
�D2�{D3{D3�{D4{D4�{D5{D5��D6
�D6��D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>
�D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN~DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp
�Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv��Dw{Dw�{Dw�{Dy��D�7
D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��HA��A��A��yA��A��A��A��A��A��A��A��A���A�A�A�%A�A�A�A�A�A�A�1A�1A�VA�VA�JA�bA��A�oA�A���A��A�K�A�M�A�Q�A��A�
=A��A�O�A���A�v�A���A��A�1'A�C�A���A���A���A���A��jA�l�A�ȴA��A�;dA��\A��7A��`A���A��FA��#A���A���A�ȴA�E�A���A�v�A���A�(�A�%A�C�A��A��A�~�A��A��`A���A�5?A�S�A���A�VA���A�9XA��RA�VA���A�5?A���A��A���A�Q�A��A� �A��jA�Q�A��A���A��A���A�G�A�n�A�JA��A�S�A�(�A��Ap�A}�A}7LA{�Az�HAz��Ay�Au�As+Ar��AqO�ApZAo�-AohsAoXAo�AnjAl�HAl �Ajz�AgdZAf��AfJAe\)Ad��AdM�Ac�Ab�Ab�jAa�-A`5?A_"�A^��A^��A]�TA[�
A[oAY��AX��AX�AW��AVĜAU��ATQ�AS��AP��AOdZAO7LAN��AL�/AJ~�AI|�AH5?AGƨAG\)AF�ADffAB�jAB��AA��AA7LA@��A?�^A>�\A>{A=��A<�A<n�A;&�A8�A6�A5;dA4��A4 �A3�wA3x�A1��A0�A/�A/S�A-��A,  A+C�A+%A*ĜA*ffA)�TA)7LA)�A(��A(^5A'&�A&5?A%�A$�!A#�^A#S�A#+A"��A"1'A!��A!�#A!�wA!p�A��A(�A�7A^5A�A��A�wA��AO�AVA�hA��A �AG�A�AoA�^A&�A��A�A	�A	/A�`A�HAȴA��A�\A�^A��A�9A-A�TAM�A��A ��@��@�x�@���@�9X@�(�@���@�J@��9@�S�@�1'@�o@���@�?}@���@�%@�"�@陚@�A�@�
=@���@��@�x�@�P@��@�-@ᙚ@�V@ߍP@݁@�o@�`B@أ�@�+@֗�@�V@��@�`B@���@ԛ�@�V@ԓu@���@��@�{@���@���@��y@�I�@ʧ�@ɑh@�E�@���@å�@�(�@ǶF@�b@��H@Ɨ�@ź^@���@�@���@�%@���@��R@���@�$�@�X@�%@�dZ@��H@���@�V@��m@�+@��@���@�7L@���@��R@�V@��@�hs@�Ĝ@�z�@��
@���@��#@��#@��@��@��@��7@�p�@���@�b@��\@�$�@��@���@��j@��j@��D@�(�@��;@�dZ@�33@��@�V@���@�%@��m@�|�@�ƨ@��m@�ƨ@��@�t�@��@���@���@���@�x�@�G�@��@�/@��@��-@��@�%@��/@��T@���@���@��H@��@���@�;d@��H@�-@�+@�(�@�I�@�ff@�z�@��;@�;d@�o@��@�E�@�bN@��@�K�@��@�V@�-@�$�@��T@�p�@���@��9@��D@��D@�I�@���@���@���@��@�l�@�S�@�
=@���@��@�
=@��!@�~�@�V@�n�@�~�@�n�@�ff@�V@�5?@��^@�p�@�7L@��@���@���@���@��@�l�@�33@�@�o@��;@��@���@�|�@�"�@���@��H@��+@���@�G�@��9@�(�@���@��;@��w@�|�@��@��!@�v�@�^5@��#@��7@�`B@��@��@�%@��@�V@���@�=q@�@�"�@��@�o@�o@�o@�@�@�@��y@��@��R@�~�@�J@��j@���@�S�@�o@�@�;d@�;d@���@�ȴ@���@�v�@�}�@uY�@f�<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��HA��A��A��yA��A��A��A��A��A��A��A��A���A�A�A�%A�A�A�A�A�A�A�1A�1A�VA�VA�JA�bA��A�oA�A���A��A�K�A�M�A�Q�A��A�
=A��A�O�A���A�v�A���A��A�1'A�C�A���A���A���A���A��jA�l�A�ȴA��A�;dA��\A��7A��`A���A��FA��#A���A���A�ȴA�E�A���A�v�A���A�(�A�%A�C�A��A��A�~�A��A��`A���A�5?A�S�A���A�VA���A�9XA��RA�VA���A�5?A���A��A���A�Q�A��A� �A��jA�Q�A��A���A��A���A�G�A�n�A�JA��A�S�A�(�A��Ap�A}�A}7LA{�Az�HAz��Ay�Au�As+Ar��AqO�ApZAo�-AohsAoXAo�AnjAl�HAl �Ajz�AgdZAf��AfJAe\)Ad��AdM�Ac�Ab�Ab�jAa�-A`5?A_"�A^��A^��A]�TA[�
A[oAY��AX��AX�AW��AVĜAU��ATQ�AS��AP��AOdZAO7LAN��AL�/AJ~�AI|�AH5?AGƨAG\)AF�ADffAB�jAB��AA��AA7LA@��A?�^A>�\A>{A=��A<�A<n�A;&�A8�A6�A5;dA4��A4 �A3�wA3x�A1��A0�A/�A/S�A-��A,  A+C�A+%A*ĜA*ffA)�TA)7LA)�A(��A(^5A'&�A&5?A%�A$�!A#�^A#S�A#+A"��A"1'A!��A!�#A!�wA!p�A��A(�A�7A^5A�A��A�wA��AO�AVA�hA��A �AG�A�AoA�^A&�A��A�A	�A	/A�`A�HAȴA��A�\A�^A��A�9A-A�TAM�A��A ��@��@�x�@���@�9X@�(�@���@�J@��9@�S�@�1'@�o@���@�?}@���@�%@�"�@陚@�A�@�
=@���@��@�x�@�P@��@�-@ᙚ@�V@ߍP@݁@�o@�`B@أ�@�+@֗�@�V@��@�`B@���@ԛ�@�V@ԓu@���@��@�{@���@���@��y@�I�@ʧ�@ɑh@�E�@���@å�@�(�@ǶF@�b@��H@Ɨ�@ź^@���@�@���@�%@���@��R@���@�$�@�X@�%@�dZ@��H@���@�V@��m@�+@��@���@�7L@���@��R@�V@��@�hs@�Ĝ@�z�@��
@���@��#@��#@��@��@��@��7@�p�@���@�b@��\@�$�@��@���@��j@��j@��D@�(�@��;@�dZ@�33@��@�V@���@�%@��m@�|�@�ƨ@��m@�ƨ@��@�t�@��@���@���@���@�x�@�G�@��@�/@��@��-@��@�%@��/@��T@���@���@��H@��@���@�;d@��H@�-@�+@�(�@�I�@�ff@�z�@��;@�;d@�o@��@�E�@�bN@��@�K�@��@�V@�-@�$�@��T@�p�@���@��9@��D@��D@�I�@���@���@���@��@�l�@�S�@�
=@���@��@�
=@��!@�~�@�V@�n�@�~�@�n�@�ff@�V@�5?@��^@�p�@�7L@��@���@���@���@��@�l�@�33@�@�o@��;@��@���@�|�@�"�@���@��H@��+@���@�G�@��9@�(�@���@��;@��w@�|�@��@��!@�v�@�^5@��#@��7@�`B@��@��@�%@��@�V@���@�=q@�@�"�@��@�o@�o@�o@�@�@�@��y@��@��R@�~�@�J@��j@���@�S�@�o@�@�;d@�;d@���@�ȴ@���@�v�@�}�@uY�@f�<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�^B�^B�XB�XB�XB�XB�^B�XB�^B�^B�^B�^B�^B�^B�dB�dB�dB�dB�dB�^B�^B�^B�^B�^B�dB�qB�qB�jB�jBBɺB��B�)B�ZB�BBJB\BbBDB��B�`B�B�B�B�NB�)B��B�B��B��B�B��B��BƨB�wB�^B��B��B�hB�+Bu�BgmB\)B[#BXBM�BF�BB�B8RB)�B1B�B�HB�)B��B��B��B�JBbNBXBP�BJ�BF�B=qB5?B'�B�B�B�BJBBB
��B
�B
�B
�yB
�ZB
�)B
�B
��B
ȴB
�9B
�B
��B
��B
��B
��B
��B
�bB
�7B
~�B
z�B
p�B
^5B
J�B
F�B
?}B
49B
0!B
-B
,B
)�B
$�B
�B
hB

=B	�B	�fB	�NB	�5B	�B	�B	��B	ɺB	ȴB	B	�RB	�!B	�B	�B	��B	��B	��B	�\B	�+B	�B	�B	{�B	r�B	iyB	dZB	^5B	R�B	Q�B	O�B	D�B	6FB	.B	&�B	#�B	 �B	�B	{B	+B	B	B��B��B��B�B�B�B�mB�TB�;B�B��BǮBŢBÖB��B�}B�jB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�uB�bB�bB�\B�PB�JB�=B�7B�%B�B�B�B�B�B~�B{�Bu�Bs�Bo�Bn�Bk�BhsBffBaHB\)BYBW
BVBR�BI�BE�BH�BK�BS�BS�BQ�BM�BE�B7LB5?B6FB5?B0!B.B,B.B.B0!B33B<jBL�BR�B<jB)�B#�B#�B#�B'�B.B,B(�B(�B(�B+B2-B7LB0!B-B1'B6FB7LB7LB49B-B)�B,B33B1'B0!B1'B7LB=qBB�BH�BQ�BH�BA�B?}BXB`BB\)BZBZBYBT�BM�BO�BT�Br�B�B�B�B�%B�B�1B�1B}�By�B�B�%B�%B�B�B� B~�B}�B�B�B�7B�bB��B��B��B��B��B��B��B��B��B�B�3B�?B�^B�}BÖBŢBɺB��B��B��B��B��B�
B�B�NB�fB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	+B	1B	VB	\B	\B	VB	{B	�B	!�B	&�B	&�B	'�B	)�B	2-B	<jB	>wB	B�B	E�B	F�B	A�B	C�B	C�B	L�B	R�B	XB	T�B	O�B	Q�B	S�B	W
B	e`B	s�B	s�B	s�B	u�B	v�B	w�B	w�B	y�B	z�B	z�B	{�B	{�B	|�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�DB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�?B	�XB	�XB	�XB	�XB	�^B	�dB	�qB	�}B	��B	�}B	�wB	��B	ÖB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�)B	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�sB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	��B	��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�^B�^B�XB�XB�XB�XB�^B�XB�^B�^B�^B�^B�^B�^B�dB�dB�dB�dB�dB�^B�^B�^B�^B�^B�dB�qB�qB�jB�jBBɺB��B�)B�ZB�BBJB\BbBDB��B�`B�B�B�B�NB�)B��B�B��B��B�B��B��BƨB�wB�^B��B��B�hB�+Bu�BgmB\)B[#BXBM�BF�BB�B8RB)�B1B�B�HB�)B��B��B��B�JBbNBXBP�BJ�BF�B=qB5?B'�B�B�B�BJBBB
��B
�B
�B
�yB
�ZB
�)B
�B
��B
ȴB
�9B
�B
��B
��B
��B
��B
��B
�bB
�7B
~�B
z�B
p�B
^5B
J�B
F�B
?}B
49B
0!B
-B
,B
)�B
$�B
�B
hB

=B	�B	�fB	�NB	�5B	�B	�B	��B	ɺB	ȴB	B	�RB	�!B	�B	�B	��B	��B	��B	�\B	�+B	�B	�B	{�B	r�B	iyB	dZB	^5B	R�B	Q�B	O�B	D�B	6FB	.B	&�B	#�B	 �B	�B	{B	+B	B	B��B��B��B�B�B�B�mB�TB�;B�B��BǮBŢBÖB��B�}B�jB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�uB�bB�bB�\B�PB�JB�=B�7B�%B�B�B�B�B�B~�B{�Bu�Bs�Bo�Bn�Bk�BhsBffBaHB\)BYBW
BVBR�BI�BE�BH�BK�BS�BS�BQ�BM�BE�B7LB5?B6FB5?B0!B.B,B.B.B0!B33B<jBL�BR�B<jB)�B#�B#�B#�B'�B.B,B(�B(�B(�B+B2-B7LB0!B-B1'B6FB7LB7LB49B-B)�B,B33B1'B0!B1'B7LB=qBB�BH�BQ�BH�BA�B?}BXB`BB\)BZBZBYBT�BM�BO�BT�Br�B�B�B�B�%B�B�1B�1B}�By�B�B�%B�%B�B�B� B~�B}�B�B�B�7B�bB��B��B��B��B��B��B��B��B��B�B�3B�?B�^B�}BÖBŢBɺB��B��B��B��B��B�
B�B�NB�fB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	+B	1B	VB	\B	\B	VB	{B	�B	!�B	&�B	&�B	'�B	)�B	2-B	<jB	>wB	B�B	E�B	F�B	A�B	C�B	C�B	L�B	R�B	XB	T�B	O�B	Q�B	S�B	W
B	e`B	s�B	s�B	s�B	u�B	v�B	w�B	w�B	y�B	z�B	z�B	{�B	{�B	|�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�DB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�?B	�XB	�XB	�XB	�XB	�^B	�dB	�qB	�}B	��B	�}B	�wB	��B	ÖB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�)B	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�sB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	��B	��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141539                              AO  ARCAADJP                                                                    20181024141539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141539  QCF$                G�O�G�O�G�O�0               