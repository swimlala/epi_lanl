CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:07:45Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181024140745  20181024140745  5904955 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6558                            2B  A   APEX                            7469                            062512                          846 @��#Ey�1   @�ᥲ@�,@4�1&�x��c����F1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy��D�3�D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�W
B��B��B��B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dw�{Dy�)D�5�D��z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�XA�dZA�ffA�hsA�jA�jA�jA�l�A�n�A�p�A�n�A�p�A�p�A�n�A�n�A�n�A�l�A�l�A�l�A�l�A�n�A�n�A�p�A�n�A�p�A�p�A�n�A�t�A�x�A�r�A�t�A�l�A��Aˉ7A�;dA��Aʡ�A�x�A�33A���AɾwA�l�A�ZA��/A�&�Aĝ�A�l�A�JA�E�A�E�A��9A�v�A�I�A���A���A�G�A���A�C�A�&�A�O�A��jA�
=A���A�n�A��A���A���A�x�A��#A��A��A��mA��;A���A��/A��-A��-A�JA�1A�~�A��`A��A�A�A��uA�JA� �A���A��A��^A���A�Q�A�ƨA��A�bA}VAx�At5?At  As7LAn�AljAj��AhȴAedZAd{AbȴAa��A`�HA`$�A_7LA^ffA\��A[p�AYdZAXv�AT�HAP5?ANZALjAK%AI�;AH�+AG��AG��AG;dAD�RADz�AD�ABn�A@�\A>�A=�wA=\)A;O�A9�hA7�;A6n�A5�#A5��A5x�A4�A3+A2A�A0�/A/�FA.�A-�PA,v�A(��A&�`A%��A%�^A%G�A$�uA#+A"�HA!�hA Q�A?}A��A|�A|�AE�A9XAS�An�AVA��A�DA�7A�yA�-A�AhsA
{A�yA�\Av�A�HAM�A�PA�A9XAC�Av�AQ�AJA�TA�TA�
A\)A �\A 9X@���@�1@��@��y@�ff@�r�@�  @���@�K�@�+@�+@�
=@���@��@�1@�=q@���@߾w@��@�V@� �@ۥ�@�E�@ؓu@�b@��m@�@ղ-@�9X@���@Ӆ@���@��@�O�@��`@ύP@·+@�@�@̼j@�b@ˮ@��@�ff@�@�`B@�O�@��`@�A�@��;@ǥ�@�ȴ@�@Ų-@�`B@ģ�@öF@�"�@�@�ff@�@��D@�t�@��@�~�@���@���@��9@�Ĝ@�Ĝ@�r�@��w@�|�@�"�@��@���@�M�@�=q@��@���@�p�@��/@�Z@���@���@�+@��@��\@��@�V@��u@�r�@�b@��@�l�@�\)@�
=@�^5@�p�@�X@�/@��/@���@�t�@��P@���@�33@�v�@�$�@�@��@��^@�/@���@���@���@���@���@�Q�@�1@���@��@���@�ȴ@�n�@�=q@��T@�hs@�/@�Ĝ@��@�b@��
@�K�@��H@�ȴ@���@��+@�^5@�@��T@�@��7@���@��@��\@��@��T@���@���@���@�ƨ@��@���@�S�@�S�@�dZ@�33@��@��\@�v�@��@�x�@�X@�7L@��@�%@�Ĝ@���@���@���@��@�|�@��y@�@�%@�z�@���@���@��@�A�@�  @��@�
=@��\@�^5@��@��-@�`B@�%@���@�1'@�(�@�(�@��@��@��w@�o@��@��H@��H@��H@�ȴ@���@���@�-@���@�&�@���@��u@�A�@��@��F@�\)@�K�@�33@�"�@�~�@�-@��@���@��D@�I�@�  @��
@���@��@�K�@�"�@��y@�M�@�-@�$�@�J@��#@���@�X@���@�Ĝ@�z�@�j@�Q�@�  @��m@��
@��w@���@���@���@���@��@�\)@�;d@�"�@�@��6@{�A@i�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�XA�dZA�ffA�hsA�jA�jA�jA�l�A�n�A�p�A�n�A�p�A�p�A�n�A�n�A�n�A�l�A�l�A�l�A�l�A�n�A�n�A�p�A�n�A�p�A�p�A�n�A�t�A�x�A�r�A�t�A�l�A��Aˉ7A�;dA��Aʡ�A�x�A�33A���AɾwA�l�A�ZA��/A�&�Aĝ�A�l�A�JA�E�A�E�A��9A�v�A�I�A���A���A�G�A���A�C�A�&�A�O�A��jA�
=A���A�n�A��A���A���A�x�A��#A��A��A��mA��;A���A��/A��-A��-A�JA�1A�~�A��`A��A�A�A��uA�JA� �A���A��A��^A���A�Q�A�ƨA��A�bA}VAx�At5?At  As7LAn�AljAj��AhȴAedZAd{AbȴAa��A`�HA`$�A_7LA^ffA\��A[p�AYdZAXv�AT�HAP5?ANZALjAK%AI�;AH�+AG��AG��AG;dAD�RADz�AD�ABn�A@�\A>�A=�wA=\)A;O�A9�hA7�;A6n�A5�#A5��A5x�A4�A3+A2A�A0�/A/�FA.�A-�PA,v�A(��A&�`A%��A%�^A%G�A$�uA#+A"�HA!�hA Q�A?}A��A|�A|�AE�A9XAS�An�AVA��A�DA�7A�yA�-A�AhsA
{A�yA�\Av�A�HAM�A�PA�A9XAC�Av�AQ�AJA�TA�TA�
A\)A �\A 9X@���@�1@��@��y@�ff@�r�@�  @���@�K�@�+@�+@�
=@���@��@�1@�=q@���@߾w@��@�V@� �@ۥ�@�E�@ؓu@�b@��m@�@ղ-@�9X@���@Ӆ@���@��@�O�@��`@ύP@·+@�@�@̼j@�b@ˮ@��@�ff@�@�`B@�O�@��`@�A�@��;@ǥ�@�ȴ@�@Ų-@�`B@ģ�@öF@�"�@�@�ff@�@��D@�t�@��@�~�@���@���@��9@�Ĝ@�Ĝ@�r�@��w@�|�@�"�@��@���@�M�@�=q@��@���@�p�@��/@�Z@���@���@�+@��@��\@��@�V@��u@�r�@�b@��@�l�@�\)@�
=@�^5@�p�@�X@�/@��/@���@�t�@��P@���@�33@�v�@�$�@�@��@��^@�/@���@���@���@���@���@�Q�@�1@���@��@���@�ȴ@�n�@�=q@��T@�hs@�/@�Ĝ@��@�b@��
@�K�@��H@�ȴ@���@��+@�^5@�@��T@�@��7@���@��@��\@��@��T@���@���@���@�ƨ@��@���@�S�@�S�@�dZ@�33@��@��\@�v�@��@�x�@�X@�7L@��@�%@�Ĝ@���@���@���@��@�|�@��y@�@�%@�z�@���@���@��@�A�@�  @��@�
=@��\@�^5@��@��-@�`B@�%@���@�1'@�(�@�(�@��@��@��w@�o@��@��H@��H@��H@�ȴ@���@���@�-@���@�&�@���@��u@�A�@��@��F@�\)@�K�@�33@�"�@�~�@�-@��@���@��D@�I�@�  @��
@���@��@�K�@�"�@��y@�M�@�-@�$�@�J@��#@���@�X@���@�Ĝ@�z�@�j@�Q�@�  @��m@��
@��w@���@���@���@���@��@�\)@�;d@�"�@�@��6@{�A@i�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bo�Bo�Bp�Bp�Bq�Bp�Bp�Br�Bp�Bq�Bq�Bp�Bq�Bo�Bp�Bo�Bn�B��BƨB�
B�HB�B��B��B1BbB�B%�BH�B|�B�B� By�Bq�BffBXBI�BO�BR�B]/BaHB_;B]/BXBM�BA�B7LB9XB>wB<jB2-B&�B1'B�BoBB�B��B�}B��B��B�DB|�Bs�B]/BI�BA�B;dB6FB�B
��B
�3B
�PB
�B
}�B
z�B
u�B
l�B
`BB
N�B
,B
\B	�B	�B	�sB	��B	�}B	�3B	��B	�hB	�+B	� B	w�B	s�B	n�B	hsB	bNB	YB	O�B	E�B	;dB	&�B	oB	
=B	B��B��B�B�B�B�B�TB�HB�;B�B��B��BɺBƨB��B�jB�XB�LB�FB�?B�9B�'B�B�B��B��B��B��B��B��B�bB�PB�JB�=B�1B�B�B�B~�B|�Bz�Bx�Bu�Bs�Br�Bo�Bl�BhsBcTBdZBdZBaHBaHBgmBe`BhsBhsBl�Bp�Bx�B|�B�B�1B�DB�DB�DB�JB�PB�JB�JB�DB�7B�1B�1B�DB�PB�\B�hB�oB��B��B��B��B��B��B��B�{B�{B��B��Bw�B��B��B��B�B�B�'B�RB�^B�dB�}BŢBȴBǮBǮBɺB��B��B��B�
B�)B�)B�)B�5B�NB�TB�ZB�`B�fB�mB�mB�yB�B�B�B��B��B��B��B��B	  B	B	B	B	B		7B	JB	JB	VB	oB	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	(�B	,B	.B	/B	/B	2-B	5?B	6FB	8RB	:^B	;dB	=qB	@�B	D�B	F�B	F�B	H�B	I�B	L�B	M�B	O�B	P�B	T�B	T�B	VB	W
B	YB	[#B	]/B	^5B	^5B	^5B	e`B	gmB	hsB	iyB	k�B	l�B	m�B	m�B	l�B	m�B	o�B	p�B	p�B	p�B	q�B	t�B	t�B	u�B	v�B	w�B	w�B	x�B	x�B	z�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�PB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�LB	�qB	�}B	��B	��B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�HB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B<jB
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
JB
~B
1B
&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bo�Bo�Bp�Bp�Bq�Bp�Bp�Br�Bp�Bq�Bq�Bp�Bq�Bo�Bp�Bo�Bn�B��BƨB�
B�HB�B��B��B1BbB�B%�BH�B|�B�B� By�Bq�BffBXBI�BO�BR�B]/BaHB_;B]/BXBM�BA�B7LB9XB>wB<jB2-B&�B1'B�BoBB�B��B�}B��B��B�DB|�Bs�B]/BI�BA�B;dB6FB�B
��B
�3B
�PB
�B
}�B
z�B
u�B
l�B
`BB
N�B
,B
\B	�B	�B	�sB	��B	�}B	�3B	��B	�hB	�+B	� B	w�B	s�B	n�B	hsB	bNB	YB	O�B	E�B	;dB	&�B	oB	
=B	B��B��B�B�B�B�B�TB�HB�;B�B��B��BɺBƨB��B�jB�XB�LB�FB�?B�9B�'B�B�B��B��B��B��B��B��B�bB�PB�JB�=B�1B�B�B�B~�B|�Bz�Bx�Bu�Bs�Br�Bo�Bl�BhsBcTBdZBdZBaHBaHBgmBe`BhsBhsBl�Bp�Bx�B|�B�B�1B�DB�DB�DB�JB�PB�JB�JB�DB�7B�1B�1B�DB�PB�\B�hB�oB��B��B��B��B��B��B��B�{B�{B��B��Bw�B��B��B��B�B�B�'B�RB�^B�dB�}BŢBȴBǮBǮBɺB��B��B��B�
B�)B�)B�)B�5B�NB�TB�ZB�`B�fB�mB�mB�yB�B�B�B��B��B��B��B��B	  B	B	B	B	B		7B	JB	JB	VB	oB	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	(�B	,B	.B	/B	/B	2-B	5?B	6FB	8RB	:^B	;dB	=qB	@�B	D�B	F�B	F�B	H�B	I�B	L�B	M�B	O�B	P�B	T�B	T�B	VB	W
B	YB	[#B	]/B	^5B	^5B	^5B	e`B	gmB	hsB	iyB	k�B	l�B	m�B	m�B	l�B	m�B	o�B	p�B	p�B	p�B	q�B	t�B	t�B	u�B	v�B	w�B	w�B	x�B	x�B	z�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�PB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�LB	�qB	�}B	��B	��B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�HB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B<jB
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
JB
~B
1B
&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140745                              AO  ARCAADJP                                                                    20181024140745    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140745  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140745  QCF$                G�O�G�O�G�O�0               