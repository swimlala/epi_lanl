CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:48Z creation      
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
resolution        =���   axis      Z        8  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  Bx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  R�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  [�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  d�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  l    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  s8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  u   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ~   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20181024141548  20181024141548  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��$�d�!1   @��%33E�@6��-V�d8���S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\y�D\��D]y�D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Dy�{D�>D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @B�]@�z�@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C
=C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D�\D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�\D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY\DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd\Dd��De�De��Dy�qD�B�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��RA��RA��FA��FA��wA�ĜA�ƨA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ĜA�A���A��^A��-A���A��hA�z�A�\)A�9XA�1A�ĜA��A�7LA�%A��A��HA�ȴA���A��hA�~�A�dZA�O�A�5?A��A��A�ƨA���A���A��A�r�A�ZA�33A��A��hA�A�jA��A�K�A��yA��A�l�A�  A�ffA��jA��
A�9XA��A�A�33A�"�A��!A��RA�  A��\A�%A�K�A��DA�x�A�=qA�$�A�A�A���A��RA�l�A��A��PA�r�A��A�7LA���A���A���A�bNA�K�A�/A�bA���A��7A���A��FA�v�A�
=A��A��hA�%A��\A�p�A��A���A��A��`A�z�A�C�A�XA�r�A���A�v�A�ƨA���A�ĜA���A��/A��HA�?}A��7A��#A��A���A�-A/A|�!A{t�Az��Az�uAx�/Aw�hAu��As�#Ar^5Apr�Ao��AnȴAl�9AjĜAit�Ag��Af��Af5?Ae��Ad�yAa�mA^  A\(�AZffAU��ASdZAR��AQ\)AOANv�AL��AJ��AH�/AF�yAFz�AE��AD�AC�;AC�AC��ACl�AC�ACdZAC�AB�jAA�#AAVA@JA?�A>r�A=��A<��A;��A:Q�A9�wA8E�A6A4��A4�uA4I�A4�/A3�A2=qA1ƨA1G�A0�A/��A.��A-��A,A�A+G�A)A(z�A'XA%��A#��A"(�A!K�A �A�AA��A��A  A�A�AVA�RA�FA�A�jA��A?}A�HA^5A�
A;dA��AA/A�wAt�A�uA`BA
�/A
�DA	ƨA	+A��A�;AS�AƨA+A�/AM�AdZA��A�\AJAG�@��@��!@���@�E�@���@�z�@�(�@���@��;@��w@�K�@�b@��y@�@���@�z�@���@�t�@�C�@��H@�@���@�bN@�1@띲@���@�@�I�@�-@��@�j@�Z@��@��D@�v�@۶F@�V@��T@٩�@ؓu@ץ�@�~�@�@Ձ@��/@Ԭ@�9X@���@��/@�o@θR@Ͳ-@��`@�r�@���@��y@�5?@�x�@���@ȋD@ǝ�@�v�@ź^@�p�@�G�@��@Ĵ9@�1'@���@ÍP@�v�@���@�j@�A�@�I�@�Z@�j@��@�z�@� �@� �@��@��@�j@�S�@�V@��/@�r�@�j@�I�@��F@�dZ@��@��@�hs@�/@�&�@���@�bN@�I�@�A�@�ƨ@��@���@��@���@�1'@��m@�|�@�o@��R@�n�@���@�7L@��@�V@��@��@�?}@�X@�p�@�x�@�V@�9X@�  @���@�;d@�5?@�{@��#@���@�%@��j@��u@��@�Z@���@�v�@��#@���@�G�@�r�@��w@��@��@��@�33@�o@�ȴ@�^5@�5?@�=q@�E�@�J@�x�@��j@�(�@���@�@��@�
=@�v�@�M�@�{@��#@��7@�/@���@�7L@�x�@�=q@���@��R@�o@��
@��@��m@��w@��F@�;d@���@�~�@�n�@�ff@�=q@��#@�`B@��@��@��@�bN@�9X@�b@��@�|�@�\)@�\)@�\)@�\)@�;d@�+@��@�
=@�ȴ@�E�@�@���@���@��;@�|�@�-@�?}@��^@��@��\@���@x4n@jc 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��RA��RA��FA��FA��wA�ĜA�ƨA�ƨA�ȴA�ȴA�ȴA�ƨA�ƨA�ĜA�A���A��^A��-A���A��hA�z�A�\)A�9XA�1A�ĜA��A�7LA�%A��A��HA�ȴA���A��hA�~�A�dZA�O�A�5?A��A��A�ƨA���A���A��A�r�A�ZA�33A��A��hA�A�jA��A�K�A��yA��A�l�A�  A�ffA��jA��
A�9XA��A�A�33A�"�A��!A��RA�  A��\A�%A�K�A��DA�x�A�=qA�$�A�A�A���A��RA�l�A��A��PA�r�A��A�7LA���A���A���A�bNA�K�A�/A�bA���A��7A���A��FA�v�A�
=A��A��hA�%A��\A�p�A��A���A��A��`A�z�A�C�A�XA�r�A���A�v�A�ƨA���A�ĜA���A��/A��HA�?}A��7A��#A��A���A�-A/A|�!A{t�Az��Az�uAx�/Aw�hAu��As�#Ar^5Apr�Ao��AnȴAl�9AjĜAit�Ag��Af��Af5?Ae��Ad�yAa�mA^  A\(�AZffAU��ASdZAR��AQ\)AOANv�AL��AJ��AH�/AF�yAFz�AE��AD�AC�;AC�AC��ACl�AC�ACdZAC�AB�jAA�#AAVA@JA?�A>r�A=��A<��A;��A:Q�A9�wA8E�A6A4��A4�uA4I�A4�/A3�A2=qA1ƨA1G�A0�A/��A.��A-��A,A�A+G�A)A(z�A'XA%��A#��A"(�A!K�A �A�AA��A��A  A�A�AVA�RA�FA�A�jA��A?}A�HA^5A�
A;dA��AA/A�wAt�A�uA`BA
�/A
�DA	ƨA	+A��A�;AS�AƨA+A�/AM�AdZA��A�\AJAG�@��@��!@���@�E�@���@�z�@�(�@���@��;@��w@�K�@�b@��y@�@���@�z�@���@�t�@�C�@��H@�@���@�bN@�1@띲@���@�@�I�@�-@��@�j@�Z@��@��D@�v�@۶F@�V@��T@٩�@ؓu@ץ�@�~�@�@Ձ@��/@Ԭ@�9X@���@��/@�o@θR@Ͳ-@��`@�r�@���@��y@�5?@�x�@���@ȋD@ǝ�@�v�@ź^@�p�@�G�@��@Ĵ9@�1'@���@ÍP@�v�@���@�j@�A�@�I�@�Z@�j@��@�z�@� �@� �@��@��@�j@�S�@�V@��/@�r�@�j@�I�@��F@�dZ@��@��@�hs@�/@�&�@���@�bN@�I�@�A�@�ƨ@��@���@��@���@�1'@��m@�|�@�o@��R@�n�@���@�7L@��@�V@��@��@�?}@�X@�p�@�x�@�V@�9X@�  @���@�;d@�5?@�{@��#@���@�%@��j@��u@��@�Z@���@�v�@��#@���@�G�@�r�@��w@��@��@��@�33@�o@�ȴ@�^5@�5?@�=q@�E�@�J@�x�@��j@�(�@���@�@��@�
=@�v�@�M�@�{@��#@��7@�/@���@�7L@�x�@�=q@���@��R@�o@��
@��@��m@��w@��F@�;d@���@�~�@�n�@�ff@�=q@��#@�`B@��@��@��@�bN@�9X@�b@��@�|�@�\)@�\)@�\)@�\)@�;d@�+@��@�
=@�ȴ@�E�@�@���@���@��;@�|�@�-@�?}@��^@��@��\@���@x4n@jc 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�)B�#B�)B�)B�B��B��B��B��B��BɺBŢB��B�qB�RB�-B�B��B��B��B�uB�\B�=B�7B�bB��B��B�'B�!B�B�B�'B�B��B��B��B�oB�=B�B}�Bs�BdZBR�B>wB(�BoB
��B
�B
��BBuB'�B-B�BDB
��B
�B
�B
��B
�ZB
B
��B
z�B
p�B
�hB
�VB
u�B
;dB
JB	�B	�jB	��B	�{B	gmB	�VB	��B
T�B
l�B
�B
�B�B
=BuB�BQ�B��B��B�NB�B�yB�`B�)B��B��B��BǮBƨBŢB��B�dB�qB�qBBĜBŢBƨB�!B��B�B��B�3B��B�\BXB=qB0!B�B+B
�sB
��B
��B
�FB
�B
��B
�7B
� B
z�B
v�B
o�B
bNB
YB
J�B
@�B
33B
+B
&�B
�B
1B
B	��B	�B	�B	�mB	�BB	��B	�B	��B	�oB	v�B	ffB	aHB	ZB	Q�B	K�B	D�B	;dB	7LB	,B	(�B	#�B	�B	�B	�B	�B	�B	�B	{B	hB	hB	�B	uB	
=B	1B	1B	B��B�B�mB�ZB�BŢB�qBǮBŢB��B��B�}B�^B�LB�LB�3B�B��B��B��B��B�uB�uB�\B�=B�B� B~�B{�By�Bv�Bs�Bq�Bn�Bm�Bk�BffBcTBaHB`BB\)BZBYBXBVBS�BN�BI�BH�BD�BC�BB�B@�B>wB=qB;dB;dB9XB8RB5?B6FB49B2-B2-B2-B/B/B.B.B.B.B/B5?B7LB6FB6FB5?B5?B49B49B<jBE�BL�BN�BP�BR�BS�BT�BVBZB]/B^5B^5B_;B`BBbNBe`BhsBhsBhsBgmBiyBffBffBdZBcTBdZBe`BffBgmBgmBffBffBdZBcTBbNB`BBe`Bl�Bm�Bq�Bs�Bt�By�B~�B�B�%B�1B�1B�JB�hB�{B�{B��B��B��B��B��B��B��B�9B�RB�^B�wBBŢB��B��B��B��B��B��B��B��B�mB�B�B�B��B��B��B��B	B	1B	1B	1B	1B	
=B		7B		7B	
=B	VB	\B	hB	bB	oB	uB	�B	�B	�B	�B	 �B	(�B	/B	2-B	49B	6FB	8RB	:^B	<jB	A�B	K�B	P�B	S�B	W
B	ZB	^5B	^5B	_;B	`BB	cTB	e`B	iyB	l�B	m�B	o�B	q�B	s�B	t�B	t�B	v�B	x�B	|�B	x�B	x�B	x�B	x�B	y�B	y�B	z�B	|�B	�B	�B	�%B	�B	�B	�+B	�1B	�=B	�VB	�hB	�oB	�uB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�!B	�9B	�9B	�9B	�FB	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	ɺB	ĜB	B	�wB	�XB	�^B	�FB	�B	��B	��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�)B�#B�)B�)B�B��B��B��B��B��BɺBŢB��B�qB�RB�-B�B��B��B��B�uB�\B�=B�7B�bB��B��B�'B�!B�B�B�'B�B��B��B��B�oB�=B�B}�Bs�BdZBR�B>wB(�BoB
��B
�B
��BBuB'�B-B�BDB
��B
�B
�B
��B
�ZB
B
��B
z�B
p�B
�hB
�VB
u�B
;dB
JB	�B	�jB	��B	�{B	gmB	�VB	��B
T�B
l�B
�B
�B�B
=BuB�BQ�B��B��B�NB�B�yB�`B�)B��B��B��BǮBƨBŢB��B�dB�qB�qBBĜBŢBƨB�!B��B�B��B�3B��B�\BXB=qB0!B�B+B
�sB
��B
��B
�FB
�B
��B
�7B
� B
z�B
v�B
o�B
bNB
YB
J�B
@�B
33B
+B
&�B
�B
1B
B	��B	�B	�B	�mB	�BB	��B	�B	��B	�oB	v�B	ffB	aHB	ZB	Q�B	K�B	D�B	;dB	7LB	,B	(�B	#�B	�B	�B	�B	�B	�B	�B	{B	hB	hB	�B	uB	
=B	1B	1B	B��B�B�mB�ZB�BŢB�qBǮBŢB��B��B�}B�^B�LB�LB�3B�B��B��B��B��B�uB�uB�\B�=B�B� B~�B{�By�Bv�Bs�Bq�Bn�Bm�Bk�BffBcTBaHB`BB\)BZBYBXBVBS�BN�BI�BH�BD�BC�BB�B@�B>wB=qB;dB;dB9XB8RB5?B6FB49B2-B2-B2-B/B/B.B.B.B.B/B5?B7LB6FB6FB5?B5?B49B49B<jBE�BL�BN�BP�BR�BS�BT�BVBZB]/B^5B^5B_;B`BBbNBe`BhsBhsBhsBgmBiyBffBffBdZBcTBdZBe`BffBgmBgmBffBffBdZBcTBbNB`BBe`Bl�Bm�Bq�Bs�Bt�By�B~�B�B�%B�1B�1B�JB�hB�{B�{B��B��B��B��B��B��B��B�9B�RB�^B�wBBŢB��B��B��B��B��B��B��B��B�mB�B�B�B��B��B��B��B	B	1B	1B	1B	1B	
=B		7B		7B	
=B	VB	\B	hB	bB	oB	uB	�B	�B	�B	�B	 �B	(�B	/B	2-B	49B	6FB	8RB	:^B	<jB	A�B	K�B	P�B	S�B	W
B	ZB	^5B	^5B	_;B	`BB	cTB	e`B	iyB	l�B	m�B	o�B	q�B	s�B	t�B	t�B	v�B	x�B	|�B	x�B	x�B	x�B	x�B	y�B	y�B	z�B	|�B	�B	�B	�%B	�B	�B	�+B	�1B	�=B	�VB	�hB	�oB	�uB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�!B	�9B	�9B	�9B	�FB	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	ɺB	ĜB	B	�wB	�XB	�^B	�FB	�B	��B	��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141548                              AO  ARCAADJP                                                                    20181024141548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141548  QCF$                G�O�G�O�G�O�0               