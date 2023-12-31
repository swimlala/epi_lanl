CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:42Z creation      
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
resolution        =���   axis      Z        <  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  B|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  R�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  [�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  d�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  sT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  u$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  ~0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20181005191742  20181005191742  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$����1   @��%8㠖@4����+�d�Q��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C=�fC?�fCB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCs�fCu�fCw�fCz  C|  C~  C�  C��3C��3C��3C��3C��3C��3C�  C��C��C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C��C��C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C�  C��C��C��3C�  C��C�  C��C�  C��3C��3C�  C��3C��3C��C��C�  C��3C��3C�  C��C�  C��3C��3C��C��C��3C��3C��C�  C�  C��C��C��C��3C��3C��3C��3C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D��D� D��D� DfD� D  Dy�D��D� D��Dy�D  D�fD	  D	y�D
  D
y�DfD�fD  D�fD  D� D  Dy�D  D� D  D� D  D�fD  D� D  D� D  D� DfDy�D  D� D  D�fDfD� D��Dy�D  D�fDfD�fD  Dy�D  D� D  D� D  Dy�D   D � D ��D!� D"fD"�fD#fD#� D$  D$� D%  D%�fD&fD&�fD'fD'� D(  D(y�D(��D)� D)��D*� D+fD+�fD,  D,� D-  D-� D-��D.� D/  D/y�D0  D0�fD1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D5��D6�fD7  D7� D8  D8� D9fD9y�D9��D:�fD;fD;� D;��D<� D=fD=�fD=��D>� D?  D?y�D@fD@y�DA�DA� DB  DB�fDB��DC� DD  DDy�DE  DE�fDE��DF� DGfDG�fDHfDH�fDH��DIs3DI�3DJ� DKfDKy�DLfDL�fDL��DM�fDNfDNy�DO  DO�fDO��DP� DQfDQ�fDR  DR�fDR��DS� DTfDTy�DU  DU� DU��DV� DWfDWy�DW��DX�fDY  DYy�DY��DZ� D[fD[� D[��D\� D]fD]� D]�3D^y�D_  D_�fD`fD`y�DafDa�fDa��Dby�Dc  Dc�fDc��Dd�fDe  Des3Df  Dy�\D�5qD�]q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Dz�@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�(�A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BYzB`�Bh�Bp�Bx�B�W
B�W
B�W
B�W
B��=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�CEC+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�CEC +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8�C:+�C<+�C>�C@�CB+�CD+�CF+�CHECJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr�Ct�Cv�Cx�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C�"�C�"�C�"�C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C�"�C��C��C��C�"�C��C��C��C��C��C��C�"�C�"�C�"�C��C��C��C�"�C��C��C�"�C�"�C��C��C�"�C��C�"�C��C��C��C��C��C��C�"�C�"�C��C��C��C��C�"�C��C��C��C�"�C�"�C��C��C�"�C��C��C�"�C�"�C�"�C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C�"�C��C��C��C��C��C��C��C��D 
�D ��D
�D��D{D��D{D��DGD��D
�D�{D{D��D{D�{D
�D�GD	
�D	�{D

�D
�{DGD�GD
�D�GD
�D��D
�D�{D
�D��D
�D��D
�D�GD
�D��D
�D��D
�D��DGD�{D
�D��D
�D�GDGD��D{D�{D
�D�GDGD�GD
�D�{D
�D��D
�D��D
�D�{D 
�D ��D!{D!��D"GD"�GD#GD#��D$
�D$��D%
�D%�GD&GD&�GD'GD'��D(
�D(�{D){D)��D*{D*��D+GD+�GD,
�D,��D-
�D-��D.{D.��D/
�D/�{D0
�D0�GD1
�D1��D2
�D2�GD3
�D3��D4
�D4��D5
�D5��D6{D6�GD7
�D7��D8
�D8��D9GD9�{D:{D:�GD;GD;��D<{D<��D=GD=�GD>{D>��D?
�D?�{D@GD@�{DA�DA��DB
�DB�GDC{DC��DD
�DD�{DE
�DE�GDF{DF��DGGDG�GDHGDH�GDI{DI~DI�DJ��DKGDK�{DLGDL�GDM{DM�GDNGDN�{DO
�DO�GDP{DP��DQGDQ�GDR
�DR�GDS{DS��DTGDT�{DU
�DU��DV{DV��DWGDW�{DX{DX�GDY
�DY�{DZ{DZ��D[GD[��D\{D\��D]GD]��D]�D^�{D_
�D_�GD`GD`�{DaGDa�GDb{Db�{Dc
�Dc�GDd{Dd�GDe
�De~Df
�Dy�=D�:�D�b�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aȣ�A�r�AǼjA�n�A�7LA��AƟ�AƇ+A�p�A�bNA�\)A�M�A�33A�+A��A��;A���A���AŲ-Aŏ\A�l�A�oA���AľwAć+A�XA��A��yA��A��
A�ƨA�5?A©�A� �A��A���A�|�A�bNA�VA�E�A�1'A���A��A��A��DA��A�A�z�A�v�A�bA���A��HA���A�Q�A���A�1A�S�A��A���A�|�A�JA��!A�1A���A��`A�O�A��
A�p�A�VA���A��PA�z�A�$�A��A�hsA��\A�ZA��A���A��hA�x�A�VA��#A���A��A��wA�{A�A�A�JA��A�~�A�
=A���A��+A�jA��A��PA���A��A��A���A�l�A��;A�A��DA|n�A|  A{ƨA{K�Ay�TAy33AwC�AuAt��As��As`BArffAqhsAp1'Al��AiƨAhVAg�wAf�Ad�+AcC�A`��A^v�A\�A[VAXv�AV^5AU�mAT�ATE�AS�-AS�AR��AQ�-AP(�AN�HAMK�AKp�AI�^AHffAG��AG�AE��ACx�AA�FA@^5A?
=A=&�A;l�A9��A6�A4jA3��A2�jA2A1"�A0E�A.��A.9XA-��A-O�A-�A,$�A+?}A*jA)�A(��A(M�A'O�A%��A%33A$ZA#A#O�A"��A!A �A�#AoA$�A"�A-A��A�uA~�AVA1'AƨA�jA1Al�AĜAA��Ax�A�A��A�+A�AdZA|�A$�A+A�A��A�7A�A
1'A	K�A�A��A9XA�A��A/A�/A5?AȴAbNA�mAK�A�/A9XA��A ��A ffA {@�C�@��+@�`B@���@�K�@���@��D@��@�J@���@�Ĝ@�E�@��@�A�@��y@�G�@�I�@�
=@�^@�&�@�D@��@�-@�h@�G�@�V@�b@��@�b@���@�M�@�M�@�M�@ݩ�@�&�@�Q�@���@�/@��@Ցh@ԛ�@ӍP@�O�@�E�@�M�@ǶF@ư!@Ų-@�%@ă@��@Å@�@��@���@�+@��R@�M�@��T@��@���@�O�@�?}@���@���@��@�bN@�S�@�5?@��@���@��H@���@�n�@�v�@��\@��R@��\@�V@�$�@��+@���@�Z@���@�^5@��+@�{@�~�@�5?@�v�@�V@�J@��@�@��h@�?}@�A�@�\)@��!@���@���@�1@�|�@���@��@�5?@�n�@�=q@��-@��@���@��@��h@���@�\)@�C�@�"�@���@��@�~�@�E�@�J@�hs@�(�@�b@���@��@�K�@��F@��;@�C�@�{@��^@��^@�v�@��H@�ȴ@�-@��@�@�hs@���@�bN@��@��F@�;d@�M�@�?}@��@���@�"�@�x�@�V@���@�l�@��@��+@���@�v�@�^5@��@�J@�p�@�V@�V@�J@��#@�p�@�%@�Ĝ@�Q�@���@�S�@�;d@�"�@�o@�"�@�\)@�;d@�33@��@���@�~�@�V@�5?@�-@�J@��T@�@���@���@���@�`B@�&�@�V@���@��`@�Ĝ@���@�r�@�(�@���@��;@��;@��F@���@�|�@�\)@�+@��y@�ȴ@�ȴ@�ȴ@���@���@�-@���@��7@�`B@��@��@���@�Z@�A�@�(�@�1@�1@�  @���@w9�@c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aȣ�A�r�AǼjA�n�A�7LA��AƟ�AƇ+A�p�A�bNA�\)A�M�A�33A�+A��A��;A���A���AŲ-Aŏ\A�l�A�oA���AľwAć+A�XA��A��yA��A��
A�ƨA�5?A©�A� �A��A���A�|�A�bNA�VA�E�A�1'A���A��A��A��DA��A�A�z�A�v�A�bA���A��HA���A�Q�A���A�1A�S�A��A���A�|�A�JA��!A�1A���A��`A�O�A��
A�p�A�VA���A��PA�z�A�$�A��A�hsA��\A�ZA��A���A��hA�x�A�VA��#A���A��A��wA�{A�A�A�JA��A�~�A�
=A���A��+A�jA��A��PA���A��A��A���A�l�A��;A�A��DA|n�A|  A{ƨA{K�Ay�TAy33AwC�AuAt��As��As`BArffAqhsAp1'Al��AiƨAhVAg�wAf�Ad�+AcC�A`��A^v�A\�A[VAXv�AV^5AU�mAT�ATE�AS�-AS�AR��AQ�-AP(�AN�HAMK�AKp�AI�^AHffAG��AG�AE��ACx�AA�FA@^5A?
=A=&�A;l�A9��A6�A4jA3��A2�jA2A1"�A0E�A.��A.9XA-��A-O�A-�A,$�A+?}A*jA)�A(��A(M�A'O�A%��A%33A$ZA#A#O�A"��A!A �A�#AoA$�A"�A-A��A�uA~�AVA1'AƨA�jA1Al�AĜAA��Ax�A�A��A�+A�AdZA|�A$�A+A�A��A�7A�A
1'A	K�A�A��A9XA�A��A/A�/A5?AȴAbNA�mAK�A�/A9XA��A ��A ffA {@�C�@��+@�`B@���@�K�@���@��D@��@�J@���@�Ĝ@�E�@��@�A�@��y@�G�@�I�@�
=@�^@�&�@�D@��@�-@�h@�G�@�V@�b@��@�b@���@�M�@�M�@�M�@ݩ�@�&�@�Q�@���@�/@��@Ցh@ԛ�@ӍP@�O�@�E�@�M�@ǶF@ư!@Ų-@�%@ă@��@Å@�@��@���@�+@��R@�M�@��T@��@���@�O�@�?}@���@���@��@�bN@�S�@�5?@��@���@��H@���@�n�@�v�@��\@��R@��\@�V@�$�@��+@���@�Z@���@�^5@��+@�{@�~�@�5?@�v�@�V@�J@��@�@��h@�?}@�A�@�\)@��!@���@���@�1@�|�@���@��@�5?@�n�@�=q@��-@��@���@��@��h@���@�\)@�C�@�"�@���@��@�~�@�E�@�J@�hs@�(�@�b@���@��@�K�@��F@��;@�C�@�{@��^@��^@�v�@��H@�ȴ@�-@��@�@�hs@���@�bN@��@��F@�;d@�M�@�?}@��@���@�"�@�x�@�V@���@�l�@��@��+@���@�v�@�^5@��@�J@�p�@�V@�V@�J@��#@�p�@�%@�Ĝ@�Q�@���@�S�@�;d@�"�@�o@�"�@�\)@�;d@�33@��@���@�~�@�V@�5?@�-@�J@��T@�@���@���@���@�`B@�&�@�V@���@��`@�Ĝ@���@�r�@�(�@���@��;@��;@��F@���@�|�@�\)@�+@��y@�ȴ@�ȴ@�ȴ@���@���@�-@���@��7@�`B@��@��@���@�Z@�A�@�(�@�1@�1@�  @���@w9�@c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�dB�HB �B-B49BG�BK�BM�BL�BL�BL�BK�BJ�BJ�BH�BE�BE�BE�BD�BD�BF�BQ�BXBZB^5BaHBffBjBl�Bl�Bm�Bw�B� B�=B�\B�hB��B��B�B�'B��BȴB��B��BƨB��B�}B�^B�'B��B��B��B��B��B��B��B��B��B�=B�Bx�Bq�BhsBW
BH�BE�BC�BC�BD�BE�BF�BD�B@�B=qB�BDB\B�B{BDB��B�5B�qB�B��B��B�DB{�BS�B33B�B+B
�B
�B
�mB
�5B
��B
��B
��B
�{B
�bB
��B
��B
��B
�bB
v�B
r�B
o�B
k�B
aHB
\)B
N�B
C�B
;dB
5?B
0!B
(�B
 �B
�B
B	�B	�ZB	�5B	��B	��B	ÖB	�LB	��B	��B	�oB	�B	x�B	t�B	p�B	l�B	jB	e`B	aHB	\)B	R�B	I�B	@�B	5?B	+B	#�B	 �B	�B	{B		7B	B��B�B�yB�`B�/B��B��B��B��B��B��B��B�wB�qB�qB�qB�dB�^B�^B�jB�}B�wB�jB�XB�LB�?B�9B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�hB�bB�VB�PB�PB�DB�1B�+B�+B�%B�B�B�B~�B~�B}�B}�B}�B}�B|�B|�B|�B{�Bz�Bz�By�By�By�Bx�Bx�By�By�Bz�Bz�By�By�By�By�By�Bx�Bx�Bx�Bw�Bw�Bx�By�Bx�By�Bz�Bz�Bz�B{�Bz�B{�B~�B~�B� B� B� B~�B}�B� B�B�B�B�B�B�B�B�1B�=B�7B�DB�PB�VB�VB�DB�B�B�%B�%B�+B�1B�1B�+B�1B�=B�oB�{B��B��B��B��B��B�B�'B�FB�dB�qB�wB��BÖBĜBÖBB��B��BBÖBƨBȴB��B��B�B�#B�B�5B�NB�yB�B��B	B	
=B	\B	oB	uB	�B	�B	�B	�B	$�B	&�B	)�B	2-B	6FB	:^B	<jB	<jB	>wB	@�B	A�B	?}B	@�B	A�B	F�B	I�B	O�B	S�B	S�B	S�B	T�B	T�B	VB	T�B	S�B	Q�B	M�B	O�B	T�B	T�B	XB	aHB	ffB	gmB	ffB	hsB	k�B	q�B	v�B	x�B	{�B	|�B	}�B	� B	� B	�B	�B	�B	� B	~�B	}�B	� B	�B	�B	� B	�B	~�B	}�B	}�B	� B	�B	�%B	�1B	�=B	�=B	�=B	�DB	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�dB	�qB	�wB	�}B	�}B	�}B	��B	B	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�B
�B
'2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�dB�HB �B-B49BG�BK�BM�BL�BL�BL�BK�BJ�BJ�BH�BE�BE�BE�BD�BD�BF�BQ�BXBZB^5BaHBffBjBl�Bl�Bm�Bw�B� B�=B�\B�hB��B��B�B�'B��BȴB��B��BƨB��B�}B�^B�'B��B��B��B��B��B��B��B��B��B�=B�Bx�Bq�BhsBW
BH�BE�BC�BC�BD�BE�BF�BD�B@�B=qB�BDB\B�B{BDB��B�5B�qB�B��B��B�DB{�BS�B33B�B+B
�B
�B
�mB
�5B
��B
��B
��B
�{B
�bB
��B
��B
��B
�bB
v�B
r�B
o�B
k�B
aHB
\)B
N�B
C�B
;dB
5?B
0!B
(�B
 �B
�B
B	�B	�ZB	�5B	��B	��B	ÖB	�LB	��B	��B	�oB	�B	x�B	t�B	p�B	l�B	jB	e`B	aHB	\)B	R�B	I�B	@�B	5?B	+B	#�B	 �B	�B	{B		7B	B��B�B�yB�`B�/B��B��B��B��B��B��B��B�wB�qB�qB�qB�dB�^B�^B�jB�}B�wB�jB�XB�LB�?B�9B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�hB�bB�VB�PB�PB�DB�1B�+B�+B�%B�B�B�B~�B~�B}�B}�B}�B}�B|�B|�B|�B{�Bz�Bz�By�By�By�Bx�Bx�By�By�Bz�Bz�By�By�By�By�By�Bx�Bx�Bx�Bw�Bw�Bx�By�Bx�By�Bz�Bz�Bz�B{�Bz�B{�B~�B~�B� B� B� B~�B}�B� B�B�B�B�B�B�B�B�1B�=B�7B�DB�PB�VB�VB�DB�B�B�%B�%B�+B�1B�1B�+B�1B�=B�oB�{B��B��B��B��B��B�B�'B�FB�dB�qB�wB��BÖBĜBÖBB��B��BBÖBƨBȴB��B��B�B�#B�B�5B�NB�yB�B��B	B	
=B	\B	oB	uB	�B	�B	�B	�B	$�B	&�B	)�B	2-B	6FB	:^B	<jB	<jB	>wB	@�B	A�B	?}B	@�B	A�B	F�B	I�B	O�B	S�B	S�B	S�B	T�B	T�B	VB	T�B	S�B	Q�B	M�B	O�B	T�B	T�B	XB	aHB	ffB	gmB	ffB	hsB	k�B	q�B	v�B	x�B	{�B	|�B	}�B	� B	� B	�B	�B	�B	� B	~�B	}�B	� B	�B	�B	� B	�B	~�B	}�B	}�B	� B	�B	�%B	�1B	�=B	�=B	�=B	�DB	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�dB	�qB	�wB	�}B	�}B	�}B	��B	B	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�B
�B
'2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191742                              AO  ARCAADJP                                                                    20181005191742    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191742  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191742  QCF$                G�O�G�O�G�O�8000            