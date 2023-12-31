CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:17Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190617  20181005190617  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               	A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ףu�~1   @ף+�Y@3���n��c�V�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      	A   A   A   @9��@�  @�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C5�fC8  C9�fC;�fC>  C@�CB�CD�CF�CH�CJ  CK�fCM�fCP  CR�CT  CV  CX�CZ�C\  C^�C`33Cb�Cc�fCe�fCh  Cj33Cl33Cn�Cp33Cr33Ct33Cv�Cx  Cy��C|  C~  C�fC��3C��fC��3C��fC��fC��fC�  C��C��C��fC��C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C��C�  C��C�  C��fC�  C��C��3C��C��3C��C��fC��C�  C��fC��fC��C��C��fC��fC��fC��fC��fC��3C��3C�  C��C��C��C�  C��C��C��C��C��C��C��3C�  C��C�  C��3C�  C��C��3C�  C�  C�  C�  C�  C��C��C��C�  C�  C��C��C�  C�  C�  C��3C��3C��C��C��C�  C�  C��C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3D   D �fD  Dy�D  D��D  D� D  Dy�D  D� DfD� D  D�fDfD� D�3D	s3D	��D
� D�D� D��D�fD  Dy�D��Dy�D��Dy�D  D�fDfD� DfD� D��D�fDfDy�D  D�fDfD�fDfD��D  Dy�D��D� DfD��D  Dy�D  D�fD  Ds3D  D��D�D� D��D y�D �3D!s3D!��D"y�D"��D#s3D#�3D$s3D$�3D%s3D%�3D&y�D'fD'�fD(fD(��D)�D)� D)�3D*s3D*��D+�fD,  D,s3D-  D-�fD.�D.�fD/  D/y�D0  D0� D0��D1� D2  D2y�D3  D3�fD4  D4� D5fD5�fD6  D6�fD7  D7� D8  D8� D9  D9� D:fD:� D;  D;y�D<  D<� D<��D=� D>fD>� D>��D?y�D@  D@y�DA  DA� DB  DBy�DB��DC� DD�DD��DE�DE�fDFfDF�fDG  DG� DH  DH� DI  DI�fDJfDJ��DK  DKy�DL  DLy�DL�3DMs3DM��DN�fDO�DO��DP�DP��DQfDQ� DR  DRy�DR�3DSs3DS�3DTs3DT�3DUy�DVfDV��DWfDWy�DW��DX�fDYfDY� DY��DZy�DZ��D[� D\  D\� D]  D]y�D]�3D^� D_fD_�fD`  D`y�Da  Da�fDb  Dby�Db��Dc� Dd  Dd�fDe  Dey�De��Df� Df��Dg� DhfDhy�Di  Di�fDjfDj�fDk  Dk� Dl  Dl� Dm  Dm� DnfDn�fDofDo� DpfDpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu�fDvfDvy�DvٚDy��D�K�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Dz�@�p�@�p�A�RA"�RAB�RAa�A��\A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�BzB �B(�B0�B8�B@�BH�BP�BX�B`�Bh�BqzByzB�W
B�W
B�W
B�W
B�#�B�W
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
B��=B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�CEC+�C+�C+�C+�C�C+�C+�C+�C +�C"+�C$+�C&+�C(EC*+�C,+�C.+�C0+�C2+�C4+�C6�C8+�C:�C<�C>+�C@ECBECDECFECHECJ+�CL�CN�CP+�CRECT+�CV+�CXECZEC\+�C^EC`^�CbECd�Cf�Ch+�Cj^�Cl^�CnECp^�Cr^�Ct^�CvECx+�Cy�RC|+�C~+�C��C��C��)C��C��)C��)C��)C��C�"�C�"�C��)C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�"�C��C�"�C��C��C��C��C��C�"�C��C�/]C��C��)C��C�/]C��C�"�C��C�"�C��)C�"�C��C��)C��)C�"�C�"�C��)C��)C��)C��)C��)C��C��C��C�"�C�/]C�"�C��C�"�C�/]C�/]C�/]C�/]C�"�C��C��C�"�C��C��C��C�"�C��C��C��C��C��C��C�"�C�"�C�"�C��C��C�"�C�"�C��C��C��C��C��C�"�C�/]C�"�C��C��C�"�C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��D 
�D �GD
�D�{D
�D��D
�D��D
�D�{D
�D��DGD��D
�D�GDGD��D�D	~D
{D
��D�D��D{D�GD
�D�{D{D�{D{D�{D
�D�GDGD��DGD��D{D�GDGD�{D
�D�GDGD�GDGD��D
�D�{D{D��DGD��D
�D�{D
�D�GD
�D~D
�D��D�D��D {D �{D �D!~D"{D"�{D#{D#~D#�D$~D$�D%~D%�D&�{D'GD'�GD(GD(��D)�D)��D)�D*~D+{D+�GD,
�D,~D-
�D-�GD.�D.�GD/
�D/�{D0
�D0��D1{D1��D2
�D2�{D3
�D3�GD4
�D4��D5GD5�GD6
�D6�GD7
�D7��D8
�D8��D9
�D9��D:GD:��D;
�D;�{D<
�D<��D={D=��D>GD>��D?{D?�{D@
�D@�{DA
�DA��DB
�DB�{DC{DC��DD�DD��DE�DE�GDFGDF�GDG
�DG��DH
�DH��DI
�DI�GDJGDJ��DK
�DK�{DL
�DL�{DL�DM~DN{DN�GDO�DO��DP�DP��DQGDQ��DR
�DR�{DR�DS~DS�DT~DT�DU�{DVGDV��DWGDW�{DX{DX�GDYGDY��DZ{DZ�{D[{D[��D\
�D\��D]
�D]�{D]�D^��D_GD_�GD`
�D`�{Da
�Da�GDb
�Db�{Dc{Dc��Dd
�Dd�GDe
�De�{Df{Df��Dg{Dg��DhGDh�{Di
�Di�GDjGDj�GDk
�Dk��Dl
�Dl��Dm
�Dm��DnGDn�GDoGDo��DpGDp�{Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��DuGDu�GDvGDv�{Dv�{Dy��D�P�D��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�v�A�v�A�x�A�t�A�r�A�r�A�t�A�r�A�r�A�n�A�hsA�ffA�dZA�^5A�\)A�XA�VA�XA�M�A��HA�A��A�5?A��AǛ�A�S�AƸRA�S�A�bA�ȴA�^5A��Aĩ�A�|�A��;A�33A��A�x�A��^A���A�p�A�Q�A���A�ĜA�bA�I�A�A�A�ĜA�7LA�|�A�/A��A�  A���A�I�A�p�A�n�A���A��A�^5A�~�A�;dA��mA�t�A��A��TA���A��A��mA���A�Q�A�-A��;A�x�A�(�A��A�%A�\)A��A��;A�VA��A�?}A��FA���A�M�A��PA�bA�^5A�z�A��wA���A�E�A��uA��A~��A{�TAw33Av~�At�yAt�ArQ�An�DAjbAf�RAcƨAa�A]&�AXv�AVAT��AR�9AQ�-AP�ANI�AL�AJȴAI"�AF��AC�TACt�AB��ABA�AA��A@�jA@I�A?�A?��A?dZA?33A:ffA6��A4�DA3oA2-A1�#A0�A-��A+��A*�+A)��A(n�A'XA'
=A&E�A%+A$�A#`BA"��A!�A ��A��A?}A�!A�wA�AdZA^5A+A �A��A9XA(�A��A�PA7LA?}A/A/A"�AoA1'AG�A�!A~�Av�A�A"�AVA�yAr�A�A
-A	ƨA	S�A�A�#A�;A��AA r�A b@�v�@��@��+@�/@�I�@�33@�@�?}@�j@�bN@�dZ@�E�@�G�@��@�1@�K�@�{@�1@��@�@��y@�J@�`B@��@�b@�x�@��`@�9X@۝�@߮@�r�@��@ާ�@�@���@��@��@�bN@��T@ԋD@�X@���@�A�@�A�@ͩ�@��/@��
@�hs@�A�@��
@�;d@Ə\@���@ě�@î@���@�G�@���@�Ĝ@�r�@� �@���@�
=@��!@���@���@���@�=q@�x�@��u@�b@���@�C�@��@���@�`B@�9X@���@�x�@��j@�bN@���@�"�@�ȴ@��!@���@���@��+@�v�@�J@���@���@���@�1'@�ƨ@��P@�S�@��@���@�n�@�5?@�$�@�{@��#@���@�`B@�O�@�?}@��j@���@�z�@� �@�1@�  @��
@��P@�n�@�@���@��h@��h@��h@��@�x�@�x�@�p�@��@��/@��u@��@��w@��P@�|�@�dZ@�S�@�+@�o@��+@�@��@��`@��j@�bN@�ƨ@�|�@��y@�=q@�{@�J@��#@�p�@��@�Z@��
@��@��@�t�@�S�@���@���@�x�@�G�@�7L@��@��/@��@��u@�z�@�Z@�Q�@�A�@��@�K�@�;d@�33@�+@��y@�ff@���@��@���@���@�(�@���@�33@��@�ff@��T@��^@�7L@�z�@�9X@�1'@��;@���@���@��P@�dZ@��@���@���@���@��R@���@���@��\@��+@�~�@�v�@�^5@�E�@�-@��@��@�{@���@��@���@���@��#@��-@�&�@���@�I�@���@��;@��@���@��@�C�@�@�ȴ@���@�V@���@���@�X@��j@�j@�9X@�b@��
@��P@�dZ@�;d@�"�@��@�
=@���@��R@���@���@���@�~�@�{@��@��#@��-@��@���@���@��D@�(�@��m@��@��@�C�@���@���@�ff@�ff@�ff@�J@��@���@��-@�p�@�%@�Ĝ@��@�Z@�I�@�9X@��@�ƨ@�t�@�\)@�C�@�o@�@���@��y@���@���@�v�@�5?@�{@�J@�J@��-@�G�@�M�@pl"@bYK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�t�A�v�A�v�A�x�A�t�A�r�A�r�A�t�A�r�A�r�A�n�A�hsA�ffA�dZA�^5A�\)A�XA�VA�XA�M�A��HA�A��A�5?A��AǛ�A�S�AƸRA�S�A�bA�ȴA�^5A��Aĩ�A�|�A��;A�33A��A�x�A��^A���A�p�A�Q�A���A�ĜA�bA�I�A�A�A�ĜA�7LA�|�A�/A��A�  A���A�I�A�p�A�n�A���A��A�^5A�~�A�;dA��mA�t�A��A��TA���A��A��mA���A�Q�A�-A��;A�x�A�(�A��A�%A�\)A��A��;A�VA��A�?}A��FA���A�M�A��PA�bA�^5A�z�A��wA���A�E�A��uA��A~��A{�TAw33Av~�At�yAt�ArQ�An�DAjbAf�RAcƨAa�A]&�AXv�AVAT��AR�9AQ�-AP�ANI�AL�AJȴAI"�AF��AC�TACt�AB��ABA�AA��A@�jA@I�A?�A?��A?dZA?33A:ffA6��A4�DA3oA2-A1�#A0�A-��A+��A*�+A)��A(n�A'XA'
=A&E�A%+A$�A#`BA"��A!�A ��A��A?}A�!A�wA�AdZA^5A+A �A��A9XA(�A��A�PA7LA?}A/A/A"�AoA1'AG�A�!A~�Av�A�A"�AVA�yAr�A�A
-A	ƨA	S�A�A�#A�;A��AA r�A b@�v�@��@��+@�/@�I�@�33@�@�?}@�j@�bN@�dZ@�E�@�G�@��@�1@�K�@�{@�1@��@�@��y@�J@�`B@��@�b@�x�@��`@�9X@۝�@߮@�r�@��@ާ�@�@���@��@��@�bN@��T@ԋD@�X@���@�A�@�A�@ͩ�@��/@��
@�hs@�A�@��
@�;d@Ə\@���@ě�@î@���@�G�@���@�Ĝ@�r�@� �@���@�
=@��!@���@���@���@�=q@�x�@��u@�b@���@�C�@��@���@�`B@�9X@���@�x�@��j@�bN@���@�"�@�ȴ@��!@���@���@��+@�v�@�J@���@���@���@�1'@�ƨ@��P@�S�@��@���@�n�@�5?@�$�@�{@��#@���@�`B@�O�@�?}@��j@���@�z�@� �@�1@�  @��
@��P@�n�@�@���@��h@��h@��h@��@�x�@�x�@�p�@��@��/@��u@��@��w@��P@�|�@�dZ@�S�@�+@�o@��+@�@��@��`@��j@�bN@�ƨ@�|�@��y@�=q@�{@�J@��#@�p�@��@�Z@��
@��@��@�t�@�S�@���@���@�x�@�G�@�7L@��@��/@��@��u@�z�@�Z@�Q�@�A�@��@�K�@�;d@�33@�+@��y@�ff@���@��@���@���@�(�@���@�33@��@�ff@��T@��^@�7L@�z�@�9X@�1'@��;@���@���@��P@�dZ@��@���@���@���@��R@���@���@��\@��+@�~�@�v�@�^5@�E�@�-@��@��@�{@���@��@���@���@��#@��-@�&�@���@�I�@���@��;@��@���@��@�C�@�@�ȴ@���@�V@���@���@�X@��j@�j@�9X@�b@��
@��P@�dZ@�;d@�"�@��@�
=@���@��R@���@���@���@�~�@�{@��@��#@��-@��@���@���@��D@�(�@��m@��@��@�C�@���@���@�ff@�ff@�ff@�J@��@���@��-@�p�@�%@�Ĝ@��@�Z@�I�@�9X@��@�ƨ@�t�@�\)@�C�@�o@�@���@��y@���@���@�v�@�5?@�{@�J@�J@��-@�G�@�M�@pl"@bYK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�`B#�Br�B�hB��B�B�dB��B�HB�`B�BB�B$�B0!B7LB5?B9XB:^B7LB+B'�BO�BW
BZBgmB�-B�B�NB��B%BBB��B�B��B�RB�B��B��B��B��B�uB�hB�VB�7B�B� By�Bv�Bs�Bm�B_;BS�B(�BɺB�!B��BjB1'B"�B/B5?B1'B'�B�BoBB
�B
��B
��B
�B
w�B
cTB
YB
S�B
J�B
;dB
�B
oB
+B	��B	�B	��B	�B	�oB	v�B	_;B	6FB	\B	B��B�B�TB�)B��B��B�LB�'B��B��B��B��B��B��B��B��B��B��B��B��B�bB�1B�B�B��B��B�B�B�'B�LB�qB��BĜBŢBɺB��B��B��B�B�5B�TB�BB�/B�/B�/B�B�B�B��B��BɺB��BƨB��BŢB��B��B��B��B��B��B��B�B�/B�5B�5B�;B�NB�NB�HB�BB�/B�B��B��BɺB��B�-B��B��B��B�{B�VB�JB�\B�bB�bB�bB�bB�bB�bB�\B�\B�\B�VB�VB�\B�bB�bB�\B�hB�oB��B��B��B��B��B��B��B��B�B��B�NB�sB�yB�yB�B�yB�sB�ZB�fB�fB�B��B��B�B�B��B��B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	1B	1B	1B	1B	
=B	\B	�B	�B	�B	�B	�B	�B	�B	!�B	(�B	/B	33B	5?B	:^B	=qB	?}B	@�B	@�B	A�B	A�B	B�B	F�B	G�B	H�B	L�B	P�B	S�B	T�B	VB	YB	[#B	]/B	_;B	_;B	`BB	bNB	cTB	ffB	ffB	gmB	o�B	q�B	r�B	v�B	w�B	w�B	x�B	x�B	{�B	~�B	�B	�B	�%B	�%B	�1B	�7B	�7B	�7B	�DB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�9B	�LB	�XB	�XB	�dB	�}B	��B	B	ÖB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�NB	�TB	�TB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B

=B
�B
 vB
.�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�`B#�Br�B�hB��B�B�dB��B�HB�`B�BB�B$�B0!B7LB5?B9XB:^B7LB+B'�BO�BW
BZBgmB�-B�B�NB��B%BBB��B�B��B�RB�B��B��B��B��B�uB�hB�VB�7B�B� By�Bv�Bs�Bm�B_;BS�B(�BɺB�!B��BjB1'B"�B/B5?B1'B'�B�BoBB
�B
��B
��B
�B
w�B
cTB
YB
S�B
J�B
;dB
�B
oB
+B	��B	�B	��B	�B	�oB	v�B	_;B	6FB	\B	B��B�B�TB�)B��B��B�LB�'B��B��B��B��B��B��B��B��B��B��B��B��B�bB�1B�B�B��B��B�B�B�'B�LB�qB��BĜBŢBɺB��B��B��B�B�5B�TB�BB�/B�/B�/B�B�B�B��B��BɺB��BƨB��BŢB��B��B��B��B��B��B��B�B�/B�5B�5B�;B�NB�NB�HB�BB�/B�B��B��BɺB��B�-B��B��B��B�{B�VB�JB�\B�bB�bB�bB�bB�bB�bB�\B�\B�\B�VB�VB�\B�bB�bB�\B�hB�oB��B��B��B��B��B��B��B��B�B��B�NB�sB�yB�yB�B�yB�sB�ZB�fB�fB�B��B��B�B�B��B��B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	1B	1B	1B	1B	
=B	\B	�B	�B	�B	�B	�B	�B	�B	!�B	(�B	/B	33B	5?B	:^B	=qB	?}B	@�B	@�B	A�B	A�B	B�B	F�B	G�B	H�B	L�B	P�B	S�B	T�B	VB	YB	[#B	]/B	_;B	_;B	`BB	bNB	cTB	ffB	ffB	gmB	o�B	q�B	r�B	v�B	w�B	w�B	x�B	x�B	{�B	~�B	�B	�B	�%B	�%B	�1B	�7B	�7B	�7B	�DB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�9B	�LB	�XB	�XB	�dB	�}B	��B	B	ÖB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�NB	�TB	�TB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B

=B
�B
 vB
.�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190617                              AO  ARCAADJP                                                                    20181005190617    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190617  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190617  QCF$                G�O�G�O�G�O�8000            