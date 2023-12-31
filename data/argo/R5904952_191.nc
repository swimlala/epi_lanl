CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:49Z creation      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005190549  20181005190549  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)Z1   @��)�M�@1��hr��c��x���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B���B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3D   D �fD  D�fDfD�fDfD�fD  Dy�D��Dy�D��D� D  Dy�D  D� D	fD	� D
  D
� D  D� D  D�fDfD� D��Dy�D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D��D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D y�D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D-��D.y�D/  D/�fD0  D0y�D1  D1�fD2  D2y�D3  D3� D4  D4� D5fD5� D5��D6y�D6��D7� D8  D8�fD9  D9y�D:  D:� D;  D;y�D<  D<�fD=fD=y�D>  D>y�D>��D?� D@  D@� DA  DA� DA��DB� DC  DCy�DD  DDy�DE  DE� DF  DF� DG  DG� DH  DHy�DH��DI� DJfDJ�fDK  DKy�DL  DL�fDM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW�fDXfDX�fDY  DY� DZ  DZ� D[  D[y�D[��D\� D]fD]�fD^fD^�fDj� Dj��Dk� Dl  Dl� Dl��Dm� DnfDn�fDo  Doy�Do��Dp� DqfDq� Dq��Dr� Ds  Ds� Dt  Dty�Du  Du�fDv  Dv� Dw  Dw� Dw��Dy�RD�4)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�\)AG�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�ByQ�B���B�u�B�B�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�B�B�u�B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0!GC2!GC4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�*>C�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�*>C�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC��C��C��C�qC�qC�qC�qC�qC�qC�*>C�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC��C�qC�qC��C�qC�qC�qC��D �D �D�D�DD�DD�D�D�RDRD�RDRD��D�D�RD�D��D	D	��D
�D
��D�D��D�D�DD��DRD�RD�D��D�D�RD�D��D�D�RD�D��D�D��D�D��DRD��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��DD��D�D��D �D �RD!�D!��D"�D"��D#D#��D$�D$��D%�D%��D&D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-D-��D.RD.�RD/�D/�D0�D0�RD1�D1�D2�D2�RD3�D3��D4�D4��D5D5��D6RD6�RD7RD7��D8�D8�D9�D9�RD:�D:��D;�D;�RD<�D<�D=D=�RD>�D>�RD?RD?��D@�D@��DA�DA��DBRDB��DC�DC�RDD�DD�RDE�DE��DF�DF��DG�DG��DH�DH�RDIRDI��DJDJ�DK�DK�RDL�DL�DM�DM��DNDN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DWDW�DXDX�DY�DY��DZ�DZ��D[�D[�RD\RD\��D]D]�D^D^�Dj��DkRDk��Dl�Dl��DmRDm��DnDn�Do�Do�RDpRDp��DqDq��DrRDr��Ds�Ds��Dt�Dt�RDu�Du�Dv�Dv��Dw�Dw��DwۅDy�
D�;�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aɴ9Aɲ-AɶFAɴ9AɮAȑhA�G�A�1A���AƮAƟ�AƉ7A�x�A�t�A�XA�I�A�C�A�=qA�9XA�;dA�?}A�E�A�I�A�^5A�`BA�XA�^5A�p�A�v�AƁAƋDAƕ�Aƙ�Aơ�AƩ�AƼjA��yA�9XAǲ-A���A�E�A�E�A�9XA�VA���Aǉ7A�  A��mA��/A�A�ƨA���A��A���A��A�l�A��Ať�A�l�A���A�M�AüjA� �A°!A���A�A��A�K�A�oA�x�A���A�$�A��A��#A��A� �A��A���A�l�A�I�A���A�;dA�Q�A��A�=qA��DA�Q�A��A��A���A���A���A���A�;dA�ffA�33A�/A�t�A�1A�A�{A���A�/A��A�Q�A�JA�`BA�
=A���A��\A���A|r�Av��Aq|�An�DAl-Ajv�Ah^5Aa&�A^��A]�PA\M�A[��A[?}AY"�AW�AUO�ATbAS"�AQl�AO&�AK�mAH�/AE`BAC��ABffA?��A<$�A:�DA8�`A7��A6  A5oA3dZA29XA1�PA0ffA.�A-XA,(�A*�A(�yA'�FA#�A"��A"�+A!��A!C�A!VA �`A �+A�Ax�AQ�A"�A�wAM�A��AbNA�Al�A�A�9A�HA��A��A�DA�A��A ��@�@�%@��H@�r�@���@�@�@�-@�  @��@���@���@ꟾ@�?}@�1'@��
@柾@噚@�  @��H@��T@���@ߕ�@��@�ff@ݩ�@ܬ@�1@ۍP@ڟ�@ڏ\@�V@�{@�@�%@�j@�\)@֟�@�{@���@�Q�@��
@���@��T@�&�@�z�@���@�K�@��H@Ο�@�n�@�M�@��@͙�@͑h@͉7@�V@���@�A�@��@���@˕�@�"�@�ȴ@�v�@��#@�O�@�&�@��@���@���@�Z@�  @�l�@�@ƸR@�n�@�$�@���@�hs@Ĭ@�Z@�A�@�1@þw@�l�@�ȴ@+@�@��@��/@���@���@���@�G�@��u@�"�@�v�@�5?@��@�@�p�@�?}@�/@�V@���@���@��@�9X@���@�S�@��R@�ff@�=q@�{@��@��h@�G�@�?}@�G�@�/@��@��j@��D@�A�@��@�1@�9X@��@�ƨ@��@��@���@�b@��@��@�K�@��!@��@�&�@��9@��@�9X@��@���@�"�@�@���@��j@�bN@� �@�1@��P@�@�ff@���@��@�9X@�t�@��@��@���@��!@�v�@�$�@��@�`B@��/@�Z@�9X@� �@�t�@�o@��!@��\@��+@�~�@�n�@�V@�M�@�=q@�=q@�$�@�$�@�J@���@�X@��@�V@��@��/@���@��9@��u@�j@�(�@��;@���@�|�@�"�@���@��@��@���@���@�ff@���@��^@��7@���@��@�bN@�Q�@�(�@�  @��m@��;@�t�@�+@��y@��R@�ff@��@��@���@�X@��@��`@�Ĝ@��@���@�;d@�o@�@��@��H@��@���@���@���@���@��@�Z@�  @�;d@�v�@��T@���@��h@�?}@�Ĝ@���@��@���@���@���@�t�@�;d@��H@�v�@���@��#@��h@�`B@�Ĝ@�I�@�(�@� �@�b@���@(@m��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aɴ9Aɲ-AɶFAɴ9AɮAȑhA�G�A�1A���AƮAƟ�AƉ7A�x�A�t�A�XA�I�A�C�A�=qA�9XA�;dA�?}A�E�A�I�A�^5A�`BA�XA�^5A�p�A�v�AƁAƋDAƕ�Aƙ�Aơ�AƩ�AƼjA��yA�9XAǲ-A���A�E�A�E�A�9XA�VA���Aǉ7A�  A��mA��/A�A�ƨA���A��A���A��A�l�A��Ať�A�l�A���A�M�AüjA� �A°!A���A�A��A�K�A�oA�x�A���A�$�A��A��#A��A� �A��A���A�l�A�I�A���A�;dA�Q�A��A�=qA��DA�Q�A��A��A���A���A���A���A�;dA�ffA�33A�/A�t�A�1A�A�{A���A�/A��A�Q�A�JA�`BA�
=A���A��\A���A|r�Av��Aq|�An�DAl-Ajv�Ah^5Aa&�A^��A]�PA\M�A[��A[?}AY"�AW�AUO�ATbAS"�AQl�AO&�AK�mAH�/AE`BAC��ABffA?��A<$�A:�DA8�`A7��A6  A5oA3dZA29XA1�PA0ffA.�A-XA,(�A*�A(�yA'�FA#�A"��A"�+A!��A!C�A!VA �`A �+A�Ax�AQ�A"�A�wAM�A��AbNA�Al�A�A�9A�HA��A��A�DA�A��A ��@�@�%@��H@�r�@���@�@�@�-@�  @��@���@���@ꟾ@�?}@�1'@��
@柾@噚@�  @��H@��T@���@ߕ�@��@�ff@ݩ�@ܬ@�1@ۍP@ڟ�@ڏ\@�V@�{@�@�%@�j@�\)@֟�@�{@���@�Q�@��
@���@��T@�&�@�z�@���@�K�@��H@Ο�@�n�@�M�@��@͙�@͑h@͉7@�V@���@�A�@��@���@˕�@�"�@�ȴ@�v�@��#@�O�@�&�@��@���@���@�Z@�  @�l�@�@ƸR@�n�@�$�@���@�hs@Ĭ@�Z@�A�@�1@þw@�l�@�ȴ@+@�@��@��/@���@���@���@�G�@��u@�"�@�v�@�5?@��@�@�p�@�?}@�/@�V@���@���@��@�9X@���@�S�@��R@�ff@�=q@�{@��@��h@�G�@�?}@�G�@�/@��@��j@��D@�A�@��@�1@�9X@��@�ƨ@��@��@���@�b@��@��@�K�@��!@��@�&�@��9@��@�9X@��@���@�"�@�@���@��j@�bN@� �@�1@��P@�@�ff@���@��@�9X@�t�@��@��@���@��!@�v�@�$�@��@�`B@��/@�Z@�9X@� �@�t�@�o@��!@��\@��+@�~�@�n�@�V@�M�@�=q@�=q@�$�@�$�@�J@���@�X@��@�V@��@��/@���@��9@��u@�j@�(�@��;@���@�|�@�"�@���@��@��@���@���@�ff@���@��^@��7@���@��@�bN@�Q�@�(�@�  @��m@��;@�t�@�+@��y@��R@�ff@��@��@���@�X@��@��`@�Ĝ@��@���@�;d@�o@�@��@��H@��@���@���@���@���@��@�Z@�  @�;d@�v�@��T@���@��h@�?}@�Ĝ@���@��@���@���@���@�t�@�;d@��H@�v�@���@��#@��h@�`B@�Ĝ@�I�@�(�@� �@�b@���@(@m��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�TB�TB�ZB�sB�B
B
t�B
|�B
y�B
u�B
s�B
p�B
m�B
l�B
iyB
hsB
hsB
iyB
iyB
l�B
o�B
r�B
v�B
�B
�B
�B
�B
�=B
�JB
�\B
�uB
��B
��B
��B
��B
��B
�B
ȴB
��B�B<jBE�BE�BB�B=qB9XB6FB7LB<jBK�BQ�B[#BcTBiyB}�B�%B|�B�B�\B��B��B�?B�}B��B�ZB��BVBoB�B!�B33BF�BI�BM�BH�B.B"�B#�B<jBK�BO�BR�BVBW
BT�BT�BN�BB�B6FB�BB��B�9B��Bu�BYB@�B/B �BhB
�B
�/B
��B
��B
XB
@�B
0!B
 �B
bB	��B	�B	ȴB	�B	��B	�JB	}�B	q�B	cTB	F�B	=qB	8RB	33B	/B	,B	!�B	�B	hB	DB	%B��B�B�HB��BÖB�qB�?B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��A�VB�wB�jB�XB�LB�FB�?B�LB�RB�dB�wBÖBǮBǮB��B��B��B��B��B��B�
B�)B�;B�BB�NB�ZB�fB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	
=B	VB	\B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	$�B	$�B	&�B	(�B	-B	.B	.B	0!B	1'B	2-B	33B	33B	8RB	=qB	?}B	@�B	B�B	G�B	I�B	L�B	O�B	P�B	P�B	P�B	R�B	VB	[#B	]/B	]/B	`BB	bNB	dZB	gmB	gmB	iyB	jB	l�B	n�B	n�B	q�B	u�B	w�B	�B	�B	�B	�+B	�DB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�9B	�LB	�RB	�RB	�LB	�LB	�RB	�^B	�jB	�jB	�dB	�^B	�RB	�FB	�?B	�LB	�FB	�FB	�FB	�RB	�XB	�^B	�dB	�qB	�qB	�wB	��B	��B	B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
DB
DB
JB
JB
VB
bB
bB
bB
oB
�B
#�B
./22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B�TB�TB�ZB�sB�B
B
t�B
|�B
y�B
u�B
s�B
p�B
m�B
l�B
iyB
hsB
hsB
iyB
iyB
l�B
o�B
r�B
v�B
�B
�B
�B
�B
�=B
�JB
�\B
�uB
��B
��B
��B
��B
��B
�B
ȴB
��B�B<jBE�BE�BB�B=qB9XB6FB7LB<jBK�BQ�B[#BcTBiyB}�B�%B|�B�B�\B��B��B�?B�}B��B�ZB��BVBoB�B!�B33BF�BI�BM�BH�B.B"�B#�B<jBK�BO�BR�BVBW
BT�BT�BN�BB�B6FB�BB��B�9B��Bu�BYB@�B/B �BhB
�B
�/B
��B
��B
XB
@�B
0!B
 �B
bB	��B	�B	ȴB	�B	��B	�JB	}�B	q�B	cTB	F�B	=qB	8RB	33B	/B	,B	!�B	�B	hB	DB	%B��B�B�HB��BÖB�qB�?B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��A�VB�wB�jB�XB�LB�FB�?B�LB�RB�dB�wBÖBǮBǮB��B��B��B��B��B��B�
B�)B�;B�BB�NB�ZB�fB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	
=B	VB	\B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	$�B	$�B	&�B	(�B	-B	.B	.B	0!B	1'B	2-B	33B	33B	8RB	=qB	?}B	@�B	B�B	G�B	I�B	L�B	O�B	P�B	P�B	P�B	R�B	VB	[#B	]/B	]/B	`BB	bNB	dZB	gmB	gmB	iyB	jB	l�B	n�B	n�B	q�B	u�B	w�B	�B	�B	�B	�+B	�DB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�9B	�LB	�RB	�RB	�LB	�LB	�RB	�^B	�jB	�jB	�dB	�^B	�RB	�FB	�?B	�LB	�FB	�FB	�FB	�RB	�XB	�^B	�dB	�qB	�qB	�wB	��B	��B	B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
DB
DB
JB
JB
VB
bB
bB
bB
oB
�B
#�B
./22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190549                              AO  ARCAADJP                                                                    20181005190549    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190549  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190549  QCF$                G�O�G�O�G�O�8000            