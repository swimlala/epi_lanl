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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190509  20181005190509  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ת��Ӳ�1   @ת�F<@2�(�\�c�Q��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A>ffA`  A~ffA�  A���A�  A�33A�  A�33A�  B   BffB  B  B��B(  B0  B8  B@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C�  C��C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  D   D � D  D�fD  D� D  D� DfD� D  Dy�D��Dy�D��D� DfD� D	  D	� D
  D
� DfD� D  D� D  D�fDfD� D  D�fDfD� D��D� D  D� D  D�fD��Dy�D  Dy�D��D� D  D� DfD� D��Dy�D  D� DfD�fD  D� DfD� D  D� DfD� D��D � D!fD!� D!��D"y�D#  D#� D#��D$y�D%  D%� D%��D&� D'  D'y�D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2� D3  D3� D4fD4� D5  D5� D6  D6y�D7  D7� D8  D8y�D8��D9� D:fD:� D:��D;� D<  D<y�D<��D=� D>  D>� D?fD?� D@  D@y�DA  DA� DB  DB�fDC  DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DK  DK� DK��DL� DM  DM� DNfDN�fDOfDO� DO��DPy�DQ  DQ�fDR  DRy�DR��DS� DT  DTy�DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Dby�Db��Dc� Dc��Dd� DefDe� De��Dfy�Df��Dg�fDhfDh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dq��Dry�Dr��Dsy�Ds��Dt� Du  Du� Dv  Dv�fDwfDw�fDy}qD�'
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���A ��A ��A?33A`��A33A�ffA�33A�ffA���A�ffAߙ�A�ffB 33B��B33B33B��B(33B033B833B@33BH33BP33BX��B`33Bg��Bp33Bx33B��B��B��B��B��B��B��B�L�B�L�B��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C-�3C0�C2&gC4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CK�3CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd&gCf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�3C�3C�3C�fC�fC�fC�fC�3C�fC�fC�fC�3C�3C�fC���C�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�3C�fC���C�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C�3C�3C�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C�3C�3C�3C�3C�3C�fC�fC�fC�fC�3C�3C�fC���C���C���C���C���C�fC�fC�fC�fC�fC�fC�fC�fC���C���C���C���C�fC�fD 3D �3D3D��D3D�3D3D�3D	�D�3D3D|�D��D|�D��D�3D	�D�3D	3D	�3D
3D
�3D	�D�3D3D�3D3D��D	�D�3D3D��D	�D�3D��D�3D3D�3D3D��D��D|�D3D|�D��D�3D3D�3D	�D�3D��D|�D3D�3D	�D��D3D�3D	�D�3D3D�3D	�D�3D��D �3D!	�D!�3D!��D"|�D#3D#�3D#��D$|�D%3D%�3D%��D&�3D'3D'|�D(3D(�3D)3D)�3D*3D*|�D+3D+�3D,3D,�3D-3D-�3D.3D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2�3D33D3�3D4	�D4�3D53D5�3D63D6|�D73D7�3D83D8|�D8��D9�3D:	�D:�3D:��D;�3D<3D<|�D<��D=�3D>3D>�3D?	�D?�3D@3D@|�DA3DA�3DB3DB��DC3DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DK3DK�3DK��DL�3DM3DM�3DN	�DN��DO	�DO�3DO��DP|�DQ3DQ��DR3DR|�DR��DS�3DT3DT|�DU3DU�3DV3DV��DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D]��D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db|�Db��Dc�3Dc��Dd�3De	�De�3De��Df|�Df��Dg��Dh	�Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dk��Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Do��Dp�3Dq3Dq�3Dq��Dr|�Dr��Ds|�Ds��Dt�3Du3Du�3Dv3Dv��Dw	�Dw��Dy��D�(�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A΋DA�`BA�G�A�C�A�9XA�?}A�33A�/A�-A��A�oA�JA�
=A�oA��A�$�A� �A� �A�"�A�"�A�"�A�{A��A͸RA�ffA���Ạ�ÁA�dZA�ZA�Q�A�K�A�+A��A��A��/A��`A��A��A�A˥�AˋDA�~�A�p�A�A�A�r�Aɛ�Aȥ�A�K�Aǉ7A�$�A��A�ĜA�M�A�M�A���Aá�A�n�A��mAA�ffA�7LA�ȴA�=qA��A�33A��A�;dA�oA���A��A���A��A��A���A�A���A��^A�1A��^A��hA�`BA��jA�dZA��wA���A��A�M�A��PA��A��DA��
A��A�7LA��-A�O�A�n�A��A�hsA�v�A��PA��A�A�A�  A�"�A�9XA��wA�K�A�p�A���A��wA�~�A��PA�O�A�;dA�/A�r�A�I�A��9A��A`BA}�PAx�As��Ao�AjM�Ag�AeK�Aa�-A`��A_��A^{AY��AW�AVA�AT�/ASS�AP�\AL��AH�AGO�AF5?AD��A@�+A=/A:A�A7�PA4��A333A2$�A0ZA.��A,��A*ȴA)��A)��A)��A)��A)p�A(��A'�-A%�PA#��A#�FA#�A#`BA#G�A#C�A#33A"I�A �uA/A1AjAx�AI�A�-A&�A�uA(�A�A�;A��A�A�9AZA�mAO�A�\A�/A?}A��AffA=qA5?A5?A=qAE�AA�A5?A��AC�A
��A
~�A
9XA	��AI�A�A�FA��A�`A��A �AbAA1A�-A�jA�TA��A�A ��A jA �@�E�@�bN@�  @�ƨ@�5?@���@���@���@���@��j@� �@���@�n�@���@��@�t�@�G�@�b@��@�1@�h@�1'@�S�@�+@��@�%@�w@�M�@�7@���@���@ݺ^@��
@�=q@�I�@�=q@�/@ԛ�@��@���@�hs@ϝ�@�@��@���@ɡ�@�Z@���@�o@��T@š�@őh@�G�@ě�@�dZ@�
=@��y@�M�@��@�`B@��j@�1@��P@�o@��+@�M�@�-@�{@��@�@�`B@��@��D@�(�@�1@��@���@���@�l�@��@��+@���@���@�z�@��@�~�@���@�  @��@���@��@��H@�{@�Ĝ@��@�+@�v�@�=q@��^@�Ĝ@�E�@��!@�@��#@���@��/@��@��+@�X@�J@�"�@�ȴ@��-@�G�@�`B@��@�5?@�v�@�ff@�J@���@�hs@��@��`@�%@�j@�Q�@�(�@��P@��+@���@�@�@��h@�hs@�/@��m@�l�@�K�@��@��H@�^5@��+@���@���@���@��\@��+@�v�@�^5@�=q@�/@�j@�bN@�Ĝ@�Ĝ@� �@��w@�33@��R@��+@��\@�5?@�X@��;@�C�@�o@��@���@��@���@�l�@�;d@�\)@���@�5?@�J@��#@�@��-@��^@�@��^@��-@���@���@�x�@�V@���@�A�@�I�@�33@�"�@���@��@��h@�`B@��D@��@���@�dZ@�@�/@�  @�l�@�dZ@�\)@�K�@�;d@�o@��@�n�@��7@�`B@�`B@�`B@�hs@�`B@�X@�`B@���@�@��#@�/@��
@��w@��@���@�|�@�l�@���@��@���@�t�@�\)@�;d@��@�@�ȴ@�~�@�5?@���@���@��^@��7@�`B@���@�9X@�  @�ƨ@���@�l�@�;d@��H@���@��@��-@�V@�r�@�9X@��@���@��j@�9X@�  @���@��@���@�ƨ@���@�q@tĜ@f�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A΋DA�`BA�G�A�C�A�9XA�?}A�33A�/A�-A��A�oA�JA�
=A�oA��A�$�A� �A� �A�"�A�"�A�"�A�{A��A͸RA�ffA���Ạ�ÁA�dZA�ZA�Q�A�K�A�+A��A��A��/A��`A��A��A�A˥�AˋDA�~�A�p�A�A�A�r�Aɛ�Aȥ�A�K�Aǉ7A�$�A��A�ĜA�M�A�M�A���Aá�A�n�A��mAA�ffA�7LA�ȴA�=qA��A�33A��A�;dA�oA���A��A���A��A��A���A�A���A��^A�1A��^A��hA�`BA��jA�dZA��wA���A��A�M�A��PA��A��DA��
A��A�7LA��-A�O�A�n�A��A�hsA�v�A��PA��A�A�A�  A�"�A�9XA��wA�K�A�p�A���A��wA�~�A��PA�O�A�;dA�/A�r�A�I�A��9A��A`BA}�PAx�As��Ao�AjM�Ag�AeK�Aa�-A`��A_��A^{AY��AW�AVA�AT�/ASS�AP�\AL��AH�AGO�AF5?AD��A@�+A=/A:A�A7�PA4��A333A2$�A0ZA.��A,��A*ȴA)��A)��A)��A)��A)p�A(��A'�-A%�PA#��A#�FA#�A#`BA#G�A#C�A#33A"I�A �uA/A1AjAx�AI�A�-A&�A�uA(�A�A�;A��A�A�9AZA�mAO�A�\A�/A?}A��AffA=qA5?A5?A=qAE�AA�A5?A��AC�A
��A
~�A
9XA	��AI�A�A�FA��A�`A��A �AbAA1A�-A�jA�TA��A�A ��A jA �@�E�@�bN@�  @�ƨ@�5?@���@���@���@���@��j@� �@���@�n�@���@��@�t�@�G�@�b@��@�1@�h@�1'@�S�@�+@��@�%@�w@�M�@�7@���@���@ݺ^@��
@�=q@�I�@�=q@�/@ԛ�@��@���@�hs@ϝ�@�@��@���@ɡ�@�Z@���@�o@��T@š�@őh@�G�@ě�@�dZ@�
=@��y@�M�@��@�`B@��j@�1@��P@�o@��+@�M�@�-@�{@��@�@�`B@��@��D@�(�@�1@��@���@���@�l�@��@��+@���@���@�z�@��@�~�@���@�  @��@���@��@��H@�{@�Ĝ@��@�+@�v�@�=q@��^@�Ĝ@�E�@��!@�@��#@���@��/@��@��+@�X@�J@�"�@�ȴ@��-@�G�@�`B@��@�5?@�v�@�ff@�J@���@�hs@��@��`@�%@�j@�Q�@�(�@��P@��+@���@�@�@��h@�hs@�/@��m@�l�@�K�@��@��H@�^5@��+@���@���@���@��\@��+@�v�@�^5@�=q@�/@�j@�bN@�Ĝ@�Ĝ@� �@��w@�33@��R@��+@��\@�5?@�X@��;@�C�@�o@��@���@��@���@�l�@�;d@�\)@���@�5?@�J@��#@�@��-@��^@�@��^@��-@���@���@�x�@�V@���@�A�@�I�@�33@�"�@���@��@��h@�`B@��D@��@���@�dZ@�@�/@�  @�l�@�dZ@�\)@�K�@�;d@�o@��@�n�@��7@�`B@�`B@�`B@�hs@�`B@�X@�`B@���@�@��#@�/@��
@��w@��@���@�|�@�l�@���@��@���@�t�@�\)@�;d@��@�@�ȴ@�~�@�5?@���@���@��^@��7@�`B@���@�9X@�  @�ƨ@���@�l�@�;d@��H@���@��@��-@�V@�r�@�9X@��@���@��j@�9X@�  @���@��@���@�ƨ@���@�q@tĜ@f�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
$�B
;dB
B�B
F�B
H�B
K�B
L�B
K�B
M�B
T�B
[#B
`BB
gmB
p�B
�=B
��B
��B
�B
�B
�!B
�?B
�dB
��B
ɺB
��B
�HB
�fB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B  B  B
��B
��B
��B
��B
��BB!�B<jBVB`BBx�B��B��B��B�B�LB��BǮB��B�B�NB�TB�fB�B��BB+B	7BhBoBoBoBhBbBbB�B�B%�B1'B:^B;dB<jB>wBE�BG�BP�BVBVBT�BP�BM�BH�B@�B<jB33B/B,B%�B�B�BPB�BB��B�VB|�B`BB=qB'�B�BB
�`B
ǮB
�FB
�hB
}�B
o�B
H�B
8RB
.B
�B
bB
  B	�/B	�}B	��B	�+B	u�B	hsB	VB	O�B	H�B	>wB	)�B	�B	�B	bB	+B��B�B�ZB�HB�/B�BȴB�wB�?B�!B�B�B�'B�XB�'B�3B�LBĜB�#B�5B�;B�;B�/B�B�
B�B�#B�/B�;B�HB�HB�BB�HB�ZB�sB�B�B�B�B�yB�mB�`B�B�`B��B�B�fB�`B�ZB�TB�HB�5B�;B�ZB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�fB�NB�5B�/B�/B�/B�/B�)B�#B�B�
B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��BɺBĜB��B�jB�9B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�jB��BBȴB��B��B��B��B�B�B�;B�HB�TB�fB�B�B�B�B�B��B��B��B��B��B	  B	B	1B	JB	VB	VB	PB	PB	VB	VB	bB	uB	�B	�B	�B	 �B	%�B	(�B	)�B	)�B	)�B	-B	2-B	7LB	9XB	:^B	7LB	49B	2-B	2-B	:^B	H�B	N�B	K�B	F�B	D�B	H�B	N�B	R�B	VB	YB	ffB	jB	l�B	m�B	e`B	\)B	[#B	\)B	_;B	hsB	r�B	t�B	t�B	v�B	z�B	�B	�DB	�VB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	�!B	�!B	�!B	�!B	�FB	�^B	�jB	�jB	�jB	�jB	�qB	�qB	�jB	�jB	�qB	�qB	��B	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�HB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B
  B
  B	��B
  B
B
%B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
VB
\B
bB
oB
oB
uB
{B
{B
{B
�B
�B
{B
uB
oB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
0o22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
$�B
;dB
B�B
F�B
H�B
K�B
L�B
K�B
M�B
T�B
[#B
`BB
gmB
p�B
�=B
��B
��B
�B
�B
�!B
�?B
�dB
��B
ɺB
��B
�HB
�fB
�yB
�B
�B
�B
��B
��B
��B
��B
��B
��B  B  B
��B
��B
��B
��B
��BB!�B<jBVB`BBx�B��B��B��B�B�LB��BǮB��B�B�NB�TB�fB�B��BB+B	7BhBoBoBoBhBbBbB�B�B%�B1'B:^B;dB<jB>wBE�BG�BP�BVBVBT�BP�BM�BH�B@�B<jB33B/B,B%�B�B�BPB�BB��B�VB|�B`BB=qB'�B�BB
�`B
ǮB
�FB
�hB
}�B
o�B
H�B
8RB
.B
�B
bB
  B	�/B	�}B	��B	�+B	u�B	hsB	VB	O�B	H�B	>wB	)�B	�B	�B	bB	+B��B�B�ZB�HB�/B�BȴB�wB�?B�!B�B�B�'B�XB�'B�3B�LBĜB�#B�5B�;B�;B�/B�B�
B�B�#B�/B�;B�HB�HB�BB�HB�ZB�sB�B�B�B�B�yB�mB�`B�B�`B��B�B�fB�`B�ZB�TB�HB�5B�;B�ZB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�fB�NB�5B�/B�/B�/B�/B�)B�#B�B�
B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��BɺBĜB��B�jB�9B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�jB��BBȴB��B��B��B��B�B�B�;B�HB�TB�fB�B�B�B�B�B��B��B��B��B��B	  B	B	1B	JB	VB	VB	PB	PB	VB	VB	bB	uB	�B	�B	�B	 �B	%�B	(�B	)�B	)�B	)�B	-B	2-B	7LB	9XB	:^B	7LB	49B	2-B	2-B	:^B	H�B	N�B	K�B	F�B	D�B	H�B	N�B	R�B	VB	YB	ffB	jB	l�B	m�B	e`B	\)B	[#B	\)B	_;B	hsB	r�B	t�B	t�B	v�B	z�B	�B	�DB	�VB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	�!B	�!B	�!B	�!B	�FB	�^B	�jB	�jB	�jB	�jB	�qB	�qB	�jB	�jB	�qB	�qB	��B	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�HB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B
  B
  B	��B
  B
B
%B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
VB
\B
bB
oB
oB
uB
{B
{B
{B
�B
�B
{B
uB
oB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
0o22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190509                              AO  ARCAADJP                                                                    20181005190509    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190509  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190509  QCF$                G�O�G�O�G�O�8000            