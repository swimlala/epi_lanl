CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:26Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191726  20181005191726  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���>cp
1   @�����ߦ@5�     �d|���S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,�C.�C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP�CR  CS�fCU�fCX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl�Cm�fCp  Cr�Ct�Cv�Cx  Cy�fC{�fC~  C�  C�  C��3C�  C��3C�  C�  C��3C�  C�  C��3C��C��C��3C��fC�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C��3C��C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C��3C�  C��C��C��C��C��C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� DfD� D	  D	� D
  D
� D  D�fD  D� DfD� D  D�fDfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��D� DfD�fD  D� D  D� D��Dy�D  Dy�D��Dy�D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.fD.� D/  D/�fD0  D0� D1fD1� D2  D2y�D3  D3�fD4fD4�fD5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;y�D<  D<� D=  D=� D>  D>� D>��D?y�D@  D@� DA  DA�fDA��DBy�DC  DCy�DC��DDy�DD��DE� DF  DF� DG  DG� DH  DHy�DH��DI� DJfDJ�fDK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQy�DR  DR� DS  DS�fDT  DTy�DT��DUy�DV  DV� DW  DW� DW��DXy�DY  DY� DZ  DZy�D[  D[�fD\fD\�fD]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Db��Dcy�Dc��Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Diy�Di��Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Dr��Dsy�Dt  Dt�fDufDu�fDv  Dv� Dw  Dw� Dw��Dy�qD�PR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�33A�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B�L�B�L�B��B��B��gB��B��B��B��gB��B��B��B��B�L�B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C&gC �C"�C$�C&�C(�C*�C,&gC.&gC0�C2�C4&gC6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CI�3CL�CN�CP&gCR�CS�3CU�3CX�CZ�C\�C^�C`�Cb�Cc�3Cf�Ch�Cj�Cl&gCm�3Cp�Cr&gCt&gCv&gCx�Cy�3C{�3C~�C�fC�fC���C�fC���C�fC�fC���C�fC�fC���C�3C�3C���C���C�fC�fC�fC�fC�fC�fC�3C�fC���C�fC�fC���C�fC�fC�fC���C�3C�fC�fC�fC�fC�3C�fC���C�fC�fC���C���C�fC�fC�fC�fC�3C�fC�fC�3C�fC�fC�fC�fC���C�fC�3C�3C���C�fC�  C�3C�3C�3C�3C�fC���C���C�fC�fC�fC�3C�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�3C�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�3C�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D|�D3D��D3D�3D3D�3D	�D�3D	3D	�3D
3D
�3D3D��D3D�3D	�D�3D3D��D	�D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D��D�3D3D�3D��D�3D	�D��D3D�3D3D�3D��D|�D3D|�D��D|�D��D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-��D.	�D.�3D/3D/��D03D0�3D1	�D1�3D23D2|�D33D3��D4	�D4��D5	�D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D:��D;|�D<3D<�3D=3D=�3D>3D>�3D>��D?|�D@3D@�3DA3DA��DA��DB|�DC3DC|�DC��DD|�DD��DE�3DF3DF�3DG3DG�3DH3DH|�DH��DI�3DJ	�DJ��DK3DK|�DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DP��DQ|�DR3DR�3DS3DS��DT3DT|�DT��DU|�DV3DV�3DW3DW�3DW��DX|�DY3DY�3DZ3DZ|�D[3D[��D\	�D\��D]3D]�3D^3D^�3D^��D_�3D`3D`�3Da3Da�3Db3Db�3Db��Dc|�Dc��Dd�3De3De�3Df3Df|�Dg3Dg�3Dh3Dh�3Di3Di|�Di��Dj�3Dk3Dk�3Dl3Dl��Dm3Dm�3Dn3Dn|�Do3Do�3Dp3Dp�3Dp��Dq�3Dr3Dr�3Dr��Ds|�Dt3Dt��Du	�Du��Dv3Dv�3Dw3Dw�3Dw� Dy��D�Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�1A�A�  A�%A�
=A�1A�
=A�
=A�oA�oA�{A�{A��A��A��A��A��A��A� �A� �A��A��A��A��A��yA�dZA���A׸RA�ȴA���A���A��;A���A��Aև+A�5?A�jA�%A�+A���A���A�-Aç�A�;dA��^A�ĜA�7LA�A�+A��+A��#A���A��A�A�A�K�A�?}A��-A��A��/A��A�?}A���A�I�A��A�`BA�7LA��A��A�=qA���A��`A�dZA��/A� �A��#A�"�A���A��
A��A�7LA���A�`BA�x�A��yA�bNA�z�A���A��hA��A��A��A�%A���A��A��jA��DA�Q�A�7LA��yA�K�A�~�A�;dA�ZA��^A�jA�ĜA~�/A|1A{%AxA�At5?Ar(�Am��AlVAk��Akp�AkG�Ak�AjM�Af��Ad9XA_
=A^ZA]`BA[�TAXffAVZATI�AR�AO�AK�AI��AIS�AH1'AG�AF�AE�AEl�AD��AA��A>�A<�RA;�hA:�/A9��A8ȴA7S�A5+A4�A3��A2bNA1%A.�DA,��A+�mA*�RA)�FA)A'�mA&�`A&1'A%+A#7LA!�A!A��An�At�AJAl�A �A��AI�A�A�A��A��A��AffA1A�A�A%A�jA��A;dA
�A
jA	hsA��A��A��A�A{A�uA�PAx�A33A{AE�A/A%A �`A ȴA ��A {@��@�^5@�n�@�&�@���@�l�@�n�@���@��9@��j@���@��@�M�@�X@�j@�ƨ@��@�ƨ@�o@��@��;@�R@�v�@�M�@���@�@��@�A�@�$�@۶F@��@�Z@�\)@֗�@��@Չ7@��@� �@ӝ�@�\)@�n�@Ь@��@Ͼw@�
=@�M�@��`@ˍP@�ȴ@ɉ7@Ǖ�@�J@�hs@�`B@�&�@ļj@�A�@�"�@�x�@�
=@�G�@��j@��9@���@�Q�@�S�@�{@�V@���@���@��u@��@�1@� �@���@���@���@��@��j@�1@�S�@�"�@���@��H@��R@�$�@���@��@��+@�-@�O�@���@��/@�9X@��P@��j@��R@�|�@�ff@�/@���@��@���@� �@��P@��R@���@��`@���@��u@�j@��@�l�@�l�@�
=@�%@�dZ@�@��`@�V@�V@�%@���@��9@�j@���@��
@�"�@�"�@�b@�ff@�G�@��D@�\)@�@��7@���@���@�@�@�M�@�n�@��@���@�^5@�$�@�{@�@���@��-@��-@���@�hs@�&�@���@��/@���@�%@�V@�V@��@��j@��u@�A�@�A�@�1@���@���@�~�@�n�@�^5@�M�@�J@���@��h@��@�X@�/@��@�I�@�  @���@�t�@�S�@�
=@���@��+@��\@��\@�M�@��^@���@�9X@��P@�;d@��P@�S�@��@���@�5?@��^@�hs@��@��@�Z@�Q�@�I�@�1'@��m@��F@�;d@�@��@�33@�t�@��@�G�@�7L@��@��9@���@�b@�l�@���@��\@�{@�J@�{@�M�@���@�C�@��9@��#@�G�@��@�1'@���@�ƨ@���@��@��w@��@�9X@�Q�@� �@�l�@�o@�J@�X@�V@���@�j@�1'@�  @���@��@�|�@�t�@�|�@�\)@�C�@�;d@�"�@�l�@��F@��w@��@��@��m@�ƨ@��P@�S�@�;d@�{@�=q@�^5@�-@�-@�{@��h@�G�@��@���@���@�z�@��@��@��@o@O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A�1A�A�  A�%A�
=A�1A�
=A�
=A�oA�oA�{A�{A��A��A��A��A��A��A� �A� �A��A��A��A��A��yA�dZA���A׸RA�ȴA���A���A��;A���A��Aև+A�5?A�jA�%A�+A���A���A�-Aç�A�;dA��^A�ĜA�7LA�A�+A��+A��#A���A��A�A�A�K�A�?}A��-A��A��/A��A�?}A���A�I�A��A�`BA�7LA��A��A�=qA���A��`A�dZA��/A� �A��#A�"�A���A��
A��A�7LA���A�`BA�x�A��yA�bNA�z�A���A��hA��A��A��A�%A���A��A��jA��DA�Q�A�7LA��yA�K�A�~�A�;dA�ZA��^A�jA�ĜA~�/A|1A{%AxA�At5?Ar(�Am��AlVAk��Akp�AkG�Ak�AjM�Af��Ad9XA_
=A^ZA]`BA[�TAXffAVZATI�AR�AO�AK�AI��AIS�AH1'AG�AF�AE�AEl�AD��AA��A>�A<�RA;�hA:�/A9��A8ȴA7S�A5+A4�A3��A2bNA1%A.�DA,��A+�mA*�RA)�FA)A'�mA&�`A&1'A%+A#7LA!�A!A��An�At�AJAl�A �A��AI�A�A�A��A��A��AffA1A�A�A%A�jA��A;dA
�A
jA	hsA��A��A��A�A{A�uA�PAx�A33A{AE�A/A%A �`A ȴA ��A {@��@�^5@�n�@�&�@���@�l�@�n�@���@��9@��j@���@��@�M�@�X@�j@�ƨ@��@�ƨ@�o@��@��;@�R@�v�@�M�@���@�@��@�A�@�$�@۶F@��@�Z@�\)@֗�@��@Չ7@��@� �@ӝ�@�\)@�n�@Ь@��@Ͼw@�
=@�M�@��`@ˍP@�ȴ@ɉ7@Ǖ�@�J@�hs@�`B@�&�@ļj@�A�@�"�@�x�@�
=@�G�@��j@��9@���@�Q�@�S�@�{@�V@���@���@��u@��@�1@� �@���@���@���@��@��j@�1@�S�@�"�@���@��H@��R@�$�@���@��@��+@�-@�O�@���@��/@�9X@��P@��j@��R@�|�@�ff@�/@���@��@���@� �@��P@��R@���@��`@���@��u@�j@��@�l�@�l�@�
=@�%@�dZ@�@��`@�V@�V@�%@���@��9@�j@���@��
@�"�@�"�@�b@�ff@�G�@��D@�\)@�@��7@���@���@�@�@�M�@�n�@��@���@�^5@�$�@�{@�@���@��-@��-@���@�hs@�&�@���@��/@���@�%@�V@�V@��@��j@��u@�A�@�A�@�1@���@���@�~�@�n�@�^5@�M�@�J@���@��h@��@�X@�/@��@�I�@�  @���@�t�@�S�@�
=@���@��+@��\@��\@�M�@��^@���@�9X@��P@�;d@��P@�S�@��@���@�5?@��^@�hs@��@��@�Z@�Q�@�I�@�1'@��m@��F@�;d@�@��@�33@�t�@��@�G�@�7L@��@��9@���@�b@�l�@���@��\@�{@�J@�{@�M�@���@�C�@��9@��#@�G�@��@�1'@���@�ƨ@���@��@��w@��@�9X@�Q�@� �@�l�@�o@�J@�X@�V@���@�j@�1'@�  @���@��@�|�@�t�@�|�@�\)@�C�@�;d@�"�@�l�@��F@��w@��@��@��m@�ƨ@��P@�S�@�;d@�{@�=q@�^5@�-@�-@�{@��h@�G�@��@���@���@�z�@��@��@��@o@O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B
��BB1B
=BVB&�Bk�B�jB�`B�B�B�B��B	7B1'BC�BYB`BBhsB�VB��B��B�B�wB��B�#B�/B�5B�;B�BB�BB�HB�NB�HB�BB�5B�;B�5B�)B��BɺB�}B�LB�B� Bv�Br�Bn�Bn�Bv�B�By�Bs�Bl�BaHBQ�BE�B=qB'�B{B
=B	7BB��B��B�yB��B�'B�bBv�BgmB`BB\)BN�BD�B-B�BB
�B
�B
�
B
ȴB
�XB
��B
��B
�{B
�1B
gmB
Q�B
G�B
5?B
�B
\B	�B	�mB	�NB	�BB	�5B	�)B	��B	�dB	�B	�PB	�B	|�B	q�B	aHB	VB	I�B	=qB	-B	�B	JB		7B	B��B��B�B�B�yB�BB�B��B��BȴBǮBĜBBB��B��BŢB��B�RB�?B�-B�B��B��B��B��B��B��B�uB�bB�VB�JB�1B�B�Bz�Bu�Bs�Bq�Bm�BhsBcTBaHB`BB_;B]/B[#BXBVBT�BR�BQ�BQ�BO�BL�BO�BbNBffBl�Bz�Bs�Bw�B�B�B� Bu�Bp�Bo�Bo�Bn�Bm�Bm�Bk�BjBl�Bm�Bk�BjBjBo�Bu�Bx�Bx�Bv�Br�Bq�Bo�Bn�Bk�BjBiyBe`BcTBdZBdZBe`BdZBbNB_;B[#BZBZBZBYBYBXBXBXBXBXBW
BW
BW
BZB[#B[#B\)B^5BaHBk�Bp�Bp�Bw�B{�B{�B|�B|�B|�B|�B{�Bx�Bv�Bt�Bu�B{�B�B�B�B�B�B�B� B~�B}�B}�B� B�B� B}�B|�By�By�Bz�Bz�Bz�Bz�Bz�Bz�By�B~�B�B�B�\B��B��B��B��B�dB��B��B�B��B��B�B�
B�BB��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	%B	%B	+B	+B	+B	+B	+B	%B	%B	1B	\B	
=B	B	B	B	  B	B	B	1B	JB	bB	uB	�B	�B	 �B	#�B	%�B	&�B	)�B	/B	/B	0!B	2-B	49B	49B	6FB	8RB	<jB	>wB	?}B	?}B	?}B	A�B	E�B	G�B	O�B	VB	VB	XB	ZB	_;B	aHB	bNB	e`B	hsB	jB	k�B	n�B	q�B	s�B	w�B	x�B	z�B	z�B	{�B	|�B	� B	�B	�B	�B	�%B	�+B	�+B	�%B	�+B	�=B	�hB	��B	�{B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ŢB	ĜB	ŢB	ǮB	ɺB	��B	��B	�B	�5B	�`B	�`B	�`B	�ZB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
1B
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
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
��B
��BB1B
=BVB&�Bk�B�jB�`B�B�B�B��B	7B1'BC�BYB`BBhsB�VB��B��B�B�wB��B�#B�/B�5B�;B�BB�BB�HB�NB�HB�BB�5B�;B�5B�)B��BɺB�}B�LB�B� Bv�Br�Bn�Bn�Bv�B�By�Bs�Bl�BaHBQ�BE�B=qB'�B{B
=B	7BB��B��B�yB��B�'B�bBv�BgmB`BB\)BN�BD�B-B�BB
�B
�B
�
B
ȴB
�XB
��B
��B
�{B
�1B
gmB
Q�B
G�B
5?B
�B
\B	�B	�mB	�NB	�BB	�5B	�)B	��B	�dB	�B	�PB	�B	|�B	q�B	aHB	VB	I�B	=qB	-B	�B	JB		7B	B��B��B�B�B�yB�BB�B��B��BȴBǮBĜBBB��B��BŢB��B�RB�?B�-B�B��B��B��B��B��B��B�uB�bB�VB�JB�1B�B�Bz�Bu�Bs�Bq�Bm�BhsBcTBaHB`BB_;B]/B[#BXBVBT�BR�BQ�BQ�BO�BL�BO�BbNBffBl�Bz�Bs�Bw�B�B�B� Bu�Bp�Bo�Bo�Bn�Bm�Bm�Bk�BjBl�Bm�Bk�BjBjBo�Bu�Bx�Bx�Bv�Br�Bq�Bo�Bn�Bk�BjBiyBe`BcTBdZBdZBe`BdZBbNB_;B[#BZBZBZBYBYBXBXBXBXBXBW
BW
BW
BZB[#B[#B\)B^5BaHBk�Bp�Bp�Bw�B{�B{�B|�B|�B|�B|�B{�Bx�Bv�Bt�Bu�B{�B�B�B�B�B�B�B� B~�B}�B}�B� B�B� B}�B|�By�By�Bz�Bz�Bz�Bz�Bz�Bz�By�B~�B�B�B�\B��B��B��B��B�dB��B��B�B��B��B�B�
B�BB��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	%B	%B	+B	+B	+B	+B	+B	%B	%B	1B	\B	
=B	B	B	B	  B	B	B	1B	JB	bB	uB	�B	�B	 �B	#�B	%�B	&�B	)�B	/B	/B	0!B	2-B	49B	49B	6FB	8RB	<jB	>wB	?}B	?}B	?}B	A�B	E�B	G�B	O�B	VB	VB	XB	ZB	_;B	aHB	bNB	e`B	hsB	jB	k�B	n�B	q�B	s�B	w�B	x�B	z�B	z�B	{�B	|�B	� B	�B	�B	�B	�%B	�+B	�+B	�%B	�+B	�=B	�hB	��B	�{B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ŢB	ĜB	ŢB	ǮB	ɺB	��B	��B	�B	�5B	�`B	�`B	�`B	�ZB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
1B
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191726                              AO  ARCAADJP                                                                    20181005191726    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191726  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191726  QCF$                G�O�G�O�G�O�8000            