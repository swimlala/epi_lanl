CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-13T00:35:17Z creation;2017-09-13T00:35:20Z conversion to V3.1;2019-12-19T07:57:43Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170913003517  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_159                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�%�:g� 1   @�%���J @4o��Ft�d��Z���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D���D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @;�@���@���A{A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Cs�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D���D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D�)D�:�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�hsA�^5A�M�A�?}A�5?A�1A�FA�A�A�JA��mA��#A���A�wA�FA�A���A���A��A�|�A�n�A�ZA��A�bA� �A�ĜA��A�I�A�A�ƨA��A�dZA�l�A֬A��;AӼjA�&�AЋDAΑhA�(�Aɲ-A�AǅAŇ+A���A� �A�ZA�-A���A�9XA��A��yA�dZA���A���A�?}A�z�A���A��9A���A���A���A�`BA�n�A���A���A�oA�A�A��-A�1'A�ĜA� �A���A�%A��^A�G�A��RA���A��
A��RA�1A�-A�p�A�/A��A��/A���A�z�A�1A�%A�A�JA��FA��A�-A��9A�
=A�bA�bNA�VA�%A�E�A���A���A�JA�?}A���A���A��+A�hsA��;A�/A��9A�A��A�ȴA�;dA~�DA{�Az{Av�9As�-Ar�yApVAm`BAj1'Af��AcdZAa�Aa;dA_��A^ �A\�`A[7LAZ1'AY��AXbAV�\AS�AR��ARbAQ�AQS�AO�mANI�AL�+AJ5?AH�yAG/AE`BAD��ADZAC�AAt�A?�TA>��A=C�A<A;VA:ffA9�wA8jA6-A3�mA3hsA2��A1�;A0�jA/��A-l�A,ffA+�A+
=A*9XA(��A(n�A'+A&�+A&1'A%��A%�A$�A#7LA!��A �HA�
A��A5?AG�AM�A�TA��A�\A��A��A�+AG�Ap�A��A�hAz�A\)A|�A
 �A��AM�A�AffA��A`BA�A��A��A�HAjA1A�A�
A ��@�+@�=q@�@�z�@�|�@�33@���@���@�O�@���@�ƨ@���@�G�@@�`B@�w@��@��@�1@�@�@�p�@��@�S�@�+@އ+@���@���@�K�@���@��#@�`B@�l�@��@���@�5?@�?}@���@���@���@̛�@��@�S�@ʰ!@���@�`B@�&�@ȣ�@�\)@��T@ź^@���@�Q�@ÍP@+@�E�@�{@��7@�X@��j@��w@�"�@�v�@�5?@�-@�{@�{@��T@�hs@��F@�o@�@��@��@��D@���@�l�@�@���@�n�@��@��^@���@�(�@�ƨ@��P@��@���@��#@�@�x�@�G�@�%@�(�@���@��P@�\)@�S�@�S�@�K�@�;d@�C�@��@��!@�^5@�J@���@��7@�%@���@�b@��F@���@���@�1'@��/@��@��@��@�j@�b@���@��@��;@��
@���@��F@���@��@�dZ@�K�@��!@���@���@���@���@�&�@� �@��@���@���@�C�@�"�@�
=@�
=@���@���@�=q@�-@�@���@�/@��@�I�@���@��m@���@�"�@���@��+@�ff@���@�`B@�O�@�?}@���@�j@��@��w@��@�+@��@��R@��R@���@�$�@�@��@�J@��@�@��h@�hs@�&�@�V@���@���@�Ĝ@��@�bN@�b@��w@���@�|�@�\)@��@���@�v�@�@�p�@�7L@�V@���@���@��/@��j@�j@��;@���@�dZ@�;d@�
=@���@���@�~�@�J@�@���@��@��@�x�@�X@�&�@��@���@�Q�@���@���@�l�@���@��H@�{@��T@�p�@�&�@���@��D@�r�@�I�@�b@���@��@���@�dZ@�+@��@��+@�5?@�@���@��^@��@�?}@��@���@��@�bN@�9X@��@�  @��m@��
@��P@�"�@��H@�~�@�V@�E�@�=q@�@���@���@�`B@�&�@��@���@��@��@�Q�@�1@��@���@�ƨ@��F@��P@�;d@�@��H@���@��+@�M�@��#@��^@��^@��-@��h@�?}@�V@��/@���@��@�1'@��F@�t�@�;d@��@��y@��\@�ff@�=q@�J@��T@��^@�?}@��@��u@�z�@�r�@�bN@�A�@�(�@�b@�P@l�@�@~v�@~5?@~{@}�T@}�-@}p�@}/@|�@|z�@|(�@{�m@{��@{S�@z�@zM�@y�@y7L@x �@w��@w+@v$�@u@u��@u`B@uO�@uV@t��@t��@t9X@sƨ@sdZ@r�\@r-@q�^@q&�@p�9@p1'@o�@o|�@o�@n�y@nE�@m�h@mp�@mV@l9X@k�@kS�@j�!@ix�@i%@h��@h�@hQ�@g�w@g+@fȴ@fE�@e�-@eO�@d�@d9X@cƨ@ct�@c@b�\@b^5@b=q@bJ@a�#@ax�@aG�@a%@`�u@`1'@_�@_
=@^�@^�y@^v�@^E�@^{@]@]?}@\�j@\�@\�D@\�@[dZ@[33@[o@Z�\@Y�@Y�7@YG�@XĜ@XbN@W�@WK�@V��@U�T@U`B@T��@T��@T9X@S��@S�m@S�
@Sƨ@SC�@R��@R��@R��@R�\@R^5@Q��@Q�^@Qx�@QG�@Q�@PĜ@P��@P�@PbN@PQ�@PQ�@Pb@O�w@O\)@O�@O
=@N��@NE�@N{@M@M��@M��@M��@M�@L�/@L�D@LI�@K�
@K��@KC�@K"�@J�H@J�\@J=q@J�@JJ@I�#@I�7@IG�@I&�@I�@I%@H�`@H��@H�@HQ�@HA�@G��@Gl�@G�@F�y@F�+@FE�@F$�@F@E�@E`B@E?}@E�@D�@D�/@D��@D�D@DZ@C�m@C��@Ct�@B�@B��@B�\@B~�@Bn�@BM�@B�@A�@A��@AG�@@�`@@Ĝ@@�9@@�u@@Q�@@Q�@@ �@@  @?\)@>�@>ȴ@>��@>�+@>ff@>V@>E�@=��@=V@<��@<��@<�D@<I�@<(�@<1@;ƨ@;��@;33@;"�@;@:��@:�\@:^5@:-@:�@9��@9��@9&�@9�@8�u@8 �@8b@7�w@7|�@7K�@7;d@6�@6V@5�@5��@5`B@5/@5�@4�@49X@4�@3��@3ƨ@3��@3"�@2�H@2��@2M�@1��@1��@1�^@1��@1�7@1�@0��@0Ĝ@0Ĝ@0�@0b@/�;@/�w@/��@/|�@/\)@/+@.��@.��@.ȴ@.ff@.5?@.$�@-�@-�h@-�@-O�@,��@,�@,(�@+�F@+�@+�@+C�@*�@*��@*��@*�\@*n�@*^5@*=q@)�#@)�7@)G�@)&�@)�@(��@(bN@'�@'|�@'K�@'K�@'+@'�@'
=@&��@&�@&��@&ff@&E�@&$�@&@%�@%��@%@%�@%`B@%?}@$�/@$z�@$(�@#��@#ƨ@#�@#33@#@"�H@"��@"��@"n�@"-@"�@!�@!�^@!hs@!7L@!%@ �`@ �u@ Q�@  �@��@\)@;d@+@��@��@v�@5?@�@��@��@��@�h@`B@/@��@�/@�@z�@j@I�@�@�m@��@C�@�@�!@��@��@�\@^5@J@��@�7@hs@7L@%@�`@Ĝ@�@Q�@ �@�;@��@l�@l�@�P@K�@;d@ȴ@��@V@5?@{@�T@`B@�@��@�/@�j@Z@�@�
@�@t�@dZ@S�@@�!@��@~�@-@�@�#@�7@hs@X@�@��@�u@ �@b@�@��@��@l�@+@
=@��@�@ȴ@�R@��@��@��@ff@E�@@�@�T@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�hsA�^5A�M�A�?}A�5?A�1A�FA�A�A�JA��mA��#A���A�wA�FA�A���A���A��A�|�A�n�A�ZA��A�bA� �A�ĜA��A�I�A�A�ƨA��A�dZA�l�A֬A��;AӼjA�&�AЋDAΑhA�(�Aɲ-A�AǅAŇ+A���A� �A�ZA�-A���A�9XA��A��yA�dZA���A���A�?}A�z�A���A��9A���A���A���A�`BA�n�A���A���A�oA�A�A��-A�1'A�ĜA� �A���A�%A��^A�G�A��RA���A��
A��RA�1A�-A�p�A�/A��A��/A���A�z�A�1A�%A�A�JA��FA��A�-A��9A�
=A�bA�bNA�VA�%A�E�A���A���A�JA�?}A���A���A��+A�hsA��;A�/A��9A�A��A�ȴA�;dA~�DA{�Az{Av�9As�-Ar�yApVAm`BAj1'Af��AcdZAa�Aa;dA_��A^ �A\�`A[7LAZ1'AY��AXbAV�\AS�AR��ARbAQ�AQS�AO�mANI�AL�+AJ5?AH�yAG/AE`BAD��ADZAC�AAt�A?�TA>��A=C�A<A;VA:ffA9�wA8jA6-A3�mA3hsA2��A1�;A0�jA/��A-l�A,ffA+�A+
=A*9XA(��A(n�A'+A&�+A&1'A%��A%�A$�A#7LA!��A �HA�
A��A5?AG�AM�A�TA��A�\A��A��A�+AG�Ap�A��A�hAz�A\)A|�A
 �A��AM�A�AffA��A`BA�A��A��A�HAjA1A�A�
A ��@�+@�=q@�@�z�@�|�@�33@���@���@�O�@���@�ƨ@���@�G�@@�`B@�w@��@��@�1@�@�@�p�@��@�S�@�+@އ+@���@���@�K�@���@��#@�`B@�l�@��@���@�5?@�?}@���@���@���@̛�@��@�S�@ʰ!@���@�`B@�&�@ȣ�@�\)@��T@ź^@���@�Q�@ÍP@+@�E�@�{@��7@�X@��j@��w@�"�@�v�@�5?@�-@�{@�{@��T@�hs@��F@�o@�@��@��@��D@���@�l�@�@���@�n�@��@��^@���@�(�@�ƨ@��P@��@���@��#@�@�x�@�G�@�%@�(�@���@��P@�\)@�S�@�S�@�K�@�;d@�C�@��@��!@�^5@�J@���@��7@�%@���@�b@��F@���@���@�1'@��/@��@��@��@�j@�b@���@��@��;@��
@���@��F@���@��@�dZ@�K�@��!@���@���@���@���@�&�@� �@��@���@���@�C�@�"�@�
=@�
=@���@���@�=q@�-@�@���@�/@��@�I�@���@��m@���@�"�@���@��+@�ff@���@�`B@�O�@�?}@���@�j@��@��w@��@�+@��@��R@��R@���@�$�@�@��@�J@��@�@��h@�hs@�&�@�V@���@���@�Ĝ@��@�bN@�b@��w@���@�|�@�\)@��@���@�v�@�@�p�@�7L@�V@���@���@��/@��j@�j@��;@���@�dZ@�;d@�
=@���@���@�~�@�J@�@���@��@��@�x�@�X@�&�@��@���@�Q�@���@���@�l�@���@��H@�{@��T@�p�@�&�@���@��D@�r�@�I�@�b@���@��@���@�dZ@�+@��@��+@�5?@�@���@��^@��@�?}@��@���@��@�bN@�9X@��@�  @��m@��
@��P@�"�@��H@�~�@�V@�E�@�=q@�@���@���@�`B@�&�@��@���@��@��@�Q�@�1@��@���@�ƨ@��F@��P@�;d@�@��H@���@��+@�M�@��#@��^@��^@��-@��h@�?}@�V@��/@���@��@�1'@��F@�t�@�;d@��@��y@��\@�ff@�=q@�J@��T@��^@�?}@��@��u@�z�@�r�@�bN@�A�@�(�@�b@�P@l�@�@~v�@~5?@~{@}�T@}�-@}p�@}/@|�@|z�@|(�@{�m@{��@{S�@z�@zM�@y�@y7L@x �@w��@w+@v$�@u@u��@u`B@uO�@uV@t��@t��@t9X@sƨ@sdZ@r�\@r-@q�^@q&�@p�9@p1'@o�@o|�@o�@n�y@nE�@m�h@mp�@mV@l9X@k�@kS�@j�!@ix�@i%@h��@h�@hQ�@g�w@g+@fȴ@fE�@e�-@eO�@d�@d9X@cƨ@ct�@c@b�\@b^5@b=q@bJ@a�#@ax�@aG�@a%@`�u@`1'@_�@_
=@^�@^�y@^v�@^E�@^{@]@]?}@\�j@\�@\�D@\�@[dZ@[33@[o@Z�\@Y�@Y�7@YG�@XĜ@XbN@W�@WK�@V��@U�T@U`B@T��@T��@T9X@S��@S�m@S�
@Sƨ@SC�@R��@R��@R��@R�\@R^5@Q��@Q�^@Qx�@QG�@Q�@PĜ@P��@P�@PbN@PQ�@PQ�@Pb@O�w@O\)@O�@O
=@N��@NE�@N{@M@M��@M��@M��@M�@L�/@L�D@LI�@K�
@K��@KC�@K"�@J�H@J�\@J=q@J�@JJ@I�#@I�7@IG�@I&�@I�@I%@H�`@H��@H�@HQ�@HA�@G��@Gl�@G�@F�y@F�+@FE�@F$�@F@E�@E`B@E?}@E�@D�@D�/@D��@D�D@DZ@C�m@C��@Ct�@B�@B��@B�\@B~�@Bn�@BM�@B�@A�@A��@AG�@@�`@@Ĝ@@�9@@�u@@Q�@@Q�@@ �@@  @?\)@>�@>ȴ@>��@>�+@>ff@>V@>E�@=��@=V@<��@<��@<�D@<I�@<(�@<1@;ƨ@;��@;33@;"�@;@:��@:�\@:^5@:-@:�@9��@9��@9&�@9�@8�u@8 �@8b@7�w@7|�@7K�@7;d@6�@6V@5�@5��@5`B@5/@5�@4�@49X@4�@3��@3ƨ@3��@3"�@2�H@2��@2M�@1��@1��@1�^@1��@1�7@1�@0��@0Ĝ@0Ĝ@0�@0b@/�;@/�w@/��@/|�@/\)@/+@.��@.��@.ȴ@.ff@.5?@.$�@-�@-�h@-�@-O�@,��@,�@,(�@+�F@+�@+�@+C�@*�@*��@*��@*�\@*n�@*^5@*=q@)�#@)�7@)G�@)&�@)�@(��@(bN@'�@'|�@'K�@'K�@'+@'�@'
=@&��@&�@&��@&ff@&E�@&$�@&@%�@%��@%@%�@%`B@%?}@$�/@$z�@$(�@#��@#ƨ@#�@#33@#@"�H@"��@"��@"n�@"-@"�@!�@!�^@!hs@!7L@!%@ �`@ �u@ Q�@  �@��@\)@;d@+@��@��@v�@5?@�@��@��@��@�h@`B@/@��@�/@�@z�@j@I�@�@�m@��@C�@�@�!@��@��@�\@^5@J@��@�7@hs@7L@%@�`@Ĝ@�@Q�@ �@�;@��@l�@l�@�P@K�@;d@ȴ@��@V@5?@{@�T@`B@�@��@�/@�j@Z@�@�
@�@t�@dZ@S�@@�!@��@~�@-@�@�#@�7@hs@X@�@��@�u@ �@b@�@��@��@l�@+@
=@��@�@ȴ@�R@��@��@��@ff@E�@@�@�T@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
�B
�B
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
�B
�B
�TB
��B
�LB
�LB
�LB
�XB
�!B
��B
��B
�DB
�B
�B
��B
�B
�mB#�B&�BN�BE�BQ�Bl�B�oB�BÖB�HB�B�qB��B�LB��B�B�BB�BbBuBPBVB(�B5?B?}B>wBuBoB7LBW
BD�B'�B1'B2-B1'B(�B�BhBoBB�B�BB�
B�dB��B�1BffBO�BT�BZB`BBr�Br�BgmBbNBW
BE�B/BbB\B
��B
�fB
��B
ƨB
�FB
��B
�VB
�B
�B
�+B
�+B
�B
� B
w�B
v�B
iyB
_;B
\)B
Q�B
?}B
$�B
�B

=B	��B	�B	�/B	B	��B	�1B	u�B	~�B	{�B	o�B	dZB	\)B	P�B	J�B	B�B	6FB	)�B	�B	�B	�B	�B	�B	oB	DB	B��B��B��B�B�B�B�ZB�#B��B��B��BŢBŢBÖB�}B�FB�B��B�!B�B��B��B��B�VB��B��B�uB�hB�JB�\B�JB�oB��B�uB�VB�1B�%B� B|�Bx�Bm�Bm�Bs�Bq�Br�BgmBjBs�Bt�Bs�Bl�BcTBn�Bl�BiyBiyBe`Be`BffBiyBcTBffBffBffBn�Bl�Bs�Bt�Bo�Bk�Be`B\)B\)B\)BhsBm�Bk�Bl�Bn�BiyBcTBk�Bt�Br�Br�Bo�BiyBiyBl�Bp�Bp�Bp�Bu�Bt�Bp�BhsBffBp�Bp�Br�Bv�B{�B�B�B�+B�B}�Bw�Bv�By�By�Bx�Bz�B{�B~�B�B�B�B�%B�1B�+B�%B�=B�{B�uB��B��B��B��B��B��B�B�B�B�LB�dB�}B��B��B��B��B�wB��B��B��B��B��B��B�B�)B�TB�mB�sB�yB�B�B�B�B�B�B��B�B��B��B��B��B��B	B	+B		7B	VB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	'�B	,B	2-B	9XB	>wB	@�B	C�B	D�B	C�B	E�B	K�B	N�B	O�B	O�B	O�B	P�B	R�B	T�B	VB	W
B	VB	XB	\)B	]/B	\)B	\)B	]/B	cTB	dZB	dZB	e`B	hsB	iyB	k�B	k�B	k�B	o�B	s�B	s�B	r�B	t�B	x�B	z�B	�B	�B	�B	�%B	�DB	�VB	�bB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	�qB	�jB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�HB	�BB	�fB	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
%B
+B
1B
1B
1B
1B
+B
	7B
DB
DB

=B
	7B

=B
DB
DB

=B
1B
	7B

=B
DB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
uB
uB
uB
uB
uB
uB
uB
oB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
 �B
#�B
#�B
!�B
$�B
%�B
$�B
$�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
)�B
+B
)�B
+B
+B
)�B
+B
+B
+B
-B
.B
-B
.B
.B
/B
/B
/B
/B
0!B
/B
/B
0!B
0!B
/B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
33B
49B
49B
49B
5?B
5?B
6FB
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
8RB
9XB
9XB
9XB
:^B
9XB
9XB
:^B
:^B
<jB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
=qB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
H�B
I�B
J�B
J�B
I�B
J�B
J�B
J�B
I�B
I�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
O�B
O�B
N�B
O�B
O�B
O�B
P�B
O�B
O�B
O�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
XB
W
B
W
B
W
B
W
B
XB
XB
W
B
W
B
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
e`B
ffB
ffB
ffB
gmB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
iyB
jB
k�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
�%B
�[B
�iB
�B
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
�tB
ΥB
�>B
�RB
��B
��B
��B
��B
��B
�BB
�}B
�eB
�2B
�RB
�B%`B+kBR�BK�BTFBp;B��B��B��B� B�1B�uB�@B��B�sB�B BB�B�oB�B2B�BHB)*B5�B@�BA�BB&B7�BY�BIlB+6B2GB3�B2GB+6B�BaBBtB��B��B�B��B��B�xBj�BQ�BU�BZ�Ba�BuBt�BiyBcnBX�BHKB2�BaBB
��B
�_B
�gB
�fB
�B
��B
�HB
��B
�B
��B
��B
��B
�oB
yXB
x8B
k�B
`�B
\�B
S�B
BB
(�B
�B
"B	�B	�MB	�vB	�tB	�B	�dB	y�B	��B	}"B	q�B	fLB	]�B	R�B	LB	C�B	8lB	,"B	�B	�B	�B	�B	jB	{B	�B	�B��B��B��B�B�B�OB�2B�IB��B�aB��B�EB��BĜB��B�lB��B�qB��B� B��B�hB�]B� B��B��B�{B��B��B�bB��B�@B�
B�,B�vB��B��B��B~]Bz�BpBo�BuBr�Bs�BjBl"Bt�Bu�Bt�BnIBe�Bo�BnBk6BkQBg�Bg8Bh$BjeBd�BgBg8BgBo Bl�Bs�Bt�BpUBlqBf�B]�B]�B]�BiBnBlWBmCBo5Bj�Be,BlqBu?Bs�BshBp�Bj�BkBm�Bq[BqvBq�BvFButBq�Bj�Bg�Bp�Bq[Bs�Bw�B|jB��B��B��B�uBByXBw�Bz�Bz�By�B{�B|�B}B�oB��B��B��B��B��B�+B�)B��B�B�B�IB�OB� B�2B�XB�]B��B��B��B��B��B��B��B��B��B�BB�"B�B�FBөB��B�BܒB�B�B��B��B��B�B�B��B��B�B�%B�TB�B�6B�(B�BB��B	SB	_B		lB	pB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	$@B	($B	,"B	1�B	9	B	>(B	@�B	C�B	D�B	C�B	E�B	K�B	N�B	O�B	O�B	O�B	P�B	SB	T�B	V9B	W?B	V�B	X_B	\CB	]IB	\xB	\�B	]�B	cnB	dtB	dtB	e�B	h�B	i�B	k�B	k�B	k�B	o�B	s�B	s�B	r�B	u%B	y$B	{0B	�;B	�3B	�gB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�B	�0B	�B	�/B	�IB	�;B	�3B	�TB	�`B	��B	��B	�rB	��B	�B	��B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	� B	�B	�,B	�2B	�9B	�2B	�EB	�7B	�7B	�#B	�=B	�B	�7B	�QB	�kB	�qB	�xB	�dB	�jB	ބB	�|B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�0B	�B	�"B	�BB
 B
 B
 B
 4B
;B
;B
;B
'B
B
-B
-B
GB
GB
9B
?B
?B
EB
EB
_B
?B
_B
KB
fB
1B
KB
_B
	RB
DB
DB

XB
	lB

rB
DB
^B

rB
�B
	�B

rB
^B
JB
~B
~B
jB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
!�B
 �B
�B
 �B
#�B
$&B
"B
$�B
%�B
%B
$�B
$B
$�B
&B
%�B
%�B
'B
($B
'8B
($B
)B
)*B
)�B
*B
+B
*B
+B
*0B
+B
+B
*0B
+6B
+6B
+6B
-)B
.B
-CB
./B
./B
/5B
/5B
/5B
/B
0UB
/OB
/5B
0!B
0!B
/OB
./B
/OB
0;B
0UB
0UB
0UB
0oB
0oB
1vB
3hB
4nB
4nB
4TB
5ZB
5ZB
6FB
5ZB
5tB
6`B
72B
7fB
7fB
7LB
7LB
7fB
7fB
7LB
8lB
7LB
8lB
9XB
9rB
9rB
:^B
9rB
9rB
:xB
:^B
<jB
;�B
<�B
=�B
=�B
>wB
>wB
>]B
=�B
<�B
=�B
>�B
>�B
?�B
?�B
?�B
?}B
@�B
@�B
A�B
A�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
H�B
I�B
J�B
J�B
I�B
J�B
J�B
J�B
I�B
I�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
O�B
O�B
N�B
O�B
O�B
O�B
P�B
O�B
PB
O�B
Q B
PB
Q B
Q�B
RB
Q�B
SB
SB
R B
R B
SB
SB
TB
TB
TB
T,B
T,B
T�B
UB
UB
UB
UB
UB
VB
VB
VB
W$B
XB
W$B
W
B
W?B
W$B
XB
XB
W$B
W$B
X+B
Y1B
YB
Y1B
Y1B
Y1B
Y1B
YB
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[=B
[WB
[=B
[WB
]IB
]/B
]/B
]IB
]IB
^5B
^5B
^5B
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_VB
^OB
^jB
_pB
abB
aHB
abB
a-B
aHB
aHB
abB
abB
aHB
bhB
bhB
bNB
bNB
bNB
bhB
bhB
bhB
bNB
b�B
bhB
bhB
cnB
cnB
cnB
cTB
dtB
dtB
e`B
e`B
dtB
ezB
e`B
ezB
ezB
ezB
ezB
ffB
ffB
ezB
f�B
f�B
f�B
g�B
h�B
hsB
gmB
gmB
h�B
h�B
h�B
i�B
iyB
iyB
iyB
i�B
i�B
i�B
i�B
jB
jB
jeB
j�B
jB
jB
j�B
i�B
j�B
k�B
lqB
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709170035292017091700352920170917003529201806221319052018062213190520180622131905201804050721312018040507213120180405072131  JA  ARFMdecpA19c                                                                20170913093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170913003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170913003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170913003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170913003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170913003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170913003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170913003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170913003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170913003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20170913005632                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170913153309  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20170916153529  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170916153529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222131  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041905  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                