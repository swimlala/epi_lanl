CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:31Z AOML 3.0 creation; 2016-06-01T00:08:13Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230831  20160531170813  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               2A   AO  4055_7112_050                   2C  D   APEX                            5374                            041511                          846 @֪ڍ�?�1   @֪�Tb�@9��t�j�c�C��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    2A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�3D�C3D���D��3D�3D�I�D���D�ٚD�� D�<�D�|�D��fD�fD�I�Dڙ�D��fD���D�9�D�|�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl!GCn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�RDy�D��D�J�D��)D�ʏD��D�P�D���D���D��\D�D)D��)D���D��D�P�Dڠ�D���D� �D�@�D�)D�ʏ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�7LA�;dA�;dA�7LA�5?A�9XA�33A�33A�33A�7LA�9XA�(�A��mA�ĜAǣ�AǑhAǉ7AǅA�~�A�\)A�JA��A���A���A�^5A�bA�1A���A��FA�A���A���A��wA�O�A�JA��A��DA��HA�S�A��PA���A�ZA���A�ffA� �A��wA��A��;A�dZA��A��A��A��!A�K�A���A�|�A��A�`BA��FA�+A���A��A�A��A���A�5?A���A���A��A��A�|�A���A��HA�hsA���A�/A���A���A���A���A�33A���A��#A�C�A���A���A��FA���A���A�~�A�x�A�?}A�-A�C�A��wA���A�v�A�Q�A��^A��A|Az-AyK�Ax^5Av1'AudZAtffAq��An�9Ak�^Aj��Ah�RAg�7AeXAb�Aa��A`�+A^��A]
=A\{AZ��AZ�\AZVAX~�AW7LAUhsATbAS+AQ�TAO�#AN�\AM��AL�AL�yAK�AK"�AJ�!AI��AI��AI`BAH��AG�TAGS�AF�+AD�HAD�DAChsAB^5AA�wA@�yA@n�A>�A=K�A<�+A;�A;K�A:�RA7�A6��A5&�A2�A1�^A1O�A1+A1�A0ȴA.�HA.I�A.^5A-;dA,VA,bA+��A+dZA+S�A+VA*�A)hsA(5?A'33A&�DA%A$��A#A#|�A#/A"jA!x�A ĜA��A�!AJAC�A��A�AM�A�AC�A%AQ�A|�A�A�A9XAO�A=qAƨA�A�AA�PA"�AffAJA�
A��At�A�AI�A
9XA�A1AA�AXA�jA�HAjAS�A��AjA��A�A`B@��w@���@�Z@��@�j@��P@�v�@��@��j@�P@���@�x�@@�"�@�$�@�ƨ@�J@�1@�@�D@�b@�
=@�+@�-@��m@�o@��y@�E�@��@�I�@�o@�@�
=@���@�z�@��@ӶF@�~�@�O�@���@�\)@�=q@�&�@�b@�S�@�ff@�5?@���@ɡ�@�?}@ȴ9@�ƨ@��@�7L@���@�j@��@�O�@�1'@�^5@��h@��`@��u@�A�@���@�33@��\@�O�@�b@�@�5?@�O�@��9@���@�J@��`@�1'@���@�E�@��@�`B@���@�Q�@���@���@�l�@�n�@���@�G�@��`@��@�bN@�1@��@�V@��^@�7L@��/@�I�@�  @��
@�33@��!@��@�?}@�1'@��P@�;d@��y@�@�7L@�G�@�7L@��`@�1'@��@�K�@���@�V@�=q@���@�?}@��9@�Q�@�A�@��@�ƨ@�
=@���@�$�@���@�%@���@���@�r�@� �@���@�\)@�33@�o@��!@�E�@���@��-@�x�@�G�@��9@�bN@�(�@�  @��m@�ƨ@��@�"�@��+@�{@��#@���@�G�@�/@��@��@��D@��D@��@�r�@�  @���@�C�@��@�
=@��H@���@�-@���@��@���@���@�Z@�1'@���@��P@�"�@���@���@�n�@�=q@�J@��#@��h@�G�@��@��/@��@��@�9X@�  @��w@���@�l�@�+@�
=@���@���@��\@�v�@�M�@�$�@��@��^@���@�X@��@���@���@�Ĝ@��@���@��u@�bN@��@�;@��@�w@�@l�@
=@~ff@~$�@}��@}O�@|Z@|1@{ƨ@{S�@z�H@z�\@z-@y�#@yG�@x��@x��@xQ�@w�;@w+@vv�@u�T@up�@u/@t�/@t�j@t�@tZ@pQ�@i��@c�@[��@R��@L��@D�D@>��@8A�@3@.�y@+o@$9X@�T@~�@z�@%@�-@
^5@  @9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A�7LA�;dA�;dA�7LA�5?A�9XA�33A�33A�33A�7LA�9XA�(�A��mA�ĜAǣ�AǑhAǉ7AǅA�~�A�\)A�JA��A���A���A�^5A�bA�1A���A��FA�A���A���A��wA�O�A�JA��A��DA��HA�S�A��PA���A�ZA���A�ffA� �A��wA��A��;A�dZA��A��A��A��!A�K�A���A�|�A��A�`BA��FA�+A���A��A�A��A���A�5?A���A���A��A��A�|�A���A��HA�hsA���A�/A���A���A���A���A�33A���A��#A�C�A���A���A��FA���A���A�~�A�x�A�?}A�-A�C�A��wA���A�v�A�Q�A��^A��A|Az-AyK�Ax^5Av1'AudZAtffAq��An�9Ak�^Aj��Ah�RAg�7AeXAb�Aa��A`�+A^��A]
=A\{AZ��AZ�\AZVAX~�AW7LAUhsATbAS+AQ�TAO�#AN�\AM��AL�AL�yAK�AK"�AJ�!AI��AI��AI`BAH��AG�TAGS�AF�+AD�HAD�DAChsAB^5AA�wA@�yA@n�A>�A=K�A<�+A;�A;K�A:�RA7�A6��A5&�A2�A1�^A1O�A1+A1�A0ȴA.�HA.I�A.^5A-;dA,VA,bA+��A+dZA+S�A+VA*�A)hsA(5?A'33A&�DA%A$��A#A#|�A#/A"jA!x�A ĜA��A�!AJAC�A��A�AM�A�AC�A%AQ�A|�A�A�A9XAO�A=qAƨA�A�AA�PA"�AffAJA�
A��At�A�AI�A
9XA�A1AA�AXA�jA�HAjAS�A��AjA��A�A`B@��w@���@�Z@��@�j@��P@�v�@��@��j@�P@���@�x�@@�"�@�$�@�ƨ@�J@�1@�@�D@�b@�
=@�+@�-@��m@�o@��y@�E�@��@�I�@�o@�@�
=@���@�z�@��@ӶF@�~�@�O�@���@�\)@�=q@�&�@�b@�S�@�ff@�5?@���@ɡ�@�?}@ȴ9@�ƨ@��@�7L@���@�j@��@�O�@�1'@�^5@��h@��`@��u@�A�@���@�33@��\@�O�@�b@�@�5?@�O�@��9@���@�J@��`@�1'@���@�E�@��@�`B@���@�Q�@���@���@�l�@�n�@���@�G�@��`@��@�bN@�1@��@�V@��^@�7L@��/@�I�@�  @��
@�33@��!@��@�?}@�1'@��P@�;d@��y@�@�7L@�G�@�7L@��`@�1'@��@�K�@���@�V@�=q@���@�?}@��9@�Q�@�A�@��@�ƨ@�
=@���@�$�@���@�%@���@���@�r�@� �@���@�\)@�33@�o@��!@�E�@���@��-@�x�@�G�@��9@�bN@�(�@�  @��m@�ƨ@��@�"�@��+@�{@��#@���@�G�@�/@��@��@��D@��D@��@�r�@�  @���@�C�@��@�
=@��H@���@�-@���@��@���@���@�Z@�1'@���@��P@�"�@���@���@�n�@�=q@�J@��#@��h@�G�@��@��/@��@��@�9X@�  @��w@���@�l�@�+@�
=@���@���@��\@�v�@�M�@�$�@��@��^@���@�X@��@���@���@�Ĝ@��@���@��u@�bN@��@�;@��@�w@�@l�@
=@~ff@~$�@}��@}O�@|Z@|1@{ƨ@{S�@z�H@z�\@z-@y�#@yG�@x��@x��@xQ�@w�;@w+@vv�@u�T@up�@u/@t�/@t�j@t�@tZ@pQ�@i��@c�@[��@R��@L��@D�D@>��@8A�@3@.�y@+o@$9X@�T@~�@z�@%@�-@
^5@  @9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B+B+B+B+B+B+B+B+B+B+B+B'�B"�B �B�B�B�B�B�B��B��Bn�BVBL�BF�BE�BS�Bz�B�bBffBbNBz�B|�By�Bv�Bp�Bt�Br�BhsB\)B[#BS�BL�BJ�BG�BC�B:^B-B$�B�B�B%�B'�B �B�B{BDB��B�B�TB�NB��B��B�B��B��BŢB��B�jB�3B��B�\B{�Bs�BbNBS�BF�B33B"�B�BDB�B�
BȴB��B�hBx�BQ�B&�BPB
��B
�;B
ȴB
�RB
�B
��B
�B
k�B
M�B
2-B
oB
B	��B	�B	�HB	�B	��B	�RB	��B	�JB	�B	w�B	l�B	`BB	Q�B	I�B	D�B	<jB	0!B	(�B	#�B	.B	33B	"�B	uB	B	B��B��B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�mB�B�B�sB�ZB�BB�#B��B��BŢB��B�qB�RB�B�B��B��B��B��B��B��B��B��B�B�9B�3B�3B�-B�'B�-B�3B�-B�B��B�B��B��B��B��B��B��B��B�{B�hB�VB�=B�+B�B�B� B~�B}�B{�Bx�Bv�Bs�Bo�Bk�BgmBbNB]/BVBJ�BE�BB�B@�B?}B=qB;dB:^B9XB8RB6FB33B.B'�B&�B%�B$�B#�B!�B�B�B�B�B�B�B�B{BhB\BVBJBDB
=B
=B	7B	7B1B+B+B%B+B+B+B%B+B%B	7B	7B	7B	7B
=BDBPBVBVBPBbBVBPBJB
=BJBJBJBJBJBPBPBVBbBhBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B%�B'�B'�B'�B(�B(�B)�B,B/B1'B2-B49B5?B7LB;dB>wB?}BC�BD�BE�BG�BH�BJ�BL�BL�BL�BO�BP�BS�BT�BVBVBW
BZB]/B_;BaHBbNBdZBe`BffBgmBiyBn�Bq�Bw�Bz�B|�B}�B�B�B�%B�+B�1B�DB�JB�PB�bB�uB�uB�uB��B��B��B��B��B��B��B�B�!B�3B�?B�FB�LB�RB�XB�qB�}B�}B��B��BƨBȴB��B��B��B��B��B�B�B�)B�5B�HB�TB�mB�sB�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	+B	1B	
=B	VB	uB	�B	�B	�B	�B	�B	!�B	$�B	'�B	-B	.B	/B	1'B	33B	49B	7LB	;dB	<jB	>wB	@�B	A�B	D�B	F�B	H�B	I�B	K�B	N�B	O�B	Q�B	S�B	T�B	T�B	W
B	YB	ZB	\)B	^5B	aHB	cTB	dZB	ffB	gmB	hsB	hsB	iyB	jB	m�B	o�B	o�B	p�B	p�B	q�B	t�B	w�B	y�B	z�B	}�B	�B	�B	�%B	�7B	�DB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�RB	��B	�NB	��B
+B
oB
 �B
(�B
33B
9XB
?}B
E�B
N�B
W
B
[#B
bNB
gmB
jB
n�B
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B)�B*�B*�B*�B*�B*�B*�B*�B*�B*�B*�B*�B'�B"�B �B�B�B�B�BB��B��Bn�BU�BL�BF�BE�BS�Bz�B�SBfTBb?Bz�B|�By�Bv�Bp�Bt�Br�BhaB\B[BS�BL�BJ�BG�BC�B:IB,�B$�B�B�B%�B'�B �BBnB1B��B�}B�?B�>B��B��B��B��BʫBŋB�uB�VB�B��B�GB{�Bs�Bb:BS�BF�B3B"�B�B.B�B��BȠB��B�RBx�BQ�B&�B;B
��B
�'B
ȠB
�=B
��B
��B
��B
ksB
M�B
2B
^B
 B	��B	�B	�:B	�B	��B	�DB	��B	�=B	�B	w�B	l�B	`6B	Q�B	I�B	D�B	<aB	0B	(�B	#�B	.B	3*B	"�B	mB		B	B��B��B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�eB�B�{B�kB�TB�<B�B��B��BśB�|B�jB�MB�B��B��B��B��B��B��B��B��B��B�B�3B�+B�*B�&B�B�'B�,B�&B�B��B��B��B��B��B��B��B��B��B�wB�eB�OB�8B�'B�B�B�B~�B}�B{�Bx�Bv�Bs�Bo�Bk�BgjBbJB]+BVBJ�BE�BB�B@�B?zB=SB;aB:\B9WB8QB6CB32B.B'�B&�B%�B$�B#�B!�B�B�B�BpBkBdBeB`BJB>B:B-B'B
!B
!B	B	BBB*BBBB*BBB#B	B	6B	5B	B
!BBBNB;B:BMBDB9BNBGB
9B.BGBHBHB-B1BNB8B_BLBlBrBbBB~B~BbBcB�B�B�B�B�B�B�B �B$�B%�B'�B'�B'�B(�B(�B)�B,B/B1#B2(B44B58B7GB;^B>nB?vBC�BD�BE�BG�BH�BJ�BL�BL�BL�BO�BP�BS�BT�BU�BU�BWBZB](B_4BaBBbEBdQBeTBf_BgeBipBn�Bq�Bw�Bz�B|�B}�B��B�B�B�$B�&B�:B�@B�FB�VB�jB�kB�iB��B��B��B��B��B��B��B� B�B�$B�3B�9B�@B�CB�JB�dB�pB�oB�tB�}BƙBȨBʴB��B��B��B��B� B�B�B�'B�:B�EB�^B�eB�pB�xB�B�B�B��B��B��B��B��B��B	 �B	B	B	B	#B	
-B	GB	dB	�B	�B	�B	�B	�B	!�B	$�B	'�B	,�B	.B	/	B	1B	3!B	4'B	7:B	;TB	<XB	>eB	@rB	AyB	D�B	F�B	H�B	I�B	K�B	N�B	O�B	Q�B	S�B	T�B	T�B	V�B	YB	ZB	\B	^"B	a4B	c@B	dHB	fRB	gZB	h`B	haB	ifB	jkB	m}B	o�B	o�B	p�B	p�B	q�B	t�B	w�B	y�B	z�B	}�B	�B	�B	�B	�!B	�/B	�=B	�GB	�OB	�`B	�rB	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�=B	��B	�4B	��B
B
WB
 �B
(�B
3B
9=B
?dB
E�B
N�B
V�B
[
B
b5B
gRB
jeB
n~B
q�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708132016053117081320160531170813  AO  ARCAADJP                                                                    20140721230831    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230831  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230831  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170813  IP                  G�O�G�O�G�O�                