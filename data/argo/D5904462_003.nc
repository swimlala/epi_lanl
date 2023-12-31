CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:01Z AOML 3.0 creation; 2016-08-07T21:51:09Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150226221401  20160807145109  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_003                   2C  D   APEX                            6529                            072314                          846 @��ş�1   @������@21&�x��c��+J1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�33B�ffB�  B���B���B�  B�ffB���B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  DuS3Dy�3D�  D�6fD�ffD�� D�	�D�P D�|�D�ɚD��fD�@ D�s3D���D���D�)�Dڐ D��3D�fD�P D� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@ȣ�A�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQ�GBYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��B��qB��>B��>B��qB��qB��qB��B�#�B̽qBЊ>BԊ>BؽqB�#�B��>B�qB�qB�qB�qB��qB��>B��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D�HD�D��D�D��D�D�HD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Duj�Dy��D��D�B=D�r=D���D�qD�[�D���D��qD�=D�K�D�
D�ؤD��D�5qDڛ�D��
D�=D�[�D��D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�\)A�^5A�n�A�p�A�n�A�jA�jA�p�A�p�A�ffA�S�A�I�A�33A�7LA�/A۸RA��A���A���Aٴ9A٥�A�z�A�K�A�7LA�5?A�/A�(�A�oA�
=A�A���A���AؾwA؏\A�VA�
=A�bNA��mA�K�A��/A�M�A�z�A�
=A�p�A�~�A�l�A�Q�A�=qA�bA��A��A�dZA�ƨA��7A��A�
=A�l�A�jA��`A�t�A��A�bA�ƨA�9XA���A��A�t�A�hsA��A�
=A�+A�M�A�7LA���A�A�XA��jA�A�A��\A���A�5?A�r�A�O�A�^5A���A��A�9XA��TA���A�$�A���A���A��HA�p�A��A��A�p�A���A��+A}�FAx$�AuVAs�;Ar1'Ap  Am;dAjffAf��AdbNAc�A`jA\z�AX9XAS�PAR$�AQG�AP�yAP��APE�AN�HAHA�AE��AD��AChsABZA@ĜA;O�A6�A4(�A2��A1G�A/?}A,bA)O�A(�A'�#A&jA%oA#��A"��A Q�A�`A��A�+Ap�A�`AAhsA�A��AO�At�A�+A��AVA �Ax�A��AE�At�A�Ap�A;dA��A9XA{A�A��A"�A
�DA
�DA
�uA
ȴA
�A
��A\)A��A��A
=A	�A�AȴA��AjA1A1A�AĜA^5A|�A�PA�A�-A�`AVA�-Av�A�A�/A�FA �A �`A v�@���@��@�%@��9@��@��F@���@��@�$�@���@� �@�K�@��y@���@��H@���@�5?@�-@��@�@�t�@�l�@���@��@��@��@�@�G�@��D@�  @�\@�X@�D@�(�@�1'@�(�@�  @�w@땁@�P@�|�@�C�@���@�^@�z�@�;d@�ff@�ff@���@�j@�^5@�`B@߅@�J@�J@ݑh@܃@��@ە�@��@���@ڏ\@�@ف@��`@ج@أ�@�I�@�  @��
@ו�@�\)@�
=@�n�@�E�@��@���@��@ӍP@�K�@�;d@��@�o@�
=@���@��y@��@�ȴ@Ұ!@җ�@�~�@�^5@�@�@��/@ϕ�@�C�@���@�=q@́@�hs@��@̴9@�A�@�1@˕�@�t�@�"�@��@�ff@��T@�X@�V@���@�Ĝ@ȣ�@�I�@�  @�S�@���@�{@���@Ų-@��@�Q�@�1@öF@�K�@�@�E�@��^@���@���@�?}@�I�@�t�@�S�@�C�@�33@�
=@�V@�hs@��@��`@���@�z�@�ƨ@�t�@�+@�o@���@��@�V@���@�&�@��j@��@���@�n�@�^5@�5?@��#@�hs@��j@��@� �@�l�@��P@�t�@��y@��@�p�@�/@�bN@��m@���@�dZ@�+@��@��+@�@���@��7@��`@��D@�Z@� �@���@�ƨ@���@�S�@��@��@��\@��@��#@�/@���@��@�\)@��@��\@���@��h@�x�@���@�b@���@���@���@���@�|�@�t�@�\)@�;d@�
=@��@��@���@��y@�v�@���@�`B@�7L@��/@���@�o@�ff@���@�p�@�`B@�G�@�?}@�?}@�G�@�O�@�O�@�X@�G�@�1'@���@�K�@���@�~�@�{@�hs@��@���@��`@�Ĝ@���@��D@�I�@��@���@���@��@���@���@�l�@�n�@��T@�7L@�&�@�&�@�&�@��@�(�@�l�@�K�@�"�@�
=@��@���@�=q@��@�&�@���@��/@���@��j@�z�@��w@�+@��R@���@�(�@�\)@��/@���@x  @o�@b��@Vff@N{@I7L@CdZ@7�P@/�@)G�@$(�@�@�@��@�H@K�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�\)A�\)A�^5A�n�A�p�A�n�A�jA�jA�p�A�p�A�ffA�S�A�I�A�33A�7LA�/A۸RA��A���A���Aٴ9A٥�A�z�A�K�A�7LA�5?A�/A�(�A�oA�
=A�A���A���AؾwA؏\A�VA�
=A�bNA��mA�K�A��/A�M�A�z�A�
=A�p�A�~�A�l�A�Q�A�=qA�bA��A��A�dZA�ƨA��7A��A�
=A�l�A�jA��`A�t�A��A�bA�ƨA�9XA���A��A�t�A�hsA��A�
=A�+A�M�A�7LA���A�A�XA��jA�A�A��\A���A�5?A�r�A�O�A�^5A���A��A�9XA��TA���A�$�A���A���A��HA�p�A��A��A�p�A���A��+A}�FAx$�AuVAs�;Ar1'Ap  Am;dAjffAf��AdbNAc�A`jA\z�AX9XAS�PAR$�AQG�AP�yAP��APE�AN�HAHA�AE��AD��AChsABZA@ĜA;O�A6�A4(�A2��A1G�A/?}A,bA)O�A(�A'�#A&jA%oA#��A"��A Q�A�`A��A�+Ap�A�`AAhsA�A��AO�At�A�+A��AVA �Ax�A��AE�At�A�Ap�A;dA��A9XA{A�A��A"�A
�DA
�DA
�uA
ȴA
�A
��A\)A��A��A
=A	�A�AȴA��AjA1A1A�AĜA^5A|�A�PA�A�-A�`AVA�-Av�A�A�/A�FA �A �`A v�@���@��@�%@��9@��@��F@���@��@�$�@���@� �@�K�@��y@���@��H@���@�5?@�-@��@�@�t�@�l�@���@��@��@��@�@�G�@��D@�  @�\@�X@�D@�(�@�1'@�(�@�  @�w@땁@�P@�|�@�C�@���@�^@�z�@�;d@�ff@�ff@���@�j@�^5@�`B@߅@�J@�J@ݑh@܃@��@ە�@��@���@ڏ\@�@ف@��`@ج@أ�@�I�@�  @��
@ו�@�\)@�
=@�n�@�E�@��@���@��@ӍP@�K�@�;d@��@�o@�
=@���@��y@��@�ȴ@Ұ!@җ�@�~�@�^5@�@�@��/@ϕ�@�C�@���@�=q@́@�hs@��@̴9@�A�@�1@˕�@�t�@�"�@��@�ff@��T@�X@�V@���@�Ĝ@ȣ�@�I�@�  @�S�@���@�{@���@Ų-@��@�Q�@�1@öF@�K�@�@�E�@��^@���@���@�?}@�I�@�t�@�S�@�C�@�33@�
=@�V@�hs@��@��`@���@�z�@�ƨ@�t�@�+@�o@���@��@�V@���@�&�@��j@��@���@�n�@�^5@�5?@��#@�hs@��j@��@� �@�l�@��P@�t�@��y@��@�p�@�/@�bN@��m@���@�dZ@�+@��@��+@�@���@��7@��`@��D@�Z@� �@���@�ƨ@���@�S�@��@��@��\@��@��#@�/@���@��@�\)@��@��\@���@��h@�x�@���@�b@���@���@���@���@�|�@�t�@�\)@�;d@�
=@��@��@���@��y@�v�@���@�`B@�7L@��/@���@�o@�ff@���@�p�@�`B@�G�@�?}@�?}@�G�@�O�@�O�@�X@�G�@�1'@���@�K�@���@�~�@�{@�hs@��@���@��`@�Ĝ@���@��D@�I�@��@���@���@��@���@���@�l�@�n�@��T@�7L@�&�@�&�@�&�@��@�(�@�l�@�K�@�"�@�
=@��@���@�=q@��@�&�@���@��/@���@��j@�z�@��w@�+@��RG�O�@�(�@�\)@��/@���@x  @o�@b��@Vff@N{@I7L@CdZ@7�P@/�@)G�@$(�@�@�@��@�H@K�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB.B.B.B-B-B-B,B,B-B-B,B+B)�B'�B'�B&�B�BhB\B\BhBuBoBJB
=B
=B
=B
=B	7B	7B
=BDBBB
��B
��B
�B
�TB
��B
��B
��B
��B
ȴB
�XB
�-B
�-B
�?B
�XB
��B
�mB
�mBB
=B
=B1B+BPB!�BbNBM�B)�B#�B49B�B��B��B�VBcTBH�B�B0!BB�BM�B[#B\)BS�BR�BE�BVBk�Bs�Bu�BjBK�B'�BB
�yB
��B
�XB
��B
�+B
}�B
{�B
o�B
M�B
5?B
hB
  B	��B	�5B	ÖB	��B	�7B	�B	s�B	ffB	W
B	G�B	6FB	)�B	"�B	{B	B��B�B�B�B�B�yB�sB�`B�fB�mB�sB�B�B�B��B�dB�?BĜB�
B�sB�HB�B�B�B��B��B��B��B��B��B��B��B��B��B�`B��B	B��B�B�B�fB�B��B��BƨB�}B��BŢBŢBŢBĜBŢBŢBƨBǮBȴB��B��B��B��B�/B�fB�yB�B�B��B	+B	B	B	B	
=B	uB	�B	 �B	'�B	;dB	D�B	B�B	G�B	XB	ZB	ZB	ZB	ZB	\)B	W
B	O�B	H�B	D�B	F�B	I�B	L�B	P�B	R�B	VB	VB	T�B	T�B	S�B	S�B	YB	[#B	_;B	dZB	ffB	iyB	p�B	r�B	s�B	u�B	w�B	y�B	z�B	{�B	~�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�1B	�=B	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�9B	�9B	�9B	�?B	�?B	�?B	�FB	�LB	�RB	�XB	�XB	�RB	�LB	�LB	�XB	�^B	�jB	�wB	�}B	�}B	��B	B	ŢB	ƨB	ŢB	ƨB	ƨB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�)B	�#B	�#B	�#B	�#B	�#B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
\B
oB
�B
 �B
+B
8RB
>wB
B�B
F�B
J�B
Q�B
W
B
]/B
aHB
e`B
hsB
l�B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B-�B-�B-�B,�B,�B,�B+�B+�B,�B,�B+�B*�B)�B'�B'�B&�B�BNBABABLB[BUB1B
 B
 B
 B
 B	B	B
 B+BB�B
��B
��B
��B
�9B
��B
ͷB
��B
��B
șB
�?B
�B
�B
�"B
�>B
��B
�OB
�OB�B
B
 BBB0B!�Bb1BM�B)�B#�B4B��B��B��B�8Bc4BH�B�B0 BBoBM�B[B\BS�BR�BEBU�BkaBs�Bu�Bj\BK�B'�B�B
�XB
��B
�7B
�B
�B
}�B
{�B
o~B
M�B
5 B
KB	��B	��B	�B	�xB	��B	�B	��B	s�B	fKB	V�B	G�B	6.B	)�B	"�B	eB	�B��B�vB�rB�hB�hB�eB�[B�KB�OB�VB�\B�gB�B�hB��B�MB�+BĈB��B�YB�1B�B�B��B��BͺBοB̵B˯B˯B˰B˯BͼB��B�CB��B	�B��B�B�iB�JB��B��BͺBƎB�cB�qBŉBŇBňBĂBňBņBƊBǓBțB̲B��B��B��B�B�JB�\B�zB�B��B	B	B	�B	�B	
 B	XB	�B	 �B	'�B	;DB	D|B	BnB	G�B	W�B	Y�B	Y�B	Y�B	Y�B	\B	V�B	O�B	H�B	D~B	F�B	I�B	L�B	P�B	R�B	U�B	U�B	T�B	T�B	S�B	S�B	X�B	[B	_B	d9B	fDB	iXB	p�B	r�B	s�B	u�B	w�B	y�B	z�B	{�B	~�B	��B	��B	� B	��B	�B	� B	�	B	�B	�B	�3B	�>B	�QB	�YB	�jB	�uB	�xB	�{B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�|B	�hB	�\B	�jB	�nB	�vB	�uB	�{B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�,B	�2B	�3B	�*B	�%B	�%B	�2B	�9B	�EB	�RB	�XB	�WB	�]B	�iB	�|B	ƂB	�zB	ƃB	ƂB	ǈB	ǉB	ɓB	ɓB	ʙB	ʙB	˟B	ϸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	� B	�&B	�$B	�)B	�*B	�2B	�4B	�1B	�3B	�7B	�:B	�9B	�6B	�:B	�7B	�7B	�3B	�/B	�6B	�=B	�<B	�FB	�DB	�KB	�PB	�]B	�_B	�dB	�bB	�bB	�eB	�^B	�\B	�[B	�]B	�[B	�]B	�[B	�]B	�pB	�vB	�{B	�uB	�oB	�mB	�iB	�hB	�}B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 G�O�B
B
2B
EB
rB
 �B
*�B
8(B
>KB
BeB
F|B
J�B
Q�B
V�B
]B
aB
e5B
hGB
l_B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451092016080714510920160807145109  AO  ARCAADJP                                                                    20150226221401    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221401  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221401  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145109  IP                  G�O�G�O�G�O�                