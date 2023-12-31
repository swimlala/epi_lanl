CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-03T19:16:52Z AOML 3.0 creation; 2016-08-07T21:36:35Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150603191652  20160807143635  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               4A   AO  5286_8897_052                   2C  D   APEX                            6531                            072314                          846 @�U��̀1   @�U���`@2�E����c��\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    4A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt�3Dy�3D� D�0 D�i�D���D��D�I�D��3D��3D��3D�6fD�l�Dǰ D��D�FfD�L�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`8RCb�Cd�Cf�Ch�Cj�Cl�Cn�CpCr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��DtDt��Dt��Dy��D��D�3�D�mqD��qD� �D�MqD��
D��
D��
D�:=D�p�Dǳ�D��D�J=D�P�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԥ�Aԧ�Aԥ�Aԡ�Aԡ�AԅA�jA�O�A�1AӺ^A�z�A���AҼjAҮAҕ�A�I�A�JA���A�~�A�XA�G�A�=qA�$�A�ƨAжFAЬAЇ+A���A��yA��yA��A��mA�+A�bA�
=A�
=A���AɍPAɃA�-A�ĜA�`BA�$�A��yA��`A��A�p�A��mA�5?A���A��A�dZA��;A�VA�t�A���A�ȴA�;dA�ƨA��-A�/A�r�A�E�A�hsA��TA�O�A�"�A��/A�E�A��TA�(�A��
A�bA�{A�r�A� �A�A�I�A�dZA�
=A���A��A��!A�x�A���A�"�A�A��A���A���A��^A�M�A��7A�l�A��9A�M�A�-A�7LA�ĜA�A�A�&�A��A���A��A���A�7LA��wA�S�A��DA~�RA{��AyVAvr�Ap�yAl�9AeXAa�Aa+A`$�A]|�A\  A[K�A[oAZ��AZ�RAY��AV(�AT5?AQ�hAO�^AM�AL=qAJ  AHbNAG��AFĜAC�A?x�A>JA:��A9oA8bA6ĜA6A4bNA1
=A.5?A-�A+�TA+G�A+;dA+;dA++A*��A(�A&ZA$�yA$�+A$9XA$A#�A"ȴA!��A v�A��A��A��Ar�AG�A�`A�jA��A^5A�A�A?}A%A��A��A�A�yA�\A�^AXA�`AQ�A��A��AQ�A�7A��AAG�A�A��Ap�A��AVAx�A	\)A�A�AO�A��AĜA�+AA�AjA �A�PA/@�-@�p�@��P@�=q@���@�O�@�?}@��
@�@�&�@�  @�p�@���@�`B@⟾@�@�j@�Z@�(�@�  @߾w@��H@�{@�G�@ܴ9@�Z@���@��y@�{@ف@�/@��;@��@֟�@�-@պ^@պ^@ՙ�@�O�@�%@ԣ�@�j@�(�@�b@Ӯ@ҧ�@�&�@���@�A�@͡�@�z�@�S�@�@���@�Q�@ǝ�@�t�@�t�@ǥ�@��
@ǍP@�
=@��#@�/@ċD@�I�@�I�@�|�@�M�@�@�-@+@�ȴ@���@¸R@��@� �@�S�@���@�~�@�-@��@�@���@���@�@�5?@�@�7L@�j@���@��y@��+@��@��@���@�J@�33@�K�@�;d@��@�~�@�v�@�M�@�J@��@�Q�@��@���@�n�@�=q@���@�/@��@�"�@��@��y@��@���@�ƨ@�Q�@�j@�9X@��@��@�M�@��-@�x�@�G�@��@��@��D@� �@���@��w@���@�l�@�K�@��@���@��@��!@�^5@�5?@�J@��-@�?}@��/@�A�@��
@���@�l�@�S�@�;d@�"�@�@��@��y@���@��R@���@�v�@�{@�O�@��@���@��/@���@���@��u@�r�@�r�@�Z@�(�@��
@�l�@�33@���@�M�@���@���@��-@���@�hs@��@��D@��;@���@�l�@�\)@�
=@��!@�M�@�$�@��@��@�@���@�O�@��@��/@��9@�Z@���@��@��@�;d@�"�@���@��@���@�V@��#@�`B@�G�@���@�j@�ƨ@��@�;d@��H@��\@�~�@��T@��-@��h@��@�X@�&�@�%@��@�Z@�b@��w@�C�@���@�V@�E�@�E�@�E�@�=q@�@��^@�O�@��@���@�z�@�bN@�9X@� �@�1@���@�ƨ@���@�t�@���@�ff@���@��#@���@���@��@�/@��u@�z�@�bN@��@��@��@��y@�ff@��^@�`B@���@�j@�1'@�b@�z�@�V@�E�@�A�@w��@pr�@h �@["�@R��@HQ�@>V@:~�@4�@,9X@&��@ 1'@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  Aԥ�Aԧ�Aԥ�Aԡ�Aԡ�AԅA�jA�O�A�1AӺ^A�z�A���AҼjAҮAҕ�A�I�A�JA���A�~�A�XA�G�A�=qA�$�A�ƨAжFAЬAЇ+A���A��yA��yA��A��mA�+A�bA�
=A�
=A���AɍPAɃA�-A�ĜA�`BA�$�A��yA��`A��A�p�A��mA�5?A���A��A�dZA��;A�VA�t�A���A�ȴA�;dA�ƨA��-A�/A�r�A�E�A�hsA��TA�O�A�"�A��/A�E�A��TA�(�A��
A�bA�{A�r�A� �A�A�I�A�dZA�
=A���A��A��!A�x�A���A�"�A�A��A���A���A��^A�M�A��7A�l�A��9A�M�A�-A�7LA�ĜA�A�A�&�A��A���A��A���A�7LA��wA�S�A��DA~�RA{��AyVAvr�Ap�yAl�9AeXAa�Aa+A`$�A]|�A\  A[K�A[oAZ��AZ�RAY��AV(�AT5?AQ�hAO�^AM�AL=qAJ  AHbNAG��AFĜAC�A?x�A>JA:��A9oA8bA6ĜA6A4bNA1
=A.5?A-�A+�TA+G�A+;dA+;dA++A*��A(�A&ZA$�yA$�+A$9XA$A#�A"ȴA!��A v�A��A��A��Ar�AG�A�`A�jA��A^5A�A�A?}A%A��A��A�A�yA�\A�^AXA�`AQ�A��A��AQ�A�7A��AAG�A�A��Ap�A��AVAx�A	\)A�A�AO�A��AĜA�+AA�AjA �A�PA/@�-@�p�@��P@�=q@���@�O�@�?}@��
@�@�&�@�  @�p�@���@�`B@⟾@�@�j@�Z@�(�@�  @߾w@��H@�{@�G�@ܴ9@�Z@���@��y@�{@ف@�/@��;@��@֟�@�-@պ^@պ^@ՙ�@�O�@�%@ԣ�@�j@�(�@�b@Ӯ@ҧ�@�&�@���@�A�@͡�@�z�@�S�@�@���@�Q�@ǝ�@�t�@�t�@ǥ�@��
@ǍP@�
=@��#@�/@ċD@�I�@�I�@�|�@�M�@�@�-@+@�ȴ@���@¸R@��@� �@�S�@���@�~�@�-@��@�@���@���@�@�5?@�@�7L@�j@���@��y@��+@��@��@���@�J@�33@�K�@�;d@��@�~�@�v�@�M�@�J@��@�Q�@��@���@�n�@�=q@���@�/@��@�"�@��@��y@��@���@�ƨ@�Q�@�j@�9X@��@��@�M�@��-@�x�@�G�@��@��@��D@� �@���@��w@���@�l�@�K�@��@���@��@��!@�^5@�5?@�J@��-@�?}@��/@�A�@��
@���@�l�@�S�@�;d@�"�@�@��@��y@���@��R@���@�v�@�{@�O�@��@���@��/@���@���@��u@�r�@�r�@�Z@�(�@��
@�l�@�33@���@�M�@���@���@��-@���@�hs@��@��D@��;@���@�l�@�\)@�
=@��!@�M�@�$�@��@��@�@���@�O�@��@��/@��9@�Z@���@��@��@�;d@�"�@���@��@���@�V@��#@�`B@�G�@���@�j@�ƨ@��@�;d@��H@��\@�~�@��T@��-@��h@��@�X@�&�@�%@��@�Z@�b@��w@�C�@���@�V@�E�@�E�@�E�@�=q@�@��^@�O�@��@���@�z�@�bN@�9X@� �@�1@���@�ƨ@���@�t�@���@�ff@���@��#@���@���@��@�/@��u@�z�@�bN@��@��@��@��y@�ff@��^@�`B@���@�j@�1'G�O�@�z�@�V@�E�@�A�@w��@pr�@h �@["�@R��@HQ�@>V@:~�@4�@,9X@&��@ 1'@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	u�B	u�B	u�B	u�B	u�B	u�B	t�B	u�B	t�B	t�B	n�B	cTB	_;B	_;B	^5B	[#B	YB	ZB	ffB	k�B	n�B	p�B	u�B	�B	�}B	��B	�B	�B
"�B
�7B
ŢBuB?}BJ�BS�BbNB�+B��B�BĜB��B�#B�ZB�B�B��BuB'�B>wBH�BYBe`Bm�Bv�B�=B�{B��B��B�?B�LBBŢB��B�?B��BaHBA�BM�B�B��B�B��B��B�'B�=B=qB�B�B�B��B��B��B�7Bv�Bn�BffBK�B$�BoB
��B
�ZB
��B
ƨB
ĜB
�}B
�!B
��B
��B
�B
�B
�B
�B
|�B
XB
49B
(�B
�B
�B
%B	�B	��B	�qB	��B	�PB	x�B	N�B	9XB	49B	-B	 �B	�B	�B	{B	uB	hB	
=B��B�B�sB�NB�5B�B�B��B��BȴB�wB�XB�FB�RB�?B�!B�3B�RB�?B�LB�dB�dB��B�}B�}B�wB�qB�dB�dB�wB�}B�}B�}B�}B�wB�wB�wB��B��B��B��BĜBŢBƨBǮBƨBǮBǮBǮBɺB��B��BɺBɺBɺBɺB��BɺB��B��B��B��B�B�B�B�5B�5B�;B�HB�mB�yB�sB�mB�sB�B�B�yB�yB�B�B�B�B�B�B�mB�BB�;B�sB�sB�mB�sB�B�B�yB�fB�`B�NB�NB�ZB�yB�yB�B�B�yB�yB�sB�fB�fB�`B�fB�fB�fB�mB�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	B	%B	B	B	B	  B��B��B��B��B��B	  B	B	B	+B		7B	{B	�B	�B	!�B	#�B	%�B	$�B	&�B	(�B	.B	33B	7LB	;dB	<jB	;dB	;dB	=qB	@�B	B�B	C�B	D�B	F�B	J�B	O�B	Q�B	T�B	YB	ZB	[#B	ZB	YB	[#B	\)B	_;B	bNB	ffB	r�B	�B	�%B	�+B	�1B	�JB	�PB	�PB	�PB	�hB	�{B	�hB	�bB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	�B	�3B	�FB	�RB	�dB	�jB	�jB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	��B	B	B	B	B	B	B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�
B	�B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
PB
oB
�B
�B
�B
#�B
&�B
2-B
9XB
A�B
H�B
K�B
P�B
W
B
]/B
bNB
cT11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  B	u�B	u�B	u�B	u�B	u�B	u�B	t�B	u�B	t�B	t�B	n�B	cXB	_DB	_@B	^;B	[(B	YB	Z#B	fhB	k�B	n�B	p�B	u�B	�	B	�}B	��B	�~B	�B
"�B
�2B
ŝBnB?uBJ�BS�BbEB� B��B�BĒB��B�B�QB�B�B��BmB'�B>oBH�BY
BeUBm�Bv�B�5B�rB��B��B�7B�CBBŘB�~B�4B��Ba;BA�BM�B��B��B��B��B��B�B�5B=jB�B�B�B��B��B��B�-Bv�Bn�Bf^BK�B$�BeB
��B
�SB
��B
ƜB
ĕB
�wB
�B
��B
��B
�B
��B
��B
�B
|�B
X	B
46B
(�B
�B
}B
!B	�B	��B	�nB	��B	�PB	x�B	N�B	9ZB	4;B	-B	 �B	�B	�B	B	xB	nB	
@B��B�B�yB�TB�9B�"B�B��B��BȼB�~B�^B�NB�XB�EB�(B�9B�YB�DB�QB�jB�jB��B��B��B�~B�wB�hB�jB�{B��B��B��B��B�}B�}B�|B��B��B��B��BĢBũBƫBǯBƫBǯBǰBǳBɿB��B��BɾBɿBɾB��B��B��B��B��B��B�B�	B�B�"B�8B�8B�?B�KB�oB�}B�uB�qB�uB�B�B�~B�zB�B�B�B�B�B�B�nB�DB�<B�tB�uB�kB�tB�B�B�{B�iB�`B�QB�QB�[B�zB�yB�B�B�}B�zB�uB�gB�eB�^B�gB�hB�hB�oB�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	B	%B	B	B	B��B��B��B��B��B��B��B	B	B	)B		5B	}B	�B	�B	!�B	#�B	%�B	$�B	&�B	(�B	.B	30B	7IB	;aB	<gB	;bB	;cB	=pB	@�B	B�B	C�B	D�B	F�B	J�B	O�B	Q�B	T�B	YB	ZB	[ B	ZB	YB	[B	\'B	_8B	bJB	f`B	r�B	� B	�B	�%B	�-B	�DB	�LB	�LB	�LB	�dB	�xB	�bB	�^B	�^B	�_B	�cB	�pB	�B	��B	��B	��B	��B	��B	�B	�-B	�@B	�JB	�^B	�dB	�fB	�dB	�jB	�rB	�uB	�vB	�~B	�|B	��B	��B	��B	B	B	B	B	B	B	B	B	ÍB	ÍB	ėB	ĘB	ŚB	ƠB	ƣB	ǩB	ǦB	ǦB	ǧB	ǧB	ǧB	ǩB	ǧB	ǨB	ǦB	ǪB	ȫB	ʼB	˿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�(B	�3B	�:B	�DB	�KB	�RB	�QB	�SB	�QB	�_B	�^B	�gB	�lB	�kB	�wB	�uB	�|B	�}B	�|B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
B
	B
B

B
B
B
B
B
B
B
&B
)B
(G�O�B
FB
dB
vB
�B
�B
#�B
&�B
2%B
9LB
A|B
H�B
K�B
P�B
V�B
]#B
bDB
cI11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436352016080714363520160807143635  AO  ARCAADJP                                                                    20150603191652    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150603191652  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150603191652  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143635  IP                  G�O�G�O�G�O�                