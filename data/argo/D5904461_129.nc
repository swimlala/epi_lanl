CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-13T18:01:38Z AOML 3.0 creation; 2016-08-07T21:36:48Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160713180138  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286_8897_129                   2C  D   APEX                            6531                            072314                          846 @׻�WI�1   @׻8�;@6h1&�x��c z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�3D�3D�I�D�p D���D��D�L�D�p D�� D�fD�6fD�|�D��3D� D�@ D�@ D��fD�fD�9�D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@��
A�A!�AA�Aa�A���A���A���A�A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`�GBh�GBpz�Bxz�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:8RC<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�DJDJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtt{Dy��D�
D�MqD�s�D��D��D�P�D�s�D���D�
=D�:=D���D��
D��D�C�D�C�D��=D�
=D�=qD�mqD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��TA��HA��;A��;A��;A��/A��;A��/A��
A���A�Aͣ�A͏\A�~�A�ffA�5?A��A�%A���A��/A���A���A̴9A̧�A̟�ȂhA�\)A�1'A˙�Aʥ�A�  A��#A�dZA��
A�1A���AǑhA��AƧ�A��HA�O�A�|�A�E�A� �A��A�ĜA��TA���A��7A���A��A��hA�
=A�/A�VA���A�l�A�33A���A�&�A�"�A�C�A�9XA��A�p�A��A��A��uA�t�A���A���A��mA�\)A�bNA��A���A�G�A��TA�/A���A�`BA��FA��A��A���A�%A��mA��DA�7LA�|�A�A�5?A��jA���A��A��9A��A�9XA�x�A���A�z�A��A��^A�|�A��A�?}A��A�ffA�`BA�-A��;A�l�A�\)A��uA�A���A�33A�A���A�+A�FA~JA{dZAxM�AuXAs�ArbAohsAl��Ak�#AjZAhM�Ae�7Ab��A_A]�mA\�A\  AY��AU�AS�FAOK�AJ��AG�^ADbNACO�ABQ�A@�HA?ƨA>  A;"�A8�!A61'A5A5x�A4��A3�PA2��A2A1`BA0��A/O�A. �A-hsA+��A*�+A)O�A'��A%�#A$�A#��A#��A#S�A!K�A �A�
AJAVA�7A�RA�A`BA�9AQ�AJA/A��A"�A-AƨA�^A�A�PAS�A��A�A��AoA7LA�uA/A1A^5A��A��A�jAI�A�;A�;Ap�A9XA�FAx�A`BA`BA/A�A�`AA�A	�A�A��AE�A�wAx�AS�A~�AC�A�AȴA�\A33A   @��T@�V@��@��F@��!@��^@�+@�A�@�~�@�(�@�+@�bN@�ff@��@�dZ@�n�@��@�F@�+@�ȴ@�7@ߕ�@��@ݺ^@�I�@�+@�5?@�x�@�%@���@���@��@��/@؃@�ƨ@�33@�o@���@֟�@֏\@�~�@�$�@�`B@Դ9@ԣ�@�j@�(�@��@��@��@�V@ѡ�@�`B@��/@�I�@�1@ύP@θR@�5?@���@��@��@�^5@�O�@�j@���@�dZ@ʧ�@�V@�t�@ř�@�V@ļj@�1@��@\@�-@��^@��7@�G�@�Ĝ@�bN@�  @��@�@�v�@�-@���@�%@��@�\)@�\)@���@�\)@�
=@�@�^5@�7L@�I�@�dZ@�33@�v�@�X@�{@��@��-@��@�%@�%@��j@���@��u@�Q�@�K�@�~�@�n�@��7@� �@��H@�hs@�X@�X@�V@��u@�I�@�(�@�1@��m@�Q�@�z�@�Z@�I�@�9X@�A�@��@���@�z�@�Z@�A�@�9X@��@�+@�~�@��R@�dZ@�|�@��!@�M�@�M�@�$�@��@���@���@�/@�Ĝ@�1'@�1'@�b@��@�K�@��H@���@�7L@�bN@�b@��w@��P@��@�ȴ@�v�@�ff@�M�@��@��-@��@�x�@�hs@�&�@��@�r�@�9X@�ƨ@�ƨ@���@�dZ@�;d@�@�ȴ@�M�@���@�p�@�7L@��/@���@��@�z�@�9X@��@���@��@��;@���@�33@�
=@��y@��@�ȴ@�ȴ@���@��R@��!@���@�v�@�J@�@��T@���@�x�@�hs@�`B@�/@���@��/@��j@���@�z�@�Q�@�b@��;@�;d@��y@�ȴ@���@���@��+@�$�@��^@�x�@�?}@��@��D@��P@���@��\@���@��#@��h@�%@��/@���@���@�z�@�|�@�o@�`B@���@�t�@~�+@v$�@k��@`bN@Z�@T�@O�@Fȴ@@Ĝ@:�@3��@-`B@*J@$�@ r�@j@��@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��HA��TA��HA��;A��;A��;A��/A��;A��/A��
A���A�Aͣ�A͏\A�~�A�ffA�5?A��A�%A���A��/A���A���A̴9A̧�A̟�ȂhA�\)A�1'A˙�Aʥ�A�  A��#A�dZA��
A�1A���AǑhA��AƧ�A��HA�O�A�|�A�E�A� �A��A�ĜA��TA���A��7A���A��A��hA�
=A�/A�VA���A�l�A�33A���A�&�A�"�A�C�A�9XA��A�p�A��A��A��uA�t�A���A���A��mA�\)A�bNA��A���A�G�A��TA�/A���A�`BA��FA��A��A���A�%A��mA��DA�7LA�|�A�A�5?A��jA���A��A��9A��A�9XA�x�A���A�z�A��A��^A�|�A��A�?}A��A�ffA�`BA�-A��;A�l�A�\)A��uA�A���A�33A�A���A�+A�FA~JA{dZAxM�AuXAs�ArbAohsAl��Ak�#AjZAhM�Ae�7Ab��A_A]�mA\�A\  AY��AU�AS�FAOK�AJ��AG�^ADbNACO�ABQ�A@�HA?ƨA>  A;"�A8�!A61'A5A5x�A4��A3�PA2��A2A1`BA0��A/O�A. �A-hsA+��A*�+A)O�A'��A%�#A$�A#��A#��A#S�A!K�A �A�
AJAVA�7A�RA�A`BA�9AQ�AJA/A��A"�A-AƨA�^A�A�PAS�A��A�A��AoA7LA�uA/A1A^5A��A��A�jAI�A�;A�;Ap�A9XA�FAx�A`BA`BA/A�A�`AA�A	�A�A��AE�A�wAx�AS�A~�AC�A�AȴA�\A33A   @��T@�V@��@��F@��!@��^@�+@�A�@�~�@�(�@�+@�bN@�ff@��@�dZ@�n�@��@�F@�+@�ȴ@�7@ߕ�@��@ݺ^@�I�@�+@�5?@�x�@�%@���@���@��@��/@؃@�ƨ@�33@�o@���@֟�@֏\@�~�@�$�@�`B@Դ9@ԣ�@�j@�(�@��@��@��@�V@ѡ�@�`B@��/@�I�@�1@ύP@θR@�5?@���@��@��@�^5@�O�@�j@���@�dZ@ʧ�@�V@�t�@ř�@�V@ļj@�1@��@\@�-@��^@��7@�G�@�Ĝ@�bN@�  @��@�@�v�@�-@���@�%@��@�\)@�\)@���@�\)@�
=@�@�^5@�7L@�I�@�dZ@�33@�v�@�X@�{@��@��-@��@�%@�%@��j@���@��u@�Q�@�K�@�~�@�n�@��7@� �@��H@�hs@�X@�X@�V@��u@�I�@�(�@�1@��m@�Q�@�z�@�Z@�I�@�9X@�A�@��@���@�z�@�Z@�A�@�9X@��@�+@�~�@��R@�dZ@�|�@��!@�M�@�M�@�$�@��@���@���@�/@�Ĝ@�1'@�1'@�b@��@�K�@��H@���@�7L@�bN@�b@��w@��P@��@�ȴ@�v�@�ff@�M�@��@��-@��@�x�@�hs@�&�@��@�r�@�9X@�ƨ@�ƨ@���@�dZ@�;d@�@�ȴ@�M�@���@�p�@�7L@��/@���@��@�z�@�9X@��@���@��@��;@���@�33@�
=@��y@��@�ȴ@�ȴ@���@��R@��!@���@�v�@�J@�@��T@���@�x�@�hs@�`B@�/@���@��/@��j@���@�z�@�Q�@�b@��;@�;d@��y@�ȴ@���@���@��+@�$�@��^@�x�@�?}@��@��D@��P@���@��\@���@��#@��h@�%@��/@���@���@�z�@�|�G�O�@�`B@���@�t�@~�+@v$�@k��@`bN@Z�@T�@O�@Fȴ@@Ĝ@:�@3��@-`B@*J@$�@ r�@j@��@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
oB
bB
\B
\B
VB
VB
PB
PB
JB
JB
JB
PB
\B
oB
�B
$�B
D�B
dZB
�}B
��B�B=qBH�BC�B<jBPB
�HB
��B
��B
��B
�sB
��BB%�B,B�B$�B^5BiyBcTB�VB�XB��BĜB��B��B+B+B��BL�BXBD�BA�B8RB@�BP�B^5BjB{�B{�Bz�By�B{�By�Bs�Be`BT�BG�BB�B49BVB��BPB�B�B�B�B{B�B$�B�B
=B��B�5B��B��Bp�BN�B-BbB
�B
�B
�FB
��B
�oB
�B
�JB
�uB
�!B
�wB
�'B
��B
�=B
iyB
G�B
1'B
�B
VB	��B	�TB	��B	�XB	�'B	��B	�uB	�PB	�%B	x�B	gmB	VB	C�B	:^B	2-B	(�B	�B	B�B�B�^B�B��B��B��B�hB�PB�Bs�BcTB\)B\)B[#B[#B\)B^5BdZBgmBhsBiyBe`BdZBffBe`BdZB^5B^5BZBYBaHBbNBW
B[#B`BBZB]/B[#BW
BS�BVBVBVBVBVBZB]/B[#B\)B\)B\)BbNBcTBjBdZBl�Bs�By�B��B��B�jBÖB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��BɺBB�}B�qB�dB�^B�jB��B��B��BB��B�qB�RB�9B�3B�'B�B�B�B��B��B��B��B��B��B�{B�{B�{B��B��B��B��B��B��B�B�B�B�3B�FB�XBÖB��B�B�B�)B�/B�HB�B�B�B�B�B�B�B��B��B	B	B	B	B	B	B	1B		7B	1B	1B	1B		7B		7B		7B	JB	JB	JB	JB	\B	�B	�B	!�B	!�B	%�B	%�B	$�B	#�B	"�B	%�B	+B	,B	-B	.B	.B	.B	-B	.B	1'B	2-B	2-B	2-B	2-B	7LB	9XB	=qB	>wB	>wB	>wB	?}B	D�B	G�B	H�B	I�B	J�B	J�B	G�B	E�B	E�B	B�B	B�B	M�B	N�B	O�B	Q�B	S�B	VB	W
B	XB	YB	ZB	XB	XB	XB	VB	T�B	R�B	Q�B	Q�B	Q�B	T�B	[#B	[#B	\)B	\)B	]/B	gmB	k�B	n�B	p�B	q�B	u�B	}�B	� B	� B	~�B	� B	�B	�B	�%B	�DB	�VB	�uB	��B	�{B	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�RB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�}B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�)B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�NB	�TB	�NB	�TB	�B	��B
%B
VB
�B
�B
&�B
.B
/B
7LB
?}B
A�B
H�B
K�B
P�B
T�B
YB
]/B
aHB
ffB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~B
zB
tB
kB
mB
`B
YB
[B
WB
VB
LB
MB
GB
HB
JB
NB
ZB
kB
�B
$�B
D�B
dVB
�vB
��B�B=jBH�BC�B<_BGB
�FB
��B
ʻB
��B
�nB
��BB%�B+�B�B$�B^-BinBcMB�MB�LB�BēBʶB��B*�BB��BL�BXBD�BA}B8IB@wBP�B^*BjuB{�B{�Bz�By�B{�By�Bs�BeVBT�BG�BB�B41BJB��BFB�B�B�BwBrB�B$�B�B
2B��B�*B��B��Bp�BN�B-BWB
�B
��B
�=B
��B
�fB
�
B
�DB
�mB
�B
�mB
�B
��B
�7B
isB
G�B
1 B
�B
RB	��B	�RB	��B	�TB	�%B	��B	�uB	�PB	�#B	x�B	gnB	VB	C�B	:`B	2.B	(�B	�B	#B�B�B�gB�B��B��B��B�rB�[B�(Bs�Bc^B\3B\4B[-B[/B\3B^@BdeBgxBh}Bi�BelBdcBfqBefBdbB^BB^BBZ(BY BaTBbXBWB[.B`LBZ(B]9B[-BWBTBVBVBVBVBVBZ#B]4B[/B\4B\1B\1BbWBc\Bj�BdeBl�Bs�By�B��B��B�lBÚB��B��B��B��B��B��B��B��B��B��B�B�B�#B�"B�B�BɿBB��B�tB�fB�dB�oB��B��B��BB��B�vB�XB�;B�8B�+B�B�B�	B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�6B�LB�YBØB��B�B� B�+B�0B�IB�B�B�B�B�B�B�B��B��B	B	B	B	B	B	 B	1B		6B	3B	2B	1B		5B		9B		9B	GB	HB	JB	IB	\B	�B	�B	!�B	!�B	%�B	%�B	$�B	#�B	"�B	%�B	+B	,B	-B	.B	.B	.B	-B	.B	1#B	2,B	2,B	2+B	2)B	7JB	9WB	=nB	>rB	>tB	>sB	?yB	D�B	G�B	H�B	I�B	J�B	J�B	G�B	E�B	E�B	B�B	B�B	M�B	N�B	O�B	Q�B	S�B	VB	WB	XB	YB	ZB	XB	XB	XB	U�B	T�B	R�B	Q�B	Q�B	Q�B	T�B	[ B	[B	\&B	\&B	],B	giB	k�B	n�B	p�B	q�B	u�B	}�B	�B	�B	~�B	�B	�B	�B	�B	�>B	�OB	�oB	��B	�vB	�vB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�*B	�-B	�3B	�9B	�7B	�MB	�PB	�XB	�`B	�[B	�cB	�fB	�lB	�jB	�tB	B	ÏB	ĔB	řB	ŜB	ŚB	řB	řB	ŜB	ơB	ŚB	ŜB	śB	ŚB	ƢB	ǧB	ǥB	ǧB	ȮB	ɴB	ɲB	ɴB	ɳB	��B	��B	˿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	� B	�)B	�.B	�B	�B	�B	�B	�B	�B	�B	�/B	�5B	�CB	�LB	�GG�O�B	�B	��B
B
LB
uB
�B
&�B
.B
/B
7AB
?pB
A�B
H�B
K�B
P�B
T�B
Y
B
]%B
a=B
fZB
k{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436482016080714364820160807143648  AO  ARCAADJP                                                                    20160713180138    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160713180138  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160713180138  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143648  IP                  G�O�G�O�G�O�                