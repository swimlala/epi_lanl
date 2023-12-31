CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:33Z AOML 3.0 creation; 2016-08-07T21:36:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221333  20160807143629  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_014                   2C  D   APEX                            6531                            072314                          846 @�#zj��1   @�#z�O��@1p��
=q�c��\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dyl�D�3D�C3D���D�� D�  D�6fD�0 D��fD�3D�S3D���Dǳ3D��D�I�D�vfD��3D��D�9�D�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�BĨ�BǨ�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�CT{CT{C!GC:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZT{C\T{C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DthRDy{�D�
�D�J�D��)D��\D�'\D�=�D�7\D���D��D�Z�D��)DǺ�D� �D�P�D�}�D�ڏD�)D�@�D�)D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�^5A�M�A�I�A�9XA�VA�oA��A�{A�7LA��A��yA�
=A��A��A�JA���A��`A���A���A׮Aץ�Aס�Aם�Aכ�Aי�Aס�Aק�Aק�Aף�Aץ�A׬A״9A׸RA׼jA�ƨA���A���A���A���A�ȴA�ȴA���A���A�ȴA׾wAש�Aׇ+A���A�ȴAƋDA��A�M�A���A��HA��7A�bNA�$�A��hA��A�n�A�+A���A��A�7LA��DA��A�ZA��RA��A�?}A�?}A���A�bA�ƨA��A��A�~�A�%A��`A�G�A��-A�VA���A��#A�bA�O�A�
=A�n�A�
=A�1'A�`BA�{A�-A�ffA�-A��+A�1A���A�^5A��;A��FA��A��A�\)A�-A���A��#A��HA}��At�uArAn�jAkƨAf��Ab5?A]�A[�^AW��AR�AQXALE�AHZAE�ACS�AA�A>�A:��A9�A9O�A6�A5O�A4��A3O�A2  A1/A/��A.1'A-/A+dZA)�FA(��A(ffA(1A't�A&z�A$E�A#G�A"I�A!"�A Q�A�mA"�A$�At�A�AJA�FA��A
=A�A�TA��AZA�mA��AS�A��A\)A�A�A��A\)A%AjA�
A%A��A-A��Ar�A\)AȴA�9Ar�A�A
�A	�PA	C�A��AffA5?A(�A�AbA�TAĜA�#A%AI�A��AffA�@��@�{@��@�ff@�{@��#@���@��T@�l�@�@���@�G�@��/@�ƨ@�M�@���@�r�@�b@�33@�@���@���@��@���@�\@���@�+@ᙚ@�1'@ߝ�@�"�@��@�V@��@ݡ�@�G�@ܬ@۝�@�M�@�x�@ؼj@��m@�
=@���@�1@��@��@��@���@�X@���@�1'@��m@�
=@�ȴ@ʏ\@�=q@ɉ7@�1'@Ə\@ũ�@��/@�9X@��m@Ý�@��H@§�@+@�=q@���@�@��-@��9@�A�@�dZ@��\@��H@���@���@��y@�C�@��@�K�@��@���@��^@���@�X@�?}@�%@�1@���@���@���@��@�|�@��P@��@���@�b@���@��;@���@�l�@�C�@��@�M�@��@���@�/@�z�@��@�(�@� �@�Q�@���@��/@�Ĝ@���@�z�@�j@�z�@�1@�o@�~�@�V@���@��\@�=q@��@�M�@�M�@�{@���@��/@� �@���@��@�"�@���@���@�`B@�%@���@���@�j@���@��P@��m@� �@��@��w@�\)@���@��!@�v�@�V@�@���@���@���@���@���@�r�@�Z@��F@�l�@���@�n�@���@��+@�v�@�n�@��+@�E�@��-@���@��h@�&�@��/@���@���@��j@�Ĝ@�z�@���@�;d@�~�@��^@�`B@��@��j@�I�@�ƨ@�o@�ȴ@�v�@�V@�$�@�@��h@��h@��@��F@�n�@��h@��#@���@��7@���@�O�@�z�@�|�@�+@��@�ȴ@�ȴ@�ȴ@��R@��!@���@�~�@�E�@�@���@�?}@���@�1'@��@��m@��;@���@���@�;d@���@�v�@�J@��@��#@��#@��-@�X@���@��@�9X@��
@���@�t�@�33@��@���@�^5@��@��@�@��^@��7@���@��@���@��D@�9X@���@��m@���@�S�@�33@�@��y@���@���@�~�@�^5@�=q@���@��@���@���@��7@�x�@�&�@��/@��9@�bN@�(�@��y@��@��@y�@pb@d�D@Y��@St�@N{@E�h@=��@7+@/�@'�w@!hs@9X@��@J@��@
��@A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�|�A�^5A�M�A�I�A�9XA�VA�oA��A�{A�7LA��A��yA�
=A��A��A�JA���A��`A���A���A׮Aץ�Aס�Aם�Aכ�Aי�Aס�Aק�Aק�Aף�Aץ�A׬A״9A׸RA׼jA�ƨA���A���A���A���A�ȴA�ȴA���A���A�ȴA׾wAש�Aׇ+A���A�ȴAƋDA��A�M�A���A��HA��7A�bNA�$�A��hA��A�n�A�+A���A��A�7LA��DA��A�ZA��RA��A�?}A�?}A���A�bA�ƨA��A��A�~�A�%A��`A�G�A��-A�VA���A��#A�bA�O�A�
=A�n�A�
=A�1'A�`BA�{A�-A�ffA�-A��+A�1A���A�^5A��;A��FA��A��A�\)A�-A���A��#A��HA}��At�uArAn�jAkƨAf��Ab5?A]�A[�^AW��AR�AQXALE�AHZAE�ACS�AA�A>�A:��A9�A9O�A6�A5O�A4��A3O�A2  A1/A/��A.1'A-/A+dZA)�FA(��A(ffA(1A't�A&z�A$E�A#G�A"I�A!"�A Q�A�mA"�A$�At�A�AJA�FA��A
=A�A�TA��AZA�mA��AS�A��A\)A�A�A��A\)A%AjA�
A%A��A-A��Ar�A\)AȴA�9Ar�A�A
�A	�PA	C�A��AffA5?A(�A�AbA�TAĜA�#A%AI�A��AffA�@��@�{@��@�ff@�{@��#@���@��T@�l�@�@���@�G�@��/@�ƨ@�M�@���@�r�@�b@�33@�@���@���@��@���@�\@���@�+@ᙚ@�1'@ߝ�@�"�@��@�V@��@ݡ�@�G�@ܬ@۝�@�M�@�x�@ؼj@��m@�
=@���@�1@��@��@��@���@�X@���@�1'@��m@�
=@�ȴ@ʏ\@�=q@ɉ7@�1'@Ə\@ũ�@��/@�9X@��m@Ý�@��H@§�@+@�=q@���@�@��-@��9@�A�@�dZ@��\@��H@���@���@��y@�C�@��@�K�@��@���@��^@���@�X@�?}@�%@�1@���@���@���@��@�|�@��P@��@���@�b@���@��;@���@�l�@�C�@��@�M�@��@���@�/@�z�@��@�(�@� �@�Q�@���@��/@�Ĝ@���@�z�@�j@�z�@�1@�o@�~�@�V@���@��\@�=q@��@�M�@�M�@�{@���@��/@� �@���@��@�"�@���@���@�`B@�%@���@���@�j@���@��P@��m@� �@��@��w@�\)@���@��!@�v�@�V@�@���@���@���@���@���@�r�@�Z@��F@�l�@���@�n�@���@��+@�v�@�n�@��+@�E�@��-@���@��h@�&�@��/@���@���@��j@�Ĝ@�z�@���@�;d@�~�@��^@�`B@��@��j@�I�@�ƨ@�o@�ȴ@�v�@�V@�$�@�@��h@��h@��@��F@�n�@��h@��#@���@��7@���@�O�@�z�@�|�@�+@��@�ȴ@�ȴ@�ȴ@��R@��!@���@�~�@�E�@�@���@�?}@���@�1'@��@��m@��;@���@���@�;d@���@�v�@�J@��@��#@��#@��-@�X@���@��@�9X@��
@���@�t�@�33@��@���@�^5@��@��@�@��^@��7@���@��@���@��D@�9X@���@��m@���@�S�@�33@�@��y@���@���@�~�@�^5@�=q@���@��@���@���@��7@�x�@�&�@��/@��9@�bNG�O�@��y@��@��@y�@pb@d�D@Y��@St�@N{@E�h@=��@7+@/�@'�w@!hs@9X@��@J@��@
��@A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�`B
�`B
�`B
�fB
�mB
�B
��B
��B{B5?B7LB;dBK�BR�B\)BbNBbNBdZBffBgmBgmBgmBgmBgmBgmBhsBjBl�Bm�Bm�Bm�Bo�Br�Bs�Bt�Bw�By�B}�B~�B~�B~�B~�B� B�B� B� B|�Bz�BdZBJBVBhBPB	7B
=BuB#�BO�BZBZBZB`BBhsB�B�bBx�BgmBcTBZBO�BI�BD�BB�BE�BcTB�jB��B��B�/BuBuB%B��B��B��B�B@�B<jB&�B{B�mBȴB��Bn�B:^B�BuB+BB
��B
�B
��B
�oB
u�B
cTB
C�B
,B
�B
%B	�
B	��B	�DB	s�B	[#B	:^B	"�B	\B	B�B�;B��BǮB�}B�FB�-B��B��B�B�B�B�B�B�B�B�B�B�!B�-B�3B�FB�XB�dB�jB�qB��B��B�BB�sB�B��B�B�B�B�B�`B�HB�mB�B�B�B�B��B��B	  B	B	B	B��B��B	+B	JB	\B	bB	oB	�B	�B	�B	�B	�B	�B	�B	bB	\B	�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	bB	PB	PB	
=B	B	B	B��B��B�B�B�B�B�mB�TB�B�B�B�B��B��B��B��B	  B	  B��B��B	B	B	B	B	B	B	+B		7B	JB	JB	bB	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	'�B	)�B	1'B	33B	0!B	)�B	&�B	&�B	,B	0!B	49B	8RB	;dB	A�B	A�B	A�B	@�B	A�B	G�B	K�B	L�B	N�B	Q�B	R�B	S�B	W
B	XB	ZB	[#B	`BB	dZB	ffB	e`B	ffB	ffB	gmB	jB	m�B	p�B	r�B	t�B	}�B	�B	�B	� B	�B	�%B	�1B	�1B	�7B	�7B	�VB	�\B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�FB	�dB	�dB	�wB	ÖB	ŢB	ŢB	ŢB	ÖB	ÖB	ŢB	ǮB	ǮB	ƨB	ƨB	ŢB	ŢB	ƨB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�/B	�;B	�BB	�TB	�TB	�ZB	�fB	�mB	�`B	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B
+B
PB
�B
�B
#�B
(�B
2-B
8RB
<jB
D�B
I�B
O�B
VB
\)B
bNB
gmB
l�B
q�B
t�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�SB
�RB
�PB
�VB
�_B
�oB
��B
��BjB5.B79B;RBK�BR�B\Bb;Bb=BdGBfUBg^Bg\Bg\Bg[Bg^Bg^BhbBjoBl{BmBmBm�Bo�Br�Bs�Bt�Bw�By�B}�B~�B~�B~�B~�B�B��B�B�B|�Bz�BdIB9BGBVB>B	&B
,BbB#�BO�BZBZ	BZ	B`/Bh^B�B�OBx�BgZBcABZ	BO�BI�BD�BBBE�BcAB�VB��B��B�B^BaBB��B��B��B�B@pB<VB&�BfB�UBȠB��Bn�B:GB�B`BBB
��B
�tB
ʬB
�]B
u�B
c@B
C�B
+�B
uB
B	��B	��B	�:B	s�B	[B	:UB	"�B	RB	B�B�4B��BǨB�xB�BB�(B��B��B�B�B��B�	B�B�B�B�B�B�B�&B�/B�?B�PB�]B�bB�hB��B��B�9B�iB�B��B�B�B�B�~B�UB�<B�eB�B�B�B�B��B��B��B	 B	 B	 �B��B��B	B	?B	RB	VB	dB	tB	zB	zB	�B	�B	�B	sB	YB	OB	yB	 �B	�B	�B	�B	�B	�B	�B	�B	B	�B	yB	tB	UB	BB	FB	
1B	B	B	 �B��B��B�B�B�B�~B�`B�KB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B		*B	<B	>B	WB	xB	�B	�B	�B	�B	�B	�B	$�B	%�B	'�B	)�B	1B	3#B	0B	)�B	&�B	&�B	+�B	0B	4)B	8BB	;TB	AxB	AxB	A{B	@sB	AzB	G�B	K�B	L�B	N�B	Q�B	R�B	S�B	V�B	W�B	ZB	[B	`3B	dIB	fRB	eMB	fUB	fSB	g\B	jlB	m~B	p�B	r�B	t�B	}�B	�B	��B	�B	��B	�B	�B	�B	�$B	�&B	�DB	�IB	�IB	�\B	�lB	�tB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�1B	�QB	�RB	�bB	�~B	ōB	ŎB	ŌB	ÂB	ÀB	ōB	ǙB	ǙB	ƑB	ƒB	ŎB	ŋB	ƓB	ȜB	ǙB	ǚB	ɧB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�,B	�?B	�>B	�DB	�PB	�WB	�JB	�OB	�]B	�^B	�^B	�cB	�uB	�uB	�tB	�|B	�{B	�vB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	�vB	�oB	�mB	�uB	�yB	�yB	�zB	�zB	�|B	�B	�{B	�{B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
�B
B
	B
B
B
G�O�B
B
6B
oB
�B
#�B
(�B
2B
88B
<PB
D�B
I�B
O�B
U�B
\B
b3B
gSB
lrB
q�B
t�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436292016080714362920160807143629  AO  ARCAADJP                                                                    20150226221333    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221333  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221333  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143629  IP                  G�O�G�O�G�O�                