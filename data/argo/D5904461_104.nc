CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-03T20:18:23Z AOML 3.0 creation; 2016-08-07T21:36:44Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160303201823  20160807143644  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               hA   AO  5286_8897_104                   2C  D   APEX                            6531                            072314                          846 @ך% ��1   @ך%�@�@36E�����cR�1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    hA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�fD��D�FfD��3D���D��D�FfD�|�D�� D�  D�6fD�s3D�ٚD��D�C3D�l�D��fD�	�D�0 D�s3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B��HB�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C=qC=qC#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN=qCP=qCR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�\Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy�\D�HD�J�D���D��HD�HD�J�D��HD��{D�{D�:�D�w�D��D�HD�G�D�qHD���D�D�4{D�w�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�O�A�Q�A�I�A�33A�1A�
=A�^5A���A�VA��A��A��
A��
A��
A���Aǲ-A�t�A�dZA�ZA�G�A�;dA��A���A��A��
AƶFAƍPA�v�Aũ�Aġ�AÏ\Aº^A�VA�ȴA�E�A�&�A�{A�O�A���A�K�A���A�33A��jA�z�A�%A��;A���A��yA�ffA�n�A��hA��A���A���A�Q�A��`A�M�A��A���A��A�S�A� �A���A�7LA�JA��A�A�I�A�z�A�"�A�A�+A�&�A��A���A�JA��A�~�A�33A�bA�|�A�&�A�XA��uA���A~ffA{�-AvbNAq"�An�Al�Aj��AhAe`BAbȴAa�wA^�!AZ�AX��AV1AUVAT^5AR�`AP��AM;dALE�AK��AJȴAI�#AF��AEoAD��AC&�ABbNAA��A@r�A>��A=�-A<�A;A:�DA:bA9�7A8��A7�#A7oA6v�A5`BA2n�A1&�A0��A0~�A0JA/hsA.z�A-��A,��A+��A*�A*��A)��A'��A%+A#��A"��A!�FA A�AG�AZA��A��A�A�A��A��A�wAC�A��Ap�Ar�A��A�mA�At�A��A��Ap�A
ĜA
9XA	S�AA�A�wA��A�A|�A33A��A�Ar�A-A�AdZA��AC�@�+@�`B@�;d@�^5@���@� �@��!@���@��@�l�@�^@�o@�Z@�dZ@�"�@��`@���@�33@�~�@��u@݉7@�|�@��H@��#@�X@��@���@؛�@�j@�A�@��@ץ�@�dZ@��y@���@Ӆ@�ff@ѡ�@�t�@Η�@���@�
=@��#@ɡ�@ɑh@���@� �@��@�=q@�V@�S�@�~�@��^@��@��7@���@�%@�^5@��-@�`B@��/@�A�@��
@�K�@�@��H@���@�@�O�@��/@��@�Q�@�b@�l�@��H@��\@���@���@� �@�  @�|�@�V@��^@��@��9@��D@�A�@��@��@��@���@�{@�z�@��@��\@��@��@��-@��7@��@�z�@��
@��@�C�@�o@���@��!@�M�@�=q@�5?@�{@�J@��@�%@��`@���@�z�@��@�\)@��y@�ȴ@���@��!@�ff@�E�@�E�@�E�@���@��7@�?}@�V@���@��`@��`@��`@���@�j@��@�"�@���@��@��#@�hs@���@��@��@�j@�I�@�I�@�A�@�  @��m@��F@�C�@��y@�M�@��@��@���@���@�G�@�7L@�V@��`@��`@��/@�Ĝ@��9@���@���@��u@��D@��@�r�@�I�@��;@��@�;d@��@��!@�v�@�^5@�M�@�M�@�=q@�J@���@��T@��h@�V@���@�bN@� �@��@��;@��;@��@�\)@�
=@��R@�^5@�E�@��@��@�G�@���@��@��/@�Ĝ@�z�@��m@��P@��y@��\@�M�@��@���@���@�X@�/@��/@��@��u@��D@�r�@�Q�@��
@��@���@��@�t�@�\)@�K�@�+@���@�^5@�@���@�O�@�%@���@��9@�bN@� �@��@��@�l�@��@��R@�v�@�ff@�M�@�@��#@�@�/@��@��j@�Z@��@��@��@�K�@�33@�+@��@�@��y@���@��+@�~�@�V@��@�`B@��@��`@��@�r�@�Z@�I�@�1'@�(�@� �@�b@�  @��@�@�P@�P@l�@~��@}@}�h@}O�@|�j@z�@tZ@ko@cdZ@^{@U��@Lz�@E`B@>�y@:M�@0�`@*��@&{@�P@J@��@��@\)@Z@��@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�K�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�O�A�Q�A�I�A�33A�1A�
=A�^5A���A�VA��A��A��
A��
A��
A���Aǲ-A�t�A�dZA�ZA�G�A�;dA��A���A��A��
AƶFAƍPA�v�Aũ�Aġ�AÏ\Aº^A�VA�ȴA�E�A�&�A�{A�O�A���A�K�A���A�33A��jA�z�A�%A��;A���A��yA�ffA�n�A��hA��A���A���A�Q�A��`A�M�A��A���A��A�S�A� �A���A�7LA�JA��A�A�I�A�z�A�"�A�A�+A�&�A��A���A�JA��A�~�A�33A�bA�|�A�&�A�XA��uA���A~ffA{�-AvbNAq"�An�Al�Aj��AhAe`BAbȴAa�wA^�!AZ�AX��AV1AUVAT^5AR�`AP��AM;dALE�AK��AJȴAI�#AF��AEoAD��AC&�ABbNAA��A@r�A>��A=�-A<�A;A:�DA:bA9�7A8��A7�#A7oA6v�A5`BA2n�A1&�A0��A0~�A0JA/hsA.z�A-��A,��A+��A*�A*��A)��A'��A%+A#��A"��A!�FA A�AG�AZA��A��A�A�A��A��A�wAC�A��Ap�Ar�A��A�mA�At�A��A��Ap�A
ĜA
9XA	S�AA�A�wA��A�A|�A33A��A�Ar�A-A�AdZA��AC�@�+@�`B@�;d@�^5@���@� �@��!@���@��@�l�@�^@�o@�Z@�dZ@�"�@��`@���@�33@�~�@��u@݉7@�|�@��H@��#@�X@��@���@؛�@�j@�A�@��@ץ�@�dZ@��y@���@Ӆ@�ff@ѡ�@�t�@Η�@���@�
=@��#@ɡ�@ɑh@���@� �@��@�=q@�V@�S�@�~�@��^@��@��7@���@�%@�^5@��-@�`B@��/@�A�@��
@�K�@�@��H@���@�@�O�@��/@��@�Q�@�b@�l�@��H@��\@���@���@� �@�  @�|�@�V@��^@��@��9@��D@�A�@��@��@��@���@�{@�z�@��@��\@��@��@��-@��7@��@�z�@��
@��@�C�@�o@���@��!@�M�@�=q@�5?@�{@�J@��@�%@��`@���@�z�@��@�\)@��y@�ȴ@���@��!@�ff@�E�@�E�@�E�@���@��7@�?}@�V@���@��`@��`@��`@���@�j@��@�"�@���@��@��#@�hs@���@��@��@�j@�I�@�I�@�A�@�  @��m@��F@�C�@��y@�M�@��@��@���@���@�G�@�7L@�V@��`@��`@��/@�Ĝ@��9@���@���@��u@��D@��@�r�@�I�@��;@��@�;d@��@��!@�v�@�^5@�M�@�M�@�=q@�J@���@��T@��h@�V@���@�bN@� �@��@��;@��;@��@�\)@�
=@��R@�^5@�E�@��@��@�G�@���@��@��/@�Ĝ@�z�@��m@��P@��y@��\@�M�@��@���@���@�X@�/@��/@��@��u@��D@�r�@�Q�@��
@��@���@��@�t�@�\)@�K�@�+@���@�^5@�@���@�O�@�%@���@��9@�bN@� �@��@��@�l�@��@��R@�v�@�ff@�M�@�@��#@�@�/@��@��j@�Z@��@��@��@�K�@�33@�+@��@�@��y@���@��+@�~�@�V@��@�`B@��@��`@��@�r�@�Z@�I�@�1'@�(�@� �@�b@�  @��@�@�P@�P@l�@~��@}@}�h@}O�G�O�@z�@tZ@ko@cdZ@^{@U��@Lz�@E`B@>�y@:M�@0�`@*��@&{@�P@J@��@��@\)@Z@��@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�JB	�PB	�hB	��B	��B	��B	�RB	��B	�TB	�B	��B
+B
	7B

=B

=B
\B
JB
DB
	7B
+B
%B
B
  B
B

=B
DB
\B
�B
�B
$�B
,B
>wB
p�B
�'B1BT�Bs�Bt�B�+B�uB��B��B��B�hB�hB�hB�oB��B��B	7BJB�BƨB�dB�RB�jB�FB�qBƨBŢB�dB��B~�Bs�Bo�Bm�BiyB`BBM�B7LB&�B1B
��B
�B
�NB
�jB
��B
��B
�B
cTB
VB
1'B
�B
B	�/B	�LB	��B	}�B	\)B	J�B	=qB	-B	�B	JB��B��B�NB��B��BȴBĜBĜB��B�jB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�?B�?B�9B�-B�'B�B�B��B��B��B��B��B��B�bB�\B�bB�hB�hB�hB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�\B�PB�\B�VB�PB�PB�PB�PB�PB�JB�=B�=B�1B�%B�B�B�B�7B�7B�JB�JB�=B�JB�\B�bB�\B�VB�hB��B�oB�oB�uB�oB�JB�1B�%B�B�%B�=B�=B�JB�JB�JB�PB�PB�PB�PB�PB�PB�PB�PB�PB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�9B�FB�XB�dB�jB�qB��BÖBǮBȴB��B��B��B��B��B�)B�;B�BB�BB�NB�mB�yB�B�B��B��B��B��B	  B	  B	B	JB	�B	�B	"�B	#�B	%�B	&�B	)�B	/B	49B	6FB	8RB	:^B	:^B	<jB	?}B	@�B	@�B	@�B	@�B	D�B	G�B	H�B	H�B	J�B	M�B	P�B	S�B	T�B	VB	W
B	\)B	_;B	_;B	_;B	cTB	dZB	ffB	gmB	hsB	hsB	hsB	hsB	hsB	k�B	p�B	r�B	u�B	y�B	{�B	}�B	�B	�B	�B	�B	�+B	�+B	�1B	�=B	�DB	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	�wB	�}B	��B	��B	��B	B	B	ĜB	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�;B	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
	7B

=B

=B
DB
PB
oB
�B
$�B
(�B
1'B
9XB
>wB
D�B
H�B
Q�B
XB
]/B
cTB
gmB
jB
n�B
q�B
t�B
x�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�7B	�:B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�JB	�VB	�kB	��B	��B	��B	�RB	��B	�RB	�B	��B
(B
	4B

9B

8B
[B
IB
?B
	4B
*B
"B
B	��B
B

8B
@B
YB
�B
�B
$�B
,B
>sB
p�B
� B)BT�Bs�Bt�B� B�gB��B��B��B�\B�\B�\B�dB��B��B	-B:B�rBƜB�XB�DB�_B�9B�eBƛBŔB�YB��B~�Bs�Bo�Bm�BipB`7BM�B7?B&�B'B
��B
�B
�CB
�]B
��B
��B
�B
cKB
U�B
1!B
�B
B	�+B	�IB	��B	}�B	\)B	J�B	=qB	-B	�B	KB��B��B�SB��B��BȹBģBĢB��B�nB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�>B�AB�CB�=B�0B�*B�B�B� B��B��B��B��B��B�fB�cB�hB�lB�mB�qB�nB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�xB�sB�iB�aB�TB�aB�ZB�SB�UB�TB�TB�TB�LB�CB�@B�6B�(B� B�B�B�;B�;B�LB�LB�?B�LB�`B�iB�_B�ZB�lB��B�tB�rB�vB�tB�KB�4B�)B�B�(B�?B�?B�LB�LB�JB�QB�SB�TB�SB�SB�PB�SB�TB�QB�fB�pB�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�	B�B�B�B�B�#B�<B�FB�WB�dB�hB�qB��BÔBǫBȴBʿB��B��B��B��B�&B�9B�>B�@B�KB�kB�xB�B�B��B��B��B��B��B��B	B	GB	�B	�B	"�B	#�B	%�B	&�B	)�B	/B	45B	6BB	8MB	:YB	:YB	<dB	?vB	@~B	@|B	@|B	@}B	D�B	G�B	H�B	H�B	J�B	M�B	P�B	S�B	T�B	U�B	WB	\#B	_3B	_5B	_3B	cMB	dUB	f`B	gfB	hmB	hmB	hmB	hkB	hmB	k~B	p�B	r�B	u�B	y�B	{�B	}�B	��B	�B	�B	�B	�"B	�$B	�&B	�6B	�;B	�AB	�SB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�*B	�/B	�7B	�3B	�5B	�<B	�CB	�AB	�BB	�HB	�MB	�[B	�`B	�hB	�lB	�tB	�xB	�B	�~B	B	B	ĔB	ĒB	ƝB	ɯB	ɱB	ʶB	˻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�/B	�;B	�AB	�HB	�IB	�NB	�UB	�]B	�cB	�kB	�oB	�oB	�mB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
 B
�B
 B
�B
B
B
B
B
B
B
B
B
B
B
B
B
!B
!B
 B
 B
 B
 B
!B
'B
$B
$B
&B
'B
%B
	+B

2B

/G�O�B
BB
aB
�B
$�B
(�B
1B
9IB
>jB
D�B
H�B
Q�B
XB
] B
cFB
g`B
joB
n�B
q�B
t�B
x�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436442016080714364420160807143644  AO  ARCAADJP                                                                    20160303201823    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160303201823  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160303201823  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143644  IP                  G�O�G�O�G�O�                