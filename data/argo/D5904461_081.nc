CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-03T10:16:37Z AOML 3.0 creation; 2016-08-07T21:36:40Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151103101637  20160807143640  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               QA   AO  5286_8897_081                   2C  D   APEX                            6531                            072314                          846 @�{ͦN'l1   @�{�>��I@2��1&��c333331   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    QA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB���B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy�fD��D�P D�� D���D�3D�FfD�|�D�� D��D�P D�l�DǶfD��D�P Dډ�D�� D��D�FfD�i�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B��B��HB�G�B�G�B�G�B�G�B�G�B��B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�\D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt\)Dy�\D�!HD�T{D��{D��D��D�J�D��HD��{D�HD�T{D�qHDǺ�D�!HD�T{DڎD��{D�D�J�D�nD��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�G�A�M�A�XA�\)A�\)A�\)A�`BA�`BA�dZA�l�A�v�A�|�AܑhAܗ�Aܙ�Aܝ�Aܴ9A�ȴA�$�A��`Aݙ�A�G�A��/A�v�A�M�A�M�A��A��yA�t�A�7LA��A��mA�ȴAոRA�+A��Aҙ�AЛ�A���A�l�A�hsA��A�A�hsA�t�A��wA�-A�oA�z�A�&�A�K�A���A�r�A��A�n�A��
A��9A���A�v�A��A��/A�7LA��A���A�oA�C�A��FA��A�bA��wA��A�$�A�`BA�A�JA�
=A�A�^5A��!A�l�A��!A�v�A�bNA�A�r�A��A��uA�Q�A���A���A�7LA��+A��A���A�ZA���A�l�A�K�A�=qA��!A�1A��A���A��-A��A��A�z�A�^5A��hA���A~Q�Ax�yAu�^As7LAn$�Aj�Af�jAdr�A_��A^��A]7LAZĜAW�AU�ATZASK�AQ��AO��ANz�AK��AH�DAFAD�9AC`BAA`BA?"�A>{A<�`A;�A:$�A7��A4z�A2��A0Q�A.I�A,n�A+;dA*��A*z�A)�FA(�A'��A'�#A'/A&��A&Q�A%��A$�jA$  A#�A"��A!&�A ��A z�A �A�#AC�A��A1'AXAXA�;A��A�A�jA?}A �Av�AƨA|�A��A�uA�A�AVA	�PA	/A�RA1A;dAM�AVAp�A�A ��@��m@�M�@��@���@��\@��`@��P@�;d@���@�D@�@�7L@��@���@�"�@�G�@�9X@�V@旍@�r�@�{@�@�V@ߝ�@�~�@�1@��@Ցh@�1'@�;d@���@�^5@щ7@Ѓ@�@��T@ϕ�@җ�@ϝ�@�?}@͑h@���@��`@Ώ\@�-@��@�&�@�=q@�^5@ʟ�@��
@�v�@�^5@ċD@ǍP@���@��m@�$�@�x�@���@�r�@�=q@϶F@��
@θR@́@��
@�G�@�@�S�@�
=@�p�@��@���@�@���@��@�@�v�@��;@�|�@��`@�K�@�V@�?}@���@�"�@��!@�n�@�=q@�$�@�M�@�n�@��\@�ȴ@��y@�$�@��@���@�l�@���@�{@�1'@��9@��9@��j@��D@�Z@��u@�Ĝ@��j@��u@�r�@�bN@�Q�@�9X@�ƨ@�
=@��H@���@�5?@���@���@��7@�7L@�V@��@�7L@���@�Q�@�ƨ@�O�@���@�Z@�1@�z�@�I�@���@�1'@�o@�S�@��H@��@��T@��^@��@���@��9@��@��@�j@�9X@�9X@�9X@�9X@�1'@�I�@�1'@� �@�j@�Z@�b@���@�M�@��h@�p�@���@�&�@��/@���@�9X@���@�33@���@�G�@���@�Ĝ@���@�Q�@��m@�+@�ff@�@���@�x�@��@�r�@���@��@�1'@� �@�ƨ@�S�@�33@�33@��@�
=@�o@�o@��y@��R@�ff@�-@���@� �@�33@�^5@�X@��@���@�Ĝ@�r�@�Q�@�Q�@�(�@�1@��w@��@�\)@��@��\@�M�@��h@�?}@�V@���@�7L@���@���@���@��R@���@�ff@�E�@�-@��@��T@���@�O�@���@��9@��D@��D@��D@�1'@���@��
@��@�dZ@��@���@��R@�ȴ@��@�"�@���@���@��@��@���@�7L@���@��@���@��9@��@�1'@��@��
@��w@�|�@�t�@��P@�;d@��y@���@��R@���@�@��-@��^@���@�V@���@��/@��/@���@��@���@���@��w@��@��!@z��@pA�@g�@_+@V��@O�P@I��@Co@<j@5��@/|�@'�@#�F@$�@��@��@/@�@Q�@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�I�A�G�A�M�A�XA�\)A�\)A�\)A�`BA�`BA�dZA�l�A�v�A�|�AܑhAܗ�Aܙ�Aܝ�Aܴ9A�ȴA�$�A��`Aݙ�A�G�A��/A�v�A�M�A�M�A��A��yA�t�A�7LA��A��mA�ȴAոRA�+A��Aҙ�AЛ�A���A�l�A�hsA��A�A�hsA�t�A��wA�-A�oA�z�A�&�A�K�A���A�r�A��A�n�A��
A��9A���A�v�A��A��/A�7LA��A���A�oA�C�A��FA��A�bA��wA��A�$�A�`BA�A�JA�
=A�A�^5A��!A�l�A��!A�v�A�bNA�A�r�A��A��uA�Q�A���A���A�7LA��+A��A���A�ZA���A�l�A�K�A�=qA��!A�1A��A���A��-A��A��A�z�A�^5A��hA���A~Q�Ax�yAu�^As7LAn$�Aj�Af�jAdr�A_��A^��A]7LAZĜAW�AU�ATZASK�AQ��AO��ANz�AK��AH�DAFAD�9AC`BAA`BA?"�A>{A<�`A;�A:$�A7��A4z�A2��A0Q�A.I�A,n�A+;dA*��A*z�A)�FA(�A'��A'�#A'/A&��A&Q�A%��A$�jA$  A#�A"��A!&�A ��A z�A �A�#AC�A��A1'AXAXA�;A��A�A�jA?}A �Av�AƨA|�A��A�uA�A�AVA	�PA	/A�RA1A;dAM�AVAp�A�A ��@��m@�M�@��@���@��\@��`@��P@�;d@���@�D@�@�7L@��@���@�"�@�G�@�9X@�V@旍@�r�@�{@�@�V@ߝ�@�~�@�1@��@Ցh@�1'@�;d@���@�^5@щ7@Ѓ@�@��T@ϕ�@җ�@ϝ�@�?}@͑h@���@��`@Ώ\@�-@��@�&�@�=q@�^5@ʟ�@��
@�v�@�^5@ċD@ǍP@���@��m@�$�@�x�@���@�r�@�=q@϶F@��
@θR@́@��
@�G�@�@�S�@�
=@�p�@��@���@�@���@��@�@�v�@��;@�|�@��`@�K�@�V@�?}@���@�"�@��!@�n�@�=q@�$�@�M�@�n�@��\@�ȴ@��y@�$�@��@���@�l�@���@�{@�1'@��9@��9@��j@��D@�Z@��u@�Ĝ@��j@��u@�r�@�bN@�Q�@�9X@�ƨ@�
=@��H@���@�5?@���@���@��7@�7L@�V@��@�7L@���@�Q�@�ƨ@�O�@���@�Z@�1@�z�@�I�@���@�1'@�o@�S�@��H@��@��T@��^@��@���@��9@��@��@�j@�9X@�9X@�9X@�9X@�1'@�I�@�1'@� �@�j@�Z@�b@���@�M�@��h@�p�@���@�&�@��/@���@�9X@���@�33@���@�G�@���@�Ĝ@���@�Q�@��m@�+@�ff@�@���@�x�@��@�r�@���@��@�1'@� �@�ƨ@�S�@�33@�33@��@�
=@�o@�o@��y@��R@�ff@�-@���@� �@�33@�^5@�X@��@���@�Ĝ@�r�@�Q�@�Q�@�(�@�1@��w@��@�\)@��@��\@�M�@��h@�?}@�V@���@�7L@���@���@���@��R@���@�ff@�E�@�-@��@��T@���@�O�@���@��9@��D@��D@��D@�1'@���@��
@��@�dZ@��@���@��R@�ȴ@��@�"�@���@���@��@��@���@�7L@���@��@���@��9@��@�1'@��@��
@��w@�|�@�t�@��P@�;d@��y@���@��R@���@�@��-@��^@���@�V@���@��/@��/@���@��@���@���@��wG�O�@��!@z��@pA�@g�@_+@V��@O�P@I��@Co@<j@5��@/|�@'�@#�F@$�@��@��@/@�@Q�@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�mB	�mB	�fB	�fB	�mB	�fB	�mB	�sB	�mB	�yB	�B	��B	��B
�B
0!B
6FB
>wB
K�B
T�B
�7BM�B`BBbNB]/B_;BiyBo�B�B��B�9B�qB�qB��B�B�B33BH�BffB� B�RB��B�)B�HB�B�BB	7BDB
=BDBPBPBPBVBPBhBuBuBhBVB
=B	7BPB
=B1BBBB��B��B��B�B�B�`B�B��BŢBĜB�RB��B�PB�B|�Bz�Bt�BgmB?}B#�BPB��B�mB��B�wB�B�DBVB+B&�B�B
�B
�TB
ƨB
��B
��B
�%B
hsB
S�B
<jB
'�B
�B
DB	�B	ȴB	�'B	��B	� B	bNB	M�B	;dB	!�B	�B	oB	B�B�B�`B�NB�#B�)B�B��BÖB�jB�RB�9B�!B�B�B��B��B��B��B��B��B�oB�JB�VB�bB�\B�VB�JB�VB�bB�\B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�B�B��B��B��B��B��B��B��B� Bz�Bv�Bt�By�Bx�Bx�Bv�Bt�Bm�BhsB^5B[#B\)Bl�BjBgmBe`BhsBs�Bx�Bu�Bx�Bt�B|�B}�By�Bt�Bp�BjBdZBaHBaHBdZBcTB`BB\)BT�BO�BO�BR�BQ�BQ�BQ�BO�BN�BO�BT�BhsB{�Bq�BgmBl�Bm�Bu�B�%B�DB�JB�1B��B��B��B��BŢB�3B��B�3B��B�BB��B��B��B��B		7B	{B	�B	�B	oB	VB	B�B��B��B��B�B�B�fB�mB�fB�ZB�mB	B��B��B�B��B��B��B��B��B	B	+B	
=B	DB	VB	uB	�B	�B	�B	 �B	E�B	T�B	`BB	l�B	{�B	� B	� B	�B	� B	� B	�B	�+B	�1B	�7B	�=B	�=B	�JB	�JB	�PB	�VB	�PB	�PB	�VB	�PB	�PB	�PB	�\B	�hB	�{B	��B	��B	��B	�uB	�DB	�7B	�=B	�7B	�VB	�\B	�7B	� B	|�B	~�B	� B	~�B	~�B	~�B	� B	�B	�B	�+B	�+B	�1B	�1B	�1B	�1B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�'B	�-B	�FB	�RB	�XB	�RB	�LB	�XB	�dB	�jB	�qB	�wB	�wB	�jB	�jB	�jB	�wB	�}B	�wB	�wB	�wB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
\B
�B
!�B
(�B
1'B
6FB
=qB
D�B
I�B
O�B
T�B
\)B
`BB
ffB
l�B
o�B
t�B
w�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�kB	�nB	�cB	�cB	�kB	�cB	�pB	�tB	�oB	�yB	�B	��B	��B
�B
0B
6CB
>tB
K�B
T�B
�/BM�B`<BbDB]$B_2BioBo�B��B��B�-B�eB�gB��B�rB�B3)BH�BfZB�B�LB��B�B�=B�B�BB	-B7B
3B:BFBDBEBIBGB^BkBjB[BLB
4B	-BEB
4B&BB	BB��B��B��B�B�B�TB�B��BŔBēB�GB��B�BB��B|�Bz�Bt�BgbB?nB#�BEB��B�aB��B�kB�B�7BU�B*�B&�ByB
�B
�IB
ƞB
��B
��B
�B
hlB
S�B
<`B
'�B
�B
>B	�B	ȰB	�#B	��B	�B	bNB	M�B	;fB	!�B	�B	oB	B�B�B�bB�QB�&B�.B�B��BÙB�pB�UB�=B�$B�B�B�B��B��B��B��B��B�rB�OB�[B�gB�`B�[B�NB�]B�eB�cB�uB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�6B�B�B��B��B��B�B��B��B��B�Bz�Bv�Bt�By�Bx�Bx�Bv�Bt�Bm�BhzB^<B[)B\.Bl�Bj�BgoBeeBhyBs�Bx�Bu�Bx�Bt�B|�B}�By�Bt�Bp�Bj�Bd_BaNBaPBd]BcXB`IB\.BUBO�BO�BR�BQ�BQ�BQ�BO�BN�BO�BUBhyB{�Bq�BgpBl�Bm�Bu�B�)B�IB�JB�6B��B��B��B��BšB�4B��B�3B��B�CB��B��B��B��B		7B	yB	�B	�B	kB	RB	B�B��B��B��B�B�B�eB�jB�cB�WB�kB	B��B��B�B��B��B��B��B��B	B	)B	
8B	?B	QB	pB	|B	�B	�B	 �B	E�B	T�B	`<B	l�B	{�B	�B	�B	��B	�B	�B	�
B	� B	�)B	�.B	�6B	�5B	�BB	�CB	�IB	�LB	�HB	�GB	�MB	�GB	�HB	�IB	�RB	�_B	�vB	��B	��B	��B	�lB	�9B	�,B	�4B	�1B	�OB	�TB	�/B	�B	|�B	~�B	�B	~�B	~�B	~�B	�B	�B	�B	�"B	�#B	�(B	�(B	�(B	�(B	�@B	�OB	�eB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	�$B	�<B	�GB	�OB	�HB	�DB	�NB	�YB	�`B	�iB	�mB	�oB	�_B	�^B	�aB	�oB	�qB	�oB	�oB	�mB	�B	ɯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ʸB	ǥB	ȬB	ɯB	ʷB	˼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�BB	�OB	�MB	�UB	�UB	�[B	�^B	�\B	�TB	�\B	�ZB	�bB	�dB	�bB	�`B	�iB	�pB	�tB	�tB	�{B	�{B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
RB
�B
!�B
(�B
1B
69B
=fB
D�B
I�B
O�B
T�B
\B
`4B
fXB
l{B
o�B
t�B
w�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436402016080714364020160807143640  AO  ARCAADJP                                                                    20151103101637    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151103101637  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151103101637  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143640  IP                  G�O�G�O�G�O�                