CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-29T02:16:03Z AOML 3.0 creation; 2016-08-07T21:36:40Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151029021603  20160807143640  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               PA   AO  5286_8897_080                   2C  D   APEX                            6531                            072314                          846 @�z}'q�K1   @�z}��N@@2St�j~��c(�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    PA   B   B   @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B7��B@  BH  BP��BX  B_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�3D�I�D�vfD��3D��fD�FfD��fD���D�  D�6fD�ffD���D�	�D�I�D�s3D���D�3D�<�D�l�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���A z�A z�A>�GA`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B7�RB@�BH�BP�BX�B_�RBg�RBp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL!HCN!HCP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&RD&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM{�DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dp��Dq��Dr�Dr��Ds�Ds��Dt�DtuDy��D�)D�J�D�w\D��)D��\D�G\D��\D���D� �D�7\D�g\D���D�
�D�J�D�t)D���D�)D�=�D�m�D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�r�A�l�A�^5A�\)A�7LA�7LA�(�A�&�A�$�A�$�A�$�A�"�A�"�A��A��A��A��A��A�{A���A܍PA�l�AكA֩�A���Aϧ�A��A˰!Aɥ�A�K�A�n�A�\)A�z�A���A�p�AÕ�A¼jA�ĜA�;dA�S�A��A�?}A���A�A�A�=qA�C�A���A�1'A��uA�oA��A�/A��^A���A� �A�O�A�33A��yA��PA�
=A���A�K�A���A�p�A� �A�ĜA�VA���A��;A���A�G�A�`BA��
A���A��hA��jA�/A�33A��jA�E�A���A�VA�33A��#A�E�A��A���A��DA�A��A�C�A�1A���A�hsA�bNA��^A� �A�1'A�|�A�-A
=A~VAz�jAv��At(�Ar��An1AiC�Af�yAb�9A]�#AY
=AW�AV=qAT~�AS�AO�AM�hAL�DALE�AK��AI��AG;dAEx�AE�AB�AB9XAAXA?�hA<��A;�A:Q�A:JA9��A8��A8�`A8r�A7��A7hsA65?A5K�A5C�A6-A4jA4�+A/;dA+t�A-��A.�A-ƨA+l�A(1A&�9A#ƨA �HA �RA JAG�A;dA\)A  �A"�A�;A?}A~�A9XA��A��A�7AbAȴAĜAȴAZA  A��A��AC�AM�A�FA&�A�A�mA��A�/A�uAJA&�A�AXA7LA
ZA	33A	VA	AE�AK�A9XA�#A�mAbNA"�A�DAn�A�AZA�A�A�A��A��A��@���@��@���@���@��@�7L@�1'@�33@��`@�@��D@�\)@��T@�9X@◍@�o@��@�ƨ@�@�z�@��@�@թ�@���@�~�@���@���@�ff@щ7@�hs@�G�@�Ĝ@ϥ�@�@ΰ!@Χ�@Ο�@·+@��#@�hs@�?}@���@ʧ�@�?}@ǅ@� �@ȃ@���@�o@ƸR@ŉ7@�@��@°!@§�@§�@��7@��j@���@�/@�7L@���@��@��u@�J@°!@�j@�Ĝ@�t�@ģ�@�J@���@ź^@�/@�bN@¸R@�@�G�@��@��@���@���@��@�33@�33@���@��T@�G�@�Q�@���@�t�@�K�@��R@�{@��7@�G�@�Z@�l�@��7@���@�1@�K�@�E�@�7L@�%@�Ĝ@�A�@�S�@�33@���@��P@��@�l�@�o@�{@�%@�Z@�t�@�C�@��H@��y@��@�M�@���@�%@��H@�{@���@��#@�{@��H@�+@�C�@�\)@�|�@���@�\)@�
=@��R@��\@��\@���@���@�n�@�E�@�5?@�{@�@���@�(�@��u@�Q�@��@���@�\)@�C�@���@�^5@�{@���@��@��h@���@���@�V@�r�@��u@�%@��@���@���@�|�@���@�E�@��^@�p�@�%@��D@�9X@�(�@�Q�@��P@��R@��@�^5@��H@��R@���@�-@���@�@��;@�ff@�{@���@��@���@��
@���@��P@�33@�"�@�+@��@�ȴ@�n�@�^5@�^5@�=q@��F@�%@���@��`@���@��9@��@��D@�Z@�ƨ@���@��@�S�@�+@�"�@�S�@�|�@��@���@�dZ@�o@���@��R@��R@��\@�M�@��@��^@��@�V@��`@�Ĝ@��@� �@�l�@�C�@��@��H@�ȴ@��R@��y@�+@�@��H@��@���@���@�~�@�E�@�@���@�O�@�A�@�l�@���@��\@��\@���@��!@�ff@�5?@�{@���@��@�`B@�?}@�^5@y�7@p��@h�u@`Q�@XQ�@N��@E�-@=/@7�@2�\@-/@'l�@$�@�@o@l�@�@1'@��@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�x�A�r�A�l�A�^5A�\)A�7LA�7LA�(�A�&�A�$�A�$�A�$�A�"�A�"�A��A��A��A��A��A�{A���A܍PA�l�AكA֩�A���Aϧ�A��A˰!Aɥ�A�K�A�n�A�\)A�z�A���A�p�AÕ�A¼jA�ĜA�;dA�S�A��A�?}A���A�A�A�=qA�C�A���A�1'A��uA�oA��A�/A��^A���A� �A�O�A�33A��yA��PA�
=A���A�K�A���A�p�A� �A�ĜA�VA���A��;A���A�G�A�`BA��
A���A��hA��jA�/A�33A��jA�E�A���A�VA�33A��#A�E�A��A���A��DA�A��A�C�A�1A���A�hsA�bNA��^A� �A�1'A�|�A�-A
=A~VAz�jAv��At(�Ar��An1AiC�Af�yAb�9A]�#AY
=AW�AV=qAT~�AS�AO�AM�hAL�DALE�AK��AI��AG;dAEx�AE�AB�AB9XAAXA?�hA<��A;�A:Q�A:JA9��A8��A8�`A8r�A7��A7hsA65?A5K�A5C�A6-A4jA4�+A/;dA+t�A-��A.�A-ƨA+l�A(1A&�9A#ƨA �HA �RA JAG�A;dA\)A  �A"�A�;A?}A~�A9XA��A��A�7AbAȴAĜAȴAZA  A��A��AC�AM�A�FA&�A�A�mA��A�/A�uAJA&�A�AXA7LA
ZA	33A	VA	AE�AK�A9XA�#A�mAbNA"�A�DAn�A�AZA�A�A�A��A��A��@���@��@���@���@��@�7L@�1'@�33@��`@�@��D@�\)@��T@�9X@◍@�o@��@�ƨ@�@�z�@��@�@թ�@���@�~�@���@���@�ff@щ7@�hs@�G�@�Ĝ@ϥ�@�@ΰ!@Χ�@Ο�@·+@��#@�hs@�?}@���@ʧ�@�?}@ǅ@� �@ȃ@���@�o@ƸR@ŉ7@�@��@°!@§�@§�@��7@��j@���@�/@�7L@���@��@��u@�J@°!@�j@�Ĝ@�t�@ģ�@�J@���@ź^@�/@�bN@¸R@�@�G�@��@��@���@���@��@�33@�33@���@��T@�G�@�Q�@���@�t�@�K�@��R@�{@��7@�G�@�Z@�l�@��7@���@�1@�K�@�E�@�7L@�%@�Ĝ@�A�@�S�@�33@���@��P@��@�l�@�o@�{@�%@�Z@�t�@�C�@��H@��y@��@�M�@���@�%@��H@�{@���@��#@�{@��H@�+@�C�@�\)@�|�@���@�\)@�
=@��R@��\@��\@���@���@�n�@�E�@�5?@�{@�@���@�(�@��u@�Q�@��@���@�\)@�C�@���@�^5@�{@���@��@��h@���@���@�V@�r�@��u@�%@��@���@���@�|�@���@�E�@��^@�p�@�%@��D@�9X@�(�@�Q�@��P@��R@��@�^5@��H@��R@���@�-@���@�@��;@�ff@�{@���@��@���@��
@���@��P@�33@�"�@�+@��@�ȴ@�n�@�^5@�^5@�=q@��F@�%@���@��`@���@��9@��@��D@�Z@�ƨ@���@��@�S�@�+@�"�@�S�@�|�@��@���@�dZ@�o@���@��R@��R@��\@�M�@��@��^@��@�V@��`@�Ĝ@��@� �@�l�@�C�@��@��H@�ȴ@��R@��y@�+@�@��H@��@���@���@�~�@�E�@�@���@�O�@�A�@�l�@���@��\@��\@���@��!@�ff@�5?@�{@���@��@�`BG�O�@�^5@y�7@p��@h�u@`Q�@XQ�@N��@E�-@=/@7�@2�\@-/@'l�@$�@�@o@l�@�@1'@��@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�!B
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�FB
�wB
��B1BM�BQ�BS�Bk�B��B�B� B��B��B�BhB�B33BZB�B��B��B�B�9B�XB�RB�LB�dB��B��B��B��B�B�/B�NB�fB�B�B�B�B�B�B�B�B�B��B��B�B�B�fB��BƨBB�qB�B��Bu�Be`B\)BF�B)�BVBB��B�BȴB�9B��B�JBw�BdZB[#BS�B?}B�B
�B
��B
��B
��B
�JB
z�B
n�B
0!B
�B
7LB
 �B	��B	�yB	�B	�-B	�JB	p�B	H�B	#�B	B��B�B�sB�BB��B��BȴBǮBŢB�wB�RB�^B�RB�dB�^B�XB�XB�^B�?B�B�B�B�B��B��B��B�RB�dB�LB��B�TB��B�TB��BŢB��B	uB	.B	"�B	B�B��B�^B�RB�?B�3B�9B�LBÖBÖB�-B�dB�LB�LB�?B�'B�B��B�B�B�B�B�'B�!B�'B�FB�LB�dB�jB�dB��BǮB��B��B��B��B��B�B�/B�#B�NB�`B�mB�sB�fB�NB�`B�B��B	
=B	)�B	.B	$�B	$�B	!�B	�B	$�B	#�B	�B	uB	B�sB�B��B	B	B	1B	1B	B��B��B��B�B�BŢB�^B�RB�LB�dB�FB�?B�RB�RB�XB�wBȴBĜB��B��BBBÖBƨBɺB��B��B��B��B��B��B��B��B�B�)B�)B�ZB�B�B�B�B�B�`B�B��B��B��B��B��B	B	%B	+B	PB	uB	�B	+B	1'B	@�B	F�B	D�B	P�B	^5B	]/B	`BB	_;B	]/B	[#B	[#B	[#B	]/B	]/B	^5B	\)B	`BB	cTB	e`B	dZB	dZB	cTB	aHB	aHB	cTB	dZB	dZB	dZB	dZB	dZB	dZB	aHB	`BB	_;B	_;B	^5B	]/B	]/B	`BB	e`B	e`B	dZB	gmB	m�B	o�B	r�B	u�B	u�B	r�B	q�B	r�B	u�B	v�B	y�B	|�B	}�B	�B	�B	}�B	y�B	x�B	{�B	�B	�B	�1B	�=B	�DB	�DB	�JB	�PB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�9B	�9B	�9B	�9B	�9B	�9B	�FB	�RB	�jB	�qB	�wB	��B	��B	��B	ÖB	B	��B	��B	ÖB	ŢB	ĜB	ŢB	ȴB	ɺB	��B	��B	ɺB	ȴB	��B	��B	��B	��B	��B	��B	ƨB	�wB	�XB	�LB	�FB	�?B	�?B	�RB	�RB	�RB	�RB	�XB	�^B	�jB	��B	B	ŢB	ǮB	ɺB	��B	�5B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�HB	�HB	�NB	�TB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
	7B
hB
�B
�B
'�B
2-B
9XB
A�B
H�B
M�B
O�B
VB
\)B
aHB
e`B
hsB
n�B
q�B
s�B
w�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�$B
�B
�B
�B
�B
�B
�B
�%B
�#B
�'B
�)B
�,B
�,B
�,B
�2B
�5B
�3B
�;B
�IB
�wB
��B1BM�BQ�BS�Bk�B��B�B�B��B��B�BgB�B31BZB�!B��B��B�B�:B�WB�OB�KB�_B��B��B��B��B�B�.B�NB�dB�B�B�B�B�B�B�B�B�B��B��B�B�B�gB��BƦBB�qB�B�Bu�Be[B\)BF�B)�BTBB��B�yBȰB�5B��B�GBw�BdVB[!BS�B?|BB
�B
��B
��B
��B
�LB
z�B
n�B
0#B
�B
7KB
 �B	��B	�}B	�B	�5B	�QB	p�B	H�B	#�B	'B��B�B�B�RB�B��B��BǽBŲB��B�cB�oB�cB�uB�pB�hB�hB�oB�RB�+B�'B�'B�B�B�B�B�aB�tB�]B��B�dB�B�aB�BűB��B	�B	.!B	"�B	B�B��B�nB�dB�OB�EB�IB�]BèBçB�;B�sB�]B�]B�OB�6B�%B�B�B�B�(B�+B�6B�1B�8B�WB�\B�sB�xB�sB��BǼB��B��B��B��B��B�B�=B�0B�^B�nB�|B�B�sB�\B�mB�B��B	
GB	*B	.B	$�B	$�B	!�B	�B	$�B	#�B	�B	B	B�B�B��B	*B	B	<B	=B	B��B��B��B�B�BŰB�mB�aB�YB�oB�UB�NB�`B�`B�gB��B��BĪB��B��BBBäBƶB��B��B��B��B��B��B��B��B�B�B�7B�6B�hB�B�B�B�B�B�mB�B��B��B��B��B�B	B	1B	6B	XB	�B	�B	+
B	10B	@�B	F�B	D�B	P�B	^;B	]6B	`HB	_DB	]9B	[+B	[+B	[)B	]8B	]7B	^?B	\1B	`KB	c^B	ehB	dcB	dcB	c^B	aOB	aPB	c]B	daB	dcB	dbB	daB	daB	dcB	aQB	`HB	_BB	_EB	^;B	]8B	]7B	`IB	efB	eiB	dcB	gsB	m�B	o�B	r�B	u�B	u�B	r�B	q�B	r�B	u�B	v�B	y�B	|�B	}�B	�
B	�B	}�B	y�B	x�B	{�B	�B	�B	�7B	�DB	�KB	�KB	�PB	�VB	�[B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�
B	�(B	�:B	�;B	�=B	�=B	�>B	�<B	�?B	�>B	�JB	�VB	�nB	�uB	�{B	��B	��B	��B	ØB	B	��B	��B	ÚB	ŧB	ġB	ŦB	ȷB	ɿB	��B	��B	ɿB	ȸB	��B	��B	��B	��B	��B	��B	ƪB	�~B	�]B	�QB	�JB	�EB	�CB	�WB	�TB	�VB	�XB	�\B	�dB	�nB	��B	B	ŧB	ǳB	ɾB	��B	�8B	�2B	�>B	�>B	�>B	�?B	�=B	�>B	�=B	�JB	�KB	�RB	�XB	�]B	�qB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
	B
B
B
B
	B	��B	��B	��B	��B	��B	��B
 B
B
B
	B
	B

B
G�O�B
	;B
kB
�B
�B
'�B
2-B
9XB
A�B
H�B
M�B
O�B
VB
\*B
aHB
e_B
hrB
n�B
q�B
s�B
w�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436402016080714364020160807143640  AO  ARCAADJP                                                                    20151029021603    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151029021603  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151029021603  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143640  IP                  G�O�G�O�G�O�                