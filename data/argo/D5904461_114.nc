CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-25T19:18:14Z AOML 3.0 creation; 2016-08-07T21:36:46Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160425191814  20160807143646  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               rA   AO  5286_8897_114                   2C  D   APEX                            6531                            072314                          846 @קU�
u1   @קVO�\@4R���m�cN�G�{1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    rA   B   B   @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D�3D�9�D��3D��3D��D�I�D�l�D���D�	�D�C3D�� D��3D�	�D�9�D�s3D��fD� D�FfD�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@�
=A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�p�B�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf8RCh�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]�HD^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtt{Dy�HD�
D�=qD��
D��
D�qD�MqD�p�D�ФD�qD�G
D���D��
D�qD�=qD�w
D��=D��D�J=D�s�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʲ-Aʺ^AʾwA�A�ƨA���AʶFAʸRAʺ^Aʴ9AʮAʬAʲ-AʮAʬAʮAʩ�Aʣ�Aʧ�Aʧ�Aʥ�Aʩ�Aʰ!AʬAʥ�Aʡ�Aʕ�Aʏ\Aʇ+AʅA�v�A�n�Aɝ�A�1A�~�A��Aǰ!A��A��A�I�Aģ�A�VA�S�A�E�A���A��A��uA�ȴA�G�A���A�v�A���A��7A�ȴA��^A�5?A� �A�(�A���A�O�A� �A�^5A���A��FA�|�A�A�A�JA��yA���A��9A��A���A��
A��A��A�ffA��A���A���A���A��A�%A�9XA���A��7A�7LA��7A�A��jA���A�VA���A��hA�n�A�M�A��TA�r�A�A�A��7A�l�A�`BA��uA�ĜA�A��A�A�-A~��Ay�;Aw�Au�As;dAp~�Al�+Ag�mAb�A_��A\�AZ�\AX5?AW|�AU��ATI�ARr�AO�hAM��AK�#AJ�AIC�AI%AH��AG�mAE�ADr�AC�AC�ABv�AA��A?��A>1A=|�A<��A;l�A:$�A8��A5C�A4v�A41'A4 �A3�FA1�TA01A.�+A-��A-�A+VA*I�A'��A&�yA%�TA$��A#��A#C�A"�RA!K�AVA  A��A�jA�A��A�A��A �A��Az�A�A�TA�HA�An�Ap�A�HAjA�A?}A��A�FA
A�A	�;A	x�A�A�FAdZAM�A�HA�-A��AQ�A�^A �/A z�A ^5A Q�A 1@�t�@�n�@��/@�bN@�7L@�C�@�I�@���@��^@���@��@���@�A�@�@��y@�
=@�@�?}@�V@��@�P@��@��@�$�@�@�M�@��/@��m@�ƨ@�w@���@��@�X@�t�@��#@�"�@�V@�{@�@��@ّh@�S�@ղ-@��@���@�`B@�bN@��@���@́@���@˥�@�V@�hs@��`@��H@˅@ˍP@�Q�@ģ�@�ȴ@�V@��7@�Z@��@��+@���@��7@�7L@�%@��9@��@�Z@�1@��P@�;d@�j@���@��@���@�v�@���@��/@��j@�bN@�9X@��@�t�@�|�@�9X@�l�@��y@�@�v�@��@��h@��@�Ĝ@��@�r�@�x�@���@�@���@��9@��u@�Q�@���@��R@�9X@�r�@��9@��9@�A�@�9X@��@��j@��@�G�@��@�1'@�|�@��w@�Z@�t�@��@��H@�V@�@���@��^@��h@�p�@��`@�z�@��;@��@��@�ff@��^@��j@�bN@�\)@���@���@���@��+@�v�@�=q@�-@��@��@��^@�`B@�7L@��@��D@�A�@�1@��
@��F@���@�\)@�K�@��@���@�n�@�=q@��@���@���@�p�@�X@���@�I�@��@��w@���@�33@�"�@�~�@��@���@��@��`@��D@�bN@�1'@��F@���@��+@�{@���@��T@���@��-@��h@�x�@�G�@�&�@�%@���@��`@��j@���@�Q�@�  @�@�^5@��T@��^@���@�hs@�7L@��j@�Q�@��@���@��;@��;@��w@���@�S�@�+@�
=@��@���@���@���@���@���@�^5@�-@��@�{@���@�X@�?}@�/@�&�@�V@��@�1'@��@�ƨ@��@��P@�S�@��@���@�{@���@��T@�@��h@�hs@�O�@�?}@��@�Ĝ@�bN@� �@���@��
@��@��P@�t�@�dZ@�\)@�K�@�o@���@��y@��R@�5?@���@���@��@���@��-@��@���@�j@��`@�Q�@x�@m�@e�@[�
@R^5@L1@C��@=�@7K�@1�#@.�R@)��@%V@;d@�m@\)@��@p�@	�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aʲ-Aʺ^AʾwA�A�ƨA���AʶFAʸRAʺ^Aʴ9AʮAʬAʲ-AʮAʬAʮAʩ�Aʣ�Aʧ�Aʧ�Aʥ�Aʩ�Aʰ!AʬAʥ�Aʡ�Aʕ�Aʏ\Aʇ+AʅA�v�A�n�Aɝ�A�1A�~�A��Aǰ!A��A��A�I�Aģ�A�VA�S�A�E�A���A��A��uA�ȴA�G�A���A�v�A���A��7A�ȴA��^A�5?A� �A�(�A���A�O�A� �A�^5A���A��FA�|�A�A�A�JA��yA���A��9A��A���A��
A��A��A�ffA��A���A���A���A��A�%A�9XA���A��7A�7LA��7A�A��jA���A�VA���A��hA�n�A�M�A��TA�r�A�A�A��7A�l�A�`BA��uA�ĜA�A��A�A�-A~��Ay�;Aw�Au�As;dAp~�Al�+Ag�mAb�A_��A\�AZ�\AX5?AW|�AU��ATI�ARr�AO�hAM��AK�#AJ�AIC�AI%AH��AG�mAE�ADr�AC�AC�ABv�AA��A?��A>1A=|�A<��A;l�A:$�A8��A5C�A4v�A41'A4 �A3�FA1�TA01A.�+A-��A-�A+VA*I�A'��A&�yA%�TA$��A#��A#C�A"�RA!K�AVA  A��A�jA�A��A�A��A �A��Az�A�A�TA�HA�An�Ap�A�HAjA�A?}A��A�FA
A�A	�;A	x�A�A�FAdZAM�A�HA�-A��AQ�A�^A �/A z�A ^5A Q�A 1@�t�@�n�@��/@�bN@�7L@�C�@�I�@���@��^@���@��@���@�A�@�@��y@�
=@�@�?}@�V@��@�P@��@��@�$�@�@�M�@��/@��m@�ƨ@�w@���@��@�X@�t�@��#@�"�@�V@�{@�@��@ّh@�S�@ղ-@��@���@�`B@�bN@��@���@́@���@˥�@�V@�hs@��`@��H@˅@ˍP@�Q�@ģ�@�ȴ@�V@��7@�Z@��@��+@���@��7@�7L@�%@��9@��@�Z@�1@��P@�;d@�j@���@��@���@�v�@���@��/@��j@�bN@�9X@��@�t�@�|�@�9X@�l�@��y@�@�v�@��@��h@��@�Ĝ@��@�r�@�x�@���@�@���@��9@��u@�Q�@���@��R@�9X@�r�@��9@��9@�A�@�9X@��@��j@��@�G�@��@�1'@�|�@��w@�Z@�t�@��@��H@�V@�@���@��^@��h@�p�@��`@�z�@��;@��@��@�ff@��^@��j@�bN@�\)@���@���@���@��+@�v�@�=q@�-@��@��@��^@�`B@�7L@��@��D@�A�@�1@��
@��F@���@�\)@�K�@��@���@�n�@�=q@��@���@���@�p�@�X@���@�I�@��@��w@���@�33@�"�@�~�@��@���@��@��`@��D@�bN@�1'@��F@���@��+@�{@���@��T@���@��-@��h@�x�@�G�@�&�@�%@���@��`@��j@���@�Q�@�  @�@�^5@��T@��^@���@�hs@�7L@��j@�Q�@��@���@��;@��;@��w@���@�S�@�+@�
=@��@���@���@���@���@���@�^5@�-@��@�{@���@�X@�?}@�/@�&�@�V@��@�1'@��@�ƨ@��@��P@�S�@��@���@�{@���@��T@�@��h@�hs@�O�@�?}@��@�Ĝ@�bN@� �@���@��
@��@��P@�t�@�dZ@�\)@�K�@�o@���@��y@��R@�5?@���@���@��@���@��-@��@���G�O�@��`@�Q�@x�@m�@e�@[�
@R^5@L1@C��@=�@7K�@1�#@.�R@)��@%V@;d@�m@\)@��@p�@	�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�^B	�^B	�dB	�XB	�^B	�RB	�?B	�?B	�?B	�9B	�9B	�9B	�3B	�3B	�-B	�-B	�-B	�-B	�-B	�-B	�'B	�-B	�9B	�-B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�B
`BB
��B
�BB
��B
�NB
�;B
��BBJB�B+B6FB;dBS�Be`B� B��B��B��B��B��B�Bq�Bs�Bt�Bs�Br�Bm�BcTBXBJ�BI�BK�BN�BP�BR�BK�B(�B,B(�B+B1'B�BDB
��B
�B
�B
ǮB
�qB
�3B
�'B
�B
��B
��B
�1B
~�B
u�B
s�B
r�B
p�B
n�B
hsB
bNB
XB
H�B
;dB
0!B
"�B
+B	��B	�TB	��B	��B	ÖB	��B	�uB	�%B	t�B	aHB	E�B	0!B	�B	PB	B��B��B��B�B�B�TB�
B��BǮBŢBĜBÖBB��BɺBȴBǮBƨBƨB��B��B��B��B��B��B��BƨBŢBĜBĜBǮB��B��B��B��B��BǮB�wB�wB�XB�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�hB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�!B�B�B�B��B��B��B��B�B�}B�RB�!B��B��B��B�B�!B�!B�!B�wB�/B�B��B��B�B�B�B�HB��BŢBĜB��B��B��B�qB�qB�dB�RB�FB�RB�RB�LB�LB�FB�?B�^B�qB��BÖBŢBȴB��B��B��B��B�B�B�5B�ZB��B	  B	B��B�B�yB�sB�sB�B�B�B�B�B�B�B�B�B�B�B�B��B	B	B	JB	oB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	(�B	.B	33B	5?B	49B	2-B	0!B	1'B	;dB	>wB	C�B	N�B	VB	[#B	\)B	^5B	_;B	_;B	]/B	YB	T�B	XB	ZB	\)B	^5B	aHB	e`B	hsB	o�B	t�B	w�B	x�B	w�B	|�B	�B	�B	� B	� B	�B	�B	�B	�B	�B	�B	�=B	�=B	�DB	�DB	�=B	�DB	�DB	�VB	�VB	�PB	�JB	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�3B	�3B	�9B	�?B	�FB	�FB	�FB	�LB	�FB	�?B	�?B	�LB	�dB	�^B	�^B	�RB	�RB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�wB	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
\B
�B
!�B
+B
49B
@�B
A�B
K�B
L�B
R�B
T�B
YB
^5B
cTB
gmB
jB
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�^B	�_B	�fB	�XB	�^B	�VB	�>B	�@B	�@B	�<B	�:B	�9B	�5B	�5B	�-B	�1B	�-B	�.B	�1B	�/B	�*B	�.B	�:B	�.B	�.B	�.B	�6B	�6B	�;B	�;B	�BB	�GB	�B
`=B
��B
�BB
��B
�FB
�2B
��BBCB�B*�B6=B;ZBS�BeWB�B��B��B��B��B��B�Bq�Bs�Bt�Bs�Br�Bm�BcIBX	BJ�BI�BK�BN�BP�BR�BK�B(�B+�B(�B*�B1B�B;B
��B
�B
�B
ǧB
�lB
�)B
�!B
��B
��B
�zB
�)B
~�B
u�B
s�B
r�B
p�B
n�B
hkB
bHB
XB
H�B
;`B
0B
"�B
(B	��B	�RB	��B	��B	ÒB	��B	�tB	�%B	t�B	aGB	E�B	0%B	�B	SB	B��B��B��B�B�B�]B�B��BǳBūBĤBÞBB��BɿBȹBǷBƮBƮB��B��B��B��B��B��B��BƪBŦBĢBġBǲB��B��B��B��B��BǰB�~B�}B�_B�EB�5B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�pB�nB�vB�|B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�2B�$B� B�B�B��B��B��B��B�B��B�TB�&B��B��B�B�B�%B�%B�&B�zB�1B�B��B��B�B�B�B�JB��BŢBĠB��B��B��B�sB�uB�fB�VB�IB�TB�VB�RB�RB�IB�DB�aB�rB��BØBŤBȶB��B��B��B��B�B�!B�7B�]B��B��B	B��B�B�xB�vB�uB�B�~B�B�B�B�B�B�B�B�B�B�B��B	B	B	IB	mB	tB	|B	�B	�B	�B	�B	�B	�B	!�B	(�B	.B	32B	5?B	47B	2,B	0 B	1'B	;bB	>rB	C�B	N�B	VB	[B	\&B	^1B	_6B	_7B	]+B	YB	T�B	XB	ZB	\'B	^2B	aEB	e]B	hpB	o�B	t�B	w�B	x�B	w�B	|�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�6B	�8B	�>B	�?B	�8B	�?B	�@B	�PB	�OB	�JB	�DB	�EB	�_B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�(B	�0B	�,B	�-B	�3B	�6B	�>B	�=B	�=B	�CB	�=B	�7B	�6B	�FB	�`B	�XB	�WB	�KB	�LB	�JB	�OB	�QB	�RB	�WB	�XB	�`B	�oB	�{B	B	ÎB	ĕB	ĔB	ŜB	ơB	ƠB	ȮB	ɴB	ʹB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�&B	�'B	�.B	�,B	�2B	�3B	�0B	�8B	�:B	�9B	�:B	�:B	�@B	�FB	�FB	�BB	�QB	�VB	�]B	�^B	�^B	�]B	�eB	�fB	�kB	�iB	�pB	�pB	�vB	�zB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
'B
TB
�B
!�B
*�B
4.B
@vB
A}B
K�B
L�B
R�B
T�B
YB
^)B
cHB
gcB
jsB
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436462016080714364620160807143646  AO  ARCAADJP                                                                    20160425191814    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160425191814  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160425191814  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143646  IP                  G�O�G�O�G�O�                