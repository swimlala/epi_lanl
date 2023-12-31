CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:04Z AOML 3.0 creation; 2016-08-07T21:17:32Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221304  20160807141732  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_025                   2C  D   APEX                            6487                            072314                          846 @�0m�4@1   @�0n5�@-�x����d z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B���B�  B���B���B�  B�  B�  B�33B�  B���B�  B���B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�fD�VfD�� D��3D��D�FfD�y�D��3D�	�D�P D�p Dǩ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @_\)@��H@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJ\)BR\)BZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�ǮB�.B�.B���B�.B�ǮB���B�.B�.B�.B�aGB�.B���B�.B���B�.B�aGB�.B���B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
C�
C�
C�
C�
C
�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C �
C"�
C$�
C&�
C(�
C*�
C,�
C.�
C0�
C2�
C4�
C6�
C8�
C:�
C<�
C>�
C@�
CB�
CD�
CF�
CH�
CJ�
CL�
CN�
CP�
CR�
CT�
CV�
CX�
CZ�
C\�
C^�
C`�
Cb�
Cd�
Cf�
Ch�
Cj�
Cl�
Cn�
Cp�
Cr�
Ct�
Cv�
Cx�
Cz�
C|�
C~�
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt�]Dy�)D�)GD�iGD���D��D��D�YGD��{D��D�{D�b�D���DǼ{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�dZA�hsA�hsA�jA�l�A�n�A�n�A�n�A�n�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�r�A�t�A�t�A�v�A�v�A�x�A�x�A�x�A�z�A�~�A�~�AҁA҇+AґhAҝ�A�E�A� �A�;dA�O�A�K�A��A�%A�E�A��A�z�A�%A��!A��A���A�K�A���A�1A��A�"�A�r�A��\A��-A��A��A�;dA���A��A�ȴA�  A�&�A��TA�C�A���A���A�ȴA�+A��9A��#A� �A��/A��PA}�
Aw��Ar��Aq�#An�Ajv�AfZAcp�Aa�A[�
AXz�AWO�AVn�AT��AQ33AO`BAK�#AH$�AG�AD��AD$�A@��A<�/A;�A8^5A5�PA4��A4{A3��A2ffA0I�A.�jA-��A-\)A+&�A*z�A(��A'G�A%+A$bNA#C�A"  A �yA �uA �DA ��A#��A$$�A"��A!��A!`BA A�A�RA^5A��A�+A�A�A�yA��A�A��A�AbA33A$�A�A�hA�!A�A33A+A
�A
A(�A%A�/A(�A&�A�DAƨA?}A�uA�PA �!A ��A ��A M�A I�A ��A �DA E�AVA�PA �@�t�@��+@��H@�-@��@���@��`@��@�@���@�K�@�;d@�J@��@�X@�I�@�J@��9@�%@�^5@�p�@���@�@�5?@�-@�X@�j@��@���@��@�x�@�7@���@�h@�%@�@�G�@�j@�@���@���@��H@�ȴ@�^5@�/@�z�@��@띲@��@�=q@���@陚@��@��@�o@柾@�=q@�{@�-@�h@�j@�ƨ@�F@㝲@�J@�^@��@�=q@�7@�Ĝ@�9@�j@��m@߶F@�S�@�ȴ@���@�`B@ܓu@� �@۾w@�l�@���@��@��@�Z@��;@׍P@�33@֧�@�v�@�{@�G�@��`@Ԭ@�j@�b@�|�@���@җ�@�{@�@�7L@�bN@��m@�\)@���@Η�@�v�@�5?@ͩ�@���@�j@�A�@�9X@��@��m@˶F@�~�@�X@ț�@�Q�@�ƨ@�C�@�;d@�33@��y@Ƨ�@�~�@�@Ł@���@ļj@ēu@�Z@�9X@�b@ÍP@§�@�ff@�=q@�@�/@�1'@�b@���@�C�@�o@�dZ@�33@��H@�n�@���@�p�@��/@��@��D@�z�@�bN@�Z@�Z@� �@��@��P@�S�@��R@�~�@�V@�@��7@�G�@�/@���@�1@��
@��F@�dZ@�+@��@�V@���@�x�@�7L@��`@��@� �@���@��P@�K�@��y@��+@�5?@�@��7@�X@���@�j@�(�@��
@�;d@�=q@�hs@���@�Q�@��@�"�@���@�n�@�V@�$�@���@�X@�V@��@��/@�Ĝ@��D@�A�@��@���@�\)@�K�@�;d@�@��\@�^5@�hs@�z�@���@�l�@�"�@��!@�V@���@��^@��@�%@���@��@�Q�@�I�@�A�@�b@��@��@�dZ@�
=@��!@�^5@��T@�?}@��9@�9X@�b@��w@�dZ@�K�@��@��y@��R@�V@��@���@���@���@�X@�&�@�/@��`@�r�@��w@�;d@�
=@��@�ȴ@�v�@�{@���@�x�@�`B@�G�@��@��/@��u@��@�S�@�o@�+@��y@��!@��\@�n�@�V@�=q@�{@�hs@��@���@��@�A�@��@�  @��w@�|�@�t�@�\)@�+@���@��R@�v�@�=q@���@�x�@�O�@�/@�V@�Ĝ@�Q�@�1@���@���@~ȴ@t9X@l1@cdZ@[ƨ@T�@M/@Ep�@=�@7l�@1&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A�bNA�dZA�hsA�hsA�jA�l�A�n�A�n�A�n�A�n�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�r�A�t�A�t�A�v�A�v�A�x�A�x�A�x�A�z�A�~�A�~�AҁA҇+AґhAҝ�A�E�A� �A�;dA�O�A�K�A��A�%A�E�A��A�z�A�%A��!A��A���A�K�A���A�1A��A�"�A�r�A��\A��-A��A��A�;dA���A��A�ȴA�  A�&�A��TA�C�A���A���A�ȴA�+A��9A��#A� �A��/A��PA}�
Aw��Ar��Aq�#An�Ajv�AfZAcp�Aa�A[�
AXz�AWO�AVn�AT��AQ33AO`BAK�#AH$�AG�AD��AD$�A@��A<�/A;�A8^5A5�PA4��A4{A3��A2ffA0I�A.�jA-��A-\)A+&�A*z�A(��A'G�A%+A$bNA#C�A"  A �yA �uA �DA ��A#��A$$�A"��A!��A!`BA A�A�RA^5A��A�+A�A�A�yA��A�A��A�AbA33A$�A�A�hA�!A�A33A+A
�A
A(�A%A�/A(�A&�A�DAƨA?}A�uA�PA �!A ��A ��A M�A I�A ��A �DA E�AVA�PA �@�t�@��+@��H@�-@��@���@��`@��@�@���@�K�@�;d@�J@��@�X@�I�@�J@��9@�%@�^5@�p�@���@�@�5?@�-@�X@�j@��@���@��@�x�@�7@���@�h@�%@�@�G�@�j@�@���@���@��H@�ȴ@�^5@�/@�z�@��@띲@��@�=q@���@陚@��@��@�o@柾@�=q@�{@�-@�h@�j@�ƨ@�F@㝲@�J@�^@��@�=q@�7@�Ĝ@�9@�j@��m@߶F@�S�@�ȴ@���@�`B@ܓu@� �@۾w@�l�@���@��@��@�Z@��;@׍P@�33@֧�@�v�@�{@�G�@��`@Ԭ@�j@�b@�|�@���@җ�@�{@�@�7L@�bN@��m@�\)@���@Η�@�v�@�5?@ͩ�@���@�j@�A�@�9X@��@��m@˶F@�~�@�X@ț�@�Q�@�ƨ@�C�@�;d@�33@��y@Ƨ�@�~�@�@Ł@���@ļj@ēu@�Z@�9X@�b@ÍP@§�@�ff@�=q@�@�/@�1'@�b@���@�C�@�o@�dZ@�33@��H@�n�@���@�p�@��/@��@��D@�z�@�bN@�Z@�Z@� �@��@��P@�S�@��R@�~�@�V@�@��7@�G�@�/@���@�1@��
@��F@�dZ@�+@��@�V@���@�x�@�7L@��`@��@� �@���@��P@�K�@��y@��+@�5?@�@��7@�X@���@�j@�(�@��
@�;d@�=q@�hs@���@�Q�@��@�"�@���@�n�@�V@�$�@���@�X@�V@��@��/@�Ĝ@��D@�A�@��@���@�\)@�K�@�;d@�@��\@�^5@�hs@�z�@���@�l�@�"�@��!@�V@���@��^@��@�%@���@��@�Q�@�I�@�A�@�b@��@��@�dZ@�
=@��!@�^5@��T@�?}@��9@�9X@�b@��w@�dZ@�K�@��@��y@��R@�V@��@���@���@���@�X@�&�@�/@��`@�r�@��w@�;d@�
=@��@�ȴ@�v�@�{@���@�x�@�`B@�G�@��@��/@��u@��@�S�@�o@�+@��y@��!@��\@�n�@�V@�=q@�{@�hs@��@���@��@�A�@��@�  @��w@�|�@�t�@�\)@�+@���@��R@�v�@�=q@���@�x�@�O�@�/@�V@�Ĝ@�Q�G�O�@���@���@~ȴ@t9X@l1@cdZ@[ƨ@T�@M/@Ep�@=�@7l�@1&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B5?B5?B7LB=qBL�Bw�B��B��B	DB	6FB	�B
�B
S�B
�B
��B�B�B�B�BPBbBbB	7B
��B
�mB
�B
ĜB
�LB
�B
��B
��B
��B
��B
��B
��B
�1B
y�B
s�B
k�B
bNB
ZB
O�B
G�B
8RB
�B
B	�B	�B	�^B	��B	��B	�+B	k�B	VB	A�B	2-B	�B	PB	+B	B��B�B�fB�5B�B��B��B��BǮBƨBBÖBǮBǮBƨBŢBÖB�^B�9B�!B�B�B�B�B�B��B��B��B��B�B�'B�FB��B��B	�B	\B	oB	�B	VB	B	\B	�B	+B	6FB	A�B	F�B	F�B	9XB	)�B	�B	JB		7B	oB	�B	{B	oB	hB	�B	#�B	!�B	�B	�B	hB	�B	oB	\B	\B	VB	JB	JB	\B	oB	�B	!�B	#�B	&�B	+B	,B	,B	@�B	XB	S�B	Q�B	R�B	[#B	^5B	]/B	bNB	iyB	l�B	y�B	�B	�=B	�DB	�7B	�%B	�B	�B	}�B	z�B	�B	�hB	�VB	�VB	�JB	�7B	�DB	�\B	�PB	�=B	�PB	��B	�B	�'B	�9B	�FB	�FB	�dB	�jB	�jB	��B	ƨB	ɺB	��B	��B	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�B	�B	�B	�B	�;B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�sB	�mB	�mB	�fB	�`B	�ZB	�NB	�NB	�TB	�TB	�TB	�ZB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�sB	�sB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
JB
PB
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
hB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
&�B
)�B
/B
7LB
<jB
C�B
I�B
M�B
Q�B
VB
[#B
_;B
cTB
gm1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3B3	B3B3B3B3B3B3B3B3B5B5B7B=CBL�Bw�B��B��B	(B	6'B	��B
�B
S�B
��B
��BoB{B{BVB!B/B-B	B
�B
�8B
��B
�hB
�B
��B
��B
��B
��B
��B
��B
�eB
��B
y�B
s�B
kOB
bB
Y�B
O�B
G|B
8B
yB
�B	�`B	��B	�,B	��B	�lB	��B	kTB	U�B	AZB	1�B	~B	!B	�B	�B��B�]B�7B�	B��B��BϲB̠B�B�xB�bB�fB�{B�{B�xB�rB�gB�.B�	B��B��B��B��B��B��B��B��B��B��B��B��B�B�WB��B	]B	&B	:B	XB	!B	�B	&B	VB	*�B	6B	ATB	FqB	FpB	9B	)�B	oB	B		 B	6B	RB	CB	7B	1B	XB	#�B	!�B	B	JB	0B	GB	3B	$B	#B	B	B	B	"B	4B	YB	!�B	#�B	&�B	*�B	+�B	+�B	@FB	W�B	S�B	Q�B	R�B	Z�B	]�B	\�B	bB	i=B	lNB	y�B	��B	�B	�B	��B	��B	��B	��B	}�B	z�B	��B	�(B	�B	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�$B	�+B	�(B	�AB	�gB	�yB	ΗB	ΗB	ϟB	УB	��B	��B	ԻB	ӶB	ѫB	ҰB	ұB	ҳB	үB	ұB	ұB	ԽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�=B	�7B	�6B	�8B	�:B	�:B	�CB	�<B	�7B	�7B	�7B	�8B	�2B	�)B	�(B	�"B	�B	�B	�	B	�
B	�B	�B	�B	�B	�!B	�"B	�+B	�+B	�+B	�/B	�1B	�6B	�5B	�5B	�/B	�-B	�(B	�+B	�*B	�)B	�(B	�0B	�1B	�*B	�.B	�0B	�/B	�4B	�;B	�:B	�:B	�CB	�AB	�;B	�:B	�9B	�;B	�9B	�;B	�<B	�AB	�@B	�FB	�KB	�NB	�SB	�TB	�UB	�VB	�NB	�LB	�NB	�QB	�TB	�VB	�YB	�YB	�eB	�gB	�jB	�fB	�iB	�B	�~B	�zB	�xB	�rB	�tB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
B

�B

�B

�B

�B
B
	B
B
B
B
B
B
B
B
B
!B
B
!B
%B
&B
 B
(B
)B
'B
&B
'B
.B
0B
/B
3B
2B
3B
8B
;B
;B
:B
:B
:B
;B
5B
4B
9B
9B
:B
9B
8B
:B
;B
:B
=B
8B
?B
?B
@B
AB
EB
MB
MB
NB
LB
SB
RB
SB
LB
RB
VB
cB
eB
eB
jB
oB
oB
qB
jB
rB
wB
wB
 B
 }B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�G�O�B
)�B
.�B
7B
<"B
CLB
IrB
M�B
Q�B
U�B
Z�B
^�B
cB
g#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.59 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417322016080714173220160807141732  AO  ARCAADJP                                                                    20150226221304    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221304  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221304  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141732  IP                  G�O�G�O�G�O�                