CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-30T02:15:57Z AOML 3.0 creation; 2016-08-07T21:17:41Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150930021557  20160807141741  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               LA   AO  5285_8895_076                   2C  D   APEX                            6487                            072314                          846 @�s<�r�V1   @�s=��@+Ձ$�/�cЛ��S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    LA   B   B   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  Aљ�A�ffA�  B   B  B  B  B   B'��B0ffB7��B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy` D�fD�S3D��3D�ɚD�3D�VfD�� D�� D�3D�<�D�i�DǠ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�A
�HA*�HAJ�HAlz�A�p�A�p�A�p�A�p�A�p�A�
>A��
A�p�B�RB
�RB�RB�RB"�RB*Q�B3�B:Q�BB�RBJ�RBRQ�BZ�RBb�RBj�RBr�RBz�RB�\)B�\)B��\B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�c�C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
C�W
D +�D ��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D	+�D	��D
+�D
��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D+�D��D +�D ��D!+�D!��D"+�D"��D#+�D#��D$+�D$��D%+�D%��D&+�D&��D'+�D'��D(+�D(��D)+�D)��D*+�D*��D++�D+��D,+�D,��D-+�D-��D.+�D.��D/+�D/��D0+�D0��D1+�D1��D2+�D2��D3+�D3��D4+�D4��D5+�D5��D6+�D6��D7+�D7��D8+�D8��D9+�D9��D:+�D:��D;+�D;��D<+�D<��D=+�D=��D>+�D>��D?+�D?��D@+�D@��DA+�DA��DB+�DB��DC+�DC��DD+�DD��DE+�DE��DF+�DF��DG+�DG��DH+�DH��DI+�DI��DJ+�DJ��DK+�DK��DL+�DL��DM+�DM��DN+�DN��DO+�DO��DP+�DP��DQ+�DQ��DR+�DR��DS+�DS��DT+�DT��DU+�DU��DV+�DV��DW+�DW��DX+�DX��DY+�DY��DZ+�DZ��D[+�D[��D\+�D\��D]+�D]��D^+�D^��D_+�D_��D`+�D`��Da+�Da��Db+�Db��Dc+�Dc��Dd+�Dd��De+�De��Df+�Df��Dg+�Dg��Dh+�Dh��Di+�Di��Dj+�Dj��Dk+�Dk��Dl+�Dl��Dm+�Dm��Dn+�Dn��Do+�Do��Dp+�Dp��Dq+�Dq��Dr+�Dr��Ds+�Ds��Dt+�Dt��Dy��D�)D�h�D���D��]D��D�l)D���D���D��D�R�D�]Dǵ�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AꙚAꕁA�ZA�G�A�7LA�-A�"�A��A�JA��TA�ȴA�+A�hA�A�A���A�ƨA�"�A��A�bNA׸RA�`BAҧ�AЬAΓuA͛�A���Aʥ�A��A�  A��AƁA�AŴ9A�A�33A���A�S�A��A��mA�C�A�`BA���A���A�t�A�"�A��A�;dA���A�+A�G�A�JA��^A�%A���A���A�r�A�$�A��A�hsA��FA��9A�dZA���A��A��A�K�A�bA�K�A��A���A���A�1A�bA��A�  A�ZA��9A��A�C�Az��Aw�7Ao��Afz�Aa�A`$�A^AZ�AT=qAO��AL�9AJ��AHn�AE�
AD��AC�A@��A?��A=�hA;/A7��A5+A3�A1dZA/�A/�FA/�A/\)A.��A,��A+`BA+&�A)S�A(Q�A((�A'�-A&r�A%��A%C�A%A$�A#��A#�A"�uA!+A �A bAl�A~�Ax�A�A�;AjA�#A�yAE�A9XA1'A-A1'AoA%A��A%AbNA�A�TAO�A�A��A-Ap�A�HA�A�jAjA��A�PA�7AK�A&�A��A�A��AffA-A�^A��AZA�A��A��A�PA7LA
�/A
~�A
E�A	�A	��A	l�A	S�A	/A�HA�A�TA�AA�yA�uA(�A�-A�hAx�A\)A\)AG�AoA�\A�Az�Ar�AVAAx�A��A  AƨA/A ��A �A I�@��@��\@���@��/@�|�@�
=@��y@�V@�O�@�Ĝ@��@�E�@��T@��h@��@�@�I�@��@�F@�dZ@�"�@�@��@�r�@�+@�@�b@���@�-@��#@�`B@���@�@�(�@畁@��@旍@�@噚@�7L@��m@��@���@���@���@���@��@��#@�j@�(�@�b@�l�@�+@�x�@�hs@�`B@�r�@�|�@�G�@���@�b@�33@�o@��@���@�Q�@���@�+@�^5@ՙ�@�7L@���@Լj@ԃ@�1'@�"�@ҏ\@�J@��@�;d@�o@�o@θR@��T@�G�@�O�@��`@�A�@�l�@��@�S�@��y@�=q@�x�@�`B@�&�@�A�@�  @ǶF@�C�@Ƨ�@ũ�@ũ�@�X@��@�j@��
@�l�@�M�@�G�@�Ĝ@�1@���@�\)@��@��@���@�n�@��T@���@�p�@�7L@�%@��@��/@���@���@��@�@���@��@�hs@�7L@��@���@���@�j@�A�@��w@�t�@��y@�E�@�$�@���@�p�@�&�@���@���@��@���@���@�~�@�J@��h@�O�@�&�@��@��@�1'@�ƨ@�l�@�C�@�
=@���@�ȴ@�ff@�@�p�@�V@�r�@�Z@�(�@���@�|�@�;d@��H@���@�ff@��@�`B@�V@���@�j@�A�@��@��@��@���@�=q@��@��^@�`B@�G�@��`@�Z@�b@��w@�dZ@�
=@�^5@��T@�p�@��@��@�Ĝ@���@�z�@�Z@�9X@���@�K�@��@��@���@�^5@��@��@�x�@�G�@��@���@���@���@��j@���@�b@�\)@�33@���@��!@�n�@�{@��-@�`B@�&�@��9@�bN@�b@��@��@�@��H@�ȴ@��!@�v�@�M�@�-@�{@��#@�x�@�G�@�7L@��/@� �@�ƨ@��@�\)@�o@���@��H@��\@�5?@��@��T@���@���@�?}@��@��9@��D@�Z@�9X@�(�@�b@���@��y@�~�@�$�@�(�@��H@�p�@z�@k�
@b�H@XA�@N��@Gl�@C@<�@7�;@1%@)hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  AꙚAꕁA�ZA�G�A�7LA�-A�"�A��A�JA��TA�ȴA�+A�hA�A�A���A�ƨA�"�A��A�bNA׸RA�`BAҧ�AЬAΓuA͛�A���Aʥ�A��A�  A��AƁA�AŴ9A�A�33A���A�S�A��A��mA�C�A�`BA���A���A�t�A�"�A��A�;dA���A�+A�G�A�JA��^A�%A���A���A�r�A�$�A��A�hsA��FA��9A�dZA���A��A��A�K�A�bA�K�A��A���A���A�1A�bA��A�  A�ZA��9A��A�C�Az��Aw�7Ao��Afz�Aa�A`$�A^AZ�AT=qAO��AL�9AJ��AHn�AE�
AD��AC�A@��A?��A=�hA;/A7��A5+A3�A1dZA/�A/�FA/�A/\)A.��A,��A+`BA+&�A)S�A(Q�A((�A'�-A&r�A%��A%C�A%A$�A#��A#�A"�uA!+A �A bAl�A~�Ax�A�A�;AjA�#A�yAE�A9XA1'A-A1'AoA%A��A%AbNA�A�TAO�A�A��A-Ap�A�HA�A�jAjA��A�PA�7AK�A&�A��A�A��AffA-A�^A��AZA�A��A��A�PA7LA
�/A
~�A
E�A	�A	��A	l�A	S�A	/A�HA�A�TA�AA�yA�uA(�A�-A�hAx�A\)A\)AG�AoA�\A�Az�Ar�AVAAx�A��A  AƨA/A ��A �A I�@��@��\@���@��/@�|�@�
=@��y@�V@�O�@�Ĝ@��@�E�@��T@��h@��@�@�I�@��@�F@�dZ@�"�@�@��@�r�@�+@�@�b@���@�-@��#@�`B@���@�@�(�@畁@��@旍@�@噚@�7L@��m@��@���@���@���@���@��@��#@�j@�(�@�b@�l�@�+@�x�@�hs@�`B@�r�@�|�@�G�@���@�b@�33@�o@��@���@�Q�@���@�+@�^5@ՙ�@�7L@���@Լj@ԃ@�1'@�"�@ҏ\@�J@��@�;d@�o@�o@θR@��T@�G�@�O�@��`@�A�@�l�@��@�S�@��y@�=q@�x�@�`B@�&�@�A�@�  @ǶF@�C�@Ƨ�@ũ�@ũ�@�X@��@�j@��
@�l�@�M�@�G�@�Ĝ@�1@���@�\)@��@��@���@�n�@��T@���@�p�@�7L@�%@��@��/@���@���@��@�@���@��@�hs@�7L@��@���@���@�j@�A�@��w@�t�@��y@�E�@�$�@���@�p�@�&�@���@���@��@���@���@�~�@�J@��h@�O�@�&�@��@��@�1'@�ƨ@�l�@�C�@�
=@���@�ȴ@�ff@�@�p�@�V@�r�@�Z@�(�@���@�|�@�;d@��H@���@�ff@��@�`B@�V@���@�j@�A�@��@��@��@���@�=q@��@��^@�`B@�G�@��`@�Z@�b@��w@�dZ@�
=@�^5@��T@�p�@��@��@�Ĝ@���@�z�@�Z@�9X@���@�K�@��@��@���@�^5@��@��@�x�@�G�@��@���@���@���@��j@���@�b@�\)@�33@���@��!@�n�@�{@��-@�`B@�&�@��9@�bN@�b@��@��@�@��H@�ȴ@��!@�v�@�M�@�-@�{@��#@�x�@�G�@�7L@��/@� �@�ƨ@��@�\)@�o@���@��H@��\@�5?@��@��T@���@���@�?}@��@��9@��D@�Z@�9X@�(�@�b@���@��y@�~�G�O�@�(�@��H@�p�@z�@k�
@b�H@XA�@N��@Gl�@C@<�@7�;@1%@)hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B�#B�HB�B��B��B	33B	��B
_;B
z�B
v�B
�B
�VB
��B
��BJB.B^5B��B��B��B+B6FBI�B`BB�B�+B�%B�DB�oB��B�B��B��B��B��B��B�{B�PB�oB�hB�{B{�B�BP�B_;B@�B;dB<jBoB1BBB��B��B�/BPB�B��B�#BB��B�1Be`BO�BO�BW
B�B
�/B
�LB
�uB
}�B
iyB
49B	�TB	�FB	ffB	49B	�B	�B	JB��B�B�mB�;B�B��B��B��B��B��B��B��B��B�B�B�5B�B�B��B	  B	+B	
=B	JB	{B	 �B	)�B	)�B	(�B	+B	0!B	8RB	@�B	D�B	N�B	\)B	^5B	YB	[#B	_;B	`BB	bNB	cTB	bNB	gmB	w�B	�B	�{B	�uB	�{B	��B	��B	��B	��B	��B	�hB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�?B	�-B	�3B	�LB	�FB	�FB	�FB	�FB	�RB	�XB	�jB	�wB	��B	��B	B	ĜB	ĜB	ĜB	ǮB	ǮB	ɺB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�BB	�NB	�TB	�fB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�ZB	�NB	�NB	�`B	�fB	�mB	�fB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�`B	�HB	�/B	�B	�
B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�5B	�B	�B	�B	�#B	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
(�B
,B
1'B
5?B
>wB
E�B
L�B
R�B
ZB
\)B
_;B
bNB
ffB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B��B��B��B��B��B��B�B�)B�fB��B��B	3B	��B
_B
z�B
v�B
��B
�$B
��B
��BB-�B^ B�NB�QB��B*�B6BI�B`B��B��B��B�B�5B��B��B��B��B�aB�xB��B�>B�B�8B�,B�@B{�B��BP�B_B@DB;&B<.B1B�B �B�BұBˈB��BBGB��B��B�QB��B��Be$BO�BO�BV�BjB
��B
�B
�6B
}�B
i:B
3�B	�B	�
B	f+B	4 B	rB	JB	B��B�rB�4B�B��BҼBϩB͛BˍBʉBʈBˏB��B��B��B��B�PB�nB��B��B	�B	
 B	
B	=B	 �B	)�B	)�B	(�B	*�B	/�B	8B	@DB	DZB	N�B	[�B	]�B	X�B	Z�B	^�B	_�B	bB	cB	b	B	g)B	w�B	��B	�5B	�/B	�6B	�XB	�XB	�QB	�fB	�XB	�"B	�B	�B	�$B	�TB	�yB	�tB	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�"B	�-B	�:B	�;B	�GB	�SB	�SB	�VB	�dB	�eB	�oB	�qB	�iB	�wB	̄B	ΐB	ϓB	ЙB	ΐB	̄B	̀B	͉B	͉B	ҩB	ԴB	ԴB	��B	��B	�B	�B	�B	�:B	�AB	�_B	�qB	�oB	�uB	�wB	�wB	�pB	�^B	�_B	�iB	�kB	�]B	�NB	�?B	�5B	�B	�B	� B	�B	�B	�B	�"B	�B	�B	�B	�B	�B	�!B	�#B	�&B	�>B	�>B	�?B	�>B	�>B	�0B	�B	��B	��B	��B	ֽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�tB	�sB	�nB	�gB	��B	�{B	�vB	�tB	�tB	�zB	�zB	�yB	�wB	�sB	�tB	�{B	�wB	�yB	�zB	�uB	�aB	�[B	�`B	�bB	�nB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
 �B	��B
 �B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 B
�B
�B
B
B
B
B
	B
	B

B
B
B
B

B
B
B
B
B
B
%B
(B
/B
4B
5B
3B
4B
4B
3B
5B
5B
-B
3B
3B
;B
AB
BB
@B
IB
EB
KB
KB
KB
NB
UB
RB
TB
XB
XB
WB
WB
_B
eB
`B
^B
dB
bB
eB
aB
WB
`B
]B
dB
kB
 tB
lB
 rB
!xB
!wB
!xB
!vB
!vB
!vB
!uB
"�B
"~B
"}B
#�B
#�B
#�B
#�B
#�B
$�G�O�B
(�B
+�B
0�B
4�B
>"B
EOB
LzB
R�B
Y�B
[�B
^�B
a�B
fB
j)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.68 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417412016080714174120160807141741  AO  ARCAADJP                                                                    20150930021557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150930021557  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150930021557  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141741  IP                  G�O�G�O�G�O�                