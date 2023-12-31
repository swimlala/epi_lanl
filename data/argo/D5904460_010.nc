CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:46Z AOML 3.0 creation; 2016-08-07T21:17:30Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221246  20160807141730  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               
A   AO  5285_8895_010                   2C  D   APEX                            6487                            072314                          846 @��>���1   @��β@ @-�t�j~��c��\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    
A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��Bw��B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyL�D��D�C3D�vfD���D� D�33D��3D�ٚD�3D�I�D�� DǦf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @e@��H@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�A��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJ\)BR\)BZ\)Bb\)Bj\)Bs(�By��B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.C �
C�
C�
C��C��C
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
C:��C<�
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
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�>�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�>�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt�]Dyr�D�,{D�VD��GD�ϮD�"�D�FD��D��{D�&D�\{D���DǹG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�r�A�33A�
=A��A���Aۺ^A۬AۋDA�ffA�/AڸRA�r�A�hsA�`BA�dZA�dZA�~�A�l�A�G�A�;dA�/A�/A٧�A�ĜA���A��/A�M�A�ȴAґhA��mA�1A�I�A��`A�M�A�v�A�|�A�bNA��wA�G�A���A��-A���A�v�A�O�A��A���A��A���A��A��hA���A��`A�z�A��uA�\)A�v�A�ZA��A���A�-A���A�O�A�ȴA�/A�$�A���A��A�z�A�XA��A�bA� �A�t�A���A�"�A��uA�bNA�%A}A{��Ay\)Av��Av{Au`BAqS�An(�AljAiG�Ae�Ad�Ab5?A]�A[�-AX�yAX=qATQ�AP�yAMXAJ=qAGt�AF$�AC�mA?�TA?;dA=�A:~�A8VA7G�A6��A6 �A4�A4I�A4A2��A1�#A0��A0 �A/K�A.z�A.jA.�jA.n�A-�A+/A(�A'�#A%��A"ĜA jA�FA\)A�A��A�A?}AO�A`BA�7A��A JA�-A (�A jA ffA VAdZA�A��A��A�/Az�A�TA33Av�Al�A9XA�;A�A�jAr�A�/A�An�A{AJA��A��A^5AoA�AbNA�AG�A�jA�;A�DA��A�7AXA
�A��A�#A��At�AdZA�AhsA
�uA	�^A�PAK�A�FA	�PA	�A	�#A
�9A
��A
-A��A	dZA��A�Ax�A��A�#Ap�AXAA�AA�AbAJAS�A�AbNAK�A =qA b@�l�@�C�@�V@���@���@��D@��u@�j@�bN@�dZ@�n�@�`B@���@�%@���@��m@���@�j@�@�~�@�M�@�=q@��@��@�@@�;d@�"�@��@�!@�V@�x�@��`@�r�@�\)@�E�@�p�@�G�@�%@�bN@��m@�K�@柾@�^@���@�D@��@�P@�R@��#@�9@�b@���@��@ߍP@�@ݙ�@�p�@�?}@�Ĝ@���@��@�V@�@٩�@�p�@�9X@�C�@֧�@�E�@��#@ՙ�@�X@�%@ԃ@Ӯ@�"�@�n�@�5?@��T@љ�@�V@� �@ύP@�dZ@��H@�n�@�5?@�@���@Ͳ-@͡�@͉7@�p�@�X@��`@�z�@��
@�@���@�~�@��@���@�/@���@�r�@�9X@�b@���@�t�@��@ư!@Ƈ+@�5?@Ų-@�&�@��@�9X@î@�K�@��y@+@�M�@��7@���@��9@���@�Z@�1'@�ƨ@�33@��@��@�~�@�@��^@�x�@�X@���@�1'@��;@��
@��@�@��+@�$�@�@�7L@��`@�r�@�1'@���@�t�@��@��H@�M�@��#@���@�/@��@��@�bN@��@���@�;d@��@���@�n�@�J@�x�@��@��@��@�(�@�\)@���@��R@���@���@�-@��#@���@�?}@��@�%@��@�Ĝ@�j@�  @���@���@���@���@��\@�v�@��@��@�Z@�  @��m@���@��@�|�@�+@���@��@���@�n�@�-@��@���@�X@�%@���@��9@�bN@�1@���@�l�@�33@��H@��R@�M�@���@�O�@��@��@�Q�@�b@��F@�t�@�"�@��y@���@��\@�E�@��T@�O�@��@��j@��u@�Q�@� �@��@��
@�|�@���@���@��+@�5?@��@��@���@���@�p�@�&�@�%@�bN@��
@�\)@��H@���@�M�@�5?@�@��@��#@���@�&�@���@��;@�O�@��9@��w@|1@r^5@j�H@a��@Xr�@N��@Fv�@>�y@6��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A�bNA�r�A�33A�
=A��A���Aۺ^A۬AۋDA�ffA�/AڸRA�r�A�hsA�`BA�dZA�dZA�~�A�l�A�G�A�;dA�/A�/A٧�A�ĜA���A��/A�M�A�ȴAґhA��mA�1A�I�A��`A�M�A�v�A�|�A�bNA��wA�G�A���A��-A���A�v�A�O�A��A���A��A���A��A��hA���A��`A�z�A��uA�\)A�v�A�ZA��A���A�-A���A�O�A�ȴA�/A�$�A���A��A�z�A�XA��A�bA� �A�t�A���A�"�A��uA�bNA�%A}A{��Ay\)Av��Av{Au`BAqS�An(�AljAiG�Ae�Ad�Ab5?A]�A[�-AX�yAX=qATQ�AP�yAMXAJ=qAGt�AF$�AC�mA?�TA?;dA=�A:~�A8VA7G�A6��A6 �A4�A4I�A4A2��A1�#A0��A0 �A/K�A.z�A.jA.�jA.n�A-�A+/A(�A'�#A%��A"ĜA jA�FA\)A�A��A�A?}AO�A`BA�7A��A JA�-A (�A jA ffA VAdZA�A��A��A�/Az�A�TA33Av�Al�A9XA�;A�A�jAr�A�/A�An�A{AJA��A��A^5AoA�AbNA�AG�A�jA�;A�DA��A�7AXA
�A��A�#A��At�AdZA�AhsA
�uA	�^A�PAK�A�FA	�PA	�A	�#A
�9A
��A
-A��A	dZA��A�Ax�A��A�#Ap�AXAA�AA�AbAJAS�A�AbNAK�A =qA b@�l�@�C�@�V@���@���@��D@��u@�j@�bN@�dZ@�n�@�`B@���@�%@���@��m@���@�j@�@�~�@�M�@�=q@��@��@�@@�;d@�"�@��@�!@�V@�x�@��`@�r�@�\)@�E�@�p�@�G�@�%@�bN@��m@�K�@柾@�^@���@�D@��@�P@�R@��#@�9@�b@���@��@ߍP@�@ݙ�@�p�@�?}@�Ĝ@���@��@�V@�@٩�@�p�@�9X@�C�@֧�@�E�@��#@ՙ�@�X@�%@ԃ@Ӯ@�"�@�n�@�5?@��T@љ�@�V@� �@ύP@�dZ@��H@�n�@�5?@�@���@Ͳ-@͡�@͉7@�p�@�X@��`@�z�@��
@�@���@�~�@��@���@�/@���@�r�@�9X@�b@���@�t�@��@ư!@Ƈ+@�5?@Ų-@�&�@��@�9X@î@�K�@��y@+@�M�@��7@���@��9@���@�Z@�1'@�ƨ@�33@��@��@�~�@�@��^@�x�@�X@���@�1'@��;@��
@��@�@��+@�$�@�@�7L@��`@�r�@�1'@���@�t�@��@��H@�M�@��#@���@�/@��@��@�bN@��@���@�;d@��@���@�n�@�J@�x�@��@��@��@�(�@�\)@���@��R@���@���@�-@��#@���@�?}@��@�%@��@�Ĝ@�j@�  @���@���@���@���@��\@�v�@��@��@�Z@�  @��m@���@��@�|�@�+@���@��@���@�n�@�-@��@���@�X@�%@���@��9@�bN@�1@���@�l�@�33@��H@��R@�M�@���@�O�@��@��@�Q�@�b@��F@�t�@�"�@��y@���@��\@�E�@��T@�O�@��@��j@��u@�Q�@� �@��@��
@�|�@���@���@��+@�5?@��@��@���@���@�p�@�&�@�%@�bN@��
@�\)@��H@���@�M�@�5?@�@��@��#@���@�&�G�O�@��;@�O�@��9@��w@|1@r^5@j�H@a��@Xr�@N��@Fv�@>�y@6��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB �B �B-BD�BW
B_;BdZBjBm�Bq�Bt�B{�B�B�B�B�7B�hB�B�B�B�B�B�'B�B��B��B��B��B��B��B�BcTBt�B�uB��BɺB�fB�B�B�B%B�BhBhB �B!�B)�BE�BL�B�dB�!B��B�DBF�BuB��B�B�
B�LB��B��B�+Bu�Be`BS�BI�B@�B �B+B
�yB
�)B
��B
��B
r�B
I�B
!�B
  B	�B	�`B	��B	ĜB	�9B	��B	��B	��B	~�B	k�B	`BB	O�B	@�B	8RB	.B	�B	{B	PB		7B��B�B�ZB�5B�)B�B�
B�B�TB�HB�TB�B�B��B��B��B��B��B��B��B��B	  B	B	�B	(�B	8RB	@�B	;dB	1'B	'�B	%�B	�B	
=B	B��B��B	bB	)�B	A�B	L�B	R�B	[#B	e`B	v�B	�B	�DB	��B	��B	�B	�-B	�!B	�B	�!B	�9B	�-B	�3B	�-B	�-B	�B	�B	�B	�B	�-B	�?B	�jB	ǮB	��B	��B	ǮB	�LB	��B	��B	��B	��B	��B	�B	�^B	�fB	�ZB	�/B	��B	ĜB	�FB	�-B	�-B	�qB	ĜB	��B	�fB	�`B	�BB	��B	��B	��B	�^B	�dB	ÖB	�HB	�sB	�yB	��B	��B	��B	�B
  B
B
1B
+B
VB
hB
VB
VB

=B
B
B
B
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�mB	�fB	�fB	�fB	�fB	�`B	�ZB	�NB	�NB	�TB	�TB	�TB	�TB	�mB	�mB	�fB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�fB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
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
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
,B
33B
;dB
@�B
D�B
H�B
O�B
VB
[#B
aH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  B �B �B,�BDrBV�B_Bd-BjPBmcBqzBt�B{�B��B��B��B�
B�9B��B��B��B��B��B��B��B��B��B��B�}B��B��B��Bc$Bt�B�BB��BɇB�2B�tB�XB�B�BKB6B5B �B!�B)�BEoBL�B�2B��B��B�BFsB;B��B�QB��B�B��B�jB��Bu�Be+BS�BI�B@LB �B�B
�AB
��B
̘B
��B
ryB
I�B
!�B	��B	�]B	�0B	��B	�iB	�B	��B	��B	�QB	~�B	kTB	`B	O�B	@SB	8"B	-�B	�B	MB	"B		B��B�xB�+B�B��B��B��B��B�%B�B�%B�TB�`B��B��B��B��B��B��B��B��B��B	�B	KB	(�B	8B	@NB	;-B	0�B	'�B	%�B	fB	
B	 �B��B��B	,B	)�B	ARB	L�B	R�B	Z�B	e'B	v�B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�.B	�qB	̐B	ˋB	�rB	�B	��B	��B	��B	��B	�jB	��B	�B	�&B	�B	��B	ϟB	�^B	�B	��B	��B	�2B	�^B	ӸB	�&B	�!B	�B	ԾB	͗B	͖B	� B	�&B	�XB	�B	�5B	�:B	��B	��B	��B	�vB	��B
�B
�B
�B
B
'B
B
B
	�B
�B
�B
 �B
 �B	��B	��B	��B	�iB	�KB	�XB	�PB	�cB	�jB	�cB	�\B	�_B	�oB	�|B	�B	��B	�zB	�uB	��B	��B	��B	��B	�hB	�CB	�+B	�#B	�#B	�$B	�$B	� B	�B	�B	�B	�B	�B	�B	�B	�,B	�+B	�#B	�B	�$B	�2B	�>B	�EB	�EB	�IB	�EB	�=B	�CB	�<B	�IB	�IB	�CB	�9B	�.B	�$B	�B	�$B	�<B	�HB	�CB	�JB	�OB	�TB	�[B	�TB	�UB	�\B	�XB	�UB	�GB	�;B	�6B	�8B	�8B	�FB	�MB	�UB	�RB	�\B	�\B	�aB	�jB	�jB	�rB	�wB	�sB	�kB	�kB	�fB	�bB	�`B	�ZB	�[B	�cB	�]B	�aB	�aB	�^B	�\B	�ZB	�VB	�TB	�XB	�[B	�XB	�_B	�aB	�`B	�aB	�gB	�fB	�hB	�_B	�bB	�fB	�_B	�aB	�aB	�_B	�`B	�_B	�ZB	�_B	�`B	�jB	�qB	�oB	�wB	�xB	�yB	�wB	�wB	�~B	�B	�~B	�B	�~B	�B	�wB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 B

�B
B
B
B
B
B
B

B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
"B
"B
)B
(B
)B
)B
(B
'B
/B
,B
0B
0B
0B
-B
0B
0B
/B
3B
4B
4B
5B
3B
4B
1B
:B
9B
AB
@B
FB
FB
OB
RB
SB
RB
RB
YB
[B
_B
_B
_G�O�B
YB
xB
&�B
+�B
2�B
;B
@=B
DSB
HlB
O�B
U�B
Z�B
`�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.59 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417302016080714173020160807141730  AO  ARCAADJP                                                                    20150226221246    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221246  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221246  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141730  IP                  G�O�G�O�G�O�                