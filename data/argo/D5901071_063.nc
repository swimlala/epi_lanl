CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:08Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ?A   AO  20111130140535  20190522121826  1727_5046_063                   2C  D   APEX                            2143                            040306                          846 @�g�`1   @�g�@�@7^5?|��c�hr�!1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�#3D�P D���D��fD�33D���D���D��fD�)�D�c3D�c3D�� D�  D�` D�FfD��D�)�D�S3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�#3D�P D���D��fD�33D���D���D��fD�)�D�c3D�c3D�� D�  D�` D�FfD��D�)�D�S3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��A���A�  A�A�
=A��A�"�A�+A�5?A�7LA�-A� �A�{A�A��9A�=qA�A��
A���A�A��wA��-A��PA�7LA��/A��\A���A���A���A���A���A��7A�G�A��A�A��hA��PA��+A�|�A�x�A�t�A�r�A�v�A�v�A�n�A�jA�dZA�^5A�ZA�Q�A�E�A�-A��A�VA�1A���A��yA���A���A�ffA�ȴA��A���A��9A�v�A�ZA�Q�A�O�A�;dA�VA��A�l�A�I�A�;dA�%A�ƨA�G�A�&�A�|�A��;A�\)A���A�x�A�I�A�A��A�7LA��mA���A�"�A���A��A�bNA��A��^A�Q�A���A��uA�=qA�ƨA��uA��A�+A���A��FA�33A���A���A��mA�n�A��HA���A��hA�z�A�bA�|�A��!A�=qA�r�A�=qA� �A��A���A��\A�?}A�=qA�ĜA�  A��\A��A�-A�\)A���A���A�ffA��A{�7Ayp�Ax�9Aw�-Av�\At��Asx�ArbNAop�Am�wAl�9Ak��AiG�Af~�Ab�Aa33A_�-A^�A]��A]�A[��AXE�AV�!AVA�AU��AUG�AS+APbNAOoANv�AM�
AM�-AL��AL�9ALn�AK��AJ�AJ�AH�AGO�AF�AFn�AF1'AE�#AE&�AC�mAB�AA�A@�HA?�A?�A=��A;�mA;S�A:�9A9�wA8�jA7��A65?A5%A4��A3�A2~�A1��A1dZA0��A/ƨA/
=A.�A-`BA,�A,z�A,A�A+�A)`BA(JA'�PA'?}A%�;A%VA%
=A$ĜA$-A#�wA"r�A!K�A �A ȴA 1'A��AZA��A��AAA5?AhsA?}AE�AoA9XA��An�A;dA��A$�A��Az�A?}AƨAbA
�A
=A
��AȴA	K�A	�7A�Al�AZAS�AG�A33AjAZA�AXA
=AbNA Z@�^5@��7@�1@��@��-@띲@�Ĝ@�+@�%@�ƨ@�J@�"�@�1'@�@�K�@֟�@��@�@�G�@ӝ�@ҧ�@��@Ѻ^@�Z@ϝ�@�+@˶F@�J@��`@ũ�@�%@ă@��@�G�@��@��w@�t�@�J@��@��P@���@�@�@�%@�7L@��@���@���@�+@�&�@�j@�1@�dZ@� �@��`@�S�@�^5@�x�@��7@�$�@���@��T@��-@��-@�?}@�A�@��@��@�ȴ@�p�@��m@���@�p�@���@���@�o@�v�@�E�@��T@��-@�@��@���@���@�&�@�ƨ@�dZ@�1@�j@���@��`@���@�X@��@��@���@��\@�E�@�@��@�{@�v�@��+@���@��@�33@�K�@�S�@��@�v�@��h@�O�@��T@��@�z�@�9X@��@��@��H@��m@��@���@���@��\@�V@�/@�z�@��@��@���@���@��\@�M�@�V@�$�@��#@��-@��@���@���@��u@�j@�9X@��m@�dZ@�33@��H@���@���@�@�@��y@��@���@�E�@�J@�x�@�V@��`@���@�Ĝ@�r�@�A�@���@�ƨ@�S�@�+@���@�E�@��@�J@��T@���@��-@���@���@��7@�`B@�V@�Ĝ@��u@�I�@��@�ƨ@�ƨ@�K�@��H@���@���@���@�~�@�ff@�ff@�^5@�5?@���@��h@��@��j@��9@�bN@�9X@�(�@�1@�1@��@��m@��;@�ƨ@�K�@��@�ȴ@�E�@�J@�@���@��T@���@���@��7@�`B@�;@y�@pĜ@i�^@c"�@Z��@R�!@KC�@D�D@>��@9%@2~�@-V@(Ĝ@$1@ff@��@�@ff@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��A���A�  A�A�
=A��A�"�A�+A�5?A�7LA�-A� �A�{A�A��9A�=qA�A��
A���A�A��wA��-A��PA�7LA��/A��\A���A���A���A���A���A��7A�G�A��A�A��hA��PA��+A�|�A�x�A�t�A�r�A�v�A�v�A�n�A�jA�dZA�^5A�ZA�Q�A�E�A�-A��A�VA�1A���A��yA���A���A�ffA�ȴA��A���A��9A�v�A�ZA�Q�A�O�A�;dA�VA��A�l�A�I�A�;dA�%A�ƨA�G�A�&�A�|�A��;A�\)A���A�x�A�I�A�A��A�7LA��mA���A�"�A���A��A�bNA��A��^A�Q�A���A��uA�=qA�ƨA��uA��A�+A���A��FA�33A���A���A��mA�n�A��HA���A��hA�z�A�bA�|�A��!A�=qA�r�A�=qA� �A��A���A��\A�?}A�=qA�ĜA�  A��\A��A�-A�\)A���A���A�ffA��A{�7Ayp�Ax�9Aw�-Av�\At��Asx�ArbNAop�Am�wAl�9Ak��AiG�Af~�Ab�Aa33A_�-A^�A]��A]�A[��AXE�AV�!AVA�AU��AUG�AS+APbNAOoANv�AM�
AM�-AL��AL�9ALn�AK��AJ�AJ�AH�AGO�AF�AFn�AF1'AE�#AE&�AC�mAB�AA�A@�HA?�A?�A=��A;�mA;S�A:�9A9�wA8�jA7��A65?A5%A4��A3�A2~�A1��A1dZA0��A/ƨA/
=A.�A-`BA,�A,z�A,A�A+�A)`BA(JA'�PA'?}A%�;A%VA%
=A$ĜA$-A#�wA"r�A!K�A �A ȴA 1'A��AZA��A��AAA5?AhsA?}AE�AoA9XA��An�A;dA��A$�A��Az�A?}AƨAbA
�A
=A
��AȴA	K�A	�7A�Al�AZAS�AG�A33AjAZA�AXA
=AbNA Z@�^5@��7@�1@��@��-@띲@�Ĝ@�+@�%@�ƨ@�J@�"�@�1'@�@�K�@֟�@��@�@�G�@ӝ�@ҧ�@��@Ѻ^@�Z@ϝ�@�+@˶F@�J@��`@ũ�@�%@ă@��@�G�@��@��w@�t�@�J@��@��P@���@�@�@�%@�7L@��@���@���@�+@�&�@�j@�1@�dZ@� �@��`@�S�@�^5@�x�@��7@�$�@���@��T@��-@��-@�?}@�A�@��@��@�ȴ@�p�@��m@���@�p�@���@���@�o@�v�@�E�@��T@��-@�@��@���@���@�&�@�ƨ@�dZ@�1@�j@���@��`@���@�X@��@��@���@��\@�E�@�@��@�{@�v�@��+@���@��@�33@�K�@�S�@��@�v�@��h@�O�@��T@��@�z�@�9X@��@��@��H@��m@��@���@���@��\@�V@�/@�z�@��@��@���@���@��\@�M�@�V@�$�@��#@��-@��@���@���@��u@�j@�9X@��m@�dZ@�33@��H@���@���@�@�@��y@��@���@�E�@�J@�x�@�V@��`@���@�Ĝ@�r�@�A�@���@�ƨ@�S�@�+@���@�E�@��@�J@��T@���@��-@���@���@��7@�`B@�V@�Ĝ@��u@�I�@��@�ƨ@�ƨ@�K�@��H@���@���@���@�~�@�ff@�ff@�^5@�5?@���@��h@��@��j@��9@�bN@�9X@�(�@�1@�1@��@��m@��;@�ƨ@�K�@��@�ȴ@�E�@�J@�@���@��T@���@���@��7@�`B@�;@y�@pĜ@i�^@c"�@Z��@R�!@KC�@D�D@>��@9%@2~�@-V@(Ĝ@$1@ff@��@�@ff@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBuB�B �B#�B#�B&�B/B6FBC�BR�B`BBbNBe`BgmBjBq�By�Bz�Bz�B{�B{�B{�B{�B}�B|�Bx�B{�B�7B�oB��B��B��B��B��B�B�-B�dB�jB�qB�wB�wB�}BB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�5B�ZB�fB�mB�mB�mB�fB�fB�`B�ZB�TB�HB�5B�B��BŢB�dB�^B��BƨBǮBɺB��B��B��B��B�B�BB�B��B��B��B�B�B�B�#B��B��B�^B�=Bp�B[#BD�B9XB2-B�BuB%B�B��B��B��B�{B�B|�Bq�Bo�Bl�BXB1'B�B
��B
�sB
ɺB
��B
�DB
w�B
v�B
{�B
z�B
s�B
iyB
`BB
I�B
>wB
=qB
7LB
0!B
$�B
�B
hB
B	��B	�B	�fB	�B	ĜB	�B	��B	��B	�{B	�DB	�B	w�B	jB	e`B	cTB	aHB	`BB	_;B	R�B	J�B	F�B	B�B	?}B	:^B	9XB	7LB	1'B	,B	%�B	�B	�B	�B	�B	�B	�B	hB		7B	B��B��B�B�yB�fB�BB�)B�B��B��BƨB�wB�dB�^B�LB�9B�9B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�VB�JB�=B�1B�1B�1B�=B�1B�7B�uB��B��B��B��B��B�\B�%B}�Br�Bp�B�B�Bs�B��B��B��B�JBy�Bt�Bv�B�B��B��B��B��B��B��B��B�uB�1Bv�BiyBaHBcTB[#BW
BS�BP�BM�BK�BK�BI�BH�BF�BF�BE�BC�BC�BA�B?}B=qB;dB9XB7LB8RB7LB5?B6FB7LB6FB7LB:^B@�BD�BK�BP�BP�BO�BP�BR�BQ�BQ�B]/Bp�Bw�B�B�B}�B}�B}�B~�B�1B�B}�B}�B� B�%B�hB��B��B��B��B��B��B��B��B��B��B�uB�hB��B��B��B��B��B�3B��BĜBȴB��B��B��B��B��B�B�fB�yB�B�B�B�B��B��B��B��B��B��B	%B	DB	\B	uB	�B	�B	�B	�B	!�B	"�B	"�B	#�B	&�B	/B	.B	.B	.B	0!B	0!B	.B	(�B	+B	1'B	7LB	<jB	=qB	;dB	:^B	8RB	8RB	:^B	=qB	B�B	F�B	G�B	H�B	I�B	I�B	I�B	L�B	O�B	P�B	P�B	P�B	Q�B	S�B	VB	ZB	_;B	aHB	aHB	dZB	ffB	gmB	iyB	jB	jB	m�B	o�B	o�B	p�B	p�B	q�B	r�B	r�B	r�B	u�B	v�B	x�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�JB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�3B	�FB	�LB	�XB	�XB	�^B	�^B	�^B	�dB	��B	B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	�B	�B
B
uB
�B
%�B
.B
7LB
>wB
C�B
H�B
O�B
T�B
ZB
_;B
dZB
k�B
o�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BuB�B �B#�B#�B&�B/B6FBC�BR�B`BBbNBe`BgmBk�Bs�Bz�B{�Bz�B{�B{�B{�B|�B� B~�By�B{�B�7B�oB��B��B��B��B��B�B�3B�dB�jB�qB�wB�wB�}BB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�;B�`B�fB�mB�mB�sB�sB�mB�fB�ZB�ZB�NB�BB�/B�B��BB�qBÖBǮBɺB��B��B��B�B�B�)B�ZB�B��B��B��B��B�B�B�/B�B��BƨB�hBu�BbNBG�B<jB7LB!�B�BJB��B�
B�B��B��B�1B�Br�Bp�Bu�BgmB9XB�BB
�B
��B
�-B
�bB
y�B
z�B
�B
� B
u�B
l�B
k�B
O�B
@�B
@�B
:^B
5?B
'�B
"�B
�B
	7B
  B	��B	�B	�;B	��B	�3B	��B	��B	��B	�PB	�7B	�B	o�B	ffB	e`B	cTB	gmB	gmB	W
B	L�B	H�B	C�B	B�B	:^B	:^B	9XB	33B	.B	(�B	!�B	�B	�B	�B	�B	�B	�B	JB	%B��B��B�B�B�B�NB�5B�)B�
B��B��B��B�jB�}B�XB�LB�?B�FB�9B�'B�!B�B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�PB�DB�7B�JB�VB�DB�PB��B��B��B��B��B��B�{B�DB�Bv�Bo�B�%B�%Br�B��B��B��B�{B|�Bt�Bv�B~�B��B��B��B�B�B��B��B��B�bB{�Bo�Be`BhsB_;BZBVBS�BR�BP�BO�BM�BI�BG�BF�BG�BF�BE�BB�B@�B?}B<jB:^B=qB;dB:^B:^B7LB8RB9XB:^B<jB@�BE�BN�BS�BR�BP�BQ�BS�BR�BQ�B\)Bp�Bw�B�%B�+B~�B~�B~�B}�B�PB�B� B� B� B�B�bB��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B�9B��BĜBȴB��B��B��B��B��B�
B�fB�yB�B�B�B�B��B��B��B��B��B��B	%B	DB	\B	uB	�B	�B	�B	�B	"�B	#�B	$�B	#�B	%�B	0!B	/B	/B	/B	0!B	2-B	33B	+B	+B	1'B	8RB	=qB	?}B	<jB	;dB	:^B	8RB	:^B	>wB	C�B	F�B	G�B	I�B	I�B	I�B	J�B	M�B	O�B	P�B	P�B	Q�B	R�B	S�B	W
B	ZB	_;B	aHB	aHB	dZB	ffB	hsB	jB	jB	k�B	n�B	o�B	o�B	p�B	q�B	q�B	r�B	r�B	s�B	u�B	v�B	y�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�DB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�FB	�LB	�XB	�XB	�^B	�^B	�^B	�jB	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	�B	�B
B
uB
�B
%�B
.B
7LB
>wB
C�B
H�B
O�B
T�B
ZB
_;B
dZB
k�B
o�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446562012010314465620120103144656  AO  ARGQ                                                                        20111130140535  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140535  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144656  IP                  G�O�G�O�G�O�                