CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:05Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               3A   AO  20111130140233  20190522121826  1727_5046_051                   2C  D   APEX                            2143                            040306                          846 @�W���1   @�W�F)��@7�1'�c�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&y�D'  D'�fD(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DyٚD�0 D�` D��fD��D�9�D�l�D���D��D�#3D�Y�D��fD��3D�&fD�Y�Dڠ D��fD��D�i�D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@  @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&y�D'  D'�fD(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DyٚD�0 D�` D��fD��D�9�D�l�D���D��D�#3D�Y�D��fD��3D�&fD�Y�Dڠ D��fD��D�i�D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�Q�A�VA�S�A�XA�^5A�\)A�\)A�bNA�ffA�n�A�n�A�n�A�l�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�x�A�z�A�|�A�|�A�z�A�r�A�l�A�~�AAA�p�A�hsA�`BA�Q�A�;dA�9XA�A��hA�bNA�K�A��jA�&�A���A�/A��A��-A���A�ZA�1A�S�A�;dA��wA�9XA�jA���A�-A�5?A��RA��uA�$�A�G�A���A��9A���A�+A���A�x�A���A���A�{A�n�A�A�A��/A�VA��RA�Q�A���A�G�A��A�l�A�bNA�1A�~�A��HA��A���A�"�A��A��A���A�l�A���A���A�ȴA�bA��+A��;A�r�A�VA�G�A�x�A���A�VA��A��uA�JA�ƨA�Q�A�I�A�^5A��7A�;dA�S�A��wA��A�C�A�bNA�9XAC�A}�FA|VAy�Axn�Aw/Au�Aq�^Am��Ai�^AihsAi;dAh��Ae�Ad�Ac�AaXA_�A_S�A^VA\��A[AW7LAV{ATn�ATA�AS�AQ�-APjAP1AOG�AMl�AJ�9AIƨAHA�AG%AE�
AE�ADZACABE�AA�PA@��A?&�A=�A=33A<�A;O�A:�jA:5?A9t�A7S�A6bNA6I�A5��A3�A37LA2��A2�uA1%A/�A.��A.�DA-A-�A,�HA,JA+ƨA+��A+G�A*��A*A�A(ĜA'�A%�#A$ĜA#�^A#p�A#S�A#�A"�A!��A��A�^AffA��A��A�hA��A��A%AM�A�^AK�A"�A
=A�jAA"�A��A�A5?A�AE�A��A
�jA	S�A��A=qA?}AQ�AXA9XA%A�A �y@���@�V@��@��@��;@��^@�A�@���@���@���@���@���@�\)@��@�x�@���@◍@��@�O�@�j@�b@�C�@�{@ݑh@��@ܬ@۾w@�~�@٩�@�I�@���@�b@Ӆ@�@�5?@��@�1@�@�`B@ˮ@�n�@��`@ȴ9@�ƨ@�o@��T@ě�@Ý�@�o@�x�@�l�@��T@��/@�Q�@�;d@�@���@��+@�X@�A�@�ƨ@�  @�K�@��H@��+@��@��7@�r�@�A�@�9X@�C�@��\@�^5@��@���@�j@�ƨ@�ff@�`B@��@���@�I�@��F@��H@�~�@���@��@��@��@��T@���@�hs@��j@�Q�@�S�@�ff@�@��-@���@��D@�(�@��w@�C�@���@���@�`B@��`@��9@��u@��
@�33@��H@��\@�V@���@��T@�p�@�X@�G�@��D@�9X@��@�\)@�+@�+@���@�V@�E�@�-@�{@���@���@���@���@��-@�X@�V@���@��j@���@��u@�j@�1@��
@���@�K�@��@���@��\@�~�@�^5@�{@���@��T@��h@�&�@��@�%@��@��`@���@�r�@�Z@�I�@�A�@��@�A�@�9X@�1@��;@�ƨ@�|�@�
=@�
=@�"�@�"�@��@���@��y@���@���@���@��+@�~�@�E�@�$�@�$�@�{@�@��^@��`@��@�z�@�Q�@�(�@��m@���@��P@�"�@���@�v�@�5?@��@��@�@�X@�7L@�/@���@�9X@��m@��m@�A�@�b@�bN@���@��@��D@��@�z�@�ƨ@�|�@�33@���@���@�"�@��@�5?@���@�X@�Q�@���@��m@��
@�K�@��H@��@�K�@�ff@���@�V@��9@�z�@�9X@��@�@x��@o�;@k"�@gl�@]�@T�/@M�h@F�R@Ax�@=O�@5`B@-`B@'\)@!��@O�@G�@��@V@
-@K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�Q�A�VA�S�A�XA�^5A�\)A�\)A�bNA�ffA�n�A�n�A�n�A�l�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�x�A�z�A�|�A�|�A�z�A�r�A�l�A�~�AAA�p�A�hsA�`BA�Q�A�;dA�9XA�A��hA�bNA�K�A��jA�&�A���A�/A��A��-A���A�ZA�1A�S�A�;dA��wA�9XA�jA���A�-A�5?A��RA��uA�$�A�G�A���A��9A���A�+A���A�x�A���A���A�{A�n�A�A�A��/A�VA��RA�Q�A���A�G�A��A�l�A�bNA�1A�~�A��HA��A���A�"�A��A��A���A�l�A���A���A�ȴA�bA��+A��;A�r�A�VA�G�A�x�A���A�VA��A��uA�JA�ƨA�Q�A�I�A�^5A��7A�;dA�S�A��wA��A�C�A�bNA�9XAC�A}�FA|VAy�Axn�Aw/Au�Aq�^Am��Ai�^AihsAi;dAh��Ae�Ad�Ac�AaXA_�A_S�A^VA\��A[AW7LAV{ATn�ATA�AS�AQ�-APjAP1AOG�AMl�AJ�9AIƨAHA�AG%AE�
AE�ADZACABE�AA�PA@��A?&�A=�A=33A<�A;O�A:�jA:5?A9t�A7S�A6bNA6I�A5��A3�A37LA2��A2�uA1%A/�A.��A.�DA-A-�A,�HA,JA+ƨA+��A+G�A*��A*A�A(ĜA'�A%�#A$ĜA#�^A#p�A#S�A#�A"�A!��A��A�^AffA��A��A�hA��A��A%AM�A�^AK�A"�A
=A�jAA"�A��A�A5?A�AE�A��A
�jA	S�A��A=qA?}AQ�AXA9XA%A�A �y@���@�V@��@��@��;@��^@�A�@���@���@���@���@���@�\)@��@�x�@���@◍@��@�O�@�j@�b@�C�@�{@ݑh@��@ܬ@۾w@�~�@٩�@�I�@���@�b@Ӆ@�@�5?@��@�1@�@�`B@ˮ@�n�@��`@ȴ9@�ƨ@�o@��T@ě�@Ý�@�o@�x�@�l�@��T@��/@�Q�@�;d@�@���@��+@�X@�A�@�ƨ@�  @�K�@��H@��+@��@��7@�r�@�A�@�9X@�C�@��\@�^5@��@���@�j@�ƨ@�ff@�`B@��@���@�I�@��F@��H@�~�@���@��@��@��@��T@���@�hs@��j@�Q�@�S�@�ff@�@��-@���@��D@�(�@��w@�C�@���@���@�`B@��`@��9@��u@��
@�33@��H@��\@�V@���@��T@�p�@�X@�G�@��D@�9X@��@�\)@�+@�+@���@�V@�E�@�-@�{@���@���@���@���@��-@�X@�V@���@��j@���@��u@�j@�1@��
@���@�K�@��@���@��\@�~�@�^5@�{@���@��T@��h@�&�@��@�%@��@��`@���@�r�@�Z@�I�@�A�@��@�A�@�9X@�1@��;@�ƨ@�|�@�
=@�
=@�"�@�"�@��@���@��y@���@���@���@��+@�~�@�E�@�$�@�$�@�{@�@��^@��`@��@�z�@�Q�@�(�@��m@���@��P@�"�@���@�v�@�5?@��@��@�@�X@�7L@�/@���@�9X@��m@��m@�A�@�b@�bN@���@��@��D@��@�z�@�ƨ@�|�@�33@���@���@�"�@��@�5?@���@�X@�Q�@���@��m@��
@�K�@��H@��@�K�@�ff@���@�V@��9@�z�@�9X@��@�@x��@o�;@k"�@gl�@]�@T�/@M�h@F�R@Ax�@=O�@5`B@-`B@'\)@!��@O�@G�@��@V@
-@K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BQ�BP�BP�BP�BP�BP�BP�BP�BP�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BP�BO�BN�BP�BP�BP�BN�BM�BN�BO�BT�BT�BYBjB}�B�bB�-B��B�B��B��B��B��B�B�B�;B��B�wB�XB�}B�dB�-B�'B�-B�-B�?B�}BBɺB��B��B��BɺBÖB�LB��B��B��B�{Bz�BI�B8RB7LBL�BVBN�BC�B/B�BDB��B�B�HB�#B��B�?B��B�hB�Bs�BffB[#BP�BJ�BF�B9XB+B�BuB1B
��B
��B
�B
�B
�;B
��B
ƨB
��B
�LB
�B
��B
��B
�=B
v�B
t�B
m�B
hsB
\)B
S�B
K�B
@�B
0!B
�B
1B
%B
B	��B	�B	�TB	�/B	��B	ɺB	ĜB	�qB	�!B	��B	�+B	}�B	s�B	q�B	m�B	aHB	ZB	VB	L�B	?}B	6FB	1'B	(�B	 �B	�B	�B	�B	�B	hB	VB		7B	B	B��B��B��B��B��B�B�B�mB�fB�HB�B�B��B��B��BǮBĜBB�wB�dB�RB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B��B�{B�hB�bB�VB�DB�7B�%B�B�B~�B|�B{�B{�B{�Bz�By�Bw�Bu�Br�Bp�Bo�Bl�BgmBdZB`BB\)BZBXBT�BQ�BO�BL�BJ�BG�BE�BB�BA�B@�B>wB=qB;dB:^B8RB6FB49B1'B1'B1'B/B.B-B.B.B-B-B-B-B,B-B-B-B,B,B-B-B+B+B0!B2-B33B49B7LB7LB7LB<jB>wB?}BD�BI�BM�BR�BW
B[#B[#B\)B^5BaHBbNBffBe`BbNBcTBcTBbNB`BB\)B]/B_;B_;BdZBffBgmBgmBffBffBgmBe`BdZBgmBgmBhsBhsBffBffBgmBhsBhsBjBp�Bt�Bv�Bz�B|�B|�B|�B~�B�%B�+B�+B�7B�=B�PB�bB��B��B��B��B��B��B��B��B��B�B�B�B�!B�?B�LB�jBBƨBȴB��B�
B�B�#B�/B�BB�ZB�mB�sB�B�B��B��B	B	+B	1B	
=B	\B	hB	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	)�B	/B	33B	33B	49B	6FB	8RB	8RB	9XB	;dB	?}B	?}B	?}B	@�B	@�B	B�B	E�B	H�B	M�B	P�B	VB	ZB	\)B	]/B	^5B	`BB	`BB	`BB	cTB	ffB	ffB	gmB	hsB	jB	l�B	o�B	p�B	q�B	q�B	s�B	t�B	t�B	t�B	t�B	s�B	u�B	v�B	w�B	x�B	y�B	z�B	{�B	{�B	{�B	z�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�1B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�3B	�3B	�LB	�RB	�RB	�^B	�jB	�wB	��B	�B	��B
oB
�B
,B
6FB
;dB
@�B
C�B
J�B
Q�B
YB
]/B
^5B
bNB
iyB
n�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BQ�BP�BP�BP�BP�BP�BP�BP�BP�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BP�BO�BN�BP�BP�BP�BN�BM�BN�BO�BT�BVB[#Bk�B}�B�VB�!B��B�HB�B�
B��B��B�B�B�mB��B��B�jB��B�wB�FB�3B�3B�9B�XBBƨB��B��B�B�B��BǮB�}B��B��B��B��B�%BN�B9XB:^BM�BYBR�BI�B5?B�BVBB�B�TB�;B��B�jB��B��B�+Bw�BiyB_;BR�BK�BM�BC�B0!B�B�BPB  B
��B
��B
�B
�ZB
�B
ȴB
ƨB
�^B
�3B
��B
��B
��B
y�B
x�B
q�B
n�B
`BB
W
B
O�B
H�B
8RB
 �B
	7B
%B
%B
B	�B	�fB	�HB	��B	��B	ǮB	��B	�?B	��B	�=B	�B	t�B	r�B	s�B	e`B	[#B	YB	R�B	F�B	8RB	49B	,B	#�B	�B	�B	�B	�B	uB	bB	PB	+B	B	B	  B��B��B��B��B�B�mB�sB�mB�#B�
B��B��B��B��BŢBĜB��B�jB�^B�?B�3B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B�hB�oB�VB�PB�=B�%B�B�B� B}�B}�B|�B{�Bz�Bz�Bx�Bw�Bs�Bq�Bp�Bm�BgmBffB`BB[#BZBXBS�BR�BO�BM�BJ�BH�BE�BC�BA�B?}B>wB>wB<jB;dB9XB8RB6FB49B49B49B33B0!B0!B/B/B.B.B/B.B.B-B.B.B.B/B0!B/B.B1'B33B5?B6FB9XB9XB:^B?}B@�BA�BD�BJ�BN�BS�BXB\)B\)B^5BaHBcTBdZBgmBgmBdZBffBe`BdZBbNB]/B]/B`BB`BBe`BgmBhsBiyBgmBffBiyBffBdZBgmBhsBhsBjBiyBhsBhsBiyBiyBk�Br�Bu�Bw�Bz�B|�B|�B|�B~�B�%B�1B�+B�DB�JB�VB�hB��B��B��B��B��B��B��B��B�B�B�B�B�'B�FB�RB�qBÖBƨBɺB��B�
B�B�)B�5B�HB�ZB�mB�yB�B�B��B��B	B	+B	1B	
=B	\B	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	(�B	+B	0!B	33B	33B	49B	7LB	8RB	8RB	:^B	<jB	?}B	?}B	?}B	@�B	A�B	B�B	E�B	H�B	M�B	P�B	VB	ZB	\)B	]/B	^5B	aHB	aHB	`BB	cTB	ffB	ffB	gmB	hsB	jB	l�B	o�B	p�B	q�B	q�B	s�B	t�B	t�B	t�B	u�B	t�B	u�B	v�B	w�B	x�B	z�B	z�B	|�B	|�B	|�B	{�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�9B	�9B	�RB	�RB	�RB	�^B	�jB	�wB	��B	�B	��B
oB
�B
,B
6FB
;dB
@�B
C�B
J�B
Q�B
YB
]/B
^5B
bNB
iyB
n�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446512012010314465120120103144651  AO  ARGQ                                                                        20111130140233  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140233  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144651  IP                  G�O�G�O�G�O�                